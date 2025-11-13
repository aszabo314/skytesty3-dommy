namespace Testy3

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Rendering.Raytracing
open Aardvark.Rendering.Vulkan
open Aardvark.SceneGraph
open Aardvark.SceneGraph.Raytracing
open FSharp.Data.Adaptive

[<AutoOpen>]
module Semantics =
    module HitGroup =
        let Model = Sym.ofString "HitGroupModel"
        let Quad = Sym.ofString "HitGroupQuad"
        
    module InstanceAttribute =
        let NormalMatrix = Sym.ofString "NormalMatrix"
        
    module RayIds =
        let PrimaryRay = Sym.ofString "PrimaryRay"
        let ShadowRay = Sym.ofString "ShadowRay"
        let ShadowRayMiss = Sym.ofString "ShadowRayMiss"
        
module Tracy =
        
    module AccumulateShader =
        open FShade
        module Heat = 
            let heatMapColors =
                let fromInt (i : int) =
                    C4b(
                        byte ((i >>> 16) &&& 0xFF),
                        byte ((i >>> 8) &&& 0xFF),
                        byte (i &&& 0xFF),
                        255uy
                    ).ToC4f().ToV4f()   
                Array.map fromInt [|
                    0x1639fa
                    0x2050fa
                    0x3275fb
                    0x459afa
                    0x55bdfb
                    0x67e1fc
                    0x72f9f4
                    0x72f8d3
                    0x72f7ad
                    0x71f787
                    0x71f55f
                    0x70f538
                    0x74f530
                    0x86f631
                    0x9ff633
                    0xbbf735
                    0xd9f938
                    0xf7fa3b
                    0xfae238
                    0xf4be31
                    0xf29c2d
                    0xee7627
                    0xec5223
                    0xeb3b22
                |]

            [<ReflectedDefinition>]
            let heat (tc : float32) =
                let tc = clamp 0.0f 1.0f tc
                let fid = tc * float32 24 - 0.5f
                let id = int (floor fid)
                if id < 0 then 
                    heatMapColors.[0]
                elif id >= 24 - 1 then
                    heatMapColors.[24 - 1]
                else
                    let c0 = heatMapColors.[id]
                    let c1 = heatMapColors.[id + 1]
                    let t = fid - float32 id
                    if t>0.5f then c1 else c0
                    //(c0 * (1.0 - t) + c1 * t)

        
        open Heat
        
        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
            member x.SunDirections : V4f[] = uniform?StorageBuffer?SunDirections
            member x.NumSunDirections : int = uniform?NumSunDirections
            member x.NormalizationFactor : float32 = uniform?NormalizationFactor
            
            member x.PlaneTrafo : M44f = uniform?PlaneTrafo
            member x.PlaneTrafoInvTransposed : M44f = uniform?PlaneTrafoInvTransposed
            
            member x.Efficiency : float32 = uniform?Efficiency
            member x.TimeStep : float32 = uniform?TimeStep
            // new uniform for normalization maximum (was 4194300.0f)
            member x.NormalizeMax : float32 = uniform?NormalizeMax
            member x.ShaderIsoLines : bool = uniform?ShaderIsoLines
        
        type Payload =
            {
                skycount : int
            }
        
        let private mainScene =
            scene {
                accelerationStructure uniform?MainScene
            }
        let rgenMain (input : RayGenerationInput) =
            raygen {
                let tc = (V2f input.work.id.XY + V2f.Half) / V2f input.work.size.XY
                let totalArea = 4.0f * Vec.length (Vec.cross uniform.PlaneTrafo.C0.XYZ uniform.PlaneTrafo.C1.XYZ)
                let pixelArea = 1.0f//totalArea / float32 (input.work.size.X * input.work.size.Y)
                let ndc = tc * 2.0f - V2f.II
                let wp = uniform.PlaneTrafo * V4f(ndc, 0.0f, 1.0f)
                let wn = (uniform.PlaneTrafoInvTransposed * V4f(0.0f, 0.0f, 1.0f, 0.0f)).XYZ |> Vec.normalize
                let origin = wp.XYZ / wp.W
                let mutable acc = 0.0f
                for i in 0 .. uniform.NumSunDirections - 1 do
                    let dir = uniform.SunDirections.[i].XYZ
                    let res = mainScene.TraceRay<Payload>(origin,dir)
                    if res.skycount > 0 then
                        let energy = (1025.0f * Vec.dot wn dir) * pixelArea * uniform.TimeStep * uniform.Efficiency //J
                        acc <- acc + energy
                let joule = acc
                // grayscale mapping uses uniform maximum
                let gray = clamp 0.0f 1.0f (float32 joule / uniform.NormalizeMax)
                let color = heat gray
                if uniform.ShaderIsoLines then
                    //iso lines
                    uniform.OutputBuffer.[input.work.id.XY] <- V4f(color.XYZ,1.0f)
                else
                    uniform.OutputBuffer.[input.work.id.XY] <- V4f(color.XYZ,1.0f)
            }
        let chit (input : RayHitInput<Payload>) =
            closestHit {
                return { unchanged with skycount = 0 }
            }
    
        let missSky (input : RayMissInput) =
            miss {
                return { unchanged with skycount = 1 }
            }
        let private hitGroupModel =
            hitgroup {
                closestHit chit
            }
        let main =
            raytracingEffect {
                raygen rgenMain
                miss missSky
                hitgroup HitGroup.Model hitGroupModel
            }
    
    module ForwardShaders =
        open FShade
        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
            member x.Normals        : V4f[]  = uniform?StorageBuffer?Normals
            member x.TextureCoords   : V2f[]  = uniform?StorageBuffer?DiffuseColorCoordinates
            member x.SunDirection : V3f = uniform?SunDirection
        let private accumTexture =
            sampler2d {
                texture uniform?AccumTexture
                filter Filter.MinMagPointMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        [<ReflectedDefinition>]
        let fromBarycentric (v0 : V3f) (v1 : V3f) (v2 : V3f) (coords : V2f) =
            let barycentricCoords = V3f(1.0f - coords.X - coords.Y, coords.X, coords.Y)
            v0 * barycentricCoords.X + v1 * barycentricCoords.Y + v2 * barycentricCoords.Z
        [<ReflectedDefinition>]
        let fromBarycentric2d (v0 : V2f) (v1 : V2f) (v2 : V2f) (coords : V2f) =
            let barycentricCoords = V3f(1.0f - coords.X - coords.Y, coords.X, coords.Y)
            v0 * barycentricCoords.X + v1 * barycentricCoords.Y + v2 * barycentricCoords.Z

        [<ReflectedDefinition; Inline>]
        let getPosition (input : RayHitInput<'T, V2f>) =
            let p0 = input.hit.positions.[0]
            let p1 = input.hit.positions.[1]
            let p2 = input.hit.positions.[2]
            input.hit.attribute |> fromBarycentric p0 p1 p2
            
        [<ReflectedDefinition>]
        let getNormal (indices : V3i) (input : RayHitInput<'T, V2f>) =
            let n0 = uniform.Normals.[indices.X].XYZ
            let n1 = uniform.Normals.[indices.Y].XYZ
            let n2 = uniform.Normals.[indices.Z].XYZ
            input.hit.attribute |> fromBarycentric n0 n1 n2
            
        [<ReflectedDefinition>]
        let getTextureCoords (indices : V3i) (input : RayHitInput<'T, V2f>) =
            let uv0 = uniform.TextureCoords.[indices.X]
            let uv1 = uniform.TextureCoords.[indices.Y]
            let uv2 = uniform.TextureCoords.[indices.Z]
            input.hit.attribute |> fromBarycentric2d uv0 uv1 uv2
            
        type Payload =
            {
                color : V4f
            }
        type ShadowPayload =
            {
                light : float32
            }
        
        let private mainScene =
            scene {
                accelerationStructure uniform?MainScene
            }
        let rgenMain (input : RayGenerationInput) =
            raygen {
                let tc = (V2f input.work.id.XY + V2f.Half) / V2f input.work.size.XY
                let ndc = tc * 2.0f - V2f.II
                let p = uniform.ViewProjTrafoInv * V4f(ndc, -1.0f, 1.0f)
                let origin = p.XYZ / p.W
                
                let p = uniform.ViewProjTrafoInv * V4f(ndc, 0.0f, 1.0f)
                let dir = p.XYZ / p.W - origin |> Vec.normalize
                
                let res = mainScene.TraceRay<Payload>(origin,dir)
                uniform.OutputBuffer.[input.work.id.XY] <- res.color
            }
            
        let chitQuad (input : RayHitInput<Payload>) =
            closestHit {
                let info = TraceGeometryInfo.ofRayHit input
                let indices = TraceGeometryInfo.getIndices info input
                let tc = getTextureCoords indices input
                let color = accumTexture.Sample(tc)
                return { unchanged with color = color }
            }
        let chit (input : RayHitInput<Payload>) =
            closestHit {
                let info = TraceGeometryInfo.ofRayHit input
                let indices = TraceGeometryInfo.getIndices info input
                let normal = getNormal indices input
                    
                let pos = getPosition input
                let sunDirection = uniform.SunDirection.XYZ
                let sunVisible =
                    mainScene.TraceRay<ShadowPayload>(
                        pos, sunDirection,
                        ray = RayIds.ShadowRay,
                        miss = RayIds.ShadowRayMiss,
                        cullMask = 1
                    )
                let n = normal
                let l = sunDirection |> Vec.normalize
                let cosine = max 0.0f (Vec.dot n l)
                let color = V4f(sunVisible.light * cosine * V3f.III, 1.0f)
                return { unchanged with color = color }
            }
        let missGlobal (input : RayMissInput) =
            miss {
                return { unchanged with color = V4f.Zero }
            }
        let chitShadow (input : RayHitInput<ShadowPayload>) =
            closestHit {
                return { unchanged with light = 0.0f }
            }
        let missShadow (input : RayMissInput) =
            miss {
                return { unchanged with light = 1.0f }
            }
        let private hitGroupModel =
            hitgroup {
                closestHit chit
                closestHit RayIds.ShadowRay chitShadow
            }
        let private hitGroupQuad =
            hitgroup {
                closestHit chitQuad
            }
        let main =
            raytracingEffect {
                raygen rgenMain
                miss missGlobal
                miss RayIds.ShadowRayMiss missShadow
                hitgroup HitGroup.Model hitGroupModel
                hitgroup HitGroup.Quad hitGroupQuad
            }
        
    let indexedGeometryToTraceObject (geom : IndexedGeometry) (trafo : Trafo3d) (hitGroup : Symbol) (mask : int)=
        geom
        |> TraceObject.ofIndexedGeometry GeometryFlags.Opaque trafo
        |> TraceObject.hitGroup hitGroup
        |> TraceObject.frontFace WindingOrder.CounterClockwise
        |> TraceObject.mask mask
        
    let createScene (runtime : IRuntime) (objects : aset<TraceObject>) =
        let geometryPool =
            let signature =
                let vertexAttributes =
                    Map.ofList [
                        DefaultSemantic.Normals, typeof<V4f>
                        DefaultSemantic.DiffuseColorCoordinates, typeof<V2f>
                    ]
                { IndexType              = IndexType.Int32
                  VertexAttributeTypes   = vertexAttributes
                  FaceAttributeTypes     = Map.empty
                  InstanceAttributeTypes = Map.empty
                  GeometryAttributeTypes = Map.empty }

            new ManagedTracePool(runtime, signature)
        geometryPool,RaytracingScene.ofPool geometryPool objects