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
        
module Tracy =
        
    module Shader =
        open FShade
        
        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
            member x.SunDirections : V4f[] = uniform?StorageBuffer?SunDirections
            member x.NumSunDirections : int = uniform?NumSunDirections
        
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
                let wp = V2f input.work.id.XY / 1024.0f * 2.0f - V2f.II
                let mutable acc = 0
                for i in 0 .. uniform.NumSunDirections - 1 do
                    let dir = uniform.SunDirections.[i].XYZ
                    let res = mainScene.TraceRay<Payload>(wp.XYO,dir)
                    acc <- acc + res.skycount
                let color = float32 acc / float32 uniform.NumSunDirections
                uniform.OutputBuffer.[input.work.id.XY] <- V4f(color,color,color,1.0f)
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
        [<ReflectedDefinition>]
        let fromBarycentric (v0 : V3f) (v1 : V3f) (v2 : V3f) (coords : V2f) =
            let barycentricCoords = V3f(1.0f - coords.X - coords.Y, coords.X, coords.Y)
            v0 * barycentricCoords.X + v1 * barycentricCoords.Y + v2 * barycentricCoords.Z

        [<ReflectedDefinition; Inline>]
        let getPosition (input : RayHitInput<'T, V2f>) =
            let p0 = input.hit.positions.[0]
            let p1 = input.hit.positions.[1]
            let p2 = input.hit.positions.[2]
            input.hit.attribute |> fromBarycentric p0 p1 p2
            
        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
            member x.SunDirection : V3f = uniform?SunDirection
        
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
            
        let chit (input : RayHitInput<Payload>) =
            closestHit {
                let pos = getPosition input
                let sunVisible = mainScene.TraceRay<ShadowPayload>(pos,uniform.SunDirection.XYZ, ray = RayIds.ShadowRay, miss = RayIds.ShadowRay)
                let color = V4f(sunVisible.light * V3f.III,1.0f)
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
        let main =
            raytracingEffect {
                raygen rgenMain
                miss missGlobal
                miss RayIds.ShadowRay missShadow
                hitgroup HitGroup.Model hitGroupModel
            }
        
    let indexedGeometryToTraceObject (geom : IndexedGeometry) (trafo : Trafo3d) (hitGroup : Symbol)=
        geom
        |> TraceObject.ofIndexedGeometry GeometryFlags.Opaque Trafo3d.Identity
        |> TraceObject.transform trafo
        |> TraceObject.hitGroup hitGroup
        |> TraceObject.frontFace WindingOrder.CounterClockwise
        
    let createScene (runtime : IRuntime) (objects : aset<TraceObject>) =
        let geometryPool =
            let signature =
                let vertexAttributes =
                    Map.ofList [
                        if not runtime.SupportsPositionFetch then DefaultSemantic.Positions, typeof<V4f>
                        DefaultSemantic.Normals, typeof<V4f>
                        DefaultSemantic.DiffuseColorCoordinates, typeof<V2f>
                    ]

                { IndexType              = IndexType.Int32
                  VertexAttributeTypes   = vertexAttributes
                  FaceAttributeTypes     = Map.empty
                  InstanceAttributeTypes = Map.empty
                  GeometryAttributeTypes = Map.empty }

            new ManagedTracePool(runtime, signature)
        RaytracingScene.ofPool geometryPool objects
        
    let testy() =
        let app = new HeadlessVulkanApplication()
        let rt = app.Runtime :> IRuntime
        let rand = RandomSystem()
        Log.startTimed("build")
        let dirs =
            Array.init 10000 (fun _ ->
                let mutable r = rand.UniformV3fDirection()
                if r.Z < 0.0f then r.Z <- -r.Z
                r
            ) 
        
        let tris =
            IndexedGeometryPrimitives.Box.solidBox (Box3d.FromCenterAndSize(V3d.OOI*0.25,V3d.III*0.5)) C4b.Red
            
        let uniforms = 
            uniformMap {
                    buffer  "SunDirections"  (dirs |> Array.map V4f)
                    value  "NumSunDirections"  dirs.Length
                }
            
        let tos = indexedGeometryToTraceObject tris (Trafo3d.RotationEulerInDegrees(12.0,24.0,36.0)) HitGroup.Model
        let scene = createScene rt (ASet.single tos)
        
        let pipeline =
            {
                Effect            = Shader.main
                Scenes            = Map.ofList [Sym.ofString "MainScene", scene]
                Uniforms          = uniforms
                MaxRecursionDepth = AVal.constant 2
            }
        
        Log.stop()
        Log.startTimed("trace")
        let traceOutput = rt.TraceTo2D(AVal.constant (V2i.II*1024), TextureFormat.Rgba8, "OutputBuffer", pipeline)
        traceOutput.Acquire()
        let tex = traceOutput |> AVal.force
        Log.stop()
        let pix = tex.Download()
        traceOutput.Release()
        pix.SaveAsPng @"C:\Temp\tracy_test.png"
            
        ()
        
        
        