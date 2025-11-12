namespace Testy3

open System
open System.IO
open Aardvark.Base
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Physics.Sky
open Aardvark.Rendering.Text
open Aardvark.Dom
open Aardvark.Rendering.Raytracing
open Aardvark.SceneGraph.Raytracing
open Testy3.LuiSceneGraph


module Sg =
    let rand = RandomSystem()
    let ra() = (rand.UniformDouble() * 2.0 - 1.0) * Constant.Pi
    
    let sg
        (m : AdaptiveModel)
        (viewTrafo : aval<Trafo3d>)
        (projTrafo : aval<Trafo3d>)
        (runtime : IRuntime)
        (size : aval<V2i>)
        (moonTexture : ITexture) =
        let trafos =
            [
                for x in -1..1 do
                    for y in -1..1 do
                        for z in -1..1 do
                            let x = float x * 2.0
                            let y = float y * 2.0
                            let z = float z * 2.0
                            yield Trafo3d.Translation(x,y,z)
            ] |> List.map (fun b ->
                if rand.UniformDouble() > 0.8 then
                    b * (Trafo3d.Rotation(rand.UniformV3dDirection(), rand.UniformDouble() * Constant.Pi * 2.0))
                else
                    b
            )
            |> List.append [
                Trafo3d.Scale(V3d(30.0,30.0,1.0)) * Trafo3d.Translation(V3d.OON*4.0)
            ]
            
        let ig = IndexedGeometryPrimitives.Box.solidBox (Box3d.FromCenterAndSize(V3d.OOO,V3d.III)) C4b.White
        let tos =
            trafos
            |> List.toArray
            |> Array.map (fun too ->
                Tracy.indexedGeometryToTraceObject ig too HitGroup.Model 1
            )
        
        let accumQuadTrafo =
            Trafo3d.Translation(V3d(0.0,0.0,-3.4)) * Trafo3d.Scale(V3d(5.0,5.0,1.0))
        let accTo =
            let ig =
                let pos =
                    [|
                        (accumQuadTrafo.TransformPos V3d.NNO) |> V3f
                        (accumQuadTrafo.TransformPos V3d.PNO) |> V3f
                        (accumQuadTrafo.TransformPos V3d.IIO) |> V3f
                        (accumQuadTrafo.TransformPos V3d.NPO) |> V3f
                    |]
                let tc =
                    [| V2f.OO; V2f.IO; V2f.II; V2f.OI |]
                let n = Array.replicate 4 V3d.OOI |> Array.map (fun v -> accumQuadTrafo.Backward.TransposedTransformDir v |> V4f)
                let idx = [| 0;1;2; 0;2;3 |]
                IndexedGeometry(
                    Mode = IndexedGeometryMode.TriangleList,
                    IndexArray = idx,
                    IndexedAttributes = SymDict.ofList [
                        DefaultSemantic.Positions, pos :> Array
                        DefaultSemantic.Normals, n :> Array
                        DefaultSemantic.DiffuseColorCoordinates, tc :> Array
                    ]
                )
            Tracy.indexedGeometryToTraceObject ig Trafo3d.Identity HitGroup.Quad 2
        let tos = Array.append tos [| accTo |]
        let geometryPool,scene = Tracy.createScene runtime (ASet.ofArray tos)
        let sunDir = m.geoInfo |> AVal.map _.SunDirection
        let viewProj = (viewTrafo, projTrafo) ||> AVal.map2 (fun v p -> v * p)
        
        let numDirs = 1440
        let sunDirections =
            m.geoInfo |> AVal.map (fun gi ->
                let start = gi.time.Date
                Array.init numDirs (fun i -> 
                    let time = start + TimeSpan.FromMinutes(float i * 1.0)
                    let gi = {gi with time = time}
                    gi.SunDirection |> V4f
                )
                |> Array.filter (fun d -> d.Z > 0.0f)
            )
        //1025 W / m² maximum solar irradiance on surface
        let efficiency = 0.24
        let timeStep = 24.0 * 60.0 * 60.0 / float numDirs
        let accumUniforms =
            let custom = 
                uniformMap {
                    value  "PlaneTrafo"           accumQuadTrafo
                    value  "PlaneTrafoInvTransposed"   accumQuadTrafo.Backward.Transposed
                    buffer "SunDirections"        sunDirections
                    value  "NumSunDirections"     (sunDirections |> AVal.map _.Length)
                    value   "NormalizationFactor"   (2.0f / float32 numDirs)
                    value "TimeStep"  timeStep
                    value "Efficiency"  efficiency
                }
            UniformProvider.union geometryPool.Uniforms custom
        let accumpipeline =
            {
                Effect            = Tracy.AccumulateShader.main
                Scenes            = Map.ofList [Sym.ofString "MainScene", scene]
                Uniforms          = accumUniforms
                MaxRecursionDepth = AVal.constant 2
            }
        let accumOutput = runtime.TraceTo2D(V2i.II * 2048, TextureFormat.Rgba32f, "OutputBuffer", accumpipeline)
        let uniforms =
            let custom = 
                uniformMap {
                        value  "SunDirection"          sunDir
                        value  "ViewProjTrafo"         viewProj
                        value  "ViewProjTrafoInv"      (viewProj |> AVal.map Trafo.inverse)
                        texture "AccumTexture" accumOutput
                    }
            UniformProvider.union geometryPool.Uniforms custom
        let pipeline =
            {
                Effect            = Tracy.ForwardShaders.main
                Scenes            = Map.ofList [Sym.ofString "MainScene", scene]
                Uniforms          = uniforms
                MaxRecursionDepth = AVal.constant 2
            }
        let traceOutput = runtime.TraceTo2D(size, TextureFormat.Rgba8, "OutputBuffer", pipeline)
        let accumVis =
            Sg.fullScreenQuad
            |> Sg.scale 0.1
            |> Sg.translate 0.9 0.9 0.0
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.diffuseTexture
            }
            |> Sg.diffuseTexture accumOutput
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.pass RenderPass.passPlusOne
            |> Sg.viewTrafo' Trafo3d.Identity
            |> Sg.projTrafo' Trafo3d.Identity
        
        let tracedSceneSg =
            Sg.fullScreenQuad
            |> Sg.shader {
                do! DefaultSurfaces.diffuseTexture
            }
            |> Sg.diffuseTexture traceOutput
            |> Sg.blendMode' BlendMode.Blend
            |> Sg.pass RenderPass.main
        let skySg =
            LuiSceneGraph.Sky.skySg
                (m.geoInfo |> AVal.map (_.SunPosition))
                (m.geoInfo |> AVal.map (_.MoonPosition))
                (m.geoInfo |> AVal.map (_.timeZone))
                (m.geoInfo |> AVal.map (_.gpsLong))
                (m.geoInfo |> AVal.map (_.gpsLat))
                (m.geoInfo |> AVal.map (_.time))
                (m.skyInfo |> AVal.map (_.lightPollution))
                (m.skyInfo |> AVal.map (_.turbidity))
                (m.skyInfo |> AVal.map (_.res))
                (m.skyInfo |> AVal.map (_.cieType))
                (m.skyInfo |> AVal.map (_.skyType))
                m.planetScale
                m.magBoost
                m.skyFov
                runtime
                viewTrafo
                projTrafo
                size
                m.exposureMode
                m.exposure
                m.key
                m.starLinesVisible
                moonTexture
        Sg.ofList [
            tracedSceneSg
            skySg
            accumVis
        ]