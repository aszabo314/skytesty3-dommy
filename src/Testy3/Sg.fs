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
    
    module Shader =
        open FShade
        type Fragment =
            {
                [<Color>] c : V4f
                [<Depth>] d : float32
                [<Semantic("ViewSpaceNormal")>] vn : V3f
                
            }
            
        let colorSam =
            sampler2d {
                texture uniform?DiffuseColorTexture
                filter Filter.MinMagPoint
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
                
            }
            
        let pickSam =
            sampler2d {
                texture uniform?PickTexture
                filter Filter.MinMagPoint
                addressU WrapMode.Clamp
                addressV WrapMode.Clamp
            }
            
        let read (v : Effects.Vertex) =
            fragment {
                let color = colorSam.Sample(v.tc, 0.0f)
                let pick = pickSam.SampleLevel(v.tc, 0.0f)
                if pick.W >= 1.0f then discard()
                
                return {
                    c = color
                    vn = pick.XYZ |> Vec.normalize
                    d = 0.5f * (pick.W + 1.0f)
                }
                
            }
            
    
    let sg
        (m : AdaptiveModel)
        (viewTrafo : aval<Trafo3d>)
        (projTrafo : aval<Trafo3d>)
        (runtime : IRuntime)
        (size : aval<V2i>)
        (accumQuadTrafo : aval<Trafo3d>)
        (moonTexture : ITexture) =
        
        let accTo =
            accumQuadTrafo |> ASet.bind (fun accumQuadTrafo ->
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
                Tracy.indexedGeometryToTraceObject ig Trafo3d.Identity HitGroup.Quad 2 |> ASet.single
            )
        let wienTo =
            let ig = Mesh.meshToIg @"C:\bla\wienzentrum.obj"
            let trafo = Trafo3d.Scale(100.0)
            Tracy.indexedGeometryToTraceObject ig trafo HitGroup.Model 1
        let tos =
            ASet.unionMany
                (ASet.ofList [
                    accTo
                    wienTo |> ASet.single
                ])
        let geometryPool,scene = Tracy.createScene runtime tos
        let sunDir = m.geoInfo |> AVal.map _.SunDirection
        let viewProj = (viewTrafo, projTrafo) ||> AVal.map2 (fun v p -> v * p)
        
        let totalTime = m.timeframeDays |> AVal.map TimeSpan.FromDays
        let sampleTime = m.sampletimeHours |> AVal.map TimeSpan.FromHours
        let numDirs = (totalTime,sampleTime) ||> AVal.map2 (fun totalTime sampleTime -> totalTime/sampleTime |> ceil |> int)
        let stepTime = (totalTime,numDirs) ||> AVal.map2 (fun totalTime numDirs -> TimeSpan.FromSeconds(totalTime.TotalSeconds / float numDirs))
        let sunDirections =
            (m.geoInfo,numDirs,stepTime) |||> AVal.map3 (fun gi numDirs stepTime ->
                let start = gi.time.Date
                Array.init numDirs (fun i ->
                    let time = start + float i*stepTime
                    let gi = {gi with time = time}
                    gi.SunDirection |> V4f
                )
                |> Array.filter (fun d -> d.Z > 0.0f)
            )
        //1025 W / m² maximum solar irradiance on surface
        let efficiency = 0.24
        let accumUniforms =
            let custom = 
                uniformMap {
                    value  "PlaneTrafo"           accumQuadTrafo
                    value  "PlaneTrafoInvTransposed"   (accumQuadTrafo |> AVal.map _.Backward.Transposed)
                    buffer "SunDirections"        sunDirections
                    value  "NumSunDirections"     (sunDirections |> AVal.map _.Length)
                    value   "NormalizationFactor"   (numDirs |> AVal.map (fun numDirs -> (2.0f / float32 numDirs)))
                    value "TimeStep"  (stepTime |> AVal.map _.TotalSeconds)
                    value "TotalTimeSeconds" (totalTime |> AVal.map _.TotalSeconds)
                    value "Efficiency"  efficiency
                    value "NormalizeMax" (m.normalizeMax |> AVal.map float32)
                }
            UniformProvider.union geometryPool.Uniforms custom
        let accumpipeline =
            {
                Effect            = Tracy.AccumulateShader.main
                Scenes            = Map.ofList [Sym.ofString "MainScene", scene]
                Uniforms          = accumUniforms
                MaxRecursionDepth = AVal.constant 2
            }
        let accumSize = V2i.II * 64
        let accumOutput = runtime.TraceTo2D(accumSize, TextureFormat.Rgba32f, "OutputBuffer", accumpipeline)
        
        let colorTex = runtime.CreateTexture2D(size, TextureFormat.Rgba8)
        let pickTex =  runtime.CreateTexture2D(size, TextureFormat.Rgba32f)
        let effectiveSunDirections =
            m.globalRenderingMode |> AVal.bind (fun grm ->
                if grm then
                    sunDirections
                else
                    sunDir |> AVal.map (V3f >> V4f >> Array.singleton)
            )
        let uniforms =
            let custom =
                uniformMap {
                    texture "OutputBuffer" colorTex
                    texture "PickBuffer" pickTex
                    buffer "SunDirections"        effectiveSunDirections
                    value  "NumSunDirections"     (effectiveSunDirections |> AVal.map _.Length)
                    value   "NormalizationFactor"   (numDirs |> AVal.map (fun numDirs -> (2.0f / float32 numDirs)))
                    value "TimeStep"  (stepTime |> AVal.map _.TotalSeconds)
                    value "TotalTimeSeconds" (totalTime |> AVal.map _.TotalSeconds)
                    value "Efficiency"  efficiency
                    value "NormalizeMax" (m.normalizeMax |> AVal.map float32)
                    value  "ViewTrafo"             viewTrafo
                    value  "ViewProjTrafo"         viewProj
                    value  "ViewProjTrafoInv"      (viewProj |> AVal.map Trafo.inverse)
                    value "GlobalRenderingMode" m.globalRenderingMode
                    texture "AccumTexture" accumOutput
                    }
            UniformProvider.union geometryPool.Uniforms custom
        let pipeline =
            {
                Effect            = Tracy.ForwardShaders2.main
                Scenes            = Map.ofList [Sym.ofString "MainScene", scene]
                Uniforms          = uniforms
                MaxRecursionDepth = AVal.constant 2
            }
        let traceOutput =   
            (colorTex, pickTex) ||> AdaptiveResource.bind2 (fun colorTex pickTex ->
                let cmds =
                    AList.ofList [
                        RaytracingCommand.TransformLayout(colorTex, TextureLayout.ShaderRead, TextureLayout.ShaderWrite)
                        RaytracingCommand.TransformLayout(pickTex, TextureLayout.ShaderRead, TextureLayout.ShaderWrite)
                        RaytracingCommand.TraceRays colorTex.Size
                        RaytracingCommand.TransformLayout(colorTex, TextureLayout.ShaderWrite, TextureLayout.ShaderRead) 
                        RaytracingCommand.TransformLayout(pickTex, TextureLayout.ShaderWrite, TextureLayout.ShaderRead) 
                    ]
                
                let task = runtime.CompileTrace(pipeline,cmds)
            
                AVal.custom (fun t ->
                    task.Run(t)
                    colorTex :> ITexture, pickTex :> ITexture
                )
            )
        let tracedSceneSg =
            sg {
                Sg.Uniform("DiffuseColorTexture", AdaptiveResource.map fst traceOutput)
                Sg.Uniform("PickTexture", AdaptiveResource.map snd traceOutput)
                Sg.Pass RenderPass.passPlusOne
                Sg.BlendMode BlendMode.Blend
                Sg.Shader {
                    Shader.read
                }
                Primitives.FullscreenQuad
            }
            // Sg.fullScreenQuad
            // |> Sg.shader {
            //     do! DefaultSurfaces.diffuseTexture
            // }
            // |> Sg.diffuseTexture traceOutput
            // |> Sg.blendMode' BlendMode.Blend
            // |> Sg.pass RenderPass.main
        
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
        
        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
            ]
        
        let skyTex =
            skySg
            |> Sg.compile runtime signature
            |> RenderTask.renderToColorWithClear size (clear { depth 1.0; color C4f.Black })
        
        let skySg =
            sg {
                Sg.NoEvents
                Sg.Uniform("DiffuseColorTexture", skyTex)
                Sg.Shader {
                    DefaultSurfaces.diffuseTexture
                }
                Primitives.ScreenQuad 1.0
            }
        sg {
            tracedSceneSg
            skySg
        }