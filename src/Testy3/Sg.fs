namespace Testy3

open System
open Aardvark.Base
open Aardvark.SceneGraph
open FSharp.Data.Adaptive
open Aardvark.Rendering
open Aardvark.Physics.Sky
open Aardvark.Rendering.Text
open Aardvark.Dom
module Sg =
    module RenderPass =
        let passMinusOne = RenderPass.before "asdasd" RenderPassOrder.Arbitrary RenderPass.main
        let passMinusTwo = RenderPass.before "asdasdf" RenderPassOrder.Arbitrary passMinusOne
    [<AutoOpen>]
    module SgExtensions = 
        open Aardvark.Base.Ag
        open Aardvark.SceneGraph
        open Aardvark.SceneGraph.Semantics
        type ViewSpaceTrafoApplicator(child : IAdaptiveValue<ISg>) =
            inherit Sg.AbstractApplicator(child)
        [<Rule>]
        type ShapeSem() =
            member x.ModelTrafoStack(b : ViewSpaceTrafoApplicator, scope : Ag.Scope) =
                let trafo =
                    scope.ViewTrafo
                        |> AVal.map (fun view ->
                            let camPos = view.Backward.TransformPos V3d.Zero
                            Trafo3d.Translation(camPos)
                        )
                b.Child?ModelTrafoStack <- trafo::scope.ModelTrafoStack
    module Sky =
        module SkyInfo =
            module Calculations =
                let lumVector = V3d(0.2126, 0.7152, 0.0722)        
                let skyBackLumiance = 1.7e-4                                      
                let nightTimeFadeout (theta : float) =
                    let solar_elevation = Constant.PiHalf - theta - 0.05
                    let fadeout = if solar_elevation >= 0.0 then 1.0 else max 0.0 (1.0 / (solar_elevation.Abs() * 50.0).Exp() - 0.00001)
                    fadeout
                let calcSphereRefl (viewDir : V3d) (lightDir : V3d) = 
                    let rnd = HaltonRandomSeries(2, RandomSystem(42))
                    let trafo = Trafo3d.FromNormalFrame(V3d.OOO, -viewDir)
                    let mutable sum = 0.0
                    for i in 0..63 do 
                        let uv = RandomSample.Disk(rnd, 0)
                        let len = uv.Length
                        let nz = sqrt (1.0 - len * len)
                        let n = V3d(uv.X, uv.Y, nz)
                        let nv = trafo.Forward.TransformDir(n)
                        let a = max 0.0 (Vec.dot nv lightDir)
                        sum <- sum + a
                    let refl = sum / 64.0
                    refl
                    
            let orientationMarkersSg() =
                let circle = 
                    Array.init 13 (fun i -> 
                        let a = float i / 12.0 * Constant.PiTimesTwo
                        V3f(cos a, sin a, 0.0)
                    )
                let orientationMarks = 
                    Array.init 8 (fun i ->
                        let a = (float)(i/2) / 4.0 * Constant.PiTimesTwo
                        let u = ((float)(i%2) - 0.5) * 2.0 * Constant.RadiansPerDegree
                        V3f(cos a, sin a, u)
                    )
                let equatorSg = 
                    DrawCallInfo(13) 
                    |> Sg.render IndexedGeometryMode.LineStrip
                    |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant circle)
                    |> Sg.effect [ LuiShaders.markerEffect] 
                    |> Sg.uniform' "LineWidth" 2.0
                    |> Sg.uniform' "Color" C4f.White
                let orientationMarksSg = 
                    DrawCallInfo(8) 
                    |> Sg.render IndexedGeometryMode.LineList
                    |> Sg.vertexAttribute DefaultSemantic.Positions (AVal.constant orientationMarks)
                    |> Sg.effect [ LuiShaders.markerEffect] 
                    |> Sg.uniform' "LineWidth" 2.0
                    |> Sg.uniform' "Color" C4f.White
                let cfg = { font = Font("Arial"); color = C4b.White; align = TextAlignment.Center; flipViewDependent = false; renderStyle = RenderStyle.Billboard }
                let markLabelStr = [| "N"; "O"; "S"; "W" |]
                let markLabels = 
                    markLabelStr 
                    |> Array.mapi (fun i str ->      
                        let label = Aardvark.Rendering.Text.Sg.textWithConfig cfg (AVal.init str)
                        let ang = float i / 4.0 * Constant.PiTimesTwo
                        let dir = V3d(sin ang, cos ang, -0.06) // clockwise, starting with [0, 1, 0] / North
                        ViewSpaceTrafoApplicator(AVal.constant label)
                            |> Sg.trafo (AVal.constant (Trafo3d.Translation(dir * 20.0)))
                    )
                [
                    equatorSg
                    orientationMarksSg
                    Sg.ofArray(markLabels)
                ] |> Sg.ofSeq
                    
            let sunSg
                (geoInfoSunPosition : aval<struct(SphericalCoordinate * float)>)
                (turbidity: aval<float>)
                (cameraFov: aval<V2d>)
                (spaceVisible: aval<bool>) = 
                let sunPhiTheta = geoInfoSunPosition |> AVal.map (fun (struct(sunPhiTheta, _)) -> sunPhiTheta)
                let sunDist = geoInfoSunPosition |> AVal.map (fun (struct(_, sunDist)) -> sunDist)
                let sunDirection = sunPhiTheta |> AVal.map (fun phiTheta -> Sky.V3dFromPhiTheta(phiTheta.Phi, phiTheta.Theta))
                let sunDiameter = sunDist |> AVal.map (fun distance -> Astronomy.AngularDiameter(Astronomy.SunDiameter, distance))
                let sunColor = 
                    (sunPhiTheta, turbidity) ||> AVal.map2 (fun coord tu -> 
                        let sunColorXYZ = SunLightScattering(coord.Phi, coord.Theta, tu).GetRadiance().ToC3f()
                        sunColorXYZ.XYZinC3fToLinearSRGB().Clamped(0.0f, float32 1e30)
                    ) 
                let sgSun = 
                    DrawCallInfo(1) 
                    |> Sg.render IndexedGeometryMode.PointList
                    |> Sg.effect [ LuiShaders.sunEffect ]
                    |> Sg.cullMode' CullMode.None
                    |> Sg.uniform "SunColor" sunColor
                    |> Sg.uniform "SunDirection" sunDirection
                    |> Sg.uniform "SunSize" sunDiameter
                    |> Sg.uniform "CameraFov" cameraFov
                    |> Sg.writeBuffers' (Set.ofList [WriteBuffer.Color DefaultSemantic.Colors])
                    |> Sg.blendMode' { BlendMode.Add with SourceAlphaFactor = BlendFactor.Zero }
                    |> Sg.pass RenderPass.passMinusOne
                    |> Sg.onOff spaceVisible
                sgSun
            
            let moonSg
                (geoInfoMoonPosition : aval<struct(SphericalCoordinate * float)>)
                (geoInfoSunPosition : aval<struct(SphericalCoordinate * float)>)
                (turbidity: aval<float>)
                (cameraFov: aval<V2d>)
                (spaceVisible: aval<bool>)
                (moonTexture : ITexture)= 
                let sunPhiTheta = geoInfoSunPosition |> AVal.map (fun (struct(sunPhiTheta, _)) -> sunPhiTheta)
                let sunDirection = sunPhiTheta |> AVal.map (fun phiTheta -> Sky.V3dFromPhiTheta(phiTheta.Phi, phiTheta.Theta))
                let moonPhiTheta = geoInfoMoonPosition |> AVal.map (fun (struct(moonPhiTheta, _)) -> moonPhiTheta)
                let moonDist = geoInfoMoonPosition |> AVal.map (fun (struct(_, moonDist)) -> moonDist)
                let moonDirection = moonPhiTheta |> AVal.map (fun phiTheta -> Sky.V3dFromPhiTheta(phiTheta.Phi, phiTheta.Theta))
                let moonDiameter = moonDist |> AVal.map (fun distance -> Astronomy.AngularDiameter(Astronomy.MoonDiameter, distance))
                let moonColor = 
                    (moonPhiTheta, turbidity) ||> AVal.map2 (fun mp tu ->
                        let moonColorXYZ = SunLightScattering(mp.Phi, mp.Theta, tu).GetRadiance().ToC3f()
                        let moonColorRgb = moonColorXYZ.XYZinC3fToLinearSRGB().Clamped(0.0f, float32 1e30).ToC3d()
                        let moonLuminance = moonColorRgb * 2.5e3 / 1.6e9
                        let srSun = Constant.PiTimesTwo * (1.0 - cos (Constant.RadiansPerDegree * 0.533 * 0.5))
                        let i = srSun * 1.6e9
                        let lum = i * 0.12 / Constant.PiTimesTwo
                        moonLuminance)
                DrawCallInfo(1) 
                |> Sg.render IndexedGeometryMode.PointList
                |> Sg.effect [ LuiShaders.moonEffect ]
                |> Sg.cullMode' CullMode.None
                |> Sg.uniform "MoonColor" moonColor
                |> Sg.uniform "MoonDirection" moonDirection
                |> Sg.uniform "MoonSize" moonDiameter
                |> Sg.uniform "SunDirection" moonDirection // this is the fake sun direction for the sunSpirteGS
                |> Sg.uniform "SunSize" moonDiameter // this is the fake sun size for the sunSpriteGS
                |> Sg.uniform "CameraFov" cameraFov
                |> Sg.uniform "RealSunDirection" sunDirection
                |> Sg.texture' (Symbol.Create "MoonTexture") moonTexture
                |> Sg.writeBuffers' (Set.ofList [WriteBuffer.Color DefaultSemantic.Colors])
                |> Sg.blendMode' { BlendMode.Add with SourceAlphaFactor = BlendFactor.Zero }
                |> Sg.pass RenderPass.passMinusOne
                |> Sg.onOff spaceVisible

            let skyInfoSg
                (geoInfoSunPosition : aval<struct(SphericalCoordinate * float)>)
                (geoInfoMoonPosition : aval<struct(SphericalCoordinate * float)>)
                (lightPollution : aval<float>)
                (skyInfoTurbidity : aval<float>)
                (skyInfoRes : aval<int>)
                (skyInfocieType : aval<CIESkyType>)
                (skyInfoSkyType : aval<SkyType>)
                 = 
                let lightPol = lightPollution |> AVal.map (fun x -> Calculations.skyBackLumiance + Calculations.skyBackLumiance * x)
                let moonRefl =
                    (geoInfoSunPosition,geoInfoMoonPosition) ||> AVal.map2 (fun (struct(sunPhiTheta,_)) (struct(moonPhiTheta,_)) ->
                        let sunDir = Sky.V3dFromPhiTheta(sunPhiTheta.Phi, sunPhiTheta.Theta)
                        let moonDir = Sky.V3dFromPhiTheta(moonPhiTheta.Phi, moonPhiTheta.Theta)
                        Calculations.calcSphereRefl moonDir sunDir
                    )
                let skyImage = adaptive {
                    let! (struct(sunPhiTheta,_)) = geoInfoSunPosition
                    let! (struct(moonPhiTheta,_)) = geoInfoSunPosition
                    let! turb = skyInfoTurbidity
                    let! res = skyInfoRes
                    let! moonRefl = moonRefl
                    let! pol = lightPol
                    let! cie = skyInfocieType
                    let! skyType = skyInfoSkyType
                    let inline createSky p t =
                        match skyType with
                        | Preetham -> new PreethamSky(p, t, clamp 1.7 10.0 turb) :> IPhysicalSky
                        | CIE -> new CIESky(p, t, cie, -1.0, -1.0) :> IPhysicalSky
                        | HosekWilkie -> new HosekSky(p, t, clamp 1.0 10.0 turb, C3f.Gray50, Col.Format.CieXYZ) :> IPhysicalSky
                    let skySun = createSky sunPhiTheta.Phi sunPhiTheta.Theta
                    let skyMoon = createSky moonPhiTheta.Phi moonPhiTheta.Theta
                    let inline lightPolFun (v : V3d) = 
                        (1.0 - (abs v.Z)) * 0.33 + 0.67
                    let pol : C3d = 
                        //let polCol = C3b(255uy, 209uy, 163uy).ToC3f() // 4000k sRGB
                        //let polCol = C3b(255uy, 228uy, 206uy).ToC3f() // 5000k sRGB
                        let polCol = C3b(64uy, 64uy, 96uy).ToC3f()
                        let polColLum = Vec.dot (polCol.ToV3d()) Calculations.lumVector
                        pol * polCol.SRGBToXYZinC3f().ToC3d() / polColLum
                    let sunScale = Calculations.nightTimeFadeout sunPhiTheta.Theta
                    let cubeFaces = Array.init 6 (fun i -> 
                        PixImage.CreateCubeMapSide<float32, C4f>(i, res, 4, 
                            fun v ->
                                let mutable xyz = C3f.Black
                                xyz <- xyz + skySun.GetRadiance(v).ToC3d() * sunScale
                                xyz <- xyz + skyMoon.GetRadiance(v).ToC3d() * 2.5e3 / 1.6e9 * moonRefl // TODO: actual amount of reflected light
                                xyz <- xyz + (lightPolFun v) * pol
                                let rgb = xyz.XYZinC3fToLinearSRGB().Clamped(0.0f, Single.MaxValue)
                                if rgb.ToV3f().AnyNaN then 
                                    C4f.Black
                                else
                                    rgb.ToC4f()
                            ) :> PixImage)
                    let cubeImg = 
                        PixCube.Create ([
                            CubeSide.PositiveX, cubeFaces.[2]
                            CubeSide.NegativeX, cubeFaces.[0]
                            CubeSide.PositiveY, cubeFaces.[5]
                            CubeSide.NegativeY, cubeFaces.[4]
                            CubeSide.PositiveZ, cubeFaces.[1]
                            CubeSide.NegativeZ, cubeFaces.[3]
                        ] |> Map.ofList)
                    let tex = cubeImg |> PixCube.toTexture true
                    return tex
                }
                let sgBkg = 
                    DrawCallInfo(4) |> Sg.render IndexedGeometryMode.TriangleStrip
                    |> Sg.effect [ LuiShaders.skyEffect ]
                    |> Sg.cullMode' CullMode.None
                    |> Sg.writeBuffers' (Set.ofList [WriteBuffer.Color DefaultSemantic.Colors])
                    |> Sg.texture (Symbol.Create "SkyImage") skyImage
                    |> Sg.pass RenderPass.passMinusTwo
                sgBkg

        let skySg
            (geoInfoSunPosition : aval<struct(SphericalCoordinate * float)>)
            (geoInfoMoonPosition : aval<struct(SphericalCoordinate * float)>)
            (lightPollution : aval<float>)
            (skyInfoTurbidity : aval<float>)
            (skyInfoRes : aval<int>)
            (skyInfocieType : aval<CIESkyType>)
            (skyInfoSkyType : aval<SkyType>)
            (fov : aval<float>)
            (runtime : IRuntime)
            (viewTrafo : aval<Trafo3d>)
            (projTrafo : aval<Trafo3d>)
            (size : aval<V2i>)
            (exposureMode : aval<ExposureMode>)
            (exposure   : aval<float>)
            (key : aval<float>)
            (moonTexture : ITexture)=
            let spaceVisible = 
                 (skyInfoSkyType,skyInfocieType) ||> AVal.map2 (fun skyType cieType ->
                    match skyType with
                    | CIE -> CIESkyExt.IsSunVisible(cieType)
                    | _ -> true
                )
            let cameraFov = fov |> AVal.map (fun fv -> V2d(fv * Constant.RadiansPerDegree, fv * Constant.RadiansPerDegree))
            let sgSky = 
                [ 
                    SkyInfo.skyInfoSg geoInfoSunPosition geoInfoMoonPosition lightPollution skyInfoTurbidity skyInfoRes skyInfocieType skyInfoSkyType
                    SkyInfo.sunSg geoInfoSunPosition skyInfoTurbidity cameraFov spaceVisible
                    SkyInfo.moonSg geoInfoMoonPosition geoInfoSunPosition skyInfoTurbidity cameraFov spaceVisible moonTexture
                    // Planets.sg m.geoInfo m.planetScale m.starInfo.magBoost cameraFov spaceVisible
                    // starSg
                ] |> Sg.ofSeq 
            let sgOverlay = 
                [ 
                    SkyInfo.orientationMarkersSg()
                    //starOverlaySg 
                ] |> Sg.ofSeq
            let quad =
                let positions = [| V3f(-1,-1,0); V3f(1,-1,0); V3f(-1,1,0); V3f(1,1,0) |]
                let texcoords = [| V2f(0,0); V2f(1,0); V2f(0,1); V2f(1,1) |]
                DrawCallInfo(FaceVertexCount = 4, InstanceCount = 1)
                |> Sg.render IndexedGeometryMode.TriangleStrip 
                |> Sg.vertexAttribute' DefaultSemantic.Positions positions
                |> Sg.vertexAttribute' DefaultSemantic.DiffuseColorCoordinates texcoords
            let hdrColorSig = runtime.CreateFramebufferSignature([
                    DefaultSemantic.Colors, TextureFormat.Rgba32f; 
                    DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
                    ]
                )
            let lumSig = runtime.CreateFramebufferSignature([
                    DefaultSemantic.Colors, TextureFormat.R32f; 
                    ]
                )
            let sceneTex = 
                sgSky 
                |> Sg.viewTrafo viewTrafo
                |> Sg.projTrafo projTrafo
                |> Aardvark.SceneGraph.RuntimeSgExtensions.Sg.compile runtime hdrColorSig
                |> RenderTask.renderToColor size   
            let lumInitTask = 
                quad 
                |> Sg.effect [ LuiShaders.lumInitEffect ]
                |> Sg.uniform "SceneTexture" sceneTex
                |> Aardvark.SceneGraph.RuntimeSgExtensions.Sg.compile runtime lumSig
            let lumAtt =
                let levels = size |> AVal.map Fun.MipmapLevels
                runtime.CreateTextureAttachment(
                    runtime.CreateTexture2D(size, TextureFormat.R32f, levels, AVal.constant 1), 0, 0
                )
            let lumFbo = runtime.CreateFramebuffer(lumSig, [DefaultSemantic.Colors, lumAtt])
            let lumTex =
                RenderTask.renderTo lumFbo lumInitTask
                |> AdaptiveResource.mapNonAdaptive  (fun fbo ->
                    let out = fbo.Attachments.[DefaultSemantic.Colors] :?> ITextureLevel
                    runtime.GenerateMipMaps(out.Texture)
                    out.Texture
                )
            let sgFinal =
                quad
                |> Sg.effect [ LuiShaders.tonemapEffect ]
                |> Sg.uniform "SceneTexture" sceneTex
                |> Sg.uniform "LumTexture" lumTex
                |> Sg.uniform "ExposureMode" exposureMode
                |> Sg.uniform "MiddleGray" key
                |> Sg.uniform "Exposure" exposure
                |> Sg.writeBuffers' (Set.ofList [WriteBuffer.Color DefaultSemantic.Colors])
                |> Sg.depthTest' DepthTest.None
            let sgOverlay =
                sgOverlay
                |> Sg.viewTrafo viewTrafo
                |> Sg.projTrafo projTrafo
            let resultSg =
                Sg.ofSeq [ 
                    sgFinal |> Sg.pass RenderPass.passMinusTwo
                    sgOverlay |> Sg.pass RenderPass.passMinusOne
                ]
            resultSg
            |> Sg.depthTest' DepthTest.None
            |> Sg.depthWrite' false
            |> Sg.cullMode' CullMode.None 
            
    let rand = RandomSystem()
    let ra() = (rand.UniformDouble() * 2.0 - 1.0) * Constant.Pi
    let sg
        (m : AdaptiveModel)
        (viewTrafo : aval<Trafo3d>)
        (projTrafo : aval<Trafo3d>)
        (runtime : IRuntime)
        (size : aval<V2i>)
        (moonTexture : ITexture) =
        let shadowDepthCell = cval Unchecked.defaultof<IAdaptiveResource<IBackendTexture>>
        let sceneSgCell = cval Sg.empty
        let shadowDepth = shadowDepthCell |> AdaptiveResource.bind (fun r -> r)
        
        let sunPos = (m.geoInfo |> AVal.map (_.SunPosition >> (fun struct(s, _) -> Sky.V3dFromPhiTheta(s.Phi, s.Theta) * 100.0)))
        let sceneSgNoShader =
            let bs =
                [
                    for x in -2..2 do
                        for y in -2..2 do
                            for z in -1..1 do
                                let x = float x * 2.0
                                let y = float y * 2.0
                                let z = float z * 2.0
                                let b =
                                    Sg.box' C4b.White (Box3d.FromCenterAndSize(V3d.OOO,V3d.III))
                                    |> Sg.translate x y z
                                yield b
                ] |> List.map (fun b ->
                    if rand.UniformDouble() > 0.8 then
                        b |> Sg.rotate (ra()) (ra()) (ra())
                    else
                        b
                )
                |> List.append [
                    yield
                        Sg.box' C4b.VRVisGreen (Box3d.FromCenterAndSize(V3d.OON*4.0,V3d(30.0,30.0,1.0)))
                ]
            Sg.ofList bs 
            |> Sg.uniform "LightLocation" sunPos
            |> Sg.uniform' "HasSpecularColorTexture" false
            |> Sg.texture "SpecularColorTexture" DefaultTextures.checkerboard
            
        let lightView =
            sunPos |> AVal.map (fun sp ->
                let dir = Vec.normalize sp
                Trafo3d.FromNormalFrame(V3d.Zero, dir)
            )
        let sceneBounds =
            sceneSgCell |> AVal.bind (fun sceneSg ->
                Aardvark.SceneGraph.Semantics.BoundingBoxes.Semantic.globalBoundingBox Ag.Scope.Root sceneSg
            )
        let lightProj =
            (lightView, sceneBounds) ||> AVal.map2 (fun view bounds ->
                let bb = bounds.ComputeCorners() |> Array.map (fun p -> view.Forward.TransformPos p) |> Box3d
                Frustum.ortho bb |> Frustum.projTrafo
            )
        let lightViewProj = (lightView,lightProj) ||> AVal.map2 (*)
        let sceneSgNormal =
            sceneSgNoShader
            |> Sg.shader {
                do! DefaultSurfaces.trafo
                do! DefaultSurfaces.vertexColor
                do! DefaultSurfaces.lighting false
                do! Shaders.shadowShader
            }
            |> Sg.texture "ShadowDepth" shadowDepth
            |> Sg.uniform "LightViewProj" lightViewProj
            |> Sg.viewTrafo viewTrafo
            |> Sg.projTrafo projTrafo
            
        let shadowMapSize = V2i(2048, 2048) |> AVal.constant
        let signature = 
           runtime.CreateFramebufferSignature [
               DefaultSemantic.DepthStencil, TextureFormat.DepthComponent32
           ]
        let shadowDepthTex =
            let shadowRos =
                let cc = FShade.Effect.ofFunction <| DefaultSurfaces.constantColor C4f.White
                let objs = Aardvark.SceneGraph.Semantics.RenderObjectSemantics.Semantic.renderObjects Ag.Scope.Root sceneSgNormal
                objs |> ASet.map (fun ro ->
                    match ro with
                    | :? RenderObject as ro ->
                        let nro = RenderObject.Clone(ro)
                        match nro.Surface with
                        | Surface.Effect old -> 
                            nro.Surface <- Surface.Effect (FShade.Effect.compose [old; cc])
                            nro.Uniforms <-
                                UniformProvider.union
                                    (UniformProvider.ofList [
                                        "ViewTrafo", lightView :> IAdaptiveValue
                                        "ProjTrafo", lightProj
                                    ])
                                    nro.Uniforms
                            nro :> IRenderObject
                        | _ -> nro :> IRenderObject
                    | _ -> ro
                )
            runtime.CompileRender(signature, shadowRos)
            |> RenderTask.renderToDepthWithClear shadowMapSize (clear {depth 1.0; stencil 0})
        transact (fun _ ->
            shadowDepthCell.Value <- shadowDepthTex
            sceneSgCell.Value <- sceneSgNormal    
        )   
        let skySg =
            Sky.skySg
                (m.geoInfo |> AVal.map (_.SunPosition))
                (m.geoInfo |> AVal.map (_.MoonPosition))
                (m.skyInfo |> AVal.map (_.lightPollution))
                (m.skyInfo |> AVal.map (_.turbidity))
                (m.skyInfo |> AVal.map (_.res))
                (m.skyInfo |> AVal.map (_.cieType))
                (m.skyInfo |> AVal.map (_.skyType))
                m.skyFov
                runtime
                viewTrafo
                projTrafo
                size
                m.exposureMode
                m.exposure
                m.key
                moonTexture
        Sg.ofList [
            sceneSgNormal
            skySg
        ]