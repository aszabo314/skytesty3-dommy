namespace Testy3

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Dom

module LuiShaders =
    type VertexSky = {
        [<Position>]            pos : V4d
        [<Semantic("SkyDir")>]  dir : V3d
    }
    type FSQVertex = {
        [<VertexId>]        vid     : uint32
        [<Position>]        pos     : V4d
        [<TexCoord>]        tc      : V2d
    }
    type VertexWithUV = {
        [<Position>]        pos     : V4d
        [<TexCoord>]        tc      : V2d
        [<SourceVertexIndex>] svi   : int
        [<VertexId>]        vi      : int
    }
    type VertexPlanet = {
        [<Position>] p : V4d
        [<Color>]    c : V4d
        [<TexCoord>] uv : V2d
    }
    type VertexStar = {
        [<Position>]  p : V4d
        [<Color>]     c : V4d
        [<PointSize>] s : float
    }
    type UniformScope with
        member x.SunSize : float = x?SunSize
        member x.SunDirection : V3d = x?SunDirection
        member x.SunColor : V3d = x?SunColor
        member x.CameraFov : V2d = x?CameraFov //horizontal and vertical fov in radians
        member x.MoonSize : float = x?MoonSize
        member x.MoonDirection : V3d = x?MoonDirection
        member x.MoonColor : V3d = x?MoonColor
        member x.RealSunDirection : V3d = x?RealSunDirection
        member x.PlanetSize : float = x?PlanetSize
        member x.PlanetDir : V3d = x?PlanetDir
        member x.PlanetColor : V3d = x?PlanetColor
        member x.ExposureMode : ExposureMode = uniform?ExposureMode
        member x.MiddleGray : float = x?MiddleGray
        member x.Exposure : float = x?Exposure
        member x.MagBoost : float = x?MagBoost
    let private lumTexture =
        sampler2d {
            texture uniform?LumTexture
            filter Filter.MinMagPoint
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
    let private sceneTexture =
        sampler2d {
            texture uniform?SceneTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
    let moonTextureSampler = 
        sampler2d {
            texture uniform?MoonTexture
            filter Filter.Anisotropic
            addressU WrapMode.Wrap
            addressV WrapMode.Clamp
        }
    let lumVector = V3d(0.2126, 0.7152, 0.0722)
    let lumInit (v : FSQVertex) =
        fragment {
            let scene = sceneTexture.Sample(v.tc)
            let lum = Vec.dot scene.XYZ lumVector
            let logLumClamped = clamp -10.0 20.0 (log lum)
            return V4d(logLumClamped, 0.0, 0.0, 0.0) 
        }
    let screenQuad (v : FSQVertex) = 
        vertex {
            let x = float (v.vid >>> 1)   // /2: 0, 0, 1, 1 
            let y = float (v.vid &&& 1u)  // %2: 0, 1, 0, 1
            let coord = V2d(x, y)
            let pos = V4d(coord.X * 2.0 - 1.0, coord.Y * 2.0 - 1.0, 0.0, 1.0)
            return {
                vid = v.vid
                pos = pos
                tc = coord
            }
        }
    let vsSky (v : VertexSky) =
        vertex {
            let viewDir = (uniform.ProjTrafoInv * v.pos).XY
            let cubeDir = (uniform.ModelViewTrafoInv * V4d(viewDir.X, viewDir.Y, -1.0, 0.0)).XYZ 
            let posFar = V4d(v.pos.X, v.pos.Y, 1.0, 1.0) 
            return { v with pos = posFar; dir = cubeDir }
        }
    let cubeMapSampler =
        samplerCube {
            texture uniform?SkyImage
            filter Filter.MinMagMipLinear
        }
    let psSky (v : VertexSky) =
        fragment {
            let dir = v.dir
            let dir = V3d(dir.X, -dir.Z, dir.Y)
            return V4d(cubeMapSampler.Sample(dir).XYZ, 1.0)
        }
    let borderPixelSize = 64.0
    let sunCoronaExponent = 256.0
    let sunSpriteGs (v : Point<VertexWithUV>) =
        triangle {
            let viewDir = (uniform.ViewTrafo * V4d(uniform.SunDirection, 0.0)).XYZ
            if viewDir.Z < 0.0 then
                let proj = V3d(viewDir.X * uniform.ProjTrafo.M00, viewDir.Y * uniform.ProjTrafo.M11, viewDir.Z * uniform.ProjTrafo.M22)
                let projDir = proj.XY / proj.Z
                let borderSize = borderPixelSize / V2d(uniform.ViewportSize)
                let extendOffset = (2.0 * uniform.SunSize / uniform.CameraFov + borderSize) * 1.2 

                for i in 0..3 do
                    let x = float (i &&& 0x1) // 0, 1, 0, 1
                    let y = float (i >>> 1)   // 0, 0, 1, 1
                    let extend = V2d(x - 0.5, y - 0.5) * 2.0 * extendOffset
                    let pos = V4d(projDir.X + extend.X, projDir.Y + extend.Y, 1.0, 1.0)
                    let temp = (uniform.ProjTrafoInv * V4d(pos.X, pos.Y, 0.0, 0.0)).XY
                    let dir = (uniform.ViewTrafoInv * V4d(temp.X, temp.Y, -1.0, 0.0)).XYZ
                    yield { pos = pos; dir = dir.Normalized }
        }
    let moonSpritePs (v : VertexSky) =
        fragment {
            let pos = v.pos
            let temp = (uniform.ProjTrafoInv * V4d(pos.X, pos.Y, 0.0, 0.0)).XY
            let dir = (uniform.ViewTrafoInv * V4d(temp.X, temp.Y, -1.0, 0.0)).XYZ
            let vdir = dir.Normalized
            let moonSizeAng = uniform.MoonSize
            let moonDir = uniform.MoonDirection
            let viewAng = acos (min (Vec.dot vdir moonDir) 1.0)
            if viewAng > moonSizeAng then
                discard()
            let moonSurfaceNormal, texCoord = 
                if viewAng > 1e-4 then
                    let x = viewAng / moonSizeAng
                    let moonSurfaceNormalZ =  sqrt (1.0 - x * x)
                    let up = V3d.OOI
                    let right = Vec.cross moonDir up |> Vec.normalize
                    let up = Vec.cross right moonDir |> Vec.normalize
                    let x = (Vec.dot vdir right) / (sin moonSizeAng)
                    let y = (Vec.dot vdir up) / (sin moonSizeAng)
                    let viewNormal = V3d(x, y, -moonSurfaceNormalZ)
                    let xx = Vec.dot viewNormal (V3d(right.X, up.X, moonDir.X))
                    let yy = Vec.dot viewNormal (V3d(right.Y, up.Y, moonDir.Y))
                    let zz = Vec.dot viewNormal (V3d(right.Z, up.Z, moonDir.Z))
                    let u = (atan2 x moonSurfaceNormalZ) * Constant.PiInv * 0.5 + 0.5
                    let v = 1.0 - (acos (clamp y -1.0 1.0) * Constant.PiInv)
                    (V3d(xx, yy, zz), V2d(u, v))
                else 
                    (-moonDir, V2d(0.5))
            let tex = moonTextureSampler.Sample(texCoord)
            let texNorm = 0.75
            let shade = max 0.0 (Vec.dot moonSurfaceNormal uniform.RealSunDirection)
            let shade = 0.0001 + shade * 0.9999
            let moonColor = uniform.MoonColor * shade / texNorm
            let colMax = max moonColor.X (max moonColor.Y moonColor.Z)
            let moonNorm = moonColor * 30000.0 / max 30000.0 colMax
            return V4d(moonNorm * tex.XYZ, 1.0)
        }
    let sunSpritePs (v : VertexSky) =
        fragment {
            let pos = v.pos
            let temp = (uniform.ProjTrafoInv * V4d(pos.X, pos.Y, 0.0, 0.0)).XY
            let dir = (uniform.ViewTrafoInv * V4d(temp.X, temp.Y, -1.0, 0.0)).XYZ
            let vdir = dir.Normalized
            let sunSizeAng = uniform.SunSize
            let sunDir = uniform.SunDirection
            let viewAng = acos (min (Vec.dot vdir sunDir) 1.0)
            let coronaAng = uniform.SunSize + 4.0 * Vec.Dot(V2d(0.5, 0.5), borderPixelSize * uniform.CameraFov / V2d(uniform.ViewportSize))
            if viewAng > coronaAng then
                discard()
            let alpha =
                if viewAng <= sunSizeAng then
                    1.0 
                else 
                    pow (abs (1.0 - (viewAng - sunSizeAng) / (coronaAng - sunSizeAng))) sunCoronaExponent
            let colMax = max uniform.SunColor.X (max uniform.SunColor.Y uniform.SunColor.Z)
            let sunNorm = uniform.SunColor * 30000.0 / max 30000.0 colMax
            return V4d(sunNorm * alpha, 1.0)
        }
    let planetSpriteGs (v : Point<VertexPlanet>) = 
        triangle {
            let viewDir = (uniform.ViewTrafo * V4d(uniform.PlanetDir, 0.0)).XYZ
            if viewDir.Z < 0.0 then
                let proj = V3d(viewDir.X * uniform.ProjTrafo.M00, viewDir.Y * uniform.ProjTrafo.M11, viewDir.Z * uniform.ProjTrafo.M22)
                let projDir = proj.XY / proj.Z
                let ar = float uniform.ViewportSize.X / float uniform.ViewportSize.Y
                let minSz = 1.0 / V2d(uniform.ViewportSize)
                let sizeX = max minSz.X uniform.PlanetSize
                let sizeY = max minSz.Y (uniform.PlanetSize * ar)
                let actualSize = uniform.PlanetSize * uniform.PlanetSize * Constant.PiHalf
                let adj = min 1.0 (actualSize / (sizeX * sizeX))
                let uvClamp = (minSz - V2d(sizeX, sizeY)) * V2d(uniform.ViewportSize) + (1.0 - Constant.Sqrt2 * 0.5)
                let uvClamp = V2d(max uvClamp.X 0.0, max uvClamp.Y 0.0)
                for i in 0..3 do
                    let x = float (i &&& 0x1)
                    let y = float (i >>> 1)
                    let uv = V2d(x - 0.5, y - 0.5) * 2.0
                    let extend = uv * V2d(sizeX, sizeY)
                    let pos = V4d(projDir.X + extend.X, projDir.Y + extend.Y, 1.0, 1.0)
                    let uvBias = V2d(float (sign uv.X) * uvClamp.X, float (sign uv.Y) * uvClamp.Y)
                    yield { p = pos; c = V4d(uniform.PlanetColor * adj, 1.0); uv = uv - uvBias }
        }
    let planetSpritePs (v : VertexPlanet) = 
        fragment {
            if v.uv.LengthSquared > 1.0 then discard()
            return v.c
        }
    [<GLSLIntrinsic("exp({0})")>]
    let Exp<'a when 'a :> IVector> (a : 'a) : 'a = onlyInShaderCode ""
    
    [<GLSLIntrinsic("mix({0},{1},{2})")>]
    let LerpV<'a when 'a :> IVector> (a : 'a) (b : 'a) (s : 'a) : 'a = onlyInShaderCode ""

    [<GLSLIntrinsic("lessThanEqual({0},{1})")>]
    let LessThanEqual<'a when 'a :> IVector> (a : 'a) (b : 'a) : 'a = onlyInShaderCode ""

    [<ReflectedDefinition>]
    let private LinearToGammaSRGBVec(c : V3d) : V3d =
        let rTrue = c * 12.92
        let rFalse = 1.055 * V3d(pow c (V3d(1.0 / 2.4))) - 0.055
        LerpV rFalse rTrue (LessThanEqual c (V3d 0.0031308))
    let tonemap (v : FSQVertex) =
        fragment {
            let scene = sceneTexture.Sample(v.tc).XYZ
            let ev = 
                if uniform.ExposureMode = ExposureMode.Manual then
                    exp uniform.Exposure
                else
                    let last = lumTexture.MipMapLevels - 1
                    let avgLum = exp (lumTexture.Read(V2i(0, 0), last).X)
                    let key = if uniform.ExposureMode = ExposureMode.Auto then
                                1.001 - (2.0 / (2.0 + log(avgLum + 1.0) / log(10.0)))
                              else
                                uniform.MiddleGray
                    key / avgLum
            let color = scene * ev
            let color = color / (1.0 + color)
            let color = LinearToGammaSRGBVec color
            return V4d(color, 1.0)
        }
    let magBoost (v : Effects.Vertex) =
        vertex {
            let boost = uniform.MagBoost
            let intScalePerMag = 2.511
            let scale = pow intScalePerMag boost
            return { v with c = V4d(v.c.XYZ * scale, v.c.W) }
        }
    let equatorTrafo (v : Effects.Vertex) =
        vertex {
            let p = v.pos
            let p = uniform.ModelTrafo * V4d(p.XYZ, 0.0)
            let p = uniform.ViewTrafo * V4d(p.XYZ, 0.0)
            let p = uniform.ProjTrafo * V4d(p.XYZ, 1.0)
            return { v with pos = p }
        }
    let starTrafo (v : VertexStar) =
        vertex {
            let dir = v.p.XYZ
            let direarth = (uniform.ModelTrafo * V4d(dir, 0.0)).XYZ
            let vdir = (uniform.ViewTrafo * V4d(direarth, 0.0)).XYZ
            let p = uniform.ProjTrafo * V4d(vdir, 1.0)
            let pp = if p.Z <= 0.0 then V2d(-666.0) else p.XY / p.W
            let fovRad = uniform.CameraFov
            let vpz = uniform.ViewportSize
            let sunDiameterRad = 0.533 * Constant.RadiansPerDegree
            let sunRadPx = (sunDiameterRad / fovRad.X * float vpz.X) * 0.5
            let sunPixels = sunRadPx * sunRadPx * Constant.Pi
            let c = V4d(v.c.XYZ * sunPixels, 1.0)
            return { p = V4d(pp.X, pp.Y, 1.0, 1.0); c = c; s = 1.0 }
        }
    let lumInitEffect = 
        toEffect  lumInit
    let tonemapEffect = 
        toEffect tonemap
    let planetEffect = 
        Effect.compose [
            toEffect planetSpriteGs
            toEffect planetSpritePs
            toEffect magBoost
        ]
    let starEffect = 
        Effect.compose [
            toEffect starTrafo
            toEffect magBoost   
        ]
    let starSignEffect = 
        Effect.compose [
            toEffect equatorTrafo
            toEffect DefaultSurfaces.sgColor
        ]
    let markerEffect = 
        Effect.compose [
            toEffect equatorTrafo
            toEffect DefaultSurfaces.thickLine
            toEffect DefaultSurfaces.sgColor
        ]
    let moonEffect = 
        Effect.compose [
            toEffect sunSpriteGs
            toEffect moonSpritePs
        ]
    let skyEffect = 
        Effect.compose [
            toEffect screenQuad
            toEffect vsSky
            toEffect psSky
        ]
    let sunEffect =
        Effect.compose [
            toEffect sunSpriteGs
            toEffect sunSpritePs
        ]