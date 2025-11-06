namespace Testy3

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Dom

module LuiShaders =
    type VertexSky = {
        [<Position>]            pos : V4f
        [<Semantic("SkyDir")>]  dir : V3f
    }
    type FSQVertex = {
        [<VertexId>]        vid     : uint32
        [<Position>]        pos     : V4f
        [<TexCoord>]        tc      : V2f
    }
    type VertexWithUV = {
        [<Position>]        pos     : V4f
        [<TexCoord>]        tc      : V2f
        [<SourceVertexIndex>] svi   : int
        [<VertexId>]        vi      : int
    }
    type VertexPlanet = {
        [<Position>] p : V4f
        [<Color>]    c : V4f
        [<TexCoord>] uv : V2f
    }
    type VertexStar = {
        [<Position>]  p : V4f
        [<Color>]     c : V4f
        [<PointSize>] s : float32
    }
    type UniformScope with
        member x.SunSize : float32 = x?SunSize
        member x.SunDirection : V3f = x?SunDirection
        member x.SunColor : V3f = x?SunColor
        member x.CameraFov : V2f = x?CameraFov //horizontal and vertical fov in radians
        member x.MoonSize : float32 = x?MoonSize
        member x.MoonDirection : V3f = x?MoonDirection
        member x.MoonColor : V3f = x?MoonColor
        member x.RealSunDirection : V3f = x?RealSunDirection
        member x.PlanetSize : float32 = x?PlanetSize
        member x.PlanetDir : V3f = x?PlanetDir
        member x.PlanetColor : V3f = x?PlanetColor
        member x.ExposureMode : ExposureMode = uniform?ExposureMode
        member x.MiddleGray : float32 = x?MiddleGray
        member x.Exposure : float32 = x?Exposure
        member x.MagBoost : float32 = x?MagBoost
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
    let lumVector = V3f(0.2126f, 0.7152f, 0.0722f)
    let lumInit (v : FSQVertex) =
        fragment {
            let scene = sceneTexture.Sample(v.tc)
            let lum = Vec.dot scene.XYZ lumVector
            let logLumClamped = clamp -10.0f 20.0f (log lum)
            return V4f(logLumClamped, 0.0f, 0.0f, 0.0f) 
        }
    let screenQuad (v : FSQVertex) = 
        vertex {
            let x = float32 (v.vid >>> 1)   // /2: 0, 0, 1, 1 
            let y = float32 (v.vid &&& 1u)  // %2: 0, 1, 0, 1
            let coord = V2f(x, y)
            let pos = V4f(coord.X * 2.0f - 1.0f, coord.Y * 2.0f - 1.0f, 0.0f, 1.0f)
            return {
                vid = v.vid
                pos = pos
                tc = coord
            }
        }
    let vsSky (v : VertexSky) =
        vertex {
            let viewDir = (uniform.ProjTrafoInv * v.pos).XY
            let cubeDir = (uniform.ModelViewTrafoInv * V4f(viewDir.X, viewDir.Y, -1.0f, 0.0f)).XYZ 
            let posFar = V4f(v.pos.X, v.pos.Y, 1.0f, 1.0f) 
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
            let dir = V3f(dir.X, -dir.Z, dir.Y)
            return V4f(cubeMapSampler.Sample(dir).XYZ, 1.0f)
        }
    let borderPixelSize = 64.0f
    let sunCoronaExponent = 256.0f
    let sunSpriteGs (v : Point<VertexWithUV>) =
        triangle {
            let viewDir = (uniform.ViewTrafo * V4f(uniform.SunDirection, 0.0f)).XYZ
            if viewDir.Z < 0.0f then
                let proj = V3f(viewDir.X * uniform.ProjTrafo.M00, viewDir.Y * uniform.ProjTrafo.M11, viewDir.Z * uniform.ProjTrafo.M22)
                let projDir = proj.XY / proj.Z
                let borderSize = borderPixelSize / V2f(uniform.ViewportSize)
                let extendOffset = (2.0f * uniform.SunSize / uniform.CameraFov + borderSize) * 1.2f 

                for i in 0..3 do
                    let x = float32 (i &&& 0x1) // 0, 1, 0, 1
                    let y = float32 (i >>> 1)   // 0, 0, 1, 1
                    let extend = V2f(x - 0.5f, y - 0.5f) * 2.0f * extendOffset
                    let pos = V4f(projDir.X + extend.X, projDir.Y + extend.Y, 1.0f, 1.0f)
                    let temp = (uniform.ProjTrafoInv * V4f(pos.X, pos.Y, 0.0f, 0.0f)).XY
                    let dir = (uniform.ViewTrafoInv * V4f(temp.X, temp.Y, -1.0f, 0.0f)).XYZ
                    yield { pos = pos; dir = dir.Normalized }
        }
    let moonSpritePs (v : VertexSky) =
        fragment {
            let pos = v.pos
            let temp = (uniform.ProjTrafoInv * V4f(pos.X, pos.Y, 0.0f, 0.0f)).XY
            let dir = (uniform.ViewTrafoInv * V4f(temp.X, temp.Y, -1.0f, 0.0f)).XYZ
            let vdir = dir.Normalized
            let moonSizeAng = uniform.MoonSize
            let moonDir = uniform.MoonDirection
            let viewAng = acos (min (Vec.dot vdir moonDir) 1.0f)
            if viewAng > moonSizeAng then
                discard()
            let moonSurfaceNormal, texCoord = 
                if viewAng > 1e-4f then
                    let x = viewAng / moonSizeAng
                    let moonSurfaceNormalZ =  sqrt (1.0f - x * x)
                    let up = V3f.OOI
                    let right = Vec.cross moonDir up |> Vec.normalize
                    let up = Vec.cross right moonDir |> Vec.normalize
                    let x = (Vec.dot vdir right) / (sin moonSizeAng)
                    let y = (Vec.dot vdir up) / (sin moonSizeAng)
                    let viewNormal = V3f(x, y, -moonSurfaceNormalZ)
                    let xx = Vec.dot viewNormal (V3f(right.X, up.X, moonDir.X))
                    let yy = Vec.dot viewNormal (V3f(right.Y, up.Y, moonDir.Y))
                    let zz = Vec.dot viewNormal (V3f(right.Z, up.Z, moonDir.Z))
                    let u = (atan2 x moonSurfaceNormalZ) * ConstantF.PiInv * 0.5f + 0.5f
                    let v = 1.0f - (acos (clamp y -1.0f 1.0f) * ConstantF.PiInv)
                    (V3f(xx, yy, zz), V2f(u, v))
                else 
                    (-moonDir, V2f(0.5f))
            let tex = moonTextureSampler.Sample(texCoord)
            let texNorm = 0.75f
            let shade = max 0.0f (Vec.dot moonSurfaceNormal uniform.RealSunDirection)
            let shade = 0.0001f + shade * 0.9999f
            let moonColor = uniform.MoonColor * shade / texNorm
            let colMax = max moonColor.X (max moonColor.Y moonColor.Z)
            let moonNorm = moonColor * 30000.0f / max 30000.0f colMax
            return V4f(moonNorm * tex.XYZ, 1.0f)
        }
    let sunSpritePs (v : VertexSky) =
        fragment {
            let pos = v.pos
            let temp = (uniform.ProjTrafoInv * V4f(pos.X, pos.Y, 0.0f, 0.0f)).XY
            let dir = (uniform.ViewTrafoInv * V4f(temp.X, temp.Y, -1.0f, 0.0f)).XYZ
            let vdir = dir.Normalized
            let sunSizeAng = uniform.SunSize
            let sunDir = uniform.SunDirection
            let viewAng = acos (min (Vec.dot vdir sunDir) 1.0f)
            let coronaAng = uniform.SunSize + 4.0f * Vec.Dot(V2f(0.5f, 0.5f), borderPixelSize * uniform.CameraFov / V2f(uniform.ViewportSize))
            if viewAng > coronaAng then
                discard()
            let alpha =
                if viewAng <= sunSizeAng then
                    1.0f 
                else 
                    pow (abs (1.0f - (viewAng - sunSizeAng) / (coronaAng - sunSizeAng))) sunCoronaExponent
            let colMax = max uniform.SunColor.X (max uniform.SunColor.Y uniform.SunColor.Z)
            let sunNorm = uniform.SunColor * 30000.0f / max 30000.0f colMax
            return V4f(sunNorm * alpha, 1.0f)
        }
    let planetSpriteGs (v : Point<VertexPlanet>) = 
        triangle {
            let viewDir = (uniform.ViewTrafo * V4f(uniform.PlanetDir, 0.0f)).XYZ
            if viewDir.Z < 0.0f then
                let proj = V3f(viewDir.X * uniform.ProjTrafo.M00, viewDir.Y * uniform.ProjTrafo.M11, viewDir.Z * uniform.ProjTrafo.M22)
                let projDir = proj.XY / proj.Z
                let ar = float32 uniform.ViewportSize.X / float32 uniform.ViewportSize.Y
                let minSz = 1.0f / V2f(uniform.ViewportSize)
                let sizeX = max minSz.X uniform.PlanetSize
                let sizeY = max minSz.Y (uniform.PlanetSize * ar)
                let actualSize = uniform.PlanetSize * uniform.PlanetSize * ConstantF.PiHalf
                let adj = min 1.0f (actualSize / (sizeX * sizeX))
                let uvClamp = (minSz - V2f(sizeX, sizeY)) * V2f(uniform.ViewportSize) + (1.0f - ConstantF.Sqrt2Half)
                let uvClamp = V2f(max uvClamp.X 0.0f, max uvClamp.Y 0.0f)
                for i in 0..3 do
                    let x = float32 (i &&& 0x1)
                    let y = float32 (i >>> 1)
                    let uv = V2f(x - 0.5f, y - 0.5f) * 2.0f
                    let extend = uv * V2f(sizeX, sizeY)
                    let pos = V4f(projDir.X + extend.X, projDir.Y + extend.Y, 1.0f, 1.0f)
                    let uvBias = V2f(float32 (sign uv.X) * uvClamp.X, float32 (sign uv.Y) * uvClamp.Y)
                    yield { p = pos; c = V4f(uniform.PlanetColor * adj, 1.0f); uv = uv - uvBias }
        }
    let planetSpritePs (v : VertexPlanet) = 
        fragment {
            if v.uv.LengthSquared > 1.0f then discard()
            return v.c
        }
    [<GLSLIntrinsic("exp({0})")>]
    let Exp<'a when 'a :> IVector> (a : 'a) : 'a = onlyInShaderCode ""
    
    [<GLSLIntrinsic("mix({0},{1},{2})")>]
    let LerpV<'a when 'a :> IVector> (a : 'a) (b : 'a) (s : 'a) : 'a = onlyInShaderCode ""

    [<GLSLIntrinsic("lessThanEqual({0},{1})")>]
    let LessThanEqual<'a when 'a :> IVector> (a : 'a) (b : 'a) : 'a = onlyInShaderCode ""

    [<ReflectedDefinition>]
    let private LinearToGammaSRGBVec(c : V3f) : V3f =
        let rTrue = c * 12.92f
        let rFalse = 1.055f * V3f(pow c (V3f(1.0f / 2.4f))) - 0.055f
        LerpV rFalse rTrue (LessThanEqual c (V3f 0.0031308f))
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
                                1.001f - (2.0f / (2.0f + log(avgLum + 1.0f) / log(10.0f)))
                              else
                                uniform.MiddleGray
                    key / avgLum
            let color = scene * ev
            let color = color / (1.0f + color)
            let color = LinearToGammaSRGBVec color
            return V4f(color, 1.0f)
        }
    let magBoost (v : Effects.Vertex) =
        vertex {
            let boost = uniform.MagBoost
            let intScalePerMag = 2.511f
            let scale = pow intScalePerMag boost
            return { v with c = V4f(v.c.XYZ * scale, v.c.W) }
        }
    let equatorTrafo (v : Effects.Vertex) =
        vertex {
            let p = v.pos
            let p = uniform.ModelTrafo * V4f(p.XYZ, 0.0f)
            let p = uniform.ViewTrafo * V4f(p.XYZ, 0.0f)
            let p = uniform.ProjTrafo * V4f(p.XYZ, 1.0f)
            return { v with pos = p }
        }
    let starTrafo (v : VertexStar) =
        vertex {
            let dir = v.p.XYZ
            let direarth = (uniform.ModelTrafo * V4f(dir, 0.0f)).XYZ
            let vdir = (uniform.ViewTrafo * V4f(direarth, 0.0f)).XYZ
            let p = uniform.ProjTrafo * V4f(vdir, 1.0f)
            let pp = if p.Z <= 0.0f then V2f(-666.0f) else p.XY / p.W
            let fovRad = uniform.CameraFov
            let vpz = uniform.ViewportSize
            let sunDiameterRad = 0.533f * ConstantF.RadiansPerDegree
            let sunRadPx = (sunDiameterRad / fovRad.X * float32 vpz.X) * 0.5f
            let sunPixels = sunRadPx * sunRadPx * ConstantF.Pi
            let c = V4f(v.c.XYZ * sunPixels, 1.0f)
            return { p = V4f(pp.X, pp.Y, 1.0f, 1.0f); c = c; s = 1.0f }
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