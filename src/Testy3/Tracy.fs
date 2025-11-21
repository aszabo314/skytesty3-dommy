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
                lerp c0 c1 t
                //if t>0.5f then c1 else c0
        
    module AccumulateShader =
        open Heat
        /// <summary>
        /// Computes the spherical coordinates (theta, phi) of the sun and its distance in meters.
        /// Based on https://www.aa.quae.nl/en/reken/zonpositie.html by Dr Louis Strous
        /// Accuracy: ~1°
        /// </summary>
        /// <param name="longitudeInDegrees">Longitude GPS coordinate in degrees east</param>
        /// <param name="latitudeInDegrees">Latitude GPS coordinate in degrees north</param>
        /// <param name="secondsSinceStartOfYear">Seconds elapsed since start of the year (Jan 1, 00:00:00 UTC)</param>
        /// <param name="year">Absolute year (e.g., 2024)</param>
        /// <returns>Tuple of (theta, phi, distance) where theta and phi are in radians, distance in meters</returns>
        [<ReflectedDefinition>]
        let sunDirection (longitudeInDegrees: float32) (latitudeInDegrees: float32) (secondsSinceStartOfYear: float32) (year: int) : V3f =
            
            // Constants
            let pi = 3.14159265358979323846f
            let piHalf = pi / 2.0f
            let piTimesTwo = pi * 2.0f
            let radiansPerDegree = pi / 180.0f
            let au = 149597870700.0f // Astronomical Unit in meters
            let j2000 = 2451545.0f // Julian day of 1st January 2000 12:00 (J2000)
            let julianCenturiesPerDay = 1.0f / 36525.0f
            
            // Convert year/seconds to Julian day
            // Calculate Julian day using standard formula for Gregorian calendar
            // JD for Jan 1 of given year at 00:00:00 UTC
            let year64 = int64 year
            let a = (14L - 1L) / 12L
            let y = year64 + 4800L - a
            let m = 1L + 12L * a - 3L
            let jdJan1 = 1L + (153L * m + 2L) / 5L + 365L * y + y / 4L - y / 100L + y / 400L - 32045L
            let jdJan1float32 = float32 jdJan1 - 0.5f // Julian day starts at noon, so Jan 1 00:00 is JD - 0.5f
            
            // Add the elapsed time
            let daysSinceStartOfYear = secondsSinceStartOfYear / 86400.0f
            let jd = jdJan1float32 + daysSinceStartOfYear
            
            // Earth orbital parameters
            let earthEccentricity = 0.01671f
            let earthPerihelion = 102.9373f // degrees
            let earthMeanAnomalyAtJ2000 = 357.5291f * radiansPerDegree
            let earthMeanMotion = 0.985608f * radiansPerDegree // degrees per day
            let earthSemiLatusRectum = 0.99972f // a(1 - e²) in AU
            
            // Earth anomaly approximation coefficients (degrees)
            let earthAnomalyCoeffs = [| 1.9148f; 0.0200f; 0.0003f |]
            
            // 1. Calculate Mean Anomaly
            let meanAnomaly = earthMeanAnomalyAtJ2000 + earthMeanMotion * (jd - j2000)
            
            // 2. Approximate True Anomaly (Equation of Center)
            let mutable mi = meanAnomaly
            let mutable equationOfCenter = 0.0f
            for c in earthAnomalyCoeffs do
                equationOfCenter <- equationOfCenter + c * sin(mi)
                mi <- mi + meanAnomaly
            let trueAnomaly = meanAnomaly + equationOfCenter * radiansPerDegree
            
            // 3. Calculate ecliptic longitude (lambda)
            let lambda = trueAnomaly + (earthPerihelion + 180.0f) * radiansPerDegree
            
            // 4. Calculate Earth's obliquity (epsilon) using Astronomical Almanac 2010 approximation
            let t = (jd - j2000) * julianCenturiesPerDay
            let t2 = t * t
            let t3 = t2 * t
            let t4 = t2 * t2
            let t5 = t3 * t2
            let epsilonJ2000Arcsec = 23.0f * 3600.0f + 26.0f * 60.0f + 21.406f
            let epsilonArcsec = epsilonJ2000Arcsec - 46.836769f * t - 0.0001831f * t2 + 0.00200340f * t3 - 5.76e-7f * t4 - 4.34e-8f * t5
            let epsilon = epsilonArcsec / 3600.0f * radiansPerDegree
            
            // 5. Transform geocentric ecliptic to equatorial coordinates
            // Declination (delta)
            let beta = 0.0f // For the sun, beta (ecliptic latitude) is approximately 0
            let sinBeta = sin(beta)
            let cosBeta = cos(beta)
            let sinLambda = sin(lambda)
            let cosLambda = cos(lambda)
            let sinEpsilon = sin(epsilon)
            let cosEpsilon = cos(epsilon)
            
            let delta = asin(sinBeta * cosEpsilon + cosBeta * sinEpsilon * sinLambda)
            
            // Right ascension (alpha)
            let alpha = atan2 (sinLambda * cosEpsilon - tan(beta) * sinEpsilon) cosLambda
            
            // 6. Calculate sidereal time
            let theta = piTimesTwo * (0.7790572732640f + 1.002737811911354f* (jd - j2000))
            
            // 7. Convert to observer coordinates
            let phi = latitudeInDegrees * radiansPerDegree
            let lw = -longitudeInDegrees * radiansPerDegree // convert East to West
            
            let thetaLocal = theta - lw
            let hourAngle = thetaLocal - alpha
            
            // 8. Calculate height and azimuth
            let sinPhi = sin(phi)
            let cosPhi = cos(phi)
            let sinDelta = sin(delta)
            let cosDelta = cos(delta)
            let sinH = sin(hourAngle)
            let cosH = cos(hourAngle)
            
            let height = asin(sinPhi * sinDelta + cosPhi * cosDelta * cosH)
            let azimuth = atan2 sinH (cosH * sinPhi - tan(delta) * cosPhi)
            
            // 9. Convert to spherical coordinates (theta, phi)
            let thetaSphere = piHalf - height
            let phiSphere = azimuth
            
            // Convert spherical (theta, phi) to Cartesian direction
            let x = -sin thetaSphere * sin phiSphere
            let y = -sin thetaSphere * cos phiSphere
            let z = cos thetaSphere
            let dir = V3f(x, y, z)
            Vec.normalize dir

        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba32f> = uniform?OutputBuffer
            member x.SunDirections : V4f[] = uniform?StorageBuffer?SunDirections
            member x.NumSunDirections : int = uniform?NumSunDirections
            member x.NormalizationFactor : float32 = uniform?NormalizationFactor
            member x.PlaneTrafo : M44f = uniform?PlaneTrafo
            member x.PlaneTrafoInvTransposed : M44f = uniform?PlaneTrafoInvTransposed
            member x.Efficiency : float32 = uniform?Efficiency
            member x.TimeStep : float32 = uniform?TimeStep
            member x.NormalizeMax : float32 = uniform?NormalizeMax
            member x.TotalTimeSeconds : float32 = uniform?TotalTimeSeconds
    
        let mainScene =
            scene {
                accelerationStructure uniform?MainScene
            }
        type ShadowPayload =
            {
                lightAcc : float32
            }
        [<ReflectedDefinition>]
        let illum (pos : V3f) (normal : V3f) =
            let mutable acc = 0.0f
            for i in 0 .. uniform.NumSunDirections - 1 do
                let dir = uniform.SunDirections.[i].XYZ
                let dotty = Vec.dot normal dir
                if dotty > 0.1f then
                    let res =
                        mainScene.TraceRay<ShadowPayload>(
                            pos, dir,
                            ray = RayIds.ShadowRay,
                            miss = RayIds.ShadowRayMiss,
                            cullMask = 1
                        )
                    if res.lightAcc > 0.5f then
                        acc <- acc + dotty
            acc
        [<ReflectedDefinition>]
        let normalizedIllum (pos : V3f) (normal : V3f) =
            let acc = illum pos normal
            let avgWattPerSm = acc * (1025.0f * uniform.Efficiency * uniform.TimeStep / uniform.TotalTimeSeconds)
            clamp 0.0f 1.0f (float32 avgWattPerSm / uniform.NormalizeMax)
        let rgenMain (input : RayGenerationInput) =
            raygen {
                let tc = (V2f input.work.id.XY + V2f.Half) / V2f input.work.size.XY
                let ndc = tc * 2.0f - V2f.II
                let wp = uniform.PlaneTrafo * V4f(ndc, 0.0f, 1.0f)
                let wn = (uniform.PlaneTrafoInvTransposed * V4f(0.0f, 0.0f, 1.0f, 0.0f)).XYZ |> Vec.normalize
                let origin = wp.XYZ / wp.W
                let color = heat (normalizedIllum origin wn)
                uniform.OutputBuffer.[input.work.id.XY] <- V4f(color.XYZ,1.0f)
            }
        let chit (input : RayHitInput<ShadowPayload>) =
            closestHit {
                return { unchanged with lightAcc = 0.0f }
            }
    
        let missSky (input : RayMissInput) =
            miss {
                return { unchanged with lightAcc = 1.0f }
            }
        let chitShadow (input : RayHitInput<ShadowPayload>) =
            closestHit {
                return { unchanged with lightAcc = 0.0f }
            }
        let private hitGroupModel =
            hitgroup {
                closestHit chit
                closestHit RayIds.ShadowRay chitShadow
            }
        let chitQuad (input : RayHitInput<ShadowPayload>) =
            closestHit {
                return {unchanged with lightAcc = 0.0f}
            }
        let private hitGroupQuad =
            hitgroup {
                closestHit chitQuad
                closestHit RayIds.ShadowRay chitShadow
            }
        let missShadow (input : RayMissInput) =
            miss {
                return {unchanged with lightAcc = 1.0f}
            }
        let main =
            raytracingEffect {
                raygen rgenMain
                miss missSky
                miss RayIds.ShadowRayMiss missShadow
                hitgroup HitGroup.Model hitGroupModel
                hitgroup HitGroup.Quad hitGroupQuad
            }
    
    module ForwardShaders2 =
        open FShade
        open AccumulateShader
        type UniformScope with
            member x.OutputBuffer : Image2d<Formats.rgba8> = uniform?OutputBuffer
            member x.PickBuffer : Image2d<Formats.rgba32f> = uniform?PickBuffer
            member x.Normals        : V4f[]  = uniform?StorageBuffer?Normals
            member x.TextureCoords   : V2f[]  = uniform?StorageBuffer?DiffuseColorCoordinates
            member x.GlobalRenderingMode : bool = uniform?GlobalRenderingMode
            member x.SecondsSinceStartOfYear : float32 = uniform?SecondsSinceStartOfYear
            member x.Year : int = uniform?Year
            
            member x.LongitudeInDegrees : float32 = uniform?LongitudeInDegrees
            member x.LatitudeInDegrees : float32 = uniform?LatitudeInDegrees

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
        let private accumTexture =
            sampler2d {
                texture uniform?AccumTexture
                filter Filter.MinMagPointMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }
        type Payload =
            {
                color : V4f
                normal : V3f
                depth : float32
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
                uniform.PickBuffer.[input.work.id.XY] <- V4f(res.normal, res.depth)
            }
        let chit (input : RayHitInput<Payload>) =
            closestHit {
                let info = TraceGeometryInfo.ofRayHit input
                let indices = TraceGeometryInfo.getIndices info input
                let normal = getNormal indices input
                    
                let pos = getPosition input
                let vpPos = uniform.ViewProjTrafo * V4f(pos, 1.0f)
                let depth = vpPos.Z / vpPos.W
                let color =
                    if uniform.GlobalRenderingMode then
                        let gray = normalizedIllum pos normal
                        Heat.heat gray
                    else
                        // let ambient = abs (Vec.dot normal uniform.SunDirections.[0].XYZ)
                        // V4f(0.1f * ambient + 0.9f * (illum pos normal) * V3f.III, 1.0f)                
                        let dir =
                            sunDirection
                                uniform.LongitudeInDegrees
                                uniform.LatitudeInDegrees
                                uniform.SecondsSinceStartOfYear
                                uniform.Year
                        let dotty = Vec.dot normal dir
                        let illum =
                            if dotty > 0.1f then
                                let res =
                                    mainScene.TraceRay<ShadowPayload>(
                                        pos, dir,
                                        ray = RayIds.ShadowRay,
                                        miss = RayIds.ShadowRayMiss,
                                        cullMask = 1
                                    )
                                if res.lightAcc > 0.5f then
                                    dotty
                                else
                                    0.0f
                            else
                                0.0f
                        let lerpy = 0.1f * dotty + 0.9f * illum
                        V4f(lerpy, lerpy, lerpy, 1.0f)
                
                let vn = uniform.ViewTrafo * V4f(normal, 0.0f) |> Vec.xyz |> Vec.normalize
                return { unchanged with color = color; normal = vn; depth = depth }
            }
        let missGlobal (input : RayMissInput) =
            miss {
                return { unchanged with color = V4f.Zero; depth = 1.0f; normal = V3f.Zero }
            }
        let chitShadow (input : RayHitInput<ShadowPayload>) =
            closestHit {
                return { unchanged with lightAcc = 0.0f }
            }
        let missShadow (input : RayMissInput) =
            miss {
                return { unchanged with lightAcc = 1.0f }
            }
        let chitQuad (input : RayHitInput<Payload>) =
            closestHit {
                let info = TraceGeometryInfo.ofRayHit input
                let indices = TraceGeometryInfo.getIndices info input
                let normal = getNormal indices input
                let vn = uniform.ViewTrafo * V4f(normal, 0.0f) |> Vec.xyz |> Vec.normalize
                let pos = getPosition input
                let vpPos = uniform.ViewProjTrafo * V4f(pos, 1.0f)
                let depth = vpPos.Z / vpPos.W
                let tc = getTextureCoords indices input
                let color = accumTexture.Sample(tc)
                return { unchanged with color = color; normal = vn; depth = depth }
            }
        let private hitGroupModel =
            hitgroup {
                closestHit chit
                closestHit RayIds.ShadowRay chitShadow
            }
        let private hitGroupQuad =
            hitgroup {
                closestHit chitQuad
                closestHit RayIds.ShadowRay chitShadow
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