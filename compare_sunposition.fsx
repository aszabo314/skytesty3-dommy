#r "nuget: Aardvark.Physics.Sky"
#r "nuget: Aardvark.Base"

open System
open Aardvark.Base
open Aardvark.Physics.Sky


let sunPositionCompute32 (longitudeInDegrees: float32) (latitudeInDegrees: float32) (secondsSinceStartOfYear: float32) (year: int) : V3f =
            
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

let sunPositionCompute (longitudeInDegrees: float) (latitudeInDegrees: float) (secondsSinceStartOfYear: float) (year: int) : V3d =
    
    // Constants
    let pi = 3.14159265358979323846
    let piHalf = pi / 2.0
    let piTimesTwo = pi * 2.0
    let radiansPerDegree = pi / 180.0
    let au = 149597870700.0 // Astronomical Unit in meters
    let j2000 = 2451545.0 // Julian day of 1st January 2000 12:00 (J2000)
    let julianCenturiesPerDay = 1.0 / 36525.0
    
    // Convert year/seconds to Julian day
    // Calculate Julian day using standard formula for Gregorian calendar
    // JD for Jan 1 of given year at 00:00:00 UTC
    let year64 = int64 year
    let a = (14L - 1L) / 12L
    let y = year64 + 4800L - a
    let m = 1L + 12L * a - 3L
    let jdJan1 = 1L + (153L * m + 2L) / 5L + 365L * y + y / 4L - y / 100L + y / 400L - 32045L
    let jdJan1Float = float jdJan1 - 0.5 // Julian day starts at noon, so Jan 1 00:00 is JD - 0.5
    
    // Add the elapsed time
    let daysSinceStartOfYear = secondsSinceStartOfYear / 86400.0
    let jd = jdJan1Float + daysSinceStartOfYear
    
    // Earth orbital parameters
    let earthEccentricity = 0.01671
    let earthPerihelion = 102.9373 // degrees
    let earthMeanAnomalyAtJ2000 = 357.5291 * radiansPerDegree
    let earthMeanMotion = 0.985608 * radiansPerDegree // degrees per day
    let earthSemiLatusRectum = 0.99972 // a(1 - e²) in AU
    
    // Earth anomaly approximation coefficients (degrees)
    let earthAnomalyCoeffs = [| 1.9148; 0.0200; 0.0003 |]
    
    // 1. Calculate Mean Anomaly
    let meanAnomaly = earthMeanAnomalyAtJ2000 + earthMeanMotion * (jd - j2000)
    
    // 2. Approximate True Anomaly (Equation of Center)
    let mutable mi = meanAnomaly
    let mutable equationOfCenter = 0.0
    for c in earthAnomalyCoeffs do
        equationOfCenter <- equationOfCenter + c * sin(mi)
        mi <- mi + meanAnomaly
    let trueAnomaly = meanAnomaly + equationOfCenter * radiansPerDegree
    
    // 3. Calculate ecliptic longitude (lambda)
    let lambda = trueAnomaly + (earthPerihelion + 180.0) * radiansPerDegree
    
    // 4. Calculate Earth's obliquity (epsilon) using Astronomical Almanac 2010 approximation
    let t = (jd - j2000) * julianCenturiesPerDay
    let t2 = t * t
    let t3 = t2 * t
    let t4 = t2 * t2
    let t5 = t3 * t2
    let epsilonJ2000Arcsec = 23.0 * 3600.0 + 26.0 * 60.0 + 21.406
    let epsilonArcsec = epsilonJ2000Arcsec - 46.836769 * t - 0.0001831 * t2 + 0.00200340 * t3 - 5.76e-7 * t4 - 4.34e-8 * t5
    let epsilon = epsilonArcsec / 3600.0 * radiansPerDegree
    
    // 5. Transform geocentric ecliptic to equatorial coordinates
    // Declination (delta)
    let beta = 0.0 // For the sun, beta (ecliptic latitude) is approximately 0
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
    let theta = piTimesTwo * (0.7790572732640 + 1.002737811911354* (jd - j2000))
    
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
    let dir = V3d(x, y, z)
    Vec.normalize dir

// Reference CPU implementation
type GeoInfo = 
    {
        gpsLat    : float
        gpsLong   : float 
        timeZone  : int
        time      : DateTime
    }

let cpuSunDirection (geo: GeoInfo) =
    let struct(phiTheta, _) = SunPosition.Compute(geo.time, geo.timeZone, geo.gpsLong, geo.gpsLat)
    Sky.V3dFromPhiTheta(phiTheta.Phi, phiTheta.Theta)

// GPU or alternative implementation placeholder:
// Replace this with your actual GPU/alternative implementation
let gpuSunDirection (geo: GeoInfo) =
    let utcTime = geo.time.AddHours(float -geo.timeZone)
    sunPositionCompute
        geo.gpsLong
        geo.gpsLat
        ((utcTime - DateTime(utcTime.Year, 1, 1)).TotalSeconds)
        utcTime.Year
    // sunPositionCompute32
    //     (float32 geo.gpsLong)
    //     (float32 geo.gpsLat)
    //     (float32 (utcTime - DateTime(utcTime.Year, 1, 1)).TotalSeconds)
    //     utcTime.Year

// Comparison function
let compareSunDirections geo =
    let cpuDir = cpuSunDirection geo
    let gpuDir = gpuSunDirection geo
    let angle = Vec.AngleBetween(cpuDir, V3d gpuDir) * 180.0 / Constant.Pi
    printfn "CPU: %A" cpuDir
    printfn "GPU: %A" gpuDir
    printfn "Angle difference: %.6f degrees" angle

// Example test cases
let testCases = [
    { gpsLat = 48.20849; gpsLong = 16.37208; timeZone = 2; time = DateTime(2011,7,11,8,0,0) }
    { gpsLat = 0.0; gpsLong = 0.0; timeZone = 0; time = DateTime(2020,1,1,12,0,0) }
    { gpsLat = 51.5; gpsLong = -0.1; timeZone = 0; time = DateTime(2022,6,21,12,0,0) }
    // Additional test cases
    { gpsLat = 40.7128; gpsLong = -74.0060; timeZone = -5; time = DateTime(2023,3,21,6,0,0) } // New York, spring equinox sunrise
    { gpsLat = 40.7128; gpsLong = -74.0060; timeZone = -5; time = DateTime(2023,6,21,12,0,0) } // New York, summer solstice noon
    { gpsLat = 40.7128; gpsLong = -74.0060; timeZone = -5; time = DateTime(2023,12,21,12,0,0) } // New York, winter solstice noon
    { gpsLat = -33.8688; gpsLong = 151.2093; timeZone = 10; time = DateTime(2023,12,21,18,0,0) } // Sydney, summer solstice sunset
    { gpsLat = -33.8688; gpsLong = 151.2093; timeZone = 10; time = DateTime(2023,6,21,7,0,0) } // Sydney, winter solstice sunrise
    { gpsLat = 90.0; gpsLong = 0.0; timeZone = 0; time = DateTime(2023,6,21,12,0,0) } // North Pole, summer solstice
    { gpsLat = -90.0; gpsLong = 0.0; timeZone = 0; time = DateTime(2023,12,21,12,0,0) } // South Pole, summer solstice
    { gpsLat = 35.6895; gpsLong = 139.6917; timeZone = 9; time = DateTime(2023,1,1,6,0,0) } // Tokyo, New Year sunrise
    { gpsLat = 55.7558; gpsLong = 37.6173; timeZone = 3; time = DateTime(2023,9,23,18,0,0) } // Moscow, autumn equinox sunset
    { gpsLat = 34.0522; gpsLong = -118.2437; timeZone = -8; time = DateTime(2023,7,4,12,0,0) } // Los Angeles, Independence Day noon
    { gpsLat = 19.4326; gpsLong = -99.1332; timeZone = -6; time = DateTime(2023,5,5,7,0,0) } // Mexico City, sunrise
    { gpsLat = 28.6139; gpsLong = 77.2090; timeZone = 5; time = DateTime(2023,10,2,18,0,0) } // Delhi, sunset
]

testCases |> List.iter compareSunDirections
