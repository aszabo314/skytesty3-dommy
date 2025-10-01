namespace Testy3

open Aardvark.Base
open System
open Adaptify
open FSharp.Data.Adaptive
open Aardvark.Dom
open Aardvark.Physics.Sky

type GeoInfo = 
    {
        gpsLat    : float
        gpsLong   : float 
        timeZone  : int
        time      : DateTime
    } with 
        member x.SunPosition : struct(SphericalCoordinate * float) =
            SunPosition.Compute(x.time, x.timeZone, x.gpsLong, x.gpsLat)
        member x.SunDirection : V3d =
            let struct(phiTheta, _) = x.SunPosition
            Sky.V3dFromPhiTheta(phiTheta.Phi, phiTheta.Theta)
        member x.MoonPosition : struct(SphericalCoordinate * float) = 
            MoonPosition.Compute(x.time, x.timeZone, x.gpsLong, x.gpsLat)
        member x.MoonDirection : V3d =
            let struct(moonPos, _) = x.MoonPosition
            Sky.V3dFromPhiTheta(moonPos.Phi, moonPos.Theta)
        member x.JulianDayUTC : float = 
            x.time.ComputeJulianDayUTC(float x.timeZone)
            
type SkyType = 
    | Preetham 
    | CIE 
    | HosekWilkie
    
type SkyInfo = 
    {
        skyType         : SkyType
        turbidity       : float // [1.9, 10]
        cieType         : CIESkyType
        lightPollution  : float
        res             : int
    }

type ExposureMode = 
    | Manual=0 
    | MiddleGray=1 
    | Auto=2

[<ModelType>]
type Model =
    {
        //cameraState     : CameraControllerState
        
        skyInfo         : SkyInfo
        skyFov          :float
        geoInfo         : GeoInfo
        exposureMode    : ExposureMode
        exposure        : float
        planetScale     : float
        magBoost        : float
        key             : float
    }

module Model =
    let initial =
        {
            //cameraState = FreeFlyController.initial
            skyInfo = {
                skyType = Preetham
                turbidity = 1.9
                cieType = CIESkyType.ClearSky1
                lightPollution = 50.0
                res = 256
            }
            geoInfo = {
                //Wien, 11.Juli 2011
                gpsLat = 48.20849
                gpsLong = 16.37208
                timeZone = 2
                time = System.DateTime(2011, 7, 11, 8,0,0)
            }
            exposureMode = ExposureMode.MiddleGray
            exposure = 1.0
            key = 0.18
            planetScale = 1.0
            magBoost = 10.0
            skyFov = 80.0
        }