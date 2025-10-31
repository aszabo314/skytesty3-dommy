//db287158-bb32-1b67-dafe-128bd1da8893
//2dffffd2-32a9-faaa-43b2-f70fdf61287f
#nowarn "49" // upper case patterns
#nowarn "66" // upcast is unncecessary
#nowarn "1337" // internal types
#nowarn "1182" // value is unused
namespace rec Testy3

open System
open FSharp.Data.Adaptive
open Adaptify
open Testy3
[<System.Diagnostics.CodeAnalysis.SuppressMessage("NameConventions", "*")>]
type AdaptiveModel(value : Model) =
    let _skyInfo_ = FSharp.Data.Adaptive.cval(value.skyInfo)
    let _skyFov_ = FSharp.Data.Adaptive.cval(value.skyFov)
    let _geoInfo_ = FSharp.Data.Adaptive.cval(value.geoInfo)
    let _exposureMode_ = FSharp.Data.Adaptive.cval(value.exposureMode)
    let _exposure_ = FSharp.Data.Adaptive.cval(value.exposure)
    let _planetScale_ = FSharp.Data.Adaptive.cval(value.planetScale)
    let _magBoost_ = FSharp.Data.Adaptive.cval(value.magBoost)
    let _key_ = FSharp.Data.Adaptive.cval(value.key)
    let _starLinesVisible_ = FSharp.Data.Adaptive.cval(value.starLinesVisible)
    let mutable __value = value
    let __adaptive = FSharp.Data.Adaptive.AVal.custom((fun (token : FSharp.Data.Adaptive.AdaptiveToken) -> __value))
    static member Create(value : Model) = AdaptiveModel(value)
    static member Unpersist = Adaptify.Unpersist.create (fun (value : Model) -> AdaptiveModel(value)) (fun (adaptive : AdaptiveModel) (value : Model) -> adaptive.Update(value))
    member __.Update(value : Model) =
        if Microsoft.FSharp.Core.Operators.not((FSharp.Data.Adaptive.ShallowEqualityComparer<Model>.ShallowEquals(value, __value))) then
            __value <- value
            __adaptive.MarkOutdated()
            _skyInfo_.Value <- value.skyInfo
            _skyFov_.Value <- value.skyFov
            _geoInfo_.Value <- value.geoInfo
            _exposureMode_.Value <- value.exposureMode
            _exposure_.Value <- value.exposure
            _planetScale_.Value <- value.planetScale
            _magBoost_.Value <- value.magBoost
            _key_.Value <- value.key
            _starLinesVisible_.Value <- value.starLinesVisible
    member __.Current = __adaptive
    member __.skyInfo = _skyInfo_ :> FSharp.Data.Adaptive.aval<SkyInfo>
    member __.skyFov = _skyFov_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.geoInfo = _geoInfo_ :> FSharp.Data.Adaptive.aval<GeoInfo>
    member __.exposureMode = _exposureMode_ :> FSharp.Data.Adaptive.aval<ExposureMode>
    member __.exposure = _exposure_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.planetScale = _planetScale_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.magBoost = _magBoost_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.key = _key_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.float>
    member __.starLinesVisible = _starLinesVisible_ :> FSharp.Data.Adaptive.aval<Microsoft.FSharp.Core.bool>

