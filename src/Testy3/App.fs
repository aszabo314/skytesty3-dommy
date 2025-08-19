namespace testy2

open FSharp.Data.Adaptive
open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives

open testy2.Model

type Message =
    | CameraMessage of FreeFlyController.Message
    | SetDateTime of DateTime
    | Nop
module App =
    let update (m : Model) (msg : Message) =
        match msg with
        | Nop ->
            m
        | SetDateTime t ->
            { m with geoInfo.time = t }
        | CameraMessage msg ->
            { m with cameraState = FreeFlyController.update m.cameraState msg }
    let view (m : AdaptiveModel) =
        let frustum =
            Frustum.perspective 90.0 0.1 500.0 1.0 |> AVal.constant
        let att =
            [ style "position: fixed; left: 0; top: 0; width: 100%; height: 100%" ]
        let render =
            FreeFlyController.controlledControlWithClientValues
                m.cameraState
                CameraMessage
                frustum
                (AttributeMap.ofList att)
                RenderControlConfig.standard
                (fun cv -> Sg.sg m cv)

        let statustext =
            m.geoInfo |> AVal.map (fun gi ->
                    let t = gi.time
                    sprintf "Time: %A" t 
                )
        
        let mutable down = false
        body [] [
            render
            div [
                style "position: fixed; left: 20px; top: 20px; width: 100px; height: 100px; background: rgba(0,0,0,0.3)"
                onCapturedPointerDown None (fun _ _ _ -> down <- true; Nop)
                onCapturedPointerUp None (fun _ _ _ -> down <- false; Nop)
                onCapturedPointerMove None (fun a b ->
                    if down then
                        let r =  V2d b / V2d(100.0, 100.0)
                        let r =
                            let x = (1.0 + (r.X % 1.0)) % 1.0
                            let y = (1.0 + (r.Y % 1.0)) % 1.0
                            V2d(x, y)
                        let date =
                            DateTime(2021, 1, 1, 0,0,0) +
                            TimeSpan.FromDays(floor (365.0 * r.X)) +
                            TimeSpan.FromSeconds(floor (24.0 * 60.0 * 60.0 * r.Y))
                        SetDateTime date
                    else
                        Nop
                )
                
            ] [
                let att =
                    m.geoInfo |> AMap.bind (fun gi ->
                        let t = gi.time
                        let timeOfDay = t - DateTime(t.Year, t.Month, t.Day, 0,0,0) 
                        let y = timeOfDay.TotalSeconds / (24.0 * 60.0 * 60.0)
                        let timeOfYear = t - DateTime(t.Year, 1, 1, 0,0,0) 
                        let x = timeOfYear.TotalDays / 365.0
                        AMap.ofList [
                            style $"pointer-events: none; top: %.3f{y * 100.0}%%; left: %.3f{x * 100.0}%%; width: 10px; height: 10px; background: rgba(255,0,0,0.5); position: absolute;"
                        ]
                    ) |> AttributeMap.ofAMap
                Incremental.div att (AList.empty)
                div [style "pointer-events:none; user-select:none"] [Incremental.text statustext]
            ]
        ]

    let app : App<Model, AdaptiveModel, Message> =
        {
            initial = Model.initial
            update = update
            view = view
            threads = fun m -> m.cameraState |> FreeFlyController.threads |> ThreadPool.map CameraMessage
            unpersist = Unpersist.instance
        }