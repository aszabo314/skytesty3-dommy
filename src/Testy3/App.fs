namespace Testy3

open Aardvark.SceneGraph.SgFSharp.Sg
open FSharp.Data.Adaptive
open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Dom
open Aardvark.Dom.Utilities

type Message =
    | SetDateTime of DateTime
    | Nop
module App =
    let update (env : Env<Message>) (m : Model) (msg : Message)  =
        match msg with
        | Nop ->
            m
        | SetDateTime t ->
            { m with geoInfo.time = t }
            
    let view (env : Env<Message>) (m : AdaptiveModel) =
        let mutable down = false
        body {
            Style [
                Css.Position "fixed"
                Css.Left "0px"
                Css.Top "0px"
                Css.Width "100%"
                Css.Height "100%"
            ]
            
            renderControl {
                Samples 4
                Quality 50
                TabIndex 0
                Style [
                    Css.Position "fixed"
                    Css.Left "0px"
                    Css.Top "0px"
                    Css.Width "100%"
                    Css.Height "100%"
                ]
                let! viewTrafo =
                    SimpleFreeFlyController {
                        Location = V3d(3,4,5)
                        LookAt = V3d.Zero
                        Sky = V3d.OOI
                    }
                let! info = RenderControl.Info
                let projTrafo = info.ViewportSize |> AVal.map (fun s -> Frustum.perspective 90.0 0.3 300.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                Sg.Proj projTrafo
                
                sg {
                    Sg.NoEvents
                    Sg.sg m viewTrafo projTrafo info.Runtime info.ViewportSize
                }
            }
            ()
        }
        //
        // body [] [
        //     render
        //     div [
        //         style "position: fixed; left: 20px; top: 20px; width: 100px; height: 100px; background: rgba(0,0,0,0.3)"
        //         onCapturedPointerDown None (fun _ _ _ -> down <- true; Nop)
        //         onCapturedPointerUp None (fun _ _ _ -> down <- false; Nop)
        //         onCapturedPointerMove None (fun a b ->
        //             if down then
        //                 let r =  V2d b / V2d(100.0, 100.0)
        //                 let r =
        //                     let x = (1.0 + (r.X % 1.0)) % 1.0
        //                     let y = (1.0 + (r.Y % 1.0)) % 1.0
        //                     V2d(x, y)
        //                 let date =
        //                     DateTime(2021, 1, 1, 0,0,0) +
        //                     TimeSpan.FromDays(floor (365.0 * r.X)) +
        //                     TimeSpan.FromSeconds(floor (24.0 * 60.0 * 60.0 * r.Y))
        //                 SetDateTime date
        //             else
        //                 Nop
        //         )
        //         
        //     ] [
        //         let att =
        //             m.geoInfo |> AMap.bind (fun gi ->
        //                 let t = gi.time
        //                 let timeOfDay = t - DateTime(t.Year, t.Month, t.Day, 0,0,0) 
        //                 let y = timeOfDay.TotalSeconds / (24.0 * 60.0 * 60.0)
        //                 let timeOfYear = t - DateTime(t.Year, 1, 1, 0,0,0) 
        //                 let x = timeOfYear.TotalDays / 365.0
        //                 AMap.ofList [
        //                     style $"pointer-events: none; top: %.3f{y * 100.0}%%; left: %.3f{x * 100.0}%%; width: 10px; height: 10px; background: rgba(255,0,0,0.5); position: absolute;"
        //                 ]
        //             ) |> AttributeMap.ofAMap
        //         Incremental.div att (AList.empty)
        //         div [style "pointer-events:none; user-select:none"] [Incremental.text (m.geoInfo |> AVal.map (fun gi -> $"Time: {gi.time}" ))]
        //     ]
        // ]

    let app : App<Model, AdaptiveModel, Message> =
        {
            initial = Model.initial
            update = update
            view = view
            unpersist = Adaptify.Unpersist.instance
        }