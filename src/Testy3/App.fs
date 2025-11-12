namespace Testy3

open FSharp.Data.Adaptive
open System
open Aardvark.Base
open Aardvark.Rendering
open Aardvark.Dom
open Aardvark.Dom.Utilities
open Testy3.Styles

module App =
    let update (env : Env<Message>) (m : Model) (msg : Message)  =
        match msg with
        | Nop ->
            m
        | SetDateTime t ->
            { m with geoInfo.time = t }
        | SetMagBoost value ->
            { m with magBoost = value }
        | SetExposure value ->
            { m with exposure = value }
        | SetExposureMode mode ->
            { m with exposureMode = mode }
           
    let view (moonTexture : ITexture) (env : Env<Message>) (m : AdaptiveModel) =
        let mutable down = false
        let timePicker =
            let redThing =
                div {
                    m.geoInfo |> AVal.map (fun gi -> 
                        let t = gi.time
                        let timeOfDay = t - System.DateTime(t.Year, t.Month, t.Day, 0,0,0) 
                        let y = timeOfDay.TotalSeconds / (24.0 * 60.0 * 60.0)
                        let timeOfYear = t - System.DateTime(t.Year, 1, 1, 0,0,0) 
                        let x = timeOfYear.TotalDays / 365.0
                        Style [
                            PointerEvents "none"
                            Top $"%.3f{y * 100.0}%%"
                            Left $"%.3f{x * 100.0}%%"
                            Width "10px"
                            Height "10px"
                            BackgroundColor "rgba(255,0,0,0.5)"
                            Position "absolute"
                        ]
                    )
                }
            let text =
                div {
                    Style [
                        PointerEvents "none"
                        UserSelect "none"
                    ]
                    m.geoInfo |> AVal.map (fun gi -> $"{gi.time}")
                }
            let container =
                div {
                    Style [
                        Position "fixed"
                        Left "20px"
                        Top "20px"
                        Width "100px"
                        Height "100px"
                        BackgroundColor "rgba(0,0,0,0.3)"
                    ]
                    Dom.OnPointerDown((fun _ -> down <- true),pointerCapture=true)
                    Dom.OnPointerUp((fun _ -> down <- false),pointerCapture=true)
                    Dom.OnPointerMove(fun ev ->
                        if down then
                            let r = (V2d ev.ClientPosition - ev.ClientRect.Min)  / ev.ClientRect.Size
                            let r =
                                let x = (1.0 + (r.X % 1.0)) % 1.0
                                let y = (1.0 + (r.Y % 1.0)) % 1.0
                                V2d(x, y)
                            let date =
                                System.DateTime(2025, 1, 1, 0,0,0) +
                                TimeSpan.FromDays(floor (365.0 * r.X)) +
                                TimeSpan.FromSeconds(floor (24.0 * 60.0 * 60.0 * r.Y))
                            env.Emit [SetDateTime date]
                    )
                    text
                    redThing
                }
            div {
                container
            }
        let magBoostSlider = topleftslider 140 "MagBoost" 1.0 20.0 0.1 m.magBoost (fun value -> env.Emit [SetMagBoost value])
        let exposureSlider = topleftslider 220 "Exposure" 0.01 1.0 0.01 m.exposure (fun value -> env.Emit [SetExposure value])
        let exposureModeRadio =
            div {
                Style [
                    Position "fixed"
                    Left "20px"
                    Top "330px"
                    Width "200px"
                    BackgroundColor "rgba(0,0,0,0.3)"
                    Padding "10px"
                    BorderRadius "5px"
                ]
                Styles.exposureModeRadioButton ExposureMode.Manual "Manual" m.exposureMode (fun msg -> env.Emit [msg])
                Styles.exposureModeRadioButton ExposureMode.MiddleGray "Middle Gray" m.exposureMode (fun msg -> env.Emit [msg])
                Styles.exposureModeRadioButton ExposureMode.Auto "Auto" m.exposureMode (fun msg -> env.Emit [msg])
            }
        body {
            Style Styles.fullscreen
            renderControl {
                Samples 1
                Quality 50
                TabIndex 0
                Style Styles.fullscreen
                let! viewTrafo =
                    SimpleFreeFlyController {
                        Location = V3d(3,4,5)
                        LookAt = V3d.Zero
                        Sky = V3d.OOI
                        Config = None
                    }
                let! info = RenderControl.Info
                let projTrafo = info.ViewportSize |> AVal.map (fun s -> Frustum.perspective 90.0 0.3 300.0 (float s.X / float s.Y) |> Frustum.projTrafo)
                Sg.Proj projTrafo
                sg {
                    Sg.NoEvents
                    Sg.sg m viewTrafo projTrafo info.Runtime info.ViewportSize moonTexture
                }
            }
            timePicker
            magBoostSlider
            exposureSlider
            exposureModeRadio
        }
    let app (moonTexture : ITexture) : App<Model, AdaptiveModel, Message> =
        {
            initial = Model.initial
            update = update
            view = view moonTexture
            unpersist = Adaptify.Unpersist.instance
        }