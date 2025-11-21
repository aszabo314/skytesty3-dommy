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
        | SetNormalizeMax value ->
            { m with normalizeMax = value }
        | SetGlobalRenderingMode value ->
            { m with globalRenderingMode = value }
        | SetTimeFrameDays value ->
            { m with timeframeDays = value }
        | SetSampleTimeHours value ->
            { m with sampletimeHours = value }
        
           
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
                        Width "100px"
                        Height "100px"
                        BackgroundColor "rgba(0,0,0,0.3)"
                        Position "Relative"
                        PointerEvents "auto"
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
                Style [
                    
                ]
                container
            }
        let slidersDiv =
            div {
                Style [
                    Left "20px"
                    BackgroundColor "rgba(0,0,0,0.3)"
                    Padding "10px"
                    BorderRadius "5px"
                    PointerEvents "auto"
                ]
                topleftslider "MagBoost" 1.0 20.0 0.1 m.magBoost (fun value -> env.Emit [SetMagBoost value])
                topleftslider "Exposure" 0.01 1.0 0.01 m.exposure (fun value -> env.Emit [SetExposure value])
                topleftslider "Normalize" 1.0 1000.0 0.1 m.normalizeMax (fun value -> env.Emit [SetNormalizeMax value])
            }
        let exposureModeRadio =
            div {
                Style [
                    Left "20px"
                    BackgroundColor "rgba(0,0,0,0.3)"
                    Padding "10px"
                    BorderRadius "5px"
                    PointerEvents "auto"
                ]
                exposureModeRadioButton ExposureMode.Manual "Manual" m.exposureMode (fun msg -> env.Emit [msg])
                exposureModeRadioButton ExposureMode.MiddleGray "Middle Gray" m.exposureMode (fun msg -> env.Emit [msg])
                exposureModeRadioButton ExposureMode.Auto "Auto" m.exposureMode (fun msg -> env.Emit [msg])
                labeledCheckbox "GlobalHeat" "Global Heat" m.globalRenderingMode (fun value -> env.Emit [SetGlobalRenderingMode value])
                numberInputWithSubmit "Timeframe Days" 0.1 365.0 ">" (fun value -> env.Emit [SetTimeFrameDays value])
                numberInputWithSubmit "Sampletime Hours" 0.01 8760.0 ">" (fun value -> env.Emit [SetSampleTimeHours value])
            }
        let leftUiDiv =
            div {
                Style [
                    Position "fixed"
                    Left "20px"
                    Top "20px"
                    BackgroundColor "rgba(0,0,0,0.3)"
                    Padding "10px"
                    BorderRadius "5px"
                    PointerEvents "none"
                ]
                timePicker
                slidersDiv
                exposureModeRadio
            }
            
        let testy = cval (Trafo3d.Identity)
            
        body {
            Style Styles.fullscreen
            renderControl {
                Samples 1
                Quality 50
                TabIndex 0
                Style Styles.fullscreen
                let! viewTrafo =
                    // SimpleOrbitController {
                    //     Location = V3d(3,4,5)
                    //     Center = V3d.Zero
                    //     RotateButton = Button.Left
                    //     PanButton = Button.Middle
                    // }
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
                    Sg.OnTap (fun e ->
                        transact(fun () ->
                            // scale before
                            let trafo =
                                let o = e.WorldPosition
                                let z = e.WorldNormal
                                let x = Vec.cross V3d.OOI z |> Vec.normalize
                                let y = Vec.cross z x |> Vec.normalize
                                Trafo3d.FromBasis(x,y,z,o + z * 0.01)
                            
                            testy.Value <- trafo
                        )
                        printfn "%A" e.WorldPosition    
                    )
                    Sg.sg m viewTrafo projTrafo info.Runtime info.ViewportSize testy moonTexture
                }
            }
            leftUiDiv
        }
    let app (moonTexture : ITexture) : App<Model, AdaptiveModel, Message> =
        {
            initial = Model.initial
            update = update
            view = view moonTexture
            unpersist = Adaptify.Unpersist.instance
        }