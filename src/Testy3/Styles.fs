namespace Testy3

open FSharp.Data.Adaptive
open Aardvark.Dom

module Styles =
    open Aardvark.Dom
    let fullscreen = [
        Css.Position "fixed"
        Css.Left "0px"
        Css.Top "0px"
        Css.Width "100%"
        Css.Height "100%"
    ]
    let radioButtonStyle = [
        Css.Color "white"
        Css.FontFamily "Arial"
        Css.FontSize "12px"
        Css.MarginBottom "3px"
    ]
    let radioLabelStyle = [
        Css.MarginLeft "5px"
        Css.Cursor "pointer"
    ]
    let exposureModeRadioButton (modeValue: ExposureMode) (labelText: string) (currentMode: aval<ExposureMode>) (emit: Message -> unit) =
        div {
            Style radioButtonStyle
            input {
                Type "radio"
                Attribute("name", AttributeValue.String("exposureMode"))
                Attribute("id", AttributeValue.String($"mode{modeValue}"))
                currentMode |> AVal.map (fun mode ->
                    if mode = modeValue then
                        Attribute("checked", AttributeValue.String("checked"))
                    else
                        Attribute("data-unchecked", AttributeValue.String("true"))
                )
                Dom.OnChange(fun _ -> emit (SetExposureMode modeValue))
            }
            label {
                Attribute("for", AttributeValue.String($"mode{modeValue}"))
                Style radioLabelStyle
                $" {labelText}"
            }
        }
    let topleftslider (topPx : int) (labelText : string) (vmin : float) (vmax : float) (step : float) (currentValue : aval<float>) (emit : float -> unit) =
        div {
            Style [
                Position "fixed"
                Left "20px"
                Top $"{topPx}px"
                Width "200px"
                BackgroundColor "rgba(0,0,0,0.3)"
                Padding "10px"
                BorderRadius "5px"
            ]
            div {
                Style [
                    Color "white"
                    FontFamily "Arial"
                    FontSize "14px"
                    MarginBottom "5px"
                ]
                currentValue |> AVal.map (fun exp -> $"{labelText}: %.2f{exp}")
            }
            input {
                Type "range"
                Attribute("min",  AttributeValue.String($"%.2f{vmin}"))
                Attribute("max",  AttributeValue.String($"%.2f{vmax}"))
                Attribute("step", AttributeValue.String($"%.2f{step}"))
                currentValue |> AVal.map (fun exp -> Attribute("value", AttributeValue.String($"%.3f{exp}")))
                Style [
                    Width "100%"
                ]
                Dom.OnInput(fun ev ->
                    match System.Double.TryParse(ev.Value, System.Globalization.CultureInfo.InvariantCulture) with
                    | true, value -> emit value
                    | false, _ -> ()
                )
            }
        }