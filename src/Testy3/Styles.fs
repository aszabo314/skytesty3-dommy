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
