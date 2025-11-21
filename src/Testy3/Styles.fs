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
    let topleftslider (labelText : string) (vmin : float) (vmax : float) (step : float) (currentValue : aval<float>) (emit : float -> unit) =
        div {
            Style [
                Left "20px"
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
    let numberInputWithSubmit (labelText : string) (vmin : float) (vmax : float) (submitLabel : string) (emit : float -> unit) =
        // Local state for the input value
        let inputValue = AVal.init ""
        let isValid =
            inputValue |> AVal.map (fun v ->
                match System.Double.TryParse(v, System.Globalization.CultureInfo.InvariantCulture) with
                | true, _ -> true
                | false, _ -> false
            )
        div {
            Style [
                Left "20px"
                Top "20px"
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
                labelText
            }
            input {
                Type "text"
                Value inputValue.Value
                Style [
                    Width "100%"
                    MarginBottom "5px"
                ]
                Dom.OnInput(fun ev -> transact (fun _ -> inputValue.Value <- ev.Value))
                Attribute("placeholder", AttributeValue.String($"%.2f{vmin} - %.2f{vmax}"))
            }
            button {
                submitLabel
                Style [
                    Width "100%"
                    BackgroundColor "#888"
                    Color "white"
                    Border "none"
                    Padding "6px"
                    BorderRadius "3px"
                    Css.Cursor "pointer"
                ]
                isValid |> AVal.map (fun isValid ->
                    if isValid then
                        Style [
                            Opacity "1.0"
                            PointerEvents "auto"
                        ]
                    else
                        Style [
                            Opacity "0.5"
                            PointerEvents "none"
                        ]
                )
                Dom.OnClick(fun _ ->
                    match System.Double.TryParse(inputValue.Value, System.Globalization.CultureInfo.InvariantCulture) with
                    | true, value ->
                        let clamped =
                            if value < vmin then vmin
                            elif value > vmax then vmax
                            else value
                        emit clamped
                    | false, _ -> ()
                )
                isValid |> AVal.map (fun isValid ->
                    if isValid then
                        Dom.Disabled false
                    else
                        Dom.Disabled true
                )
            }
        }
    let labeledCheckbox (id : string) (labelText : string) (current : aval<bool>) (emit : bool -> unit) =
        div {
            Style radioButtonStyle
            input {
                Type "checkbox"
                Attribute("id", AttributeValue.String id)
                current |> AVal.map (fun v ->
                    if v then Dom.Checked v
                    else Attribute("data-unchecked", AttributeValue.String "true")
                )
                Dom.OnChange(fun e -> emit e.Checked)
            }
            label {
                Attribute("for", AttributeValue.String id)
                Style radioLabelStyle
                $" {labelText}"
            }
        }
