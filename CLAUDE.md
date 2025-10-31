# Claude Braindump - Testy3 Project Knowledge

**Last Updated**: 2025-10-31
**Purpose**: This document contains comprehensive knowledge about the Testy3 project for future Claude instances to continue work.

---

## Project Overview

**Testy3** is an interactive data visualization tool built with Aardvark.Dom (F#) that provides:
- Solar radiation modeling and simulation
- Photovoltaic panel performance analysis on buildings
- BIM-based specifications
- Real-time 3D rendering with WebGL
- Novel visualization techniques for solar panel planning

### Technology Stack
- **Language**: F# (.NET 8.0)
- **Dependency Management**: Paket
- **Web Framework**: Giraffe
- **3D Rendering**: Aardvark rendering platform
- **Architecture**: Elm-like pattern with adaptive (incremental) computation using FSharp.Data.Adaptive

---

## Project Structure

### Core Libraries (in dependency order)

1. **Aardvark.Dom.Core**: Foundation types including Worker API
2. **Aardvark.Dom**: Core DOM abstractions and 3D scene graph integration
   - `UI/DomNode.fs`: DomNode types (Element, VoidElement, Text, RenderControl)
   - `UI/Attribute.fs` & `AttributeMap.fs`: HTML attribute system
   - `SceneGraph/`: 3D scene graph nodes, picking (BVH, Intersectable), event handling
   - `Frontend.fs`: DOM rendering and diffing
   - `Updater.fs`: Update loop management
3. **Aardvark.Dom.Elm**: Elm-architecture pattern (App type, Env)
4. **Aardvark.Dom.Remote**: Abstract remote HTML backend for client-server sync
5. **Aardvark.Dom.Server**: Server-side rendering with WebSocket sync
6. **Aardvark.Dom.Giraffe**: Giraffe web framework integration
7. **Aardvark.Dom.Bootstrap**: Bootstrap UI components
8. **Aardvark.Dom.Utilities**: Camera controllers (OrbitController, FreeFlyController)

### Testy3 Application Files

**Location**: `src/Testy3/`

**Key Files**:
- **Model.fs**: Application state definitions
- **Model.g.fs**: Auto-generated adaptive model (by Adaptify)
- **App.fs**: Main application logic (Update, View, Messages)
- **Sg.fs**: Scene graph definitions and 3D rendering logic
- **Program.fs**: Entry point, server setup
- **Testy3.fsproj**: Project file

---

## Elm Architecture Pattern

Testy3 follows the Elm architecture with three core functions:

### 1. Model
Application state structure with Adaptify code generation:

```fsharp
[<ModelType>]
type Model = {
    skyInfo         : SkyInfo
    skyFov          : float
    geoInfo         : GeoInfo
    exposureMode    : ExposureMode
    exposure        : float
    planetScale     : float
    magBoost        : float
    key             : float
    starLinesVisible : bool
}
```

**Adaptify**: The `[<ModelType>]` attribute triggers code generation that creates an adaptive wrapper (`AdaptiveModel`) in `Model.g.fs`. This enables efficient incremental updates.

### 2. Update Function
Signature: `Env<'msg> -> 'model -> 'msg -> 'model`

Handles all messages and returns updated model:

```fsharp
let update (env : Env<Message>) (m : Model) (msg : Message) =
    match msg with
    | SetDateTime t -> { m with geoInfo.time = t }
    | SetMagBoost value -> { m with magBoost = value }
    | SetExposure value -> { m with exposure = value }
    | SetExposureMode mode -> { m with exposureMode = mode }
    | Nop -> m
```

### 3. View Function
Signature: `Env<'msg> -> 'amodel -> DomNode`

Renders adaptive model to virtual DOM:

```fsharp
let view (moonTexture : ITexture) (env : Env<Message>) (m : AdaptiveModel) =
    body {
        renderControl { /* 3D scene */ }
        timePicker
        magBoostSlider
        exposureSlider
        exposureModeRadio
    }
```

---

## Recent Changes (2025-10-31)

### Added Exposure Controls to GUI

**Modified File**: `src/Testy3/App.fs`

#### 1. New Message Types (lines 13-14)
```fsharp
| SetExposure of float
| SetExposureMode of ExposureMode
```

#### 2. Update Handlers (lines 25-28)
```fsharp
| SetExposure value -> { m with exposure = value }
| SetExposureMode mode -> { m with exposureMode = mode }
```

#### 3. Exposure Slider Component (lines 127-162)
- **Position**: `Top: 220px, Left: 20px`
- **Range**: 0.1 to 10.0, step 0.1
- **Display**: Shows current value as "Exposure: X.XX"
- **Style**: Dark semi-transparent background, white text

#### 4. Exposure Mode Radio Buttons (lines 163-270)
- **Position**: `Top: 330px, Left: 20px`
- **Options**: Manual (0), MiddleGray (1), Auto (2)
- **Binding**: Uses adaptive `m.exposureMode` to set checked state
- **Style**: Consistent with existing UI elements

#### 5. Added to View (lines 306-307)
Both components added to body after existing UI elements.

**Build Status**: ✅ Successful (no errors, no warnings)

---

## ExposureMode Enum

**Location**: `src/Testy3/Model.fs` (lines 44-47)

```fsharp
type ExposureMode =
    | Manual=0
    | MiddleGray=1
    | Auto=2
```

**Usage in Rendering**: These values are passed to shaders via `Sg.fs`:
- Line 636-638: Passed to sky scene graph
- Line 495-497: Used in tonemap shader as uniforms

```fsharp
|> Sg.uniform "ExposureMode" exposureMode
|> Sg.uniform "MiddleGray" key
|> Sg.uniform "Exposure" exposure
```

---

## Build System

### Commands

**Build entire solution**:
```bash
dotnet build Aardvark.Dom.sln
```

**Build Testy3 only**:
```bash
dotnet build src/Testy3/Testy3.fsproj
```

**Restore dependencies**:
```bash
dotnet tool restore
dotnet paket restore
```

**Run Testy3**:
```bash
dotnet run --project src/Testy3/Testy3.fsproj
```

### Output Directory
Consolidated build output: `bin\$(Configuration)\`

### Port Configuration
Default port: 53675 (check `Properties/launchSettings.json` if different)

---

## UI Component Pattern

All UI components in Testy3 follow this pattern (see `magBoostSlider` lines 85-120 as reference):

### Slider Component Structure
```fsharp
let componentName =
    div {
        Style [
            Position "fixed"
            Left "20px"
            Top "XXXpx"  // Stacked vertically
            Width "200px"
            BackgroundColor "rgba(0,0,0,0.3)"
            Padding "10px"
            BorderRadius "5px"
        ]
        // Label showing current value
        div {
            Style [
                Color "white"
                FontFamily "Arial"
                FontSize "14px"
                MarginBottom "5px"
            ]
            m.property |> AVal.map (fun value -> $"Label: {value}")
        }
        // Input element
        input {
            Type "range"
            Attribute("min", AttributeValue.String("X"))
            Attribute("max", AttributeValue.String("Y"))
            Attribute("step", AttributeValue.String("Z"))
            m.property |> AVal.map (fun value ->
                Attribute("value", AttributeValue.String($"%.3f{value}"))
            )
            Style [Width "100%"]
            Dom.OnInput(fun ev ->
                match System.Double.TryParse(ev.Value, CultureInfo.InvariantCulture) with
                | true, value -> env.Emit [SetMessage value]
                | false, _ -> ()
            )
        }
    }
```

### Current UI Layout (Top to Bottom)
1. **Time Picker** - Top: 20px (interactive date/time selector)
2. **MagBoost Slider** - Top: 140px (range 1-20)
3. **Exposure Slider** - Top: 220px (range 0.1-10.0)
4. **Exposure Mode Radio** - Top: 330px (Manual/MiddleGray/Auto)

---

## Important Patterns & Gotchas

### 1. Style Properties
**Use `Css.` prefix for some properties**:
- ✅ `Css.Cursor "pointer"`
- ✅ `Css.Position "fixed"`
- ✅ `Color "white"` (no prefix needed)
- ❌ `Cursor "pointer"` (type error)

The compiler will tell you when the prefix is needed. When in doubt, try without prefix first.

### 2. DOM Attributes for Input Elements
**Use `Attribute()` for numeric ranges**:
- ❌ `Dom.Min 0.1` (expects int, not float)
- ✅ `Attribute("min", AttributeValue.String("0.1"))`

### 3. Adaptive Values
**Always use `AVal.map` to extract values from adaptive properties**:
```fsharp
m.exposure |> AVal.map (fun exp -> $"Exposure: %.2f{exp}")
```

### 4. Radio Button Checked State
**Use conditional attribute mapping**:
```fsharp
m.exposureMode |> AVal.map (fun mode ->
    if mode = ExposureMode.Manual then
        Attribute("checked", AttributeValue.String("checked"))
    else
        Attribute("data-unchecked", AttributeValue.String("true"))
)
```

### 5. Parsing Input Values
**Always use InvariantCulture for float parsing**:
```fsharp
System.Double.TryParse(ev.Value, System.Globalization.CultureInfo.InvariantCulture)
```

### 6. Message Emission
**Use env.Emit with a list**:
```fsharp
env.Emit [SetExposure value]
```

---

## 3D Scene Integration

### RenderControl
The main 3D viewport uses `renderControl` computation expression (lines 279-303):

```fsharp
renderControl {
    Samples 4
    Quality 50
    TabIndex 0
    Style [/* fullscreen */]

    // Camera controller
    let! viewTrafo = SimpleFreeFlyController { ... }

    // Render info
    let! info = RenderControl.Info
    let projTrafo = info.ViewportSize |> AVal.map (fun s ->
        Frustum.perspective 90.0 0.3 300.0 (float s.X / float s.Y)
        |> Frustum.projTrafo
    )

    Sg.Proj projTrafo
    sg {
        Sg.NoEvents
        Sg.sg m viewTrafo projTrafo info.Runtime info.ViewportSize moonTexture
    }
}
```

### Scene Graph (Sg.fs)
The `Sg.sg` function in `Sg.fs` builds the complete 3D scene including:
- Sky rendering (with exposure/tonemapping)
- Sun/moon rendering
- Planet visualization
- Star field
- Building geometry (BIM data)
- Solar panel overlays

**Exposure uniforms are passed through the scene graph** (lines 495-497, 636-638).

---

## Model Initial Values

**Location**: `src/Testy3/Model.fs` (lines 66-89)

```fsharp
module Model =
    let initial = {
        exposureMode = ExposureMode.MiddleGray
        exposure = 1.0
        key = 0.18
        magBoost = 1.0
        planetScale = 1.0
        skyFov = 180.0
        starLinesVisible = false
        skyInfo = { /* ... */ }
        geoInfo = { /* ... */ }
    }
```

---

## Git Information

**Current Branch**: master
**Main Branch**: master
**Recent Commits**:
- a973a6e bugfix
- 29092ed magboost slider
- 9d1f87b planet
- b0e6b77 moon
- 7f829c5 picker

**Untracked**: `.claude/` directory

---

## Common Development Tasks

### Adding a New Slider

1. **Add message type** in `App.fs`:
   ```fsharp
   type Message =
       | SetNewValue of float
       | ...
   ```

2. **Add update handler**:
   ```fsharp
   | SetNewValue value -> { m with newProperty = value }
   ```

3. **Create slider component** following the pattern above

4. **Add to view**:
   ```fsharp
   body {
       // ... existing components
       newSlider
   }
   ```

5. **Build and test**:
   ```bash
   dotnet build src/Testy3/Testy3.fsproj
   ```

### Adding a New Model Property

1. **Update Model.fs**:
   ```fsharp
   [<ModelType>]
   type Model = {
       // existing fields
       newProperty : YourType
   }
   ```

2. **Update Model.initial** with default value

3. **Rebuild** - Adaptify will regenerate `Model.g.fs`

4. **Wire into UI and/or scene graph** as needed

---

## Debugging Tips

### Build Errors
- Check for type mismatches in Style properties (use `Css.` prefix)
- Ensure Attribute values are strings, not raw numbers
- Verify adaptive value usage with `AVal.map`

### Runtime Issues
- Check browser console for JavaScript errors
- Verify WebSocket connection (Aardvark.Dom uses WebSockets for sync)
- Ensure port 53675 is not in use

### UI Not Updating
- Verify message is emitted: `env.Emit [YourMessage]`
- Check update function returns new model
- Ensure property is in adaptive model with proper binding

---

## Additional Resources

- **Project instructions**: `CLAUDE.md` in repository root
- **Aardvark.Dom source**: `src/Aardvark.Dom*/` directories
- **Demo app**: `src/Demo/` - reference implementation
- **Paket dependencies**: `paket.dependencies` in root

---

## Next Steps / TODO Ideas

- Add key slider (currently fixed at 0.18 for Middle Gray mode)
- Add visibility toggles for scene elements (stars, sun, moon, etc.)
- Implement solar panel editing interface
- Add time animation controls
- Create preset configurations for different locations/times
- Add export functionality for solar analysis data
- Optimize rendering performance for large BIM models

---

## Key Takeaways for Future Claude Instances

1. **Testy3 uses Elm architecture** - all changes flow through Messages -> Update -> View
2. **Adaptify generates .g.fs files** - don't manually edit them
3. **UI components are stacked vertically** on the left side with fixed positioning
4. **Exposure values are already wired into shaders** - UI just controls the model
5. **Build succeeds with no errors** - the codebase is currently in a working state
6. **Follow existing patterns** - look at magBoostSlider as reference for new UI
7. **F# uses immutable updates** - always return new model with `{ m with ... }`
8. **The project is well-structured** - respect the separation of concerns

---

**End of Braindump**
