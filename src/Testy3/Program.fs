namespace Testy3

open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Giraffe
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Application
open Aardvark.Dom
open Aardvark.Dom.Remote
open Aardvark.Rendering
open Aardvark.Application.Slim
open Aardium
open Testy3



module Program = 
    [<EntryPoint>]
    let main _ =
        Aardvark.Init()
        Aardium.init()
        
        let app = new OpenGlApplication()

        let moonTexture =
            let path = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../resources")
            (FileTexture(Path.combine [path; "moon.png"], { wantSrgb = true; wantCompressed = false; wantMipMaps = true }) :> ITexture)
        let run (ctx : DomContext) = 
            App.start ctx (App.app moonTexture)
        Host.CreateDefaultBuilder()
            .ConfigureWebHostDefaults(
                fun webHostBuilder ->
                    webHostBuilder
                        .UseSockets()
                        .Configure(fun b -> b.UseWebSockets().UseGiraffe (DomNode.toRoute app.Runtime run))
                        .ConfigureServices(fun s -> s.AddGiraffe() |> ignore)
                        |> ignore
            )
            .Build()
            .Start()
        
        Aardium.run {
            title "Aardvark rocks \\o/"
            width 1600
            height 900
            url "http://localhost:53675"
            debug true
        }
        0
