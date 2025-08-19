namespace Testy3

open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Dom

module Shaders =
    type UniformScope with
        member x.LightViewProj : M44d = uniform?LightViewProj
        
    let private shadowSampler =
        sampler2dShadow {
            texture uniform?ShadowDepth
            filter Filter.MinMagPoint
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
        // sampler2d {
        //     texture uniform?ShadowDepth
        //     filter Filter.MinMagLinear
        //     addressU WrapMode.Border
        //     addressV WrapMode.Border
        // }

    // let shadowDebug (v : Effects.Vertex) =
    //     fragment {
    //         let d = shadowSampler.Sample(v.tc.XY).X ** 64.0
    //         return V4d(d,d,d,1.0)
    //     }
    let shadowShader (v : Effects.Vertex) =
        fragment {
            let np = uniform.LightViewProj * v.wp
            let p = np.XYZ / np.W
            let tc = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ
            let d = shadowSampler.Sample(tc.XY,tc.Z - 0.0017)
            return V4d(v.c.XYZ * d, v.c.W)
        }