namespace Testy3

open System
open Aardvark.SceneGraph.Raytracing
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Rendering
open FShade
open Aardvark.Dom

module Shaders =
    type UniformScope with
        member x.LightViewProj : M44f = uniform?LightViewProj
    let private shadowSampler =
        sampler2dShadow {
            texture uniform?ShadowDepth
            filter Filter.MinMagPoint
            addressU WrapMode.Border
            addressV WrapMode.Border
            borderColor C4f.White
            comparison ComparisonFunction.LessOrEqual
        }
    let shadowShader (v : Effects.Vertex) =
        fragment {
            let np = uniform.LightViewProj * v.wp
            let p = np.XYZ / np.W
            let tc = V3f(0.5f, 0.5f,0.5f) + V3f(0.5f, 0.5f, 0.5f) * p.XYZ
            let d = shadowSampler.Sample(tc.XY,tc.Z)
            return V4f(v.c.XYZ * d, v.c.W)
        }
    