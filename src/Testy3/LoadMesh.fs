namespace Testy3

open System
open Aardvark.Base
open Aardvark.Rendering

module Mesh =
    let loadMesh (file : string) : V3f[] * V3f[] * C4b[] =
        
        let mesh = Aardvark.Data.Wavefront.ObjParser.Load file
        
        let positions = 
            match mesh.Vertices with
            | :? System.Collections.Generic.IList<V3f> as v -> v.ToArray(v.Count)
            | :? System.Collections.Generic.IList<V3d> as v -> v.ToArray(v.Count) |> Array.map V3f
            | :? System.Collections.Generic.IList<V4f> as v -> v.ToArray(v.Count) |> Array.map Vec.xyz
            | :? System.Collections.Generic.IList<V4d> as v -> v.ToArray(v.Count) |> Array.map V3f
            | _ -> failwith ""
            
        let bounds = Box3f positions |> Box3d
            
        let trafo =
            Trafo3d.Translation(-bounds.Center) *
            Trafo3d.Scale(2.0 / bounds.Size.NormMax) *
            Trafo3d.RotationX(Constant.PiHalf)
        match mesh.Normals with
        | null ->
            [||], [||], [||]
            
        | normals ->
            let positions = positions |> Array.map (fun p -> trafo.Forward.TransformPos (V3d p) |> V3f)
            let normals = normals.ToArray(normals.Count) |> Array.map (fun n -> trafo.Backward.TransposedTransformDir (V3d n) |> Vec.normalize |> V3f)
            
            let colors =
                match mesh.VertexColors with
                | null ->  Array.create positions.Length C4b.White
                | cs -> cs.ToArray(cs.Count) |> Array.map (fun c -> c.ToC3b().ToC4b())
            
            
            let ps = ResizeArray()
            let ns = ResizeArray()
            let cs = ResizeArray()
            
            for set in mesh.FaceSets do
                
                let iPos = set.VertexIndices
                let iNormals =
                    if isNull set.NormalIndices then iPos
                    else set.NormalIndices
                let iColors = set.VertexIndices
                
                for ti in 0 .. set.ElementCount - 1 do
                    let fi = set.FirstIndices.[ti]
                    let cnt = set.FirstIndices.[ti+1] - fi
                
                    if cnt = 3 then
                        cs.Add colors.[iColors.[fi + 0]]
                        cs.Add colors.[iColors.[fi + 1]]
                        cs.Add colors.[iColors.[fi + 2]]
                        
                        ps.Add positions.[iPos.[fi + 0]]
                        ps.Add positions.[iPos.[fi + 1]]
                        ps.Add positions.[iPos.[fi + 2]]
                        
                        ns.Add normals.[iNormals.[fi + 0]]
                        ns.Add normals.[iNormals.[fi + 1]]
                        ns.Add normals.[iNormals.[fi + 2]]
            
            let mutable i = 0
            while i < ps.Count do
                let p0 = ps.[i]
                let p1 = ps.[i+1]
                let p2 = ps.[i+2]
                let n = Vec.cross (p1 - p0) (p2 - p0) |> Vec.normalize
                ns.[i]   <- n
                ns.[i+1] <- n
                ns.[i+2] <- n
                i <- i+3
            
            ps.ToArray(), ns.ToArray(), cs.ToArray()

    let meshToIg (fn : string) =
        let (ps,ns,cs) = loadMesh fn
        IndexedGeometry(
            Mode = IndexedGeometryMode.TriangleList,
            IndexedAttributes = SymDict.ofList [
                DefaultSemantic.Positions, ps :> Array
                DefaultSemantic.Normals, ns :> Array
                DefaultSemantic.Colors, cs :> Array
            ]
        )