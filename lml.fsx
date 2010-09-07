[<AutoOpen>]
module LightMarkupLanguage
    type LightMarkup =
    | T
    | Text of string
    | Attribute of string * string
    | Element of string * seq<LightMarkup>
        member root.write = 
            let sb = new System.Text.StringBuilder()
            let attr node = match node with Attribute(_,_) -> true | _ -> false
            let no_attr node = not (attr node)
            let rec build (node:LightMarkup) =
                match node with
                | T -> T |> ignore
                | Text value -> sb.Append(value) |> ignore
                | Attribute(name, value) -> sb.AppendFormat(" {0}=\"{1}\"", name, value) |> ignore
                | Element(name, nodes) ->
                    sb.AppendFormat("<{0}", name) |> ignore
                    nodes |> Seq.filter attr |> Seq.iter build
                    if Seq.isEmpty nodes then sb.Append(" />") |> ignore
                    else
                        sb.Append(">").AppendLine() |> ignore
                        nodes |> Seq.filter no_attr |> Seq.iter build
                        sb.AppendFormat("</{0}>", name) |> ignore
                    sb.AppendLine() |> ignore
            build root
            printfn "%s" (sb.ToString())
        static member (?) (self:LightMarkup, name:string) = Element(name, [])
        static member (?<-) (self:LightMarkup, name:string, nodes:seq<LightMarkup>) = Element(name, nodes)
        static member (?<-) (self:LightMarkup, name:string, node:LightMarkup) = Element(name, [node])
        static member (?<-) (self:LightMarkup, name:string, value:string) = Element(name, [Text(value)])

    let (=>) (name:string) (value:string) = Attribute(name, value)
    let (~&) (value:string) = Text(value)
