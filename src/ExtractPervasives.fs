[<AutoOpen>]
module Mimir.DomainDrivenDesign.ExtractPervasives

let inline (|Extractable|) (instance:^a) =
    (^a: (member Extract: unit -> ^value) instance)

let inline extract (Extractable value) = value