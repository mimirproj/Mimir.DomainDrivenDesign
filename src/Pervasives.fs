[<AutoOpen>]
module Mimir.DomainDrivenDesign.Pervasives

let inline (|Extractable|) (instance:^a) =
    (^a: (member Extract: unit -> ^value) instance)

let inline extract (Extractable value) = value
