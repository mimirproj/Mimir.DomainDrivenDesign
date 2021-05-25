namespace Mimir.DomainDrivenDesign

type IValueObject<'v> =
    abstract Get: unit -> 'v

[<NoEquality; NoComparison>]
type ValueObjectFactory<'t, 'v, 'e
                         when 't : equality
                          and 't : comparison
                          and 't :> IValueObject<'v>> =
    private
        { Clean: 'v -> 'v
          Constrain: 'v -> Result<'v, 'e>
          Construct: 'v -> Result<'t, 'e>
        }


[<RequireQualifiedAccess>]
module ValueObjectFactory =
    let create construct : ValueObjectFactory<'t, 'v, 'e> =
        { Clean = id
          Constrain = Ok
          Construct = construct
        }

    let clean f (factory:ValueObjectFactory<'t, 'v, 'e>) =
        { factory with
            Clean = factory.Clean >> f
        }

    let constrain f (factory:ValueObjectFactory<'t, 'v, 'e>) =
        { factory with
            Constrain =
                factory.Constrain
                >> Result.bind f
        }


[<RequireQualifiedAccess>]
module ValueObject =

    let inline get<'t, 'v when 't :> IValueObject<'v>> (valueObject:'t) =
        valueObject.Get()

    let create (factory:ValueObjectFactory<'t, 'v, 'e>) =
        factory.Clean
        >> factory.Constrain
        >> Result.bind factory.Construct


    open Mimir.Jsonic

    let encode<'t, 'v when 't :> IValueObject<'v>> (name:string)
                                                   (valueEncoder:Encoder<'v>)
                                                   : Encoder<'t> =

        fun (valueObject:'t) ->
            Encode.object [ (name, valueEncoder (get valueObject)) ]


    let decode (name:string)
               (valueDecoder:Decoder<'v>)
               (formatError:'e -> string)
               (factory:ValueObjectFactory<'t, 'v, 'e>)
               : Decoder<'t> =

        fun path value ->
            match valueDecoder path value with
            | Error e -> Error e
            | Ok decodedValue ->
                create factory decodedValue
                |> Result.mapError(fun e ->
                    let msg = $"Decoding of {name} failed with message: {formatError e}."
                    Failure {| Path=path
                               Message=msg
                            |}
                )


    let codec name
              valueEncoder
              valueDecoder
              (formatError:'e -> string)
              (factory:ValueObjectFactory<'t, 'v, 'e>)
              : Codec<'t> =

        Codec.build
            (encode name valueEncoder)
            (decode name valueDecoder formatError factory)



[<AutoOpen>]
module ValueObjectPervasive =
    let extract = ValueObject.get

    let inline (|ValueObject|) value =
        ValueObject.get value
