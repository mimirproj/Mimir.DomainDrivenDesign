namespace Mimir.DomainDrivenDesign

open System
open Mimir.Jsonic

type ValueObject<'tag, 'value when 'value : equality and 'value : comparison> =
    private {
        Value: 'value
        Proof: Phantom.Proof<'tag>
    }

    /// Extracts the value from within.
    /// Signature matches `Extractable` Active Pattern.
    member this.Extract() =
        this.Value

[<AbstractClass>]
type ValueObjectFactory<'tag, 'value, 'error when 'value : equality and 'value : comparison>
                       ( tag:'tag
                       , objectName: string
                       , valueCodec: Codec<'value>
                       ) =

    /// Checks whether the input matches what is expected and returns `Ok 'value` if so, otherwise `Error 'error`.
    /// Steps included usually include cleaning the input, chacking the input and then formatting it.
    abstract TryParse: 'value -> Result<'value, 'error>

    /// Format an `'error` into a `string`.
    abstract FormatError:'error -> string

    /// Tries to create a `ValueObject` from the given `value`.
    member this.TryCreate(value) =
        this.TryParse(value)
        |> Result.map(fun parsedValue ->
            { Value = parsedValue
              Proof = Phantom.prove tag
            }
        )


    /// Creates a `ValueObject`, an error will be raised as an exception!
    member this.Create(value) =
        match this.TryCreate(value) with
        | Error e -> failwithf $"Creation of ValueObject %A{tag} failed with error: {this.FormatError(e)}"
        | Ok v -> v



    /// The codec for `ValueObject` instances produced by this factory.
    member this.Codec: Codec<ValueObject<'tag, 'value>> =
        let valueEncoder = Codec.encoder valueCodec
        let encode : Encoder<ValueObject<'tag, 'value>> =

            fun (valueObject:ValueObject<'tag, 'value>) ->
                Encode.object [ (objectName, valueEncoder valueObject.Value) ]

        let valueDecoder = Codec.decoder valueCodec
        let decode: Decoder<ValueObject<'tag, 'value>> =
            let decode = Decode.object(fun get -> get.Required.Field objectName valueDecoder)

            fun path value ->
                match decode path value with
                | Error e -> Error e
                | Ok decodedValue ->
                    this.TryCreate(decodedValue)
                    |> Result.mapError(fun e ->
                        let msg = $"Creation of ValueObject {objectName} failed with message: {this.FormatError(e)}."
                        JsonicError.Failure
                            {| Path=path
                               Message=msg
                            |}
                    )

        Codec.build encode decode