namespace Mimir.DomainDrivenDesign

open Mimir.Jsonic

type Identifier<'tag, 'value when 'value : equality and 'value : comparison> =
    private {
        Value: 'value
        Proof: Phantom.Proof<'tag>
    }

    /// Extracts the value from within.
    /// Signature matches `Extractable` Active Pattern.
    member this.Extract() =
        this.Value


[<AbstractClass>]
type IdentifierFactory<'tag, 'value when 'value : equality and 'value : comparison>
                       ( tag:'tag
                       , identifierName: string
                       , valueCodec: Codec<'value>
                       ) =

    /// Creates an `Identifier` from the given `value`.
    member __.Create(value:'value) =
        { Value = value
          Proof = Phantom.prove tag
        }


    abstract NextValue: 'value option -> 'value

    member this.Next(last:Identifier<'tag, 'value> option) : Identifier<'tag, 'value> =
        { Value = this.NextValue (Option.map(fun v -> v.Value) last)
          Proof = Phantom.prove tag
        }


    /// The codec for `Identifier` instances produced by this factory.
    member this.Codec: Codec<Identifier<'tag, 'value>> =
        let valueEncoder = Codec.encoder valueCodec
        let encode : Encoder<Identifier<'tag, 'value>> =

            fun (identifier:Identifier<'tag, 'value>) ->
                Encode.object [ (identifierName, valueEncoder identifier.Value) ]

        let valueDecoder = Codec.decoder valueCodec
        let decode: Decoder<Identifier<'tag, 'value>> =
            let decode = Decode.object(fun get -> get.Required.Field identifierName valueDecoder)

            fun path value ->
                match decode path value with
                | Error e -> Error e
                | Ok decodedValue ->
                    this.Create(decodedValue)
                    |> Ok

        Codec.build encode decode


