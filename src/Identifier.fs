module Mimir.DomainDrivenDesign.Identifiers

open Mimir.Jsonic


[<AbstractClass>]
type IdentifierTag<'tag, 'value, 'error
                    when 'tag : ( new : unit -> 'tag )
                     and 'tag :> IdentifierTag<'tag, 'value, 'error>
                     and 'value : equality and 'value : comparison
                  >
                  ( objectName: string
                  , valueCodec: Codec<'value>
                  ) =

    static let tag = new 'tag()

    static let tryCreate (value:'value) : Result<Identifier<'tag, 'value, 'error>, 'error> =
        tag.TryParse(value)
        |> Result.map(fun parsedValue -> { Value = parsedValue })

    static let codec =
        let valueEncoder = Codec.encoder tag.ValueCodec
        let encode (identifier:Identifier<'tag, 'value, 'error>) =
            Encode.object [ (tag.ObjectName, valueEncoder identifier.Value) ]

        let valueDecoder = Codec.decoder tag.ValueCodec
        let decode: Decoder<Identifier<'tag, 'value, 'error>> =
            let decode = Decode.object(fun get -> get.Required.Field tag.ObjectName valueDecoder)

            fun path value ->
                match decode path value with
                | Error e -> Error e
                | Ok decodedValue ->
                    tryCreate decodedValue
                    |> Result.mapError(fun e ->
                        let msg = $"Creation of Identifier {tag.ObjectName} failed with message: {tag.FormatError(e)}."
                        JsonicError.Failure {| Path=path; Message=msg |}
                    )

        Codec.build encode decode

    static member Codec =
        codec

    static member TryCreate (value:'value) =
        tryCreate value


    member __.ObjectName = objectName
    member __.ValueCodec = valueCodec

    /// Format an `'error` into a `string`.
    abstract FormatError:'error -> string


    /// Checks whether the input matches what is expected and returns `Ok 'value` if so, otherwise `Error 'error`.
    /// Steps usually include cleaning the input, checking the input and then formatting it.
    abstract TryParse: 'value -> Result<'value, 'error>


and [<AbstractClass>]
    IdentifierTag<'tag, 'value
                   when 'tag : ( new : unit -> 'tag )
                    and 'tag :> IdentifierTag<'tag, 'value, ConstraintError>
                    and 'value : equality and 'value : comparison
                 >
                 ( objectName: string
                 , valueCodec: Codec<'value>
                 ) =

    inherit IdentifierTag<'tag, 'value, ConstraintError>(objectName, valueCodec)

    override __.FormatError(e)=
        Constraint.formatError e

and
    [<StructuralEquality; StructuralComparison>]
    Identifier<'tag, 'value, 'error
                when 'tag : ( new : unit -> 'tag )
                 and 'tag :> IdentifierTag<'tag, 'value, 'error>
                 and 'value : equality and 'value : comparison
              > =

    private {
        Value: 'value
    }

    static member Codec =
        IdentifierTag<'tag, 'value, 'error>.Codec

    static member TryCreate (value:'value) =
        IdentifierTag<'tag, 'value, 'error>.TryCreate value

    /// Extracts the value from within.
    /// Signature matches `Extractable` Active Pattern.
    member this.Extract() =
        this.Value

