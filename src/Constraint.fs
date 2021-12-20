namespace Mimir.DomainDrivenDesign

open System
open Elm.Regex

open Mimir.Jsonic


// We like the word constraint, too bad F# has reserved it.
#nowarn "46"

[<Struct>]
[<RequireQualifiedAccess>]
type Equality =
    | LessThan
    | LessThanOrEqual
    | Equal
    | GreaterThanOrEqual
    | GreaterThan

    static member Codec:Codec<Equality> =
        Codec.custom(fun a b c d e value ->
            match value with
            | LessThan -> a
            | LessThanOrEqual -> b
            | Equal -> c
            | GreaterThanOrEqual -> d
            | GreaterThan -> e)
        |> Codec.variant0 "lt" Equality.LessThan
        |> Codec.variant0 "lte" Equality.LessThanOrEqual
        |> Codec.variant0 "eq" Equality.Equal
        |> Codec.variant0 "gte" Equality.GreaterThanOrEqual
        |> Codec.variant0 "gt" Equality.GreaterThan
        |> Codec.buildCustom

[<RequireQualifiedAccess>]
module Equality =
    let inline compare (equality:Equality) =
        match equality with
        | Equality.LessThan -> (<)
        | Equality.LessThanOrEqual -> (<=)
        | Equality.Equal -> (=)
        | Equality.GreaterThanOrEqual -> (>=)
        | Equality.GreaterThan -> (>)

    let asString = function
        | Equality.LessThan ->           "<"
        | Equality.LessThanOrEqual ->    "<="
        | Equality.Equal ->              "="
        | Equality.GreaterThanOrEqual -> ">="
        | Equality.GreaterThan ->        ">"


type CustomConstraintError =
    { ConstraintName : string
      Error : string
    }

    static member Codec:Codec<CustomConstraintError> =
        Codec.object (fun name err -> { ConstraintName = name; Error = err })
        |> Codec.field "constraintName" (fun v -> v.ConstraintName) Codec.string
        |> Codec.field "error" (fun v -> v.Error) Codec.string
        |> Codec.buildObject


type RegexConstraintError =
    { PatternName : string
      Pattern : string
      ActualValue : string
    }

    static member Codec:Codec<RegexConstraintError> =
        Codec.object (fun name pattern value -> { PatternName = name; Pattern = pattern; ActualValue = value })
        |> Codec.field "patternName" (fun v -> v.PatternName) Codec.string
        |> Codec.field "pattern" (fun v -> v.Pattern) Codec.string
        |> Codec.field "actualValue" (fun v -> v.ActualValue) Codec.string
        |> Codec.buildObject


type LengthConstraintError =
    { Constraint : Equality
      ConstraintValue : int
      ActualValue : int
    }

    static member Codec:Codec<LengthConstraintError> =
        Codec.object (fun eq cval aval -> { Constraint = eq; ConstraintValue = cval; ActualValue = aval })
        |> Codec.field "constraint" (fun v -> v.Constraint) Equality.Codec
        |> Codec.field "constraintValue" (fun v -> v.ConstraintValue) Codec.int
        |> Codec.field "actualValue" (fun v -> v.ActualValue) Codec.int
        |> Codec.buildObject


type ValueConstraintError =
    { Constraint : Equality
      ConstraintValue : string
      ActualValue : string
    }

    static member Codec:Codec<ValueConstraintError> =
        Codec.object (fun eq cval aval -> { Constraint = eq; ConstraintValue = cval; ActualValue = aval })
        |> Codec.field "constraint" (fun v -> v.Constraint) Equality.Codec
        |> Codec.field "constraintValue" (fun v -> v.ConstraintValue) Codec.string
        |> Codec.field "actualValue" (fun v -> v.ActualValue) Codec.string
        |> Codec.buildObject


type ConstraintError =
    | CustomConstraintError of CustomConstraintError
    | RegexConstraintError of RegexConstraintError
    | LengthConstraintError of LengthConstraintError
    | ValueConstraintError of ValueConstraintError

    static member Codec:Codec<ConstraintError> =
        Codec.custom(fun a b c d value ->
            match value with
            | CustomConstraintError v -> a v
            | RegexConstraintError v ->  b v
            | LengthConstraintError v -> c v
            | ValueConstraintError v -> d v)
        |> Codec.variant1 "customConstraintError" CustomConstraintError CustomConstraintError.Codec
        |> Codec.variant1 "regexConstraintError" RegexConstraintError RegexConstraintError.Codec
        |> Codec.variant1 "lengthConstraintError" LengthConstraintError LengthConstraintError.Codec
        |> Codec.variant1 "valueConstraintError" ValueConstraintError ValueConstraintError.Codec
        |> Codec.buildCustom



[<RequireQualifiedAccess>]
module Constraint =
    let formatError error =
        match error with
        | LengthConstraintError v ->
            let symbol = Equality.asString v.Constraint

            sprintf "Length expected to be %s %i but was %i."
                symbol
                v.ConstraintValue
                v.ActualValue

        | ValueConstraintError v ->
            let symbol = Equality.asString v.Constraint

            sprintf "Value expected to be %s %s but was %s."
                symbol
                v.ConstraintValue
                v.ActualValue

        | RegexConstraintError v ->
            sprintf "The %s pattern wasn't found." v.PatternName

        | CustomConstraintError v ->
            let errorText =
                if String.IsNullOrWhiteSpace v.Error then ""
                else
                    v.Error
                    |> fun s -> s.Trim [| ','; '.'; '!'; ' ' |]
                    |> fun s -> " " + s + "."

            sprintf "The input isn't a %s.%s" v.ConstraintName errorText


    let withLength eq constraintValue getLength (source:'source) =
        let actualValue =
            getLength source

        if Equality.compare eq actualValue constraintValue then
            Ok source

        else
            Error
                (LengthConstraintError
                    { Constraint = eq
                      ConstraintValue = constraintValue
                      ActualValue = actualValue
                    }
                )

    let inline count< ^source when ^source : (member Count :int)> eq constraintValue (source:^source) =
        let getLength a = (^source: (member Count: int) a)
        withLength eq constraintValue getLength source

    let inline length< ^source when ^source : (member Length :int)> eq constraintValue (source:^source) =
        let getLength a = (^source: (member Length: int) a)
        withLength eq constraintValue getLength source


    /// Specify a constraint on a value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `convertValue`: a function that can convert values of `value` to `String`.
    /// - `source`: the source of the value.
    ///
    let withValue eq (constraintValue:'value) getValue convertValue (source:'source) =
        let actualValue =
            getValue source

        if Equality.compare eq actualValue constraintValue then
            Ok source

        else
            Error
                (ValueConstraintError
                    { Constraint = eq
                      ConstraintValue = convertValue constraintValue
                      ActualValue = convertValue actualValue
                    }
                )

    /// Specify a constraint on an `int8` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let int8 eq constraintValue (getValue:'source -> int8) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `int16` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let int16 eq constraintValue (getValue:'source -> int16) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `int32` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let int32 eq constraintValue (getValue:'source -> int32) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `int64` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let int64 eq constraintValue (getValue:'source -> int64) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `uint8` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let uint8 eq constraintValue (getValue:'source -> uint8) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `uint16` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let uint16 eq constraintValue (getValue:'source -> uint16) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `uint32` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let uint32 eq constraintValue (getValue:'source -> uint32) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on an `uint64` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let uint64 eq constraintValue (getValue:'source -> uint64) =
        withValue eq constraintValue getValue (sprintf "%i")


    /// Specify a constraint on a `float32` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let float32 eq constraintValue (getValue:'source -> float32) =
        withValue eq constraintValue getValue (sprintf "%f")


    /// Specify a constraint on a `float64` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let float64 eq constraintValue (getValue:'source -> float64) =
        withValue eq constraintValue getValue (sprintf "%f")


    /// Specify a constraint on a `decimal` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let decimal eq constraintValue (getValue:'source -> decimal) =
        withValue eq constraintValue getValue (sprintf "%f")


    /// Specify a constraint on a `DateTime` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let dateTime eq constraintValue (getValue:'source -> DateTime) =
        withValue eq constraintValue getValue (sprintf "%A")


    /// Specify a constraint on a `DateTime` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let dateTimeOffset eq constraintValue (getValue:'source -> DateTimeOffset) =
        withValue eq constraintValue getValue (sprintf "%A")


    /// Specify a constraint on a `TimeSpan` value.
    ///
    /// - `eq`: The `Equality` used when comparing the actual value to the `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let timeSpan eq constraintValue (getValue:'source -> TimeSpan) =
        withValue eq constraintValue getValue (sprintf "%A")


    let regex name pattern actualValue =
        let regex =
            Regex.fromString pattern
            |> Option.defaultValue Regex.never

        let matches =
            Regex.contains regex actualValue

        if matches then
            Ok actualValue

        else
            Error
                (RegexConstraintError
                    { Pattern = pattern
                      PatternName = name
                      ActualValue = actualValue
                    }
                )

    let inline customError constrainName error =
        CustomConstraintError
            { Error = error
              ConstraintName = constrainName
            }

    let custom name constrain (actualValue:'value) =
        constrain actualValue
        |> Result.mapError(customError name)


[<AutoOpen>]
module ConstraintBuilder =
    type ConstraintBuilder () =
        [<CompilerMessage("A Constraint doesn't support the Zero operation.", 708, IsError = true)>]
        member __.Zero () = failwith "Can't have a zero operation in the constraint builder!"
        member __.Combine (a: 'source -> Result<'source,ConstraintError>, b: 'source -> Result<'source,ConstraintError>) = a >> Result.bind b
        member __.Yield (x:'source -> Result<'source,ConstraintError>) = x
        member __.Delay expr = expr ()
        member __.Run (x:'source -> Result<'source,ConstraintError>) = x


    let constrain = ConstraintBuilder ()