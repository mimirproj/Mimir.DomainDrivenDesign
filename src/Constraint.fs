namespace Mimir.DomainDrivenDesign

open System
open Elm.Core
open Elm.Regex 

// We like the word constraint, too bad F# has reserved it.
#nowarn "46"

type ConstraintError =
    | CustomConstraintError of  {| ConstraintName : string; Error : string |}
    | RegexConstraintError of   {| PatternName : string; Pattern : string; ActualValue : string |}
    | LengthConstraintError of  {| Constraint : Order; ConstraintValue : int; ActualValue : int |}
    | ValueConstraintError of   {| Constraint : Order; ConstraintValue : string; ActualValue : string |}


[<RequireQualifiedAccess>]
module Constraint =
    let formatError error =
        match error with
        | LengthConstraintError v -> 
            let symbol =
                match v.Constraint with
                | LT -> "<"
                | EQ -> "="
                | GT -> ">"

            sprintf "Length expected to be %s %i but was %i."
                symbol
                v.ConstraintValue
                v.ActualValue

        | ValueConstraintError v ->
            let symbol =
                match v.Constraint with
                | LT -> "<"
                | EQ -> "="
                | GT -> ">"

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


    let withLength constraint constraintValue getLength (source:'source) =
        let actualValue =
            getLength source

        if compare actualValue constraintValue <> constraint then
            Error
                (LengthConstraintError
                    {| Constraint = constraint
                       ConstraintValue = constraintValue
                       ActualValue = actualValue
                    |}
                )

        else
            Ok source

    let inline count< ^source when ^source : (member Count :int)> constraint constraintValue (source:^source) =
        let getLength a = (^source: (member Count: int) a)
        withLength constraint constraintValue getLength source

    let inline length< ^source when ^source : (member Length :int)> constraint constraintValue (source:^source) =
        let getLength a = (^source: (member Length: int) a)
        withLength constraint constraintValue getLength source


    /// Specify a constraint on a value.
    ///
    /// - `constraint`: Constrain the actual value to be `LT`, `GT` or`EQ` to `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to using `compareValue`.
    /// - `getValue`: get the actual value from the `source`.
    /// - `compareValue`: a function that can compare 2 values of `value`.
    /// - `convertValue`: a function that can convert values of `value` to `String`.
    /// - `source`: the source of the value.
    ///
    let withValue constraint (constraintValue:'value) getValue compareValue convertValue (source:'source) =
        let actualValue =
            getValue source

        if compareValue actualValue constraintValue <> constraint then
            Error
                (ValueConstraintError
                    {| Constraint = constraint
                       ConstraintValue = convertValue constraintValue
                       ActualValue = convertValue actualValue
                    |}
                )

        else
            Ok source


    /// Specify a constraint on an `Int` value.
    ///
    /// - `constraint`: Constrain the actual value to be `LT`, `GT` or`EQ` to `constraintValue`.
    /// - `constraintValue`: The value that the actual value is compared to using `compareValue`.
    /// - `getValue`: get the actual value from the `source`.
    /// - `source`: the source of the value.
    ///
    let int constraint constraintValue (getValue:'source -> int) =
        withValue constraint constraintValue getValue compare String.fromInt


    let regex name pattern actualValue =
        let regex =
            Regex.fromString pattern |> Maybe.withDefault Regex.never

        let matches =
            Regex.contains regex actualValue

        if matches then
            Ok actualValue

        else
            Error
                (RegexConstraintError
                    {| Pattern = pattern
                       PatternName = name
                       ActualValue = actualValue
                    |}
                )

    let custom name constrain (actualValue:'value) =
        constrain actualValue
        |> Result.mapError(fun error ->
            CustomConstraintError
                {| Error = error
                   ConstraintName = name
                |}
        )