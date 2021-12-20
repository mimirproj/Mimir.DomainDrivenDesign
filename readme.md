# Mimir.DomainDrivenDesign.ValueObjects

Types and functions related to DDD which are used in Mimir.


## ValueObject

Value objects are types which are _immutable_ and have _value semantics_. It is important that the data encapsulated in a _Value Object_ is canonical and correct.


### Implementation

- Immutable: An F# record is used as it is immutable by default.
- Value Semantics: F# records have value semantics and type parameter constraints ensure that the value does too.




### Example

```
// Describe the data with types
module Domain =
    [<Sealed>]
    type GivenNameTag private () =
        inherit ValueObjectTag<GivenNameTag, string>("givenName", Codec.string)

        override __.TryParse (value) =
            value.Trim()
            |> constrain {
                Constraint.length Equality.GreaterThan 0
                Constraint.length Equality.LessThan 50
            }


    // OPTIONAL: An alias to simplify referencing the ValueObject
    type GivenName = ValueObject<GivenNameTag, string, ConstraintError>

    // OPTIONAL: An active pattern which extracts the data
    let inline (|GivenName|) (valueObject:ValueObject<GivenName, string>) =
        extract valueObject


module Test =
    open Domain

    let nameResult = // Result<GivenName, ConstraintError>
        GivenName.TryCreate "Bob"

```

