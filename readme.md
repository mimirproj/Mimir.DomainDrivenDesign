# Mimir.DomainDrivenDesign

Types and functions related to DDD which are used in Mimir.


## ValueObject

Value objects are types which are _immutable_ and have _value semantics_. It is important that the data encapsulated in a _Value Object_ is canonical and correct.


### Implementation

- Immutable: An F# record is used as it is immutable by default.
- Value Semantics: F# records have value semantics and type parameter constraints ensure that the value does too.
- Correctness: Since the constructor of the `ValueObject `is private, to create an instance of `ValueObject` the `TryCreate` method of the `ValueObjectFactory` must be used, it ensures correctness of the value by parsing it using `TryParse`.

**The discriminated union that is used to tag the `ValueObject` should have a single private member named the same as the type it belongs to.**



### Example

```
module Domain =
    type GivenName =
        private // NB! Private
        | GivenName

    type GivenNameError = SomeError

    type GivenNameFactory() =
        inherit ValueObjectFactory<GivenName, string, GivenNameError>(GivenName, "givenName", Codec.string)

        override __.FormatError(error) =
            match error with
            | SomeError -> "It failed but I don't know why!"

        override __.TryParse(value) =
            if String.IsNullOrWhiteSpace value then Error SomeError
            else
                Ok(value.Trim())

    let givenName = GivenNameFactory()

    let inline (|GivenName|) (valueObject:ValueObject<GivenName, string>) =
        extract valueObject
```



## Aggregates



### Declare the model

```
module Accounts =
    type CommandForZero =
        | OpenAccount

    type EventForZero =
        | OpenedAccount

    type CommandForOpen =
        | Deposit of decimal
        | Close

    type EventForOpen =
        | Deposited of decimal
        | Closed

    type Never = Elm.Core.Basics.Never

    [<RequireQualifiedAccess>]
    type Account =
        | Zero of RootState<Account, decimal, CommandForZero, EventForZero>
        | Open of RootState<Account, decimal, CommandForOpen, EventForOpen>
        | Closed of RootState<Account, decimal, Never, Never>
```

### Starting from the Zero state
```
let zero = RootState.zero Account.Zero 0m
```

### Apply functions for the various states

Notice that each function only has to match events that are applicable to that state.

```
let zeroApply e s =
    match e.EventData with
    | OpenedAccount -> s |> RootState.evolve Account.Open s.StateData e

let openApply e s =
    match e.EventData with
    | Deposited n -> s |> RootState.evolve Account.Open (s.StateData + n) e
    | Closed -> s |> RootState.evolve Account.Closed s.StateData e
```


### Applying multiple events to the state
```
let applyEvents =
    RootState.fold
        ( fun e r ->
                match r with
                | Account.Zero s -> s.Apply(zeroApply, e)
                | Account.Open s -> s.Apply(openApply, e)
                | Account.Closed s -> failwith "There cannot be events for a closed account"
        )
```