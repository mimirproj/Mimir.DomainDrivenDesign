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


[<AutoOpen>]
module ValueObjectPervasive =
    let extract = ValueObject.get

    let inline (|ValueObject|) value =
        ValueObject.get value
