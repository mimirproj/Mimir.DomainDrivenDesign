namespace Mimir.DomainDrivenDesign

open Elm.Time

type MonotonicValue =
    private
    | MonotonicValue of Posix * sequenceNumber:uint64

    interface IValueObject<Posix * uint64> with
        member this.Get() =
            let (MonotonicValue(t,s)) = this
            (t,s)


[<RequireQualifiedAccess>]
module MonotonicValue =
    let create time =
        MonotonicValue(time, 0UL)

    let step (MonotonicValue (lastTime, seqNo)) nextTime =
        if lastTime > nextTime then
            MonotonicValue(lastTime, seqNo + 1UL)
        else
            MonotonicValue(nextTime, 0UL)


module MonotonicSequence =
    let private atom =
        MonotonicValue.create (Time.now())
        |> Atom.create

    let next() =
        atom
        |> Atom.swap(fun last ->
            MonotonicValue.step last (Time.now())
        )

