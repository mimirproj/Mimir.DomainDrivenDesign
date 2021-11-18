namespace Mimir.DomainDrivenDesign

open System
open System.Collections
open System.Collections.Generic
open System.Globalization
open Elm.Time
open Mimir.Jsonic

type MonotonicValue =
    private
    | MonotonicValue of Posix * sequenceNumber:uint64

    with
        override this.ToString() =
            let (MonotonicValue (lastTime, seqNo)) = this
            sprintf "%X-%X" (Time.posixToMillis lastTime) seqNo


module Posix =
    let codec:Codec<Posix> =
        Codec.object Time.millisToPosix
        |> Codec.field "posixMs" Time.posixToMillis Codec.int64
        |> Codec.buildObject


[<RequireQualifiedAccess>]
module MonotonicValue =
    let toTime (MonotonicValue (time, _)) =
        time

    let toSeqNo (MonotonicValue (_, seqNo)) =
        seqNo

    let codec: Codec<MonotonicValue> =
        Codec.object (fun posix seqNo -> MonotonicValue(posix, seqNo))
        |> Codec.field "posix" toTime Posix.codec
        |> Codec.field "seqNo" toSeqNo Codec.uint64
        |> Codec.buildObject


    let create time =
        MonotonicValue(time, 0UL)


    let tryParse (value:string) =
        if isNull value then None
        else
            match value.Split('-') with
            | [| msPart; seqPart |] ->
                let (parsedMs, ms) = Int64.TryParse(msPart, NumberStyles.HexNumber, CultureInfo.InvariantCulture)
                let (parsedSq, sq) = UInt64.TryParse(seqPart, NumberStyles.HexNumber, CultureInfo.InvariantCulture)

                if parsedMs && parsedSq then
                    MonotonicValue(Time.millisToPosix ms, sq)
                    |> Some

                else
                    None

            | _ ->
                None


    let step nextTime (MonotonicValue (lastTime, seqNo)) =
        if lastTime >= nextTime then
            MonotonicValue(lastTime, seqNo + 1UL)
        else
            MonotonicValue(nextTime, 0UL)


type MonotonicSequence(?lastValue) =
    let atom =
        lastValue
        |> Option.defaultWith(fun _ -> MonotonicValue.create (Time.now()))
        |> Atom.create

    let next() =
        atom
        |> Atom.swap(MonotonicValue.step (Time.now()))


    let seq = Seq.initInfinite(fun _ -> next())


    interface IEnumerable with
        member self.GetEnumerator() =
            seq.GetEnumerator()

    interface IEnumerable<MonotonicValue> with
        member self.GetEnumerator() =
            seq.GetEnumerator()

    member __.Next() =
        next()