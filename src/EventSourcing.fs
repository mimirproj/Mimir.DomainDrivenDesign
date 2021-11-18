module Mimir.DomainDrivenDesign.EventSourcing

open System
open Elm.Time
open Mimir.DomainDrivenDesign
open Mimir.Jsonic


(* Stream Identifier *)
type StreamId = private | StreamId

/// Identifies a stream.
type StreamIdentifier =
    Identifier<StreamId, Guid>

[<Sealed>]
type StreamIdentifierFactory () =
    inherit IdentifierFactory<StreamId, Guid>(StreamId, "streamIdentifier", Codec.uuid)

    override __.NextValue(last) =
        Guid.NewGuid()

let inline (|StreamIdentifier|) (instance:StreamIdentifier) =
    extract instance



(* Execution Identifier *)
type ExecutionId = private | ExecutionId

/// Identifies the execution cycle which produced an event.
type ExecutionIdentifier =
    Identifier<ExecutionId, uint64>

[<Sealed>]
type ExecutionIdentifierFactory (?lastValue) =
    inherit IdentifierFactory<ExecutionId, uint64>(ExecutionId, "executionIdentifier", Codec.uint64)

    override __.NextValue(anyLast) =
        match anyLast with
        | None -> 0UL
        | Some last -> last + 1UL


let inline (|ExecutionIdentifier|) (instance:ExecutionIdentifier) =
    extract instance




(* Event Identifier *)
type EventId = private | EventId


/// Identifies an event, unique across event streams.
type EventIdentifier =
    Identifier<EventId, MonotonicValue>

[<Sealed>]
type EventIdentifierFactory () =
    inherit IdentifierFactory<EventId, MonotonicValue>(EventId, "eventIdentifier", MonotonicValue.codec)

    override __.NextValue(anyLast) =
        let now = Time.now()

        match anyLast with
        | None ->
            MonotonicValue.create now

        | Some last ->
            last
            |> MonotonicValue.step now


let inline (|EventIdentifier|) (instance:EventIdentifier) =
    extract instance



(* Causation Identifier *)
type CausationId = private | CausationId

type CausationIdentifier =
    Identifier<CausationId, Guid>

[<Sealed>]
type CausationIdentifierFactory () =
    inherit IdentifierFactory<CausationId, Guid>(CausationId, "causationIdentifier", Codec.uuid)

    override __.NextValue(last) =
        Guid.NewGuid()

let inline (|CausationIdentifier|) (instance:CausationIdentifier) =
    extract instance



(* Correlation Identifier *)
type CorrelationId = private | CorrelationId

type CorrelationIdentifier =
    Identifier<CorrelationId, Guid>

[<Sealed>]
type CorrelationIdentifierFactory () =
    inherit IdentifierFactory<CorrelationId, Guid>(CorrelationId, "correlationIdentifier", Codec.uuid)

    override __.NextValue(last) =
        Guid.NewGuid()

let inline (|CorrelationIdentifier|) (instance:CorrelationIdentifier) =
    extract instance



type Event<'eventData> =
    { /// The unique identifier of this event, unique across event streams.
      EventId: EventIdentifier

      /// The identifier of the execution cycle which produced this event.
      ExecutionId: ExecutionIdentifier

      CausationId: CausationIdentifier option

      CorrelationId: CorrelationIdentifier option

      EventData: 'eventData
    }


type IReadEvent =
    abstract EventId: EventIdentifier

type ReadEvent<'eventData> =
    { /// The unique identifier of this event, unique across event streams.
      EventId: EventIdentifier

      /// The identifier of the execution cycle which produced this event.
      ExecutionId: ExecutionIdentifier

      CausationId: CausationIdentifier option

      CorrelationId: CorrelationIdentifier option

      EventData: 'eventData

      /// The identifier for the stream this event comes from.
      StreamId: StreamIdentifier
    }

    interface IReadEvent with
        member this.EventId = this.EventId



[<RequireQualifiedAccess>]
module Event =
    /// Convert an `Event` into an `ReadEvent` by associating it with a stream.
    let inline asRead streamId (event:Event<'eventData>) : ReadEvent<'eventData> =
        {
            EventId = event.EventId
            ExecutionId = event.ExecutionId
            CausationId = event.CausationId
            CorrelationId = event.CorrelationId
            EventData = event.EventData
            StreamId = streamId
        }



