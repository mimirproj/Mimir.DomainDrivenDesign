module Mimir.DomainDrivenDesign.Aggregates

open EventSourcing
open FSharpPlus
open FSharpPlus.Data


module internal Internals =
    let eventIdFactory = EventIdentifierFactory()
    let executionIdFactory = ExecutionIdentifierFactory()

open Internals

[<NoComparison; NoEquality>]
type RootState<'root, 'stateData, 'commandData, 'eventData> =
    private {
        /// The identifier of the current execution cycle.
        /// Events produced should reference this identifier.
        /// After an event is applied, this identifier should be stepped, this is done by `evolve`.
        ExecutionId: ExecutionIdentifier

        LastEventId: EventIdentifier option

        Bind: RootState<'root, 'stateData, 'commandData, 'eventData> -> 'root

        StateData: 'stateData
    }

    member state.Execute ( execute:'commandData ->'stateData -> 'eventData
                         , command:'commandData, ?causationId, ?correlationId
                         ) =

        { EventId = eventIdFactory.Next(state.LastEventId)
          ExecutionId = executionIdFactory.Next(Some state.ExecutionId)
          CausationId = causationId
          CorrelationId = correlationId
          EventData = execute command state.StateData
        }

    member state.Apply ( apply:ReadEvent<'eventData> -> RootState<'root, 'stateData, 'commandData, 'eventData> -> 'root
                       , event:ReadEvent<'eventData>
                       ) : 'root =

        if event.ExecutionId <> state.ExecutionId then
            failwith $"Event with '%A{event.EventId}' was produced at '{event.ExecutionId}' but it is currently '%A{state.ExecutionId}'. They should be equal!"

        apply event state


    member state.Apply ( apply:ReadEvent<'eventData> -> RootState<'root, 'stateData, 'commandData, 'eventData> -> 'root
                       , event:IReadEvent
                       ) : 'root =


        match event with
        | :? ReadEvent<'eventData> as event ->
            state.Apply(apply, event)

        | _ ->
            let expectedEventData = typeof<'eventData>

            failwith $"ReadEvent with '%A{event.EventId}' couldn't be applied to state with '{state.ExecutionId}', the event type should be 'ReadEvent<{expectedEventData.FullName}>'!"


[<RequireQualifiedAccess>]
module RootState =
    let zero bind zeroVal : RootState<'root, _, 'commandData, 'eventData> =
        { ExecutionId = executionIdFactory.Create(0UL)
          LastEventId = None
          Bind = bind
          StateData = zeroVal
        }

    let evolve (bind:RootState<'root, 's2, 'c2, 'e2> -> 'root)
               (stateData)
               (event:IReadEvent)
               (lastState:RootState<'root, 's1, 'c1, 'e1>) : 'root =

        { ExecutionId = executionIdFactory.Next(Some lastState.ExecutionId)
          LastEventId = Some event.EventId
          StateData = stateData
          Bind = bind
        }
        |> bind

    let fold (mapAndApply:IReadEvent -> 'root -> 'root)
             (state:'root)
             (events:IReadEvent nelist) : 'root =

        fold (fun s e -> mapAndApply e s) state events