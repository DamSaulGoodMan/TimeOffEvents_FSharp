namespace TimeOff
open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | ValidateTimeOff of TimeOffRequest
    | AskCancelTimeOff of TimeOffRequest
    | RefuseTimeOff of  TimeOffRequest
    with
    member this.UserId =
        match this with
        | RequestTimeOff request
        | ValidateTimeOff request
        | AskCancelTimeOff request
        | RefuseTimeOff request -> request.UserId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestCancelAsked of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request
        | RequestCancelAsked request
        | RequestValidated request
        | RequestRefused request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | PendingCancel of TimeOffRequest
        | Validated of TimeOffRequest
        | Refused of TimeOffRequest
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | PendingCancel request
            | Validated request
            | Refused request -> request
            
        member this.IsActive =
            match this with
            | PendingCancel _
            | PendingValidation _
            | Validated _ -> true
            | NotCreated _
            | Refused _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest (state: RequestState) (event: RequestEvent) =
        match event with
        | RequestCreated request when state.Equals NotCreated ->
            PendingValidation request
        | RequestValidated request when state.Request.Equals PendingValidation ||
                                        state.Request.Equals PendingCancel ->
            Validated request
        | RequestRefused request when state.IsActive ->
            Refused request
        | RequestCancelAsked request when state.Request.Equals PendingValidation ||
                                        state.Request.Equals PendingCancel ->
            PendingCancel request
        | _ -> state

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest) =    
        if DateTime.Compare(request1.End.Date, request2.Start.Date) < 0 ||
            DateTime.Compare(request1.Start.Date, request2.End.Date) > 0
            then false
        elif DateTime.Compare(request1.End.Date, request2.Start.Date) = 0 &&
            ((request1.End.HalfDay = HalfDay.AM) && (request2.Start.HalfDay = HalfDay.PM))
            then false
        elif DateTime.Compare(request1.Start.Date, request2.End.Date) = 0 &&
            ((request1.Start.HalfDay = HalfDay.PM) && (request2.End.HalfDay = HalfDay.AM))
            then false
        else
            true

    let rec overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) (request: TimeOffRequest) =
        if otherRequests |> Seq.length = 0
            then false
        elif (overlapsWith (otherRequests |> Seq.item 0) request) = true
            then true
        else overlapsWithAnyRequest (otherRequests |> Seq.skip 1) request

    let createRequest (today: DateTime) activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequestByAdmin requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validate"
           
    let validateRequestByUser requestState =
        match requestState with
        | PendingCancel request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validate"
           
    let refuseRequestByAdmin requestState =
        match requestState with
        | PendingValidation request
        | Validated request
        | PendingCancel request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refuse"
            
    let refuseRequestByUser requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestRefused request]
        | _ ->
            Error "Request cannot be refuse"
    
    let cancelRequestAsk requestState =
        match requestState with
        | Validated request ->
            Ok [RequestCancelAsked request]
        | _ ->
            Error "Cannot ask to cancel request"
            
    
    let decide (today: DateTime) (userRequests: UserRequestsState) (user: User) (command: Command) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest today activeUserRequests request

            | ValidateTimeOff request ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    validateRequestByUser requestState
                else
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    validateRequestByAdmin requestState

            | RefuseTimeOff request ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    refuseRequestByUser requestState
                else
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    refuseRequestByAdmin requestState
            
            | AskCancelTimeOff request ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    cancelRequestAsk requestState