namespace TimeOff
open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | RequestValidateTimeOff of TimeOffRequest
    | RequestCancelTimeOff of TimeOffRequest
    | RequestRefuseTimeOff of  TimeOffRequest
    with
    member this.UserId =
        match this with
        | RequestTimeOff request
        | RequestValidateTimeOff request
        | RequestCancelTimeOff request
        | RequestRefuseTimeOff request -> request.UserId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
//    | RequestCancelAsked of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request
//        | RequestCancelAsked request
        | RequestCancelled request
        | RequestValidated request
        | RequestRefused request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | PendingCancel of TimeOffRequest
        | Cancelled of TimeOffRequest
        | Validated of TimeOffRequest
        | Refused of TimeOffRequest
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | Cancelled request
            | PendingValidation request
            | PendingCancel request
            | Validated request
            | Refused request -> request
            
        member this.IsActive =
            match this with
            | PendingCancel _
            | PendingValidation _
            | Validated _ -> true
            | Cancelled _
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
        | RequestCancelled request when state.Request.Equals PendingValidation ||
                                        state.Request.Equals PendingCancel ->
            Cancelled request
//        | RequestCancelAsked request when state.IsActive ->
//            PendingCancel request
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

    let createRequest activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= request.Creation then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"
           
    // User
    let refuseRequest requestState =
        match requestState with
        | PendingValidation request
        | Validated request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be validated"
    
    // User
    let cancelRequestAsk requestState =
        match requestState with
        | PendingCancel request ->
            Ok [RequestValidated request]
        | Validated request
        | PendingValidation request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be cancel"
    
    // Manager
    let cancelRequestByManager requestState =
        match requestState with
        | PendingValidation request
        | PendingCancel request
        | Validated request
        | PendingCancel request ->
            Ok [RequestCancelled request]
        | _ ->
            Error "Request cannot be cancel by manager"
            
    
    let decide (userRequests: UserRequestsState) (user: User) (command: Command) =
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

                createRequest activeUserRequests request

            | RequestValidateTimeOff request ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    validateRequest requestState

            | RequestCancelTimeOff request ->
                if user <> Manager then
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    cancelRequestAsk requestState
                    
                else
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    cancelRequestByManager requestState
            
            | RequestRefuseTimeOff request ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind request.RequestId) NotCreated
                    refuseRequest requestState