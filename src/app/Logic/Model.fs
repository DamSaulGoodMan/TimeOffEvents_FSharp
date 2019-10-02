namespace TimeOff

open System

// Then our commands
type Command =
    | RequestTimeOff of TimeOffRequest
    | RequestCancelTimeOff of TimeOffRequest
    | RequestValidateTimeOff of TimeOffRequest
    | RequestRefuseTimeOff of  TimeOffRequest
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | RequestCancelTimeOff request -> request.UserId
        | RequestValidateTimeOff request -> request.UserId
        | RequestRefuseTimeOff request -> request.UserId

// And our events
type RequestEvent =
    | RequestCreated of TimeOffRequest
    | RequestCancelAsked of TimeOffRequest
    | RequestCancelled of TimeOffRequest
    | RequestValidated of TimeOffRequest
    | RequestRefused of TimeOffRequest
    | RequestRefusedCancel of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request -> request
        | RequestCancelAsked request -> request
        | RequestCancelled request -> request
        | RequestValidated request -> request
        | RequestRefused request -> request
        | RequestRefusedCancel request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =
    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Canceled of TimeOffRequest
        | Validated of TimeOffRequest
        | RefuseCancel of TimeOffRequest
        | Refused of TimeOffRequest
        with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request
            | Canceled request
            | RefuseCancel request
            | Refused request -> request
            
        member this.IsActive =
            match this with
            | NotCreated -> false
            | PendingValidation _
            | RefuseCancel _
            | Validated _ -> true
            | Canceled _
            | Refused _ -> false

    type UserRequestsState = Map<Guid, RequestState>

    let evolveRequest state event =
        match event with
        | RequestCreated request -> PendingValidation request
//        | RequestCancelAsked request -> 
        | RequestCancelled request -> Canceled request
        | RequestValidated request -> Validated request
        | RequestRefused request -> Refused request
        | RequestRefusedCancel request -> RefuseCancel request

    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        userRequests.Add (event.Request.RequestId, newRequestState)

    let overlapsWith (request1: TimeOffRequest) (request2: TimeOffRequest) =
        if DateTime.Compare(request1.End.Date, request2.Start.Date) < 0
           || DateTime.Compare(request1.Start.Date, request2.End.Date) > 0 then
            false
        else
            true

    // @TODO UN B PUTINB DE IFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    let overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) (request: TimeOffRequest) =
        for requestToCompare in otherRequests do
            if overlapsWith requestToCompare request
            then true
            else false
            
        false

    let createRequest activeUserRequests request =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        // This DateTime.Today must go away!
        elif request.Start.Date <= DateTime.Today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request ->
            Ok [RequestValidated request]
        | _ ->
            Error "Request cannot be validated"

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
