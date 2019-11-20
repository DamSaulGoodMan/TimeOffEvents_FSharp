namespace TimeOff
open System


// First, we define our domain
type UserId = string

type User =
    | Employee of UserId
    | Manager

type HalfDay = | AM | PM

[<CLIMutable>]
type Boundary = {
    Date: DateTime
    HalfDay: HalfDay
}


// Employed
[<CLIMutable>]
type TimeOffRequest = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    End: Boundary
    Creation: DateTime
}

[<CLIMutable>]
type UpdateRequestState = {
    UserId: UserId
    RequestId: Guid
    Start: Boundary
    Creation: DateTime
}

[<CLIMutable>]
type TimeOffRefuse = {
    UserId: UserId
    RequestId: Guid
    Creation: DateTime
}