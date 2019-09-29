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
}

[<CLIMutable>]
type TimeOffCancel = {
    UserId: UserId
    RequestId: Guid
}

[<CLIMutable>]
type TimeOffRequestCancel = {
    UserId: UserId
    RequestId: Guid
}


// Manager
[<CLIMutable>]
type TimeOffValidate = {
    UserId: UserId
    RequestId: Guid
    Manager: User
}

[<CLIMutable>]
type TimeOffRefuse = {
    UserId: UserId
    RequestId: Guid
    Manager: User
}

[<CLIMutable>]
type TimeOffRefuseCancel = {
    UserId: UserId
    RequestId: Guid
    Manager: User
}

