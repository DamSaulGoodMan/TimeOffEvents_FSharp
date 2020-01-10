module TimeOff.Tests.GWTLogic.TestRequestCommand

open TimeOff
open TimeOff.Logic
open Expecto
open System


let dateOfToday = DateTime(2019, 1, 1)

let defaultEmployedName = "User"
let defaultEmployed = Employee defaultEmployedName
let defaultValideRequest = {
    UserId = defaultEmployedName
    RequestId = Guid.NewGuid()
    Start = { Date = DateTime(2019, 1, 2); HalfDay = PM }
    End = { Date = DateTime(2019, 1, 3); HalfDay = PM }
    Creation = DateTime(2019, 1, 1)
}


[<Tests>]
let creationTests =
    testList "Creation tests\n" [
        test "A request is created" {
            let request = {
                defaultValideRequest
                with UserId = "jdoe"
            }
            
            Given dateOfToday []
            |> ConnectedAs(Employee "jdoe")
            |> When(RequestTimeOff request)
            |> Then (Ok [ RequestCreated request ]) "The request should have been created"
        }
        
        test "A reques cannot be created by an other Employee" {
            let request = {
                defaultValideRequest
                with UserId = "toto"
            }
            
            Given dateOfToday []
            |> ConnectedAs(Employee "jdoe")
            |> When(RequestTimeOff request)
            |> Then (Error "Unauthorized") "The request shouldn't have been created"
        }
        
        test "A request cannot be created when it start in the past" {
            let request = {
                defaultValideRequest
                with
                    Start = { Date = DateTime(2019, 1, 1); HalfDay = AM }
                    Creation = DateTime(2019, 1, 2)
            }

            Given dateOfToday []
            |> ConnectedAs(defaultEmployed)
            |> When(RequestTimeOff request)
            |> Then (Error("The request starts in the past")) "The request shouldn't have been created"
        }
        
        test "A request cannot be created when it overlaps an other one" {
            let request = {
                defaultValideRequest
                with
                    Start = { Date = DateTime(2019, 10, 11); HalfDay = AM }
                    End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
            }
            let requestOverlapsed = {
                defaultValideRequest
                with
                    Start = { Date = DateTime(2019, 10, 11); HalfDay = AM }
                    End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
            }
            
            Given dateOfToday [ RequestCreated requestOverlapsed ]
            |> ConnectedAs(defaultEmployed)
            |> When(RequestTimeOff request)
            |> Then (Error("Overlapping request")) "The request shouldn't have been created"
        }
    ]

[<Tests>]
let validationTests =
    testList "Validation tests\n" [
        test "A request is validated" {
            let request = defaultValideRequest
            Given dateOfToday [ RequestCreated request ]
            |> ConnectedAs Manager
            |> When(ValidateRequest (defaultEmployedName, request.RequestId))
            |> Then (Ok [ RequestValidated request ]) "The request should have been validated"
        }
        
        test "A request is unvalidated if the user is not a Manager" {
            let request = defaultValideRequest

            Given dateOfToday [ RequestCreated request ]
            |> ConnectedAs (defaultEmployed)
            |> When(ValidateRequest (defaultEmployedName, request.RequestId))
            |> Then (Error "Unauthorized") "The request shouldn't have been validated"
        }
    ]
    
[<CLIMutable>]
type Boundary = {
    State: RequestState
    event: RequestEvent
}

//[PendingValidation, RequestCancelAsked]
//    [PendingCancel, RequestCancelAsked]
//    [Validated, RequestCancelAsked] 
//    [Refused, Invalid]
    
//let commandResponseForUser = [|
//    {State = NotCreated; event = RequestCreated} 
//|]


[<Tests>]
let responseForUserCommand =
    testList "Employee ask for cancel request\n" [
        test "For a request in pending validation state" {
                Given dateOfToday [ RequestCreated defaultValideRequest ]
                |> ConnectedAs (defaultEmployed)
                |> When(AskCancelRequest (defaultEmployedName, defaultValideRequest.RequestId))
                |> Then (Ok [ RequestRefused defaultValideRequest ]) "The request is cancel"
        }
        test "For a request in pending cancelation state" {
                Given dateOfToday [ RequestCreated defaultValideRequest;
                                    RequestValidated defaultValideRequest;
                                    RequestCancelAsked defaultValideRequest]
                |> ConnectedAs (defaultEmployed)
                |> When(AskCancelRequest (defaultEmployedName, defaultValideRequest.RequestId))
                |> Then (Ok [ RequestValidated defaultValideRequest ]) "The cancel asked is invalidated"
        }
        test "For a validated request" {
                Given dateOfToday [ RequestCreated defaultValideRequest;
                                    RequestValidated defaultValideRequest ]
                |> ConnectedAs (defaultEmployed)
                |> When(AskCancelRequest (defaultEmployedName, defaultValideRequest.RequestId))
                |> Then (Ok [ RequestCancelAsked defaultValideRequest ]) "The request is pending admin validation"
        }
        (*test "For a refused requesy" {
                Given dateOfToday [ RequestCreated defaultValideRequest
                                    RequestRefused defaultValideRequest ]
                |> ConnectedAs (defaultEmployed)
                |> When(AskCancelRequest defaultValideRequest)
                |> Then (Ok [ defaultValideRequest ]) "The request cannot be cancel"
        }*)
    ]

(*let commandResponseForManager = [
    [ValidateRequest,
        [NotCreated, Invalid],
        [PendingValidation, RequestValidated],
        [PendingCancel, RequestValidated],
        [Validated, Invalid],
        [refusedCase, Invalid]
    ],
    [RefuseRequest,
        [NotCreated, Invalid],
        [PendingValidation, RequestRefused],
        [PendingCancel, RequestRefused],
        [Validated, RequestRefused],
        [Refused, Invalid]
    ]
]*)