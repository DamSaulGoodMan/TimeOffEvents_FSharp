module TimeOff.Tests.GWTLogic.TestRequestCommand

open TimeOff
open TimeOff.Logic
open Expecto
open System


let dateOfToday = DateTime(2019, 1, 1)

let defaultValideRequest = {
    UserId = "jdoe"
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
            |> ConnectedAs(Employee "jdoe")
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
            |> ConnectedAs(Employee "jdoe")
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
            |> When(ValidateRequest request)
            |> Then (Ok [ RequestValidated request ]) "The request should have been validated"
        }
        
        test "A request is unvalidated if the user is not a Manager" {
            let request = defaultValideRequest

            Given dateOfToday [ RequestCreated request ]
            |> ConnectedAs (Employee "jdoe")
            |> When(ValidateRequest request)
            |> Then (Error "Unauthorized") "The request shouldn't have been validated"
        }
    ]
    
[<CLIMutable>]
type Boundary = {
    State: RequestState
    taputedemere: RequestEvent
}

(*[PendingValidation, RequestCancelAsked]
    [PendingCancel, RequestCancelAsked]
    [Validated, RequestCancelAsked] 
    [Refused, Invalid]
    
let commandResponseForUser = [|
    {State = NotCreated; taputedemere = RequestCreated} 
|]

[<Tests>]
let responseForUserCommand =
    testList "User request responses tests\n" [
        test "A request is validated" {
            for requestCell in commandResponseForUser do
                let request = defaultValideRequest

                Given dateOfToday [ RequestCancelAsked request ]
                |> ConnectedAs (Employee "Toto")
                |> When(requestCell.State request)
                |> Then (Ok [ requestCell.[1] request ]) ""
        }
    ] *)
    


let commandResponseForManager = [
    [ValidateRequest,
        [NotCreated, Invalid],
        [PendingValidation, RequestValidated],
        [PendingCancel, RequestValidated],
        [Validated, Invalid],
        [Refused, Invalid]
    ],
    [RefuseRequest,
        [NotCreated, Invalid],
        [PendingValidation, RequestRefused],
        [PendingCancel, RequestRefused],
        [Validated, RequestRefused],
        [Refused, Invalid]
    ]
]