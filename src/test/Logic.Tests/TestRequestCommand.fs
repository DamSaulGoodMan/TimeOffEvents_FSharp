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

    
let requestCannotBeValidate request =
    Error "Request cannot be validate"
    
let cannotAskToCancelRequestByUser request =
    Error "Cannot ask to cancel request"

let requestCannotBeRefuse request =
    Error "Request cannot be refuse"

let commandResponseForUser = [
    [ValidateTimeOff,
        [NotCreated, requestCannotBeValidate],
        [PendingValidation, requestCannotBeValidate],
        [PendingCancel, RequestValidated],
        [Validated, requestCannotBeValidate],
        [Refused, requestCannotBeValidate]
    ],
    [AskCancelTimeOff,
        [NotCreated, cannotAskToCancelRequestByUser],
        [PendingValidation, cannotAskToCancelRequestByUser],
        [PendingCancel, cannotAskToCancelRequestByUser],
        [Validated, RequestCancelAsked],
        [Refused, cannotAskToCancelRequestByUser]
    ],
    [RefuseTimeOff,
        [NotCreated, requestCannotBeRefuse],
        [PendingValidation, requestCannotBeRefuse],
        [PendingCancel, RequestRefused],
        [Validated, requestCannotBeRefuse],
        [Refused, requestCannotBeRefuse]
    ]
]


let commandResponseForManager = [
    [ValidateTimeOff,
        [NotCreated, requestCannotBeValidate],
        [PendingValidation, RequestValidated],
        [PendingCancel, RequestValidated],
        [Validated, requestCannotBeValidate],
        [Refused, requestCannotBeValidate]
    ],
    [RefuseTimeOff,
        [NotCreated, requestCannotBeRefuse],
        [PendingValidation, RequestRefused],
        [PendingCancel, RequestRefused],
        [Validated, RequestRefused],
        [Refused, requestCannotBeRefuse]
    ]
]

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
            |> When(ValidateTimeOff request)
            |> Then (Ok [ RequestValidated request ]) "The request should have been validated"
        }
        
        test "A request is unvalidated if the user is not a Manager" {
            let request = defaultValideRequest

            Given dateOfToday [ RequestCreated request ]
            |> ConnectedAs (Employee "jdoe")
            |> When(ValidateTimeOff request)
            |> Then (Error "Unauthorized") "The request shouldn't have been validated"
        }
    ]