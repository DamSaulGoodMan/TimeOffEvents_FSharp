module TimeOff.Tests.GWTLogic.TestRequestCommand

open TimeOff
open TimeOff.Logic
open Expecto
open System


let dateOfToday = DateTime(2019, 1, 1)

let defaultRequest = {
    UserId = "jdoe"
    RequestId = Guid.NewGuid()
    Start = { Date = DateTime(2019, 1, 2); HalfDay = PM }
    End = { Date = DateTime(2019, 1, 3); HalfDay = PM }
    Creation = DateTime(2019, 1, 1)
}

let commandResponseAccordingToStates = [
    [RequestTimeOff,
        [NotCreated, "Allowed"],
        [PendingValidation, "Unauthorized"],
        [PendingCancel, "Unauthorized"],
        [Cancelled, "Unauthorized"],
        [Validated, "Unauthorized"],
        [Refused, "Unauthorized"]
    ],
    [RequestValidateTimeOff,
        [NotCreated, "Unauthorized"],
        [PendingValidation, "Allowed"],
        [PendingCancel, "Allowed"],
        [Cancelled, "Unauthorized"],
        [Validated, "Unauthorized"],
        [Refused, "Unauthorized"]
    ],
    [RequestCancelTimeOff,
        [NotCreated, "Unauthorized"],
        [PendingValidation, "Allowed"],
        [PendingCancel, "Allowed"],
        [Cancelled, "Allowed"],
        [Validated, "Unauthorized"],
        [Refused, "Unauthorized"]
    ],
    [RequestRefuseTimeOff,
        [NotCreated, "Unauthorized"],
        [PendingValidation, "Unauthorized"],
        [PendingCancel, "Allowed"],
        [Cancelled, "Allowed"],
        [Validated, "Unauthorized"],
        [Refused, "Unauthorized"]
    ]
]


[<Tests>]
let creationTests =
    testList "Creation tests\n" [
        test "A request is created" {
            let request = {
                defaultRequest
                with UserId = "jdoe"
            }
            
            Given dateOfToday []
            |> ConnectedAs(Employee "jdoe")
            |> When(RequestTimeOff request)
            |> Then (Ok [ RequestCreated request ]) "The request should have been created"
        }
        
        test "A reques cannot be created by an other Employee" {
            let request = {
                defaultRequest
                with UserId = "toto"
            }
            
            Given dateOfToday []
            |> ConnectedAs(Employee "jdoe")
            |> When(RequestTimeOff request)
            |> Then (Error "Unauthorized") "The request shouldn't have been created"
        }
        
        test "A request cannot be created when it start in the past" {
            let request = {
                defaultRequest
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
                defaultRequest
                with
                    Start = { Date = DateTime(2019, 10, 11); HalfDay = AM }
                    End = { Date = DateTime(2019, 12, 27); HalfDay = PM }
            }
            let requestOverlapsed = {
                defaultRequest
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
            let request = defaultRequest

            Given dateOfToday [ RequestCreated request ]
            |> ConnectedAs Manager
            |> When(RequestValidateTimeOff request)
            |> Then (Ok [ RequestValidated request ]) "The request should have been validated"
        }
        
        test "A request is unvalidated if the user is not a Manager" {
            let request = defaultRequest

            Given dateOfToday [ RequestCreated request ]
            |> ConnectedAs (Employee "jdoe")
            |> When(RequestValidateTimeOff request)
            |> Then (Error "Unauthorized") "The request shouldn't have been validated"
        }
    ]