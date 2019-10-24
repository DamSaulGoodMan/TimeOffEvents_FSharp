module TimeOff.Tests

open Expecto

let Given(events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let When (command: Command) (events: RequestEvent list, user: User) = events, user, command
let Then expected message (events: RequestEvent list, user: User, command: Command) =
        let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
                let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
                let newUserState = Logic.evolveUserRequests userState event
                userStates.Add(event.Request.UserId, newUserState)

        let globalState = Seq.fold evolveGlobalState Map.empty events
        let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
        let result = Logic.decide userRequestsState user command
        Expect.equal result expected message

open System

let defaultRequest = {
    UserId = "jdoe"
    RequestId = Guid.NewGuid()
    Start = { Date = DateTime(2019, 1, 2); HalfDay = PM }
    End = { Date = DateTime(2019, 1, 3); HalfDay = PM }
    Creation = DateTime(2019, 1, 1)
}
    

[<Tests>]
let overlapTests =
    testList "Overlap tests\n" [
        test "A request overlaps with itself" {
            let request = defaultRequest
            Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with itself"
        }

        test "Requests on 2 distinct days don't overlap" {
            let request1 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 1, 2); HalfDay = PM }
                    End = { Date = DateTime(2019, 1, 3); HalfDay = PM }
            }
            let request2 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 1, 4); HalfDay = PM }
                    End = { Date = DateTime(2019, 1, 5); HalfDay = PM }
            }
            Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
        }

        test "Requests must overlap" {
            let request1 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 1, 2); HalfDay = PM }
                    End = { Date = DateTime(2019, 1, 4); HalfDay = PM }
            }

            let request2 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 1, 3); HalfDay = PM }
                    End = { Date = DateTime(2019, 1, 5); HalfDay = PM }
            }

            Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap the 10/10/2019"
        }
        
        test "Requests must overlap one half day (first day request one)" {
            let request1 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 10, 10); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 10); HalfDay = PM }
            }

            let request2 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 10, 9); HalfDay = PM }
                    End = { Date = DateTime(2019, 10, 10); HalfDay = AM }
            }

            Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap one half day"
        }
        
        test "Requests must overlap one half day (last day request one)" {
            let request1 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 10, 9); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 10); HalfDay = AM }
            }

            let request2 = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 10, 10); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 11); HalfDay = AM }
            }

            Expect.isTrue (Logic.overlapsWith request1 request2) "The requests overlap one aft day"
        }
        
        test "Request musn't overlaps with the others" {
            let requestTest = {
                defaultRequest
                with 
                    Start = { Date = DateTime(2019, 10, 9); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 10); HalfDay = AM }
            }

            let request0 = {
                defaultRequest
                with
                    Start = { Date = DateTime(2019, 10, 17); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 24); HalfDay = AM }
            }
            let request1 = {
                defaultRequest
                with
                    Start = { Date = DateTime(2019, 9, 17); HalfDay = PM }
                    End = { Date = DateTime(2019, 9, 24); HalfDay = PM }
            }

            Expect.isFalse (Logic.overlapsWithAnyRequest [request0; request1] requestTest)
                "The requests musn't overlap the others one"
        }
        
        test "Request overlaps with the others" {
            let requestTest = {
                defaultRequest
                with
                    Start = { Date = DateTime(2019, 9, 9); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 30); HalfDay = AM }
            }

            let request0 = {
                defaultRequest
                with
                    Start = { Date = DateTime(2019, 10, 17); HalfDay = AM }
                    End = { Date = DateTime(2019, 10, 24); HalfDay = AM }
            }
            let request1 = {
                defaultRequest
                with
                    Start = { Date = DateTime(2019, 9, 17); HalfDay = PM }
                    End = { Date = DateTime(2019, 9, 24); HalfDay = PM }
            }

            Expect.isTrue (Logic.overlapsWithAnyRequest [request0; request1] requestTest)
                "The requests overlap the others one"
        }
    ]

[<Tests>]
let creationTests =
    testList "Creation tests\n" [
        test "A request is created" {
            let request = {
                defaultRequest
                with UserId = "jdoe"
            }
            
            Given []
            |> ConnectedAs(Employee "jdoe")
            |> When(RequestTimeOff request)
            |> Then (Ok [ RequestCreated request ]) "The request should have been created"
        }
        
        test "A reques cannot be created by an other Employee" {
            let request = {
                defaultRequest
                with UserId = "toto"
            }
            
            Given []
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

            Given []
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
            
            Given [ RequestCreated requestOverlapsed ]
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

            Given [ RequestCreated request ]
            |> ConnectedAs Manager
            |> When(RequestValidateTimeOff request)
            |> Then (Ok [ RequestValidated request ]) "The request should have been validated"
        }
        
        test "A request is unvalidated if the user is not a Manager" {
            let request = defaultRequest

            Given [ RequestCreated request ]
            |> ConnectedAs (Employee "jdoe")
            |> When(RequestValidateTimeOff request)
            |> Then (Error "Unauthorized") "The request shouldn't have been validated"
        }
    ]
