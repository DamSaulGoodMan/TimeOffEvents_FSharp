module TimeOff.Tests.GWTLogic.TestRequestTime

open TimeOff
open Expecto
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