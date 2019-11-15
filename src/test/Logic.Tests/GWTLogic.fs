namespace TimeOff.Tests.GWTLogic

open TimeOff
open Expecto
open System

[<AutoOpen>]
module Setup =
        let Given (today: DateTime)(events: RequestEvent list) = today, events
        let ConnectedAs (user: User) (today: DateTime, events: RequestEvent list) = today, events, user
        let When (command: Command) (today: DateTime, events: RequestEvent list, user: User) = today, events, user, command
        let Then expected message (today: DateTime, events: RequestEvent list, user: User, command: Command) =
                let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
                        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
                        let newUserState = Logic.evolveUserRequests userState event
                        userStates.Add(event.Request.UserId, newUserState)

                let globalState = Seq.fold evolveGlobalState Map.empty events
                let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
                let result = Logic.decide today userRequestsState user command
                Expect.equal result expected message