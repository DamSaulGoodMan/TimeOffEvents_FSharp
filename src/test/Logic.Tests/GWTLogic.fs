module TimeOff.Tests.GWTLogic

open TimeOff
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