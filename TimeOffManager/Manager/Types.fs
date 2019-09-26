module TimeOffManager.Manager.Events

open System

// Types
type RequestId = {
    Id: int
    EmployedId: int
}

type DayDate = {
    Day: DateTime
    HalfDay: bool
}

module Manager =
    type AcceptHolidays = {
        Id: RequestId
    }
    type RefuseRequestCancel = {
        Id: RequestId
    }
    type RefuseHolidays = {
        Id: RequestId
    }
        
module Employed =
    type RequestHolidays = {
        Id: RequestId
        FirstDayOfVacation: DayDate
        LastDayOfVacation: DayDate
    }
    type RequestCancelHolidays = {
        Id: RequestId
    }
    type CancelHolidays = {
        Id: RequestId
    }


// Events
module Event =
     type HolidaysRequested = {
        Id: RequestId
        FirstDayOfVacation: DayDate
        LastDayOfVacation: DayDate
    }

type Event =
    | HolidaysAccepted of Manager.AcceptHolidays
    | HolidaysRefused of Manager.RefuseHolidays
    | CancelRequestRefused of Manager.RefuseRequestCancel
    | HolidaysRequested of Employed.RequestHolidays
    | HolidaysCancelRequested of Employed.RequestCancelHolidays
    | HolidaysCancelled of Employed.CancelHolidays


// Commands
type Command =
    | AcceptHolidays of Manager.AcceptHolidays
    | RefuseHolidays of Manager.RefuseHolidays
    | RefuseRequestCancel of Manager.RefuseRequestCancel
    | RequestHolidays of Employed.RequestHolidays
    | RequestCancelHolidays of Employed.RequestCancelHolidays
    | CancelHolidays of Employed.CancelHolidays
//    | AddHolidays