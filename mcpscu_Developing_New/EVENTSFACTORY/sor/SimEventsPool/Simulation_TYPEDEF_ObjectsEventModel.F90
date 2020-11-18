module SIMULATION_TYPEDEF_OBJECTSEVENTMODEL
    use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
    implicit none

    abstract interface
        subroutine EventModel()

        end subroutine EventModel
    end interface

    type,public::ObjectsEvent
        !integer(kind=KMCLINT)::ObjectCode PREASSIGN 0
        !integer(kind=KMCLINT)::EventCode PREASSIGN 0
        type(ObjectsCollection),pointer::TheObjectsCollection=>null()
        procedure(EventModel),pointer,nopass::TheEventsModel=>null()
        contains
    end type ObjectsEvent


    type,public::ObjectsEventList
        type(ObjectsEventList),pointer::next=>null()
        type(ObjectsEvent)::TheValue
        contains
    end type ObjectsEventList


    type,public::CrossEvent
        !integer(kind=KMCLINT)::ObjectCode PREASSIGN 0
        !integer(kind=KMCLINT)::EventCode PREASSIGN 0
        type(ObjectsCollection),pointer::TheLeftObjectsCollection=>null()
        type(ObjectsCollection),pointer::TheRightObjectsCollection=>null()
        procedure(EventModel),pointer,nopass::TheEventsModel=>null()
        contains
    end type CrossEvent

    type,public::CrossEventList
        type(CrossEventList),pointer::next=>null()
        type(CrossEvent)::TheValue
        contains
    end type CrossEventList

    type,public::ObjectsEventManager
        type(ObjectsEventList),pointer::TheOriginEventList=>null()
        type(CrossEventList),pointer::TheCrossList=>null()
        contains

        procedure,public,non_overridable,pass::RegisteOne=>DoRegist
        procedure,private,non_overridable,pass::ConstructCrossEvent
    end type


    private::RegisteOne
    private::ConstructCrossEvent

    contains

    subroutine RegisteOne(this,TheObjectsEvent)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEventManager)::this
        type(ObjectsEvent)::TheObjectsEvent
        !---Local Vars---
        !---Body---

        if(.not. associated(this%TheOriginEventList)) then
            allocate(this%TheOriginEventList)
        end if

        !this%TheOriginEventList%AppendOne(ObjectsEvent)

        call this%ConstructCrossEvent(TheObjectsEvent)

        return
    end subroutine RegisteOne

    subroutine ConstructCrossEvent(this,TheObjectsEvent)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEventManager)::this
        type(ObjectsEvent)::TheObjectsEvent
        !---Local Vars---
        !---Body---
        return
    end subroutine ConstructCrossEvent


end module SIMULATION_TYPEDEF_OBJECTSEVENTMODEL
