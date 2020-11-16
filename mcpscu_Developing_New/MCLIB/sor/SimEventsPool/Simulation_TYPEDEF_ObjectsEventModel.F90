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
        type(ObjectsEventList),pointer::TheList=>null()
        type(CrossEventList),pointer::TheCrossList=>null()
        contains

        procedure,public,non_overridable,pass::RegisteOne=>DoRegist
        procedure,private,non_overridable,pass::ConstructCrossEvent
    end type




    #define GeneralList(Name,ValueType) type,public::Name \
                              type(Name),pointer::next=>null()  \
                              ValueType::Thevalue               \
                              end type

    GeneralList(CrossEventList2,CrossEvent)

    type(CrossEventList2)::stk

    contains





end module SIMULATION_TYPEDEF_OBJECTSEVENTMODEL
