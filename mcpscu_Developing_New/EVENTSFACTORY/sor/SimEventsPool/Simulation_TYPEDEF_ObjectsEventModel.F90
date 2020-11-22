#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_OBJECTSEVENTMODEL
    use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
    implicit none

    abstract interface
        subroutine BeforeEachJobProc()

        end subroutine BeforeEachJobProc

        subroutine BeforeEachTestProc()

        end subroutine BeforeEachTestProc

        subroutine BeforeEachTimeSectionProc()

        end subroutine BeforeEachTimeSectionProc

        subroutine EachTimeStepProc()

        end subroutine EachTimeStepProc
    end interface

    !****************************************************************************
    type,public::EventModel
        character(len=20)::ModelName PREASSIGN ""
        procedure(BeforeEachJobProc),pointer,nopass::TheBeforeEachJobProc=>null()
        procedure(BeforeEachTestProc),pointer,nopass::TheBeforeEachTestProc=>null()
        procedure(BeforeEachTimeSectionProc),pointer,nopass::TheBeforeEachTimeSectionProc=>null()
        procedure(EachTimeStepProc),pointer,nopass::TheEachTimeStepProc=>null()

        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyEventModelFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_EventModel
        Generic::Assignment(=)=>CopyEventModelFromOther
        Final::CleanEventModel
    end type EventModel

    !****************************************************************************
    type,public::ObjectsEvent
        type(ObjectsCollection),pointer::TheObjectsCollection=>null()
        type(EventModel),public::TheEventModel
        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyObjectsEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_ObjectsEvent
        Generic::Assignment(=)=>CopyObjectsEventFromOther
        Final::CleanObjectsEvent
    end type ObjectsEvent


    !****Define a list ObjectsEventList with value type(ObjectsEvent)**************
    DefGeneralList(ObjectsEventList,type(ObjectsEvent))

    !****************************************************************************
    type,public::CrossEvent
        type(ObjectsCollection),pointer::TheLeftObjectsCollection=>null()
        type(ObjectsCollection),pointer::TheRightObjectsCollection=>null()
        type(EventModel),public::TheEventModel

        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCrossEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_CrossEvent
        Generic::Assignment(=)=>CopyCrossEventFromOther
        Final::CleanCrossEvent
    end type CrossEvent

    !****Define a list CrossEventList with value type(CrossEvent)**************
    DefGeneralList(CrossEventList,type(CrossEvent))

    !**************************************************************************
    type,public::ObjectsEventManager
        type(ObjectsEventList),pointer::TheOriginEventList=>null()
        type(CrossEventList),pointer::TheCrossList=>null()
        contains

        procedure,public,non_overridable,pass::CopyFromOther=>CopyObjectsEventManager
        procedure,public,non_overridable,pass::RegisteOne=>DoRegist
        procedure,private,non_overridable,pass::ConstructCrossEvent
         procedure,private,non_overridable,pass::Check=>CheckEvents
        procedure,public,non_overridable,pass::Clean=>Clean_ObjectsEventManager
        Generic::Assignment(=)=>CopyObjectsEventManager
        Final::CleanObjectsEventManager
    end type ObjectsEventManager

    private::CopyEventModelFromOther
    private::Clean_EventModel
    private::CleanEventModel
    private::CopyObjectsEventFromOther
    private::Clean_ObjectsEvent
    private::CleanObjectsEvent
    private::CopyCrossEventFromOther
    private::Clean_CrossEvent
    private::CleanCrossEvent
    private::CopyObjectsEventManager
    private::DoRegist
    private::ConstructCrossEvent
    private::CheckEvents
    private::Clean_ObjectsEventManager
    private::CleanObjectsEventManager

    contains

    !***************************************************
    subroutine CopyEventModelFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(EventModel),intent(out)::this
        CLASS(EventModel),intent(in)::other
        !---Body---
        this%ModelName = other%ModelName
        this%TheBeforeEachJobProc =>other%TheBeforeEachJobProc
        this%TheBeforeEachTestProc =>other%TheBeforeEachTestProc
        this%TheBeforeEachTimeSectionProc =>other%TheBeforeEachTimeSectionProc
        this%TheEachTimeStepProc =>other%TheEachTimeStepProc
        return
    end subroutine CopyEventModelFromOther

    !***************************************************
    subroutine Clean_EventModel(this)
        implicit none
        !---Dummy Vars---
        CLASS(EventModel)::this
        !---Body---

        this%ModelName = ""

        Nullify(this%TheBeforeEachJobProc)
        this%TheBeforeEachJobProc => null()

        Nullify(this%TheBeforeEachTestProc)
        this%TheBeforeEachTestProc => null()

        Nullify(this%TheBeforeEachTimeSectionProc)
        this%TheBeforeEachTimeSectionProc => null()

        Nullify(this%TheEachTimeStepProc)
        this%TheEachTimeStepProc => null()
        return
    end subroutine Clean_EventModel

    !***************************************************
    subroutine CleanEventModel(this)
        implicit none
        !---Dummy Vars---
        type(EventModel)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine CleanEventModel

    !***************************************************
    subroutine CopyObjectsEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEvent),intent(out)::this
        CLASS(ObjectsEvent),intent(in)::other
        !---Body---

        this%TheObjectsCollection => other%TheObjectsCollection
        !---The Assignment(=) had been overideded
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopyObjectsEventFromOther

    !***************************************************
    subroutine Clean_ObjectsEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEvent)::this
        !---Body---
        Nullify(this%TheObjectsCollection)
        this%TheObjectsCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_ObjectsEvent

    !***************************************************
    subroutine CleanObjectsEvent(this)
        implicit none
        !---Dummy Vars---
        type(ObjectsEvent)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine CleanObjectsEvent

    !****The member function of list ObjectsEventList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(ObjectsEventList,type(ObjectsEvent),Clean)

    !***************************************************
    subroutine CopyCrossEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CrossEvent),intent(out)::this
        CLASS(CrossEvent),intent(in)::other
        !---Body---

        this%TheLeftObjectsCollection => other%TheLeftObjectsCollection
        this%TheRightObjectsCollection => other%TheRightObjectsCollection
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopyCrossEventFromOther

    !***************************************************
    subroutine Clean_CrossEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(CrossEvent)::this
        !---Body---
        Nullify(this%TheLeftObjectsCollection)
        this%TheLeftObjectsCollection => null()

        Nullify(this%TheRightObjectsCollection)
        this%TheRightObjectsCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_CrossEvent

    !***************************************************
    subroutine CleanCrossEvent(this)
        implicit none
        !---Dummy Vars---
        type(CrossEvent)::this
        !---Local Vars---
        call this%Clean()
        return
    end subroutine CleanCrossEvent

    !****The member function of list CrossEventList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(CrossEventList,type(CrossEvent),Clean)

    !***************************************************
    subroutine DoRegist(this,TheObjectsEvent)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEventManager)::this
        type(ObjectsEvent)::TheObjectsEvent
        !---Local Vars---
        !---Body---

        if(.not. associated(this%TheOriginEventList)) then
            allocate(this%TheOriginEventList)
        end if

        call this%TheOriginEventList%AppendOne(ObjectsEvent)

        call this%ConstructCrossEvent(TheObjectsEvent)

        call this%Check()

        return
    end subroutine DoRegist

    !***************************************************
    subroutine ConstructCrossEvent(this,TheObjectsEvent)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEventManager)::this
        type(ObjectsEvent)::TheObjectsEvent
        !---Local Vars---
        type(ObjectsEventList),pointer::cursor=>null()
        !---Body---
        if(.not. associated(this%TheOriginEventList)) then
            return
        end if

        cursor=>this%TheOriginEventList

        Do while(associated(cursor))
            
        END Do

        return
    end subroutine ConstructCrossEvent

    !******************************************************
    subroutine Clean_ObjectsEventManager(this)
        implicit none
        !---Dummy Vars---
        CLASS(ObjectsEventManager)::this
        !---Body---
        call this%TheOriginEventList%CleanList()
        Nullify(this%TheOriginEventList)
        this%TheOriginEventList=>null()

        call this%TheCrossList%CleanList()
        Nullify(this%TheCrossList)
        this%TheCrossList=>null()

        return
    end subroutine Clean_ObjectsEventManager

    !******************************************************
    subroutine CleanObjectsEventManager(this)
        implicit none
        !---Dummy Vars---
        type(ObjectsEventManager)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanObjectsEventManager

end module SIMULATION_TYPEDEF_OBJECTSEVENTMODEL
