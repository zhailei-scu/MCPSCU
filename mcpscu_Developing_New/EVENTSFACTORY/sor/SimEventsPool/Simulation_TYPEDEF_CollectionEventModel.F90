#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_COLLECTIONEVENTMODEL
    use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
    use SIMULATION_TYPEDEF_READEDEVENTSMODELS
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
    type,public::CollectionEvent
        type(ObjectsCollection),pointer::TheCollection=>null()
        type(EventModel),public::TheEventModel
        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_CollectionEvent
        Generic::Assignment(=)=>CopyCollectionEventFromOther
        Final::CleanCollectionEvent
    end type CollectionEvent


    !****Define a list CollectionEventsList with value type(CollectionEvent)**************
    DefGeneralList(CollectionEventsList,type(CollectionEvent))

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
    type,public::CollectionEventsManager
        type(CollectionEventsList),pointer::TheOriginEventList=>null()
        type(CrossEventList),pointer::TheCrossList=>null()

        type(ReadedEventsModels)::TheReadedEventsModels
        contains
        procedure,private,non_overridable,pass::Construct=>ConstructEventsManager
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCollectionEventsManager
        procedure,public,non_overridable,pass::Clean=>Clean_CollectionEventsManager
        Generic::Assignment(=)=>CopyCollectionEventsManager
        Final::CleanCollectionEventsManager
    end type CollectionEventsManager

    private::CopyEventModelFromOther
    private::Clean_EventModel
    private::CleanEventModel
    private::CopyCollectionEventFromOther
    private::Clean_CollectionEvent
    private::CleanCollectionEvent
    private::CopyCrossEventFromOther
    private::Clean_CrossEvent
    private::CleanCrossEvent
    private::ConstructEventsManager
    private::CopyCollectionEventsManager
    private::Clean_CollectionEventsManager
    private::CleanCollectionEventsManager

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
    subroutine CopyCollectionEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEvent),intent(out)::this
        CLASS(CollectionEvent),intent(in)::other
        !---Body---

        this%TheCollection => other%TheCollection
        !---The Assignment(=) had been overideded
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopyCollectionEventFromOther

    !***************************************************
    subroutine Clean_CollectionEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEvent)::this
        !---Body---
        Nullify(this%TheCollection)
        this%TheCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_CollectionEvent

    !***************************************************
    subroutine CleanCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        type(CollectionEvent)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine CleanCollectionEvent

    !****The member function of list CollectionEventsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(CollectionEventsList,type(CollectionEvent),Clean)

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
    subroutine ConstructEventsManager(this,hEventModels)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEventsManager)::this
        integer,intent(in)::hEventModels
        !---Body---
        call this%TheReadedEventsModels%Load_ReadedEventsModels(hEventModels)

        !DO 



        return
    end subroutine ConstructEventsManager


    !***************************************************
    subroutine CopyEventsManagerFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEventsManager),intent(out)::this
        CLASS(CollectionEventsManager),intent(in)::other
        !---Body---

        this%TheOriginEventList => other%TheOriginEventList
        this%TheCrossList => other%TheCrossList
        !---The Assignment(=) had been overriaded---
        this%TheReadedEventsModels = other%TheReadedEventsModels

        return
    end subroutine CopyEventsManagerFromOther

    !******************************************************
    subroutine Clean_CollectionEventsManager(this)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEventsManager)::this
        !---Body---
        call this%TheOriginEventList%CleanList()
        Nullify(this%TheOriginEventList)
        this%TheOriginEventList=>null()

        call this%TheCrossList%CleanList()
        Nullify(this%TheCrossList)
        this%TheCrossList=>null()

        call this%TheReadedEventsModels%Clean()

        return
    end subroutine Clean_CollectionEventsManager

    !******************************************************
    subroutine CleanCollectionEventsManager(this)
        implicit none
        !---Dummy Vars---
        type(CollectionEventsManager)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanCollectionEventsManager

end module SIMULATION_TYPEDEF_COLLECTIONEVENTMODEL
