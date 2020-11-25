#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_COLLECTIONEVENTMODEL
    use COMMONLIB_TYPEDEF_EVENTMODEL
    use SIMULATION_TYPEDEF_READEDEVENTSMODELS
    use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
    implicit none

    !****************************************************************************
    type,public::SingleCollectionEvent
        type(ObjectsCollection),pointer::TheCollection=>null()
        type(EventModel),public::TheEventModel
        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopySingleCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_SingleCollectionEvent
        Generic::Assignment(=)=>CopySingleCollectionEventFromOther
        Final::CleanSingleCollectionEvent
    end type SingleCollectionEvent

    !****Define a list SingleCollectionEventsList with value type(SingleCollectionEvent)**************
    DefGeneralList(SingleCollectionEventsList,type(SingleCollectionEvent))

    !****************************************************************************
    type,public::CrossCollectionEvent
        type(ObjectsCollection),pointer::TheLeftObjectsCollection=>null()
        type(ObjectsCollection),pointer::TheRightObjectsCollection=>null()
        type(EventModel),public::TheEventModel

        contains
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCrossCollectionEventFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_CrossCollectionEvent
        Generic::Assignment(=)=>CopyCrossCollectionEventFromOther
        Final::CleanCrossCollectionEvent
    end type CrossCollectionEvent

    !****Define a list CrossCollectionEventsList with value type(CrossCollectionEvent)**************
    DefGeneralList(CrossCollectionEventsList,type(CrossCollectionEvent))

    !**************************************************************************
    type,public::CollectionEventsManager
        type(SingleCollectionEventsList)::TheSingleEventsList
        type(CrossCollectionEventsList)::TheCrossCollectionEventsList

        type(ReadedEventsModels)::TheReadedEventsModels
        contains
        procedure,private,non_overridable,pass::Construct=>ConstructEventsManager
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCollectionEventsManager
        procedure,public,non_overridable,pass::Clean=>Clean_CollectionEventsManager
        Generic::Assignment(=)=>CopyCollectionEventsManager
        Final::CleanCollectionEventsManager
    end type CollectionEventsManager

    private::CopySingleCollectionEventFromOther
    private::Clean_SingleCollectionEvent
    private::CleanSingleCollectionEvent
    private::CopyCrossCollectionEventFromOther
    private::Clean_CrossCollectionEvent
    private::CleanCrossCollectionEvent
    private::ConstructEventsManager
    private::CopyCollectionEventsManager
    private::Clean_CollectionEventsManager
    private::CleanCollectionEventsManager

    contains

    !***************************************************
    subroutine CopySingleCollectionEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(SingleCollectionEvent),intent(out)::this
        CLASS(SingleCollectionEvent),intent(in)::other
        !---Body---

        this%TheCollection => other%TheCollection
        !---The Assignment(=) had been overideded
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopySingleCollectionEventFromOther

    !***************************************************
    subroutine Clean_SingleCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(SingleCollectionEvent)::this
        !---Body---
        Nullify(this%TheCollection)
        this%TheCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_SingleCollectionEvent

    !***************************************************
    subroutine CleanSingleCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        type(SingleCollectionEvent)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine CleanSingleCollectionEvent

    !****The member function of list SingleCollectionEventsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(SingleCollectionEventsList,type(SingleCollectionEvent),Clean)

    !***************************************************
    subroutine CopyCrossCollectionEventFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CrossCollectionEvent),intent(out)::this
        CLASS(CrossCollectionEvent),intent(in)::other
        !---Body---

        this%TheLeftObjectsCollection => other%TheLeftObjectsCollection
        this%TheRightObjectsCollection => other%TheRightObjectsCollection
        this%TheEventModel = other%TheEventModel

        return
    end subroutine CopyCrossCollectionEventFromOther

    !***************************************************
    subroutine Clean_CrossCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        CLASS(CrossCollectionEvent)::this
        !---Body---
        Nullify(this%TheLeftObjectsCollection)
        this%TheLeftObjectsCollection => null()

        Nullify(this%TheRightObjectsCollection)
        this%TheRightObjectsCollection => null()

        call this%TheEventModel%Clean()
        return
    end subroutine Clean_CrossCollectionEvent

    !***************************************************
    subroutine CleanCrossCollectionEvent(this)
        implicit none
        !---Dummy Vars---
        type(CrossCollectionEvent)::this
        !---Local Vars---
        call this%Clean()
        return
    end subroutine CleanCrossCollectionEvent

    !****The member function of list CrossCollectionEventsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(CrossCollectionEventsList,type(CrossCollectionEvent),Clean)

    !***************************************************
    subroutine ConstructEventsManager(this,hEventModels)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEventsManager)::this
        integer,intent(in)::hEventModels
        !---Local Vars---
        integer::I
        integer::ReadedPairsNum
        type(EventsModelsPair)::tempEventsModelsPair
        !---Body---
        call this%TheReadedEventsModels%Load_ReadedEventsModels(hEventModels)

        ReadedPairsNum = this%TheReadedEventsModels%TheEventsModelsPairList%GetListCount()

        DO I = 1, ReadedPairsNum
            tempEventsModelsPair = this%TheReadedEventsModels%TheEventsModelsPairList%GetValueByListIndex(I)

            if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()) then
                if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .ne. tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()) then
                    write(*,*) "MCPSCUERROR: It is impossible that the cross event is not same with subject and object events when subject and object events is same."
                    write(*,*) "Subject: ",adjustl(trim(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Object: ",adjustl(trim(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Cross: ",adjustl(trim(tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                !this%TheSingleEventsList%
            else

            end if
        END DO

        return
    end subroutine ConstructEventsManager

    !***************************************************
    subroutine CopyEventsManagerFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CollectionEventsManager),intent(out)::this
        CLASS(CollectionEventsManager),intent(in)::other
        !---Body---

        this%TheSingleEventsList = other%TheSingleEventsList
        this%TheCrossCollectionEventsList = other%TheCrossCollectionEventsList
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
        call this%TheSingleEventsList%CleanList()

        call this%TheCrossCollectionEventsList%CleanList()

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
