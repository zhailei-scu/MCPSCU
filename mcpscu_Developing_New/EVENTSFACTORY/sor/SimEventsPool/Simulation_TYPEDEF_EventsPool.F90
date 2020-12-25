module SIMULATION_TYPEDEF_EVENTSPOOL
    use MC_CollectionEvent_MIGCOALE_CLUSTER_GPU
    use SIMULATION_TYPEDEF_DATAPOOL
    implicit none

    type(MC_MIGCOALE_CLUSTER_GPU),pointer,public::m_MC_MIGCOALE_CLUSTER_GPU=>null()

    type,public::CollectionEventRegister
        Class(CollectionEvent),pointer::TheCollectionEvent=>null()
        character*100::TheName PREASSIGN ""

        contains

        procedure,public,non_overridable,pass::CopyFromOther=>CopyCollectionEventRegister
        procedure,public,non_overridable,pass::Clean=>Clean_CollectionEventRegister
        Generic::Assignment(=)=>CopyCollectionEventRegister
        Final::CleanCollectionEventRegister
    end type

    !****Define a list CollectionEventRegisterList with value class(CollectionEventRegister),pointer**************
    DefGeneralList(CollectionEventRegisterList,class(CollectionEventRegister))

    type,public::EventsPool
        type(CollectionEventRegisterList)::TheCollectionEventRegisterList
        contains
        procedure,public,non_overridable,pass::Init=>InitEventsPool
        procedure,public,non_overridable,pass::RegisterOneSingle=>RegisterOneSingleEvent
        procedure,public,non_overridable,pass::RegisterOneCross=>RegisterOneCrossEvent
        procedure,public,non_overridable,pass::CopyFromOther=>CopyEventsPool
        procedure,public,non_overridable,pass::Clean=>Clean_EventsPool
        Generic::Assignment(=)=>CopyEventsPool
        Final::CleanEventsPool
    end type

    private::CopyCollectionEventRegister
    private::Clean_CollectionEventRegister
    private::CleanCollectionEventRegister
    private::InitEventsPool
    private::RegisterOneSingleEvent
    private::RegisterOneCrossEvent
    private::CopyEventsPool
    private::Clean_EventsPool
    private::CleanEventsPool

    contains

    !**********************************************
    subroutine CopyCollectionEventRegister(this,other)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventRegister),intent(out)::this
        Class(CollectionEventRegister),intent(in)::other
        !---Body---
        this%TheCollectionEvent => other%TheCollectionEvent
        this%TheName = other%TheName
        return
    end subroutine CopyCollectionEventRegister


    !**********************************************
    subroutine Clean_CollectionEventRegister(this)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventRegister)::this
        !---Body---
        Nullify(this%TheCollectionEvent)
        this%TheCollectionEvent=>Null()
        this%TheName = ""
        return
    end subroutine Clean_CollectionEventRegister

    !*********************************************
    subroutine CleanCollectionEventRegister(this)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventRegister)::this
        !---Body---

        call this%Clean()
    end subroutine CleanCollectionEventRegister

    !****The member function of list CollectionEventRegisterList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(CollectionEventRegisterList,class(CollectionEventRegister),Clean)

    !**********************************************
    subroutine InitEventsPool(this)
        implicit none
        !---Dummy Vars---
        Class(EventsPool)::this
        !---Body---

        call this%Clean()

        call this%RegisterOneSingleEvent()



        return
    end subroutine InitEventsPool

    !**********************************************
    subroutine RegisterOneSingleEvent(this,EventModelName,TheDataPool,ObjectCollectionName)
        implicit none
        !---Dummy Vars---
        Class(EventsPool)::this
        character*(*),intent(in)::EventModelName
        type(DataPool),intent(in)::TheDataPool
        character*(*),intent(in)::ObjectCollectionName
        !---Local Vars---
        type(CollectionEventRegister)::tempCollectionEventRegister
        !---Body---

        call tempCollectionEventRegister%Clean()
        tempCollectionEventRegister%TheName = adjutl(trim(EventModelName))
        
        this%TheCollectionEventRegisterList

        return
    end subroutine RegisterOneSingleEvent

    !**********************************************
    subroutine RegisterOneCrossEvent(this)
        implicit none
        !---Dummy Vars---
        Class(EventsPool)::this
        !---Body---



        return
    end subroutine RegisterOneCrossEvent

    !************************************************
    subroutine CopyEventsPool(this,other)
        implicit none
        !---Dummy Vars---
        Class(EventsPool),intent(out)::this
        Class(EventsPool),intent(in)::other
        !---Body---

        !---The Assignment(=) had been overridable
        this%TheCollectionEventRegisterList = other%TheCollectionEventRegisterList

    end subroutine CopyEventsPool

    !**********************************************
    subroutine Clean_EventsPool(this)
        implicit none
        !---Dummy Vars---
        Class(EventsPool)::this
        !---Body---

        call this%TheCollectionEventRegisterList%CleanList()

        return
    end subroutine Clean_EventsPool

    !***********************************************

    subroutine CleanEventsPool(this)
        implicit none
        !---Dummy Vars---
        type(EventsPool)::this
        !---Body---

        call this%Clean()

        return
    end subroutine CleanEventsPool
end module SIMULATION_TYPEDEF_EVENTSPOOL
