#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_COLLECTIONEVENTMANAGER
    use COMMONLIB_TYPEDEF_COLLECTIONEVENT
    use SIMULATION_TYPEDEF_READEDEVENTSMODELS
    use MC_Method_MIGCOALE_CLUSTER_GPU
    implicit none

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


    private::ConstructEventsManager
    private::CopyCollectionEventsManager
    private::Clean_CollectionEventsManager
    private::CleanCollectionEventsManager

    contains

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
                !---Single Event---
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
                !---Cross Event---

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

end module SIMULATION_TYPEDEF_COLLECTIONEVENTMANAGER
