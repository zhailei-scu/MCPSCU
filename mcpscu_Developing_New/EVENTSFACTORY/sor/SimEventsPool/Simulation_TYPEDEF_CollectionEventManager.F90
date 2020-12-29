#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_COLLECTIONEVENTMANAGER
    use COMMONLIB_TYPEDEF_COLLECTIONEVENT
    use SIMULATION_TYPEDEF_READEDEVENTSMODELS
    use SIMULATION_TYPEDEF_DATARELATIONPOOL
    use SIMULATION_TYPEDEF_COLLECTIONEVENTSREGISTERCENTER
    implicit none

    !**************************************************************************
    type,public::CollectionEventsManager
        type(SingleCollectionEventsList_P)::TheSingleEventsList
        type(CrossCollectionEventsList_P)::TheCrossCollectionEventsList

        type(ReadedEventsModels),private::TheReadedEventsModels
        type(DataRelationPool),private::TheDataRelationPool
        type(CollectionEventsRegisterCenter),private::TheCollectionEventsRegisterCenter
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
        Class(SingleCollectionEventsList_P),pointer::cursorSingleCollectionEvent=>null()
        Class(CrossCollectionEventsList_P),pointer::cursorCrossCollectionEvent=>null()
        character*100::tempEventModelSymbol
        type(CollectionEventRegister)::tempCollectionEventRegister
        !---Body---
        call this%TheReadedEventsModels%Load_ReadedEventsModels(hEventModels)

        call this%TheDataRelationPool%Init()

        call this%TheCollectionEventsRegisterCenter%Init()

        ReadedPairsNum = this%TheReadedEventsModels%TheEventsModelsPairList%GetListCount()

        cursorSingleCollectionEvent=>this%TheSingleEventsList%GetListTailP()
        cursorCrossCollectionEvent=>this%TheCrossCollectionEventsList%GetListTailP()

        DO I = 1, ReadedPairsNum
            tempEventsModelsPair = this%TheReadedEventsModels%TheEventsModelsPairList%GetValueByListIndex(I)

            if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()) then
                !---Single Event---
                allocate(cursorSingleCollectionEvent)

                if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .ne. tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()) then
                    write(*,*) "MCPSCUERROR: It is impossible that the cross event is not same with subject and object events when subject and object events is same."
                    write(*,*) "Subject: ",adjustl(trim(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Object: ",adjustl(trim(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Cross: ",adjustl(trim(tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross .or. &
                   tempEventsModelsPair%ObjectEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross .or. &
                   tempEventsModelsPair%CrossEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross) then
                    write(*,*) "MCPSCUERROR: It is impossible that the subject is same with object ,but one of subject ,object and cross event is a cross model."
                    write(*,*) "Subject: ",adjustl(trim(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Object: ",adjustl(trim(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Cross: ",adjustl(trim(tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                tempEventModelSymbol = tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol()

                tempCollectionEventRegister = this%TheCollectionEventsRegisterCenter%GetOneCollectionEventRegisterByName(tempEventModelSymbol)

                cursorSingleCollectionEvent%TheValue=>tempCollectionEventRegister%TheCollectionEvent

                !tempCollectionEventRegister%TheCollectionEvent

                !newSingleCollectionEvent%TheCollection => tempCollectionEventRegister%TheCollection
                
                !call this%TheSingleEventsList%AppendOne(newSingleCollectionEvent)

                !select case(tempEventModelSymbol(1:LENTRIM(tempEventModelSymbol)))
                !    case("MC_MIGCOALE_CLUSTER_GPU")
                !        newSingleCollectionEvent=>m_MC_MIGCOALE_CLUSTER_GPU
                !        if(associated(newSingleCollectionEvent)) then
                !            deallocate(newSingleCollectionEvent)
                !            Nullify(newSingleCollectionEvent)
                !        end if
                !        allocate(newSingleCollectionEvent)
                !        call newSingleCollectionEvent%Constructor()
                !    case default
                !        write(*,*) "MCPSCUERROR: Unkonwn single event collenction: ",tempEventModelSymbol(1:LENTRIM(tempEventModelSymbol))
                !        pause
                !        stop
                !end select

                !call this%TheSingleEventsList%AppendOne(newSingleCollectionEvent)

                !this%TheSingleEventsList%
                cursorSingleCollectionEvent=>cursorSingleCollectionEvent%next
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
        this%TheDataRelationPool = other%TheDataRelationPool
        this%TheCollectionEventsRegisterCenter = other%TheCollectionEventsRegisterCenter

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

        call this%TheDataRelationPool%Clean()

        call this%TheCollectionEventsRegisterCenter%Clean()

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
