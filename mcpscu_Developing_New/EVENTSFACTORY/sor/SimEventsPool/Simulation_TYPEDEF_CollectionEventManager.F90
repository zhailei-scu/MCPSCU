#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_COLLECTIONEVENTMANAGER
    use COMMONLIB_TYPEDEF_COLLECTIONEVENT
    use SIMULATION_TYPEDEF_READEDCOLLECTIONEVENTS
    use SIMULATION_TYPEDEF_DATARELATIONPOOL
    use SIMULATION_TYPEDEF_COLLECTIONEVENTSREGISTERCENTER
    implicit none

    !**************************************************************************
    type,public::CollectionEventsManager
        type(SingleCollectionEventsList_P)::TheSingleEventsList
        type(CrossCollectionEventsList_P)::TheCrossCollectionEventsList

        type(ReadedCollectionEvents),private::TheReadedCollectionEvents
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
        type(DataRelationRegister)::tempLeftDataRelationRegister
        type(DataRelationRegister)::tempRightDataRelationRegister
        !---Body---
        call this%TheReadedCollectionEvents%Load_ReadedCollectionEvents(hEventModels)

        call this%TheDataRelationPool%Init()

        call this%TheCollectionEventsRegisterCenter%Init()

        ReadedPairsNum = this%TheReadedCollectionEvents%TheEventsModelsPairList%GetListCount()

        cursorSingleCollectionEvent=>this%TheSingleEventsList%GetListTailP()
        cursorCrossCollectionEvent=>this%TheCrossCollectionEventsList%GetListTailP()

        DO I = 1, ReadedPairsNum
            tempEventsModelsPair = this%TheReadedCollectionEvents%TheEventsModelsPairList%GetValueByListIndex(I)

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

                !---(1) Based on our test, if the select type(xxx) is used , PGI fortran not support xxx like that tempCollectionEventRegister%TheCollectionEvent
                !---so we have to use a alias scuh as TheCollectionEventAllias=>tempCollectionEventRegister%TheCollectionEvent to stand it, then use
                !---select type(TheCollectionEventAllias),or we can use a pointer pp=>tempCollectionEventRegister%TheCollectionEvent, then use select type(pp)
                !---(2) it is a amazing feature of select type(), for instance, here, the tempCollectionEventRegister%TheCollectionEvent is class(CollectionEvent),pointer
                !--- and type(CollectionEvent) --extend to--> type,abstract(SingleCollectionEvent) --extend to--> type(MC_MIGCOALE_CLUSTER_GPU), which means CollectionEvent is
                !--- the accent of MC_MIGCOALE_CLUSTER_GPU. And the function  TheDefSingleCollectionEventonstructProc is appear in child type SingleCollectionEvent and MC_MIGCOALE_CLUSTER_GPU.
                !--- The amazing thing is: by the help of select type(), we can use tempCollectionEventRegister%TheCollectionEvent to visit the function definded in grandchildren class
                !--- MC_MIGCOALE_CLUSTER_GPU !!!!! Which equal to c++ where the type convert from accent to child !!!!!!
                !---(3) It is an other amazing feature for allocate(MC_MIGCOALE_CLUSTER_GPU::t), where t is defeined:: class(CollectionEvent),pointer::t
                !--- based on our test, when use allocate(MC_MIGCOALE_CLUSTER_GPU::t), t is in fact instance as a type of MC_MIGCOALE_CLUSTER_GPU, which means all member in
                !--- t is not only included in the defination of CollectionEvent, but also child type MC_MIGCOALE_CLUSTER_GPU' members are also initialized!!!!!!!
                !--- which means allocate(MC_MIGCOALE_CLUSTER_GPU::t) equal to C++ type convert  MC_MIGCOALE_CLUSTER_GPU *tt = new (MC_MIGCOALE_CLUSTER_GPU)t ,where t is type
                !--- of class(CollectionEvent),pointer.
                Associate(TheCollectionEventAllias=>tempCollectionEventRegister%TheCollectionEvent)
                    if(.not. associated(TheCollectionEventAllias)) then
                        select type(TheCollectionEventAllias)
                            type is(MC_MIGCOALE_CLUSTER_GPU)
                                allocate(MC_MIGCOALE_CLUSTER_GPU::TheCollectionEventAllias)
                                call TheCollectionEventAllias%TheDefSingleCollectionEventonstructProc()
                        end select
                    end if
                End Associate

                call c_f_pointer(c_loc(tempCollectionEventRegister%TheCollectionEvent),cursorSingleCollectionEvent%TheValue)

                !---The Date----
                tempLeftDataRelationRegister = this%TheDataRelationPool%GetOneByName(tempEventsModelsPair%SubjectEventsModelDef%RelativeData(1)%GetObjectCollectionSymbol())

                Associate(TheCollectionAllias=>tempLeftDataRelationRegister%TheObjectsCollection)
                    if(.not. associated(TheCollectionAllias)) then
                        select type(TheCollectionAllias)
                            type is(SimulationBoxes)
                                allocate(SimulationBoxes::TheCollectionAllias)
                        end select
                    end if
                End Associate

                cursorSingleCollectionEvent%TheValue%TheCollection=>tempLeftDataRelationRegister%TheObjectsCollection

                
                
                cursorSingleCollectionEvent=>cursorSingleCollectionEvent%next
            else

                if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()) then
                    write(*,*) "MCPSCUERROR: It is impossible that the cross event is  same with subject and object events when subject and object events is not same."
                    write(*,*) "Subject: ",adjustl(trim(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Object: ",adjustl(trim(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()
                    write(*,*) "Cross: ",adjustl(trim(tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                if(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross) then
                    write(*,*) "MCPSCUERROR: It is impossible that the subject event is a cross model."
                    write(*,*) "Subject: ",adjustl(trim(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                if(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross) then
                    write(*,*) "MCPSCUERROR: It is impossible that the object event is a cross model."
                    write(*,*) "Object: ",adjustl(trim(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                if(tempEventsModelsPair%CrossEventsModelDef%GetEventModelType() .ne. p_ModelType_Cross) then
                    write(*,*) "MCPSCUERROR: It is impossible that the cross event is not a cross model."
                    write(*,*) "Cross: ",adjustl(trim(tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)), tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()
                    pause
                    stop
                end if

                !---subject single event---

                allocate(cursorSingleCollectionEvent)

                tempEventModelSymbol = tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol()

                tempCollectionEventRegister = this%TheCollectionEventsRegisterCenter%GetOneCollectionEventRegisterByName(tempEventModelSymbol)

                call c_f_pointer(c_loc(tempCollectionEventRegister%TheCollectionEvent),cursorSingleCollectionEvent%TheValue)

                tempLeftDataRelationRegister = this%TheDataRelationPool%GetOneByName(tempEventsModelsPair%SubjectEventsModelDef%RelativeData(1)%GetObjectCollectionSymbol())

                cursorSingleCollectionEvent%TheValue%TheCollection=>tempLeftDataRelationRegister%TheObjectsCollection

                cursorSingleCollectionEvent=>cursorSingleCollectionEvent%next
                
                !---object single event---

                allocate(cursorSingleCollectionEvent)

                tempEventModelSymbol = tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol()

                tempCollectionEventRegister = this%TheCollectionEventsRegisterCenter%GetOneCollectionEventRegisterByName(tempEventModelSymbol)

                call c_f_pointer(c_loc(tempCollectionEventRegister%TheCollectionEvent),cursorSingleCollectionEvent%TheValue)

                tempLeftDataRelationRegister = this%TheDataRelationPool%GetOneByName(tempEventsModelsPair%SubjectEventsModelDef%RelativeData(1)%GetObjectCollectionSymbol())

                cursorSingleCollectionEvent%TheValue%TheCollection=>tempLeftDataRelationRegister%TheObjectsCollection

                cursorSingleCollectionEvent=>cursorSingleCollectionEvent%next

                !---Cross Event---
                allocate(cursorCrossCollectionEvent)

                tempEventModelSymbol = tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol()

                tempCollectionEventRegister = this%TheCollectionEventsRegisterCenter%GetOneCollectionEventRegisterByName(tempEventModelSymbol)

                call c_f_pointer(c_loc(tempCollectionEventRegister%TheCollectionEvent),cursorCrossCollectionEvent%TheValue)

                tempLeftDataRelationRegister = this%TheDataRelationPool%GetOneByName(tempEventsModelsPair%SubjectEventsModelDef%RelativeData(1)%GetObjectCollectionSymbol())

                cursorCrossCollectionEvent%TheValue%TheLeftObjectsCollection=>tempLeftDataRelationRegister%TheObjectsCollection

                tempRightDataRelationRegister = this%TheDataRelationPool%GetOneByName(tempEventsModelsPair%SubjectEventsModelDef%RelativeData(2)%GetObjectCollectionSymbol())

                cursorCrossCollectionEvent%TheValue%TheRightObjectsCollection=>tempRightDataRelationRegister%TheObjectsCollection

                cursorCrossCollectionEvent=>cursorCrossCollectionEvent%next
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
        this%TheReadedCollectionEvents = other%TheReadedCollectionEvents
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

        call this%TheReadedCollectionEvents%Clean()

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
