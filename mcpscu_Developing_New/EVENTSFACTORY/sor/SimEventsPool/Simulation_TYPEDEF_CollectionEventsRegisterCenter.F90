#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_COLLECTIONEVENTSREGISTERCENTER
    use MC_CollectionEvent_MIGCOALE_CLUSTER_GPU
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

    !****Define a list CollectionEventRegisterList with value type(CollectionEventRegister),pointer**************
    DefGeneralList(CollectionEventRegisterList,type(CollectionEventRegister))

    type,public::CollectionEventsRegisterCenter
        type(CollectionEventRegisterList)::TheCollectionEventRegisterList
        contains
        procedure,public,non_overridable,pass::Init=>InitCollectionEventsRegisterCenter
        procedure,public,non_overridable,pass::RegisterOne=>RegisterOneCollectionEvent
        procedure,public,non_overridable,pass::GetOneCollectionEventRegisterByName=>GetOne_CollectionEventRegisterByName
        procedure,public,non_overridable,pass::CopyFromOther=>CopyCollectionEventsRegisterCenter
        procedure,public,non_overridable,pass::Clean=>Clean_CollectionEventsRegisterCenter
        Generic::Assignment(=)=>CopyCollectionEventsRegisterCenter
        Final::CleanCollectionEventsRegisterCenter
    end type CollectionEventsRegisterCenter

    private::CopyCollectionEventRegister
    private::Clean_CollectionEventRegister
    private::CleanCollectionEventRegister
    private::InitCollectionEventsRegisterCenter
    private::RegisterOneCollectionEvent
    private::GetOne_CollectionEventRegisterByName
    private::CopyCollectionEventsRegisterCenter
    private::Clean_CollectionEventsRegisterCenter
    private::CleanCollectionEventsRegisterCenter

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
        type(CollectionEventRegister)::this
        !---Body---

        call this%Clean()
    end subroutine CleanCollectionEventRegister

    !****The member function of list CollectionEventRegisterList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(CollectionEventRegisterList,type(CollectionEventRegister),Clean)

    !**********************************************
    subroutine InitCollectionEventsRegisterCenter(this)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventsRegisterCenter)::this
        !---Body---

        call this%Clean()

        !---User need to register each event model---
        call this%RegisterOne("MC_MIGCOALE_CLUSTER_GPU",m_MC_MIGCOALE_CLUSTER_GPU)

        return
    end subroutine InitCollectionEventsRegisterCenter

    !**********************************************
    subroutine RegisterOneCollectionEvent(this,EventModelName,TheEventModel)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventsRegisterCenter)::this
        character*(*),intent(in)::EventModelName
        Class(CollectionEvent),pointer::TheEventModel
        !---Local Vars---
        integer::I
        type(CollectionEventRegister)::newCollectionEventRegister
        type(CollectionEventRegister)::tempCollectionEventRegister
        !---Body---

        DO I = 1,this%TheCollectionEventRegisterList%GetListCount()
            tempCollectionEventRegister = this%TheCollectionEventRegisterList%GetValueByListIndex(I)

            if(ISSTREQUAL(tempCollectionEventRegister%TheName,adjustl(trim(EventModelName)))) then
                write(*,*) "MCPSCUERROR: The model had been registed , please do not registe it again: ",adjustl(trim(EventModelName))
                pause
                stop
            end if
        END DO

        call newCollectionEventRegister%Clean()
        newCollectionEventRegister%TheName = adjustl(trim(EventModelName))
        newCollectionEventRegister%TheCollectionEvent=>TheEventModel

        call this%TheCollectionEventRegisterList%AppendOne(newCollectionEventRegister)

        return
    end subroutine RegisterOneCollectionEvent

    !************************************************
    function GetOne_CollectionEventRegisterByName(this,TheName) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventsRegisterCenter)::this
        character*(*),intent(in)::TheName
        type(CollectionEventRegister),intent(out)::TheResult
        !---Local Vars---
        integer::I
        logical::Finded
        !---Body---
        Finded = .false.
        DO I = 1,this%TheCollectionEventRegisterList%GetListCount()
            TheResult = this%TheCollectionEventRegisterList%GetValueByListIndex(I)

            if(ISSTREQUAL(TheResult%TheName,adjustl(trim(TheName)))) then
                Finded = .true.
                exit
            end if
        END DO

        if(.not. Finded) then
            write(*,*) "MCPSCUERROR: The model had not been registed",adjustl(trim(TheName))
            pause
            stop
        end if

        return
    end function GetOne_CollectionEventRegisterByName

    !************************************************
    subroutine CopyCollectionEventsRegisterCenter(this,other)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventsRegisterCenter),intent(out)::this
        Class(CollectionEventsRegisterCenter),intent(in)::other
        !---Body---

        !---The Assignment(=) had been overridable
        this%TheCollectionEventRegisterList = other%TheCollectionEventRegisterList

    end subroutine CopyCollectionEventsRegisterCenter

    !**********************************************
    subroutine Clean_CollectionEventsRegisterCenter(this)
        implicit none
        !---Dummy Vars---
        Class(CollectionEventsRegisterCenter)::this
        !---Body---

        call this%TheCollectionEventRegisterList%CleanList()

        return
    end subroutine Clean_CollectionEventsRegisterCenter

    !***********************************************

    subroutine CleanCollectionEventsRegisterCenter(this)
        implicit none
        !---Dummy Vars---
        type(CollectionEventsRegisterCenter)::this
        !---Body---

        call this%Clean()

        return
    end subroutine CleanCollectionEventsRegisterCenter


end module SIMULATION_TYPEDEF_COLLECTIONEVENTSREGISTERCENTER