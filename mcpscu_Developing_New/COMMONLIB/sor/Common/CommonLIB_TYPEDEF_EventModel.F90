#include "../../../MACRO/Macro"
module COMMONLIB_TYPEDEF_EVENTMODEL

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


    private::CopyEventModelFromOther
    private::Clean_EventModel
    private::CleanEventModel

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

end module COMMONLIB_TYPEDEF_EVENTMODEL