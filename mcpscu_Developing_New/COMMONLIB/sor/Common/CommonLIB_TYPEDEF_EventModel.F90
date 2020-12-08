#include "../../../MACRO/Macro"
module COMMONLIB_TYPEDEF_EVENTMODEL
    implicit none

    abstract interface

        !*******For each work*************
        subroutine BeforeEachWorkProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine BeforeEachWorkProc

        subroutine AfterEachWorkProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end  subroutine AfterEachWorkProc

        !******For each job***************
        subroutine BeforeEachJobProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine BeforeEachJobProc

        subroutine AfterEachJobProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine AfterEachJobProc

        !*****For each test***************
        subroutine BeforeEachTestProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine BeforeEachTestProc

        subroutine AfterEachTestProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine AfterEachTestProc

        !*****For each time section*******
        subroutine BeforeEachTimeSectionProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine BeforeEachTimeSectionProc

        subroutine AfterEachTimeSectionProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine AfterEachTimeSectionProc

        !****For each time step***********
        subroutine BeforeEachTimeStepProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine BeforeEachTimeStepProc

        subroutine EachTimeStepProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine EachTimeStepProc

        subroutine AfterEachTimeStepProc(TheObjectsCollection,TheEventCtrl)
            use COMMONLIB_TYPEDEF_OBJECTSCOLLECTION
            use COMMONLIB_TYPEDEF_EVENTCONTROL
            implicit none
            !---Dummy Vars----
            CLASS(ObjectsCollection),pointer::TheObjectsCollection
            CLASS(EventCtrl),pointer::TheEventCtrl
            !---Body---
        end subroutine AfterEachTimeStepProc
    end interface

    !****************************************************************************
    type,public::EventModel
        character(len=100)::ModelName PREASSIGN ""
        procedure(BeforeEachWorkProc),pointer,nopass::TheBeforeEachWorkProc=>null()
        procedure(AfterEachWorkProc),pointer,nopass::TheAfterEachWorkProc=>null()
        procedure(BeforeEachJobProc),pointer,nopass::TheBeforeEachJobProc=>null()
        procedure(AfterEachJobProc),pointer,nopass::TheAfterEachJobProc=>null()
        procedure(BeforeEachTestProc),pointer,nopass::TheBeforeEachTestProc=>null()
        procedure(AfterEachTestProc),pointer,nopass::TheAfterEachTestProc=>null()
        procedure(BeforeEachTimeSectionProc),pointer,nopass::TheBeforeEachTimeSectionProc=>null()
        procedure(AfterEachTimeSectionProc),pointer,nopass::TheAfterEachTimeSectionProc=>null()
        procedure(BeforeEachTimeStepProc),pointer,nopass::TheBeforeEachTimeStepProc=>null()
        procedure(EachTimeStepProc),pointer,nopass::TheEachTimeStepProc=>null()
        procedure(AfterEachTimeStepProc),pointer,nopass::TheAfterEachTimeStepProc=>null()

        contains
        !---Based on our test, if the FINAL subroutine (deconstructor) is used, the CopyEventModelFromOther in
        !--the Generic::Assignment(=)=>CopyEventModelFromOther cannot be assig to another symbol such as CopyFromOther=>CopyEventModelFromOther
        !procedure,public,non_overridable,pass::CopyFromOther=>CopyEventModelFromOther
        procedure,public,non_overridable,pass::CopyEventModelFromOther
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
        this%TheBeforeEachWorkProc =>other%TheBeforeEachWorkProc
        this%TheAfterEachWorkProc =>other%TheAfterEachWorkProc
        this%TheBeforeEachJobProc =>other%TheBeforeEachJobProc
        this%TheAfterEachJobProc =>other%TheAfterEachJobProc
        this%TheBeforeEachTestProc =>other%TheBeforeEachTestProc
        this%TheAfterEachTestProc =>other%TheAfterEachTestProc
        this%TheBeforeEachTimeSectionProc =>other%TheBeforeEachTimeSectionProc
        this%TheAfterEachTimeSectionProc =>other%TheAfterEachTimeSectionProc
        this%TheBeforeEachTimeStepProc =>other%TheBeforeEachTimeStepProc
        this%TheEachTimeStepProc =>other%TheEachTimeStepProc
        this%TheAfterEachTimeStepProc =>other%TheAfterEachTimeStepProc
        return
    end subroutine CopyEventModelFromOther

    !***************************************************
    subroutine Clean_EventModel(this)
        implicit none
        !---Dummy Vars---
        CLASS(EventModel)::this
        !---Body---

        this%ModelName = ""

        Nullify(this%TheBeforeEachWorkProc)
        this%TheBeforeEachWorkProc => null()

        Nullify(this%TheAfterEachWorkProc)
        this%TheAfterEachWorkProc => null()

        Nullify(this%TheBeforeEachJobProc)
        this%TheBeforeEachJobProc => null()

        Nullify(this%TheAfterEachJobProc)
        this%TheAfterEachJobProc => null()

        Nullify(this%TheBeforeEachTestProc)
        this%TheBeforeEachTestProc => null()

        Nullify(this%TheAfterEachTestProc)
        this%TheAfterEachTestProc => null()

        Nullify(this%TheBeforeEachTimeSectionProc)
        this%TheBeforeEachTimeSectionProc => null()

        Nullify(this%TheAfterEachTimeSectionProc)
        this%TheAfterEachTimeSectionProc => null()

        Nullify(this%TheBeforeEachTimeStepProc)
        this%TheBeforeEachTimeStepProc => null()

        Nullify(this%TheEachTimeStepProc)
        this%TheEachTimeStepProc => null()

        Nullify(this%TheAfterEachTimeStepProc)
        this%TheAfterEachTimeStepProc => null()
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