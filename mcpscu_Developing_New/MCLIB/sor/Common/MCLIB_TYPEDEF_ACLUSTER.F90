#include "../../../Macro"

module MCLIB_TYPEDEF_ACLUSTER
    #ifdef MC_PROFILING
    USE MCLIB_TimeProfile
    #endif
    USE MCLIB_CONSTANTS
    implicit none

    TYPE,PUBLIC::Single_AtomsSet
        integer::m_ID PREASSIGN 0
        integer::m_NA PREASSIGN 0
    END TYPE Single_AtomsSet

    TYPE,PUBLIC::Single_AtomsSetRange

        integer::m_ID = 0

        integer::m_NA_From = 0

        integer::m_NA_To = 0

    END TYPE Single_AtomsSetRange

    TYPE,PUBLIC::AtomsSetRange
        type(Single_AtomsSetRange),dimension(p_ATOMS_GROUPS_NUMBER)::m_SetsRange

        contains
        procedure,public,pass,non_overridable::ReleaseSetsRange
        procedure,private,pass,non_overridable::PermutationAtomsSetRange2ClusterList
        procedure,public,pass,non_overridable::AtomsSetRange2ClusterList
        procedure,public,pass,non_overridable::CopyAtomsSetRangeFromOther
        Generic::Assignment(=)=>CopyAtomsSetRangeFromOther
    END TYPE AtomsSetRange


    TYPE,PUBLIC::ACluster
         type(Single_AtomsSet),dimension(p_ATOMS_GROUPS_NUMBER)::m_Atoms
         real(kind=KINDDF),dimension(3)::m_POS PREASSIGN 0.D0
         integer::m_Layer PREASSIGN 1
         real(kind=KINDDF)::m_RAD PREASSIGN 0
         integer::m_Statu PREASSIGN p_Empty
         integer,dimension(2)::m_GrainID PREASSIGN 0
         real(kind=KINDDF)::m_DiffCoeff PREASSIGN 0.D0
         real(kind=KINDDF),dimension(3)::m_DiffuseDirection PREASSIGN 0.D0
         real(kind=KINDDF)::m_DiffuseRotateCoeff PREASSIGN 0.D0   ! frequence
         integer,dimension(2)::m_Record PREASSIGN 0

         contains
         procedure,non_overridable,pass,public::CopyClusterFromOther
         procedure,non_overridable,pass,public::Clean_Cluster
         procedure,non_overridable,pass,public::IsSameKindCluster
         Generic::Assignment(=)=>CopyClusterFromOther
         !*********Important note: The PGI CUDA Fortran not support the Final symbol, the final symbol would cause the
         !*********Compiler error************
         !Final::CleanCluster
    END TYPE ACluster

    TYPE,PUBLIC::AClusterList
        type(ACluster),public::TheCluster
        real(kind=KINDDF),public::quantififyValue = 0.D0   ! This value is used to record some quantifify value such as counts or concentrate in some application


        integer,private::ListCount = 0
        type(AClusterList),pointer::next=>null()

        contains
        procedure,public,pass,non_overridable::AppendOneCluster
        procedure,public,pass,non_overridable::AppendOtherClusterList
        procedure,public,pass,non_overridable::GetList_Count=>GetClustersList_Count
        procedure,public,pass,non_overridable::Find=>FindCluster
        procedure,public,pass,non_overridable::CopyClustersListFromOther
        procedure,public,pass,non_overridable::Clean_ClusterList
        Generic::Assignment(=)=>CopyClustersListFromOther
        Final::CleanClusterList
    END TYPE

    type,abstract::SecondOrder_ClusterLists


    end type

    TYPE,extends(SecondOrder_ClusterLists),public::SecondOrder_AClusterLists
        type(AClusterList),public::TheList
        integer,public::Identify = -1

        integer,private::ListCount = 0
        type(SecondOrder_AClusterLists),pointer::next=>null()

        contains
        procedure,public,pass,non_overridable::AppendOneClusterList
        procedure,public,pass,non_overridable::AppendOtherSecondOrder_AClusterLists
        procedure,public,pass,non_overridable::GetList_Count=>GetSecondOrder_AClusterLists_Count
        procedure,public,pass,non_overridable::Find=>FindClusterListByIdentify
        procedure,public,pass,non_overridable::CopySecondOrder_AClusterListsFromOther
        procedure,public,pass,non_overridable::CopySecondOrder_AClusterListsToOther
        procedure,public,pass,non_overridable::Clean_SecondOrder_AClusterLists
        Generic::Assignment(=)=>CopySecondOrder_AClusterListsFromOther
        Final::CleanSecondOrder_AClusterLists

    END TYPE

    private::CopyClusterFromOther
    private::Clean_Cluster
    private::IsSameKindCluster
    private::CleanCluster
    private::ReleaseSetsRange
    private::PermutationAtomsSetRange2ClusterList
    private::AtomsSetRange2ClusterList
    private::CopyAtomsSetRangeFromOther
    private::AppendOneCluster
    private::AppendOtherClusterList
    private::GetClustersList_Count
    private::FindCluster
    private::CopyClustersListFromOther
    private::Clean_ClusterList
    private::CleanClusterList
    private::AppendOneClusterList
    private::AppendOtherSecondOrder_AClusterLists
    private::GetSecondOrder_AClusterLists_Count
    private::FindClusterListByIdentify
    private::CopySecondOrder_AClusterListsFromOther
    private::CopySecondOrder_AClusterListsToOther
    private::Clean_SecondOrder_AClusterLists
    private::CleanSecondOrder_AClusterLists

    contains

    !*************For AtomsSetRange******************
    subroutine ReleaseSetsRange(this)
        implicit none
        !---Dummy Vars---
        Class(AtomsSetRange)::this
        !---Local Vars---
        integer::I
        !---Body---

        DO I = 1,size(this%m_SetsRange)
            this%m_SetsRange(I)%m_ID = 0
            this%m_SetsRange(I)%m_NA_From = 0
            this%m_SetsRange(I)%m_NA_To = 0
        END DO

        return
    end subroutine ReleaseSetsRange

    !*************For ACluster***********************

    subroutine CopyClusterFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ACluster),intent(out)::this
        CLASS(ACluster),intent(in)::other
        !---Local Vars---
        integer::IElement
        !---Body---
        DO IElement = 1,p_ATOMS_GROUPS_NUMBER
            this%m_Atoms(IElement) = other%m_Atoms(IElement)
        END DO

        this%m_POS = other%m_POS

        this%m_RAD = other%m_RAD

        this%m_Layer = other%m_Layer

        this%m_Statu = other%m_Statu

        this%m_GrainID = other%m_GrainID

        this%m_DiffCoeff = other%m_DiffCoeff

        this%m_DiffuseDirection = other%m_DiffuseDirection

        this%m_DiffuseRotateCoeff = other%m_DiffuseRotateCoeff

        this%m_Record = other%m_Record

        return
    end subroutine CopyClusterFromOther


    subroutine Clean_Cluster(this)
        implicit none
        !---Dummy Vars---
        CLASS(ACluster)::this
        !---Local Vars---
        integer::I
        !---Body---

        DO I = 1,p_ATOMS_GROUPS_NUMBER
            this%m_Atoms(I)%m_ID = 0
            this%m_Atoms(I)%m_NA = 0
        END DO

        this%m_POS = 0
        this%m_Layer = 1
        this%m_RAD = 0
        this%m_Statu = p_Empty
        this%m_GrainID(2) = 0
        this%m_DiffCoeff = 0.D0
        this%m_DiffuseDirection = 0.D0
        this%m_DiffuseRotateCoeff = 0.D0
        this%m_Record = 0

        return
    end subroutine

    function IsSameKindCluster(this,OtherCluster) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(ACluster)::this
        type(ACluster)::OtherCluster
        logical,intent(out)::TheResult
        !---Local Vars---
        integer::IElement
        !---Body---
        TheResult = .true.

        DO IElement = 1,p_ATOMS_GROUPS_NUMBER

            if(this%m_Atoms(IElement)%m_ID .ne. OtherCluster%m_Atoms(IElement)%m_ID .or. &
               this%m_Atoms(IElement)%m_NA .ne. OtherCluster%m_Atoms(IElement)%m_NA) then
                TheResult = .false.
                exit
            end if

        END DO

        return
    end function IsSameKindCluster

    subroutine CleanCluster(this)
        implicit none
        !---Dummy Vars---
        type(ACluster)::this
        !---Local Vars---
        call this%Clean_Cluster()

        return
    end subroutine


    integer function Get_MemoryConsuming_ClusterType()
        implicit none
        type(ACluster)::aCluster

        Get_MemoryConsuming_ClusterType = sizeof(aCluster)
        return
    end function Get_MemoryConsuming_ClusterType

    !**************************************
    function AtomsSetRange2ClusterList(this,SingleAtomsDivideArrays) result(List)
        implicit none
        !---Dummy Vars---
        CLASS(AtomsSetRange)::this
        integer,dimension(:,:),allocatable::SingleAtomsDivideArrays
        type(AClusterList)::List
        !---Local Vars---
        type(ACluster)::Cluster
        !---body---

        call List%Clean_ClusterList()

        call this%PermutationAtomsSetRange2ClusterList(List,Cluster,SingleAtomsDivideArrays,1,1)

        return
    end function

    !*****************************************
    subroutine CopyAtomsSetRangeFromOther(this,other)
        implicit none
        !---Dummy Vars---
        Class(AtomsSetRange),intent(out)::this
        type(AtomsSetRange),intent(in)::other

        !---Body---
        this%m_SetsRange = other%m_SetsRange

        return
    end subroutine

    !**************************************************
    recursive subroutine PermutationAtomsSetRange2ClusterList(this,List,Cluster,SingleAtomsDivideArrays,TheLevel,choosen)
        implicit none
        !---Dummy Vars---
        CLASS(AtomsSetRange)::this
        type(AClusterList)::List
        type(ACluster)::Cluster
        integer,dimension(:,:),allocatable::SingleAtomsDivideArrays
        integer::TheLevel
        integer::choosen
        !---Local Vars---
        integer::IGroup
        !---Body---

        if(TheLevel .GT. 1) then

            Cluster%m_Atoms(TheLevel-1)%m_NA = SingleAtomsDivideArrays(TheLevel-1,choosen)

            Cluster%m_Atoms(TheLevel-1)%m_ID = this%m_SetsRange(TheLevel-1)%m_ID
        end if

        if(TheLevel .eq. (size(this%m_SetsRange)+1)) then
            call List%AppendOneCluster(Cluster)
            return
        else
            DO IGroup = 1,size(SingleAtomsDivideArrays(TheLevel,:))
                if(this%m_SetsRange(TheLevel)%m_NA_From .GT. SingleAtomsDivideArrays(TheLevel,IGroup)) then
                    cycle
                end if

                call this%PermutationAtomsSetRange2ClusterList(List,Cluster,SingleAtomsDivideArrays,TheLevel+1,IGroup)

                if(this%m_SetsRange(TheLevel)%m_NA_To .LE. SingleAtomsDivideArrays(TheLevel,IGroup)) then
                    if(IGroup .GT. 1) then
                        exit
                    end if
                end if

                if(this%m_SetsRange(TheLevel)%m_NA_From .eq. this%m_SetsRange(TheLevel)%m_NA_To) then
                    exit
                end if
            END DO

        end if

        return
    end subroutine

    !***************************************
    subroutine AppendOneCluster(this,newOne,theQuantififyValue)
        implicit none
        !---Dummy Vars---
        CLASS(AClusterList),target::this
        type(ACluster)::newOne
        real(kind=KINDDF),optional::theQuantififyValue
        !---Local Vars---
        type(AClusterList),pointer::cursor=>null(),cursorP=>null()
        !---Body---

        cursorP=>this

        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: you need to init the AAClusterList first!"
            pause
            stop
        end if

        if(this%GetList_Count() .LE. 0) then
            this%ListCount = 1
            this%TheCluster = newOne
            if(present(theQuantififyValue)) then
                this%quantififyValue = theQuantififyValue
            end if
        else
            cursor=>this%next
            cursorP=>this

            DO while(associated(cursor))
                cursor=>cursor%next
                cursorP=>cursorP%next
            END DO

            this%ListCount = this%ListCount + 1

            allocate(cursor)
            NUllify(cursor%next)
            cursor%next=>null()
            ! The assignment(=) had been overrided
            cursor%TheCluster = newOne
            if(present(theQuantififyValue)) then
                cursor%quantififyValue = theQuantififyValue
            end if

            cursorP%next=>cursor
        end if

        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()
        return
    end subroutine AppendOneCluster

    !**************************************
    subroutine AppendOtherClusterList(this,OtherList)
        implicit none
        !---Dummy Vars---
        CLASS(AClusterList),target::this
        type(AClusterList),target::OtherList
        !---Local Vars---
        type(AClusterList),pointer::cursorThis=>null()
        type(AClusterList),pointer::cursorOther=>null()
        !---Body---
        cursorThis=>this

        if(.not. associated(cursorThis)) then
            write(*,*) "MCPSCUERROR: you need to init the AAClusterList first!"
            pause
            stop
        end if

        cursorOther=>OtherList

        if(.not. associated(cursorOther)) then
            return
        end if

        if(cursorOther%GetList_Count() .LE. 0) then
            return
        end if

        DO While(associated(cursorOther))
            call this%AppendOneCluster(cursorOther%TheCluster,cursorOther%quantififyValue)
            cursorOther=>cursorOther%next
        END DO

        return
    end subroutine

    !**************************************
    integer function GetClustersList_Count(this)
        implicit none
        !---Dummy Vars---
        CLASS(AClusterList),target::this
        !---Local Vars---
        type(AClusterList),pointer::cursor=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: you need to init the AAClusterList first!"
            pause
            stop
        end if

        GetClustersList_Count = this%ListCount

        Nullify(cursor)
        cursor=>null()

        return
    end function

    !********************************************
    function FindCluster(this,targetClusterType) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(AClusterList),target::this
        type(ACluster),intent(in)::targetClusterType
        type(AClusterList),pointer::TheResult
        !---Local Vars---
        logical::Finded
        !---Body---

        TheResult=>null()

        TheResult=>this
        if(.not. associated(TheResult)) then
            write(*,*) "MCPSCUERROR: you need to init the AClusterList first!"
            pause
            stop
        end if

        Finded = .false.

        DO while(associated(TheResult))

            if(TheResult%TheCluster%IsSameKindCluster(targetClusterType) .eq. .true.) then
                Finded = .true.
                exit
            end if

            TheResult=>TheResult%next
        END DO

        if(Finded .eq. .false.) then
            Nullify(TheResult)
            TheResult=>null()
        end if

    end function FindCluster

    !**************************************
    subroutine CopyClustersListFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(AClusterList),intent(out),target::this
        CLASS(AClusterList),intent(in),target::other
        !---Local Vars---
        type(AClusterList),pointer::thisCursor=>null()
        type(AClusterList),pointer::otherCursor=>null()
        type(AClusterList),pointer::thisCursorP=>null()
        type(AClusterList),pointer::otherCursorP=>null()
        !---Body---

        thisCursorP=>this
        if(.not. associated(thisCursorP)) then
            write(*,*) "MCPSCUERROR: You must allocate the list first !"
            pause
            stop
        end if

        call this%Clean_ClusterList()

        otherCursorP=>other
        if(.not. associated(otherCursorP)) then
            return
        end if

        if(otherCursorP%GetList_Count() .LE. 0) then
            return
        end if

        ! The assignment(=) had been override
        thisCursorP%TheCluster = otherCursorP%TheCluster
        thisCursorP%quantififyValue = otherCursorP%quantififyValue

        this%ListCount = this%ListCount + 1

        thisCursor=>thisCursorP%next
        otherCursor=>otherCursorP%next
        DO While(associated(otherCursor))

            allocate(thisCursor)
            ! The assignment(=) had been override
            thisCursor%TheCluster = otherCursor%TheCluster
            thisCursor%quantififyValue = otherCursor%quantififyValue

            this%ListCount = this%ListCount + 1

            thisCursorP%next=>thisCursor

            thisCursorP=>thisCursor
            otherCursorP=>otherCursor

            otherCursor=>otherCursor%next
            thisCursor=>thisCursor%next
        END DO

        Nullify(thisCursor)
        thisCursor=>null()
        Nullify(thisCursorP)
        thisCursorP=>null()
        Nullify(otherCursor)
        otherCursor=>null()
        Nullify(otherCursorP)
        otherCursorP=>null()
        return
    end subroutine

    !**************************************
    subroutine Clean_ClusterList(this)
        implicit none
        !---Dummy Vars---
        CLASS(AClusterList),target::this
        !---Local Vars---
        type(AClusterList),pointer::cursor=>null()
        type(AClusterList),pointer::next=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        call this%TheCluster%Clean_Cluster()
        this%quantififyValue = 0.D0

        DO While(associated(cursor))
            next=>cursor%next
            call Clean_Cluster(cursor%TheCluster)
            cursor%quantififyValue = 0.D0
            cursor%next=>null()
            deallocate(cursor)
            Nullify(cursor)
            cursor=>next
        END DO

        this%next=>null()

        this%ListCount = 0

        Nullify(cursor)
        Nullify(next)
        cursor=>null()
        next=>null()

        return
    end subroutine Clean_ClusterList

    !************************************
    subroutine CleanClusterList(this)
        implicit none
        !---Dummy Vars---
        type(AClusterList)::this
        !---Body---

        call this%Clean_ClusterList()

        return
    end subroutine CleanClusterList

    !***************************************
    function AppendOneClusterList(this,newOne,theIdentify) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),target::this
        type(AClusterList)::newOne
        integer,intent(in)::theIdentify
        type(SecondOrder_AClusterLists),pointer::TheResult
        !---Local Vars---
        type(SecondOrder_AClusterLists),pointer::cursor=>null(),cursorP=>null()
        !---Body---

        TheResult=>null()

        cursorP=>this

        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: you need to init the SecondOrder_AClusterLists first!"
            pause
            stop
        end if

        if(this%GetList_Count() .LE. 0) then
            this%ListCount = 1
            !---The Assignment(=) had been overrided---
            this%TheList = newOne

            this%Identify = theIdentify
        else

            if(associated(this%Find(theIdentify))) then
                write(*,*) "MCPSCUERROR: The ClusterList with ID: ",theIdentify," had existed , please do not overwrite it"
                pause
                stop
            end if

            cursor=>this%next
            cursorP=>this

            DO while(associated(cursor))
                cursor=>cursor%next
                cursorP=>cursorP%next
            END DO

            this%ListCount = this%ListCount + 1

            allocate(cursor)
            NUllify(cursor%next)
            cursor%next=>null()
            ! The assignment(=) had been overrided
            cursor%TheList = newOne

            cursor%Identify = theIdentify

            cursorP%next=>cursor

            TheResult=>cursor
        end if

        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()
        return
    end function AppendOneClusterList

    !**************************************
    subroutine AppendOtherSecondOrder_AClusterLists(this,OtherList)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),target::this
        type(SecondOrder_AClusterLists),target::OtherList
        !---Local Vars---
        type(SecondOrder_AClusterLists),pointer::cursorThis=>null()
        type(SecondOrder_AClusterLists),pointer::cursorOther=>null()
        !---Body---
        cursorThis=>this

        if(.not. associated(cursorThis)) then
            write(*,*) "MCPSCUERROR: you need to init the SecondOrder_AClusterLists first!"
            pause
            stop
        end if

        cursorOther=>OtherList

        if(.not. associated(cursorOther)) then
            return
        end if

        if(cursorOther%GetList_Count() .LE. 0) then
            return
        end if

        DO While(associated(cursorOther))
            call this%AppendOneClusterList(cursorOther%TheList,cursorOther%Identify)
            cursorOther=>cursorOther%next
        END DO

        return
    end subroutine AppendOtherSecondOrder_AClusterLists

    !**************************************
    integer function GetSecondOrder_AClusterLists_Count(this)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),target::this
        !---Local Vars---
        type(SecondOrder_AClusterLists),pointer::cursor=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: you need to init the SecondOrder_AClusterLists first!"
            pause
            stop
        end if

        GetSecondOrder_AClusterLists_Count = this%ListCount

        Nullify(cursor)
        cursor=>null()

        return
    end function GetSecondOrder_AClusterLists_Count

    !*************************************************************
    function FindClusterListByIdentify(this,theID) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),target::this
        integer,intent(in)::theID
        type(SecondOrder_AClusterLists),pointer::TheResult
        !---Local Vars---
        logical::Finded
        !---Body---

        TheResult=>null()

        TheResult=>this
        if(.not. associated(TheResult)) then
            write(*,*) "MCPSCUERROR: you need to init the SecondOrder_AClusterLists first!"
            pause
            stop
        end if

        Finded = .false.

        DO while(associated(TheResult))

            if(TheResult%Identify .eq. theID) then
                Finded = .true.
                exit
            end if

            TheResult=>TheResult%next
        END DO

        if(Finded .eq. .false.) then
            Nullify(TheResult)
            TheResult=>null()
        end if

    end function FindClusterListByIdentify

    !*************************************************************
    subroutine CopySecondOrder_AClusterListsFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),intent(out),target::this
        CLASS(SecondOrder_AClusterLists),intent(in),target::other
        !---Local Vars---
        type(SecondOrder_AClusterLists),pointer::thisCursor=>null()
        type(SecondOrder_AClusterLists),pointer::otherCursor=>null()
        type(SecondOrder_AClusterLists),pointer::thisCursorP=>null()
        type(SecondOrder_AClusterLists),pointer::otherCursorP=>null()
        !---Body---

        thisCursorP=>this
        if(.not. associated(thisCursorP)) then
            write(*,*) "MCPSCUERROR: You must allocate the list first !"
            pause
            stop
        end if

        call this%Clean_SecondOrder_AClusterLists()

        otherCursorP=>other
        if(.not. associated(otherCursorP)) then
            return
        end if

        if(otherCursorP%GetList_Count() .LE. 0) then
            return
        end if

        ! The assignment(=) had been override
        thisCursorP%TheList = otherCursorP%TheList
        thisCursorP%Identify = otherCursorP%Identify

        this%ListCount = this%ListCount + 1

        thisCursor=>thisCursorP%next
        otherCursor=>otherCursorP%next
        DO While(associated(otherCursor))

            allocate(thisCursor)
            ! The assignment(=) had been override
            thisCursor%TheList = otherCursor%TheList
            thisCursor%Identify = otherCursor%Identify

            this%ListCount = this%ListCount + 1

            thisCursorP%next=>thisCursor

            thisCursorP=>thisCursor
            otherCursorP=>otherCursor

            otherCursor=>otherCursor%next
            thisCursor=>thisCursor%next
        END DO

        Nullify(thisCursor)
        thisCursor=>null()
        Nullify(thisCursorP)
        thisCursorP=>null()
        Nullify(otherCursor)
        otherCursor=>null()
        Nullify(otherCursorP)
        otherCursorP=>null()
        return
    end subroutine CopySecondOrder_AClusterListsFromOther

    !*************************************************************
    subroutine CopySecondOrder_AClusterListsToOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),intent(in),target::this
        CLASS(SecondOrder_AClusterLists),intent(out),target::other
        !---Local Vars---
        type(SecondOrder_AClusterLists),pointer::thisCursor=>null()
        type(SecondOrder_AClusterLists),pointer::otherCursor=>null()
        !---Body---

        thisCursor=>this
        if(.not. associated(thisCursor)) then
            return
        end if

        otherCursor=>other
        if(.not. associated(otherCursor)) then
            write(*,*) "MCPSCUERROR: You must allocate the list first !"
            pause
            stop
        end if

        call other%Clean_SecondOrder_AClusterLists()

        if(this%GetList_Count() .LE. 0) then
            return
        end if

        !---The Assignment had been overried---
        other = this

        return
    end subroutine CopySecondOrder_AClusterListsToOther

    !**************************************
    subroutine Clean_SecondOrder_AClusterLists(this)
        implicit none
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),target::this
        !---Local Vars---
        type(SecondOrder_AClusterLists),pointer::cursor=>null()
        type(SecondOrder_AClusterLists),pointer::next=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        call this%TheList%Clean_ClusterList()
        this%Identify = -1

        DO While(associated(cursor))
            next=>cursor%next
            call CleanClusterList(cursor%TheList)
            cursor%Identify = -1
            cursor%next=>null()
            deallocate(cursor)
            Nullify(cursor)
            cursor=>next
        END DO

        this%next=>null()

        this%ListCount = 0

        Nullify(cursor)
        Nullify(next)
        cursor=>null()
        next=>null()

        return
    end subroutine Clean_SecondOrder_AClusterLists

    !************************************
    subroutine CleanSecondOrder_AClusterLists(this)
        implicit none
        !---Dummy Vars---
        type(SecondOrder_AClusterLists)::this
        !---Body---

        call this%Clean_SecondOrder_AClusterLists()

        return
    end subroutine CleanSecondOrder_AClusterLists

end module MCLIB_TYPEDEF_ACLUSTER
