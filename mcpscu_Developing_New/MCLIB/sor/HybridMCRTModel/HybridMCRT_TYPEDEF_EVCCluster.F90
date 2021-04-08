#include "../../../Macro"
module HYBRIDMCRT_TYPEDEF_EVCCLUSTER
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    TYPE,extends(ACluster),public::EVCCluster
        type(AClusterList),public::TheList

        contains

        procedure,public,non_overridable,pass::CopyEVCClusterFromOther
        procedure,public,non_overridable,pass::Clean_EVCCluster
        Generic::Assignment(=)=>CopyEVCClusterFromOther
        Final::CleanEVCCluster
    end type

    TYPE,extends(SecondOrder_ClusterLists),public::EVCClustersList

        type(EVCCluster)::TheEVCCluster
        integer,public::Identify = 0

        integer,private::ListCount = 0
        type(EVCClustersList),pointer::next=>null()

        contains

        procedure,public,pass,non_overridable::AppendOneEVCCluster
        procedure,public,pass,non_overridable::AppendOtherEVCClustersList
        procedure,public,pass,non_overridable::GetList_Count=>GetEVCClustersList_Count
        procedure,public,pass,non_overridable::Find=>FindEVCClustersListByIdentify
        procedure,public,pass,non_overridable::CopyEVCClustersListFromOther
        procedure,public,pass,non_overridable::CopyEVCClustersListToOther
        procedure,public,pass,non_overridable::Clean_EVCClustersList
        Generic::Assignment(=)=>CopyEVCClustersListFromOther
        Final::CleanEVCClustersList
    END TYPE EVCClustersList

    private::CopyEVCClusterFromOther
    private::Clean_EVCCluster
    private::CleanEVCCluster
    private::AppendOneEVCCluster
    private::AppendOtherEVCClustersList
    private::GetEVCClustersList_Count
    private::FindEVCClustersListByIdentify
    private::CopyEVCClustersListFromOther
    private::CopyEVCClustersListToOther
    private::Clean_EVCClustersList
    private::CleanEVCClustersList

    contains

    !**************************************************
    subroutine CopyEVCClusterFromOther(this,other)
        implicit none
        !---Dummy Vars---
        Class(EVCCluster),intent(out)::this
        type(EVCCluster),intent(in)::other

        !---The Assignment(=) had been overrided
        this%TheList = other%TheList

        this%ACluster = other%ACluster
        return
    end subroutine CopyEVCClusterFromOther

    !**************************************************
    subroutine Clean_EVCCluster(this)
        implicit none
        !---Dummy Vars---
        Class(EVCCluster)::this

        call this%TheList%Clean_ClusterList()
        call this%ACluster%Clean_Cluster()
        return
    end subroutine Clean_EVCCluster

    !**************************************************
    subroutine CleanEVCCluster(this)
        implicit none
        !---Dummy Vars---
        type(EVCCluster)::this

        call this%Clean_EVCCluster()
        return
    end subroutine CleanEVCCluster

    !***************************************
    function AppendOneEVCCluster(this,newOne,theIdentify) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),target::this
        type(EVCCluster)::newOne
        integer,intent(in)::theIdentify
        type(EVCClustersList),pointer::TheResult
        !---Local Vars---
        type(EVCClustersList),pointer::cursor=>null(),cursorP=>null()
        !---Body---

        TheResult=>null()

        cursorP=>this

        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: you need to init the EVCClustersList first!"
            pause
            stop
        end if

        if(this%GetList_Count() .LE. 0) then
            this%ListCount = 1
            !---The Assignment(=) had been overrided---
            this%TheEVCCluster = newOne

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
            cursor%TheEVCCluster = newOne

            cursor%Identify = theIdentify

            cursorP%next=>cursor

            TheResult=>cursor
        end if

        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()
        return
    end function AppendOneEVCCluster

    !**************************************
    subroutine AppendOtherEVCClustersList(this,OtherList)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),target::this
        type(EVCClustersList),target::OtherList
        !---Local Vars---
        type(EVCClustersList),pointer::cursorThis=>null()
        type(EVCClustersList),pointer::cursorOther=>null()
        !---Body---
        cursorThis=>this

        if(.not. associated(cursorThis)) then
            write(*,*) "MCPSCUERROR: you need to init the EVCClustersList first!"
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
            call this%AppendOneEVCCluster(cursorOther%TheEVCCluster,cursorOther%Identify)
            cursorOther=>cursorOther%next
        END DO

        return
    end subroutine AppendOtherEVCClustersList

    !**************************************
    integer function GetEVCClustersList_Count(this)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),target::this
        !---Local Vars---
        type(EVCClustersList),pointer::cursor=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: you need to init the EVCClustersList first!"
            pause
            stop
        end if

        GetEVCClustersList_Count = this%ListCount

        Nullify(cursor)
        cursor=>null()

        return
    end function GetEVCClustersList_Count

    !*************************************************************
    function FindEVCClustersListByIdentify(this,theID) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),target::this
        integer,intent(in)::theID
        type(EVCClustersList),pointer::TheResult
        !---Local Vars---
        logical::Finded
        !---Body---

        TheResult=>null()

        TheResult=>this
        if(.not. associated(TheResult)) then
            write(*,*) "MCPSCUERROR: you need to init the EVCClustersList first!"
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

    end function FindEVCClustersListByIdentify

    !*************************************************************
    subroutine CopyEVCClustersListFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),intent(out),target::this
        CLASS(EVCClustersList),intent(in),target::other
        !---Local Vars---
        type(EVCClustersList),pointer::thisCursor=>null()
        type(EVCClustersList),pointer::otherCursor=>null()
        type(EVCClustersList),pointer::thisCursorP=>null()
        type(EVCClustersList),pointer::otherCursorP=>null()
        !---Body---

        thisCursorP=>this
        if(.not. associated(thisCursorP)) then
            write(*,*) "MCPSCUERROR: You must allocate the list first !"
            pause
            stop
        end if

        call this%Clean_EVCClustersList()

        otherCursorP=>other
        if(.not. associated(otherCursorP)) then
            return
        end if

        if(otherCursorP%GetList_Count() .LE. 0) then
            return
        end if

        ! The assignment(=) had been override
        thisCursorP%TheEVCCluster = otherCursorP%TheEVCCluster
        thisCursorP%Identify = otherCursorP%Identify

        this%ListCount = this%ListCount + 1

        thisCursor=>thisCursorP%next
        otherCursor=>otherCursorP%next
        DO While(associated(otherCursor))

            allocate(thisCursor)
            ! The assignment(=) had been override
            thisCursor%TheEVCCluster = otherCursor%TheEVCCluster
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
    end subroutine CopyEVCClustersListFromOther

    !*************************************************************
    subroutine CopyEVCClustersListToOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),intent(in),target::this
        CLASS(EVCClustersList),intent(out),target::other
        !---Local Vars---
        type(EVCClustersList),pointer::thisCursor=>null()
        type(EVCClustersList),pointer::otherCursor=>null()
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

        call other%Clean_EVCClustersList()

        if(this%GetList_Count() .LE. 0) then
            return
        end if

        !---The Assignment had been overried---
        other = this

        return
    end subroutine CopyEVCClustersListToOther

    !**************************************
    subroutine Clean_EVCClustersList(this)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),target::this
        !---Local Vars---
        type(EVCClustersList),pointer::cursor=>null()
        type(EVCClustersList),pointer::next=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        call this%TheEVCCluster%Clean_EVCCluster()
        this%Identify = 0

        DO While(associated(cursor))
            next=>cursor%next
            call CleanEVCCluster(cursor%TheEVCCluster)
            cursor%Identify = 0
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
    end subroutine Clean_EVCClustersList

    !************************************
    subroutine CleanEVCClustersList(this)
        implicit none
        !---Dummy Vars---
        type(EVCClustersList)::this
        !---Body---

        call this%Clean_EVCClustersList()

        return
    end subroutine CleanEVCClustersList

end module HYBRIDMCRT_TYPEDEF_EVCCLUSTER
