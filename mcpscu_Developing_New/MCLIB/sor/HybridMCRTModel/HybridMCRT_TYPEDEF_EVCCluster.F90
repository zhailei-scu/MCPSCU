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

        contains

        procedure,public,pass,non_overridable::AppendOneEVCCluster
        procedure,public,pass,non_overridable::AppendOtherEVCClustersList
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
        Class(SecondOrder_ClusterLists),pointer::TheResult
        !---Local Vars---
        Class(SecondOrder_ClusterLists),pointer::cursor=>null(),cursorP=>null()
        !---Body---

        TheResult=>this

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
            allocate(EVCClustersList::cursor)
            NUllify(cursor%next)
            cursor%next=>null()

            select type(cursor)
                type is(EVCClustersList)
                    ! The assignment(=) had been overrided
                    cursor%TheEVCCluster = newOne

            end select

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
        CLASS(SecondOrder_ClusterLists),pointer::cursorThis=>null()
        CLASS(SecondOrder_ClusterLists),pointer::cursorOther=>null()
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
            select type(cursorOther)
                type is(EVCClustersList)
                    call this%AppendOneEVCCluster(cursorOther%TheEVCCluster,cursorOther%Identify)
            end select
            cursorOther=>cursorOther%next
        END DO

        return
    end subroutine AppendOtherEVCClustersList

    !*************************************************************
    subroutine CopyEVCClustersListFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(EVCClustersList),intent(out),target::this
        CLASS(EVCClustersList),intent(in),target::other
        !---Local Vars---
        Class(SecondOrder_ClusterLists),pointer::thisCursor=>null()
        Class(SecondOrder_ClusterLists),pointer::otherCursor=>null()
        Class(SecondOrder_ClusterLists),pointer::thisCursorP=>null()
        Class(SecondOrder_ClusterLists),pointer::otherCursorP=>null()
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

        select type(thisCursorP)
            type is(EVCClustersList)
                select type(otherCursorP)
                    type is(EVCClustersList)
                        ! The assignment(=) had been override
                        thisCursorP%TheEVCCluster = otherCursorP%TheEVCCluster
                end select
        end select

        thisCursorP%Identify = otherCursorP%Identify

        this%ListCount = this%ListCount + 1

        thisCursor=>thisCursorP%next
        otherCursor=>otherCursorP%next
        DO While(associated(otherCursor))

            allocate(EVCClustersList::thisCursor)
            ! The assignment(=) had been override
            select type(thisCursor)
                type is(EVCClustersList)
                    select type(otherCursor)
                        type is(EVCClustersList)
                            thisCursor%TheEVCCluster = otherCursor%TheEVCCluster
                    end select
            end select
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
        CLASS(SecondOrder_ClusterLists),pointer::cursor=>null()
        CLASS(SecondOrder_ClusterLists),pointer::next=>null()
        !---Body---

        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        call this%TheEVCCluster%Clean_EVCCluster()
        this%Identify = -1

        DO While(associated(cursor))
            next=>cursor%next

            select type(cursor)
                type is(EVCClustersList)
                    call CleanEVCCluster(cursor%TheEVCCluster)
            end select
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
