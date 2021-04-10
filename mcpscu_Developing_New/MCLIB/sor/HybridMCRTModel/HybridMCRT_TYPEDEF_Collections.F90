module HYBRIDMCRT_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use HYBRIDMCRT_TYPEDEF_EVCCLUSTER
    implicit none

    type,public::Hybrid_ClusterFoldLists
        Class(SecondOrder_ClusterLists),pointer::Fold_List=>null()
        contains
        procedure,public,pass,non_overridable::AppendOne=>AppendOneFoldList
        procedure,public,non_overridable,pass::CopyHybrid_ClusterFoldListsFromOther
        procedure,public,non_overridable,pass::Clean_Hybrid_ClusterFoldLists
        Generic::Assignment(=)=>CopyHybrid_ClusterFoldListsFromOther
        Final::CleanHybrid_ClusterFoldLists


    end type

    !----------------------------------------
    type,public::HybridCollections
        type(Hybrid_ClusterFoldLists),dimension(:),allocatable::Collections
        contains
        procedure,non_overridable,pass,public::CopyHybridCollectionsFromOther
        procedure,non_overridable,pass,public::Clean_HybridCollections
        Generic::Assignment(=)=>CopyHybridCollectionsFromOther
        Final::CleanHybridCollections
    end type HybridCollections

    private::AppendOneFoldList
    private::CopyHybrid_ClusterFoldListsFromOther
    private::Clean_Hybrid_ClusterFoldLists
    private::CleanHybrid_ClusterFoldLists
    private::CopyHybridCollectionsFromOther
    private::Clean_HybridCollections
    private::CleanHybridCollections

    contains

    !*************************************************
    function AppendOneFoldList(this,newOne) result
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterFoldLists)::this
        Class(SecondOrder_ClusterLists)::newOne
        !---Body---
        integer,intent(in)::theIdentify
        Class(SecondOrder_ClusterLists),pointer::TheResult
        !---Local Vars---
        CLASS(SecondOrder_ClusterLists),pointer::cursor=>null(),cursorP=>null()
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
            allocate(SecondOrder_AClusterLists::cursor)
            NUllify(cursor%next)
            cursor%next=>null()

            select type(cursor)
                type is(SecondOrder_AClusterLists)
                ! The assignment(=) had been overrided
                cursor%TheList = newOne
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

        return
    end subroutine

    !*************************************************
    subroutine CopyHybrid_ClusterFoldListsFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterFoldLists),intent(out)::this
        type(Hybrid_ClusterFoldLists),intent(in)::Other
        !---Body---

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
        call this%Clean_Hybrid_ClusterFoldLists()

        Associate(OtherFoldList=>Other%Fold_List)
            if(associated(OtherFoldList)) then

                select type(OtherFoldList)

                    type is(SecondOrder_AClusterLists)
                        allocate(SecondOrder_AClusterLists::this%Fold_List)
                        call OtherFoldList%CopySecondOrder_AClusterListsToOther(this%Fold_List)
                    type is(EVCClustersList)
                        allocate(EVCClustersList::this%Fold_List)
                        call OtherFoldList%CopyEVCClustersListToOther(this%Fold_List)
                end select
            end if
        END Associate

        return
    end subroutine CopyHybrid_ClusterFoldListsFromOther

    !*************************************************
    subroutine Clean_Hybrid_ClusterFoldLists(this)
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterFoldLists)::this
        !---Body---

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
        !---Dummy Vars---
        CLASS(SecondOrder_AClusterLists),target::this
        !---Local Vars---
        CLASS(SecondOrder_ClusterLists),pointer::cursor=>null()
        CLASS(SecondOrder_ClusterLists),pointer::next=>null()
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

            select type(cursor)
                type is(SecondOrder_AClusterLists)
                    call CleanClusterList(cursor%TheList)
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
    end subroutine Clean_Hybrid_ClusterFoldLists

    !*************************************************
    subroutine CleanHybrid_ClusterFoldLists(this)
        implicit none
        !---Dummy Vars---
        type(Hybrid_ClusterFoldLists)::this
        !---Body---
        call this%Clean_Hybrid_ClusterFoldLists()

        return
    end subroutine CleanHybrid_ClusterFoldLists

    !*************************************************
    subroutine CopyHybridCollectionsFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(HybridCollections),intent(out)::this
        type(HybridCollections),intent(in)::Other
        !---Local Vars---
        integer::I
        !---Body---

        if(allocated(this%Collections)) deallocate(this%Collections)

        if(allocated(Other%Collections)) then
            allocate(this%Collections(size(Other%Collections)))

            DO I = 1,size(Other%Collections)
                !---The Assignment(=) had been overrided---
                this%Collections(I) = Other%Collections(I)
            END DO
        end if

        return
    end subroutine CopyHybridCollectionsFromOther

    !*************************************************
    subroutine Clean_HybridCollections(this)
        implicit none
        !--Dummy Vars---
        Class(HybridCollections)::this
        !---Local Vars---
        integer::I
        !---Body---
        if(allocated(this%Collections)) then
            DO I = 1,size(this%Collections)
                call this%Collections(I)%Clean_Hybrid_ClusterFoldLists()
            END DO
        end if

        deallocate(this%Collections)

        return
    end subroutine Clean_HybridCollections

    !*************************************************
    subroutine CleanHybridCollections(this)
        implicit none
        !--Dummy Vars---
        type(HybridCollections)::this
        !---Body---
        call this%Clean_HybridCollections()

        return
    end subroutine CleanHybridCollections


end module HYBRIDMCRT_TYPEDEF_COLLECTIONS
