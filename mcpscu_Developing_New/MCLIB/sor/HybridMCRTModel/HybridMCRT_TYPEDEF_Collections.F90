module HYBRIDMCRT_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use HYBRIDMCRT_TYPEDEF_EVCCLUSTER
    implicit none

    type,public::Hybrid_ClusterFoldLists
        Class(SecondOrder_ClusterLists),pointer::Fold_List=>null()
        contains
        procedure,public,pass,non_overridable::AppendOne=>AppendOneFoldList
        procedure,public,pass,non_overridable::GetTailP=>GetTailP_Fold_List
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
    private::GetTailP_Fold_List
    private::CopyHybrid_ClusterFoldListsFromOther
    private::Clean_Hybrid_ClusterFoldLists
    private::CleanHybrid_ClusterFoldLists
    private::CopyHybridCollectionsFromOther
    private::Clean_HybridCollections
    private::CleanHybridCollections

    contains

    !*************************************************
    function AppendOneFoldList(this,newOne) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterFoldLists)::this
        Class(SecondOrder_ClusterLists),target::newOne
        Class(SecondOrder_ClusterLists),pointer::TheResult
        !---Local Vars---
        Class(SecondOrder_ClusterLists),pointer::cursorAddIn=>null()
        CLASS(SecondOrder_ClusterLists),pointer::cursorP=>null()
        CLASS(SecondOrder_ClusterLists),pointer::cursor=>null()
        integer::TheCount
        !---Body---
        cursorAddIn=>newOne

        Do while(associated(cursorAddIn))

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
            Associate(TheFoldList=>this%Fold_List)
            if(.not. associated(TheFoldList)) then
                select type(cursorAddIn)
                    type is(SecondOrder_AClusterLists)
                        allocate(SecondOrder_AClusterLists::TheFoldList)

                        select type(TheFoldList)
                            type is(SecondOrder_AClusterLists)
                                call TheFoldList%AppendOneClusterList(cursorAddIn%TheList,cursorAddIn%Identify)
                        end select

                    type is(EVCClustersList)
                        allocate(EVCClustersList::TheFoldList)

                        select type(TheFoldList)
                            type is(EVCClustersList)
                                call TheFoldList%AppendOneEVCCluster(cursorAddIn%TheEVCCluster,cursorAddIn%Identify)
                        end select

                end select

                Nullify(TheFoldList%next)
                TheFoldList%next=>null()
                TheFoldList%ListCount = 1

                TheResult=>TheFoldList
            else
                TheFoldList%ListCount = TheFoldList%ListCount + 1
                cursor=>TheFoldList%next
                cursorP=>TheFoldList

                DO while(associated(cursor))
                    cursor=>cursor%next
                    cursorP=>cursorP%next
                END DO

                select type(cursorAddIn)
                    type is(SecondOrder_AClusterLists)
                        allocate(SecondOrder_AClusterLists::cursor)

                        select type(cursor)
                            type is(SecondOrder_AClusterLists)
                                call cursor%AppendOneClusterList(cursorAddIn%TheList,cursorAddIn%Identify)
                        end select

                    type is(EVCClustersList)
                        allocate(EVCClustersList::cursor)
                        select type(cursor)
                            type is(EVCClustersList)
                                call cursor%AppendOneEVCCluster(cursorAddIn%TheEVCCluster,cursorAddIn%Identify)
                        end select
                end select
                Nullify(cursor%next)
                cursor%next=>null()
                cursorP%next=>cursor

                TheResult=>cursor
            end if

            END Associate

            cursorAddIn=>cursorAddIn%next

        END DO

        Nullify(cursorAddIn)
        cursorAddIn=>null()
        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()
        return
    end function AppendOneFoldList

    !*************************************************
    function GetTailP_Fold_List(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterFoldLists),intent(in),target::this
        Class(SecondOrder_ClusterLists),pointer::TheResult
        !---Local Vars---
        Class(Hybrid_ClusterFoldLists),pointer::cursorP
        !---Body---

        cursorP=>this

        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: you need to init the Hybrid_ClusterFoldLists first!"
            pause
            stop
        end if


        TheResult=>this%Fold_List

        Do while(associated(TheResult))

            TheResult=>TheResult%next
        End Do

        return
    end function GetTailP_Fold_List

    !*************************************************
    subroutine CopyHybrid_ClusterFoldListsFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterFoldLists),intent(out)::this
        type(Hybrid_ClusterFoldLists),intent(in)::Other
        !---Body---
        call this%Clean_Hybrid_ClusterFoldLists()

        call this%AppendOne(Other%Fold_List)

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
        !---Local Vars---
        CLASS(SecondOrder_ClusterLists),pointer::cursor=>null()
        CLASS(SecondOrder_ClusterLists),pointer::next=>null()
        !---Body---

        cursor=>this%Fold_List

        if(.not. associated(cursor)) then
            return
        end if

        DO While(associated(cursor))
            next=>cursor%next

            select type(cursor)
                type is(SecondOrder_AClusterLists)
                    call cursor%TheList%Clean_ClusterList()
                type is(EVCClustersList)
                    call cursor%TheEVCCluster%Clean_EVCCluster()
            end select
            cursor%Identify = -1
            cursor%ListCount = 0
            cursor%next=>null()
            deallocate(cursor)
            Nullify(cursor)
            cursor=>next
        END DO

        Nullify(cursor)
        Nullify(next)
        cursor=>null()
        next=>null()

        Nullify(this%Fold_List)
        this%Fold_List=>null()

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
