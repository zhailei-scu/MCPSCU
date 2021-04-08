module HYBRIDMCRT_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use HYBRIDMCRT_TYPEDEF_EVCCLUSTER
    implicit none

    type,public::Hybrid_ClusterListsFold
        Class(SecondOrder_ClusterLists),pointer::Fold_List=>null()
        contains
        procedure,public,non_overridable,pass::CopyHybrid_ClusterListsFoldFromOther
        procedure,public,non_overridable,pass::Clean_Hybrid_ClusterListsFold
        Generic::Assignment(=)=>CopyHybrid_ClusterListsFoldFromOther
        Final::CleanHybrid_ClusterListsFold
    end type

    !----------------------------------------
    type,public::HybridCollections
        type(Hybrid_ClusterListsFold),dimension(:),allocatable::Collections
        contains
        procedure,non_overridable,pass,public::CopyHybridCollectionsFromOther
        procedure,non_overridable,pass,public::Clean_HybridCollections
        Generic::Assignment(=)=>CopyHybridCollectionsFromOther
        Final::CleanHybridCollections
    end type HybridCollections

    private::CopyHybrid_ClusterListsFoldFromOther
    private::Clean_Hybrid_ClusterListsFold
    private::CleanHybrid_ClusterListsFold
    private::CopyHybridCollectionsFromOther
    private::Clean_HybridCollections
    private::CleanHybridCollections


    contains

    !*************************************************
    subroutine CopyHybrid_ClusterListsFoldFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(Hybrid_ClusterListsFold),intent(out)::this
        type(Hybrid_ClusterListsFold),intent(in)::Other

        if(associated(this%Fold_List)) then

            select type(this%Fold_List)

                type is(SecondOrder_AClusterLists)
                    call this%Fold_List%Clean
            end select

        end if


        if(associated(Other%Fold_List)) then



        end if

        return
    end subroutine


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

                select type(this%Collections(I)%Pack_List)
                    type is(SecondOrder_AClusterLists)
                        !---The Assignment(=) had been overrided
                        call this%Collections(I)%CopySecondOrder_AClusterListsFromOther(Other%Collections(I))
                    type is(EVCClustersList)
                        call this%Collections(I)%CopyEVCClustersListFromOther(Other%Collections(I))
                end select
            END DO
        end if

        return
    end subroutine CopyHybridCollectionsFromOther

    !*************************************************
    subroutine Clean_HybridCollections(this)
        implicit none
        !--Dummy Vars---
        Class(MFCOLLECTIONS)::this
        !---Local Vars---
        integer::I
        !---Body---
        if(allocated(this%Collections)) then
            DO I = 1,size(this%Collections)
                call this%Collections(I)%Clean_Pack_ClusterLists()
            END DO
        end if

        deallocate(this%Collections)

        return
    end subroutine Clean_HybridCollections

    !*************************************************
    subroutine CleanHybridCollections(this)
        implicit none
        !--Dummy Vars---
        type(MFCOLLECTIONS)::this
        !---Body---
        call this%Clean_MFCollections()

        return
    end subroutine CleanHybridCollections


end module HYBRIDMCRT_TYPEDEF_COLLECTIONS
