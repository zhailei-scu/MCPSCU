#include "../../../Macro"
module MF_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    TYPE,extends(ACluster),public::EVCCluster
        type(AClusterList),public::TheList
    end type

    TYPE,extends(SecondOrder_ClusterLists),public::EVCClustersList
        integer,public::Identify = 0

        integer,private::ListCount = 0
        type(EVCClustersList),pointer::next=>null()

        contains

        procedure,public,pass,non_overridable::AppendOneEVCCluster
        procedure,public,pass,non_overridable::AppendOtherEVCClustersList
        procedure,public,pass,non_overridable::GetList_Count=>GetEVCClustersList_Count
        procedure,public,pass,non_overridable::Find=>FindEVCClustersListByIdentify
        procedure,public,pass,non_overridable::CopyEVCClustersListFromOther
        procedure,public,pass,non_overridable::Clean_EVCClustersList
        Generic::Assignment(=)=>CopyEVCClustersListFromOther
        Final::CleanEVCClustersList
    END TYPE EVCClustersList


    type,public::MFCOLLECTIONS
        type(Pack_ClusterLists),dimension(:)::Collections=>null()
        contains
        procedure,non_overridable,pass,public::CopyMFCollectionsFromOther
        procedure,non_overridable,pass,public::Clean_MFCollections
        Generic::Assignment(=)=>CopyMFCollectionsFromOther
        Final::CleanMFCollections
    end type

    private::CopyMFCollectionsFromOther
    private::Clean_MFCollections
    private::CleanMFCollections
    private::AppendOneEVCCluster
    private::AppendOtherEVCClustersList
    private::GetEVCClustersList_Count
    private::FindEVCClustersListByIdentify
    private::CopyEVCClustersListFromOther
    private::Clean_EVCClustersList
    private::CleanEVCClustersList
    contains

    !*************************************************
    subroutine CopyMFCollectionsFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(MFCOLLECTIONS),intent(out)::this
        type(MFCOLLECTIONS),intent(in)::Other
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

                select type(this%Collections(I))
                    type is(SecondOrder_AClusterLists)
                        !---The Assignment(=) had been overrided
                        call this%Collections(I)%CopySecondOrder_AClusterListsFromOther(Other%Collections(I))
                    type is(EVCClustersList)
                        call this%Collections(I)%CopyEVCClustersListFromOther(Other%Collections(I))
                end select
            END DO
        end if

        return
    end subroutine

    !*************************************************
    subroutine Clean_MFCollections(this)
        implicit none
        !--Dummy Vars---
        Class(MFCOLLECTIONS)::this
        !---Local Vars---
        integer::I
        !---Body---
        if(allocated(this%Collections)) then
            DO I = 1,size(this%Collections)
                call this%Collections(I)%
            END DO
        end if

        deallocate(this%Collections)

        return
    end subroutine

    !*************************************************
    subroutine CleanMFCollections(this)
        implicit none
        !--Dummy Vars---
        type(MFCOLLECTIONS)::this
        !---Body---
        call this%Clean_MFCollections()

        return
    end subroutine CleanMFCollections

end module
