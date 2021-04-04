#include "../../../Macro"
module MF_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    type,public,extends(ACluster)::MFCluster
        real(kind=KINDDF)::Concentrate PREASSIGN 0.D0

        contains
        procedure,non_overridable,pass,public::CopyMFClusterFromOther
        procedure,non_overridable,pass,public::Clean_MFCluster
        Generic::Assignment(=)=>CopyMFClusterFromOther
        Final::CleanMFCluster
    end type

    type,public::MFCOLLECTIONS
        type(MFCluster),dimension(:),allocatable::Collections
        integer,dimension(:,:),allocatable::SEIndexBox
        contains
        procedure,non_overridable,pass,public::CopyMFCollectionsFromOther
        procedure,non_overridable,pass,public::Clean_MFCollections
        procedure,non_overridable,pass,public::TransformMCToRT
        Generic::Assignment(=)=>CopyMFCollectionsFromOther
        Final::CleanMFCollections
    end type

    private::CopyMFClusterFromOther
    private::Clean_MFCluster
    private::CleanMFCluster
    private::CopyMFCollectionsFromOther
    private::Clean_MFCollections
    private::CleanMFCollections
    private::TransformMCToRT
    contains

    !*************************************************
    subroutine CopyMFClusterFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        CLASS(MFCluster),intent(out)::this
        type(MFCluster),intent(in)::Other
        !---Body---

        call this%ACluster%CopyClusterFromOther(Other%ACluster)
        !---The Assignment(=) cannot be defined in abstract class---
        !this%ACluster = Other%ACluster

        this%Concentrate = Other%Concentrate

    end subroutine

    !*************************************************
    subroutine Clean_MFCluster(this)
        implicit none
        !---Dummy Vars---
        Class(MFCluster)::this
        !---Body---
        this%Concentrate = 0.D0

        call this%ACluster%Clean_Cluster()
        return
    end subroutine

    !*************************************************
    subroutine CleanMFCluster(this)
        implicit none
        !---Dummy Vars---
        type(MFCluster)::this
        !---Body---
        call this%Clean_Cluster()
        return
    end subroutine

    !*************************************************
    subroutine CopyMFCollectionsFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(MFCOLLECTIONS),intent(out)::this
        type(MFCOLLECTIONS),intent(in)::Other
        !---Body---

        if(allocated(this%Collections)) deallocate(this%Collections)

        if(allocated(Other%Collections)) then
            allocate(this%Collections(size(Other%Collections)))
            this%Collections = Other%Collections
        end if

        call DeAllocateArray_Host(this%SEIndexBox,"this%SEIndexBox")

        if(allocated(Other%SEIndexBox)) then
            call AllocateArray_Host(this%SEIndexBox,size(Other%SEIndexBox,dim=1),size(Other%SEIndexBox,dim=2),"this%SEIndexBox")
            this%SEIndexBox = Other%SEIndexBox
        end if

        return
    end subroutine

    !*************************************************
    subroutine Clean_MFCollections(this)
        implicit none
        !--Dummy Vars---
        Class(MFCOLLECTIONS)::this
        !---Body---
        deallocate(this%Collections)

        call DeAllocateArray_Host(this%SEIndexBox,"this%SEIndexBox")

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


    !************************************************
    subroutine TransformMCToRT(this,Host_SimBoxes,Host_SimuCtrlParamList)
        implicit none
        !--Dummy Vars---
        Class(MFCOLLECTIONS)::this
        type(SimulationBoxes)::Host_SimBoxes
        type(SimulationCtrlParamList),target::Host_SimuCtrlParamList
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::ICFROM
        integer::ICTO
        integer::IC
        integer::IKind
        integer::IKindFrom
        integer::IKindTo
        integer::NOccur
        type(AClusterList),dimension(:),allocatable::ClusterKindLists
        type(AClusterList),pointer::cursor=>null()
        !---Body---

        MultiBox = Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox

        call DeAllocateArray_Host(this%SEIndexBox,"this%SEIndexBox")
        call AllocateArray_Host(this%SEIndexBox,MultiBox,2,"this%SEIndexBox")

        allocate(ClusterKindLists,MultiBox)

        this%SEIndexBox = 0

        TotalSize = 0

        DO IBox = 1,MultiBox

            call ClusterKindLists(IBox)%Clean_ClusterList()

            ICFROM = Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            if(IBox .ne. 1) then
                IKindFrom = this%SEIndexBox(IBox-1,2) + 1
            else
                IKindFrom = 1
            end if
            NOccur = 0

            DO IC = ICFROM,ICTO

                if(Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                   Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU) then

                    Finded = .false.

                    if(ClusterKindLists(IBox)%GetList_Count() .GT. 0) then
                        cursor=>ClusterKindLists(IBox)
                        DO While(associated(cursor))
                            if(cursor%TheCluster%IsSameKindCluster((Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC))) .eq. .true.) then
                                cursor%quantififyValue = cursor%quantififyValue + 1
                                Finded = .true.
                            end if

                            cursor=>cursor%next
                        END DO
                    end if

                    if(Finded .eq. .false.) then
                        call ClusterKindLists(IBox)%AppendOneCluster(Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC),1)
                        NOccur = NOccur + 1
                        TotalSize = TotalSize + 1
                    end if

                end if

            END DO


            this%SEIndexBox(IBox,1) = IKindFrom
            this%SEIndexBox(IBox,2) = IKindFrom + NOccur - 1

        END DO

        if(TotalSize .ne. (Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(MutliBox,2) - Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(1,1) + 1)) then
            write(*,*) "MCPSCUERROR: The calculated total kinds are not right, please check the program"
            write(*,*) TotalSize,Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(MutliBox,2) - Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(1,1) + 1
            pause
            stop
        end if


        if(TotalSize .GT. 0) then
            allocate(this%Collections(TotalSize))
        end if

        DO IBox = 1,MultiBox

            IKindFrom = this%SEIndexBox(IBox,1)
            IKindTo = this%SEIndexBox(IBox,2)

            if((IKindTo-IKindFrom+1) .ne. ClusterKindLists(IBox)%GetList_Count()) then
                write(*,*) "MCPSCUERROR: The calculated total kinds are not right in box " ,IBox, " please check the program"
                write(*,*) IKindTo-IKindFrom+1,ClusterKindLists(IBox)%GetList_Count()
            end if

            if((IKindTo-IKindFrom+1) .GT. 0) then
                cursor=>ClusterKindLists(IBox)

                DO IKind = IKindFrom,IKindTo
                    this%Collections(IKind)%ACluster = cursor%TheCluster
                    this%Collections(IKind)%Concentrate = cursor%quantififyValue
                    cursor=>cursor%next
                END DO

            end if

        END Do

        if(allocated(ClusterKindLists)) then
            DO IBox = 1,MultiBox
                call ClusterKindLists(IBox)%Clean_ClusterList()
            END DO

            deallocate(ClusterKindLists)
        end if

        Nullify(cursor)
        cursor=>null()

        return
    end subroutine

end module
