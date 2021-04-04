#include "../../../Macro"
module MF_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none

    type,public::MFCOLLECTIONS
        type(AClusterList),dimension(:),allocatable::Collections
        contains
        procedure,non_overridable,pass,public::CopyMFCollectionsFromOther
        procedure,non_overridable,pass,public::Clean_MFCollections
        procedure,non_overridable,pass,public::TransformMCToRT
        Generic::Assignment(=)=>CopyMFCollectionsFromOther
        Final::CleanMFCollections
    end type

    private::CopyMFCollectionsFromOther
    private::Clean_MFCollections
    private::CleanMFCollections
    private::TransformMCToRT
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
                !---The Assignment(=) had been overrided
                this%Collections(I) = Other%Collections(I)
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
                call this%Collections(I)%Clean_ClusterList()
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
        real(kind=KINDDF)::BoxVolum
        type(AClusterList),pointer::cursor=>null()
        !---Body---

        MultiBox = Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox

        BoxVolum = Host_SimBoxes%BOXSIZE(1)*Host_SimBoxes%BOXSIZE(2)*Host_SimBoxes%BOXSIZE(3)

        call this%Clean_MFCollections()
        allocate(this%Collections(MultiBox))

        DO IBox = 1,MultiBox

            call this%Collections(IBox)%Clean_ClusterList()

            ICFROM = Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            DO IC = ICFROM,ICTO

                if(Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                   Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU) then

                    Finded = .false.

                    if(this%Collections(IBox)%GetList_Count() .GT. 0) then
                        cursor=>this%Collections(IBox)
                        DO While(associated(cursor))
                            if(cursor%TheCluster%IsSameKindCluster((Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC))) .eq. .true.) then
                                cursor%quantififyValue = cursor%quantififyValue + 1.D0/BoxVolum
                                Finded = .true.
                            end if

                            cursor=>cursor%next
                        END DO
                    end if

                    if(Finded .eq. .false.) then
                        call this%Collections(IBox)%AppendOneCluster(Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC),1.D0/BoxVolum)
                    end if

                end if

            END DO

        END DO

        Nullify(cursor)
        cursor=>null()

        return
    end subroutine

end module
