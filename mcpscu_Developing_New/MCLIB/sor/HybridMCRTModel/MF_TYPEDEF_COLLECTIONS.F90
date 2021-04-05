#include "../../../Macro"
module MF_TYPEDEF_COLLECTIONS
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none

    type,public::MFCOLLECTIONS
        type(AClusterList),dimension(:,:),pointer::Collections=>null()   ! (IBox,IGroup)
        contains
        procedure,non_overridable,pass,public::CopyMFCollectionsFromOther
        procedure,non_overridable,pass,public::Clean_MFCollections
        Generic::Assignment(=)=>CopyMFCollectionsFromOther
        Final::CleanMFCollections
    end type

    private::CopyMFCollectionsFromOther
    private::Clean_MFCollections
    private::CleanMFCollections
    contains

    !*************************************************
    subroutine CopyMFCollectionsFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        Class(MFCOLLECTIONS),intent(out)::this
        type(MFCOLLECTIONS),intent(in)::Other
        !---Local Vars---
        integer::I
        integer::J
        !---Body---

        if(allocated(this%Collections)) deallocate(this%Collections)

        if(allocated(Other%Collections)) then
            allocate(this%Collections(size(Other%Collections,dim=1),size(Other%Collections,dim=2)))

            DO I = 1,size(Other%Collections,dim=1)
                DO J = 1,size(Other%Collections,dim=2)
                    !---The Assignment(=) had been overrided
                    this%Collections(I,J) = Other%Collections(I,J)
                END DO
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
        integer::J
        !---Body---
        if(allocated(this%Collections)) then
            DO I = 1,size(this%Collections,dim=1)
                DO J = 1,size(this%Collections,dim=2)
                    call this%Collections(I,J)%Clean_ClusterList()
                END DO
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
