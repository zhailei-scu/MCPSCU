module MIGCOALE_TYPEDEF_CAPTURECAL_GPU
    use cudafor
    use MCLIB_CONSTANTS
    use MCLIB_UTILITIES
    use MCLIB_Utilities_GPU
    use MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    implicit none

    type,public::CaptureCal_Dev
        real(kind=KINDDF),device,dimension(:,:),allocatable::dm_CascadeCenter
        real(kind=KINDDF),device,dimension(:),allocatable::dm_maxDistance
        integer,dimension(:),device,allocatable::dm_NVACInEachBox

        real(kind=KINDDF),device,dimension(:),allocatable::dm_RSIADistributeToCent
        real(kind=KINDDF),device,dimension(:),allocatable::dm_ROutAbsorbToCent

        contains
        procedure,public,non_overridable,pass::Init=>InitCaptureCal_Dev
        procedure,public,non_overridable,pass::copyCaptureCalFromHost
        procedure,public,non_overridable,pass::Clean=>Clean_CaptureCal_Dev
        Final::CleanCaptureCal_Dev
    end type CaptureCal_Dev


    private::InitCaptureCal_Dev
    private::copyCaptureCalFromHost
    private::Clean_CaptureCal_Dev
    private::CleanCaptureCal_Dev
    contains

    subroutine InitCaptureCal_Dev(this,MultiBox,Host_CaptureCal)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal_Dev)::this
        integer,intent(in)::MultiBox
        type(CaptureCal),intent(in)::Host_CaptureCal

        !---Body---
        call DeAllocateArray_GPU(this%dm_CascadeCenter,"dm_CascadeCenter")
        call AllocateArray_GPU(this%dm_CascadeCenter,MultiBox,3,"dm_CascadeCenter")

        call DeAllocateArray_GPU(this%dm_maxDistance,"dm_maxDistance")
        call AllocateArray_GPU(this%dm_maxDistance,MultiBox,"dm_maxDistance")

        call DeAllocateArray_GPU(this%dm_NVACInEachBox,"dm_NVACInEachBox")
        call AllocateArray_GPU(this%dm_NVACInEachBox,MultiBox,"dm_NVACInEachBox")

        call DeAllocateArray_GPU(this%dm_RSIADistributeToCent,"dm_RSIADistributeToCent")
        call AllocateArray_GPU(this%dm_RSIADistributeToCent,MultiBox,"dm_RSIADistributeToCent")

        call DeAllocateArray_GPU(this%dm_ROutAbsorbToCent,"dm_ROutAbsorbToCent")
        call AllocateArray_GPU(this%dm_ROutAbsorbToCent,MultiBox,"dm_ROutAbsorbToCent")

        call this%copyCaptureCalFromHost(Host_CaptureCal)

        return
    end subroutine

    subroutine copyCaptureCalFromHost(this,Host_CapCal)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal_Dev)::this
        type(CaptureCal),intent(in)::Host_CapCal
        !---Local Vars---
        integer::NSize
        integer::err
        !---Body---
        NSize = size(Host_CapCal%m_CascadeCenter)
        if(NSize .ne. size(this%dm_CascadeCenter)) then
            write(*,*) "MCPSCUERROR: The size bwtween m_CascadeCenter and dm_CascadeCenter is not equal and the copy would be stop"
            pause
            stop
        end if
        err = cudaMemcpy(this%dm_CascadeCenter,Host_CapCal%m_CascadeCenter,NSize,cudaMemcpyHostToDevice)

        NSize = size(Host_CapCal%m_maxDistance)
        if(NSize .ne. size(this%dm_maxDistance)) then
            write(*,*) "MCPSCUERROR: The size bwtween m_maxDistance and dm_maxDistance is not equal and the copy would be stop"
            pause
            stop
        end if
        err = cudaMemcpy(this%dm_maxDistance,Host_CapCal%m_maxDistance,NSize,cudaMemcpyHostToDevice)

        NSize = size(Host_CapCal%m_NVACInEachBox)
        if(NSize .ne. size(this%dm_NVACInEachBox)) then
            write(*,*) "MCPSCUERROR: The size bwtween m_NVACInEachBox and dm_NVACInEachBox is not equal and the copy would be stop"
            pause
            stop
        end if
        err = cudaMemcpy(this%dm_NVACInEachBox,Host_CapCal%m_NVACInEachBox,NSize,cudaMemcpyHostToDevice)

        NSize = size(Host_CapCal%m_RSIADistributeToCent)
        if(NSize .ne. size(this%dm_RSIADistributeToCent)) then
            write(*,*) "MCPSCUERROR: The size bwtween dm_RSIADistributeToCent and m_RSIADistributeToCent is not equal and the copy would be stop"
            pause
            stop
        end if
        err = cudaMemcpy(this%dm_RSIADistributeToCent,Host_CapCal%m_RSIADistributeToCent,NSize,cudaMemcpyHostToDevice)

        NSize = size(Host_CapCal%m_ROutAbsorbToCent)
        if(NSize .ne. size(this%dm_ROutAbsorbToCent)) then
            write(*,*) "MCPSCUERROR: The size bwtween dm_ROutAbsorbToCent and m_ROutAbsorbToCent is not equal and the copy would be stop"
            pause
            stop
        end if
        err = cudaMemcpy(this%dm_ROutAbsorbToCent,Host_CapCal%m_ROutAbsorbToCent,NSize,cudaMemcpyHostToDevice)

        return
    end subroutine


    subroutine Clean_CaptureCal_Dev(this)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal_Dev)::this
        !---Body---
        call DeAllocateArray_GPU(this%dm_CascadeCenter,"dm_CascadeCenter")

        call DeAllocateArray_GPU(this%dm_maxDistance,"dm_maxDistance")

        call DeAllocateArray_GPU(this%dm_NVACInEachBox,"dm_NVACInEachBox")

        call DeAllocateArray_GPU(this%dm_RSIADistributeToCent,"dm_RSIADistributeToCent")

        call DeAllocateArray_GPU(this%dm_ROutAbsorbToCent,"dm_ROutAbsorbToCent")

        return
    end subroutine Clean_CaptureCal_Dev


    subroutine CleanCaptureCal_Dev(this)
        implicit none
        !---Dummy Vars---
        type(CaptureCal_Dev)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanCaptureCal_Dev


end module
