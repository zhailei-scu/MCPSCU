module MCLIB_CONSTANTS_GPU
    use cudafor
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_CONSTANTS

    implicit none

    !***Box Info

    real(kind=KINDSF),constant::dm_BOXBOUNDARY(3,2)                                ! siumulation box boundary, in unit of atomic radiua
    real(kind=KINDSF),constant::dm_BOXSIZE(3)                                      ! simulation box size
    real(kind=KINDSF),constant::dm_HBOXSIZE(3)                                     ! half box size
    integer,constant::dm_PERIOD(3)
    real(kind=KINDDF),constant::dm_TKB
    real(kind=KINDDF),constant::dm_LatticeLength


    !********************GPU Parameters******************************
    integer,parameter::p_BLOCKSIZE = 128
    integer,parameter::p_BLOCKDIMX = 256
    integer,parameter::p_NUMINSEG = 10000

    !integer,parameter::p_ReduceAllStatu_BLOCKSIZE = 256
    integer,parameter::p_Reduce_BLOCKSIZE = 512

    contains


    subroutine copyInBoxParamsConstant(BOXBOUNDARY,BOXSIZE,HBOXSIZE,LatticeLength)
        implicit none
        !---dummy Vars----
        real(kind=KINDDF),intent(in)::BOXBOUNDARY(3,2)
        real(kind=KINDDF),intent(in)::BOXSIZE(3)
        real(kind=KINDDF),intent(in)::HBOXSIZE(3)
        real(kind=KINDDF),intent(in)::LatticeLength
        !---Body----
        !*** copy to device constant memory
        dm_BOXBOUNDARY = BOXBOUNDARY
        dm_BOXSIZE  = BOXSIZE
        dm_HBOXSIZE = HBOXSIZE

        dm_LatticeLength = LatticeLength

        return
    end subroutine

    subroutine copyInPhyParamsConstant(Host_SimuCtrlParam)
        implicit none
        !---dummy Vars----
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Body----
        !*** copy to device constant memory
        dm_PERIOD  = Host_SimuCtrlParam%PERIOD
        dm_TKB = Host_SimuCtrlParam%TKB

        return
    end subroutine

end module MCLIB_CONSTANTS_GPU
