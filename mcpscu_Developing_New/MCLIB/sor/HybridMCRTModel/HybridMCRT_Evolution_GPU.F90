module HYBRIDMCRT_EVOLUTION_GPU
  use cudafor
  use MCLIB_CONSTANTS
  use MCLIB_TYPEDEF_ACLUSTER
  use MCLIB_GLOBAL_GPU
  use MCLIB_TYPEDEF_GEOMETRY_GPU
  use MODEL_ECR_GPU
  implicit none


  contains

  !********************************************************
!  subroutine WalkOneStep(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,Record,TSTEP)
!    implicit none
!    !---Dummy Vars---
!    type(SimulationBoxes)::Host_Boxes
!    type(SimulationCtrlParam)::Host_SimuCtrlParam
!    type(SimulationBoxes_GPU)::Dev_Boxes
!    type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!    type(MigCoalClusterRecord)::Record
!    real(kind=KINDDF)::TSTEP
!    !---Local Vars----
!    !---Body---
!    return
!  end subroutine WalkOneStep



end module HYBRIDMCRT_EVOLUTION_GPU
