!*********************************************************************************!
!--- Description:
!--- Author : Lei Zhai, Insti. of Nucle. Sci. and Tech., Sichuan University
!--- Email : zhaileiytp@163.com
!--- data: From 2017-09 to 2021-12
!--- License: MIT License (There are no limitation for anyone to use/modify/sale of this code, be happy to use this code)
!--- Please ref "Lei Zhai, Chaoqiong Ma, Jiechao Cui and Qing Hou, GPU-based acceleration of Monte Carlo simulations for migration-coalescence evolution of gas bubbles in materials
!---                        Modelling Simul. Mater. Sci. Eng. 2019. 27 055008,https://iopscience.iop.org/article/10.1088/1361-651X/ab1d14
!*********************************************************************************!
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
