module MIGCOALE_GLOBALVARS_DEV
    use cudafor
    use MCLIB_CONSTANTS
    use CudaRandomC2F_M
    use MCLIB_Utilities_GPU
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    type,public::MigCoale_RandDev

        integer(kind=int_ptr_kind())::m_ranGen_ClustersRandomWalk = 0

        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_Layer = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_X = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_Y = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_Z = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSizeDist = 0
        !---Random number array In Device
        ! The random array for diffusion direction choose
        real(kind=KMCDF),device,dimension(:),allocatable::dm_RandArray_Walk
        ! The space distribution array for new Implant clusters
        real(kind=KMCDF),device,dimension(:),allocatable::dm_SpaceDist_Implant
        ! The size distribution array for new Implant clusters
        real(kind=KMCDF),device,dimension(:),allocatable::dm_SizeDist_Implant

        contains

        procedure,non_overridable,public,pass::Init=>InitMigCoale_RandDev

        procedure,non_overridable,public,pass::ReSizeWalkRandNum=>ResizeMigCoale_WalkRandNumDev

        procedure,non_overridable,public,pass::ReSizeImplantRandNum=>ResizeMigCoale_ImplantRandNumDev

        procedure,non_overridable,public,pass::Clean=>Clean_MigCoale_RandDev
        Final::CleanMigCoale_RandDev
    end type MigCoale_RandDev

    type,public::MigCoale_GVarsDev

        type(MigCoale_RandDev)::dm_MigCoale_RandDev

        contains
        procedure,non_overridable,public,pass::Init=>InitMigCoale_GVarsDev
        procedure,non_overridable,public,pass::Clean=>Clean_MigCoale_GVarsDev
        Final::CleanMigCoale_GVarsDev
    end type MigCoale_GVarsDev

    private::InitMigCoale_RandDev
    private::ResizeMigCoale_WalkRandNumDev
    private::ResizeMigCoale_ImplantRandNumDev
    private::Clean_MigCoale_RandDev
    private::CleanMigCoale_RandDev
    private::InitMigCoale_GVarsDev
    private::Clean_MigCoale_GVarsDev
    private::CleanMigCoale_GVarsDev

    contains
    !*******************For type MigCoale_RandDev**********************************
    subroutine InitMigCoale_RandDev(this,Host_Boxes,Host_SimuCtrlParam)
        use RAND32_MODULE
        use RAND32SEEDLIB_MODULE
        implicit none
        !---Dummy Vars---
        CLASS(MigCoale_RandDev)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        integer::MultiBox
        integer::TotalUsedNC
        integer::ISEED_Curand
        integer::SEED(2)
        integer::err
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        TotalUsedNC = sum(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1) + 1)

        call AllocateArray_GPU(this%dm_RandArray_Walk,TotalUsedNC*3,"dm_RandArray_Walk")
        call AllocateArray_GPU(this%dm_SpaceDist_Implant,0,"dm_SpaceDist_Implant")
        call AllocateArray_GPU(this%dm_SizeDist_Implant,0,"dm_SizeDist_Implant")

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersRandomWalk .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersRandomWalk)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersRandomWalk, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersRandomWalk, INT(SEED(1),kind=KMCLINT))

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_Layer .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Layer)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_Layer, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_Layer, INT(SEED(1),kind=KMCLINT))

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_X .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_X)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_X, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_X, INT(SEED(1),kind=KMCLINT))

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_Y .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Y)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_Y, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_Y, INT(SEED(1),kind=KMCLINT))

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_Z .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Z)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_Z, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_Z, INT(SEED(1),kind=KMCLINT))

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSizeDist .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSizeDist)
        end if
        err= curandCreateGenerator(this%m_ranGen_ClustersSizeDist,CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSizeDist,INT(SEED(1),kind=KMCLINT))

        return
    end subroutine

    !********************************************************************
    subroutine ResizeMigCoale_WalkRandNumDev(this,ReSize)
        !---Dummy Vars---
        CLASS(MigCoale_RandDev)::this
        integer,intent(in)::ReSize
        !---Body---
        call DeAllocateArray_GPU(this%dm_RandArray_Walk,"dm_RandArray_Walk")
        call AllocateArray_GPU(this%dm_RandArray_Walk,ReSize*3,"dm_RandArray_Walk")

        return
    end subroutine ResizeMigCoale_WalkRandNumDev

    !********************************************************************
    subroutine ResizeMigCoale_ImplantRandNumDev(this,ReSize)
        !---Dummy Vars---
        CLASS(MigCoale_RandDev)::this
        integer,intent(in)::ReSize
        !---Body---
        call DeAllocateArray_GPU(this%dm_SpaceDist_Implant,"dm_SpaceDist_Implant")
        call AllocateArray_GPU(this%dm_SpaceDist_Implant,ReSize*4,"dm_SpaceDist_Implant")

        call DeAllocateArray_GPU(this%dm_SizeDist_Implant,"dm_SizeDist_Implant")
        call AllocateArray_GPU(this%dm_SizeDist_Implant,ReSize,"dm_SizeDist_Implant")

        return
    end subroutine ResizeMigCoale_ImplantRandNumDev


    !********************************************************************
    subroutine Clean_MigCoale_RandDev(this)
        !---Dummy Vars---
        CLASS(MigCoale_RandDev)::this
        !---Local Vars---
        integer::err
        !---Body---

        call DeAllocateArray_GPU(this%dm_RandArray_Walk,"dm_RandArray_Walk")
        call DeAllocateArray_GPU(this%dm_SpaceDist_Implant,"dm_SpaceDist_Implant")
        call DeAllocateArray_GPU(this%dm_SizeDist_Implant,"dm_SizeDist_Implant")

        if(this%m_ranGen_ClustersRandomWalk .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersRandomWalk)
        end if

        if(this%m_ranGen_ClustersSpaceDist_Layer .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Layer)
        end if

        if(this%m_ranGen_ClustersSpaceDist_X .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_X)
        end if

        if(this%m_ranGen_ClustersSpaceDist_Y .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Y)
        end if

        if(this%m_ranGen_ClustersSpaceDist_Z .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Z)
        end if

        if(this%m_ranGen_ClustersSizeDist .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSizeDist)
        end if

        return
    end subroutine Clean_MigCoale_RandDev

    !********************************************************************
    subroutine CleanMigCoale_RandDev(this)
        !---Dummy Vars---
        type(MigCoale_RandDev)::this
        !---Body---

        call this%Clean()

        return
    end subroutine CleanMigCoale_RandDev

    !********************For type MigCoale_GVarsDev**********************
    subroutine InitMigCoale_GVarsDev(this,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoale_GVarsDev)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Body---

        call this%dm_MigCoale_RandDev%Init(Host_Boxes,Host_SimuCtrlParam)

        return
    end subroutine InitMigCoale_GVarsDev

    !********************************************************************
    subroutine Clean_MigCoale_GVarsDev(this)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoale_GVarsDev)::this
        !---Body---

        call this%dm_MigCoale_RandDev%Clean()
        return
    end subroutine Clean_MigCoale_GVarsDev

    !********************************************************************
    subroutine CleanMigCoale_GVarsDev(this)
        implicit none
        !---Dummy Vars---
        type(MigCoale_GVarsDev)::this
        !---Body---

        call this%Clean()
        return
    end subroutine

end module MIGCOALE_GLOBALVARS_DEV


