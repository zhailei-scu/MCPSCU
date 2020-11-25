module MCMIGCOALE_GLOBALVARS_DEV
    use cudafor
    use curand_device
    use MCLIB_CONSTANTS
    use CudaRandomC2F_M
    use MCLIB_Utilities_GPU
    use MCMIGCOALE_TYPEDEF_SIMRECORD
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    type,public::MCMigCoale_RandDev

        integer(kind=int_ptr_kind())::m_ranGen_ClustersRandomWalk = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersReaction = 0

        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_Layer = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_X = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_Y = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSpaceDist_Z = 0
        integer(kind=int_ptr_kind())::m_ranGen_ClustersSizeDist = 0
        !---Random number array In Device
        ! The random array for diffusion direction choose
        real(kind=KINDDF),device,dimension(:),allocatable::dm_RandArray_Walk


        type(curandStateXORWOW),device,dimension(:),allocatable::dm_DevRandRecord

        ! The random array for reactions determine
        real(kind=KINDDF),device,dimension(:),allocatable::dm_RandArray_Reaction
        ! The space distribution array for new Implant clusters
        real(kind=KINDDF),device,dimension(:),allocatable::dm_SpaceDist_Implant
        ! The size distribution array for new Implant clusters
        real(kind=KINDDF),device,dimension(:),allocatable::dm_SizeDist_Implant

        contains

        procedure,non_overridable,public,pass::Init=>InitMCMigCoale_RandDev

        procedure,non_overridable,public,pass::ReSizeWalkRandNum=>ResizeMCMigCoale_WalkRandNumDev

        procedure,non_overridable,public,pass::ReSizeDevRandRecord=>ResizeMCMigCoale_WalkRandNumDevRecord

        procedure,non_overridable,public,pass::ReSizeReactionRandNum=>ResizeMCMigCoale_ReactionRandNumDev

        procedure,non_overridable,public,pass::ReSizeImplantRandNum=>ResizeMCMigCoale_ImplantRandNumDev

        procedure,non_overridable,public,pass::Clean=>Clean_MCMigCoale_RandDev
        Final::CleanMCMigCoale_RandDev
    end type MCMigCoale_RandDev

    type,public::MCMigCoale_GVarsDev

        type(MCMigCoale_RandDev)::dm_MCMigCoale_RandDev

        contains
        procedure,non_overridable,public,pass::Init=>InitMCMigCoale_GVarsDev
        procedure,non_overridable,public,pass::Clean=>Clean_MCMigCoale_GVarsDev
        Final::CleanMCMigCoale_GVarsDev
    end type MCMigCoale_GVarsDev

    private::InitMCMigCoale_RandDev
    private::ResizeMCMigCoale_WalkRandNumDev
    private::ResizeMCMigCoale_WalkRandNumDevRecord
    private::ResizeMCMigCoale_ReactionRandNumDev
    private::ResizeMCMigCoale_ImplantRandNumDev
    private::Clean_MCMigCoale_RandDev
    private::CleanMCMigCoale_RandDev
    private::InitMCMigCoale_GVarsDev
    private::Clean_MCMigCoale_GVarsDev
    private::CleanMCMigCoale_GVarsDev

    contains
    !*******************For type MCMigCoale_RandDev**********************************
    subroutine InitMCMigCoale_RandDev(this,Host_Boxes,Host_SimuCtrlParam,Record)
        use RAND32_MODULE
        use RAND32SEEDLIB_MODULE
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoale_RandDev)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MCMigCoalClusterRecord)::Record
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
        allocate(this%dm_DevRandRecord(TotalUsedNC))
        call AllocateArray_GPU(this%dm_RandArray_Reaction,TotalUsedNC,"dm_RandArray_Reaction")
        call AllocateArray_GPU(this%dm_SpaceDist_Implant,0,"dm_SpaceDist_Implant")
        call AllocateArray_GPU(this%dm_SizeDist_Implant,0,"dm_SizeDist_Implant")

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersRandomWalk .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersRandomWalk)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersRandomWalk, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersRandomWalk, INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_OutDevWalk = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersReaction .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersReaction)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersReaction,CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersReaction,INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_Reaction = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_Layer .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Layer)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_Layer, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_Layer, INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_SpaceDist_Implant_Layer = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_X .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_X)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_X, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_X, INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_SpaceDist_Implant_X = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_Y .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Y)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_Y, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_Y, INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_SpaceDist_Implant_Y = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSpaceDist_Z .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSpaceDist_Z)
        end if
        err = curandCreateGenerator(this%m_ranGen_ClustersSpaceDist_Z, CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSpaceDist_Z, INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_SpaceDist_Implant_Z = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))
        if(this%m_ranGen_ClustersSizeDist .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersSizeDist)
        end if
        err= curandCreateGenerator(this%m_ranGen_ClustersSizeDist,CURAND_RNG_PSEUDO_DEFAULT)
        err = curandSetPseudoRandomGeneratorSeed(this%m_ranGen_ClustersSizeDist,INT(SEED(1),kind=KINDINT8))
        Record%RandSeed_SizeDist_Implant = SEED

        ISEED_Curand = DRand32()*RAND32SEEDLIB_SIZE
        call GetSeed_RAND32SEEDLIB(ISEED_Curand,SEED(1),SEED(2))

        if(Host_SimuCtrlParam%UPDATETSTEPSTRATEGY .eq. mp_SelfAdjustlStep_NNDR_LastPassage_Integer) then
            call InitialDevRandRecordArray(this%dm_DevRandRecord,TotalUsedNC,SEED(1),Record%GetSimuSteps()*(3 + (Host_SimuCtrlParam%LastPassageFactor+2)*3 + 2))
            ! 3 is for three random boundary condition for 1-D diffusion , (Host_SimuCtrlParam%LastPassageFactor+2)*3 is for random walk , 2 is for the random 1-D direction for new generated cluster in pre and back merge
        else
            call InitialDevRandRecordArray(this%dm_DevRandRecord,TotalUsedNC,SEED(1),Record%GetSimuSteps()*(3 + 2))
            ! 3 is for three random boundary condition for 1-D diffusion , 2 is for the random 1-D direction for new generated cluster in pre and back merge
        end if

        Record%RandSeed_InnerDevWalk = SEED

        return
    end subroutine

    !********************************************************************
    subroutine ResizeMCMigCoale_WalkRandNumDev(this,ReSize)
        !---Dummy Vars---
        CLASS(MCMigCoale_RandDev)::this
        integer,intent(in)::ReSize
        !---Body---
        call DeAllocateArray_GPU(this%dm_RandArray_Walk,"dm_RandArray_Walk")
        call AllocateArray_GPU(this%dm_RandArray_Walk,ReSize*3,"dm_RandArray_Walk")

        return
    end subroutine ResizeMCMigCoale_WalkRandNumDev

    !********************************************************************
    subroutine ResizeMCMigCoale_WalkRandNumDevRecord(this,ReSize,Seed,Offset)
        !---Dummy Vars---
        CLASS(MCMigCoale_RandDev)::this
        integer,intent(in)::ReSize
        integer,intent(in)::Seed
        integer,intent(in)::Offset
        !---Body---
        if(allocated(this%dm_DevRandRecord)) deallocate(this%dm_DevRandRecord)

        if(ReSize .GT. 0) then
            allocate(this%dm_DevRandRecord(ReSize))
        end if

        call InitialDevRandRecordArray(this%dm_DevRandRecord,ReSize,Seed,offset)

        return
    end subroutine

    !********************************************************************

        !********************************************************************
    subroutine ResizeMCMigCoale_ReactionRandNumDev(this,ReSize)
        !---Dummy Vars---
        CLASS(MCMigCoale_RandDev)::this
        integer,intent(in)::ReSize
        !---Body---
        call DeAllocateArray_GPU(this%dm_RandArray_Reaction,"dm_RandArray_Reaction")
        call AllocateArray_GPU(this%dm_RandArray_Reaction,ReSize,"dm_RandArray_Reaction")

        return
    end subroutine ResizeMCMigCoale_ReactionRandNumDev

    !********************************************************************
    subroutine ResizeMCMigCoale_ImplantRandNumDev(this,ReSize)
        !---Dummy Vars---
        CLASS(MCMigCoale_RandDev)::this
        integer,intent(in)::ReSize
        !---Body---
        call DeAllocateArray_GPU(this%dm_SpaceDist_Implant,"dm_SpaceDist_Implant")
        call AllocateArray_GPU(this%dm_SpaceDist_Implant,ReSize*4,"dm_SpaceDist_Implant")

        call DeAllocateArray_GPU(this%dm_SizeDist_Implant,"dm_SizeDist_Implant")
        call AllocateArray_GPU(this%dm_SizeDist_Implant,ReSize,"dm_SizeDist_Implant")

        return
    end subroutine ResizeMCMigCoale_ImplantRandNumDev


    !********************************************************************
    subroutine Clean_MCMigCoale_RandDev(this)
        !---Dummy Vars---
        CLASS(MCMigCoale_RandDev)::this
        !---Local Vars---
        integer::err
        !---Body---

        call DeAllocateArray_GPU(this%dm_RandArray_Walk,"dm_RandArray_Walk")
        call DeAllocateArray_GPU(this%dm_RandArray_Reaction,"dm_RandArray_Reaction")
        call DeAllocateArray_GPU(this%dm_SpaceDist_Implant,"dm_SpaceDist_Implant")
        call DeAllocateArray_GPU(this%dm_SizeDist_Implant,"dm_SizeDist_Implant")
        deallocate(this%dm_DevRandRecord)

        if(this%m_ranGen_ClustersRandomWalk .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersRandomWalk)
        end if

        if(this%m_ranGen_ClustersReaction .GT. 0) then
            err = curandDestroyGenerator(this%m_ranGen_ClustersReaction)
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
    end subroutine Clean_MCMigCoale_RandDev

    !********************************************************************
    subroutine CleanMCMigCoale_RandDev(this)
        !---Dummy Vars---
        type(MCMigCoale_RandDev)::this
        !---Body---

        call this%Clean()

        return
    end subroutine CleanMCMigCoale_RandDev

    !********************For type MCMigCoale_GVarsDev**********************
    subroutine InitMCMigCoale_GVarsDev(this,Host_Boxes,Host_SimuCtrlParam,Record)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoale_GVarsDev)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MCMigCoalClusterRecord)::Record
        !---Body---

        call this%dm_MCMigCoale_RandDev%Init(Host_Boxes,Host_SimuCtrlParam,Record)

        return
    end subroutine InitMCMigCoale_GVarsDev

    !********************************************************************
    subroutine Clean_MCMigCoale_GVarsDev(this)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoale_GVarsDev)::this
        !---Body---

        call this%dm_MCMigCoale_RandDev%Clean()
        return
    end subroutine Clean_MCMigCoale_GVarsDev

    !********************************************************************
    subroutine CleanMCMigCoale_GVarsDev(this)
        implicit none
        !---Dummy Vars---
        type(MCMigCoale_GVarsDev)::this
        !---Body---

        call this%Clean()
        return
    end subroutine

end module MCMIGCOALE_GLOBALVARS_DEV


