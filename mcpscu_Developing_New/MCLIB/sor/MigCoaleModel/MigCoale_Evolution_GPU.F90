module MIGCOALE_EVOLUTION_GPU
  use cudafor
  use MCLIB_CONSTANTS
  use MCLIB_TYPEDEF_ACLUSTER
  use MCLIB_GLOBAL_GPU
  use MIGCOALE_GLOBALVARS_DEV
  use MIGCOALE_ADDONDATA_DEV
  use MCLIB_TYPEDEF_GEOMETRY_GPU
  use MODEL_ECR_GPU
  implicit none

  contains

  !********************************************************
  subroutine WalkOneStep(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TSTEP)
    implicit none
    !---Dummy Vars---
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(SimulationBoxes_GPU)::Dev_Boxes
    type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
    real(kind=KINDDF)::TSTEP
    !---Local Vars---
    integer::MULTIBOX
    integer::IBox
    integer::BlockNumEachBox
    integer::UsedNum
    integer::maxUsedNC
    integer::TotalNC
    type(dim3)::blocks
    type(dim3)::threads
    integer::BX,BY,NB,err
    !---Body---

    ASSOCIATE(Dev_ClusterInfo_GPU=>Dev_Boxes%dm_ClusterInfo_GPU,Dev_DiffusorMap=>Dev_Boxes%dm_DiffusorTypesMap,Dev_Rand=>Dev_MigCoaleGVars%dm_MigCoale_RandDev)

        MULTIBOX = Host_SimuCtrlParam%MultiBox
        maxUsedNC = 0

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
            TotalNC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            TotalNC = 0
        end if

        DO IBox = 1,MULTIBOX
            UsedNum = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            if(UsedNum .LE. 0) then
                UsedNum = 0
            end if

            if(maxUsedNC .LT. UsedNum) then
                maxUsedNC = UsedNum
            end if
        END DO

        if(maxUsedNC .LE. 0) then
            return
        end if
        BlockNumEachBox = (maxUsedNC - 1)/p_BLOCKSIZE + 1
        NB = BlockNumEachBox*MultiBox

        !*** to determine the block size
        BX = p_BLOCKSIZE
        BY = 1
        !*** to determine the dimension of blocks

        blocks  = dim3(NB, 1, 1)
        threads = dim3(BX, BY, 1)

        ! Generate the Random number
        if(TotalNC*3 .GT. size(Dev_Rand%dm_RandArray_Walk)) then
            call Dev_Rand%ReSizeWalkRandNum(TotalNC)
        end if


        err = curandGenerateUniformDouble(Dev_Rand%m_ranGen_ClustersRandomWalk,Dev_Rand%dm_RandArray_Walk,TotalNC*3) !Async in multiple streams

        call WalkOneStep_Kernel<<<blocks,threads>>>(BlockNumEachBox,                             &
                                                    TotalNC,                                     &
                                                    Dev_ClusterInfo_GPU%dm_Clusters,             &
                                                    Dev_Boxes%dm_SEUsedIndexBox,                 &
                                                    Dev_Rand%dm_RandArray_Walk,                  &
                                                    Dev_ClusterInfo_GPU%dm_ActiveStatus,         &
                                                    Host_Boxes%m_GrainBoundary%GrainNum,         &
                                                    Dev_Boxes%dm_GrainBoundary%dm_GrainSeeds,    &
                                                    Dev_DiffusorMap%Dev_TypesEntities,           &
                                                    Dev_DiffusorMap%Dev_SingleAtomsDivideArrays, &
                                                    TSTEP)

    END ASSOCIATE

    return
  end subroutine

  !********************************************************
  attributes(global) subroutine WalkOneStep_Kernel(BlockNumEachBox,TotalNC,Dev_Clusters,Dev_SEUsedIndexBox, &
                                                   Dev_RandArray,Dev_ActiveStatu,NSeeds,Dev_GrainSeeds,Dev_TypesEntities,Dev_SingleAtomsDivideArrays,TSTEP)
    implicit none
    !---Dummy Vars---
    integer,value::BlockNumEachBox
    integer,value::TotalNC
    type(Acluster),device::Dev_Clusters(:)
    integer,device::Dev_SEUsedIndexBox(:,:)
    real(kind=KINDDF),device::Dev_RandArray(:)
    integer,device::Dev_ActiveStatu(:)
    integer,value::NSeeds
    type(GrainSeed),device::Dev_GrainSeeds(:)
    type(DiffusorTypeEntity),device::Dev_TypesEntities(:)
    integer,device::Dev_SingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
    real(kind=KINDDF),value::TSTEP
    !---Local Vars---
    integer::tid,bid,bid0,cid
    integer::IC
    integer::IBox
    integer::scid,ecid
    real(kind=KINDDF)::tempPos(3)
    real(kind=KINDDF)::crossPos(3)
    real(kind=KINDDF)::normVector(3)
    real(kind=KINDDF)::ArrowLen
    real(kind=KINDDF)::RR
    real(kind=KINDDF)::POS(3)
    real(kind=KINDSF)::SEP(3)
    real(kind=KINDDF)::Seed1Pos(3)
    real(kind=KINDDF)::Seed2Pos(3)
    integer::SeedID
    integer::Statu
    type(DiffusorValue)::TheDiffusorValue
    real(kind=KINDDF)::VectorLen
    integer::RandomSign
    !---Body---
    tid = (threadidx%y - 1)*blockdim%x + threadidx%x
    bid = (blockidx%y  - 1)*griddim%x  + blockidx%x
    cid = (bid -1)*p_BLOCKSIZE + tid

    IBox = (bid - 1)/BlockNumEachBox + 1

    bid0 = (IBox - 1)*BlockNumEachBox

    scid = Dev_SEUsedIndexBox(IBox,1)

    ecid = Dev_SEUsedIndexBox(IBox,2)

    IC = scid + (cid - bid0*p_BlockSize -1)

    if(IC .LE. ecid) then

      Statu = Dev_Clusters(IC)%m_Statu

      Dev_ActiveStatu(IC) = Statu

      if(Statu .eq. p_ACTIVEFREE_STATU) then

        ! To get the coefficiency for the cluster.
        ! This is dependent of the physics model for the dependence of diffusion coefficience
        ! on the size of the cluster

        POS = Dev_Clusters(IC)%m_POS

        !The average displacement:by using the Einstein Relation
        RR  = DSQRT(6.D0*Dev_Clusters(IC)%m_DiffCoeff*TSTEP)


        VectorLen = Dev_Clusters(IC)%m_DiffuseDirection(1)*Dev_Clusters(IC)%m_DiffuseDirection(1) + &
                    Dev_Clusters(IC)%m_DiffuseDirection(2)*Dev_Clusters(IC)%m_DiffuseDirection(2) + &
                    Dev_Clusters(IC)%m_DiffuseDirection(3)*Dev_Clusters(IC)%m_DiffuseDirection(3)

        if(VectorLen*TENPOWFIVE .LT. 1) then   ! for three-dimension-diffusion
            tempPos(1) =  Dev_RandArray(IC)-0.5D0
            tempPos(2) =  Dev_RandArray(IC + TotalNC)-0.5D0
            tempPos(3) =  Dev_RandArray(IC + 2*TotalNC)-0.5D0

            ArrowLen = DSQRT(tempPos(1)*tempPos(1) + tempPos(2)*tempPos(2) + tempPos(3)*tempPos(3))

            tempPos(1) = RR*tempPos(1)/ArrowLen
            tempPos(2) = RR*tempPos(2)/ArrowLen
            tempPos(3) = RR*tempPos(3)/ArrowLen
        else
            RandomSign = 1
            if(Dev_RandArray(IC) .GT. 0.5D0) then
                RandomSign = -1
            end if
            tempPos = RandomSign*RR*Dev_Clusters(IC)%m_DiffuseDirection  ! for one-dimension-diffusion
        end if

        tempPos = tempPos + POS

        if(tempPos(1) .GT. dm_BOXBOUNDARY(1,2) .and. dm_PERIOD(1)) then
            tempPos(1) = tempPos(1) - dm_BOXSIZE(1)
        else if(tempPos(1) .LT. dm_BOXBOUNDARY(1,1) .and. dm_PERIOD(1)) then
            tempPos(1) = tempPos(1) + dm_BOXSIZE(1)
        end if

        if(tempPos(2) .GT. dm_BOXBOUNDARY(2,2) .and. dm_PERIOD(2)) then
            tempPos(2) = tempPos(2) - dm_BOXSIZE(2)
        else if(tempPos(2) .LT. dm_BOXBOUNDARY(2,1) .and. dm_PERIOD(2)) then
            tempPos(2) = tempPos(2) + dm_BOXSIZE(2)
        end if

        if(tempPos(3) .GT. dm_BOXBOUNDARY(3,2) .and. dm_PERIOD(3)) then
            tempPos(3) = tempPos(3) - dm_BOXSIZE(3)
        else if(tempPos(3) .LT. dm_BOXBOUNDARY(3,1) .and. dm_PERIOD(3)) then
            tempPos(3) = tempPos(3) + dm_BOXSIZE(3)
        end if

        SeedID = GrainBelongsTo_Dev(NSeeds,Dev_GrainSeeds,tempPos)

        if(SeedID .ne. Dev_Clusters(IC)%m_GrainID(1)) then
            call CalCrossPointInGB_Dev(Dev_GrainSeeds,Dev_Clusters(IC)%m_GrainID(1),SeedID,POS,tempPos,crossPos)
            tempPos = crossPos
            Dev_Clusters(IC)%m_GrainID(2) = SeedID
            Dev_Clusters(IC)%m_Statu = p_ACTIVEINGB_STATU
            Dev_ActiveStatu(IC) = p_ACTIVEINGB_STATU

            ! In current implementation, the diffusion coeffficencies, radius are calculated when diffusors are created, statu changed or reaction occur
            call Dev_GetValueFromDiffusorsMap(Dev_Clusters(IC),Dev_TypesEntities,Dev_SingleAtomsDivideArrays,TheDiffusorValue)

            select case(TheDiffusorValue%ECRValueType_InGB)
                case(p_ECR_ByValue)
                    Dev_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_InGB
                case(p_ECR_ByBCluster)
                    Dev_Clusters(IC)%m_RAD = Cal_ECR_ByBCluster_Dev(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1),dm_TKB)
            end select

            select case(TheDiffusorValue%DiffusorValueType_InGB)
                case(p_DiffuseCoefficient_ByValue)
                    Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_InGB_Value
                case(p_DiffuseCoefficient_ByArrhenius)
                    Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                case(p_DiffuseCoefficient_ByBCluster)
                    ! Here we adopt a model that D=D0*(1/R)**Gama
                    Dev_Clusters(IC)%m_DiffCoeff = dm_GBSURDIFPRE*(Dev_Clusters(IC)%m_RAD**(-p_GAMMA))
                case(p_DiffuseCoefficient_BySIACluster)
                    Dev_Clusters(IC)%m_DiffCoeff = (sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_InGB))* &
                                                    TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                case(p_DiffuseCoefficient_ByVcCluster)
                    Dev_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_InGB)**(1-sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
                                                    TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
            end select

            Dev_Clusters(IC)%m_DiffuseDirection = 0.D0

        end if

        Dev_Clusters(IC)%m_POS = tempPos

        ! if the new position is out of the box, the cluster is destroyed
        if(dm_PERIOD(3) .eq. 0) then
            if((tempPos(3) - Dev_Clusters(IC)%m_RAD) .lt. dm_BOXBOUNDARY(3,1)) then
                Dev_Clusters(IC)%m_Statu = p_OUT_DESTROY_STATU
                Dev_ActiveStatu(IC) = p_OUT_DESTROY_STATU
            end if

            if(tempPos(3) .gt. dm_BOXBOUNDARY(3,2)) then
                Dev_Clusters(IC)%m_Statu = p_MIS_DESTROY_STATU
                Dev_ActiveStatu(IC) = p_MIS_DESTROY_STATU
            end if

        end if

      else if(Statu .eq. p_ACTIVEINGB_STATU) then

        POS = Dev_Clusters(IC)%m_POS

        Seed1Pos = Dev_GrainSeeds(Dev_Clusters(IC)%m_GrainID(1))%m_POS
        SEP = Seed1Pos - POS
        if(ABS(SEP(1)) .GT. dm_HBOXSIZE(1) .AND. dm_PERIOD(1) .GT. 0) then
            Seed1Pos(1) = Seed1Pos(1) - SIGN(dm_BOXSIZE(1),SEP(1))
        end if
        if(ABS(SEP(2)) .GT. dm_HBOXSIZE(2) .AND. dm_PERIOD(2) .GT. 0) then
            Seed1Pos(2) = Seed1Pos(2) - SIGN(dm_BOXSIZE(2),SEP(2))
        end if
        if(ABS(SEP(3)) .GT. dm_HBOXSIZE(3) .AND. dm_PERIOD(3) .GT. 0) then
            Seed1Pos(3) = Seed1Pos(3) - SIGN(dm_BOXSIZE(3),SEP(3))
        end if

        Seed2Pos = Dev_GrainSeeds(Dev_Clusters(IC)%m_GrainID(2))%m_POS
        SEP = Seed2Pos - POS
        if(ABS(SEP(1)) .GT. dm_HBOXSIZE(1) .AND. dm_PERIOD(1) .GT. 0) then
            Seed2Pos(1) = Seed2Pos(1) - SIGN(dm_BOXSIZE(1),SEP(1))
        end if
        if(ABS(SEP(2)) .GT. dm_HBOXSIZE(2) .AND. dm_PERIOD(2) .GT. 0) then
            Seed2Pos(2) = Seed2Pos(2) - SIGN(dm_BOXSIZE(2),SEP(2))
        end if
        if(ABS(SEP(3)) .GT. dm_HBOXSIZE(3) .AND. dm_PERIOD(3) .GT. 0) then
            Seed2Pos(3) = Seed2Pos(3) - SIGN(dm_BOXSIZE(3),SEP(3))
        end if

        normVector = Seed1Pos - Seed2Pos

        if(ABS(normVector(1)*TENPOWEIGHT) .GE. 1) then
            tempPos(2) =  Dev_RandArray(IC + TotalNC)-0.5D0
            tempPos(3) =  Dev_RandArray(IC + 2*TotalNC)-0.5D0
            tempPos(1) = -(normVector(2)*tempPos(2) + normVector(3)*tempPos(3))/normVector(1)
        end if

        if(ABS(normVector(2)*TENPOWEIGHT) .GE. 1) then
            tempPos(1) =  Dev_RandArray(IC)-0.5D0
            tempPos(3) =  Dev_RandArray(IC + 2*TotalNC)-0.5D0
            tempPos(2) = -(normVector(1)*tempPos(1) + normVector(3)*tempPos(3))/normVector(2)
        end if

        if(ABS(normVector(3)*TENPOWEIGHT) .GE. 1) then
            tempPos(1) =  Dev_RandArray(IC)-0.5D0
            tempPos(2) =  Dev_RandArray(IC + TotalNC)-0.5D0
            tempPos(3) = -(normVector(1)*tempPos(1) + normVector(2)*tempPos(2))/normVector(3)
        end if

        ArrowLen = DSQRT(tempPos(1)*tempPos(1) + tempPos(2)*tempPos(2) + tempPos(3)*tempPos(3))

        !The average displacement:by using the Einstein Relation
        RR  = DSQRT(4.D0*Dev_Clusters(IC)%m_DiffCoeff*TSTEP)

        tempPos(1) = RR*tempPos(1)/ArrowLen
        tempPos(2) = RR*tempPos(2)/ArrowLen
        tempPos(3) = RR*tempPos(3)/ArrowLen

        tempPos = tempPos + POS

        if(tempPos(1) .GT. dm_BOXBOUNDARY(1,2) .and. dm_PERIOD(1) .GT. 0) then
            tempPos(1) = tempPos(1) - dm_BOXSIZE(1)
        else if(tempPos(1) .LT. dm_BOXBOUNDARY(1,1) .and. dm_PERIOD(1) .GT. 0) then
            tempPos(1) = tempPos(1) + dm_BOXSIZE(1)
        end if

        if(tempPos(2) .GT. dm_BOXBOUNDARY(2,2) .and. dm_PERIOD(2) .GT. 0) then
            tempPos(2) = tempPos(2) - dm_BOXSIZE(2)
        else if(tempPos(2) .LT. dm_BOXBOUNDARY(2,1) .and. dm_PERIOD(2) .GT. 0) then
            tempPos(2) = tempPos(2) + dm_BOXSIZE(2)
        end if

        if(tempPos(3) .GT. dm_BOXBOUNDARY(3,2) .and. dm_PERIOD(3) .GT. 0) then
            tempPos(3) = tempPos(3) - dm_BOXSIZE(3)
        else if(tempPos(3) .LT. dm_BOXBOUNDARY(3,1) .and. dm_PERIOD(3) .GT. 0) then
            tempPos(3) = tempPos(3) + dm_BOXSIZE(3)
        end if

        Dev_Clusters(IC)%m_POS = tempPos

        ! if the new position is out of the box, the cluster is destroyed
        if(dm_PERIOD(3) .eq. 0) then
            if((tempPos(3) - Dev_Clusters(IC)%m_RAD) .lt. dm_BOXBOUNDARY(3,1)) then
                Dev_Clusters(IC)%m_Statu = p_OUT_DESTROY_STATU
                Dev_ActiveStatu(IC) = p_OUT_DESTROY_STATU
            end if

            if(tempPos(3) .gt. dm_BOXBOUNDARY(3,2)) then
                Dev_Clusters(IC)%m_Statu = p_MIS_DESTROY_STATU
                Dev_ActiveStatu(IC) = p_MIS_DESTROY_STATU
            end if
        end if

      end if

    end if

    return
  end subroutine WalkOneStep_Kernel

  !********************************************************
  subroutine MergeClusters(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TSTEP)
    implicit none
    !---Dummy Vars---
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(SimulationBoxes_GPU)::Dev_Boxes
    type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
    real(kind=KINDDF)::TSTEP
    !---Local Vars---
    integer::MULTIBOX
    integer::BlockNumEachBox
    type(dim3)::blocks
    type(dim3)::threads
    integer::BX,BY,NB,err
    real(kind=KINDDF)::ATOMV0
    real(kind=KINDDF)::DIF0
    integer::TotalNC
    !---Body---

    ASSOCIATE(Dev_ClusterInfo_GPU=>Dev_Boxes%dm_ClusterInfo_GPU,Dev_DiffusorMap=>Dev_Boxes%dm_DiffusorTypesMap,Dev_ReactionsMap=>Dev_Boxes%dm_ReactionsMap, &
              Dev_Rand=>Dev_MigCoaleGVars%dm_MigCoale_RandDev)

        MULTIBOX = Host_SimuCtrlParam%MultiBox

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
            TotalNC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            TotalNC = 0
        end if

        if(maxval(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1)) .LT. 0) then
            return
        end if

        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1)))/p_BLOCKSIZE + 1
        NB = BlockNumEachBox*MultiBox

        !*** to determine the block size
        BX = p_BLOCKSIZE
        BY = 1
        !*** to determine the dimension of blocks

        blocks  = dim3(NB, 1, 1)
        threads = dim3(BX, BY, 1)

        ! Generate the Random number
        if(TotalNC .GT. size(Dev_Rand%dm_RandArray_Reaction)) then
            call Dev_Rand%ReSizeReactionRandNum(TotalNC)
        end if

        err = curandGenerateUniformDouble(Dev_Rand%m_ranGen_ClustersReaction,Dev_Rand%dm_RandArray_Reaction,TotalNC) !Async in multiple streams

        call Merge_PreJudge_Kernel<<<blocks,threads>>>(BlockNumEachBox,                             &
                                                       Dev_ClusterInfo_GPU%dm_Clusters,             &
                                                       Dev_Boxes%dm_SEUsedIndexBox,                 &
                                                       Dev_ClusterInfo_GPU%dm_MergeINDI,            &
                                                       Dev_ClusterInfo_GPU%dm_MergeKVOIS,           &
                                                       Dev_ClusterInfo_GPU%dm_KVOIS,                &
                                                       Dev_ClusterInfo_GPU%dm_INDI)


        ! We evolute the bubble merge in GPU
        !---Pre-Direction
        call MergePre_Kernel<<<blocks,threads>>>(BlockNumEachBox,                               &
                                                 Dev_ClusterInfo_GPU%dm_Clusters,               &
                                                 Dev_Boxes%dm_SEUsedIndexBox,                   &
                                                 Dev_DiffusorMap%Dev_TypesEntities,             &
                                                 Dev_DiffusorMap%Dev_SingleAtomsDivideArrays,   &
                                                 Dev_ReactionsMap%Dev_RecordsEntities,          &
                                                 Dev_ReactionsMap%Dev_SingleAtomsDivideArrays,  &
                                                 Dev_Rand%dm_RandArray_Reaction,                &
                                                 Dev_ClusterInfo_GPU%dm_MergeINDI,              &
                                                 Dev_ClusterInfo_GPU%dm_MergeKVOIS,             &
                                                 Dev_ClusterInfo_GPU%dm_ActiveStatus)
        !---Back Direction
        call MergeBack_Kernel<<<blocks,threads>>>(BlockNumEachBox,                              &
                                                 Dev_ClusterInfo_GPU%dm_Clusters,               &
                                                 Dev_Boxes%dm_SEUsedIndexBox,                   &
                                                 Dev_DiffusorMap%Dev_TypesEntities,             &
                                                 Dev_DiffusorMap%Dev_SingleAtomsDivideArrays,   &
                                                 Dev_ReactionsMap%Dev_RecordsEntities,          &
                                                 Dev_ReactionsMap%Dev_SingleAtomsDivideArrays,  &
                                                 Dev_Rand%dm_RandArray_Reaction,                &
                                                 Dev_ClusterInfo_GPU%dm_MergeINDI,              &
                                                 Dev_ClusterInfo_GPU%dm_MergeKVOIS,             &
                                                 Dev_ClusterInfo_GPU%dm_ActiveStatus)

    END ASSOCIATE

    return
  end subroutine MergeClusters

  !********************************************************
  attributes(global) subroutine Merge_PreJudge_Kernel(BlockNumEachBox,Dev_Clusters,Dev_SEUsedIndexBox,MergeTable_INDI,MergeTable_KVOIS,Neighbor_KVOIS,Neighbor_INDI)
    implicit none
    !---Dummy Vars---
    integer,value::BlockNumEachBox
    type(Acluster),device::Dev_Clusters(:)
    integer,device::Dev_SEUsedIndexBox(:,:)
    integer,device::MergeTable_KVOIS(:)
    integer,device::MergeTable_INDI(:,:)
    integer,device::Neighbor_KVOIS(:)
    integer,device::Neighbor_INDI(:,:)
    !---Local Vars---
    integer::tid,bid,bid0,cid
    integer::IC
    integer::IBox
    integer::scid,ecid
    real(kind=KINDSF)::Pos_X,Pos_Y,Pos_Z
    real(kind=KINDSF)::Sep_X,Sep_Y,Sep_Z
    real(kind=KINDSF)::RADA,RADB,DIST,RR
    integer::N_Neighbor,NewNA
    integer::I,J,JC,NN
    !---Body---
    tid = (threadidx%y - 1)*blockdim%x + threadidx%x
    bid = (blockidx%y  - 1)*griddim%x  + blockidx%x
    cid = (bid -1)*p_BLOCKSIZE + tid

    IBox = (bid - 1)/BlockNumEachBox + 1

    bid0 = (IBox - 1)*BlockNumEachBox

    scid = Dev_SEUsedIndexBox(IBox,1)

    ecid = Dev_SEUsedIndexBox(IBox,2)

    IC = scid + (cid - bid0*p_BlockSize -1)

    if(IC .LE. ecid) then
        MergeTable_KVOIS(IC) = 0

        if(Dev_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. Dev_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU) then
            Pos_X = Dev_Clusters(IC)%m_POS(1)
            Pos_Y = Dev_Clusters(IC)%m_POS(2)
            Pos_Z = Dev_Clusters(IC)%m_POS(3)

            RADA = Dev_Clusters(IC)%m_RAD

            N_Neighbor = Neighbor_KVOIS(IC)

            NN = 0
            !scan the neighbor clusters
            DO J=1, N_Neighbor
                JC = Neighbor_INDI(IC,J)

                if(JC .GT. ecid) then
                    cycle
                end if

                if((Dev_Clusters(JC)%m_Statu .ne. p_ACTIVEFREE_STATU .AND. Dev_Clusters(JC)%m_Statu .ne. p_ACTIVEINGB_STATU) .or. IC .eq. JC) then
                    cycle
                end if

                Sep_X = Pos_X - Dev_Clusters(JC)%m_POS(1)
                Sep_Y = Pos_Y - Dev_Clusters(JC)%m_POS(2)
                Sep_Z = Pos_Z - Dev_Clusters(JC)%m_POS(3)

                if(ABS(Sep_X) .GT. dm_HBOXSIZE(1) .AND. dm_PERIOD(1)) then
                    Sep_X = Sep_X - SIGN(dm_BOXSIZE(1),Sep_X)
                end if

                if(ABS(Sep_Y) .GT. dm_HBOXSIZE(2) .AND. dm_PERIOD(2)) then
                    Sep_Y = Sep_Y - SIGN(dm_BOXSIZE(2),Sep_Y)
                end if

                if(ABS(Sep_Z) .GT. dm_HBOXSIZE(3) .AND. dm_PERIOD(3)) then
                    Sep_Z = Sep_Z - SIGN(dm_BOXSIZE(3),Sep_Z)
                end if

                Sep_X = ABS(Sep_X)
                Sep_Y = ABS(Sep_Y)
                Sep_Z = ABS(Sep_Z)

                RADB = Dev_Clusters(JC)%m_RAD

                RR = RADA+RADB

                if(Sep_X.GT.RR .or. Sep_Y.GT.RR .or. Sep_Z.GT.RR) then
                    cycle
                end if

                DIST = SQRT(Sep_X*Sep_X + Sep_Y*Sep_Y + Sep_Z*Sep_Z)
                if(DIST .GT. RR ) then
                    cycle
                end if

                NN = NN + 1

                MergeTable_INDI(IC,NN) = JC

            END DO

            MergeTable_KVOIS(IC) = NN

       end if

    end if

  end subroutine Merge_PreJudge_Kernel

  !************************************************************************
  attributes(global) subroutine MergePre_Kernel(BlockNumEachBox,Dev_Clusters,Dev_SEUsedIndexBox,Dev_DiffuTypesEntities,Dev_DiffuSingleAtomsDivideArrays, &
                                                Dev_ReactRecordsEntities,Dev_ReactSingleAtomsDivideArrays,Dev_RandArran_Reaction, &
                                                Dev_MergeINDI,Dev_MergeKVOIS,Dev_ActiveStatu)
    implicit none
    !---Dummy Vars---
    integer,value::BlockNumEachBox
    type(Acluster),device::Dev_Clusters(:)
    integer,device::Dev_SEUsedIndexBox(:,:)
    type(DiffusorTypeEntity),device::Dev_DiffuTypesEntities(:)
    integer,device::Dev_DiffuSingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
    type(ReactionEntity),device::Dev_ReactRecordsEntities(:)
    integer,device::Dev_ReactSingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
    real(kind=KINDDF),device::Dev_RandArran_Reaction(:)
    integer,device::Dev_MergeINDI(:,:)
    integer,device::Dev_MergeKVOIS(:)
    integer,device::Dev_ActiveStatu(:)
    !---Local Vars---
    integer::tid,bid,bid0,cid
    integer::IC
    integer::IBox
    integer::scid,ecid
    integer::JC
    integer::I,K,S,NewNA
    integer::N_Merge
    real::PosA_X,PosA_Y,PosA_Z,PosB_X,PosB_Y,PosB_Z
    real::Sep_X,Sep_Y,Sep_Z
    integer::tempNA,tempNB
    integer::SubjectStatu
    integer::ObjectStatu
    type(DiffusorValue)::TheDiffusorValue
    type(ReactionValue)::TheReactionValue
    real(kind=KINDDF)::ReactionCoeff
    integer::SubjectElementIndex
    integer::ObjectElementIndex
    integer::SubjectNANum
    integer::ObjectNANum
    !---Body---

    tid = (threadidx%y - 1)*blockdim%x + threadidx%x
    bid = (blockidx%y  - 1)*griddim%x  + blockidx%x
    cid = (bid -1)*p_BLOCKSIZE + tid

    IBox = (bid - 1)/BlockNumEachBox + 1

    bid0 = (IBox - 1)*BlockNumEachBox

    scid = Dev_SEUsedIndexBox(IBox,1)

    ecid = Dev_SEUsedIndexBox(IBox,2)

    IC = scid + (cid - bid0*p_BlockSize -1)

    if(IC .LE. ecid) then

      SubjectStatu = Dev_ActiveStatu(IC)

      if(SubjectStatu .eq. p_ACTIVEFREE_STATU .or. SubjectStatu .eq. p_ACTIVEINGB_STATU) then

        N_Merge = Dev_MergeKVOIS(IC)

        DO I=1,N_Merge
          JC = Dev_MergeINDI(IC,I)

          ObjectStatu = Dev_ActiveStatu(JC)

          if(IC .GT. JC .or. (ObjectStatu .ne. p_ACTIVEFREE_STATU .AND. ObjectStatu .ne. p_ACTIVEINGB_STATU)) then    ! to ensure pre-direction merge
            cycle
          end if

          !---Step 1: Self check and atomicadd one
          if(atomiccas(Dev_ActiveStatu(IC),SubjectStatu,p_ABSORBED_STATU) .NE. SubjectStatu) then
            return
          end if

          !--Step 2: Object check and  atomicadd one
          if(atomiccas(Dev_ActiveStatu(JC),ObjectStatu,p_ABSORBED_STATU) .NE. ObjectStatu) then
            S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,SubjectStatu)
            cycle
          end if

          !---Step 3:Do something

          PosA_X = Dev_Clusters(IC)%m_POS(1)
          PosA_Y = Dev_Clusters(IC)%m_POS(2)
          PosA_Z = Dev_Clusters(IC)%m_POS(3)

          PosB_X = Dev_Clusters(JC)%m_POS(1)
          PosB_Y = Dev_Clusters(JC)%m_POS(2)
          PosB_Z = Dev_Clusters(JC)%m_POS(3)

          Sep_X = PosA_X - PosB_X
          Sep_Y = PosA_Y - PosB_Y
          Sep_Z = PosA_Z - PosB_Z


          if(ABS(Sep_X).GT.dm_HBOXSIZE(1) .AND. dm_PERIOD(1)) then
             PosB_X = PosB_X + SIGN(dm_BOXSIZE(1),Sep_X)
          end if

          if(ABS(Sep_Y).GT.dm_HBOXSIZE(2) .AND. dm_PERIOD(2)) then
             PosB_Y = PosB_Y + SIGN(dm_BOXSIZE(2),Sep_Y)
          end if

          if(ABS(Sep_Z).GT.dm_HBOXSIZE(3) .AND. dm_PERIOD(3)) then
             PosB_Z = PosB_Z + SIGN(dm_BOXSIZE(3),Sep_Z)
          end if

          tempNA = sum(Dev_Clusters(IC)%m_Atoms(:)%m_NA,dim=1)
          tempNB = sum(Dev_Clusters(JC)%m_Atoms(:)%m_NA,dim=1)

          NewNA = tempNA + tempNB

          PosA_X = (PosA_X*tempNA + PosB_X*tempNB)/NewNA
          PosA_Y = (PosA_Y*tempNA + PosB_Y*tempNB)/NewNA
          PosA_Z = (PosA_Z*tempNA + PosB_Z*tempNB)/NewNA

          if(PosA_X .GT. dm_BOXBOUNDARY(1,2) .and. dm_PERIOD(1)) then
             PosA_X = PosA_X - dm_BOXSIZE(1)
          else if(PosA_X .LT. dm_BOXBOUNDARY(1,1) .and. dm_PERIOD(1)) then
             PosA_X = PosA_X + dm_BOXSIZE(1)
          end if

          if(PosA_Y .GT. dm_BOXBOUNDARY(2,2) .and. dm_PERIOD(2)) then
             PosA_Y = PosA_Y - dm_BOXSIZE(2)
          else if(PosA_Y .LT. dm_BOXBOUNDARY(2,1) .and. dm_PERIOD(2)) then
             PosA_Y = PosA_Y + dm_BOXSIZE(2)
          end if

          if(PosA_Z .GT. dm_BOXBOUNDARY(3,2) .and. dm_PERIOD(3)) then
             PosA_Z = PosA_Z - dm_BOXSIZE(3)
          else if(PosA_Z .LT. dm_BOXBOUNDARY(3,1) .and. dm_PERIOD(3)) then
             PosA_Z = PosA_Z + dm_BOXSIZE(3)
          end if

          Dev_Clusters(IC)%m_POS(1) =  PosA_X
          Dev_Clusters(IC)%m_POS(2) =  PosA_Y
          Dev_Clusters(IC)%m_POS(3) =  PosA_Z

          call Dev_GetValueFromReactionsMap(Dev_Clusters(IC),Dev_Clusters(JC),Dev_ReactRecordsEntities,Dev_ReactSingleAtomsDivideArrays,TheReactionValue)

          ReactionCoeff = 0.D0
          select case(TheReactionValue%ReactionCoefficientType)
            case(p_ReactionCoefficient_ByValue)
                 ReactionCoeff = TheReactionValue%ReactionCoefficient_Value
            case(p_ReactionCoefficient_ByArrhenius)
                 ReactionCoeff = TheReactionValue%PreFactor*exp(-C_EV2ERG*TheReactionValue%ActEnergy/dm_TKB)
          end select

          ! @todo (zhail#1#): whether the rand1() + rand2() still be normal distribution, it is necessary to be checked
          if(ReactionCoeff .GE. (Dev_RandArran_Reaction(IC) + Dev_RandArran_Reaction(JC))/2.D0) then

            select case(TheReactionValue%ProductionType)
                case(p_ProductionType_BySimplePlus)
                    Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA =  Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA + &
                                                                              Dev_Clusters(JC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
                case(p_ProductionType_BySubtract)

                    SubjectElementIndex = TheReactionValue%ElemetIndex_Subject
                    ObjectElementIndex = TheReactionValue%ElemetIndex_Object

                    Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA =  Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA + &
                                                                              Dev_Clusters(JC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA

                    SubjectNANum = Dev_Clusters(IC)%m_Atoms(SubjectElementIndex)%m_NA
                    ObjectNANum  = Dev_Clusters(IC)%m_Atoms(ObjectElementIndex)%m_NA

                    Dev_Clusters(IC)%m_Atoms(SubjectElementIndex)%m_NA = max(SubjectNANum - ObjectNANum,0)

                    Dev_Clusters(IC)%m_Atoms(ObjectElementIndex)%m_NA = max(ObjectNANum - SubjectNANum,0)
            end select

            call Dev_GetValueFromDiffusorsMap(Dev_Clusters(IC),Dev_DiffuTypesEntities,Dev_DiffuSingleAtomsDivideArrays,TheDiffusorValue)

            if(SubjectStatu .eq. p_ACTIVEFREE_STATU) then

                select case(TheDiffusorValue%ECRValueType_Free)
                    case(p_ECR_ByValue)
                        Dev_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                    case(p_ECR_ByBCluster)
                        Dev_Clusters(IC)%m_RAD =  Cal_ECR_ByBCluster_Dev(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1),dm_TKB)
                end select

                select case(TheDiffusorValue%DiffusorValueType_Free)
                    case(p_DiffuseCoefficient_ByValue)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                    case(p_DiffuseCoefficient_ByArrhenius)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
                    case(p_DiffuseCoefficient_ByBCluster)
                        ! Here we adopt a model that D=D0*(1/R)**Gama
                        Dev_Clusters(IC)%m_DiffCoeff = dm_FREESURDIFPRE*(Dev_Clusters(IC)%m_RAD**(-p_GAMMA))
                    case(p_DiffuseCoefficient_BySIACluster)
                        Dev_Clusters(IC)%m_DiffCoeff = (sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                       TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
                    case(p_DiffuseCoefficient_ByVcCluster)
                        Dev_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
                                                       TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
                end select

                Dev_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

            else if(SubjectStatu .eq. p_ACTIVEINGB_STATU) then

                select case(TheDiffusorValue%ECRValueType_InGB)
                    case(p_ECR_ByValue)
                        Dev_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_InGB
                    case(p_ECR_ByBCluster)
                        Dev_Clusters(IC)%m_RAD =  Cal_ECR_ByBCluster_Dev(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1),dm_TKB)
                end select

                select case(TheDiffusorValue%DiffusorValueType_InGB)
                    case(p_DiffuseCoefficient_ByValue)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_InGB_Value
                    case(p_DiffuseCoefficient_ByArrhenius)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                    case(p_DiffuseCoefficient_ByBCluster)
                        ! Here we adopt a model that D=D0*(1/R)**Gama
                        Dev_Clusters(IC)%m_DiffCoeff = dm_GBSURDIFPRE*(Dev_Clusters(IC)%m_RAD**(-p_GAMMA))

                    case(p_DiffuseCoefficient_BySIACluster)
                        Dev_Clusters(IC)%m_DiffCoeff = (sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_InGB))* &
                                                        TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                    case(p_DiffuseCoefficient_ByVcCluster)
                        Dev_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_InGB)**(1-sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
                                                        TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                end select
            end if

            if(dm_PERIOD(3) .EQ. 0) THEN  !We have surface
                if( (PosA_Z - Dev_Clusters(IC)%m_RAD) .LE. dm_BOXBOUNDARY(3,1)) then
                    Dev_Clusters(IC)%m_Statu = p_EXP_DESTROY_STATU
                    S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,p_EXP_DESTROY_STATU)
                endif
            end if

            if(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1) .LE. 0) then
                Dev_Clusters(IC)%m_Statu = p_ANNIHILATE_STATU
                S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,p_ANNIHILATE_STATU)
            end if

            !Dev_Clusters(JC)%m_RAD = 0.D0
            Dev_Clusters(JC)%m_Statu = p_ABSORBED_STATU
            Dev_Clusters(JC)%m_Atoms(:)%m_NA = 0
          end if

          !---Step 4:Release self
          S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,SubjectStatu)

         END DO
      end if
    end if

    return
  end subroutine MergePre_Kernel

  !************************************************************************
  attributes(global) subroutine MergeBack_Kernel(BlockNumEachBox,Dev_Clusters,Dev_SEUsedIndexBox,Dev_DiffuTypesEntities,Dev_DiffuSingleAtomsDivideArrays, &
                                                Dev_ReactRecordsEntities,Dev_ReactSingleAtomsDivideArrays,Dev_RandArran_Reaction, &
                                                Dev_MergeINDI,Dev_MergeKVOIS,Dev_ActiveStatu)
    implicit none
    !---Dummy Vars---
    integer,value::BlockNumEachBox
    type(Acluster),device::Dev_Clusters(:)
    integer,device::Dev_SEUsedIndexBox(:,:)
    type(DiffusorTypeEntity),device::Dev_DiffuTypesEntities(:)
    integer,device::Dev_DiffuSingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
    type(ReactionEntity),device::Dev_ReactRecordsEntities(:)
    integer,device::Dev_ReactSingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
    real(kind=KINDDF),device::Dev_RandArran_Reaction(:)
    integer,device::Dev_MergeINDI(:,:)
    integer,device::Dev_MergeKVOIS(:)
    integer,device::Dev_ActiveStatu(:)
    !---Local Vars---
    integer::tid,bid,bid0,cid
    integer::IC
    integer::IBox
    integer::scid,ecid
    integer::JC
    integer::I,K,S,NewNA
    integer::N_Merge
    real::PosA_X,PosA_Y,PosA_Z,PosB_X,PosB_Y,PosB_Z
    real::Sep_X,Sep_Y,Sep_Z
    integer::tempNA,tempNB
    integer::SubjectStatu
    integer::ObjectStatu
    type(DiffusorValue)::TheDiffusorValue
    type(ReactionValue)::TheReactionValue
    real(kind=KINDDF)::ReactionCoeff
    integer::SubjectElementIndex
    integer::ObjectElementIndex
    integer::SubjectNANum
    integer::ObjectNANum
    !---Body---

    tid = (threadidx%y - 1)*blockdim%x + threadidx%x
    bid = (blockidx%y  - 1)*griddim%x  + blockidx%x
    cid = (bid -1)*p_BLOCKSIZE + tid

    IBox = (bid - 1)/BlockNumEachBox + 1

    bid0 = (IBox - 1)*BlockNumEachBox

    scid = Dev_SEUsedIndexBox(IBox,1)

    ecid = Dev_SEUsedIndexBox(IBox,2)

    IC = scid + (cid - bid0*p_BlockSize -1)

    if(IC .LE. ecid) then

      SubjectStatu = Dev_ActiveStatu(IC)

      if(SubjectStatu .eq. p_ACTIVEFREE_STATU .or. SubjectStatu .eq. p_ACTIVEINGB_STATU) then

        N_Merge = Dev_MergeKVOIS(IC)

        DO I=1,N_Merge
          JC = Dev_MergeINDI(IC,I)

          ObjectStatu = Dev_ActiveStatu(JC)

          if(IC .LT. JC .or. (ObjectStatu .ne. p_ACTIVEFREE_STATU .AND. ObjectStatu .ne. p_ACTIVEINGB_STATU)) then    ! to ensure back-direction merge
            cycle
          end if

          !---Step 1: Self check and atomicadd one
          if(atomiccas(Dev_ActiveStatu(IC),SubjectStatu,p_ABSORBED_STATU) .NE. SubjectStatu) then
            return
          end if

          !--Step 2: Object check and  atomicadd one
          if(atomiccas(Dev_ActiveStatu(JC),ObjectStatu,p_ABSORBED_STATU) .NE. ObjectStatu) then
            S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,SubjectStatu)
            cycle
          end if

          !---Step 3:Do something

          PosA_X = Dev_Clusters(IC)%m_POS(1)
          PosA_Y = Dev_Clusters(IC)%m_POS(2)
          PosA_Z = Dev_Clusters(IC)%m_POS(3)

          PosB_X = Dev_Clusters(JC)%m_POS(1)
          PosB_Y = Dev_Clusters(JC)%m_POS(2)
          PosB_Z = Dev_Clusters(JC)%m_POS(3)

          Sep_X = PosA_X - PosB_X
          Sep_Y = PosA_Y - PosB_Y
          Sep_Z = PosA_Z - PosB_Z


          if(ABS(Sep_X).GT.dm_HBOXSIZE(1) .AND. dm_PERIOD(1)) then
             PosB_X = PosB_X + SIGN(dm_BOXSIZE(1),Sep_X)
          end if

          if(ABS(Sep_Y).GT.dm_HBOXSIZE(2) .AND. dm_PERIOD(2)) then
             PosB_Y = PosB_Y + SIGN(dm_BOXSIZE(2),Sep_Y)
          end if

          if(ABS(Sep_Z).GT.dm_HBOXSIZE(3) .AND. dm_PERIOD(3)) then
             PosB_Z = PosB_Z + SIGN(dm_BOXSIZE(3),Sep_Z)
          end if

          tempNA = sum(Dev_Clusters(IC)%m_Atoms(:)%m_NA,dim=1)
          tempNB = sum(Dev_Clusters(JC)%m_Atoms(:)%m_NA,dim=1)

          NewNA = tempNA + tempNB

          PosA_X = (PosA_X*tempNA + PosB_X*tempNB)/NewNA
          PosA_Y = (PosA_Y*tempNA + PosB_Y*tempNB)/NewNA
          PosA_Z = (PosA_Z*tempNA + PosB_Z*tempNB)/NewNA

          if(PosA_X .GT. dm_BOXBOUNDARY(1,2) .and. dm_PERIOD(1)) then
             PosA_X = PosA_X - dm_BOXSIZE(1)
          else if(PosA_X .LT. dm_BOXBOUNDARY(1,1) .and. dm_PERIOD(1)) then
             PosA_X = PosA_X + dm_BOXSIZE(1)
          end if

          if(PosA_Y .GT. dm_BOXBOUNDARY(2,2) .and. dm_PERIOD(2)) then
             PosA_Y = PosA_Y - dm_BOXSIZE(2)
          else if(PosA_Y .LT. dm_BOXBOUNDARY(2,1) .and. dm_PERIOD(2)) then
             PosA_Y = PosA_Y + dm_BOXSIZE(2)
          end if

          if(PosA_Z .GT. dm_BOXBOUNDARY(3,2) .and. dm_PERIOD(3)) then
             PosA_Z = PosA_Z - dm_BOXSIZE(3)
          else if(PosA_Z .LT. dm_BOXBOUNDARY(3,1) .and. dm_PERIOD(3)) then
             PosA_Z = PosA_Z + dm_BOXSIZE(3)
          end if

          Dev_Clusters(IC)%m_POS(1) =  PosA_X
          Dev_Clusters(IC)%m_POS(2) =  PosA_Y
          Dev_Clusters(IC)%m_POS(3) =  PosA_Z

          call Dev_GetValueFromReactionsMap(Dev_Clusters(IC),Dev_Clusters(JC),Dev_ReactRecordsEntities,Dev_ReactSingleAtomsDivideArrays,TheReactionValue)

          ReactionCoeff = 0.D0
          select case(TheReactionValue%ReactionCoefficientType)
            case(p_ReactionCoefficient_ByValue)
                 ReactionCoeff = TheReactionValue%ReactionCoefficient_Value
            case(p_ReactionCoefficient_ByArrhenius)
                 ReactionCoeff = TheReactionValue%PreFactor*exp(-C_EV2ERG*TheReactionValue%ActEnergy/dm_TKB)
          end select

          ! @todo (zhail#1#): whether the rand1() + rand2() still be normal distribution, it is necessary to be checked
          if(ReactionCoeff .GE. (Dev_RandArran_Reaction(IC) + Dev_RandArran_Reaction(JC))/2.D0) then

            select case(TheReactionValue%ProductionType)
                case(p_ProductionType_BySimplePlus)
                    Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA =  Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA + &
                                                                              Dev_Clusters(JC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
                case(p_ProductionType_BySubtract)

                    SubjectElementIndex = TheReactionValue%ElemetIndex_Subject
                    ObjectElementIndex = TheReactionValue%ElemetIndex_Object

                    Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA =  Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA + &
                                                                              Dev_Clusters(JC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA

                    SubjectNANum = Dev_Clusters(IC)%m_Atoms(SubjectElementIndex)%m_NA
                    ObjectNANum  = Dev_Clusters(IC)%m_Atoms(ObjectElementIndex)%m_NA

                    Dev_Clusters(IC)%m_Atoms(SubjectElementIndex)%m_NA = max(SubjectNANum - ObjectNANum,0)

                    Dev_Clusters(IC)%m_Atoms(ObjectElementIndex)%m_NA = max(ObjectNANum - SubjectNANum,0)
            end select

            call Dev_GetValueFromDiffusorsMap(Dev_Clusters(IC),Dev_DiffuTypesEntities,Dev_DiffuSingleAtomsDivideArrays,TheDiffusorValue)

            if(SubjectStatu .eq. p_ACTIVEFREE_STATU) then

                select case(TheDiffusorValue%ECRValueType_Free)
                    case(p_ECR_ByValue)
                        Dev_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                    case(p_ECR_ByBCluster)
                        Dev_Clusters(IC)%m_RAD =  Cal_ECR_ByBCluster_Dev(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1),dm_TKB)
                end select

                select case(TheDiffusorValue%DiffusorValueType_Free)
                    case(p_DiffuseCoefficient_ByValue)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                    case(p_DiffuseCoefficient_ByArrhenius)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
                    case(p_DiffuseCoefficient_ByBCluster)
                        ! Here we adopt a model that D=D0*(1/R)**Gama
                        Dev_Clusters(IC)%m_DiffCoeff = dm_FREESURDIFPRE*(Dev_Clusters(IC)%m_RAD**(-p_GAMMA))
                    case(p_DiffuseCoefficient_BySIACluster)
                        Dev_Clusters(IC)%m_DiffCoeff = (sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                       TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
                    case(p_DiffuseCoefficient_ByVcCluster)
                        Dev_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
                                                       TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
                end select

                Dev_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

            else if(SubjectStatu .eq. p_ACTIVEINGB_STATU) then

                select case(TheDiffusorValue%ECRValueType_InGB)
                    case(p_ECR_ByValue)
                        Dev_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_InGB
                    case(p_ECR_ByBCluster)
                        Dev_Clusters(IC)%m_RAD =  Cal_ECR_ByBCluster_Dev(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1),dm_TKB)
                end select

                select case(TheDiffusorValue%DiffusorValueType_InGB)
                    case(p_DiffuseCoefficient_ByValue)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_InGB_Value
                    case(p_DiffuseCoefficient_ByArrhenius)
                        Dev_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                    case(p_DiffuseCoefficient_ByBCluster)
                        ! Here we adopt a model that D=D0*(1/R)**Gama
                        Dev_Clusters(IC)%m_DiffCoeff = dm_GBSURDIFPRE*(Dev_Clusters(IC)%m_RAD**(-p_GAMMA))
                    case(p_DiffuseCoefficient_BySIACluster)
                        Dev_Clusters(IC)%m_DiffCoeff = (sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_InGB))* &
                                                        TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                    case(p_DiffuseCoefficient_ByVcCluster)
                        Dev_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_InGB)**(1-sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
                                                        TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
                end select
            end if

            if(dm_PERIOD(3) .EQ. 0) THEN  !We have surface
                if( (PosA_Z - Dev_Clusters(IC)%m_RAD) .LE. dm_BOXBOUNDARY(3,1)) then
                    Dev_Clusters(IC)%m_Statu = p_EXP_DESTROY_STATU
                    S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,p_EXP_DESTROY_STATU)
                endif
            end if

            if(sum(Dev_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1) .LE. 0) then
                Dev_Clusters(IC)%m_Statu = p_ANNIHILATE_STATU
                S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,p_ANNIHILATE_STATU)
            end if

            !Dev_Clusters(JC)%m_RAD = 0.D0
            Dev_Clusters(JC)%m_Statu = p_ABSORBED_STATU
            Dev_Clusters(JC)%m_Atoms(:)%m_NA = 0
          end if

          !---Step 4:Release self
          S = atomiccas(Dev_ActiveStatu(IC),p_ABSORBED_STATU,SubjectStatu)

         END DO
      end if
    end if

    return
  end subroutine MergeBack_Kernel

end module MIGCOALE_EVOLUTION_GPU
