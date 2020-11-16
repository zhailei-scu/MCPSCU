module MIGCOALE_STATISTIC_GPU
    use cudafor
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY_GPU
    use MIGCOALE_TYPEDEF_STATISTICINFO
    use MCLIB_CONSTANTS
    use MCLIB_Utilities
    use MIGCOALE_ADDONDATA_DEV
    implicit none

    real(kind=KINDDF),private,device,dimension(:,:),allocatable::dm_SumRArray
    real(kind=KINDDF),private,dimension(:,:),allocatable::m_SumRArray
    real(kind=KINDDF),private,device,dimension(:,:),allocatable::dm_MaxRArray
    real(kind=KINDDF),private,dimension(:,:),allocatable::m_MaxRArray
    real(kind=KINDDF),private,device,dimension(:,:),allocatable::dm_MinRArray
    real(kind=KINDDF),private,dimension(:,:),allocatable::m_MinRArray
    real(kind=KINDDF),private,device,dimension(:,:),allocatable::dm_MaxDiffArray
    real(kind=KINDDF),private,dimension(:,:),allocatable::m_MaxDiffArray

    contains

    !************************************************************
    subroutine GetBoxesMigCoaleStat_Used_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo,Record)
      !*** Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
      implicit none
      !---Dummy Vars---
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(SimulationBoxes_GPU)::Dev_Boxes
      type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
      CLASS(SimulationRecord)::Record
      !---Body---

      call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)

      call StatisticClusters_Used_Way3_1(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)

      return
    end subroutine GetBoxesMigCoaleStat_Used_GPU

    !******************************************
    subroutine StatisticClusters_Used_Way3_1(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IBFROM
        integer::IBTO
        integer::NCCount
        integer::BlockNumEachBox
        integer::NB
        integer::BX,BY
        type(dim3)::blocks
        type(dim3)::threads
        integer::err
        integer::IStatu
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        if(maxval(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1)) .LT. 0) then
            return
        end if

        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1)))/p_Reduce_BLOCKSIZE + 1

        BlockNumEachBox = (BlockNumEachBox -1)/2 + 1

        NB = BlockNumEachBox*MultiBox
        BX = p_Reduce_BLOCKSIZE
        BY = 1
        blocks = dim3(NB,1,1)
        threads = dim3(BX,BY,1)

        if(.not. allocated(m_SumRArray)) then
            allocate(m_SumRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_SumRArray,dim=2)) then
            !---Here, the array size is domain by the virtual or expd situation, for used situation,
            !   The array size maybe a little bigger than used size
            !   The main purpose to let what happen is that we do not want to allocate a suitable memory size
            !   for used situation in each step, because for implant, the used size is keeping changing and we should not adjustment below
            !   memory each step, so we use the virtual or expd size, which mean, while GetBoxesMigCoaleStat_Expd_GPU or
            !   GetBoxesMigCoaleStat_Virtual_GPU is used, we had get a bigger block of memory size that can ensure the usage
            !   for next N steps and need not to adjustment memory size.
            deallocate(m_SumRArray)
            allocate(m_SumRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_SumRArray)) then
            allocate(dm_SumRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_SumRArray,dim=2)) then
            deallocate(dm_SumRArray)
            allocate(dm_SumRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(m_MaxRArray)) then
            allocate(m_MaxRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MaxRArray,dim=2)) then
            deallocate(m_MaxRArray)
            allocate(m_MaxRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MaxRArray)) then
            allocate(dm_MaxRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MaxRArray,dim=2)) then
            deallocate(dm_MaxRArray)
            allocate(dm_MaxRArray(p_NUMBER_OF_STATU,NB))
        end if


        if(.not. allocated(m_MinRArray)) then
            allocate(m_MinRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MinRArray,dim=2)) then
            deallocate(m_MinRArray)
            allocate(m_MinRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MinRArray)) then
            allocate(dm_MinRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MinRArray,dim=2)) then
            deallocate(dm_MinRArray)
            allocate(dm_MinRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(m_MaxDiffArray)) then
            allocate(m_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MaxDiffArray,dim=2)) then
            deallocate(m_MaxDiffArray)
            allocate(m_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MaxDiffArray)) then
            allocate(dm_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MaxDiffArray,dim=2)) then
            deallocate(dm_MaxDiffArray)
            allocate(dm_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        end if

        call Kernel_StatisticClusters3<<<blocks,threads>>>(BlockNumEachBox,                                    &
                                                           Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,           &
                                                           Dev_Boxes%dm_SEUsedIndexBox,                        &
                                                           dm_SumRArray,                                       &
                                                           dm_MaxRArray,                                       &
                                                           dm_MinRArray,                                       &
                                                           dm_MaxDiffArray)

        m_SumRArray = dm_SumRArray
        m_MaxRArray = dm_MaxRArray
        m_MinRArray = dm_MinRArray
        m_MaxDiffArray = dm_MaxDiffArray

        DO IStatu = 1,p_NUMBER_OF_STATU

            NCCount = 0

            DO IBox = 1,MultiBox
                IBFROM = (IBox -1)*BlockNumEachBox + 1
                IBTO = IBox*BlockNumEachBox
                NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)

                if(NCCount .GT. 0) then
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(IStatu) = sum(m_SumRArray(IStatu,IBFROM:IBTO))/NCCount
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(IStatu) = maxval(m_MaxRArray(IStatu,IBFROM:IBTO))
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(IStatu) = minval(m_MinRArray(IStatu,IBFROM:IBTO))
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(IStatu) = maxval(m_MaxDiffArray(IStatu,IBFROM:IBTO))
                else
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(IStatu) = 0.D0
                end if

            END DO

            NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(IStatu)

            if(NCCount .GT. 0) then
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(IStatu) = sum(m_SumRArray(IStatu,1:NB))/NCCount
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(IStatu) = maxval(m_MaxRArray(IStatu,1:NB))
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(IStatu) = minval(m_MinRArray(IStatu,1:NB))
                TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(IStatu) = maxval(m_MaxDiffArray(IStatu,1:NB))
            else
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(IStatu) = 0.D0
            end if

        END DO

        return
    end subroutine StatisticClusters_Used_Way3_1

    !************************************************************
    subroutine GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo,Record)
      !*** Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
      implicit none
      !---Dummy Vars---
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(SimulationBoxes_GPU)::Dev_Boxes
      type(MigCoaleStatisticInfo_Expd)::TheMigCoaleStatisticInfo
      CLASS(SimulationRecord)::Record
      !---Body---

      call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)

      call StatisticClusters_Expd_Way3_1(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)

      return
    end subroutine GetBoxesMigCoaleStat_Expd_GPU

    !******************************************
    subroutine StatisticClusters_Expd_Way3_1(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoaleStatisticInfo_Expd)::TheMigCoaleStatisticInfo
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IBFROM
        integer::IBTO
        integer::NCCount
        integer::NCCountTotal
        integer::BlockNumEachBox
        integer::NB
        integer::BX,BY
        type(dim3)::blocks
        type(dim3)::threads
        integer::err
        integer::IStatu
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        if(maxval(Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,1)) .LE. 0) then
            return
        end if
        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,1)))/p_Reduce_BLOCKSIZE + 1

        BlockNumEachBox = (BlockNumEachBox -1)/2 + 1

        NB = BlockNumEachBox*MultiBox
        BX = p_Reduce_BLOCKSIZE
        BY = 1
        blocks = dim3(NB,1,1)
        threads = dim3(BX,BY,1)

        if(.not. allocated(m_SumRArray)) then
            allocate(m_SumRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_SumRArray,dim=2)) then
            !---Here, the array size is domain by the virtual situation, for expd situation,
            !   The array size maybe a little bigger than expd size
            !   The main purpose to let what happen is that we do not want to allocate a suitable memory size
            !   for expd situation while the expd boundary is changed, so we use the virtual size, which mean, while
            !   GetBoxesMigCoaleStat_Virtual_GPU is used, we had get a bigger block of memory size that can ensure the usage
            !   for next N steps and need not to adjustment memory size.
            deallocate(m_SumRArray)
            allocate(m_SumRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_SumRArray)) then
            allocate(dm_SumRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_SumRArray,dim=2)) then
            deallocate(dm_SumRArray)
            allocate(dm_SumRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(m_MaxRArray)) then
            allocate(m_MaxRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MaxRArray,dim=2)) then
            deallocate(m_MaxRArray)
            allocate(m_MaxRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MaxRArray)) then
            allocate(dm_MaxRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MaxRArray,dim=2)) then
            deallocate(dm_MaxRArray)
            allocate(dm_MaxRArray(p_NUMBER_OF_STATU,NB))
        end if


        if(.not. allocated(m_MinRArray)) then
            allocate(m_MinRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MinRArray,dim=2)) then
            deallocate(m_MinRArray)
            allocate(m_MinRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MinRArray)) then
            allocate(dm_MinRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MinRArray,dim=2)) then
            deallocate(dm_MinRArray)
            allocate(dm_MinRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(m_MaxDiffArray)) then
            allocate(m_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MaxDiffArray,dim=2)) then
            deallocate(m_MaxDiffArray)
            allocate(m_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MaxDiffArray)) then
            allocate(dm_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MaxDiffArray,dim=2)) then
            deallocate(dm_MaxDiffArray)
            allocate(dm_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        end if

        call Kernel_StatisticClusters3<<<blocks,threads>>>(BlockNumEachBox,                            &
                                                           Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,   &
                                                           Dev_Boxes%dm_SEExpdIndexBox,                &
                                                           dm_SumRArray,                               &
                                                           dm_MaxRArray,                               &
                                                           dm_MinRArray,                               &
                                                           dm_MaxDiffArray)

        m_SumRArray = dm_SumRArray
        m_MaxRArray = dm_MaxRArray
        m_MinRArray = dm_MinRArray
        m_MaxDiffArray  = dm_MaxDiffArray

        DO IStatu = 1,p_NUMBER_OF_STATU

            NCCountTotal = 0

            DO IBox = 1,MultiBox
                IBFROM = (IBox -1)*BlockNumEachBox + 1
                IBTO = IBox*BlockNumEachBox
                if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
                    if(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu) .GT. 0) then
                        NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)  &
                                + Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                    else
                        NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
                    end if
                else
                    NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
                end if

                if(NCCount .GT. 0) then
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(IStatu) = sum(m_SumRArray(IStatu,IBFROM:IBTO))/NCCount
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(IStatu) = maxval(m_MaxRArray(IStatu,IBFROM:IBTO))
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(IStatu) = minval(m_MinRArray(IStatu,IBFROM:IBTO))
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(IStatu) = maxval(m_MaxDiffArray(IStatu,IBFROM:IBTO))
                else
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(IStatu) = 0.D0
                end if

                NCCountTotal = NCCountTotal + NCCount

            END DO

            if(NCCountTotal .GT. 0) then
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(IStatu) = sum(m_SumRArray(IStatu,1:NB))/NCCountTotal
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(IStatu) = maxval(m_MaxRArray(IStatu,1:NB))
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(IStatu) = minval(m_MinRArray(IStatu,1:NB))
                TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(IStatu) = maxval(m_MaxDiffArray(IStatu,1:NB))
            else
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(IStatu) = 0.D0
            end if

        END DO

        return
    end subroutine StatisticClusters_Expd_Way3_1

    !************************************************************
    subroutine GetBoxesMigCoaleStat_Virtual_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo,Record)
      !*** Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
      implicit none
      !---Dummy Vars---
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(SimulationBoxes_GPU)::Dev_Boxes
      type(MigCoaleStatisticInfo_Virtual)::TheMigCoaleStatisticInfo
      CLASS(SimulationRecord)::Record
      !---Body---

      call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)

      call StatisticClusters_Virtual_Way3_1(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
      return
    end subroutine GetBoxesMigCoaleStat_Virtual_GPU

    !******************************************
    subroutine StatisticClusters_Virtual_Way3_1(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoaleStatisticInfo_Virtual)::TheMigCoaleStatisticInfo
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IBFROM
        integer::IBTO
        integer::NCCount
        integer::NCCountTotal
        integer::BlockNumEachBox
        integer::NB
        integer::BX,BY
        type(dim3)::blocks
        type(dim3)::threads
        integer::err
        integer::IStatu
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .LE. 0) then
            return
        end if

        if(maxval(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,1) + 1) .LE. 0) then
            return
        end if
        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,1)))/p_Reduce_BLOCKSIZE + 1

        BlockNumEachBox = (BlockNumEachBox -1)/2 + 1

        NB = BlockNumEachBox*MultiBox
        BX = p_Reduce_BLOCKSIZE
        BY = 1
        blocks = dim3(NB,1,1)
        threads = dim3(BX,BY,1)

        if(.not. allocated(m_SumRArray)) then
            allocate(m_SumRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_SumRArray,dim=2)) then
            deallocate(m_SumRArray)
            allocate(m_SumRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_SumRArray)) then
            allocate(dm_SumRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_SumRArray,dim=2)) then
            deallocate(dm_SumRArray)
            allocate(dm_SumRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(m_MaxRArray)) then
            allocate(m_MaxRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MaxRArray,dim=2)) then
            deallocate(m_MaxRArray)
            allocate(m_MaxRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MaxRArray)) then
            allocate(dm_MaxRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MaxRArray,dim=2)) then
            deallocate(dm_MaxRArray)
            allocate(dm_MaxRArray(p_NUMBER_OF_STATU,NB))
        end if


        if(.not. allocated(m_MinRArray)) then
            allocate(m_MinRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MinRArray,dim=2)) then
            deallocate(m_MinRArray)
            allocate(m_MinRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MinRArray)) then
            allocate(dm_MinRArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MinRArray,dim=2)) then
            deallocate(dm_MinRArray)
            allocate(dm_MinRArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(m_MaxDiffArray)) then
            allocate(m_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(m_MaxDiffArray,dim=2)) then
            deallocate(m_MaxDiffArray)
            allocate(m_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        end if

        if(.not. allocated(dm_MaxDiffArray)) then
            allocate(dm_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        else if(NB .GT. size(dm_MaxDiffArray,dim=2)) then
            deallocate(dm_MaxDiffArray)
            allocate(dm_MaxDiffArray(p_NUMBER_OF_STATU,NB))
        end if

        call Kernel_StatisticClusters3<<<blocks,threads>>>(BlockNumEachBox,                                     &
                                                           Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,            &
                                                           Dev_Boxes%dm_SEVirtualIndexBox,                      &
                                                           dm_SumRArray,                                        &
                                                           dm_MaxRArray,                                        &
                                                           dm_MinRArray,                                        &
                                                           dm_MaxDiffArray)

        m_SumRArray = dm_SumRArray
        m_MaxRArray = dm_MaxRArray
        m_MinRArray = dm_MinRArray
        m_MaxDiffArray = dm_MaxDiffArray

        DO IStatu = 1,p_NUMBER_OF_STATU

            NCCountTotal = 0

            DO IBox = 1,MultiBox
                IBFROM = (IBox -1)*BlockNumEachBox + 1
                IBTO = IBox*BlockNumEachBox

                if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
                    if(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu) .GT. 0) then

                        NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)  &
                                + Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                    else
                        NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
                    end if
                else
                    NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
                end if

                if(NCCount .GT. 0) then
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(IStatu) = sum(m_SumRArray(IStatu,IBFROM:IBTO))/NCCount
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(IStatu) = maxval(m_MaxRArray(IStatu,IBFROM:IBTO))
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(IStatu) = minval(m_MinRArray(IStatu,IBFROM:IBTO))
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(IStatu) = maxval(m_MaxDiffArray(IStatu,IBFROM:IBTO))
                else
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(IStatu) = 0.D0
                    TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(IStatu) = 0.D0
                end if

                NCCountTotal = NCCountTotal + NCCount

            END DO

            if(NCCountTotal .GT. 0) then
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(IStatu) = sum(m_SumRArray(IStatu,1:NB))/NCCountTotal
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(IStatu) = maxval(m_MaxRArray(IStatu,1:NB))
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(IStatu) = minval(m_MinRArray(IStatu,1:NB))
                TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(IStatu) = maxval(m_MaxDiffArray(IStatu,1:NB))
            else
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(IStatu) = 0.D0
                TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(IStatu) = 0.D0
            end if
        END DO

        return
    end subroutine StatisticClusters_Virtual_Way3_1

    !*******************************************
    attributes(global) subroutine Kernel_StatisticClusters3(BlockNumEachBox,DevArray,Dev_SEIndexBox,&
                                                            ResultSumRadiusArray,                   &
                                                            ResultMaxRadiusArray,                   &
                                                            ResultMinRadiusArray,                   &
                                                            ResultMaxDiffArray)
        !use libm
        implicit none
        !---Dummy Vars---
        integer,value::BlockNumEachBox
        type(ACluster),device::DevArray(:)
        integer,device::Dev_SEIndexBox(:,:)
        real(kind=KINDDF),device::ResultSumRadiusArray(p_NUMBER_OF_STATU,*) ! When the nollvm compiler option is used, the attributes(device) dummy vars array should write as (x,*) for two dimension, cannot be (:,:)
        real(kind=KINDDF),device::ResultMaxRadiusArray(p_NUMBER_OF_STATU,*)
        real(kind=KINDDF),device::ResultMinRadiusArray(p_NUMBER_OF_STATU,*)
        real(kind=KINDDF),device::ResultMaxDiffArray(p_NUMBER_OF_STATU,*)
        !---Local Vars---
        integer::tid
        integer::bid
        integer::cid
        integer::bid0
        integer::IBox
        integer::scid
        integer::ecid
        integer::IC
        real(kind=KINDDF),shared::Share_SumRadiusOneStatu(p_Reduce_BLOCKSIZE)
        real(kind=KINDDF),shared::Share_MaxRadiusOneStatu(p_Reduce_BLOCKSIZE)
        real(kind=KINDDF),shared::Share_MinRadiusOneStatu(p_Reduce_BLOCKSIZE)
        real(kind=KINDDF),shared::Share_MaxDiffOneStatu(p_Reduce_BLOCKSIZE)
        integer::I
        real(kind=KINDDF)::tempRadius
        real(kind=KINDDF)::tempDIF
        integer::IStatu
        !---Body---
        tid = (threadidx%y - 1)*blockdim%x + threadidx%x
        bid = (blockidx%y  - 1)*griddim%x  + blockidx%x
        cid = (bid -1)*p_Reduce_BLOCKSIZE + tid

        IBox = (bid - 1)/BlockNumEachBox + 1

        bid0 = (IBox - 1)*BlockNumEachBox + 1

        scid = Dev_SEIndexBox(IBox,1)

        ecid = Dev_SEIndexBox(IBox,2)

        IC = scid + (bid - bid0)*p_Reduce_BLOCKSIZE*2 + tid - 1

        DO IStatu = 1,p_NUMBER_OF_STATU

            Share_SumRadiusOneStatu(tid) = 0.D0
            Share_MaxRadiusOneStatu(tid) = -1.D32
            Share_MinRadiusOneStatu(tid) = 1.D32
            Share_MaxDiffOneStatu(tid) = -1.D32

            if(IC .LE. ecid) then
                if(DevArray(IC)%m_Statu .eq. IStatu) then

                    Share_SumRadiusOneStatu(tid) = DevArray(IC)%m_RAD
                    Share_MaxRadiusOneStatu(tid) = DevArray(IC)%m_RAD
                    Share_MinRadiusOneStatu(tid) = DevArray(IC)%m_RAD
                    Share_MaxDiffOneStatu(tid) = DevArray(IC)%m_DiffCoeff
                end if

            end if

            if((IC + p_Reduce_BLOCKSIZE) .LE. ecid) then

                if(DevArray(IC + p_Reduce_BLOCKSIZE)%m_Statu .eq. IStatu) then

                    tempRadius = DevArray(IC + p_Reduce_BLOCKSIZE)%m_RAD

                    tempDIF = DevArray(IC + p_Reduce_BLOCKSIZE)%m_DiffCoeff

                    Share_SumRadiusOneStatu(tid) = Share_SumRadiusOneStatu(tid) + tempRadius

                    if(Share_MaxRadiusOneStatu(tid) .LT. tempRadius) then
                        Share_MaxRadiusOneStatu(tid) = tempRadius
                    end if

                    if(Share_MinRadiusOneStatu(tid) .GT. tempRadius) then
                        Share_MinRadiusOneStatu(tid) = tempRadius
                    end if

                    if(Share_MaxDiffOneStatu(tid) .LT. tempDIF) then
                        Share_MaxDiffOneStatu(tid) = tempDIF
                    end if

                end if

            end if

            call syncthreads()

            I = p_Reduce_BLOCKSIZE/2
            DO While(I .GT. 0)

                if(tid .LE. I) then

                    Share_SumRadiusOneStatu(tid) = Share_SumRadiusOneStatu(tid) + Share_SumRadiusOneStatu(tid+I)

                    if(Share_MaxRadiusOneStatu(tid) .LT. Share_MaxRadiusOneStatu(tid+I)) then
                        Share_MaxRadiusOneStatu(tid) = Share_MaxRadiusOneStatu(tid+I)
                    end if

                    if(Share_MinRadiusOneStatu(tid) .GT. Share_MinRadiusOneStatu(tid+I)) then
                        Share_MinRadiusOneStatu(tid) = Share_MinRadiusOneStatu(tid+I)
                    end if

                    if(Share_MaxDiffOneStatu(tid) .LT. Share_MaxDiffOneStatu(tid+I)) then
                        Share_MaxDiffOneStatu(tid) = Share_MaxDiffOneStatu(tid+I)
                    end if
                end if

                call syncthreads()

                I = I/2
            END DO


            if(tid .eq. 1) then
                ResultSumRadiusArray(IStatu,bid) = Share_SumRadiusOneStatu(1)
                ResultMaxRadiusArray(IStatu,bid) = Share_MaxRadiusOneStatu(1)
                ResultMinRadiusArray(IStatu,bid) = Share_MinRadiusOneStatu(1)
                ResultMaxDiffArray(IStatu,bid) = Share_MaxDiffOneStatu(1)
            end if

            call syncthreads()

        END DO

        return
    end subroutine Kernel_StatisticClusters3

!    !************************************************************
!    subroutine GetBoxesMigCoaleStat_Used_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!      !*** Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
!      implicit none
!      !---Dummy Vars---
!      type(SimulationBoxes)::Host_Boxes
!      type(SimulationCtrlParam)::Host_SimuCtrlParam
!      type(SimulationBoxes_GPU)::Dev_Boxes
!      type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
!      !---Local Vars---
!      integer::Statu
!      !---Body---
!
!      call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)
!
!      DO Statu = 1,p_NUMBER_OF_STATU
!        call StatisticOneStatuClusters_Used_Way3_1(Statu,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!      END DO
!
!      return
!    end subroutine GetBoxesMigCoaleStat_Used_GPU
!
!    !******************************************
!    subroutine StatisticOneStatuClusters_Used_Way3_1(Statu,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
!        !---Local Vars---
!        integer::MultiBox
!        integer::IBox
!        integer::IBFROM
!        integer::IBTO
!        integer::NCCount
!        integer::BlockNumEachBox
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::err
!        !---Body---
!        MultiBox = Host_SimuCtrlParam%MultiBox
!
!        if(maxval(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1)) .LT. 0) then
!            return
!        end if
!
!        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(:,1)))/p_Reduce_BLOCKSIZE + 1
!
!        BlockNumEachBox = (BlockNumEachBox -1)/2 + 1
!
!        NB = BlockNumEachBox*MultiBox
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .GT. size(m_SumROneStatuArray)) then
!            !---Here, the array size is domain by the virtual or expd situation, for used situation,
!            !   The array size maybe a little bigger than used size
!            !   The main purpose to let what happen is that we do not want to allocate a suitable memory size
!            !   for used situation in each step, because for implant, the used size is keeping changing and we should not adjustment below
!            !   memory each step, so we use the virtual or expd size, which mean, while GetBoxesMigCoaleStat_Expd_GPU or
!            !   GetBoxesMigCoaleStat_Virtual_GPU is used, we had get a bigger block of memory size that can ensure the usage
!            !   for next N steps and need not to adjustment memory size.
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .GT. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .GT. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .GT. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .GT. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .GT. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .GT. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .GT. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!        call Kernel_StatisticOneStatuClusters3<<<blocks,threads>>>(Statu,                                                       &
!                                                                   BlockNumEachBox,                                             &
!                                                                   Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                    &
!                                                                   Dev_Boxes%dm_SEUsedIndexBox,                                 &
!                                                                   dm_SumROneStatuArray,                                        &
!                                                                   dm_MaxROneStatuArray,                                        &
!                                                                   dm_MinROneStatuArray,                                        &
!                                                                   dm_MaxDiffOneStatuArray)
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        m_MaxDiffOneStatuArray = dm_MaxDiffOneStatuArray
!
!        DO IBox = 1,MultiBox
!            IBFROM = (IBox -1)*BlockNumEachBox + 1
!            IBTO = IBox*BlockNumEachBox
!            NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)
!
!            if(NCCount .GT. 0) then
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(Statu) = sum(m_SumROneStatuArray(IBFROM:IBTO))/NCCount
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(Statu) = maxval(m_MaxROneStatuArray(IBFROM:IBTO))
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(Statu) = minval(m_MinROneStatuArray(IBFROM:IBTO))
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(Statu) = maxval(m_MaxDiffOneStatuArray(IBFROM:IBTO))
!            else
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(Statu) = 0.D0
!            end if
!
!        END DO
!
!        NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(Statu)
!
!        if(NCCount .GT. 0) then
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(Statu) = sum(m_SumROneStatuArray(1:NB))/NCCount
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(Statu) = maxval(m_MaxROneStatuArray(1:NB))
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(Statu) = minval(m_MinROneStatuArray(1:NB))
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(Statu) = maxval(m_MaxDiffOneStatuArray(1:NB))
!        else
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(Statu) = 0.D0
!        end if
!
!        return
!    end subroutine StatisticOneStatuClusters_Used_Way3_1
!
!        !************************************************************
!    subroutine GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!      !*** Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
!      implicit none
!      !---Dummy Vars---
!      type(SimulationBoxes)::Host_Boxes
!      type(SimulationCtrlParam)::Host_SimuCtrlParam
!      type(SimulationBoxes_GPU)::Dev_Boxes
!      type(MigCoaleStatisticInfo_Expd)::TheMigCoaleStatisticInfo
!      !---Local Vars---
!      integer::Statu
!      !---Body---
!
!      call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)
!
!      DO Statu = 1,p_NUMBER_OF_STATU
!        call StatisticOneStatuClusters_Expd_Way3_1(Statu,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!      END DO
!
!      return
!    end subroutine GetBoxesMigCoaleStat_Expd_GPU
!
!    !******************************************
!    subroutine StatisticOneStatuClusters_Expd_Way3_1(Statu,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoaleStatisticInfo_Expd)::TheMigCoaleStatisticInfo
!        !---Local Vars---
!        integer::MultiBox
!        integer::IBox
!        integer::IBFROM
!        integer::IBTO
!        integer::NCCount
!        integer::NCCountTotal
!        integer::BlockNumEachBox
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::err
!        !---Body---
!        MultiBox = Host_SimuCtrlParam%MultiBox
!
!        if(maxval(Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,1)) .LE. 0) then
!            return
!        end if
!        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEExpdIndexBox(:,1)))/p_Reduce_BLOCKSIZE + 1
!
!        BlockNumEachBox = (BlockNumEachBox -1)/2 + 1
!
!        NB = BlockNumEachBox*MultiBox
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .GT. size(m_SumROneStatuArray)) then
!            !---Here, the array size is domain by the virtual situation, for expd situation,
!            !   The array size maybe a little bigger than expd size
!            !   The main purpose to let what happen is that we do not want to allocate a suitable memory size
!            !   for expd situation while the expd boundary is changed, so we use the virtual size, which mean, while
!            !   GetBoxesMigCoaleStat_Virtual_GPU is used, we had get a bigger block of memory size that can ensure the usage
!            !   for next N steps and need not to adjustment memory size.
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .GT. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .GT. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .GT. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .GT. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .GT. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .GT. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .GT. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!        call Kernel_StatisticOneStatuClusters3<<<blocks,threads>>>(Statu,                                                       &
!                                                                   BlockNumEachBox,                                             &
!                                                                   Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                    &
!                                                                   Dev_Boxes%dm_SEExpdIndexBox,                                 &
!                                                                   dm_SumROneStatuArray,                                        &
!                                                                   dm_MaxROneStatuArray,                                        &
!                                                                   dm_MinROneStatuArray,                                        &
!                                                                   dm_MaxDiffOneStatuArray)
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        m_MaxDiffOneStatuArray  = dm_MaxDiffOneStatuArray
!
!        NCCountTotal = 0
!
!        DO IBox = 1,MultiBox
!            IBFROM = (IBox -1)*BlockNumEachBox + 1
!            IBTO = IBox*BlockNumEachBox
!            if(Statu .eq. p_ACTIVEFREE_STATU .or. Statu .eq. p_ACTIVEINGB_STATU) then
!                if(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu) .GT. 0) then
!                    NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)  &
!                            + Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
!                else
!                    NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)
!                end if
!            else
!                NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)
!            end if
!
!            if(NCCount .GT. 0) then
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(Statu) = sum(m_SumROneStatuArray(IBFROM:IBTO))/NCCount
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(Statu) = maxval(m_MaxROneStatuArray(IBFROM:IBTO))
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(Statu) = minval(m_MinROneStatuArray(IBFROM:IBTO))
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(Statu) = maxval(m_MaxDiffOneStatuArray(IBFROM:IBTO))
!            else
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(Statu) = 0.D0
!            end if
!
!            NCCountTotal = NCCountTotal + NCCount
!
!        END DO
!
!        if(NCCountTotal .GT. 0) then
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(Statu) = sum(m_SumROneStatuArray(1:NB))/NCCountTotal
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(Statu) = maxval(m_MaxROneStatuArray(1:NB))
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(Statu) = minval(m_MinROneStatuArray(1:NB))
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(Statu) = maxval(m_MaxDiffOneStatuArray(1:NB))
!        else
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(Statu) = 0.D0
!        end if
!
!        return
!    end subroutine StatisticOneStatuClusters_Expd_Way3_1
!
!    !************************************************************
!    subroutine GetBoxesMigCoaleStat_Virtual_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!      !*** Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
!      implicit none
!      !---Dummy Vars---
!      type(SimulationBoxes)::Host_Boxes
!      type(SimulationCtrlParam)::Host_SimuCtrlParam
!      type(SimulationBoxes_GPU)::Dev_Boxes
!      type(MigCoaleStatisticInfo_Virtual)::TheMigCoaleStatisticInfo
!      !---Local Vars---
!      integer::Statu
!      !---Body---
!
!      call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)
!
!      DO Statu = 1,p_NUMBER_OF_STATU
!        call StatisticOneStatuClusters_Virtual_Way3_1(Statu,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!      END DO
!
!      return
!    end subroutine GetBoxesMigCoaleStat_Virtual_GPU
!
!    !******************************************
!    subroutine StatisticOneStatuClusters_Virtual_Way3_1(Statu,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoaleStatisticInfo_Virtual)::TheMigCoaleStatisticInfo
!        !---Local Vars---
!        integer::MultiBox
!        integer::IBox
!        integer::IBFROM
!        integer::IBTO
!        integer::NCCount
!        integer::NCCountTotal
!        integer::BlockNumEachBox
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::err
!        !---Body---
!        MultiBox = Host_SimuCtrlParam%MultiBox
!
!        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .LE. 0) then
!            return
!        end if
!
!        if(maxval(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,1) + 1) .LE. 0) then
!            return
!        end if
!        BlockNumEachBox = (maxval(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,2)-Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(:,1)))/p_Reduce_BLOCKSIZE + 1
!
!        BlockNumEachBox = (BlockNumEachBox -1)/2 + 1
!
!        NB = BlockNumEachBox*MultiBox
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .GT. size(m_SumROneStatuArray)) then
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .GT. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .GT. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .GT. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .GT. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .GT. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .GT. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .GT. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!        call Kernel_StatisticOneStatuClusters3<<<blocks,threads>>>(Statu,                                                       &
!                                                                   BlockNumEachBox,                                             &
!                                                                   Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                    &
!                                                                   Dev_Boxes%dm_SEVirtualIndexBox,                              &
!                                                                   dm_SumROneStatuArray,                                        &
!                                                                   dm_MaxROneStatuArray,                                        &
!                                                                   dm_MinROneStatuArray,                                        &
!                                                                   dm_MaxDiffOneStatuArray)
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        m_MaxDiffOneStatuArray = dm_MaxDiffOneStatuArray
!
!        NCCountTotal = 0
!
!        DO IBox = 1,MultiBox
!            IBFROM = (IBox -1)*BlockNumEachBox + 1
!            IBTO = IBox*BlockNumEachBox
!            if(Statu .eq. p_ACTIVEFREE_STATU .or. Statu .eq. p_ACTIVEINGB_STATU) then
!                if(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu) .GT. 0) then
!
!                    NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)  &
!                            + Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
!                else
!                    NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)
!                end if
!            else
!                NCCount = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(Statu)
!            end if
!
!            if(NCCount .GT. 0) then
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(Statu) = sum(m_SumROneStatuArray(IBFROM:IBTO))/NCCount
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(Statu) = maxval(m_MaxROneStatuArray(IBFROM:IBTO))
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(Statu) = minval(m_MinROneStatuArray(IBFROM:IBTO))
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(Statu) = maxval(m_MaxDiffOneStatuArray(IBFROM:IBTO))
!            else
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RAVA(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMAX(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%RMIN(Statu) = 0.D0
!                TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox)%DiffusorValueMax(Statu) = 0.D0
!            end if
!
!            NCCountTotal = NCCountTotal + NCCount
!
!        END DO
!
!        if(NCCountTotal .GT. 0) then
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(Statu) = sum(m_SumROneStatuArray(1:NB))/NCCountTotal
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(Statu) = maxval(m_MaxROneStatuArray(1:NB))
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(Statu) = minval(m_MinROneStatuArray(1:NB))
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(Statu) = maxval(m_MaxDiffOneStatuArray(1:NB))
!        else
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RAVA(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMAX(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%RMIN(Statu) = 0.D0
!            TheMigCoaleStatisticInfo%statistic_IntegralBox%DiffusorValueMax(Statu) = 0.D0
!        end if
!
!        return
!    end subroutine StatisticOneStatuClusters_Virtual_Way3_1

!    !*******************************************
!    attributes(global) subroutine Kernel_StatisticOneStatuClusters3(Statu,BlockNumEachBox,DevArray,Dev_SEIndexBox,       &
!                                                                    ResultSumRadiusOneStatuArray,                        &
!                                                                    ResultMaxRadiusOneStatuArray,                        &
!                                                                    ResultMinRadiusOneStatuArray,                        &
!                                                                    ResultMaxDiffOneStatuArray)
!        !use libm
!        implicit none
!        !---Dummy Vars---
!        integer,value::Statu
!        integer,value::BlockNumEachBox
!        type(ACluster),device::DevArray(:)
!        integer,device::Dev_SEIndexBox(:,:)
!        real(kind=KINDDF),device::ResultSumRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMaxRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMinRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMaxDiffOneStatuArray(:)
!        !---Local Vars---
!        integer::tid
!        integer::bid
!        integer::cid
!        integer::bid0
!        integer::IBox
!        integer::scid
!        integer::ecid
!        integer::IC
!        real(kind=KINDDF),shared::Share_SumRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MinRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxDiffOneStatu(p_Reduce_BLOCKSIZE)
!        integer::I
!        real(kind=KINDDF)::tempRadius
!        real(kind=KINDDF)::tempDIF
!        !---Body---
!        tid = (threadidx%y - 1)*blockdim%x + threadidx%x
!        bid = (blockidx%y  - 1)*griddim%x  + blockidx%x
!        cid = (bid -1)*p_Reduce_BLOCKSIZE + tid
!
!        IBox = (bid - 1)/BlockNumEachBox + 1
!
!        bid0 = (IBox - 1)*BlockNumEachBox + 1
!
!        scid = Dev_SEIndexBox(IBox,1)
!
!        ecid = Dev_SEIndexBox(IBox,2)
!
!        IC = scid + (bid - bid0)*p_Reduce_BLOCKSIZE*2 + tid - 1
!
!        Share_SumRadiusOneStatu(tid) = 0.D0
!        Share_MaxRadiusOneStatu(tid) = -1.D32
!        Share_MinRadiusOneStatu(tid) = 1.D32
!        Share_MaxDiffOneStatu(tid) = -1.D32
!
!        if(IC .LE. ecid) then
!            if(DevArray(IC)%m_Statu .eq. Statu) then
!
!                Share_SumRadiusOneStatu(tid) = DevArray(IC)%m_RAD
!                Share_MaxRadiusOneStatu(tid) = DevArray(IC)%m_RAD
!                Share_MinRadiusOneStatu(tid) = DevArray(IC)%m_RAD
!                Share_MaxDiffOneStatu(tid) = DevArray(IC)%m_DiffCoeff
!            end if
!
!        end if
!
!        if((IC + p_Reduce_BLOCKSIZE) .LE. ecid) then
!
!            if(DevArray(IC + p_Reduce_BLOCKSIZE)%m_Statu .eq. Statu) then
!
!                tempRadius = DevArray(IC + p_Reduce_BLOCKSIZE)%m_RAD
!
!                tempDIF = DevArray(IC + p_Reduce_BLOCKSIZE)%m_DiffCoeff
!
!                Share_SumRadiusOneStatu(tid) = Share_SumRadiusOneStatu(tid) + tempRadius
!
!                if(Share_MaxRadiusOneStatu(tid) .LT. tempRadius) then
!                    Share_MaxRadiusOneStatu(tid) = tempRadius
!                end if
!
!                if(Share_MinRadiusOneStatu(tid) .GT. tempRadius) then
!                    Share_MinRadiusOneStatu(tid) = tempRadius
!                end if
!
!                if(Share_MaxDiffOneStatu(tid) .LT. tempDIF) then
!                    Share_MaxDiffOneStatu(tid) = tempDIF
!                end if
!
!            end if
!
!        end if
!
!        call syncthreads()
!
!        I = p_Reduce_BLOCKSIZE/2
!        DO While(I .GT. 0)
!
!            if(tid .LE. I) then
!
!                Share_SumRadiusOneStatu(tid) = Share_SumRadiusOneStatu(tid) + Share_SumRadiusOneStatu(tid+I)
!
!                if(Share_MaxRadiusOneStatu(tid) .LT. Share_MaxRadiusOneStatu(tid+I)) then
!                    Share_MaxRadiusOneStatu(tid) = Share_MaxRadiusOneStatu(tid+I)
!                end if
!
!                if(Share_MinRadiusOneStatu(tid) .GT. Share_MinRadiusOneStatu(tid+I)) then
!                    Share_MinRadiusOneStatu(tid) = Share_MinRadiusOneStatu(tid+I)
!                end if
!
!                if(Share_MaxDiffOneStatu(tid) .LT. Share_MaxDiffOneStatu(tid+I)) then
!                    Share_MaxDiffOneStatu(tid) = Share_MaxDiffOneStatu(tid+I)
!                end if
!            end if
!
!            call syncthreads()
!
!            I = I/2
!        END DO
!
!
!        if(tid .eq. 1) then
!            ResultSumRadiusOneStatuArray(bid) = Share_SumRadiusOneStatu(1)
!            ResultMaxRadiusOneStatuArray(bid) = Share_MaxRadiusOneStatu(1)
!            ResultMinRadiusOneStatuArray(bid) = Share_MinRadiusOneStatu(1)
!            ResultMaxDiffOneStatuArray(bid) = Share_MaxDiffOneStatu(1)
!        end if
!
!        return
!    end subroutine Kernel_StatisticOneStatuClusters3


    !*******************************************************************

!    attributes(global) subroutine Kernel_StatisticOneStatuClusters3(Statu,N,DevArray,Dev_TypesMap,Dev_SingleAtomsDivideArrays,&
!                                                                      ResultSumRadiusOneStatuArray,&
!                                                                      ResultMaxRadiusOneStatuArray,&
!                                                                      ResultMinRadiusOneStatuArray,&
!                                                                      ResultMaxDiffOneStatuArray)
!        !use libm
!        implicit none
!        !---Dummy Vars---
!        integer,value::Statu
!        integer,value::N
!        type(ACluster),device::DevArray(N)
!        type(DiffusorTypeEntity),device::Dev_TypesMap(:)
!        integer,device::Dev_SingleAtomsDivideArrays(:,:)
!        real(kind=KINDDF),device::ResultSumRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMaxRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMinRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMaxDiffOneStatuArray(:)
!        !---Local Vars---
!        integer::IT,IB,IC
!        real(kind=KINDDF),shared::Share_SumRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MinRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxDiffOneStatu(p_Reduce_BLOCKSIZE)
!        integer::I
!        real(kind=KINDDF)::tempRadius
!        real(kind=KINDDF)::tempDIF
!        type(DiffusorValue)::TheDiffusorValue
!        integer::IATOMGROUP
!        integer::tempNA
!        !---Body---
!        IT = (threadidx%y - 1)*blockdim%x + threadidx%x
!        IB = (blockidx%y - 1)*griddim%x + blockidx%x
!        IC = (IB - 1)*(blockdim%x*blockdim%y)*2 + IT
!
!        Share_SumRadiusOneStatu(IT) = 0.D0
!        Share_MaxRadiusOneStatu(IT) = -1.D32
!        Share_MinRadiusOneStatu(IT) = 1.D32
!        Share_MaxDiffOneStatu(IT) = -1.D32
!
!        if(IC .LE. N) then
!            if(DevArray(IC)%m_Statu .eq. Statu) then
!
!                tempNA = 0
!
!                call Dev_GetValueFromDiffusorsMap(DevArray(IC),Dev_TypesMap,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!                select case(TheDiffusorValue%ECRValueType)
!                    case(p_ECR_ByValue)
!                        tempRadius = TheDiffusorValue%ECR
!                    case(p_ECR_ByBCluster)
!                        DO IATOMGROUP = 1, p_ATOMS_GROUPS_NUMBER
!                            tempNA = tempNA + DevArray(IC)%m_Atoms(IATOMGROUP)%m_NA
!                        END DO
!                        tempRadius = SQRT(tempNA/dm_RNFACTOR)
!                end select
!
!                select case(TheDiffusorValue%DiffusorValueType)
!                    case(p_DiffuseCoefficient_ByValue)
!                        tempDIF = TheDiffusorValue%DiffuseCoefficient_Value
!                    case(p_DiffuseCoefficient_ByArrhenius)
!                        tempDIF = TheDiffusorValue%PreFactor*exp(-CP_EVERG*TheDiffusorValue%ActEnergy/dm_TKB)
!                    case(p_DiffuseCoefficient_ByBCluster)
!                        ! Here we adopt a model that D=D0*(1/R)**Gama
!                        tempDIF = dm_SURDIFPRE*(tempRadius**(-p_GAMMA))
!                end select
!
!                Share_SumRadiusOneStatu(IT) = tempRadius
!                Share_MaxRadiusOneStatu(IT) = tempRadius
!                Share_MinRadiusOneStatu(IT) = tempRadius
!                Share_MaxDiffOneStatu(IT) = tempDIF
!            end if
!
!        end if
!
!        if((IC + p_Reduce_BLOCKSIZE) .LE. N) then
!
!            if(DevArray(IC + p_Reduce_BLOCKSIZE)%m_Statu .eq. Statu) then
!
!                tempNA = 0
!
!                call Dev_GetValueFromDiffusorsMap(DevArray(IC + p_Reduce_BLOCKSIZE),Dev_TypesMap,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!                select case(TheDiffusorValue%ECRValueType)
!                    case(p_ECR_ByValue)
!                        tempRadius = TheDiffusorValue%ECR
!                    case(p_ECR_ByBCluster)
!                        DO IATOMGROUP = 1, p_ATOMS_GROUPS_NUMBER
!                            tempNA = tempNA + DevArray(IC + p_Reduce_BLOCKSIZE)%m_Atoms(IATOMGROUP)%m_NA
!                        END DO
!                        tempRadius = SQRT(tempNA/dm_RNFACTOR)
!                end select
!
!                select case(TheDiffusorValue%DiffusorValueType)
!                    case(p_DiffuseCoefficient_ByValue)
!                        tempDIF = TheDiffusorValue%DiffuseCoefficient_Value
!                    case(p_DiffuseCoefficient_ByArrhenius)
!                        tempDIF = TheDiffusorValue%PreFactor*exp(-CP_EVERG*TheDiffusorValue%ActEnergy/dm_TKB)
!                    case(p_DiffuseCoefficient_ByBCluster)
!                        ! Here we adopt a model that D=D0*(1/R)**Gama
!                        tempDIF = dm_SURDIFPRE*(tempRadius**(-p_GAMMA))
!                end select
!
!                Share_SumRadiusOneStatu(IT) = Share_SumRadiusOneStatu(IT) + tempRadius
!
!                if(Share_MaxRadiusOneStatu(IT) .LT. tempRadius) then
!                    Share_MaxRadiusOneStatu(IT) = tempRadius
!                end if
!
!                if(Share_MinRadiusOneStatu(IT) .GT. tempRadius) then
!                    Share_MinRadiusOneStatu(IT) = tempRadius
!                end if
!
!                if(Share_MaxDiffOneStatu(IT) .LT. tempDIF) then
!                    Share_MaxDiffOneStatu(IT) = tempDIF
!                end if
!
!            end if
!
!        end if
!
!        call syncthreads()
!
!        I = p_Reduce_BLOCKSIZE/2
!        DO While(I .GT. 0)
!
!            if(IT .LE. I) then
!
!                Share_SumRadiusOneStatu(IT) = Share_SumRadiusOneStatu(IT) + Share_SumRadiusOneStatu(IT+I)
!
!                if(Share_MaxRadiusOneStatu(IT) .LT. Share_MaxRadiusOneStatu(IT+I)) then
!                    Share_MaxRadiusOneStatu(IT) = Share_MaxRadiusOneStatu(IT+I)
!                end if
!
!                if(Share_MinRadiusOneStatu(IT) .GT. Share_MinRadiusOneStatu(IT+I)) then
!                    Share_MinRadiusOneStatu(IT) = Share_MinRadiusOneStatu(IT+I)
!                end if
!
!                if(Share_MaxDiffOneStatu(IT) .LT. Share_MaxDiffOneStatu(IT+I)) then
!                    Share_MaxDiffOneStatu(IT) = Share_MaxDiffOneStatu(IT+I)
!                end if
!            end if
!
!            call syncthreads()
!
!            I = I/2
!        END DO
!
!
!        if(IT .eq. 1) then
!            ResultSumRadiusOneStatuArray(IB) = Share_SumRadiusOneStatu(1)
!            ResultMaxRadiusOneStatuArray(IB) = Share_MaxRadiusOneStatu(1)
!            ResultMinRadiusOneStatuArray(IB) = Share_MinRadiusOneStatu(1)
!            ResultMaxDiffOneStatuArray(IB) = Share_MaxDiffOneStatu(1)
!        end if
!
!        return
!    end subroutine Kernel_StatisticOneStatuClusters3
!
!        !******************************************
!    subroutine StatisticOneStatuClusters_Way3(Statu,N0,DevArray,Dev_TypesMap,Dev_SingleAtomsDivideArrays,SumROneStatu,MaxROneStatu,MinROneStatu,MaxDiffOneStatu)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        integer,intent(in)::N0
!        type(ACluster),device::DevArray(:)
!        type(DiffusorTypeEntity),device::Dev_TypesMap(:)
!        integer,device::Dev_SingleAtomsDivideArrays(:,:)
!        real(kind=KINDDF)::SumROneStatu
!        real(kind=KINDDF)::MaxROneStatu
!        real(kind=KINDDF)::MinROneStatu
!        real(kind=KINDDF)::MaxDiffOneStatu
!        !---Local Vars---
!        integer::N
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::ITYPE
!        !---Body---
!        N = N0
!        NB = ((N - 1)/p_Reduce_BLOCKSIZE)/2 + 1
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .ne. size(m_SumROneStatuArray)) then
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .ne. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .ne. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .ne. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .ne. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!        call Kernel_StatisticOneStatuClusters3<<<blocks,threads>>>(Statu,                       &
!                                                                   N,                           &
!                                                                   DevArray,                    &
!                                                                   Dev_TypesMap,                &
!                                                                   Dev_SingleAtomsDivideArrays, &
!                                                                   dm_SumROneStatuArray,        &
!                                                                   dm_MaxROneStatuArray,        &
!                                                                   dm_MinROneStatuArray,        &
!                                                                   dm_MaxDiffOneStatuArray)
!
!        DO While(N .GT. 1)
!            N = NB
!            NB = ((N - 1)/p_Reduce_BLOCKSIZE)/2 + 1
!            BX = p_Reduce_BLOCKSIZE
!            BY = 1
!            blocks = dim3(NB,1,1)
!            threads = dim3(BX,BY,1)
!            call Kernel_StatisticOneStatuValue3<<<blocks,threads>>>(N,              &
!                                                            dm_SumROneStatuArray,   &
!                                                            dm_MaxROneStatuArray,   &
!                                                            dm_MinROneStatuArray,   &
!                                                            dm_MaxDiffOneStatuArray)
!        END DO
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        SumROneStatu = m_SumROneStatuArray(1)
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        MaxROneStatu = m_MaxROneStatuArray(1)
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        MinROneStatu = m_MinROneStatuArray(1)
!        m_MaxDiffOneStatuArray = dm_MaxDiffOneStatuArray
!        MaxDiffOneStatu = m_MaxDiffOneStatuArray(1)
!
!        return
!    end subroutine StatisticOneStatuClusters_Way3
!
!    !******************************************
!    subroutine StatisticOneStatuClusters_Way3_1(Statu,N0,DevArray,Dev_TypesMap,Dev_SingleAtomsDivideArrays,SumROneStatu,MaxROneStatu,MinROneStatu,MaxDiffOneStatu)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        integer,intent(in)::N0
!        type(ACluster),device::DevArray(:)
!        type(DiffusorTypeEntity),device::Dev_TypesMap(:)
!        integer,device::Dev_SingleAtomsDivideArrays(:,:)
!        real(kind=KINDDF)::SumROneStatu
!        real(kind=KINDDF)::MaxROneStatu
!        real(kind=KINDDF)::MinROneStatu
!        real(kind=KINDDF)::MaxDiffOneStatu
!        !---Local Vars---
!        integer::N
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::err
!        !---Body---
!        N = N0
!        NB = ((N - 1)/p_Reduce_BLOCKSIZE)/2 + 1
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .ne. size(m_SumROneStatuArray)) then
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .ne. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .ne. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .ne. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .ne. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!        call Kernel_StatisticOneStatuClusters3<<<blocks,threads>>>(Statu,                       &
!                                                                   N,                           &
!                                                                   DevArray,                    &
!                                                                   Dev_TypesMap,                &
!                                                                   Dev_SingleAtomsDivideArrays, &
!                                                                   dm_SumROneStatuArray,        &
!                                                                   dm_MaxROneStatuArray,        &
!                                                                   dm_MinROneStatuArray,        &
!                                                                   dm_MaxDiffOneStatuArray)
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        m_MaxDiffOneStatuArray = dm_MaxDiffOneStatuArray
!
!        SumROneStatu = sum(m_SumROneStatuArray)
!        MaxROneStatu = maxval(m_MaxROneStatuArray)
!        MinROneStatu = minval(m_MinROneStatuArray)
!        MaxDiffOneStatu = maxval(m_MaxDiffOneStatuArray)
!
!        return
!    end subroutine StatisticOneStatuClusters_Way3_1


!!******************************************
!    subroutine StatisticOneStatuClusters_Way2(Statu,N0,DevArray,Dev_TypesMap,Dev_SingleAtomsDivideArrays,SumROneStatu,MaxROneStatu,MinROneStatu,MaxDiffOneStatu)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        integer,intent(in)::N0
!        type(ACluster),device::DevArray(:)
!        type(DiffusorTypeEntity),device::Dev_TypesMap(:)
!        integer,device::Dev_SingleAtomsDivideArrays(:,:)
!        real(kind=KINDDF)::SumROneStatu
!        real(kind=KINDDF)::MaxROneStatu
!        real(kind=KINDDF)::MinROneStatu
!        real(kind=KINDDF)::MaxDiffOneStatu
!        !---Local Vars---
!        integer::N
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        !---Body---
!        N = N0
!        NB = (N - 1)/p_Reduce_BLOCKSIZE + 1
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .ne. size(m_SumROneStatuArray)) then
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .ne. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .ne. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .ne. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .ne. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!
!        call Kernel_StatisticOneStatuClusters2<<<blocks,threads>>>(Statu,                       &
!                                                                   N,                           &
!                                                                   DevArray,                    &
!                                                                   Dev_TypesMap,                &
!                                                                   Dev_SingleAtomsDivideArrays, &
!                                                                   dm_SumROneStatuArray,        &
!                                                                   dm_MaxROneStatuArray,        &
!                                                                   dm_MinROneStatuArray,        &
!                                                                   dm_MaxDiffOneStatuArray)
!        DO While(N .GT. 1)
!            N = NB
!            NB = (N - 1)/p_Reduce_BLOCKSIZE + 1
!            BX = p_Reduce_BLOCKSIZE
!            BY = 1
!            blocks = dim3(NB,1,1)
!            threads = dim3(BX,BY,1)
!            call Kernel_StatisticOneStatuValue2<<<blocks,threads>>>(N,              &
!                                                            dm_SumROneStatuArray,   &
!                                                            dm_MaxROneStatuArray,   &
!                                                            dm_MinROneStatuArray,   &
!                                                            dm_MaxDiffOneStatuArray)
!        END DO
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        SumROneStatu = m_SumROneStatuArray(1)
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        MaxROneStatu = m_MaxROneStatuArray(1)
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        MinROneStatu = m_MinROneStatuArray(1)
!        m_MaxDiffOneStatuArray = dm_MaxDiffOneStatuArray
!        MaxDiffOneStatu = m_MaxDiffOneStatuArray(1)
!        return
!    end subroutine StatisticOneStatuClusters_Way2
!
!    !******************************************
!    subroutine StatisticOneStatuClusters_Way2_1(Statu,N0,DevArray,Dev_TypesMap,Dev_SingleAtomsDivideArrays,SumROneStatu,MaxROneStatu,MinROneStatu,MaxDiffOneStatu)
!        implicit none
!        !---Dummy Vars---
!        integer,intent(in)::Statu
!        integer,intent(in)::N0
!        type(ACluster),device::DevArray(:)
!        type(DiffusorTypeEntity),device::Dev_TypesMap(:)
!        integer,device::Dev_SingleAtomsDivideArrays(:,:)
!        real(kind=KINDDF)::SumROneStatu
!        real(kind=KINDDF)::MaxROneStatu
!        real(kind=KINDDF)::MinROneStatu
!        real(kind=KINDDF)::MaxDiffOneStatu
!        !---Local Vars---
!        integer::N
!        integer::NB
!        integer::BX,BY
!        type(dim3)::blocks
!        type(dim3)::threads
!        !---Body---
!        N = N0
!        NB = (N - 1)/p_Reduce_BLOCKSIZE + 1
!        BX = p_Reduce_BLOCKSIZE
!        BY = 1
!        blocks = dim3(NB,1,1)
!        threads = dim3(BX,BY,1)
!
!        if(.not. allocated(m_SumROneStatuArray)) then
!            allocate(m_SumROneStatuArray(NB))
!        else if(NB .ne. size(m_SumROneStatuArray)) then
!            deallocate(m_SumROneStatuArray)
!            allocate(m_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_SumROneStatuArray)) then
!            allocate(dm_SumROneStatuArray(NB))
!        else if(NB .ne. size(dm_SumROneStatuArray)) then
!            deallocate(dm_SumROneStatuArray)
!            allocate(dm_SumROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxROneStatuArray)) then
!            allocate(m_MaxROneStatuArray(NB))
!        else if(NB .ne. size(m_MaxROneStatuArray)) then
!            deallocate(m_MaxROneStatuArray)
!            allocate(m_MaxROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxROneStatuArray)) then
!            allocate(dm_MaxROneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxROneStatuArray)) then
!            deallocate(dm_MaxROneStatuArray)
!            allocate(dm_MaxROneStatuArray(NB))
!        end if
!
!
!        if(.not. allocated(m_MinROneStatuArray)) then
!            allocate(m_MinROneStatuArray(NB))
!        else if(NB .ne. size(m_MinROneStatuArray)) then
!            deallocate(m_MinROneStatuArray)
!            allocate(m_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MinROneStatuArray)) then
!            allocate(dm_MinROneStatuArray(NB))
!        else if(NB .ne. size(dm_MinROneStatuArray)) then
!            deallocate(dm_MinROneStatuArray)
!            allocate(dm_MinROneStatuArray(NB))
!        end if
!
!        if(.not. allocated(m_MaxDiffOneStatuArray)) then
!            allocate(m_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(m_MaxDiffOneStatuArray)) then
!            deallocate(m_MaxDiffOneStatuArray)
!            allocate(m_MaxDiffOneStatuArray(NB))
!        end if
!
!        if(.not. allocated(dm_MaxDiffOneStatuArray)) then
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        else if(NB .ne. size(dm_MaxDiffOneStatuArray)) then
!            deallocate(dm_MaxDiffOneStatuArray)
!            allocate(dm_MaxDiffOneStatuArray(NB))
!        end if
!
!
!        call Kernel_StatisticOneStatuClusters2<<<blocks,threads>>>(Statu,                       &
!                                                                   N,                           &
!                                                                   DevArray,                    &
!                                                                   Dev_TypesMap,                &
!                                                                   Dev_SingleAtomsDivideArrays, &
!                                                                   dm_SumROneStatuArray,        &
!                                                                   dm_MaxROneStatuArray,        &
!                                                                   dm_MinROneStatuArray,        &
!                                                                   dm_MaxDiffOneStatuArray)
!
!        m_SumROneStatuArray = dm_SumROneStatuArray
!        m_MaxROneStatuArray = dm_MaxROneStatuArray
!        m_MinROneStatuArray = dm_MinROneStatuArray
!        m_MaxDiffOneStatuArray = dm_MaxDiffOneStatuArray
!
!        SumROneStatu = sum(m_SumROneStatuArray)
!        MaxROneStatu = maxval(m_MaxROneStatuArray)
!        MinROneStatu = minval(m_MinROneStatuArray)
!        MaxDiffOneStatu = maxval(m_MaxDiffOneStatuArray)
!        return
!    end subroutine StatisticOneStatuClusters_Way2_1
!
!
!    !*******************************************
!    attributes(global) subroutine Kernel_StatisticOneStatuClusters2(Statu,N,DevArray,Dev_TypesMap,Dev_SingleAtomsDivideArrays,&
!                                                                    ResultSumRadiusOneStatuArray,&
!                                                                    ResultMaxRadiusOneStatuArray,&
!                                                                    ResultMinRadiusOneStatuArray,&
!                                                                    ResultMaxDiffOneStatuArray)
!        !use libm
!        implicit none
!        !---Dummy Vars---
!        integer,value::Statu
!        integer,value::N
!        type(ACluster),device::DevArray(N)
!        type(DiffusorTypeEntity),device::Dev_TypesMap(:)
!        integer,device::Dev_SingleAtomsDivideArrays(:,:)
!        real(kind=KINDDF),device::ResultSumRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMaxRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMinRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::ResultMaxDiffOneStatuArray(:)
!        !---Local Vars---
!        integer::IT,IB,IC
!        real(kind=KINDDF),shared::Share_SumRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MinRadiusOneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxDiffOneStatu(p_Reduce_BLOCKSIZE)
!        integer::I
!        real(kind=KINDDF)::tempRadius
!        real(kind=KINDDF)::tempDIF
!        integer::IAtomsGroup
!        type(DiffusorValue)::TheDiffusorValue
!        integer::tempNA
!        !---Body---
!        IT = (threadidx%y - 1)*blockdim%x + threadidx%x
!        IB = (blockidx%y - 1)*griddim%x + blockidx%x
!        IC = (IB - 1)*(blockdim%x*blockdim%y)*2 + IT
!
!        Share_SumRadiusOneStatu(IT) = 0.D0
!        Share_MaxRadiusOneStatu(IT) = -1.D32
!        Share_MinRadiusOneStatu(IT) = 1.D32
!        Share_MaxDiffOneStatu(IT) = -1.D32
!
!        if(IC .LE. N) then
!            if(DevArray(IC)%m_Statu .eq. Statu) then
!                tempNA = 0
!
!                call Dev_GetValueFromDiffusorsMap(DevArray(IC),Dev_TypesMap,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!                select case(TheDiffusorValue%ECRValueType)
!                    case(p_ECR_ByValue)
!                        tempRadius = TheDiffusorValue%ECR
!                    case(p_ECR_ByBCluster)
!                        DO IAtomsGroup = 1, p_ATOMS_GROUPS_NUMBER
!                            tempNA = tempNA + DevArray(IC)%m_Atoms(IAtomsGroup)%m_NA
!                        END DO
!                        tempRadius = SQRT(tempNA/dm_RNFACTOR)
!                end select
!
!                select case(TheDiffusorValue%DiffusorValueType)
!                    case(p_DiffuseCoefficient_ByValue)
!                        tempDIF = TheDiffusorValue%DiffuseCoefficient_Value
!                    case(p_DiffuseCoefficient_ByArrhenius)
!                        tempDIF = TheDiffusorValue%PreFactor*exp(-CP_EVERG*TheDiffusorValue%ActEnergy/dm_TKB)
!                    case(p_DiffuseCoefficient_ByBCluster)
!                        ! Here we adopt a model that D=D0*(1/R)**Gama
!                        tempDIF = dm_SURDIFPRE*(tempRadius**(-p_GAMMA))
!                end select
!
!                Share_SumRadiusOneStatu(IT) = tempRadius
!                Share_MaxRadiusOneStatu(IT) = tempRadius
!                Share_MinRadiusOneStatu(IT) = tempRadius
!                Share_MaxDiffOneStatu(IT) = tempDIF
!            end if
!
!        end if
!
!        call syncthreads()
!
!        I = p_Reduce_BLOCKSIZE/2
!        DO While(I .GT. 0)
!
!            if(IT .LE. I) then
!
!                Share_SumRadiusOneStatu(IT) = Share_SumRadiusOneStatu(IT) + Share_SumRadiusOneStatu(IT+I)
!
!                if(Share_MaxRadiusOneStatu(IT) .LT. Share_MaxRadiusOneStatu(IT+I)) then
!                    Share_MaxRadiusOneStatu(IT) = Share_MaxRadiusOneStatu(IT+I)
!                end if
!
!                if(Share_MinRadiusOneStatu(IT) .GT. Share_MinRadiusOneStatu(IT+I)) then
!                    Share_MinRadiusOneStatu(IT) = Share_MinRadiusOneStatu(IT+I)
!                end if
!
!                if(Share_MaxDiffOneStatu(IT) .LT. Share_MaxDiffOneStatu(IT+I)) then
!                    Share_MaxDiffOneStatu(IT) = Share_MaxDiffOneStatu(IT+I)
!                end if
!
!            end if
!
!            call syncthreads()
!
!            I = I/2
!        END DO
!
!        if(IT .eq. 1) then
!            ResultSumRadiusOneStatuArray(IB) = Share_SumRadiusOneStatu(1)
!            ResultMaxRadiusOneStatuArray(IB) = Share_MaxRadiusOneStatu(1)
!            ResultMinRadiusOneStatuArray(IB) = Share_MinRadiusOneStatu(1)
!            ResultMaxDiffOneStatuArray(IB) = Share_MaxDiffOneStatu(1)
!        end if
!
!        return
!    end subroutine Kernel_StatisticOneStatuClusters2
!
!    !*******************************************
!    attributes(global) subroutine Kernel_StatisticOneStatuValue2(N,                   &
!                                                               SumRadiusOneStatuArray,&
!                                                               MaxRadiusOneStatuArray,&
!                                                               MinRadiusOneStatuArray,&
!                                                               MaxDiffOneStatuArray)
!        !use libm
!        implicit none
!        !---Dummy Vars---
!        integer,value::N
!        real(kind=KINDDF),device::SumRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::MaxRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::MinRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::MaxDiffOneStatuArray(:)
!        !---Local Vars---
!        integer::IT,IB,IC
!        real(kind=KINDDF),shared::Share_SumROneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxROneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MinROneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxDiffOneStatu(p_Reduce_BLOCKSIZE)
!        integer::I
!        !---Body---
!        IT = (threadidx%y - 1)*blockdim%x + threadidx%x
!        IB = (blockidx%y - 1)*griddim%x + blockidx%x
!        IC = (IB - 1)*(blockdim%x*blockdim%y) + IT
!
!        Share_SumROneStatu(IT) = 0
!        Share_MaxROneStatu(IT) = -1.D32
!        Share_MinROneStatu(IT) = 1.D32
!        Share_MaxDiffOneStatu(IT) = -1.D32
!
!        if(IC .LE. N) then
!            Share_SumROneStatu(IT) = SumRadiusOneStatuArray(IC)
!            Share_MaxROneStatu(IT) = MaxRadiusOneStatuArray(IC)
!            Share_MinROneStatu(IT) = MinRadiusOneStatuArray(IC)
!            Share_MaxDiffOneStatu(IT) = MaxDiffOneStatuArray(IC)
!        end if
!
!        call syncthreads()
!
!        I = p_Reduce_BLOCKSIZE/2
!        DO While(I .GT. 0)
!
!            if(IT .LE. I) then
!
!                Share_SumROneStatu(IT) = Share_SumROneStatu(IT) + Share_SumROneStatu(IT+I)
!
!                if(Share_MaxROneStatu(IT) .LT. Share_MaxROneStatu(IT+I)) then
!                    Share_MaxROneStatu(IT) = Share_MaxROneStatu(IT+I)
!                end if
!
!                if(Share_MinROneStatu(IT) .GT. Share_MinROneStatu(IT+I)) then
!                    Share_MinROneStatu(IT) = Share_MinROneStatu(IT+I)
!                end if
!
!                if(Share_MaxDiffOneStatu(IT) .LT. Share_MaxDiffOneStatu(IT+I)) then
!                    Share_MaxDiffOneStatu(IT) = Share_MaxDiffOneStatu(IT+I)
!                end if
!
!            end if
!
!            call syncthreads()
!
!            I = I/2
!        END DO
!
!
!        if(IT .eq. 1) then
!            SumRadiusOneStatuArray(IB) = Share_SumROneStatu(1)
!            MaxRadiusOneStatuArray(IB) = Share_MaxROneStatu(1)
!            MinRadiusOneStatuArray(IB) = Share_MinROneStatu(1)
!            MaxDiffOneStatuArray(IB) = Share_MaxDiffOneStatu(1)
!        end if
!
!        return
!    end subroutine Kernel_StatisticOneStatuValue2
!
!    !*******************************************
!    attributes(global) subroutine Kernel_StatisticOneStatuValue3(N,                   &
!                                                               SumRadiusOneStatuArray,&
!                                                               MaxRadiusOneStatuArray,&
!                                                               MinRadiusOneStatuArray,&
!                                                               MaxDiffOneStatuArray)
!        !use libm
!        implicit none
!        !---Dummy Vars---
!        integer,value::N
!        real(kind=KINDDF),device::SumRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::MaxRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::MinRadiusOneStatuArray(:)
!        real(kind=KINDDF),device::MaxDiffOneStatuArray(:)
!        !---Local Vars---
!        integer::IT,IB,IC
!        real(kind=KINDDF),shared::Share_SumROneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxROneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MinROneStatu(p_Reduce_BLOCKSIZE)
!        real(kind=KINDDF),shared::Share_MaxDiffOneStatu(p_Reduce_BLOCKSIZE)
!        integer::I
!        !---Body---
!        IT = (threadidx%y - 1)*blockdim%x + threadidx%x
!        IB = (blockidx%y - 1)*griddim%x + blockidx%x
!        IC = (IB - 1)*(blockdim%x*blockdim%y)*2 + IT
!
!        Share_SumROneStatu(IT) = 0
!        Share_MaxROneStatu(IT) = -1.D32
!        Share_MinROneStatu(IT) = 1.D32
!        Share_MaxDiffOneStatu(IT) = -1.D32
!
!        if(IC .LE. N) then
!            Share_SumROneStatu(IT) = SumRadiusOneStatuArray(IC)
!            Share_MaxROneStatu(IT) = MaxRadiusOneStatuArray(IC)
!            Share_MinROneStatu(IT) = MinRadiusOneStatuArray(IC)
!            Share_MaxDiffOneStatu(IT) = MaxDiffOneStatuArray(IC)
!        end if
!
!        if((IC + p_Reduce_BLOCKSIZE) .LE. N) then
!
!            Share_SumROneStatu(IT) = Share_SumROneStatu(IT) + SumRadiusOneStatuArray(IC + p_Reduce_BLOCKSIZE)
!
!            if(Share_MaxROneStatu(IT) .LT. MaxRadiusOneStatuArray(IC + p_Reduce_BLOCKSIZE)) then
!                Share_MaxROneStatu(IT) = MaxRadiusOneStatuArray(IC + p_Reduce_BLOCKSIZE)
!            end if
!
!            if(Share_MinROneStatu(IT) .GT. MinRadiusOneStatuArray(IC + p_Reduce_BLOCKSIZE)) then
!                Share_MinROneStatu(IT) = MinRadiusOneStatuArray(IC + p_Reduce_BLOCKSIZE)
!            end if
!
!            if(Share_MaxDiffOneStatu(IT) .LT. MaxDiffOneStatuArray(IC + p_Reduce_BLOCKSIZE)) then
!                Share_MaxDiffOneStatu(IT) = MaxDiffOneStatuArray(IC + p_Reduce_BLOCKSIZE)
!            end if
!        end if
!
!        call syncthreads()
!
!        I = p_Reduce_BLOCKSIZE/2
!        DO While(I .GT. 0)
!
!            if(IT .LE. I) then
!
!                Share_SumROneStatu(IT) = Share_SumROneStatu(IT) + Share_SumROneStatu(IT+I)
!
!                if(Share_MaxROneStatu(IT) .LT. Share_MaxROneStatu(IT+I)) then
!                    Share_MaxROneStatu(IT) = Share_MaxROneStatu(IT+I)
!                end if
!
!                if(Share_MinROneStatu(IT) .GT. Share_MinROneStatu(IT+I)) then
!                    Share_MinROneStatu(IT) = Share_MinROneStatu(IT+I)
!                end if
!
!                if(Share_MaxDiffOneStatu(IT) .LT. Share_MaxDiffOneStatu(IT+I)) then
!                    Share_MaxDiffOneStatu(IT) = Share_MaxDiffOneStatu(IT+I)
!                end if
!
!            end if
!
!            call syncthreads()
!
!            I = I/2
!        END DO
!
!        if(IT .eq. 1) then
!            SumRadiusOneStatuArray(IB) = Share_SumROneStatu(1)
!            MaxRadiusOneStatuArray(IB) = Share_MaxROneStatu(1)
!            MinRadiusOneStatuArray(IB) = Share_MinROneStatu(1)
!            MaxDiffOneStatuArray(IB) = Share_MaxDiffOneStatu(1)
!        end if
!
!        return
!    end subroutine Kernel_StatisticOneStatuValue3

end module MIGCOALE_STATISTIC_GPU
