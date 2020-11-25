module MCMIGCOALE_STATISTIC_CPU
  use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
  use MCMIGCOALE_TYPEDEF_STATISTICINFO
  use MCMIGCOALE_ADDONDATA_HOST
  contains

  !*************************************************************
  subroutine GetBoxesMCMigCoaleStat_Used_CPU(Host_Boxes,Host_SimuCtrlParam,TheMCMigCoaleStatisticInfo)
    !***                 Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
    !                 Host_Boxes: the boxes information in host
    implicit none
    !---Dummy Vars---
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MCMigCoaleStatisticInfo_Used)::TheMCMigCoaleStatisticInfo
    !---Local Vars---
    integer::MultiBox
    integer::IBox
    integer::IStatu
    real(kind=KINDDF)::RAVA(p_NUMBER_OF_STATU)
    integer::NActCountTemp(p_NUMBER_OF_STATU)
    integer::NCCount(p_NUMBER_OF_STATU)
    !---Body---

    call Host_Boxes%GetBoxesBasicStatistic_AllStatu_CPU(Host_SimuCtrlParam)

    NCCount = 0

    RAVA = 0.D0

    ASSOCIATE(TMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_IntegralBox)

    TMigStatInfo%RMAX = -1.D32
    TMigStatInfo%RMIN = 1.D32
    TMigStatInfo%RAVA = 0.D0
    TMigStatInfo%DiffusorValueMax = -1.D32

    MultiBox = Host_SimuCtrlParam%MultiBox

    DO IBox = 1, MultiBox

        NActCountTemp = 0

        ASSOCIATE(SMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_SingleBoxes(IBox))
            call GetOneBoxMCMigCoaleStat_Used_CPU(IBox,Host_Boxes,Host_SimuCtrlParam,SMigStatInfo,NActCountTemp)

            DO IStatu = 1, p_NUMBER_OF_STATU
                if(TMigStatInfo%RMAX(IStatu) .LT. SMigStatInfo%RMAX(IStatu)) then
                    TMigStatInfo%RMAX(IStatu) = SMigStatInfo%RMAX(IStatu)
                    TMigStatInfo%ICMAX(IStatu) = SMigStatInfo%ICMAX(IStatu)
                end if

                if(TMigStatInfo%DiffusorValueMax(IStatu) .LT. SMigStatInfo%DiffusorValueMax(IStatu)) then
                    TMigStatInfo%DiffusorValueMax(IStatu) = SMigStatInfo%DiffusorValueMax(IStatu)
                end if

                if(TMigStatInfo%RMIN(IStatu) .GT. SMigStatInfo%RMIN(IStatu) .AND. NActCountTemp(IStatu) .GT. 0) then
                    TMigStatInfo%RMIN(IStatu) = SMigStatInfo%RMIN(IStatu)
                end if

                RAVA(IStatu) = RAVA(IStatu) + SMigStatInfo%RAVA(IStatu)*NActCountTemp(IStatu)
            END DO

            NCCount = NCCount + NActCountTemp

        END ASSOCIATE

    END DO

    DO IStatu = 1, p_NUMBER_OF_STATU
      if(NCCount(IStatu) .GT. 0) then
        TMigStatInfo%RAVA(IStatu) = RAVA(IStatu)/NCCount(IStatu)
      end if

    END DO


    END ASSOCIATE

    return
  end subroutine GetBoxesMCMigCoaleStat_Used_CPU

  !**************************************************************
  subroutine GetOneBoxMCMigCoaleStat_Used_CPU(IBox, Host_Boxes,Host_SimuCtrlParam,TheMCMigCoaleStatisticOneBox,NActCount)
    !   ***   Purpose        : To get average size of clusters, maxma size, and so on, at current time and this single box
    !                    IBox: the index of simulation box
    !              Host_Boxes: the boxes information in host
    implicit none
    !---Dummy Vars---
    integer, intent(in)::IBox
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MCMigCoaleStatisticOneBox)::TheMCMigCoaleStatisticOneBox
    integer,optional::NActCount(p_NUMBER_OF_STATU)
    !---Local Vars---
    integer::IC, ICFROM, ICTO
    integer::IStatu
    integer::ActiveFlag,DisappearFlag
    integer::NActCountTemp(p_NUMBER_OF_STATU)
    !---Body---
    TheMCMigCoaleStatisticOneBox%RMAX = -1.D32
    TheMCMigCoaleStatisticOneBox%RMIN = 1.D32
    TheMCMigCoaleStatisticOneBox%RAVA  = 0.D0
    TheMCMigCoaleStatisticOneBox%DiffusorValueMax = -1.D32

    ICFROM = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
    ICTO   = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

    ActiveFlag = ICFROM
    DisappearFlag = ICTO

    if(ICTO .GT. 0) then
      DO IC = ICFROM, ICTO
        IStatu = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

        if(IStatu .eq. p_Empty) then
            cycle
        end if

        if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
            ! The index for active clusters
            Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag) = IC
            ActiveFlag = ActiveFlag + 1
        else
            ! The index for unactive clusters
            Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(DisappearFlag) = IC
            DisappearFlag = DisappearFlag - 1
        end if

        TheMCMigCoaleStatisticOneBox%RAVA(IStatu) = TheMCMigCoaleStatisticOneBox%RAVA(IStatu) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff .GT. TheMCMigCoaleStatisticOneBox%DiffusorValueMax(IStatu)) then
            TheMCMigCoaleStatisticOneBox%DiffusorValueMax(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff
        end if

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .GT. TheMCMigCoaleStatisticOneBox%RMAX(IStatu)) then
            TheMCMigCoaleStatisticOneBox%RMAX(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
            TheMCMigCoaleStatisticOneBox%ICMAX(IStatu) = IC
        end if

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .LT. TheMCMigCoaleStatisticOneBox%RMIN(IStatu)) then
            TheMCMigCoaleStatisticOneBox%RMIN(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
        end if

      END DO
    end if

    ! move the unactive clusters indexes array position and inverse direction
    Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag:ActiveFlag + ICTO - DisappearFlag - 1) = Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ICTO:DisappearFlag+1:-1)

    ! The index for empty clusters space
    !FORALL(IC=DisappearFlag:ActiveFlag:-1)
    FORALL(IC=ActiveFlag + ICTO - DisappearFlag:ICTO)
        Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(IC) = IC
    END FORALL

    ! Do some check

    if((DisappearFlag + 1 - ActiveFlag) .LT. 0) then
        write(*,*)  "MCPSCUERROR: The statistic error. The active and unactive clusters index would overlap."
        pause
        stop
    end if

    NActCountTemp = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC

    DO IStatu = 1,p_NUMBER_OF_STATU
        if(NActCountTemp(IStatu) .GT. 0) then
            TheMCMigCoaleStatisticOneBox%RAVA(IStatu) = TheMCMigCoaleStatisticOneBox%RAVA(IStatu)/NActCountTemp(IStatu)
        end if
    END DO

    if(present(NActCount)) then
        NActCount = NActCountTemp
    end if

    return
  end subroutine GetOneBoxMCMigCoaleStat_Used_CPU

  !*************************************************************
  subroutine GetBoxesMCMigCoaleStat_Expd_CPU(Host_Boxes,Host_SimuCtrlParam,TheMCMigCoaleStatisticInfo)
    !***                 Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
    !                 Host_Boxes: the boxes information in host
    implicit none
    !---Dummy Vars---
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MCMigCoaleStatisticInfo_Expd)::TheMCMigCoaleStatisticInfo
    !---Local Vars---
    integer::MultiBox
    integer::IBox
    integer::IStatu
    real(kind=KINDDF)::RAVA(p_NUMBER_OF_STATU)
    integer::NActCountTemp(p_NUMBER_OF_STATU)
    integer::NCCount(p_NUMBER_OF_STATU)
    !---Body---

    NCCount = 0

    RAVA = 0.D0

    ASSOCIATE(TMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_IntegralBox)

    TMigStatInfo%RMAX = -1.D32
    TMigStatInfo%RMIN = 1.D32
    TMigStatInfo%RAVA  = 0.D0
    TMigStatInfo%DiffusorValueMax = -1.D32

    MultiBox = Host_SimuCtrlParam%MultiBox

    DO IBox = 1, MultiBox

        NActCountTemp = 0

        ASSOCIATE(SMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_SingleBoxes(IBox))
            call GetOneBoxMCMigCoaleStat_Expd_CPU(IBox,Host_Boxes,Host_SimuCtrlParam,SMigStatInfo,NActCountTemp)

            DO IStatu = 1, p_NUMBER_OF_STATU
                if(TMigStatInfo%RMAX(IStatu) .LT. SMigStatInfo%RMAX(IStatu)) then
                    TMigStatInfo%RMAX(IStatu) = SMigStatInfo%RMAX(IStatu)
                    TMigStatInfo%ICMAX(IStatu) = SMigStatInfo%ICMAX(IStatu)
                end if

                if(TMigStatInfo%DiffusorValueMax(IStatu) .LT. SMigStatInfo%DiffusorValueMax(IStatu)) then
                    TMigStatInfo%DiffusorValueMax(IStatu) = SMigStatInfo%DiffusorValueMax(IStatu)
                end if

                if(TMigStatInfo%RMIN(IStatu) .GT. SMigStatInfo%RMIN(IStatu) .AND. NActCountTemp(IStatu) .GT. 0) then
                    TMigStatInfo%RMIN(IStatu) = SMigStatInfo%RMIN(IStatu)
                end if

                RAVA(IStatu) = RAVA(IStatu) + SMigStatInfo%RAVA(IStatu)*NActCountTemp(IStatu)
            END DO

            NCCount = NCCount + NActCountTemp

        END ASSOCIATE

    END DO

    DO IStatu = 1, p_NUMBER_OF_STATU
      if(NCCount(IStatu) .GT. 0) then
        TMigStatInfo%RAVA(IStatu) = RAVA(IStatu)/NCCount(IStatu)
      end if

    END DO

    END ASSOCIATE

    return
  end subroutine GetBoxesMCMigCoaleStat_Expd_CPU

  !**************************************************************
  subroutine GetOneBoxMCMigCoaleStat_Expd_CPU(IBox, Host_Boxes,Host_SimuCtrlParam,TheMCMigCoaleStatisticOneBox,NActCount)
    !   ***   Purpose        : To get average size of clusters, maxma size, and so on, at current time and this single box
    !                    IBox: the index of simulation box
    !              Host_Boxes: the boxes information in host

    implicit none
    !---Dummy Vars---
    integer, intent(in)::IBox
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MCMigCoaleStatisticOneBox)::TheMCMigCoaleStatisticOneBox
    integer,optional::NActCount(p_NUMBER_OF_STATU)
    !---Local Vars---
    integer::IC, ICFROM, ICTO
    integer::IStatu
    integer::ActiveFlag,DisappearFlag
    integer::NActCountTemp(p_NUMBER_OF_STATU)
    !---Body---

    TheMCMigCoaleStatisticOneBox%RMAX = -1.D32
    TheMCMigCoaleStatisticOneBox%RMIN = 1.D32
    TheMCMigCoaleStatisticOneBox%RAVA  = 0.D0
    TheMCMigCoaleStatisticOneBox%DiffusorValueMax = -1.D32

    ICFROM = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,1)
    ICTO   = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2)

    ActiveFlag = ICFROM
    DisappearFlag = ICTO

    DO IC = ICFROM, ICTO
        IStatu = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

        if(IStatu .eq. p_Empty) then
            cycle
        end if

        if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
            ! The index for active clusters
            Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag) = IC
            ActiveFlag = ActiveFlag + 1
        else
            ! The index for unactive clusters
            Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(DisappearFlag) = IC
            DisappearFlag = DisappearFlag - 1
        end if

        TheMCMigCoaleStatisticOneBox%RAVA(IStatu) = TheMCMigCoaleStatisticOneBox%RAVA(IStatu) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff .GT. TheMCMigCoaleStatisticOneBox%DiffusorValueMax(IStatu)) then
            TheMCMigCoaleStatisticOneBox%DiffusorValueMax(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff
        end if

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .GT. TheMCMigCoaleStatisticOneBox%RMAX(IStatu)) then
            TheMCMigCoaleStatisticOneBox%RMAX(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
            TheMCMigCoaleStatisticOneBox%ICMAX(IStatu) = IC
        end if

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .LT. TheMCMigCoaleStatisticOneBox%RMIN(IStatu)) then
            TheMCMigCoaleStatisticOneBox%RMIN(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
        end if

    END DO

    ! move the unactive clusters indexes array position and inverse direction
    Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag:ActiveFlag + ICTO - DisappearFlag - 1) = Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ICTO:DisappearFlag+1:-1)

    ! The index for empty clusters space
    !FORALL(IC=DisappearFlag:ActiveFlag:-1)
    FORALL(IC=ActiveFlag + ICTO - DisappearFlag:ICTO)
        Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(IC) = IC
    END FORALL

    ! Do some check
    if((DisappearFlag + 1 - ActiveFlag) .LT. 0) then
        write(*,*)  "MCPSCUERROR: The statistic error. The active and unactive clusters index would overlap."
        pause
        stop
    end if

    DO IStatu = 1,p_NUMBER_OF_STATU

        if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
            if(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu) .GT. 0) then
                NActCountTemp(IStatu) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)  &
                                      + Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
            else
                NActCountTemp(IStatu) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
            end if
        else
            NActCountTemp(IStatu) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
        end if


        if(NActCountTemp(IStatu) .GT. 0) then
            TheMCMigCoaleStatisticOneBox%RAVA(IStatu) = TheMCMigCoaleStatisticOneBox%RAVA(IStatu)/NActCountTemp(IStatu)
        end if
    END DO

    if(present(NActCount)) then
        NActCount = NActCountTemp
    end if

    return
  end subroutine GetOneBoxMCMigCoaleStat_Expd_CPU

  !*************************************************************
  subroutine GetBoxesMCMigCoaleStat_Virtual_CPU(Host_Boxes,Host_SimuCtrlParam,TheMCMigCoaleStatisticInfo)
    !***                 Purpose: To get average size of clusters, maxma size, and so on, at current time(for all boxs)
    !                 Host_Boxes: the boxes information in host
    implicit none
    !---Dummy Vars---
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MCMigCoaleStatisticInfo_Virtual)::TheMCMigCoaleStatisticInfo
    !---Local Vars---
    integer::MultiBox
    integer::IBox
    integer::IStatu
    real(kind=KINDDF)::RAVA(p_NUMBER_OF_STATU)
    integer::NActCountTemp(p_NUMBER_OF_STATU)
    integer::NCCount(p_NUMBER_OF_STATU)
    !---Body---

    NCCount = 0

    RAVA = 0.D0

    ASSOCIATE(TMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_IntegralBox)

    TMigStatInfo%RMAX = -1.D32
    TMigStatInfo%RMIN = 1.D32
    TMigStatInfo%RAVA  = 0.D0
    TMigStatInfo%DiffusorValueMax = -1.D32

    MultiBox = Host_SimuCtrlParam%MultiBox

    DO IBox = 1, MultiBox

        NActCountTemp = 0

        ASSOCIATE(SMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_SingleBoxes(IBox))
            call GetOneBoxMCMigCoaleStat_Virtual_CPU(IBox,Host_Boxes,Host_SimuCtrlParam,SMigStatInfo,NActCountTemp)

            DO IStatu = 1, p_NUMBER_OF_STATU
                if(TMigStatInfo%RMAX(IStatu) .LT. SMigStatInfo%RMAX(IStatu)) then
                    TMigStatInfo%RMAX(IStatu) = SMigStatInfo%RMAX(IStatu)
                    TMigStatInfo%ICMAX(IStatu) = SMigStatInfo%ICMAX(IStatu)
                end if

                if(TMigStatInfo%DiffusorValueMax(IStatu) .LT. SMigStatInfo%DiffusorValueMax(IStatu)) then
                    TMigStatInfo%DiffusorValueMax(IStatu) = SMigStatInfo%DiffusorValueMax(IStatu)
                end if

                if(TMigStatInfo%RMIN(IStatu) .GT. SMigStatInfo%RMIN(IStatu) .AND. NActCountTemp(IStatu) .GT. 0) then
                    TMigStatInfo%RMIN(IStatu) = SMigStatInfo%RMIN(IStatu)
                end if

                RAVA(IStatu) = RAVA(IStatu) + SMigStatInfo%RAVA(IStatu)*NActCountTemp(IStatu)
            END DO

            NCCount = NCCount + NActCountTemp

        END ASSOCIATE

    END DO

    DO IStatu = 1, p_NUMBER_OF_STATU
      if(NCCount(IStatu) .GT. 0) then
        TMigStatInfo%RAVA(IStatu) = RAVA(IStatu)/NCCount(IStatu)
      end if

    END DO

    END ASSOCIATE

    return
  end subroutine GetBoxesMCMigCoaleStat_Virtual_CPU

  !**************************************************************
  subroutine GetOneBoxMCMigCoaleStat_Virtual_CPU(IBox, Host_Boxes,Host_SimuCtrlParam,TheMCMigCoaleStatisticOneBox,NActCount)
    !   ***   Purpose        : To get average size of clusters, maxma size, and so on, at current time and this single box
    !                    IBox: the index of simulation box
    !              Host_Boxes: the boxes information in host

    implicit none
    !---Dummy Vars---
    integer, intent(in)::IBox
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MCMigCoaleStatisticOneBox)::TheMCMigCoaleStatisticOneBox
    integer,optional::NActCount(p_NUMBER_OF_STATU)
    !---Local Vars---
    integer::IC, ICFROM, ICTO
    integer::IStatu
    integer::ActiveFlag,DisappearFlag
    integer::NActCountTemp(p_NUMBER_OF_STATU)
    !---Body---

    TheMCMigCoaleStatisticOneBox%RMAX = 0.D0
    TheMCMigCoaleStatisticOneBox%RMIN = 1.D32
    TheMCMigCoaleStatisticOneBox%RAVA  = 0.D0
    TheMCMigCoaleStatisticOneBox%DiffusorValueMax = 0.D0

    ICFROM = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
    ICTO   = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

    if(ICTO .LE. 0) then
        return
    end if

    ActiveFlag = ICFROM
    DisappearFlag = ICTO

    DO IC = ICFROM, ICTO
        IStatu = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

        if(IStatu .eq. p_Empty) then
            cycle
        end if

        if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
            ! The index for active clusters
            Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag) = IC
            ActiveFlag = ActiveFlag + 1
        else
            ! The index for unactive clusters
            Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(DisappearFlag) = IC
            DisappearFlag = DisappearFlag - 1
        end if

        TheMCMigCoaleStatisticOneBox%RAVA(IStatu) = TheMCMigCoaleStatisticOneBox%RAVA(IStatu) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff .GT. TheMCMigCoaleStatisticOneBox%DiffusorValueMax(IStatu)) then
            TheMCMigCoaleStatisticOneBox%DiffusorValueMax(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff
        end if

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .GT. TheMCMigCoaleStatisticOneBox%RMAX(IStatu)) then
            TheMCMigCoaleStatisticOneBox%RMAX(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
            TheMCMigCoaleStatisticOneBox%ICMAX(IStatu) = IC
        end if

        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .LT. TheMCMigCoaleStatisticOneBox%RMIN(IStatu)) then
            TheMCMigCoaleStatisticOneBox%RMIN(IStatu) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
        end if

    END DO

    ! move the unactive clusters indexes array position and inverse direction
    Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag:ActiveFlag + ICTO - DisappearFlag - 1) = Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(ICTO:DisappearFlag+1:-1)

    ! The index for empty clusters space
    !FORALL(IC=DisappearFlag:ActiveFlag:-1)
    FORALL(IC=ActiveFlag + ICTO - DisappearFlag:ICTO)
        Host_Boxes%m_ClustersInfo_CPU%m_ActiveIndex(IC) = IC
    END FORALL

    ! Do some check
    if((DisappearFlag + 1 - ActiveFlag) .LT. 0) then
        write(*,*)  "MCPSCUERROR: The statistic error. The active and unactive clusters index would overlap."
        pause
        stop
    end if

    DO IStatu = 1,p_NUMBER_OF_STATU

        if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then

            if(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu) .GT. 0) then
                NActCountTemp(IStatu) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)  &
                                      + Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
            else
                NActCountTemp(IStatu) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
            end if
        else
            NActCountTemp(IStatu) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu)
        end if


        if(NActCountTemp(IStatu) .GT. 0) then
            TheMCMigCoaleStatisticOneBox%RAVA(IStatu) = TheMCMigCoaleStatisticOneBox%RAVA(IStatu)/NActCountTemp(IStatu)
        end if
    END DO

    if(present(NActCount)) then
        NActCount = NActCountTemp
    end if

    return
  end subroutine GetOneBoxMCMigCoaleStat_Virtual_CPU

end module MCMIGCOALE_STATISTIC_CPU
