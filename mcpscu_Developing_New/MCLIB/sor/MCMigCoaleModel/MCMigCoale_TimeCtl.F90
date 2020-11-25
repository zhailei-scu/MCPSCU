module MCMIGCOALE_TIMECTL
    use cudafor
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY_GPU
    use MCMIGCOALE_STATISTIC_GPU
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    use MCMIGCOALE_TYPEDEF_STATISTICINFO
    use MCLIB_CAL_NEIGHBOR_LIST_GPU
    implicit none


    real(kind=KINDDF), private, parameter::p_ZeroNCStep = 1.D-10

    contains

    !**************************************************************
    subroutine UpdateTimeStep_MCMigCoal(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMCMigCoaleStatisticInfo,Record,TSTEP)
        ! To automatically determine the time step
        !  Host_Boxes: the boxes info in host
        !       INPUT: DIF - the diffusion coefficient
        !      OUTPUT: TSTEP - the time step
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MCMigCoaleStatisticInfo_Expd)::TheMCMigCoaleStatisticInfo
        CLASS(SimulationRecord)::Record
        real(kind=KINDDF)::TSTEP
        !---Local Vars---
        real(kind=KINDDF)::SEP, DIF
        integer::NCActFree
        integer::NCActGB
        real(kind=KINDDF)::TSTEPFREE,TSTEPGB
        integer::I
        !---Body---

        TSTEPFREE = 1.D32
        TSTEPGB = 1.D32

        ASSOCIATE(TBasicInfo=>Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral,TMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_IntegralBox)

        select case(Host_SimuCtrlParam%UPDATETSTEPSTRATEGY)
            case(mp_SelfAdjustlStep_NearestSep)
                if(Dev_Boxes%dm_ClusterInfo_GPU%GetNLUpdateCount_Dev() .LE. 0) then
                    call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                                max(TMigStatInfo%RMAX(p_ACTIVEFREE_STATU),TMigStatInfo%RMAX(p_ACTIVEINGB_STATU)),&
                                                MaxDiffuse=max(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU), &
                                                               TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU)))
                end if

                if(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .LE. 0.D0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .LE. 0.D0) then
                    TSTEP = p_ZeroNCStep
                    return
                end if

                NCActFree = TBasicInfo%NC(p_ACTIVEFREE_STATU)
                if(NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) then
                    TSTEPFREE = (TBasicInfo%AveNearestSpeFreeClusters**2)/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                NCActGB = TBasicInfo%NC(p_ACTIVEINGB_STATU)
                if(NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0) then
                    TSTEPGB = (TBasicInfo%AveNearestSpeGBClusters**2)/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                if((NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) &
                   .or. (NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0)) then
                    TSTEP = min(TSTEPFREE,TSTEPGB)
                else
                    TSTEP = p_ZeroNCStep
                end if

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TSTEP = min(TSTEP,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

            case(mp_SelfAdjustlStep_AveSep)
                if(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .LE. 0.D0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .LE. 0.D0) then
                    TSTEP = p_ZeroNCStep
                    return
                end if

                NCActFree = TBasicInfo%NC(p_ACTIVEFREE_STATU)
                if(NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) then
                    SEP = Host_SimuCtrlParam%MultiBox*Host_Boxes%BOXVOLUM/dble(NCActFree)

                    SEP = SEP**(0.33333333333333D0)
                    SEP = SEP - 2.D0*TMigStatInfo%RAVA(p_ACTIVEFREE_STATU)

                    TSTEPFREE = SEP*SEP/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                NCActGB = TBasicInfo%NC(p_ACTIVEINGB_STATU)
                if(NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0) then
                    SEP = Host_SimuCtrlParam%MultiBox*Host_Boxes%BOXVOLUM/dble(Host_Boxes%m_GrainBoundary%GrainNum)

                    SEP = 6*(SEP**(0.66666666667D0))*Host_Boxes%m_GrainBoundary%GrainNum
                    SEP = SEP/dble(NCActGB)
                    SEP = SEP**(0.5D0)
                    SEP = SEP - 2.D0*TMigStatInfo%RAVA(p_ACTIVEINGB_STATU)

                    TSTEPGB = SEP*SEP/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                if((NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) &
                   .or. (NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0)) then
                    TSTEP = min(TSTEPFREE,TSTEPGB)
                else
                    TSTEP = p_ZeroNCStep
                end if

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TSTEP = min(TSTEP,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

            case(mp_FixedTimeStep)
                TSTEP = Host_SimuCtrlParam%FixedTimeStepValue

            case(mp_SelfAdjustlStep_NNDR)
                TSTEP = minval(Dev_Boxes%dm_ClusterInfo_GPU%dm_MinTSteps)

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TSTEP = min(TSTEP,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

                TSTEP = max(TSTEP,dble(Host_SimuCtrlParam%LowerLimitTime))

            case(mp_SelfAdjustlStep_NNDR_LastPassage_Integer)
                TSTEP = floor(minval(Dev_Boxes%dm_ClusterInfo_GPU%dm_MinTSteps)/dble(Host_SimuCtrlParam%LowerLimitTime))*dble(Host_SimuCtrlParam%LowerLimitTime)

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TSTEP = min(TSTEP,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

                TSTEP = max(TSTEP,dble(Host_SimuCtrlParam%LowerLimitTime))

            case default
                write(*,*) "MCPSCUERROR: Unknown strategy to update time step :",Host_SimuCtrlParam%UPDATETSTEPSTRATEGY
                pause
                stop
        end select

        END ASSOCIATE

        !***********Focused TimePoint*********************
        call Record%TurnOffTriggerFocusedTimePoints()

        DO I = 1,Host_SimuCtrlParam%NFocusedTimePoint
            if( ((Record%GetSimuTimes() + TSTEP) .GE. Host_SimuCtrlParam%FocusedTimePoints(I)) .AND. &
                Record%GetSimuTimes() .LT. Host_SimuCtrlParam%FocusedTimePoints(I) ) then

                call Record%TurnOnTriggerFocusedTimePoints()

                TSTEP = DABS(Host_SimuCtrlParam%FocusedTimePoints(I) - Record%GetSimuTimes())

                exit
            end if
        END DO

        return
    end subroutine UpdateTimeStep_MCMigCoal

    !*********************************************************
    function Cal_VerifyTime_Implant(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMCMigCoaleStatisticInfo,Record,ImplantedNumEachBox) result(TheVerifyTime)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MCMigCoaleStatisticInfo_Virtual)::TheMCMigCoaleStatisticInfo
        CLASS(SimulationRecord)::Record
        integer,intent(in)::ImplantedNumEachBox
        real(kind=KINDDF),intent(out)::TheVerifyTime
        !---Local Vars---
        real(kind=KINDDF)::SEP, DIF
        integer::NCActFree,NCActGB
        real(kind=KINDDF)::RAVA
        real(kind=KINDDF)::TSTEPFREE,TSTEPGB
        integer::MultiBox
        integer::I
        !---Body---

        TSTEPFREE = 1.D32
        TSTEPGB = 1.D32

        MultiBox = Host_SimuCtrlParam%MultiBox

        ASSOCIATE(TBasicInfo=>Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral,TMigStatInfo=>TheMCMigCoaleStatisticInfo%statistic_IntegralBox)

        select case(Host_SimuCtrlParam%UPDATETSTEPSTRATEGY)
            case(mp_SelfAdjustlStep_NearestSep)
                if(Dev_Boxes%dm_ClusterInfo_GPU%GetNLUpdateCount_Dev() .LE. 0) then
                    call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                                max(TMigStatInfo%RMAX(p_ACTIVEFREE_STATU),TMigStatInfo%RMAX(p_ACTIVEINGB_STATU)),&
                                                MaxDiffuse=max(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU), &
                                                               TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU)))
                end if

                if(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .LE. 0.D0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .LE. 0.D0) then
                    TheVerifyTime = p_ZeroNCStep
                    return
                end if

                NCActFree = TBasicInfo%NC(p_ACTIVEFREE_STATU) + MultiBox*ImplantedNumEachBox
                if(NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) then
                    TSTEPFREE = (TBasicInfo%AveNearestSpeFreeClusters**2)/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                NCActGB = TBasicInfo%NC(p_ACTIVEINGB_STATU) + MultiBox*ImplantedNumEachBox
                if(NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0) then
                    TSTEPGB = (TBasicInfo%AveNearestSpeGBClusters**2)/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                if((NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) &
                   .or. (NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0)) then
                    TheVerifyTime = min(TSTEPFREE,TSTEPGB)
                else
                    TheVerifyTime = p_ZeroNCStep
                end if

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TheVerifyTime = min(TheVerifyTime,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

            case(mp_SelfAdjustlStep_AveSep)
                if(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .LE. 0.D0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .LE. 0.D0) then
                    TheVerifyTime = p_ZeroNCStep
                    return
                end if

                NCActFree = TBasicInfo%NC(p_ACTIVEFREE_STATU) + MultiBox*ImplantedNumEachBox
                if(NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) then
                    SEP = Host_SimuCtrlParam%MultiBox*Host_Boxes%BOXVOLUM/dble(NCActFree)

                    SEP = SEP**(0.33333333333333D0)
                    SEP = SEP - 2.D0*TMigStatInfo%RAVA(p_ACTIVEFREE_STATU)

                    TSTEPFREE = SEP*SEP/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                NCActGB = TBasicInfo%NC(p_ACTIVEINGB_STATU) + MultiBox*ImplantedNumEachBox
                if(NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0) then
                    SEP = Host_SimuCtrlParam%MultiBox*Host_Boxes%BOXVOLUM/dble(Host_Boxes%m_GrainBoundary%GrainNum)

                    SEP = 6*(SEP**(0.66666666667D0))*Host_Boxes%m_GrainBoundary%GrainNum
                    SEP = SEP/dble(NCActGB)
                    SEP = SEP**(0.5D0)
                    SEP = SEP - 2.D0*TMigStatInfo%RAVA(p_ACTIVEINGB_STATU)

                    TSTEPGB = SEP*SEP/(6.D0*TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU))*Host_SimuCtrlParam%EnlageTStepScale
                end if

                if((NCActFree .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU) .GT. 0.D0) &
                   .or. (NCActGB .GT. 0 .AND. TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU) .GT. 0.D0)) then
                    TheVerifyTime = min(TSTEPFREE,TSTEPGB)
                else
                    TheVerifyTime = p_ZeroNCStep
                end if

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TheVerifyTime = min(TheVerifyTime,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

            case(mp_FixedTimeStep)
                TheVerifyTime = Host_SimuCtrlParam%FixedTimeStepValue

            case(mp_SelfAdjustlStep_NNDR)
                TheVerifyTime = minval(Dev_Boxes%dm_ClusterInfo_GPU%dm_MinTSteps)

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TheVerifyTime = min(TheVerifyTime,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

                TheVerifyTime = max(TheVerifyTime,dble(Host_SimuCtrlParam%LowerLimitTime))

            case(mp_SelfAdjustlStep_NNDR_LastPassage_Integer)
                TheVerifyTime = floor(minval(Dev_Boxes%dm_ClusterInfo_GPU%dm_MinTSteps)/dble(Host_SimuCtrlParam%LowerLimitTime))*dble(Host_SimuCtrlParam%LowerLimitTime)

                if(Host_SimuCtrlParam%TermTFlag .eq. mp_TermTimeFlag_ByRealTime) then
                    TheVerifyTime = min(TheVerifyTime,Host_SimuCtrlParam%TermTValue/100.D0)
                end if

                TheVerifyTime = max(TheVerifyTime,dble(Host_SimuCtrlParam%LowerLimitTime))

            case default
                write(*,*) "MCPSCUERROR: Unknown strategy to update time step :",Host_SimuCtrlParam%UPDATETSTEPSTRATEGY
                pause
                stop
            end select

        END ASSOCIATE

        !***********Focused TimePoint*********************

        DO I = 1,Host_SimuCtrlParam%NFocusedTimePoint
            if( ((Record%GetSimuTimes() + TheVerifyTime) .GE. Host_SimuCtrlParam%FocusedTimePoints(I)) .AND. &
                Record%GetSimuTimes() .LT. Host_SimuCtrlParam%FocusedTimePoints(I) ) then

                call Record%TurnOnTriggerFocusedTimePoints()

                TheVerifyTime = DABS(Host_SimuCtrlParam%FocusedTimePoints(I) - Record%GetSimuTimes())
                exit
            end if
        END DO


    end function Cal_VerifyTime_Implant


end module MCMIGCOALE_TIMECTL
