module MCLIB_TYPEDEF_BASICRECORD_SUB

    USE MCLIB_CONSTANTS
    USE MCLIB_UTILITIES

    implicit none

    type,public::BoxStatis
        integer::NC0(p_NUMBER_OF_STATU) = 0                            ! initial number of clusters
        integer::NC(p_NUMBER_OF_STATU)  = 0                            ! the number of all status clusters at current time intervale
        integer(kind=KMCLINT)::NA(p_NUMBER_OF_STATU) = 0               ! the number of all status atomics at current time intervale
        integer::NCDumpAdded = 0

        real(kind=KINDDF)::AveNearestSpeFreeClusters = 0.D0
        real(kind=KINDDF)::AveNearestSpeGBClusters = 0.D0
        contains
        procedure,public,non_overridable,pass::Init=>Init_BoxStatis
        procedure,public,non_overridable,pass::Clean=>Clean_BoxStatis
        procedure,private,non_overridable,pass::CopyBoxStatisFromOther
        Generic::ASSIGNMENT(=)=>CopyBoxStatisFromOther
        Final::CleanBoxStatis
    end type BoxStatis

    type,public::BoxesBasicStatistic
        type(BoxStatis)::BoxesStatis_Integral
        type(BoxStatis),dimension(:),allocatable::BoxesStatis_Single

        contains
        procedure,public,non_overridable,pass::Init=>Init_BoxesBasicStatistic
        procedure,public,non_overridable,pass::Clean=>Clean_BoxesBasicStatistic
        procedure,private,non_overridable,pass::CopyBoxesBasicStatisticFromOther
        Generic::ASSIGNMENT(=)=>CopyBoxesBasicStatisticFromOther
        Final::CleanBoxesBasicStatistic
    end type BoxesBasicStatistic

    type,public::BoxesInfo
        integer, dimension(:,:), allocatable::SEActIndexBox            ! the start and end active clusters Index in each Box
                                                                       ! (this array is got while each box is re-scanned and
                                                                       ! dm_SEActIndexBox(1,1)=1,
                                                                       ! dm_SEActIndexBox(1,2)=dm_SEActIndexBox(1,1) + m_Boxes%Box(1)%SNC(p_ActiveFree_statu) + m_Boxes%Box(1)%SNC(p_ActiveINGB_statu) - 1,
                                                                       ! dm_SEActIndexBox(2,1)=dm_SEActIndexBox(1,2) + 1,
                                                                       ! dm_SEActIndexBox(2,2)=dm_SEActIndexBox(2,1) + m_Boxes%Box(2)%SNC(p_ActiveFree_statu) + m_Boxes%Box(2)%SNC(p_ActiveINGB_statu)- 1)

        integer, dimension(:,:), allocatable::SEUsedIndexBox               ! the actual simulated start and end index cluster in each Box (CPU)

        integer, dimension(:,:), allocatable::SEVirtualIndexBox             ! while there are clusters are implanted to the box, to improve the calculation efficiency, we would pre-allocate a bigger block of
                                                                       ! array (bigger than required) to leave some free memory space for each box, and based on the implantation type, we would put lots of
                                                                       ! clusters in these free memory one-time. So we need not put clusters in each step, in each step, we need only to move the end cluster index
                                                                       ! of each box. The SEVirtualIndexBox is the start and end index for the bigger memory space for each box. The SEUsedIndexBox is the actual start
                                                                       ! and end index for each box. By other words, we need only move the SEUsedIndexBox(:,2) for each box in each step.


        integer, dimension(:,:), allocatable::SEExpdIndexBox              ! In implantation situation, as the total clusters in each box is changing in each box, it is necessary to update neighbor-list in each step,
                                                                       ! obviously, it is pretty inefficiency. To improve the calculation efficiency, as described for SEVirtualIndexBox, a block of bigger memory space
                                                                       ! is pre-allocated for each box, we can let the neighbor-list calculation to cover all of this block, but it seems like have over-calculation,
                                                                       ! because some clusters are not used. To be a compromise strategy, we use SEExpdIndexBox to indicate the clusters index range to be calculated
                                                                       ! the SEExpdIndexBox(IBox,1) = SEUsedIndexBox(IBox,1) = SEVirtualIndexBox(IBox,1),
                                                                       !  = SEUsedIndexBox(IBox,2) < SEExpdIndexBox(IBox,2) < SEVirtualIndexBox(IBox,2)
        integer, dimension(:,:), allocatable::SEAddedClustersBoxes

        contains
        procedure,public,pass,non_overridable::Init=>InitBoxesInfo
        procedure,public,pass,non_overridable::Clean=>Clean_BoxesInfo
        procedure,private,pass,non_overridable::CopyBoxesInfoFromOther
        Generic::ASSIGNMENT(=)=>CopyBoxesInfoFromOther
        Final::CleanBoxesInfo

    end type BoxesInfo
    !------------------
    type,public::RunningRecord

        real::LastRecordOutprofileTime = 0.D0

        logical,private::stopRunningFlag = .false.
        character(LEN = 12)::Start_Clock(3), End_Clock(3)
        integer::Start_DateTime(8),End_DateTime(8)

        contains
        procedure,NON_OVERRIDABLE,public,pass::InitRunningRecord
        procedure,NON_OVERRIDABLE,public,pass::StopRunning=>Stop_Running
        procedure,NON_OVERRIDABLE,public,pass::IsStoppedRunning=>Is_StoppedRunning
        procedure,non_overridable,public,pass::CopyRunningRecordFromOther
        procedure,non_overridable,public,pass::Clean_RunningRecord
        Generic::Assignment(=)=>CopyRunningRecordFromOther
        Final::CleanRunningRecord

    end type RunningRecord


    type,abstract,public::SimulationRecord_SUB
        type(RunningRecord)::Running_Record

        integer,private::SimulaitonSteps = 0
        integer,private::SimulationPatch = 1
        real(kind=KINDDF),private::SimulationTimes = 0.D0
        real(kind=KINDDF),private::TimeStep = 0.D0
        integer,private::TimeSections = 1

        real(kind=KINDDF),private::LastUpdateStatisTime = 0.D0

        real(kind=KINDDF),private::LastUpdateNLTime = 0.D0
        integer,private::LastUpdateNL_NC0 = 1

        integer::RecordNCBeforeSweepOut_Integal(p_NUMBER_OF_STATU)  = 0
        integer,dimension(:,:),allocatable::RecordNCBeforeSweepOut_SingleBox

        real(kind=KINDDF),private::LastRecordOutConfigTime = 0.D0
        integer,private::OutPutIndex = 0

        logical,private::TriggerFocusedTimePoints = .false.

        contains
        procedure,NON_OVERRIDABLE,public,pass::InitSimulationRecord_SUB
        procedure,NON_OVERRIDABLE,public,pass::SetSimuSteps=>Set_SimuSteps
        procedure,NON_OVERRIDABLE,public,pass::GetSimuSteps=>Get_SimuSteps
        procedure,NON_OVERRIDABLE,public,pass::IncreaseOneSimuStep=>Increase_OneSimuStep

        procedure,NON_OVERRIDABLE,public,pass::SetSimuTimes=>Set_SimuTimes
        procedure,NON_OVERRIDABLE,public,pass::GetSimuTimes=>Get_SimuTimes
        procedure,NON_OVERRIDABLE,public,pass::AddSimuTimes=>Add_SimuTimes


        procedure,NON_OVERRIDABLE,public,pass::SetTimeSteps=>Set_TimeSteps
        procedure,NON_OVERRIDABLE,public,pass::GetTimeSteps=>Get_TimeSteps

        procedure,NON_OVERRIDABLE,public,pass::SetSimuPatch=>Set_SimuPatch
        procedure,NON_OVERRIDABLE,public,pass::GetSimuPatch=>Get_SimuPatch

        procedure,NON_OVERRIDABLE,public,pass::SetTimeSections=>Set_TimeSections
        procedure,NON_OVERRIDABLE,public,pass::GetTimeSections=>Get_TimeSections
        procedure,NON_OVERRIDABLE,public,pass::IncreaseOneTimeSection=>Increase_OneTimeSection

        procedure,NON_OVERRIDABLE,public,pass::GetLastUpdateStatisTime=>Get_LastUpdateStatisTime
        procedure,NON_OVERRIDABLE,public,pass::SetLastUpdateStatisTime=>Set_LastUpdateStatisTime

        procedure,NON_OVERRIDABLE,public,pass::GetLastUpdateNLTime=>Get_LastUpdateNLTime
        procedure,NON_OVERRIDABLE,public,pass::SetLastUpdateNLTime=>Set_LastUpdateNLTime

        procedure,NON_OVERRIDABLE,public,pass::GetLastUpdateNLNC0=>Get_LastUpdateNLNC0
        procedure,NON_OVERRIDABLE,public,pass::SetLastUpdateNLNC0=>Set_LastUpdateNLNC0

        procedure,NON_OVERRIDABLE,public,pass::RecordNC_ForSweepOut

        procedure,NON_OVERRIDABLE,public,pass::GetLastRecordOutConfigTime=>Get_LastRecordOutConfigTime
        procedure,NON_OVERRIDABLE,public,pass::SetLastRecordOutConfigTime=>Set_LastRecordOutConfigTime

        procedure,NON_OVERRIDABLE,public,pass::GetOutPutIndex=>Get_OutPutIndex
        procedure,NON_OVERRIDABLE,public,pass::SetOutPutIndex=>Set_OutPutIndex
        procedure,NON_OVERRIDABLE,public,pass::IncreaseOneOutPutIndex=>Increase_OneOutPutIndex

        procedure,NON_OVERRIDABLE,public,pass::TurnOnTriggerFocusedTimePoints=>TurnOn_TriggerFocusedTimePoints
        procedure,NON_OVERRIDABLE,public,pass::TurnOffTriggerFocusedTimePoints=>TurnOff_TriggerFocusedTimePoints
        procedure,NON_OVERRIDABLE,public,pass::GetStatusTriggerFocusedTimePoints=>Get_StatusTriggerFocusedTimePoints

        procedure,non_overridable,public,pass::CopySimulationRecordSUBFromOther
        procedure,non_overridable,public,pass::Clean_SimulationRecordSUB
        Generic::Assignment(=)=>CopySimulationRecordSUBFromOther

        !---abstract method---
        procedure(DefCleanProc),pass,deferred,private::TheDefCleanProc
        procedure(DefCopyProc),pass,deferred,private::TheDefCopyProc

    end type SimulationRecord_SUB

    private::Init_BoxStatis
    private::CopyBoxStatisFromOther
    private::Clean_BoxStatis
    private::Init_BoxesBasicStatistic
    private::CopyBoxesBasicStatisticFromOther
    private::Clean_BoxesBasicStatistic
    private::CleanBoxesBasicStatistic
    private::InitBoxesInfo
    private::CopyBoxesInfoFromOther
    private::Clean_BoxesInfo
    private::CleanBoxesInfo
    private::InitRunningRecord
    private::Stop_Running
    private::Is_StoppedRunning
    private::CopyRunningRecordFromOther
    private::Clean_RunningRecord
    private::CleanRunningRecord
    private::InitSimulationRecord_SUB
    private::Set_SimuSteps
    private::Get_SimuSteps
    private::Increase_OneSimuStep
    private::Set_SimuTimes
    private::Get_SimuTimes
    private::Add_SimuTimes
    private::Set_TimeSteps
    private::Get_TimeSteps
    private::Set_SimuPatch
    private::Get_SimuPatch
    private::Set_TimeSections
    private::Get_TimeSections
    private::Increase_OneTimeSection
    private::Get_LastUpdateStatisTime
    private::Set_LastUpdateStatisTime
    private::Get_LastUpdateNLTime
    private::Set_LastUpdateNLTime
    private::Get_LastUpdateNLNC0
    private::Set_LastUpdateNLNC0
    private::RecordNC_ForSweepOut
    private::Get_LastRecordOutConfigTime
    private::Set_LastRecordOutConfigTime
    private::Get_OutPutIndex
    private::Set_OutPutIndex
    private::Increase_OneOutPutIndex
    private::TurnOn_TriggerFocusedTimePoints
    private::TurnOff_TriggerFocusedTimePoints
    private::Get_StatusTriggerFocusedTimePoints
    private::CopySimulationRecordSUBFromOther
    private::Clean_SimulationRecordSUB

    contains

    !*************For type BoxStatis*******************************
    subroutine Init_BoxStatis(this)
        implicit none
        !---Dummy Vars---
        CLASS(BoxStatis)::this
        !---Body---
        this%NC0 = 0
        this%NC = 0
        this%NA = 0
        this%NCDumpAdded = 0

        this%AveNearestSpeFreeClusters = 0.D0
        this%AveNearestSpeGBClusters = 0.D0

        return
    end subroutine Init_BoxStatis

    !***************************************************************
    subroutine CopyBoxStatisFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(BoxStatis),intent(out)::this
        type(BoxStatis), intent(in)::other
        !---Body---
        call this%Clean()

        this%NC0 = other%NC0

        this%NC = other%NC

        this%NA = other%NA

        this%NCDumpAdded = other%NCDumpAdded

        this%AveNearestSpeFreeClusters = other%AveNearestSpeFreeClusters
        this%AveNearestSpeGBClusters = other%AveNearestSpeGBClusters

        return
    end subroutine

    !*****************************************************************
    subroutine Clean_BoxStatis(this)
        implicit none
        !---Dummy Vars---
        CLASS(BoxStatis)::this
        !---Body---
        this%NC0 = 0
        this%NC = 0
        this%NA = 0

        this%NCDumpAdded = 0

        this%AveNearestSpeFreeClusters = 0.D0
        this%AveNearestSpeGBClusters = 0.D0

        return
    end subroutine Clean_BoxStatis

    !*****************************************************************
    subroutine CleanBoxStatis(this)
        implicit none
        !---Dummy Vars---
        type(BoxStatis)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanBoxStatis

    !*************For type BoxesBasicStatistic***************************
    subroutine Init_BoxesBasicStatistic(this,MultiBox)
        implicit none
        !---Dummy Vars---
        CLASS(BoxesBasicStatistic)::this
        integer,intent(in)::MultiBox
        !---Local Vars---
        integer::IBox
        !---Body---
        if(allocated(this%BoxesStatis_Single)) then
            deallocate(this%BoxesStatis_Single)
        end if
        allocate(this%BoxesStatis_Single(MultiBox))

        DO IBox = 1, MultiBox
            call this%BoxesStatis_Single(IBox)%Init()
        END DO
        call this%BoxesStatis_Integral%Init()

        return
    end subroutine

    !************************************************
    subroutine Clean_BoxesBasicStatistic(this)
        implicit none
        !---Dummy Vars---
        CLASS(BoxesBasicStatistic)::this

        !---Body---
        if(allocated(this%BoxesStatis_Single)) then
            deallocate(this%BoxesStatis_Single)
        end if
        call this%BoxesStatis_Integral%Clean()

        return
    end subroutine

    !*************************************************
    subroutine CopyBoxesBasicStatisticFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(BoxesBasicStatistic),intent(out)::this
        type(BoxesBasicStatistic),intent(in)::other
        !---Local Vars---
        integer::IBox
        integer::MultiBox
        !---Body---
        call this%Clean()

        MultiBox = size(other%BoxesStatis_Single)
        call this%Init(MultiBox)

        ! The assignment(=) had been override
        this%BoxesStatis_Integral = other%BoxesStatis_Integral

        DO IBox = 1,MultiBox
            ! The assignment(=) had been override
            this%BoxesStatis_Single(IBox) = other%BoxesStatis_Single(IBox)
        END DO

        return
    end subroutine

    !*************************************************
    subroutine CleanBoxesBasicStatistic(this)
        implicit none
        !---Dummy Vars---
        type(BoxesBasicStatistic)::this
        !---Body---
        call this%Clean()

        return
    end subroutine


    !*************For type BoxesInfo********************************
    subroutine InitBoxesInfo(this,MultiBox)
        implicit none
        !---Dummy Vars---
        CLASS(BoxesInfo)::this
        integer,intent(in)::MultiBox
        !---Body---

        call AllocateArray_Host(this%SEActIndexBox,MULTIBOX,2,"SEActIndexBox")
        this%SEActIndexBox = 0

        call AllocateArray_Host(this%SEUsedIndexBox,MULTIBOX,2,"SEUsedIndexBox")
        this%SEUsedIndexBox = 0

        call AllocateArray_Host(this%SEExpdIndexBox,MULTIBOX,2,"SEExpdIndexBox")
        this%SEExpdIndexBox = 0

        call AllocateArray_Host(this%SEAddedClustersBoxes,MULTIBOX,2,"SEAddedClustersBoxes")
        this%SEAddedClustersBoxes = 0

        call AllocateArray_Host(this%SEVirtualIndexBox,MULTIBOX,2,"SEVirtualIndexBox")
        this%SEVirtualIndexBox = 0

        return
    end subroutine InitBoxesInfo

    !*****************************************************************
    subroutine CopyBoxesInfoFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        CLASS(BoxesInfo),intent(out)::this
        type(BoxesInfo),intent(in)::Other
        !---Local Vars---
        integer::MultiBox
        !---Body---
        call this%Clean()

        MultiBox = size(Other%SEUSedIndexBox,DIM=1)

        call this%Init(MultiBox)

        this%SEActIndexBox = other%SEActIndexBox

        this%SEUsedIndexBox = other%SEUsedIndexBox

        this%SEExpdIndexBox = Other%SEExpdIndexBox

        this%SEAddedClustersBoxes = other%SEAddedClustersBoxes

        this%SEVirtualIndexBox = other%SEVirtualIndexBox
        return
    end subroutine

    !******************************************************************
    subroutine Clean_BoxesInfo(this)
        implicit none
        !---Dummy Vars---
        CLASS(BoxesInfo)::this
        !---Body---

        call DeAllocateArray_Host(this%SEActIndexBox,"SEActIndexBox")

        call DeAllocateArray_Host(this%SEUsedIndexBox,"SEUsedIndexBox")

        call DeAllocateArray_Host(this%SEAddedClustersBoxes,"SEAddedClustersBoxes")

        call DeAllocateArray_Host(this%SEVirtualIndexBox,"SEVirtualIndexBox")

        call DeAllocateArray_Host(this%SEExpdIndexBox,"SEExpdIndexBox")

        return
    end subroutine Clean_BoxesInfo

    !***************************************************************
    subroutine CleanBoxesInfo(this)
        implicit none
        !---Dummy Vars---
        type(BoxesInfo)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanBoxesInfo

    !************type RunningRecord*******************
    subroutine InitRunningRecord(this)
        implicit none
        CLASS(RunningRecord)::this

        this%LastRecordOutprofileTime = 0.D0

        this%stopRunningFlag= .false.
        this%Start_Clock = ''
        this%End_Clock = ''
        this%Start_DateTime = 0
        this%End_DateTime = 0

    end subroutine


    subroutine Stop_Running(this)
        implicit none
        CLASS(RunningRecord)::this

        this%stopRunningFlag = .true.
        return
    end subroutine

    logical function Is_StoppedRunning(this)
        implicit none
        CLASS(RunningRecord)::this

        Is_StoppedRunning = this%stopRunningFlag
        return
    end function Is_StoppedRunning


    subroutine CopyRunningRecordFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        CLASS(RunningRecord),intent(out)::this
        CLASS(RunningRecord),intent(in)::Other
        !---Body---
        this%LastRecordOutprofileTime = Other%LastRecordOutprofileTime
        this%stopRunningFlag = Other%stopRunningFlag

        this%Start_Clock = Other%Start_Clock
        this%End_Clock = Other%End_Clock
        this%Start_DateTime = Other%Start_DateTime
        this%End_DateTime = Other%End_DateTime
        return
    end subroutine

    subroutine Clean_RunningRecord(this)
        implicit none
        CLASS(RunningRecord)::this

        this%LastRecordOutprofileTime = 0.D0

        this%stopRunningFlag= .false.
        this%Start_Clock = ''
        this%End_Clock = ''
        this%Start_DateTime = 0
        this%End_DateTime = 0
        return
    end subroutine

    subroutine CleanRunningRecord(this)
        implicit none
        type(RunningRecord)::this

        call this%Clean_RunningRecord()
        return
    end subroutine

    !****abstract type SimulationRecord_SUB*****************
    subroutine InitSimulationRecord_SUB(this,MultiBox,SimuSteps,SimuTimes,SimuPatchs,TimeSections)
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(in)::MultiBox
        integer,optional::SimuSteps
        real(kind=KINDDF),optional::SimuTimes
        integer,optional::SimuPatchs
        integer,optional::TimeSections
        !---Local Vars---
        integer::Steps
        real(kind=KINDDF)::Times
        integer::Patchs
        integer::TheTimeSection
        !---Body---
        Steps = 0
        Times = 0.D0
        Patchs = 1
        TheTimeSection = 1

        if(present(SimuSteps)) then
            Steps = SimuSteps
        end if

        if(present(SimuTimes)) then
            Times = SimuTimes
        end if

        if(present(SimuPatchs)) then
            Patchs = SimuPatchs
        end if

        if(present(TimeSections)) then
            TheTimeSection = TimeSections
        end if

        call this%SetSimuSteps(Steps)

        call this%SetSimuTimes(Times)

        call this%SetSimuPatch(Patchs)

        call this%SetTimeSections(TheTimeSection)

        call this%Running_Record%InitRunningRecord()

        this%LastUpdateStatisTime = 0.D0

        this%LastUpdateNLTime = 0.D0
        this%LastUpdateNL_NC0 = 1


        call AllocateArray_Host(this%RecordNCBeforeSweepOut_SingleBox,MultiBox,p_NUMBER_OF_STATU,"RecordNCBeforeSweepOut_SingleBox")
        this%RecordNCBeforeSweepOut_SingleBox = 0
        this%RecordNCBeforeSweepOut_Integal = 0

        this%LastRecordOutConfigTime = 0.D0
        this%OutPutIndex = 0

        this%TriggerFocusedTimePoints = .false.

        return
    end subroutine InitSimulationRecord_SUB

    !***************************************************
    subroutine Set_SimuSteps(this,Steps)
            implicit none
            CLASS(SimulationRecord_SUB)::this
            integer::Steps
            this%SimulaitonSteps = Steps
    end subroutine

    !***************************************************
    integer function Get_SimuSteps(this)
            implicit none
            CLASS(SimulationRecord_SUB)::this
            Get_SimuSteps = this%SimulaitonSteps
    end function


    !***************************************************
    subroutine Increase_OneSimuStep(this)
        implicit none
        CLASS(SimulationRecord_SUB)::this

        this%SimulaitonSteps = this%SimulaitonSteps + 1

        return
    end subroutine Increase_OneSimuStep

    !***************************************************
    subroutine Set_SimuTimes(this,Times)
            implicit none
            CLASS(SimulationRecord_SUB)::this
            real(kind=KINDDF)::Times
            this%SimulationTimes = Times
    end subroutine

    !***************************************************
    real(kind=KINDDF) function Get_SimuTimes(this)
            implicit none
            CLASS(SimulationRecord_SUB)::this
            Get_SimuTimes = this%SimulationTimes
    end function


    !***************************************************
    subroutine Set_TimeSteps(this,TimeStep)
            implicit none
            CLASS(SimulationRecord_SUB)::this
            real(kind=KINDDF)::TimeStep
            this%TimeStep = TimeStep
    end subroutine

    !***************************************************
    real(kind=KINDDF) function Get_TimeSteps(this)
            implicit none
            CLASS(SimulationRecord_SUB)::this
            Get_TimeSteps = this%TimeStep
    end function

    !*****************************************************
    subroutine Add_SimuTimes(this,increaseTime)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        real(kind=KINDDF),intent(in)::increaseTime
        !---Body---
        this%SimulationTimes = this%SimulationTimes + increaseTime

        return
    end subroutine Add_SimuTimes

   !********************************************************
   subroutine Set_SimuPatch(this,SimPath)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(in)::SimPath
        !---Body---

        this%SimulationPatch = SimPath

        return
   end subroutine Set_SimuPatch

   !********************************************************
   function Get_SimuPatch(this) result(SimPath)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(out)::SimPath
        !---Body---

        SimPath = this%SimulationPatch

        return
   end function Get_SimuPatch

    !***************************************************
    subroutine Set_TimeSections(this,TimeSection)
        implicit none
        CLASS(SimulationRecord_SUB)::this
        integer::TimeSection
        this%TimeSections = TimeSection
    end subroutine Set_TimeSections

    !***************************************************
    integer function Get_TimeSections(this)
        implicit none
        CLASS(SimulationRecord_SUB)::this
        Get_TimeSections = this%TimeSections
    end function Get_TimeSections


    !***************************************************
    subroutine Increase_OneTimeSection(this)
        implicit none
        CLASS(SimulationRecord_SUB)::this

        this%TimeSections = this%TimeSections + 1

        return
    end subroutine Increase_OneTimeSection

    !****************************************************
    real(kind=KINDDF) function Get_LastUpdateStatisTime(this)
        implicit none
        CLASS(SimulationRecord_SUB)::this

        Get_LastUpdateStatisTime = this%LastUpdateStatisTime
        return
    end function Get_LastUpdateStatisTime

    !***************************************************
    subroutine Set_LastUpdateStatisTime(this,TIME)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        real(kind=KINDDF),intent(in)::TIME
        !---Body---
        this%LastUpdateStatisTime = TIME

        return
    end subroutine Set_LastUpdateStatisTime

    !****************************************************
    function Get_LastUpdateNLTime(this) result(TheTime)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        real(kind=KINDDF),intent(out)::TheTime
        !---Body---
        TheTime = this%LastUpdateNLTime
        return
    end function Get_LastUpdateNLTime

    !****************************************************
    subroutine Set_LastUpdateNLTime(this,TheTime)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        real(kind=KINDDF),intent(in)::TheTime
        !---Body---
        this%LastUpdateNLTime = TheTime
        return
    end subroutine Set_LastUpdateNLTime

    !****************************************************
    function Get_LastUpdateNLNC0(this) result(NC0)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(out)::NC0
        !---Body---
        NC0 = this%LastUpdateNL_NC0
        return
    end function Get_LastUpdateNLNC0

    !****************************************************
    subroutine Set_LastUpdateNLNC0(this,NC0)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(in)::NC0
        !---Body---
        this%LastUpdateNL_NC0 = NC0

        return
    end subroutine Set_LastUpdateNLNC0

    !****************************************************
    subroutine RecordNC_ForSweepOut(this,MultiBox,TheBoxesBasicStatistic)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(in)::MultiBox
        type(BoxesBasicStatistic),intent(in)::TheBoxesBasicStatistic
        !---Local Vars---
        integer::IBox
        !---Body---
        DO IBox = 1,MultiBox
            this%RecordNCBeforeSweepOut_SingleBox(IBox,:) = TheBoxesBasicStatistic%BoxesStatis_Single(IBox)%NC + this%RecordNCBeforeSweepOut_SingleBox(IBox,:)
            this%RecordNCBeforeSweepOut_SingleBox(IBox,p_ACTIVEFREE_STATU) = TheBoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU)
            this%RecordNCBeforeSweepOut_SingleBox(IBox,p_ACTIVEINGB_STATU) = TheBoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEINGB_STATU)
        END DO

        this%RecordNCBeforeSweepOut_Integal= TheBoxesBasicStatistic%BoxesStatis_Integral%NC + this%RecordNCBeforeSweepOut_Integal
        this%RecordNCBeforeSweepOut_Integal(p_ACTIVEFREE_STATU) = TheBoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU)
        this%RecordNCBeforeSweepOut_Integal(p_ACTIVEINGB_STATU) = TheBoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEINGB_STATU)

        return
    end subroutine

    !****************************************************
    real function Get_LastRecordOutConfigTime(this)
        implicit none
        CLASS(SimulationRecord_SUB)::this

        Get_LastRecordOutConfigTime = this%LastRecordOutConfigTime
        return
    end function Get_LastRecordOutConfigTime

    !****************************************************
    subroutine Set_LastRecordOutConfigTime(this,TIME)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        real(kind=KINDDF),intent(in)::TIME
        !---Body---
        this%LastRecordOutConfigTime = TIME

        return
    end subroutine Set_LastRecordOutConfigTime

    !****************************************************
    integer function Get_OutPutIndex(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this

        Get_OutPutIndex = this%OutPutIndex

        return
    end function Get_OutPutIndex

    !****************************************************
    subroutine Set_OutPutIndex(this,OutIndex)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        integer,intent(in)::OutIndex

        this%OutPutIndex = OutIndex

        return
    end subroutine Set_OutPutIndex

    !****************************************************
    subroutine Increase_OneOutPutIndex(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this

        this%OutPutIndex = this%OutPutIndex + 1
        return
    end subroutine
    !*****The declare for abstract method***************
    !*****Note: this is not same with the "Fortran 95/2003 For Scientists and Engineers, Third Edition (chinese version)(P668)"
    !*****Because the abstract useage way in this book is not depended on PGFORTRAN compiler. The  following is based on our test
    !*****and verified to suit for pgfortran.
    subroutine DefCleanProc(this)
            implicit none
            CLASS(SimulationRecord_SUB)::this
    end subroutine

    subroutine DefCopyProc(this,Other)
        implicit none
        CLASS(SimulationRecord_SUB),intent(out)::this
        CLASS(SimulationRecord_SUB),intent(in)::Other
    end subroutine

    !******************************************************
    subroutine TurnOn_TriggerFocusedTimePoints(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this

        this%TriggerFocusedTimePoints = .true.
        return
    end subroutine


    !*******************************************************
    subroutine TurnOff_TriggerFocusedTimePoints(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this

        this%TriggerFocusedTimePoints = .false.

    end subroutine

    !*******************************************************
    integer function Get_StatusTriggerFocusedTimePoints(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this

        Get_StatusTriggerFocusedTimePoints = this%TriggerFocusedTimePoints

        return

    end function

    !***********************************************************
    subroutine CopySimulationRecordSUBFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB),intent(out)::this
        CLASS(SimulationRecord_SUB),intent(in)::Other
        !---Local Vars---
        integer::Size1
        integer::Size2
        !---Body---
        !---The Assignment(=) had been override
        this%Running_Record = Other%Running_Record

        this%SimulaitonSteps = Other%SimulaitonSteps
        this%SimulationPatch = Other%SimulationPatch
        this%SimulationTimes = Other%SimulationTimes
        this%TimeStep = Other%TimeStep
        this%TimeSections = Other%TimeSections

        this%LastUpdateStatisTime = Other%LastUpdateStatisTime

        this%LastUpdateNLTime = Other%LastUpdateNLTime
        this%LastUpdateNL_NC0 = Other%LastUpdateNL_NC0

        this%RecordNCBeforeSweepOut_Integal  = Other%RecordNCBeforeSweepOut_Integal

        if(allocated(this%RecordNCBeforeSweepOut_SingleBox)) deallocate(this%RecordNCBeforeSweepOut_SingleBox)
        if(allocated(Other%RecordNCBeforeSweepOut_SingleBox)) then
            Size1 = size(Other%RecordNCBeforeSweepOut_SingleBox,dim=1)
            Size2 = size(Other%RecordNCBeforeSweepOut_SingleBox,dim=2)
            if(Size1 .GT. 0 .AND. Size2 .GT. 0) then
                call AllocateArray_Host(this%RecordNCBeforeSweepOut_SingleBox,Size1,Size2,"RecordNCBeforeSweepOut_SingleBox")
                this%RecordNCBeforeSweepOut_SingleBox = Other%RecordNCBeforeSweepOut_SingleBox
            end if
        end if

        this%LastRecordOutConfigTime = Other%LastRecordOutConfigTime
        this%OutPutIndex = Other%OutPutIndex

        this%TriggerFocusedTimePoints = Other%TriggerFocusedTimePoints

        return
    end subroutine


    !***********************************************************
    subroutine Clean_SimulationRecordSUB(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord_SUB)::this
        !---Body---
        call this%Running_Record%Clean_RunningRecord()

        this%SimulaitonSteps = 0
        this%SimulationPatch = 1
        this%SimulationTimes = 0.D0
        this%TimeStep = 0.D0
        this%TimeSections = 1

        this%LastUpdateStatisTime = 0.D0

        this%LastUpdateNLTime = 0.D0
        this%LastUpdateNL_NC0 = 1

        this%RecordNCBeforeSweepOut_Integal = 0
        if(allocated(this%RecordNCBeforeSweepOut_SingleBox)) deallocate(this%RecordNCBeforeSweepOut_SingleBox)

        this%LastRecordOutConfigTime = 0.D0
        this%OutPutIndex = 0

        this%TriggerFocusedTimePoints = .false.

        return
    end subroutine

end module MCLIB_TYPEDEF_BASICRECORD_SUB

module MCLIB_TYPEDEF_BASICRECORD
    USE MCLIB_CONSTANTS
    USE MCLIB_UTILITIES
    USE MCLIB_TYPEDEF_BASICRECORD_SUB
    implicit none

    abstract interface
        subroutine UDefReadWriteRecord(hFile,Record,LINE)
            use MCLIB_TYPEDEF_BASICRECORD_SUB
            implicit none
            integer::hFile
            integer,optional::LINE
            CLASS(SimulationRecord_SUB)::Record
        end subroutine UDefReadWriteRecord

    end interface

    type,public::UDefReadWriteRecordList
        procedure(UDefReadWriteRecord),pointer,nopass::TheReadWriteProc=>null()

        type(UDefReadWriteRecordList),pointer::next=>null()

        integer,private::ListCount = 0

        contains

        procedure,non_overridable,public,pass::AppendOne=>Append_One

        procedure,non_overridable,public,pass::GetReadWriteRecordListCount=>Get_ReadWriteRecordListCount

        procedure,non_overridable,public,pass::CopyReadWriteRecordListFromOther

        procedure,non_overridable,public,pass::Clean_ReadWriteRecorList

        Generic::Assignment(=)=>CopyReadWriteRecordListFromOther

        !Final::CleanReadWriteRecorList

    end type UDefReadWriteRecordList

    type,public,extends(SimulationRecord_SUB)::SimulationRecord

        !*********User defined RecordListReadWrite****************************
        type(UDefReadWriteRecordList),private,pointer::UDefReadWriteRecord_List=>null()

        contains

        procedure,NON_OVERRIDABLE,public,pass::InitSimulationRecord

        procedure,non_overridable,public,pass::SetUDefReadWriteRecord_List=>Set_UDefReadWriteRecord_List
        procedure,non_overridable,public,pass::GetUDefReadWriteRecord_List=>Get_UDefReadWriteRecord_List

        !---abstract method---
        procedure,non_overridable,pass,public::TheDefCleanProc=>Clean_SimulationRecord
        procedure,non_overridable,pass,public::TheDefCopyProc=>CopySimulationRecordFromOther
        Generic::Assignment(=)=>CopySimulationRecordFromOther
        !---Based on our test, the final procedure cannot be applied in type who extended from other abstract one
        !Final:CleanSimulationRecord
    end type SimulationRecord

    private::Append_One
    private::Get_ReadWriteRecordListCount
    private::CopyReadWriteRecordListFromOther
    private::Clean_ReadWriteRecorList
    private::CleanReadWriteRecorList
    private::InitSimulationRecord
    private::Set_UDefReadWriteRecord_List
    private::Get_UDefReadWriteRecord_List
    private::Clean_SimulationRecord
    private::CopySimulationRecordFromOther

    contains

    !**************************************************************
    function Get_ReadWriteRecordListCount(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(UDefReadWriteRecordList),target::this
        integer::TheResult
        !---Body---

        TheResult = this%ListCount

        return
    end function Get_ReadWriteRecordListCount

    !**************************************************************
    subroutine Append_One(this,newOne)
        implicit none
        !---Dummy Vars---
        CLASS(UDefReadWriteRecordList),target::this
        type(UDefReadWriteRecordList)::newOne
        !---Local Vars---
        type(UDefReadWriteRecordList),pointer::cursor=>null(),cursorP=>null()

        !---Body---
        cursorP=>this

        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: You need to allocate the RecordList first!"
            pause
            stop
        end if

        if(this%GetReadWriteRecordListCount() .LE. 0) then
            this%ListCount = 1
            this%TheReadWriteProc => newOne%TheReadWriteProc
        else
            cursor=>this%next
            cursorP=>this

            DO While(associated(cursor))
                cursor=>cursor%next
                cursorP=>cursorP%next
            END DO

            this%ListCount = this%ListCount + 1

            allocate(cursor)
            Nullify(cursor%next)
            cursor%next=>null()
            cursor%TheReadWriteProc => newOne%TheReadWriteProc

            cursorP%next=>cursor
        end if

        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()

        return
    end subroutine Append_One

    !**************************************************************
    subroutine CopyReadWriteRecordListFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(UDefReadWriteRecordList),intent(out),target::this
        CLASS(UDefReadWriteRecordList),intent(in),target::other
        !---Local Vars---
        type(UDefReadWriteRecordList),pointer::thisCursorP=>null()
        type(UDefReadWriteRecordList),pointer::thisCursor=>null()
        type(UDefReadWriteRecordList),pointer::otherCursorP=>null()
        type(UDefReadWriteRecordList),pointer::otherCursor=>null()
        !---Body---
        thisCursorP=>this

        if(.not. associated(thisCursorP)) then
            write(*,*) "MCPSCUERROR: You need to allocate the RecordList first!"
            pause
            stop
        end if

        call this%Clean_ReadWriteRecorList()

        otherCursorP=>other

        if(.not. associated(otherCursorP)) then
            return
        end if

        if(otherCursorP%GetReadWriteRecordListCount() .LE. 0) then
            return
        end if

        thisCursorP%TheReadWriteProc = otherCursorP%TheReadWriteProc
        this%ListCount = this%ListCount + 1

        thisCursor=>thisCursorP%next
        otherCursor=>otherCursorP%next

        Do while(associated(otherCursor))
            allocate(thisCursor)

            thisCursor%TheReadWriteProc = otherCursor%TheReadWriteProc
            this%ListCount = this%ListCount + 1

            thisCursorP%next => thisCursor

            thisCursor => thisCursor%next
            otherCursor => otherCursor%next

            thisCursorP => thisCursorP%next
            otherCursorP => otherCursorP%next

        End Do

        nullify(thisCursor)
        thisCursor=>null()
        nullify(thisCursorP)
        thisCursorP=>null()
        nullify(otherCursor)
        otherCursor=>null()
        nullify(otherCursorP)
        otherCursorP=>null()

        return
    end subroutine

    !**************************************************************
    subroutine Clean_ReadWriteRecorList(this)
        implicit none
        !---Dummy Vars---
        CLASS(UDefReadWriteRecordList),target::this
        !---Local Vars---
        type(UDefReadWriteRecordList),pointer::cursor=>null(),next=>null()

        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        this%TheReadWriteProc=>null()

        DO While(associated(cursor))
            next=>cursor%next
            cursor%TheReadWriteProc=>null()
            Nullify(cursor)
            deallocate(cursor)
            cursor=>next
        END DO

        this%ListCount = 0

        this%next=>null()

        Nullify(cursor)
        cursor=>null()
        Nullify(next)
        next=>null()

        return
    end subroutine Clean_ReadWriteRecorList

    !**************************************************************
    subroutine CleanReadWriteRecorList(this)
        implicit none
        !---Dummy Vars---
        type(UDefReadWriteRecordList)::this
        !---Body---
        call this%Clean_ReadWriteRecorList()

        return
    end subroutine CleanReadWriteRecorList

    !******************************************************
    subroutine Set_UDefReadWriteRecord_List(this,TheUDefReadWriteRecord_List)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord)::this
        type(UDefReadWriteRecordList),target::TheUDefReadWriteRecord_List

        if(associated(this%UDefReadWriteRecord_List)) then
            call this%UDefReadWriteRecord_List%Clean_ReadWriteRecorList()
            deallocate(this%UDefReadWriteRecord_List)
        end if
        allocate(this%UDefReadWriteRecord_List)

        !---The Assignment had been override
        this%UDefReadWriteRecord_List = TheUDefReadWriteRecord_List

        return
    end subroutine


    !******************************************************
    function Get_UDefReadWriteRecord_List(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord)::this
        type(UDefReadWriteRecordList),pointer::TheResult

        TheResult=>null()

        if(associated(this%UDefReadWriteRecord_List)) then
            TheResult=>this%UDefReadWriteRecord_List
        end if

        return
    end function

    !**** type SimulationRecord*****************
    subroutine InitSimulationRecord(this,MultiBox,SimuSteps,SimuTimes,SimuPatchs,TimeSections)
        !---Dummy Vars---
        CLASS(SimulationRecord)::this
        integer,intent(in)::MultiBox
        integer,optional::SimuSteps
        real(kind=KINDDF),optional::SimuTimes
        integer,optional::SimuPatchs
        integer,optional::TimeSections
        !---Local Vars---
        integer::Steps
        real(kind=KINDDF)::Times
        integer::Patchs
        integer::TheTimeSection
        !---Body---
        Steps = 0
        Times = 0.D0
        Patchs = 1
        TheTimeSection = 1

        if(present(SimuSteps)) then
            Steps = SimuSteps
        end if

        if(present(SimuTimes)) then
            Times = SimuTimes
        end if

        if(present(SimuPatchs)) then
            Patchs = SimuPatchs
        end if

        if(present(TimeSections)) then
            TheTimeSection = TimeSections
        end if


        call this%InitSimulationRecord_SUB(MultiBox,SimuSteps=Steps,SimuTimes=Times,SimuPatchs=Patchs,TimeSections=TheTimeSection)

        allocate(this%UDefReadWriteRecord_List)

        return
    end subroutine InitSimulationRecord

    !***********************************************************
    subroutine CopySimulationRecordFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord),intent(out)::this
        CLASS(SimulationRecord),intent(in)::Other
        !---Body---
        !---The Assignment(=) had been override
        this%SimulationRecord_SUB = Other%SimulationRecord_SUB

        ! The Assignment(=) had been override
        if(associated(Other%UDefReadWriteRecord_List)) then
            if(.not. associated(this%UDefReadWriteRecord_List)) allocate(this%UDefReadWriteRecord_List)
            this%UDefReadWriteRecord_List = Other%UDefReadWriteRecord_List
        end if

        return
    end subroutine


    !***********************************************************
    subroutine Clean_SimulationRecord(this)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationRecord)::this
        !---Body---
        call this%SimulationRecord_SUB%Clean_SimulationRecordSUB()

        call this%UDefReadWriteRecord_List%Clean_ReadWriteRecorList()
        if(associated(this%UDefReadWriteRecord_List)) deallocate(this%UDefReadWriteRecord_List)
        Nullify(this%UDefReadWriteRecord_List)
        this%UDefReadWriteRecord_List=>null()

        return
    end subroutine

    !***********************************************************
    !subroutine CleanSimulationRecord(this)
    !    implicit none
    !    !---Dummy Vars---
    !    type(SimulationRecord)::this
    !    !---Body---
    !    call this%Clean_SimulationRecord()
    !
    !    return
    !end subroutine

end module

