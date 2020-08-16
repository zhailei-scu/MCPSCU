module MIGCOALE_TYPEDEF_SIMRECORD
    use MCLIB_TYPEDEF_BASICRECORD
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none

    type,public,extends(SimulationRecord)::MigCoalClusterRecord
        real(kind=KINDDF),private::StartImplantTime = 0.D0
        integer,private::ImplantedEntities = 0
        integer,private::LastRecordImplantNum = 0
        integer,private::NCUT = 0

        real(kind=KINDDF),private::LastUpdateAveSepTime = 0.D0

        integer,private::rescaleCount = 0

        logical,private::InsertOneBatchInNextStep = .false.
        integer,private::InsetedBatchNum = 0

        integer,private::SweepOutCount = 0

        integer::HSizeStatistic_TotalBox = 0
        integer::HSizeStatistic_EachBox = 0
        real(kind=KINDDF),private::LastOutSizeDistTime_IntegralBox = 0.D0
        real(kind=KINDDF),private::LastOutSizeDistTime_EachBox = 0.D0

        integer,public::RandSeed_OutDevWalk(2) = 0

        integer,public::RandSeed_InnerDevWalk(2) = 0

        integer,public::RandSeed_Reaction(2) = 0

        integer,public::RandSeed_SpaceDist_Implant_Layer(2) = 0

        integer,public::RandSeed_SpaceDist_Implant_X(2) = 0

        integer,public::RandSeed_SpaceDist_Implant_Y(2) = 0

        integer,public::RandSeed_SpaceDist_Implant_Z(2) = 0

        integer,public::RandSeed_SizeDist_Implant(2) = 0

        contains
        procedure,NON_OVERRIDABLE,public,pass::InitMigCoalClusterRecord
        procedure,non_overridable,public,pass::SetStartImplantTime=>Set_StartImplantTime
        procedure,non_overridable,public,pass::GetStartImplantTime=>Get_StartImplantTime
        procedure,non_overridable,public,pass::GetLastUpdateAveSepTime=>Get_LastUpdateAveSepTime
        procedure,non_overridable,public,pass::SetLastUpdateAveSepTime=>Set_LastUpdateAveSepTime
        procedure,non_overridable,public,pass::IncreaseOneRescaleCount=>Increase_OneRescaleCount
        procedure,non_overridable,public,pass::GetRescaleCount=>Get_RescaleCount
        procedure,non_overridable,public,pass::SetTrue_InsertOneBatchInNextStep
        procedure,non_overridable,public,pass::SetFalse_InsertOneBatchInNextStep
        procedure,non_overridable,public,pass::GetStatu_InsertOneBatchInNextStep
        procedure,non_overridable,public,pass::InCrease_OneInsertBatchNum
        procedure,non_overridable,public,pass::Get_InsertBatchNum
        procedure,non_overridable,public,pass::Set_InsertBatchNum
        procedure,non_overridable,public,pass::IncreaseOneSweepOutCount=>Increase_OneSweepOutCount
        procedure,non_overridable,public,pass::GetSweepOutCount=>Get_SweepOutCount
        procedure,NON_OVERRIDABLE,public,pass::AddImplantedEntitiesNum=>Add_ImplantedEntitiesNum
        procedure,NON_OVERRIDABLE,public,pass::GetImplantedEntitiesNum=>Get_ImplantedEntitiesNum
        procedure,NON_OVERRIDABLE,public,pass::SetImplantedEntitiesNum=>Set_ImplantedEntitiesNum
        procedure,NON_OVERRIDABLE,public,pass::SetLastRecordImplantNum=>Set_LastRecordImplantNum
        procedure,NON_OVERRIDABLE,public,pass::GetLastRecordImplantNum=>Get_LastRecordImplantNum
        procedure,NON_OVERRIDABLE,public,pass::SetNCUT=>Set_NCUT
        procedure,NON_OVERRIDABLE,public,pass::GetNCUT=>Get_NCUT

        procedure,NON_OVERRIDABLE,public,pass::SetLastOutSizeDistTime_IntegralBox
        procedure,NON_OVERRIDABLE,public,pass::GetLastOutSizeDistTime_IntegralBox
        procedure,NON_OVERRIDABLE,public,pass::SetLastOutSizeDistTime_EachBox
        procedure,NON_OVERRIDABLE,public,pass::GetLastOutSizeDistTime_EachBox
        procedure,non_overridable,public,pass::WhetherOutSizeDist_IntegralBox
        procedure,non_overridable,public,pass::WhetherOutSizeDist_EachBox
        procedure,non_overridable,pass,private::CopyMigCoalClusterRecordFromOther
        Generic::Assignment(=)=>CopyMigCoalClusterRecordFromOther
        procedure,non_overridable,pass,private::Clean_MigCoalClusterRecord
        !---Based on our test, the final procedure cannot be applied in type who extended from other abstract one
        Final::CleanMigCoalClusterRecord

    end type MigCoalClusterRecord

    private::InitMigCoalClusterRecord
    private::Set_StartImplantTime
    private::Get_StartImplantTime
    private::Set_LastUpdateAveSepTime
    private::Get_LastUpdateAveSepTime
    private::Increase_OneRescaleCount
    private::Get_RescaleCount
    private::SetTrue_InsertOneBatchInNextStep
    private::SetFalse_InsertOneBatchInNextStep
    private::GetStatu_InsertOneBatchInNextStep
    private::InCrease_OneInsertBatchNum
    private::Get_InsertBatchNum
    private::Set_InsertBatchNum
    private::Increase_OneSweepOutCount
    private::Get_SweepOutCount
    private::Add_ImplantedEntitiesNum
    private::Get_ImplantedEntitiesNum
    private::Set_ImplantedEntitiesNum
    private::Set_LastRecordImplantNum
    private::Get_LastRecordImplantNum
    private::Set_NCUT
    private::Get_NCUT
    private::SetLastOutSizeDistTime_IntegralBox
    private::GetLastOutSizeDistTime_IntegralBox
    private::SetLastOutSizeDistTime_EachBox
    private::GetLastOutSizeDistTime_EachBox
    private::WhetherOutSizeDist_IntegralBox
    private::WhetherOutSizeDist_EachBox
    private::CopyMigCoalClusterRecordFromOther
    private::Clean_MigCoalClusterRecord
    private::CleanMigCoalClusterRecord

    contains

    subroutine UDefReadWriteRecord_BatchNum(hFile,Record,LINE)
        !use MCLIB_TYPEDEF_BASICRECORD
        implicit none
        integer::hFile
        type(MigCoalClusterRecord)::Record
        integer,optional::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*32::STRTMP(20)
        integer::N
        !---Body---
        if(present(LINE)) then    ! Read file
            DO While(.true.)
                call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
                call RemoveComments(STR,"!")

                call GETKEYWORD("&",STR,KEYWORD)

                STR = adjustl(STR)

                call UPCASE(KEYWORD)

                select case(KEYWORD(1:LENTRIM(KEYWORD)))
                    case("&ENDUDEFSECTION")
                        exit

                    case("&BATCHNUM")
                        call EXTRACT_NUMB(STR,1,N,STRTMP)

                        if(N .LT. 1) then
                            write(*,*) "MCPSCUERROR: Too few parameters for &BATCHNUM setting"
                            write(*,*) "You must special the inputted batchNum"
                            pause
                            stop
                        end if

                        call Record%Set_InsertBatchNum(ISTR(STRTMP(1)))

                        if(Record%Get_InsertBatchNum() .LT. 0) then
                            write(*,*) "MCPSCUERROR: The total insert batch number cannot less than 0"
                            write(*,*) Record%Get_InsertBatchNum()
                            pause
                            stop
                        end if

                    case default
                        write(*,*) "MCPSCUERROR: Unknown flags: ",KEYWORD(1:LENTRIM(KEYWORD))
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                end select
            END DO


        else                      ! Write to file
            KEYWORD = "&BATCHNUM"
            write(hFile, FMT="(A,1x,I15)") KEYWORD(1:LENTRIM(KEYWORD)),Record%Get_InsertBatchNum()
        end if

        return
        100 write(*,*) "MCPSCUERROR: Error to load simulation record "
            write(*,*) "At line: ",LINE
            write(*,*) STR
            pause
            stop
    end subroutine UDefReadWriteRecord_BatchNum

    !***********type MigCoalClusterRecord *****************
    subroutine InitMigCoalClusterRecord(this,MultiBox,SimuSteps,SimuTimes,SimuPatchs,TimeSection)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        integer,intent(in)::MultiBox
        integer,optional::SimuSteps
        real,optional::SimuTimes
        integer,optional::SimuPatchs
        integer,optional::TimeSection
        !---Local Vars---
        integer::Steps
        real(kind=KINDDF)::Times
        integer::Patchs
        integer::TheTimeSection
        type(UDefReadWriteRecordList)::tempUDefReadWriteRecordList
        !---Body-- -::
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

        if(present(TimeSection)) then
            TheTimeSection = TimeSection
        end if

        this%LastUpdateAveSepTime = 0.D0

        this%rescaleCount = 0

        this%SweepOutCount = 0

        this%LastOutSizeDistTime_IntegralBox = 0.D0
        this%LastOutSizeDistTime_EachBox = 0.D0

        call this%InitSimulationRecord(MultiBox,SimuSteps=Steps,SimuTimes=Times,SimuPatchs=Patchs,TimeSections=TheTimeSection)

        this%StartImplantTime = 0

        this%ImplantedEntities = 0

        this%LastRecordImplantNum = 0

        this%NCUT = 0

        this%RandSeed_OutDevWalk = 0

        this%RandSeed_InnerDevWalk = 0

        this%RandSeed_Reaction = 0

        this%RandSeed_SpaceDist_Implant_Layer = 0

        this%RandSeed_SpaceDist_Implant_X = 0

        this%RandSeed_SpaceDist_Implant_Y = 0

        this%RandSeed_SpaceDist_Implant_Z = 0

        this%RandSeed_SizeDist_Implant = 0

        tempUDefReadWriteRecordList%TheReadWriteProc=>UDefReadWriteRecord_BatchNum

        call this%GetUDefReadWriteRecord_List()%AppendOne(tempUDefReadWriteRecordList)

        return
    end subroutine InitMigCoalClusterRecord

    subroutine Set_StartImplantTime(this,TheTime)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(in)::TheTime
        !---Body---
        this%StartImplantTime = TheTime
        return
    end subroutine Set_StartImplantTime

    function Get_StartImplantTime(this) result(TheTime)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(out)::TheTime
        !---Body---
        TheTime = this%StartImplantTime
        return
    end function Get_StartImplantTime

    subroutine Add_ImplantedEntitiesNum(this,AddNum)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord)::this
        integer, intent(in)::AddNum
        !---Body---
        if(AddNum .LT. 0) then
            write(*,*) "MCPSCUERROR: The new implanted clusters number is not possible less than 0 :",AddNum
            pause
            stop
        end if

        this%ImplantedEntities = this%ImplantedEntities + AddNum

        return
    end subroutine Add_ImplantedEntitiesNum

    integer function Get_ImplantedEntitiesNum(this)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this

        Get_ImplantedEntitiesNum = this%ImplantedEntities
        return
    end function Get_ImplantedEntitiesNum

    subroutine Set_ImplantedEntitiesNum(this,TheNum)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        integer,intent(in)::TheNum
        !---Body---
        this%ImplantedEntities = TheNum

        return
    end subroutine

    integer function Get_LastRecordImplantNum(this)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this

        Get_LastRecordImplantNum = this%LastRecordImplantNum
        return
    end function Get_LastRecordImplantNum

    subroutine Set_LastRecordImplantNum(this,TheNum)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        integer,intent(in)::TheNum
        !---Body---
        this%LastRecordImplantNum = TheNum

        return
    end subroutine Set_LastRecordImplantNum

    integer function Get_NCUT(this)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this

        Get_NCUT = this%NCUT
        return
    end function Get_NCUT

    subroutine Set_NCUT(this,TheNCUT)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        integer,intent(in)::TheNCUT
        !---Body---
        this%NCUT = TheNCUT

        return
    end subroutine Set_NCUT

    subroutine SetLastOutSizeDistTime_IntegralBox(this,TheTime)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(in)::TheTime
        !---Body---
        this%LastOutSizeDistTime_IntegralBox = TheTime

        return
    end subroutine SetLastOutSizeDistTime_IntegralBox

    function GetLastOutSizeDistTime_IntegralBox(this) result(TheTime)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(out)::TheTime
        !---Body---
        TheTime = this%LastOutSizeDistTime_IntegralBox

        return
    end function GetLastOutSizeDistTime_IntegralBox

    subroutine SetLastOutSizeDistTime_EachBox(this,TheTime)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(in)::TheTime
        !---Body---
        this%LastOutSizeDistTime_EachBox = TheTime

        return
    end subroutine SetLastOutSizeDistTime_EachBox

    function GetLastOutSizeDistTime_EachBox(this) result(TheTime)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(out)::TheTime
        !---Body---
        TheTime = this%LastOutSizeDistTime_EachBox

        return
    end function GetLastOutSizeDistTime_EachBox

    !******************************************
    function WhetherOutSizeDist_IntegralBox(this,Host_SimuCtrlParam) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        logical,intent(inout)::TheResult
        !---Body---
        TheResult = .false.

        if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalSteps) then
            if((this%GetSimuSteps() - this%GetLastOutSizeDistTime_IntegralBox()) .GE. Host_SimuCtrlParam%OutPutSCValue_IntegralBox) then
                TheResult = .true.
            end if

        else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalRealTime) then
            if((this%GetSimuTimes() - this%GetLastOutSizeDistTime_IntegralBox()) .GE. Host_SimuCtrlParam%OutPutSCValue_IntegralBox) then
                TheResult = .true.
            end if

        else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalTimeMagnification) then
            if((this%GetSimuTimes()/Host_SimuCtrlParam%OutPutSCValue_IntegralBox) .GE. this%GetLastOutSizeDistTime_IntegralBox()) then
                TheResult = .true.
            end if
        end if

        return
    end function WhetherOutSizeDist_IntegralBox


    !******************************************
    function WhetherOutSizeDist_EachBox(this,Host_SimuCtrlParam) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(MigCoalClusterRecord)::this
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        logical,intent(inout)::TheResult
        !---Body---
        TheResult = .false.

        if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalSteps) then
            if((this%GetSimuSteps() - this%GetLastOutSizeDistTime_EachBox()) .GE. Host_SimuCtrlParam%OutPutSCValue_EachBox) then
                TheResult = .true.
            end if

        else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalRealTime) then
            if((this%GetSimuTimes() - this%GetLastOutSizeDistTime_EachBox()) .GE. Host_SimuCtrlParam%OutPutSCValue_EachBox) then
                TheResult = .true.
            end if

        else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalTimeMagnification) then
            if((this%GetSimuTimes()/Host_SimuCtrlParam%OutPutSCValue_EachBox) .GE. this%GetLastOutSizeDistTime_EachBox()) then
                TheResult = .true.
            end if
        end if

        return
    end function WhetherOutSizeDist_EachBox

    !**************************************************************
    subroutine Clean_MigCoalClusterRecord(this)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord)::this
        !---Body---
        call this%SimulationRecord%TheDefCleanProc()

        this%StartImplantTime = 0.D0
        this%ImplantedEntities = 0
        this%LastRecordImplantNum = 0
        this%NCUT = 0

        this%LastUpdateAveSepTime = 0.D0

        this%rescaleCount = 0

        this%InsertOneBatchInNextStep = .false.
        this%InsetedBatchNum = 0

        this%SweepOutCount = 0

        this%HSizeStatistic_TotalBox = 0
        this%HSizeStatistic_EachBox = 0
        this%LastOutSizeDistTime_IntegralBox = 0.D0
        this%LastOutSizeDistTime_EachBox = 0.D0

        this%RandSeed_OutDevWalk = 0

        this%RandSeed_InnerDevWalk = 0

        this%RandSeed_Reaction = 0

        this%RandSeed_SpaceDist_Implant_Layer = 0

        this%RandSeed_SpaceDist_Implant_X = 0

        this%RandSeed_SpaceDist_Implant_Y = 0

        this%RandSeed_SpaceDist_Implant_Z = 0

        this%RandSeed_SizeDist_Implant = 0

        return
    end subroutine

    !**************************************************************
    subroutine CopyMigCoalClusterRecordFromOther(this,Other)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord),intent(out)::this
        CLass(MigCoalClusterRecord),intent(in)::Other
        !---Body---
        !---The Assignment(=) had been override
        this%SimulationRecord = Other%SimulationRecord

        this%StartImplantTime = Other%StartImplantTime
        this%ImplantedEntities = Other%ImplantedEntities
        this%LastRecordImplantNum = Other%LastRecordImplantNum
        this%NCUT = Other%NCUT

        this%LastUpdateAveSepTime = Other%LastUpdateAveSepTime

        this%rescaleCount = Other%rescaleCount

        this%InsertOneBatchInNextStep = Other%InsertOneBatchInNextStep
        this%InsetedBatchNum = Other%InsetedBatchNum

        this%SweepOutCount = Other%SweepOutCount

        this%HSizeStatistic_TotalBox = Other%HSizeStatistic_TotalBox
        this%HSizeStatistic_EachBox = Other%HSizeStatistic_EachBox
        this%LastOutSizeDistTime_IntegralBox = Other%LastOutSizeDistTime_IntegralBox
        this%LastOutSizeDistTime_EachBox = Other%LastOutSizeDistTime_EachBox

        this%RandSeed_OutDevWalk = Other%RandSeed_OutDevWalk

        this%RandSeed_InnerDevWalk = Other%RandSeed_InnerDevWalk

        this%RandSeed_Reaction = Other%RandSeed_Reaction

        this%RandSeed_SpaceDist_Implant_Layer = Other%RandSeed_SpaceDist_Implant_Layer

        this%RandSeed_SpaceDist_Implant_X = Other%RandSeed_SpaceDist_Implant_X

        this%RandSeed_SpaceDist_Implant_Y = Other%RandSeed_SpaceDist_Implant_Y

        this%RandSeed_SpaceDist_Implant_Z = Other%RandSeed_SpaceDist_Implant_Z

        this%RandSeed_SizeDist_Implant = Other%RandSeed_SizeDist_Implant
        return
    end subroutine

    !**************************************************************
    subroutine CleanMigCoalClusterRecord(this)
        implicit none
        !---Dummy Vars---
        type(MigCoalClusterRecord)::this

        call this%Clean_MigCoalClusterRecord()
        return
    end subroutine

    !**************************************************************
    subroutine Increase_OneRescaleCount(this)
        implicit none
        Class(MigCoalClusterRecord)::this

        this%rescaleCount = this%rescaleCount + 1
        return
    end subroutine Increase_OneRescaleCount

    !**************************************************************
    function Get_RescaleCount(this) result(rescaleCount)
        implicit none
        Class(MigCoalClusterRecord)::this
        integer::rescaleCount

        rescaleCount = this%rescaleCount
        return
    end function Get_RescaleCount

    !**************************************************************
    subroutine Increase_OneSweepOutCount(this)
        implicit none
        Class(MigCoalClusterRecord)::this

        this%SweepOutCount = this%SweepOutCount + 1
        return
    end subroutine Increase_OneSweepOutCount


    !*************************************************************
    subroutine SetTrue_InsertOneBatchInNextStep(this)
        implicit none
        Class(MigCoalClusterRecord)::this

        this%InsertOneBatchInNextStep = .true.
    end subroutine

    !*************************************************************
    subroutine SetFalse_InsertOneBatchInNextStep(this)
        implicit none
        Class(MigCoalClusterRecord)::this

        this%InsertOneBatchInNextStep = .false.
    end subroutine

    !*************************************************************
    function GetStatu_InsertOneBatchInNextStep(this) result(TheResult)
        implicit none
        Class(MigCoalClusterRecord)::this
        logical::TheResult


        TheResult = this%InsertOneBatchInNextStep
        return
    end function GetStatu_InsertOneBatchInNextStep

    !*************************************************************
    subroutine InCrease_OneInsertBatchNum(this)
        implicit none
        Class(MigCoalClusterRecord)::this

        this%InsetedBatchNum = this%InsetedBatchNum + 1
        return
    end subroutine

    !*************************************************************
    function Get_InsertBatchNum(this) result(TheResult)
        implicit none
        Class(MigCoalClusterRecord)::this
        integer::TheResult

        TheResult = this%InsetedBatchNum
        return
    end function

    !*************************************************************
    subroutine Set_InsertBatchNum(this,TheBatchNum)
        implicit none
        Class(MigCoalClusterRecord)::this
        integer::TheBatchNum

        this%InsetedBatchNum = TheBatchNum
    end subroutine

    !**************************************************************
    function Get_SweepOutCount(this) result(SweepOutCount)
        implicit none
        Class(MigCoalClusterRecord)::this
        integer::SweepOutCount

        SweepOutCount = this%SweepOutCount
        return
    end function Get_SweepOutCount

    !*******************************************************
    subroutine Set_LastUpdateAveSepTime(this,TheTime)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(in)::TheTime
        !---Body---
        this%LastUpdateAveSepTime = TheTime
        return
    end subroutine Set_LastUpdateAveSepTime

    function Get_LastUpdateAveSepTime(this) result(TheTime)
        implicit none
        !---Dummy Vars---
        CLass(MigCoalClusterRecord)::this
        real(kind=KINDDF),intent(out)::TheTime
        !---Body---
        TheTime = this%LastUpdateAveSepTime
        return
    end function Get_LastUpdateAveSepTime

end module
