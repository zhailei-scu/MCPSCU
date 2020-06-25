module MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
  !*** Description: This module is created for the control parameters in simulation
  !
  !    HISTORY: Created by Zhai Lei in May, 2018
  !
  !    Reference: MDPSCU module MD_TYPEDEF_SIMULATIONCTRLPARAM
  use MCLIB_CONSTANTS
  use MCLIB_UTILITIES
  use MSM_TYPEDEF_InputPaser
  use MiniUtilities, only:EXTRACT_NUMB,GETINPUTSTRLINE,GETKEYWORD,UPCASE,DRSTR,ISTR

  implicit none

  character(len=5), parameter, private::m_CTLSTARTFLAG = "&CTLF"

  type,public::SimulationCtrlParam

     !***Run status
     integer::RESTARTAT = 0                                             !=0,   start a new session
                                                                        !=-1,  restart from the end point of the previous calculation
                                                                        !=xxx(>0), restart from a xxx time step of the previouss calculation
     !***Information about simulation boxs of jobs
     integer::MultiBox = 1                                              ! the number of simulation boxs in one run
     integer::TOTALBOX = 1                                              ! the total number of indentical boxes, this variable would be used in GPU computing
     integer::INDEPBOX = 1                                              ! if all the box are independent

     !***Information for random number
     integer(kind=KINDDF)::RANDSEED(2) = (/43434, 54454532/)             ! the inputed random seed

     !***PERIOD boundary************
     integer::PERIOD(3) = (/1,1,1/)                                     ! determine if PERIOD condition used

     !***Inforamtion about neighborlist
     integer::NEIGHBORCALWAY = mp_CalcNeighborList_NNEAREST             ! 1 for NNearest way, 2 for cut-off radium way
     integer::MAXNEIGHBORNUM   = 2048                                   ! the max neighbor number for a cluster while searching neighbor-list
     real(kind=KINDDF)::CUTREGIONEXTEND = 8                              ! the parameter determine the neighbor region extend
     integer::NEIGHBORUPDATESTRATEGY = mp_NEIGHBORUPDATEBYSTEP          ! flag = 0 for output each interval steps,flag = 1 for by remind clusters number percent
     real(kind=KINDDF)::NEIGHBORUPDATE = 10                              ! if number of particles decreases to this value, neigbore list should be update

     !***Informaiton about temperature
     real(kind=KINDDF)::TEMP = 300.D0                                    ! temperature
     real(kind=KINDDF)::TKB = 300D0*C_KB                                 ! the kinetic energy

     !***Implantation section*********
     integer::ImplantSectID = 0                                         ! the implantation section index

     !***Information about Time
     integer::TermTFlag = mp_TermTimeFlag_ByRealTime                    ! = 0 stans for by steps,flag = 1 by time(s)
     real::TermTValue = 3000                                            ! terminate time

     integer::NFocusedTimePoint = 0
     real(kind=KINDDF),dimension(:),allocatable::FocusedTimePoints      ! the focused time points


     integer::UPDATETSTEPSTRATEGY = mp_SelfAdjustlStep_NearestSep       ! flag = 0 the time step is determined by average distance of nearest cluster
                                                                        ! flag = 1 is by fixed time-step
                                                                        ! flag = 2 the time step is determined by volume average distance and suppose the clusters distribute uniform in the box
     real::FixedTimeStepValue = 1                                       ! Fixed step time length for mp_FixedTimeStep strategy
     real::EnlageTStepScale = 0.01                                      ! adjustl time-step enlarge-scale mp_SelfAdjustlStep_NearestSep strategy and mp_SelfAdjustlStep_AveSep
     real::LowerLimitTime = 3.68D-13                                    ! The lower limit time of NDDR algorithm
     real::LowerLimitLength = 2.74D-8                                   ! The lower limit length of NDDR_S4 algorithm
     integer::LastPassageFactor = 5                                     ! The last passage factor

     integer::TUpdateStatisFlag = mp_UpdateStatisFlag_ByIntervalSteps   ! flag = 0 for output each interval steps,flag = 1 for output each interval time(s)
     real::TUpdateStatisValue = 10                                      ! the time interval to update statistic

     integer::OutPutConfFlag = mp_OutTimeFlag_ByIntervalSteps           ! flag = 0 for output each interval steps,flag = 1 for output each interval time(s), flag = 2 by magnification
     real::OutPutConfValue = 100                                        ! output configuration interval value

     logical::OutPutConfContent(p_NUMBER_OF_STATU) = .true.             ! to determine which status of clusters need to be output

     logical::OutPutConf_SweepOut = .true.

     integer::OutPutSCFlag = mp_OutTimeFlag_ByIntervalSteps             ! flag = 0 for output each interval steps,flag = 1 for output each interval time(s), flag = 2 by magnification
     real::OutPutSCValue_IntegralBox = 100                              ! output statistic configuration interval value(for integral box)
     real::OutPutSCValue_EachBox = 100                                  ! output statistic configuration interval value(for integral box)

     integer::OutPutFuncSFlag = mp_OutTimeFlag_ByIntervalSteps          ! flag = 0 for output each interval steps,flag = 1 for output each interval time(s), flag = 2 by magnification
     real::OutPutFuncSValue = 100                                       ! output function statistic interval value

     integer::OutPutSwapFlag = mp_OutTimeFlag_ByIntervalSteps           ! flag = 0 for output each interval steps,flag = 1 for output each interval time(s), flag = 2 by magnification
     real::OutPutSwapValue = 100                                        ! output swap value interval value

     !*****restored statments for special problem*************
     type(Statementlist),pointer::AddOnData=>null()
     type(Statementlist),pointer::ModelData=>null()

     !***File path and name restore*****
     character*1000::InputFilePath = ""
     character*1000::InputFileshortName = ""
     character*1000::IniConfig = ""
     character*1000::ImpFile = ""
     character*1000::OutFilePath = ""
     character*1000::RestartCfg = ""

     !*********Parameters for analysis processes*************
     integer::STARTJOB = -1
     integer::ENDJOB = -1
     integer::JOBSTEP = 0
     integer::STARTTSECTION = -1
     integer::ENDTSECTION = -1
     integer::TSECTIONSTEP = 0
     integer::STARTCFG = -1
     integer::ENDCFG = -1
     integer::CFGSTEP = 0
     integer::STARTBOX = -1
     integer::ENDBOX = -1
     integer::BOXSTEP = 0

     !---Determine whether the reaction are considered
     logical::FreeDiffusion = .false.

     !********************************************
     type(SimulationCtrlParam),pointer::next=>null()

     contains
     procedure,non_overridable,pass,public::AppendOne_SimulationCtrlParam
     procedure,non_overridable,pass,public::Get_P=>GetSimulationCtrlParam_P
     procedure,non_overridable,pass,private::CopyFromOther
     procedure,non_overridable,nopass,private::CleanSimulationCtrlParam
     procedure,non_overridable,pass,public::DefaultValue_CtrlParam
     procedure,non_overridable,pass,public::Load_Ctrl_Parameters
     procedure,non_overridable,pass,public::Print_CtrlParameters
     procedure,non_overridable,pass,private::Load_Ctrl_CommParameter
     procedure,non_overridable,pass,private::Load_Ctrl_AnalyParameter
     procedure,non_overridable,pass,private::Load_Ctrl_SectionParameter
     procedure,non_overridable,pass,private::Load_Ctrl_Temperature
     procedure,non_overridable,pass,private::Load_Ctrl_Boundary
     procedure,non_overridable,pass,private::Load_Ctrl_NeighborList
     procedure,non_overridable,pass,private::Load_Ctrl_Implant
     procedure,non_overridable,pass,private::Load_Ctrl_TimeStep
     procedure,non_overridable,pass,private::Load_AddOnDataStatments
     procedure,non_overridable,pass,private::Load_ModelDataStatments
     Generic::Assignment(=)=>CopyFromOther
     Final::Clean

  end type

  private::AppendOne_SimulationCtrlParam
  private::GetSimulationCtrlParam_P
  private::CopyFromOther
  private::CleanOneSimulationCtrlParam
  private::CleanSimulationCtrlParam
  private::DefaultValue_CtrlParam
  private::Load_Ctrl_Parameters
  private::Print_CtrlParameters
  private::Load_Ctrl_CommParameter
  private::Load_Ctrl_AnalyParameter
  private::Load_Ctrl_SectionParameter
  private::Load_Ctrl_Temperature
  private::Load_Ctrl_Boundary
  private::Load_Ctrl_NeighborList
  private::Load_Ctrl_Implant
  private::Load_Ctrl_TimeStep
  private::Load_AddOnDataStatments
  private::Load_ModelDataStatments
  private::Clean


  contains

  !***************************************
  subroutine AppendOne_SimulationCtrlParam(this,newOne)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),target::this
    type(SimulationCtrlParam)::newOne
    !---Local Vars---
    type(SimulationCtrlParam),pointer::cursor=>null(),cursorP=>null()
    !---Body---
    cursor=>this%next
    cursorP=>this
    DO while(associated(cursor))
       cursor=>cursor%next
       cursorP=>cursorP%next
    END DO

    allocate(cursor)
    ! The assignment(=) had been overrided
    cursor = newOne
    cursorP%next=>cursor
    return
  end subroutine AppendOne_SimulationCtrlParam


  !***********************************************************************
  function GetSimulationCtrlParam_P(this,TheIndex) result(TheResult)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),target::this
    integer,intent(in)::TheIndex
    type(SimulationCtrlParam),intent(out),pointer::TheResult
    !---Local Vars---
    type(SimulationCtrlParam),pointer::cursor=>null()
    integer::CountTemp
    !---Body---

    TheResult=>null()

    cursor=>this

    CountTemp = 0

    DO While(associated(cursor))

        CountTemp = CountTemp + 1

        if(CountTemp .eq. TheIndex) then
            TheResult=>cursor
            exit
        end if

        cursor=>cursor%next
    END DO

    Nullify(cursor)

    if(.not. associated(TheResult)) then
        write(*,*) "MCPSCUERROR: Cannot find the simulation control section by the id: ",TheIndex
        pause
        stop
    end if

    return
  end function GetSimulationCtrlParam_P


  !****************************************************************
  subroutine CopyFromOther(this,otherOne)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),intent(out)::this
    type(SimulationCtrlParam), intent(in)::otherOne
    !---Body----

    this%RESTARTAT = otherOne%RESTARTAT

    !***Information about simulation boxs of jobs
    this%MultiBox = otherOne%MultiBox
    this%TOTALBOX = otherOne%TOTALBOX
    this%INDEPBOX = otherOne%INDEPBOX

    !***Information for random number
    this%RANDSEED = otherOne%RANDSEED

    !***PERIOD boundary************
    this%PERIOD = otherOne%PERIOD

    !***Inforamtion about neighborlist
    this%NEIGHBORCALWAY = otherOne%NEIGHBORCALWAY
    this%MAXNEIGHBORNUM   = otherOne%MAXNEIGHBORNUM
    this%NEIGHBORUPDATESTRATEGY = otherOne%NEIGHBORUPDATESTRATEGY
    this%CUTREGIONEXTEND = otherOne%CUTREGIONEXTEND
    this%NEIGHBORUPDATE = otherOne%NEIGHBORUPDATE

    !***Informaiton about temperature
    this%TEMP = otherOne%TEMP
    this%TKB = otherOne%TKB

    !***Implantation sectin*********
    this%ImplantSectID = otherOne%ImplantSectID

    !***Information about Time
    this%TermTFlag = otherOne%TermTFlag
    this%TermTValue = otherOne%TermTValue

    !***Focused time points**************
    this%NFocusedTimePoint = otherOne%NFocusedTimePoint

    if(allocated(this%FocusedTimePoints)) deallocate(this%FocusedTimePoints)
    if(allocated(otherOne%FocusedTimePoints)) then
        if(size(otherOne%FocusedTimePoints) .GT. 0) then

            if(size(otherOne%FocusedTimePoints) .ne. otherOne%NFocusedTimePoint) then
                write(*,*) "MCPSCURROR: It is seems like that the dimension of FocusedTimePoints is: ", size(otherOne%FocusedTimePoints)
                write(*,*) "Bu the NFocusedTimePoint is : ",otherOne%NFocusedTimePoint
                pause
                stop
            end if

            allocate(this%FocusedTimePoints(otherOne%NFocusedTimePoint))
            this%FocusedTimePoints = otherOne%FocusedTimePoints
        end if
    else if(otherOne%NFocusedTimePoint .GT. 0) then
        write(*,*) "MCPSCUERROR: have not allocate the FocusedTimePoints, but NFocusedTimePoint greater than 0"
        write(*,*) otherOne%NFocusedTimePoint
        pause
        stop
    end if

    !******************
    this%UPDATETSTEPSTRATEGY = otherOne%UPDATETSTEPSTRATEGY
    this%FixedTimeStepValue = otherOne%FixedTimeStepValue
    this%EnlageTStepScale = otherOne%EnlageTStepScale
    this%LowerLimitTime = otherOne%LowerLimitTime
    this%LowerLimitLength = otherOne%LowerLimitLength
    this%LastPassageFactor = otherOne%LastPassageFactor

    this%TUpdateStatisFlag = otherOne%TUpdateStatisFlag
    this%TUpdateStatisValue = otherOne%TUpdateStatisValue

    this%OutPutConfFlag = otherOne%OutPutConfFlag
    this%OutPutConfValue = otherOne%OutPutConfValue

    this%OutPutConfContent = otherOne%OutPutConfContent
    this%OutPutConf_SweepOut = otherOne%OutPutConf_SweepOut

    this%OutPutSCFlag = otherOne%OutPutSCFlag
    this%OutPutSCValue_IntegralBox = otherOne%OutPutSCValue_IntegralBox
    this%OutPutSCValue_EachBox = otherOne%OutPutSCValue_EachBox

    this%OutPutFuncSFlag = otherOne%OutPutFuncSFlag
    this%OutPutFuncSValue = otherOne%OutPutFuncSValue

    this%OutPutSwapFlag = otherOne%OutPutSwapFlag
    this%OutPutSwapValue = otherOne%OutPutSwapValue

    !***File path and name restore*****
    this%InputFilePath = otherOne%InputFilePath
    this%InputFileshortName = otherOne%InputFileshortName
    this%IniConfig = otherOne%IniConfig
    this%ImpFile = otherOne%ImpFile
    this%OutFilePath = otherOne%OutFilePath
    this%RestartCfg = otherOne%RestartCfg

    !---Determine whether the reaction are considered
    this%FreeDiffusion = otherOne%FreeDiffusion

    !*********Parameters for analysis processes*************
    this%STARTJOB = otherOne%STARTJOB
    this%ENDJOB = otherOne%ENDJOB
    this%JOBSTEP = otherOne%JOBSTEP
    this%STARTTSECTION = otherOne%STARTTSECTION
    this%ENDTSECTION = otherOne%ENDTSECTION
    this%TSECTIONSTEP = otherOne%TSECTIONSTEP
    this%STARTCFG = otherOne%STARTCFG
    this%ENDCFG = otherOne%ENDCFG
    this%CFGSTEP = otherOne%CFGSTEP
    this%STARTBOX = otherOne%STARTBOX
    this%ENDBOX = this%ENDBOX
    this%BOXSTEP = this%BOXSTEP

    call Copy_StatementList(otherOne%AddOnData,this%AddOnData)

    call Copy_StatementList(otherOne%ModelData,this%ModelData)

    return
  end subroutine CopyFromOther

  !****************************************************************
  subroutine CleanOneSimulationCtrlParam(this)
    implicit none
    !---Dummy Vars---
    type(SimulationCtrlParam)::this
    !---Body---
    !***Run status
     this%RESTARTAT = 0

    !***Information about simulation boxs of jobs
     this%MultiBox = 1
     this%TOTALBOX = 1
     this%INDEPBOX = 1

     !***Information for random number
     this%RANDSEED = (/43434, 54454532/)

     !***PERIOD boundary************
     this%PERIOD = 1

     !***Inforamtion about neighborlist
     this%NEIGHBORCALWAY = mp_CalcNeighborList_NNEAREST
     this%MAXNEIGHBORNUM   = 2048
     this%NEIGHBORUPDATESTRATEGY = mp_NEIGHBORUPDATEBYSTEP
     this%CUTREGIONEXTEND = 8
     this%NEIGHBORUPDATE = 10

     !***Informaiton about temperature
     this%TEMP = 300.D0
     this%TKB = 300D0*C_KB

     !***Implantation section*********
     this%ImplantSectID = 0

     !***Information about Time
     this%TermTFlag = mp_TermTimeFlag_ByRealTime
     this%TermTValue = 3000

     this%NFocusedTimePoint = 0
     call DeAllocateOneDimd_Host(this%FocusedTimePoints,"this%FocusedTimePoints")

     this%UPDATETSTEPSTRATEGY = mp_SelfAdjustlStep_NearestSep
     this%FixedTimeStepValue = 1
     this%EnlageTStepScale = 0.01
     this%LowerLimitTime = 3.68D-13
     this%LowerLimitLength = 2.74D-8
     this%LastPassageFactor = 5

     this%TUpdateStatisFlag = mp_UpdateStatisFlag_ByIntervalSteps
     this%TUpdateStatisValue = 10

     this%OutPutConfFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutConfValue = 100

     this%OutPutConfContent = .true.
     this%OutPutConf_SweepOut = .true.

     this%OutPutSCFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutSCValue_IntegralBox = 100
     this%OutPutSCValue_EachBox = 100

     this%OutPutFuncSFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutFuncSValue = 100

     this%OutPutSwapFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutSwapValue = 100

     !***File path and name restore*****
     this%InputFilePath = ""
     this%InputFileshortName = ""
     this%IniConfig = ""
     this%ImpFile = ""
     this%OutFilePath = ""
     this%RestartCfg = ""

     !---Determine whether the reaction are considered
     this%FreeDiffusion = .false.

     !*********Parameters for analysis processes*************
     this%STARTJOB = -1
     this%ENDJOB = -1
     this%JOBSTEP = 0
     this%STARTTSECTION = -1
     this%ENDTSECTION = -1
     this%TSECTIONSTEP = 0
     this%STARTCFG = -1
     this%ENDCFG = -1
     this%CFGSTEP = 0
     this%STARTBOX = -1
     this%ENDBOX = -1
     this%BOXSTEP = 0

     call Release_StatementList(this%AddOnData)
     this%AddOnData=>null()

     call Release_StatementList(this%ModelData)
     this%ModelData=>null()

  end subroutine CleanOneSimulationCtrlParam


  !****************************************************************
  subroutine CleanSimulationCtrlParam(this)
    implicit none
    !---Dummy Vars---
    type(SimulationCtrlParam),pointer::this
    !---Local Vars--
    type(SimulationCtrlParam),pointer::cursorP=>null()
    type(SimulationCtrlParam),pointer::cursor=>null()
    !---Body---

    if(.not. associated(this)) then
        return
    end if

    cursorP=>this
    cursor=>this%next

    if(associated(cursor)) then
        DO While(associated(cursor))
            Nullify(cursorP%next)
            cursorP%next=>null()
            call CleanOneSimulationCtrlParam(cursorP)
            deallocate(cursorP)
            cursorP=>cursor
            cursor=>cursor%next
        END DO

        Nullify(cursor)
        cursor=>null()

    else
        call CleanOneSimulationCtrlParam(cursorP)
    end if

    Nullify(cursorP)
    cursorP=>null()

     return
  end subroutine

  !****************************************************************
  subroutine Clean(this)
    implicit none
    !---Dummy Vars---
    type(SimulationCtrlParam),target::this
    !---Local Vars---
    type(SimulationCtrlParam),pointer::ptr=>null()
    !---Body---
    ptr=>this

    call CleanSimulationCtrlParam(ptr)

    Nullify(ptr)
    ptr=>null()

    return
  end subroutine Clean


  !****************************************************************
  subroutine DefaultValue_CtrlParam(this)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    !---Body---
    !***Run status
     this%RESTARTAT = 0

    !***Information about simulation boxs of jobs
     this%MultiBox = 1
     this%TOTALBOX = 1
     this%INDEPBOX = 1

     !***Information for random number
     this%RANDSEED = (/43434, 54454532/)

     !***PERIOD boundary************
     this%PERIOD = 1

     !***Inforamtion about neighborlist
     this%NEIGHBORCALWAY = mp_CalcNeighborList_NNEAREST
     this%MAXNEIGHBORNUM   = 2048
     this%NEIGHBORUPDATESTRATEGY = mp_NEIGHBORUPDATEBYSTEP
     this%CUTREGIONEXTEND = 8
     this%NEIGHBORUPDATE = 10

     !***Informaiton about temperature
     this%TEMP = 300.D0
     this%TKB = 300D0*C_KB

     !***Implantation section*********
     this%ImplantSectID = 0

     !***Information about Time
     this%TermTFlag = mp_TermTimeFlag_ByRealTime
     this%TermTValue = 3000

     !*******Focused time point*****************
     this%NFocusedTimePoint = 0
     call DeAllocateOneDimd_Host(this%FocusedTimePoints,"this%FocusedTimePoints")

     this%UPDATETSTEPSTRATEGY = mp_SelfAdjustlStep_NearestSep
     this%FixedTimeStepValue = 1
     this%EnlageTStepScale = 0.01
     this%LowerLimitTime = 3.68D-13
     this%LowerLimitLength = 2.74D-8
     this%LastPassageFactor = 5

     this%TUpdateStatisFlag = mp_UpdateStatisFlag_ByIntervalSteps
     this%TUpdateStatisValue = 10

     this%OutPutConfFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutConfValue = 100

     this%OutPutConfContent = .true.
     this%OutPutConf_SweepOut = .true.

     this%OutPutSCFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutSCValue_IntegralBox = 100
     this%OutPutSCValue_EachBox = 100

     this%OutPutFuncSFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutFuncSValue = 100

     this%OutPutSwapFlag = mp_OutTimeFlag_ByIntervalSteps
     this%OutPutSwapValue = 100

     !***File path and name restore*****
     this%InputFilePath = ""
     this%InputFileshortName = ""
     this%IniConfig = ""
     this%ImpFile = ""
     this%OutFilePath = ""
     this%RestartCfg = ""

     !---Determine whether the reaction are considered
     this%FreeDiffusion = .false.

     !*********Parameters for analysis processes*************
     this%STARTJOB = -1
     this%ENDJOB = -1
     this%JOBSTEP = 0
     this%STARTTSECTION = -1
     this%ENDTSECTION = -1
     this%TSECTIONSTEP = 0
     this%STARTCFG = -1
     this%ENDCFG = -1
     this%CFGSTEP = 0
     this%STARTBOX = -1
     this%ENDBOX = -1
     this%BOXSTEP = 0

     call Release_StatementList(this%AddOnData)
     this%AddOnData=>null()

     call Release_StatementList(this%ModelData)
     this%ModelData=>null()

     this%next=>null()

    return
  end subroutine DefaultValue_CtrlParam

  !******************************************************
  subroutine Load_Ctrl_Parameters(this,hFile)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),target::this
    integer, intent(in)::hFile
    !---Local Vars---
    integer::LINE
    character*1000::STR
    character*32::KEYWORD
    type(SimulationCtrlParam),pointer::cursor=>null()
    type(SimulationCtrlParam)::tempCtrlParam
    integer::ISect
    !---Body---

    call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
    call RemoveComments(STR,"!")
    STR = adjustl(STR)
    call GETKEYWORD("&", STR, KEYWORD)
    call UPCASE(KEYWORD)
    if(KEYWORD(1:LENTRIM(KEYWORD)) .ne. m_CTLSTARTFLAG) then
      write(*,*) "MCPSCUERROR: The Start Flag of simulation Control Parameters is Illegal: ",KEYWORD(1:LENTRIM(KEYWORD))
      pause
      stop
    end if

    ISect = 0
    DO While(.TRUE.)
      call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDCTLF")
          exit

        case("&COMMSUBCTL")
          call this%Load_Ctrl_CommParameter(hFile,*100)
          cursor=>this
          DO While(.true.)
            if(.not. associated(cursor)) then
                exit
            end if
            cursor%MultiBox = this%MultiBox
            cursor%TOTALBOX = this%TOTALBOX
            cursor%INDEPBOX = this%INDEPBOX
            cursor%RANDSEED = this%RANDSEED

            cursor=>cursor%next
          END DO

        case("&ANALYSUBCTL")
            call this%Load_Ctrl_AnalyParameter(hFile,*100)
            cursor=>this
            DO While(.true.)
                if(.not. associated(cursor)) then
                    exit
                end if
                cursor%STARTJOB = this%STARTJOB
                cursor%ENDJOB = this%ENDJOB
                cursor%JOBSTEP = this%JOBSTEP
                cursor%STARTTSECTION = this%STARTTSECTION
                cursor%ENDTSECTION = this%ENDTSECTION
                cursor%TSECTIONSTEP = this%TSECTIONSTEP
                cursor%STARTCFG = this%STARTCFG
                cursor%ENDCFG = this%ENDCFG
                cursor%CFGSTEP = this%CFGSTEP
                cursor%STARTBOX = this%STARTBOX
                cursor%ENDBOX = this%ENDBOX
                cursor%BOXSTEP = this%BOXSTEP


                cursor=>cursor%next
            END DO

        case("&SECTSUBCTL")

          if(ISect .eq. 0) then
            call this%Load_Ctrl_SectionParameter(hFile,*100)
          else
            tempCtrlParam = this
            tempCtrlParam%next=>null()
            call tempCtrlParam%Load_Ctrl_SectionParameter(hFile,*100)
            call this%AppendOne_SimulationCtrlParam(tempCtrlParam)
          end if
          ISect = ISect + 1

        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD))
          write(*,*) "Please Check Control File at Line: ",LINE
          stop
      end select

    END DO

    return
    !-----------------------------------------------------
    100 write(*,*) "MCPSCU ERROR: Failer to read Simulation Control Parameters."
        write(*,*) "The process would be stop."
        stop
  end subroutine Load_Ctrl_Parameters

  !*****************************************
  subroutine Load_Ctrl_CommParameter(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    character*1000::STR
    character*1000::KEYWORD
    character*32::STRNUMB(10)
    integer::N
    integer::I
    integer::LINE
    !---Body---

    DO While(.TRUE.)
      call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDSUBCTL")
          exit
        case("&BOX")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .LT. 3) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for MultiBox Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&BOX box in one test = , total num = , independent ='."
             stop
           else
             this%MultiBox = ISTR(STRNUMB(1))
             this%TOTALBOX = ISTR(STRNUMB(2))
             this%INDEPBOX = ISTR(STRNUMB(3))

             if(this%MultiBox .LE. 0) then
               write(*,*) "MCPSCU ERROR: The number of box can not less than 0 .",this%MultiBox
               stop
             end if

             if(this%TOTALBOX .LT. this%MultiBox) then
               write(*,*) "MCPSCU ERROR: The number of total box should great than box in one test. "
               write(*,*) "At control file line: ",LINE
               write(*,*) "box in one test =",this%MultiBox," total num =",this%TOTALBOX
               pause
               stop
             end if
           end if
        case("&RANDSEED")

            call EXTRACT_NUMB(STR,2,N,STRNUMB)

            if(N .LT. size(this%RANDSEED)) then
                write(*,*) "MCPSCUERROR: The random seeds number shoud be at lease: ",size(this%RANDSEED)
                pause
                stop
            end if

            DO I = 1,size(this%RANDSEED)
                this%RANDSEED(I) = ISTR(STRNUMB(I))
            END DO

        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD)),LINE
          write(*,*) "Please Check Control File at Line: ",LINE
          stop
      end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_CommParameter

  !*****************************************
  subroutine Load_Ctrl_AnalyParameter(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    character*1000::STR
    character*1000::KEYWORD
    character*32::STRNUMB(10)
    integer::N
    integer::I
    integer::LINE
    !---Body---

    DO While(.TRUE.)
      call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDSUBCTL")
          exit
        case("&JOBSEL")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .GT. 0) then
                this%STARTJOB = ISTR(STRNUMB(1))
           end if

           if(N .GE. 2) then
                this%ENDJOB = ISTR(STRNUMB(2))
           end if

           if(N .GE. 3) then
                this%JOBSTEP = ISTR(STRNUMB(3))
           end if

        case("&CFGSEL")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .GT. 0) then
                this%STARTCFG = ISTR(STRNUMB(1))
           end if

           if(N .GE. 2) then
                this%ENDCFG = ISTR(STRNUMB(2))
           end if

           if(N .GE. 3) then
                this%CFGSTEP = ISTR(STRNUMB(3))
           end if

        case("&BOXSEL")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .GT. 0) then
                this%STARTBOX = ISTR(STRNUMB(1))
           end if

           if(N .GE. 2) then
                this%ENDBOX = ISTR(STRNUMB(2))
           end if

           if(N .GE. 3) then
                this%BOXSTEP = ISTR(STRNUMB(3))
           end if

        case("&TSECTIONSEL")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .GT. 0) then
                this%STARTTSECTION = ISTR(STRNUMB(1))
           end if

           if(N .GE. 2) then
                this%ENDTSECTION = ISTR(STRNUMB(2))
           end if

           if(N .GE. 3) then
                this%TSECTIONSTEP = ISTR(STRNUMB(3))
           end if


        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD)),LINE
          write(*,*) "Please Check Control File at Line: ",LINE
          stop
      end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_AnalyParameter

  !*****************************************
  subroutine Load_Ctrl_SectionParameter(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    character*1000::STR
    integer::LINE
    character*32::KEYWORD
    !---Body---

    DO while(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&", STR, KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case default
                write(*,*) "MCPSCUERROR: Illegl symbol : ",KEYWORD,LINE
                pause
                stop
            case("&ENDSUBCTL")
                exit
            case("&TEMPSUBCTL")
                call this%Load_Ctrl_Temperature(hFile,*100)
            case("&BOUNDSUBCTL")
                call this%Load_Ctrl_Boundary(hFile,*100)
            case("&NEIGHBSUBCTL")
                call this%Load_Ctrl_NeighborList(hFile,*100)
            case("&IMPLANTSUBCTL")
                call this%Load_Ctrl_Implant(hFile,*100)
            case("&TIMESUBCTL")
                call this%Load_Ctrl_TimeStep(hFile,*100)
            case("&ADDONDATA")
                call this%Load_AddOnDataStatments(hFile,*100)
            case("&MODELDATA")
                call this%Load_ModelDataStatments(hFile,*100)
        end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_SectionParameter

  !*****************************************
  subroutine Load_Ctrl_Temperature(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*1000::STR
    character*32::KEYWORD
    character*32::STRNUMB(10)

    DO While(.TRUE.)
      call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDSUBCTL")
          exit
        case("&TEMPERATURE")
           call EXTRACT_NUMB(STR,1,N,STRNUMB)

           if(N .LT. 1) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for TEMPERATURE Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&TEMPERATURE SYSTEM SIMULATION TEMPERATURE = (K)'."
             stop
           else
             this%TEMP = DRSTR(STRNUMB(1))
             this%TKB = this%TEMP*C_KB

             if(this%TEMP .LE. 0) then
               write(*,*) "MCPSCU ERROR: The absoloute temperature cannot less than 0 .",this%TEMP
               stop
             end if

           end if

        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD))
          write(*,*) "Please Check Control File at Line: ",LINE
          stop
      end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_Temperature

  !*****************************************
  subroutine Load_Ctrl_Boundary(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*1000::STR
    character*32::KEYWORD
    character*32::STRNUMB(10)
    !---Body---
    DO while(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case default
                write(*,*) "MCPSCUERROR: Illegl flag: ",KEYWORD,LINE
                pause
                stop
            case("&ENDSUBCTL")
                exit
            case("&PERIDIC")
                call EXTRACT_NUMB(STR,3,N,STRNUMB)

                if(N .LT. 3) then
                    write(*,*) "MCPSCUERROR: please special the boundary condition in three direction."
                    write(*,*) "At control file line: ",LINE
                    write(*,*) "Should be '&PERIDIC If use periodic boundary condition: X = , Y = , Z = '."
                    pause
                    stop
                end if
                this%PERIOD(1) = ISTR(STRNUMB(1))
                this%PERIOD(2) = ISTR(STRNUMB(2))
                this%PERIOD(3) = ISTR(STRNUMB(3))
        end select
    END DO

    return

    100 return 1
  end subroutine

  !*****************************************
  subroutine Load_Ctrl_NeighborList(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*1000::STR
    character*32::KEYWORD
    character*32::STRNUMB(10)

    DO While(.TRUE.)
      call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDSUBCTL")
          exit
        case("&STRATEGY")
            call EXTRACT_NUMB(STR,1,N,STRNUMB)

            if(N .LT. 1) then
                write(*,*) "MCPSCU ERROR: Too Few Parameters for STRATEGY Setting."
                write(*,*) "At control file line: ",LINE
                write(*,*) "Should be '&STRATEGY  The parameter determine the way to update neighbor-List = '."
            end if

            this%NEIGHBORCALWAY = ISTR(STRNUMB(1))

            if(this%NEIGHBORCALWAY .ne. mp_CalcNeighborList_NNEAREST .and. this%NEIGHBORCALWAY .ne. mp_CalcNeighborList_RCUT) then
                write(*,*) "MCPSCUERROR: Unkonwn neighbor-list update-strategy."
                pause
                stop
            end if

        case("&MAXNB")
           call EXTRACT_NUMB(STR,1,N,STRNUMB)

           if(N .LT. 1) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for MAXNB Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&MAXNB The max Number of neighbores that permitted value ='."
             pause
             stop
           else
             this%MAXNEIGHBORNUM = ISTR(STRNUMB(1))

             if(this%MAXNEIGHBORNUM .LE. 0) then
               write(*,*) "MCPSCU ERROR: The MAXNEIGHBORNUM cannot less than 0 .",this%MAXNEIGHBORNUM
               pause
               stop
             end if

           end if

        case("&UPDATEFRE")
            call EXTRACT_NUMB(STR,1,N,STRNUMB)

            if(N .LT. 1) then
                write(*,*) "MCPSCU ERROR: Too Few Parameters for UPDATEFRE Setting."
                write(*,*) "At control file line: ",LINE
                write(*,*) "Should be 'The parameter determine when the neighbore list to be updated = '."
            end if

            this%NEIGHBORUPDATE = DRSTR(STRNUMB(1))

            if(this%NEIGHBORUPDATE .LE. 0) then
                write(*,*) "MCPSCUERROR: The update frequence cannot less than 0."
                pause
                stop
            end if

            if(this%NEIGHBORUPDATE .LT. 1.D0) then
                this%NEIGHBORUPDATESTRATEGY = mp_NEIGHBORUPDATEBYNCREMIND
            else
                this%NEIGHBORUPDATESTRATEGY = mp_NEIGHBORUPDATEBYSTEP
            end if

        case("&CUTREGIONEXTEND")
            call EXTRACT_NUMB(STR,1,N,STRNUMB)

            if(N .LT. 1) then
                write(*,*) "MCPSCU ERROR: Too Few Parameters for CUTREGIONEXTEND Setting."
                write(*,*) "At control file line: ",LINE
                write(*,*) "Should be '&CUTREGIONEXTEND The cut-off region expand = '."
            end if

            this%CUTREGIONEXTEND = ISTR(STRNUMB(1))

            if(this%CUTREGIONEXTEND .LE. 0) then
                write(*,*) "MCPSCUERROR: The cut-off region expand cannot less than 0."
                pause
                stop
            end if

        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD))
          write(*,*) "Please Check Control File at Line: ",LINE
          pause
          stop
      end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_NeighborList

  !*****************************************
  subroutine Load_Ctrl_Implant(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    character*1000::STR
    character*32::KEYWORD
    character*20::SUBNUM(10)
    integer::LINE
    integer::N
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&IMPLANTID")
                call EXTRACT_NUMB(STR,1,N,SUBNUM)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the Keyword: ",KEYWORD
                    write(*,*) "You should special: '&IMPLANTID The used implantation id in current time section = '"
                    pause
                    stop
                end if
                this%ImplantSectID = ISTR(SUBNUM(1))
            case default
                write(*,*) "MCPSCUERROR: The flag is illegal: ",KEYWORD
                pause
                stop
        end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_Implant

  !*****************************************
  subroutine Load_Ctrl_TimeStep(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam)::this
    integer, intent(in)::hFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*1000::STR
    character*32::KEYWORD
    character*32::STRNUMB(20)
    integer::I
    integer::IStatu
    character*32::OneContent
    logical::Finded
    !---Body---


    DO While(.TRUE.)
      call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDSUBCTL")
          exit
        case("&TERMINATE")
           call EXTRACT_NUMB(STR,2,N,STRNUMB)

           if(N .LT. 2) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for TERMINATE Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be ' &TERMINATE Maxma simulation flag =, the time ='."
             pause
             stop
           else
             this%TermTFlag = ISTR(STRNUMB(1))
             if(this%TermTFlag .ne. mp_TermTimeFlag_ByStep .AND. this%TermTFlag .ne. mp_TermTimeFlag_ByRealTime) then
               write(*,*) "MCPSCU ERROR: The TERMINATE flag cannot is not defined.",this%TermTFlag
               pause
               stop
             end if

             this%TermTValue = DRSTR(STRNUMB(2))
             if(this%TermTValue .LT. 0) then
               write(*,*) "MCPSCU ERROR: The TERMINATE value cannot less than 0.",this%TermTValue
               pause
               stop
             end if

           end if

        case("&FOCUSETIME")
            call EXTRACT_NUMB(STR,p_MAX_FOCUSEDTIMEPOINTS,N,STRNUMB)

            if(N .GT. 0) then
                call AllocateOneDimd_Host(this%FocusedTimePoints,N,"FocusedTimePoints")
            end if

            this%NFocusedTimePoint = N

            DO I = 1,N
                this%FocusedTimePoints(I) = DRSTR(STRNUMB(I))

                if(I .GT. 1) then
                    if(this%FocusedTimePoints(I) .LE. this%FocusedTimePoints(I-1)) then
                        write(*,*) "MCPSCU ERROR: You should align the focused time-points from smaller to bigger"
                        write(*,*) "At control file line: ",LINE
                        write(*,*) STR
                        pause
                        stop
                    end if
                end if

                if(this%FocusedTimePoints(I) .GT. this%TermTValue) then
                    write(*,*) "MCPSCU ERROR: the focused time-point should less than terminate time "
                    write(*,*) "Chosen focused time point is: ", this%FocusedTimePoints(I)
                    write(*,*) "The terminate time point is: ",this%TermTValue
                    pause
                    stop
                end if
            END DO

        case("&TSTEPSTRATEGY")
           call EXTRACT_NUMB(STR,1,N,STRNUMB)

           if(N .LT. 1) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the corresponded value one = , the corresponded value two = '."
             pause
             stop
           end if

           this%UPDATETSTEPSTRATEGY = ISTR(STRNUMB(1))

           select case(this%UPDATETSTEPSTRATEGY)
                case(mp_SelfAdjustlStep_NearestSep)
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)

                    if(N .LT. 2) then
                        write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
                        write(*,*) "At control file line: ",LINE
                        write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the enlarge enlarge time Step scale =  '."
                        pause
                        stop
                    end if

                    this%EnlageTStepScale = DRSTR(STRNUMB(2))

                    if(this%EnlageTStepScale .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The time-step-enlarge-value cannot less than 0.",this%EnlageTStepScale
                        pause
                        stop
                    end if

                case(mp_FixedTimeStep)
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)

                    if(N .LT. 2) then
                        write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
                        write(*,*) "At control file line: ",LINE
                        write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the fixed time step value =  '."
                        pause
                        stop
                    end if

                    this%FixedTimeStepValue = DRSTR(STRNUMB(2))

                    if(this%FixedTimeStepValue .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The fixed time-step-value cannot less than 0.",this%FixedTimeStepValue
                        pause
                        stop
                    end if

                case(mp_SelfAdjustlStep_AveSep)
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)

                    if(N .LT. 2) then
                        write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
                        write(*,*) "At control file line: ",LINE
                        write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the enlarge enlarge time Step scale =  '."
                        pause
                        stop
                    end if

                    this%EnlageTStepScale = DRSTR(STRNUMB(2))
                    if(this%EnlageTStepScale .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The time-step-enlarge-value cannot less than 0.",this%EnlageTStepScale
                        pause
                        stop
                    end if

                case(mp_SelfAdjustlStep_NNDR)
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)

                    if(N .LT. 2) then
                        write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
                        write(*,*) "At control file line: ",LINE
                        write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the low limit time  =  '."
                        pause
                        stop
                    end if

                    this%LowerLimitTime = DRSTR(STRNUMB(2))
                    if(this%LowerLimitTime .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The lower time limit cannot less than 0.",this%LowerLimitTime
                        pause
                        stop
                    end if

                case(mp_SelfAdjustlStep_NNDR_S4)
                    call EXTRACT_NUMB(STR,3,N,STRNUMB)

                    if(N .LT. 3) then
                        write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
                        write(*,*) "At control file line: ",LINE
                        write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the low limit time  =  , the low limit length  = '."
                        pause
                        stop
                    end if

                    this%LowerLimitTime = DRSTR(STRNUMB(2))
                    if(this%LowerLimitTime .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The lower time limit cannot less than 0.",this%LowerLimitTime
                        pause
                        stop
                    end if

                    this%LowerLimitLength = DRSTR(STRNUMB(3))
                    if(this%LowerLimitLength .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The lower time limit cannot less than 0.",this%LowerLimitLength
                        pause
                        stop
                    end if

                case(mp_SelfAdjustlStep_NNDR_LastPassage)
                    call EXTRACT_NUMB(STR,4,N,STRNUMB)

                    if(N .LT. 2) then
                        write(*,*) "MCPSCU ERROR: Too Few Parameters for TSTEPSTRATEGY Setting."
                        write(*,*) "At control file line: ",LINE
                        write(*,*) "Should be '&TSTEPSTRATEGY The update time-step strategy = , the low limit time  =  , the  LowerLimit Length = , the last passage factor = '."
                        pause
                        stop
                    end if

                    this%LowerLimitTime = DRSTR(STRNUMB(2))
                    if(this%LowerLimitTime .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The lower time limit cannot less than 0.",this%LowerLimitTime
                        pause
                        stop
                    end if

                    this%LowerLimitLength = DRSTR(STRNUMB(3))
                    if(this%LowerLimitLength .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The lower time limit cannot less than 0.",this%LowerLimitLength
                        pause
                        stop
                    end if

                    this%LastPassageFactor = ISTR(STRNUMB(4))
                    if(this%LastPassageFactor .LT. 0) then
                        write(*,*) "MCPSCU ERROR: The last passage factor cannot less than 0.",this%LastPassageFactor
                        pause
                        stop
                    end if

                case default
                    write(*,*) "MCPSCU ERROR: The TSTEPSTRATEGY flag cannot is not defined.",this%UPDATETSTEPSTRATEGY
                    pause
                    stop
            end select

        case("&UPDATESTATISTIC")
           call EXTRACT_NUMB(STR,2,N,STRNUMB)

           if(N .LT. 2) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for UPDATESTATISTIC Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&UPDATESTATISTIC  Use fixd step flag = , the correspond value =       '."
             pause
             stop
           else
             this%TUpdateStatisFlag = ISTR(STRNUMB(1))
             if(this%TUpdateStatisFlag .ne. mp_UpdateStatisFlag_ByIntervalSteps .AND. this%TUpdateStatisFlag .ne. mp_UpdateStatisFlag_ByIntervalRealTime) then
               write(*,*) "MCPSCU ERROR: The TUpdateStatisFlag flag cannot is not defined.",this%TUpdateStatisFlag
               pause
               stop
             end if

             this%TUpdateStatisValue = DRSTR(STRNUMB(2))
             if(this%TUpdateStatisValue .LT. 0) then
               write(*,*) "MCPSCU ERROR: The TUpdateStatis value cannot less than 0.",this%TUpdateStatisValue
               pause
               stop
             end if

           end if

        case("&OUTPUT_CONF")
           call EXTRACT_NUMB(STR,2,N,STRNUMB)

           if(N .LT. 2) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for OUTPUT_CONF Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&OUTPUT_CONF Output instant configuration flag =, the interval = '."
             pause
             stop
           else
             this%OutPutConfFlag = ISTR(STRNUMB(1))
             if(this%OutPutConfFlag .ne. mp_OutTimeFlag_ByIntervalSteps .AND. &
                this%OutPutConfFlag .ne. mp_OutTimeFlag_ByIntervalRealTime .AND. &
                this%OutPutConfFlag .ne. mp_OutTimeFlag_ByIntervalTimeMagnification) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_CONF flag cannot is not defined.",this%OutPutConfFlag
               pause
               stop
             end if

             this%OutPutConfValue = DRSTR(STRNUMB(2))
             if(this%OutPutConfValue .LT. 0) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_CONF value cannot less than 0.",this%OutPutConfValue
               pause
               stop
             end if

           end if

        case("&CONFIG_CONTENT")
            call EXTRACT_SUBSTR(STR,p_NUMBER_OF_STATU,N,STRNUMB)

            if(N .LT. 1) then
                write(*,*) "MCPSCUERROR: Too few content to out put for CONFIG_CONTENT setting"
                write(*,*) "At control file line: ",LINE
                write(*,*) "Should be '&CONFIG_CONTENT  The information that is specialed to be out in config is '' , '' , '' "
                write(*,*) STR
                pause
                stop
            end if

            this%OutPutConfContent = .false.

            DO I = 1,N
                Finded = .false.

                OneContent = STRNUMB(I)
                call UPCASE(OneContent)

                DO IStatu = 1,p_NUMBER_OF_STATU
                    if(ISSTREQUAL(p_CStatu(IStatu),OneContent) .eq. .true. ) then
                        Finded = .true.
                        this%OutPutConfContent(IStatu) = .true.
                        exit
                    end if
                END DO

                if(Finded .eq. .false.) then
                    write(*,*) "MCPSCUERROR: Unknown output configure content: ",STRNUMB(I)
                    write(*,*) "At control file line: ",LINE
                    write(*,*) STR
                    pause
                    stop
                end if
            END DO

        case("&CONFIG_SWEEP")
            call EXTRACT_NUMB(STR,1,N,STRNUMB)

            if(N .LT. 1) then
                write(*,*) "MCPSCUERROR: Too few parameter to out put for CONFIG_SWEEP setting"
                write(*,*) "At control file line: ",LINE
                write(*,*) "Should be '&CONFIG_SWEEP  Whether output configure file before sweep out the menory = ' "
                write(*,*) STR
                pause
                stop
            end if
            if(ISTR(STRNUMB(1)) .LE. 0) then
                this%OutPutConf_SweepOut = .false.
            else
                this%OutPutConf_SweepOut = .true.
            end if

        case("&OUTPUT_SC")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .LT. 3) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for OUTPUT_SC Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&OUTPUT_SC Output instant configuration flag =, the interval for integral box =, the interval for each box =  '."
             pause
             stop
           else
             this%OutPutSCFlag = ISTR(STRNUMB(1))
             if(this%OutPutSCFlag .ne. mp_OutTimeFlag_ByIntervalSteps .AND. &
                this%OutPutSCFlag .ne. mp_OutTimeFlag_ByIntervalRealTime .AND. &
                this%OutPutSCFlag .ne. mp_OutTimeFlag_ByIntervalTimeMagnification) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_SC flag cannot is not defined.",this%OutPutSCFlag
               pause
               stop
             end if

             this%OutPutSCValue_IntegralBox = DRSTR(STRNUMB(2))
             if(this%OutPutSCValue_IntegralBox .LT. 0) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_SC value cannot less than 0 for integral box.",this%OutPutSCValue_IntegralBox
               pause
               stop
             end if

            this%OutPutSCValue_EachBox = DRSTR(STRNUMB(3))
             if(this%OutPutSCValue_EachBox .LT. 0) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_SC value cannot less than 0 for each box.",this%OutPutSCValue_EachBox
               pause
               stop
             end if

           end if

        case("&OUTPUT_FUNCS")
           call EXTRACT_NUMB(STR,2,N,STRNUMB)

           if(N .LT. 2) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for OUTPUT_FUNCS Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&OUTPUT_FUNCS Output instant configuration flag =, the interval = '."
             pause
             stop
           else
             this%OutPutFuncSFlag = ISTR(STRNUMB(1))
             if(this%OutPutFuncSFlag .ne. mp_OutTimeFlag_ByIntervalSteps .AND. &
                this%OutPutFuncSFlag .ne. mp_OutTimeFlag_ByIntervalRealTime .AND. &
                this%OutPutFuncSFlag .ne. mp_OutTimeFlag_ByIntervalTimeMagnification) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_FUNCS flag cannot is not defined.",this%OutPutFuncSFlag
               pause
               stop
             end if

             this%OutPutFuncSValue = DRSTR(STRNUMB(2))
             if(this%OutPutFuncSValue .LT. 0) then
               write(*,*) "MCPSCU ERROR: The OUTPUT_FUNCS value cannot less than 0.",this%OutPutFuncSValue
               pause
               stop
             end if

           end if

        case("&SAVE")
           call EXTRACT_NUMB(STR,2,N,STRNUMB)

           if(N .LT. 2) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for SAVE Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&SAVE Output instant configuration flag =, the interval = '."
             pause
             stop
           else
             this%OutPutSwapFlag = ISTR(STRNUMB(1))
             if(this%OutPutSwapFlag .ne. mp_OutTimeFlag_ByIntervalSteps .AND.  &
                this%OutPutSwapFlag .ne. mp_OutTimeFlag_ByIntervalRealTime .AND. &
                this%OutPutSwapFlag .ne. mp_OutTimeFlag_ByIntervalTimeMagnification) then
               write(*,*) "MCPSCU ERROR: The SAVE flag cannot is not defined.",this%OutPutSwapFlag
               pause
               stop
             end if

             this%OutPutSwapValue = DRSTR(STRNUMB(2))
             if(this%OutPutSwapValue .LT. 0) then
               write(*,*) "MCPSCU ERROR: The SAVE value cannot less than 0.",this%OutPutSwapValue
               pause
               stop
             end if

           end if

        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD))
          write(*,*) "Please Check Control File at Line: ",LINE
          pause
          stop
      end select
    END DO

    return

    100 return 1
  end subroutine Load_Ctrl_TimeStep

  !********************************************
  subroutine Load_AddOnDataStatments(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),target::this
    integer,intent(in)::hFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*1000::STR
    character*32::KEYWORD
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&", STR, KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case default
                call Add_StatementList(this%AddOnData, STR, LINE)
        end select

    END DO

    return
    100 return 1
  end subroutine


  !********************************************
  subroutine Load_ModelDataStatments(this,hFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),target::this
    integer,intent(in)::hFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*1000::STR
    character*32::KEYWORD
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR, LINE, "!", *100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&", STR, KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case default
                call Add_StatementList(this%ModelData, STR, LINE)
        end select

    END DO

    return
    100 return 1
  end subroutine Load_ModelDataStatments

  !********************************************
  subroutine Print_CtrlParameters(this,hFile)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationCtrlParam),target::this
    integer,intent(in)::hFile
    !---Local Vars---
    type(SimulationCtrlParam),pointer::cursor=>null()
    integer::ISect
    integer::IStatu
    character*1000::ConfigContent
    character*1000::CFormat
    character*1000::CNUM
    !---Body---

    write(hFile,*) "!****************Control file information***********************"
    write(hFile,fmt="('!',A70,'!',2x,I10)") "Box in one test =",this%MultiBox
    write(hFile,fmt="('!',A70,'!',2x,I10)") "Total boxes num =",this%TOTALBOX
    write(hFile,fmt="('!',A70,'!',2x,I10)") "Boxes independent =",this%INDEPBOX

    write(hFile,fmt="('!',A70,'!',2x,3I10)") "Random Seeds =",this%RANDSEED


    cursor=>this

    ISect = 1
    DO while(associated(cursor))
        write(hFile,fmt="('*******************SubSection #: ',I10)") ISect

        write(hFile,fmt="('!',A70,'!',2x,1PE16.8)") "SYSTEM SIMULATION TEMPERATURE =",cursor%TEMP

        write(hFile,fmt="('!',A70,'!',2x,3I10)") "PERIDIC condition =",cursor%PERIOD

        !***Inforamtion about neighborlist
        write(hFile,fmt="('!',A70,'!',2x,I10)") "The parameter determine the strategy to calculate neighbor-List =",cursor%NEIGHBORCALWAY

        write(hFile,fmt="('!',A70,'!',2x,I10)") "maxmun number of neighbore for an diffusor =",cursor%MAXNEIGHBORNUM

        write(hFile,fmt="('!',A70,'!',2x,I10)") "The parameter determine the way to update neighbor-List =",cursor%NEIGHBORUPDATESTRATEGY

        write(hFile,fmt="('!',A70,'!',2x,1PE16.8)") "The parameter determine when the neighbore list to be updated =",cursor%NEIGHBORUPDATE

        write(hFile,fmt="('!',A70,'!',2x,1PE16.8)") "The cut-off region expand =",cursor%CUTREGIONEXTEND

        !***Information about Implantation******************
        write(hFile,fmt="('!',A70,'!',2x,I10)") "The implantation section index is :", cursor%ImplantSectID

        !***Information about Time
        write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Maxma simulation flag = , the time =",cursor%TermTFlag,cursor%TermTValue

        write(hFile,fmt="('!',A70,'!',2x,I10)") "The focused time-points number is = ",cursor%NFocusedTimePoint

        if(allocated(cursor%FocusedTimePoints)) then

            if(size(cursor%FocusedTimePoints) .GT. 0) then
                write(CNUM,*) size(cursor%FocusedTimePoints)
                CFormat = ""
                CFormat = "('!',A70,'!',2x,"//CNUM(1:LENTRIM(CNUM))//"(1PE16.8,2x))"
                write(hFile,fmt=CFormat(1:LENTRIM(CFormat)))  "The focused time-points are : ", cursor%FocusedTimePoints
            end if
        end if

        select case(this%UPDATETSTEPSTRATEGY)
            case(mp_SelfAdjustlStep_NearestSep)
                write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Use Time-update step strategy =, the correspond value =", &
                                                                    cursor%UPDATETSTEPSTRATEGY,cursor%EnlageTStepScale
            case(mp_FixedTimeStep)
                write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Use Time-update step strategy =, the correspond value =", &
                                                                    cursor%UPDATETSTEPSTRATEGY,cursor%FixedTimeStepValue
            case(mp_SelfAdjustlStep_AveSep)
                write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Use Time-update step strategy =, the correspond value =", &
                                                                    cursor%UPDATETSTEPSTRATEGY,cursor%EnlageTStepScale

            case(mp_SelfAdjustlStep_NNDR)
                write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Use Time-update step strategy =, the correspond value =", &
                                                                    cursor%UPDATETSTEPSTRATEGY,cursor%LowerLimitTime

            case(mp_SelfAdjustlStep_NNDR_S4)
                write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8,2x,1PE16.8)") "Use Time-update step strategy =, the correspond value one  = , the correspond value two = ", &
                                                                              cursor%UPDATETSTEPSTRATEGY,cursor%LowerLimitTime,cursor%LowerLimitLength

            case(mp_SelfAdjustlStep_NNDR_LastPassage)
                write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8,2x,1PE16.8,2x,I10)") "Use Time-update step strategy =, the correspond value one  = , the correspond value two = . the corresponded value three = ", &
                                                                              cursor%UPDATETSTEPSTRATEGY,cursor%LowerLimitTime,cursor%LowerLimitLength,cursor%LastPassageFactor
        end select

        write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "The update statistic frequency flag =, the correspond value = ",cursor%TUpdateStatisFlag,cursor%TUpdateStatisValue

        write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Output instant configuration flag = , the interval =",cursor%OutPutConfFlag,cursor%OutPutConfValue

        ConfigContent = ""

        DO IStatu = 1,p_NUMBER_OF_STATU
            if(cursor%OutPutConfContent(IStatu) .eq. .true.) then
                ConfigContent = adjustl(ConfigContent)
                ConfigContent = adjustl(trim(ConfigContent))//adjustl(trim(p_CStatu(IStatu)))//" ,"
            end if
        END DO

        write(hFile,fmt="('!',A70,'!',2x,A256)") "The information that is specialed to be out in config is ",adjustl(trim(ConfigContent))

        write(hFile,fmt="('!',A70,'!',2x,L10)") "Whether output configure file before sweep out the menory = ",cursor%OutPutConf_SweepOut

        write(hFile,fmt="('!',A70,'!',2x,I10,2(2x,1PE16.8))") "Output instant size statistic information flag =, the interval for integral box =, the interval for each box =",           &
                                                               cursor%OutPutSCFlag,cursor%OutPutSCValue_IntegralBox,cursor%OutPutSCValue_EachBox

        write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Output instant function statistic information flag =, the interval =",cursor%OutPutFuncSFlag,cursor%OutPutFuncSValue

        write(hFile,fmt="('!',A70,'!',2x,I10,2x,1PE16.8)") "Output instant information for restart flag =,the interval =",cursor%OutPutSwapFlag,cursor%OutPutSwapValue

        write(hFile,fmt="('*******************END SubSection #: ',I10)") ISect

        cursor=>cursor%next
        ISect = ISect + 1

    END DO


    return
  end subroutine

end module MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
