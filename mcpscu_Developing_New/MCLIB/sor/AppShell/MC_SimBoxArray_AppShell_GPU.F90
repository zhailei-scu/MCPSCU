module MC_SimBoxArray_AppShell_GPU
    use MCLIB_GLOBAL_GPU
    use MC_MethodClass_Factory_GPU

    type(SimulationBoxes)::m_SimBoxes
    type(SimulationCtrlParamList)::m_CtrlParamList
    type(SimulationBoxes_GPU)::dm_Boxes
    type(MCMethodClassGPU)::m_MethodClass

    contains

    !*********************************************************
    subroutine AppShell_Main_GPU(NMPI,processid,INICONFIGPROC)
        use RAND32SEEDLIB_MODULE,only:GetSeed_RAND32SEEDLIB
        use RAND32_MODULE,only:DRAND32_PUTSEED
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NMPI
        integer,intent(in)::processid
        optional::INICONFIGPROC
        external::INICONFIGPROC
        interface
            subroutine INICONFIGPROC()
                implicit none
            end subroutine INICONFIGPROC
        end interface
        !---Local Vars---
        character*1000::ARG
        character*1000::filePath
        integer::err
        integer::arg_Num
        integer::start_Index_Dev = 0
        integer::num_use_Device = 1
        integer::ISEED0,ISEED(2)

        character(len=1000)::ExePath
        character(len=1000)::path
        character(len=1000)::ExeName
        character(len=1000)::ExePrefixName
        logical::exits
        integer::ISTAT
        integer::TestLoops
        integer::ILoop
        !-----------Body--------------

        arg_Num = COMMAND_ARGUMENT_COUNT()

        if(arg_NUM .GE. 1) THEN

            call GET_COMMAND_ARGUMENT(0,ARG)

            call GET_COMMAND_ARGUMENT(1,ARG)
            Read(ARG,fmt="(A256)") filePath

            if(arg_NUM .GE. 2) THEN
                call GET_COMMAND_ARGUMENT(2,ARG)
                Read(ARG,*) start_Index_Dev
            end if

            if(arg_NUM .GE. 3) THEN
                call GET_COMMAND_ARGUMENT(3,ARG)
                Read(ARG,*) num_use_Device
            end if
        end if

        !*********Init device setting*********************
        call Init_Device_Setting(start_Index_Dev,num_use_Device)

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(m_CtrlParamList,m_SimBoxes)

        !*******Init the simulation boxes*****************
        call m_SimBoxes%InitSimulationBox(m_CtrlParamList%theSimulationCtrlParam)

        !********Init the simulation methods*******************
        call m_MethodClass%Register_Method_Class(m_AppType,m_SimBoxes,m_CtrlParamList)

        ISEED0 = m_CtrlParamList%theSimulationCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,m_CtrlParamList,m_SimBoxes)

        if(m_CtrlParamList%theSimulationCtrlParam%INDEPBOX) then
            TestLoops = m_CtrlParamList%theSimulationCtrlParam%TOTALBOX/m_CtrlParamList%theSimulationCtrlParam%MultiBox
        else
            TestLoops = 1
        end if

        !call m_OneStepProcudureList%AppendOne()

        DO ILoop = 1,TestLoops
            call m_MethodClass%ForOneTest(m_SimBoxes,m_CtrlParamList,dm_Boxes,ILoop)
        END DO

        return
    end subroutine AppShell_Main_GPU

end module MC_SimBoxArray_AppShell_GPU
