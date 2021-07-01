module MC_HomogenizeBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE
    implicit none

    contains

    subroutine HomogenizeBox(Host_Boxes,Host_SimuCtrlParam,Record,hFileOut)
        use RAND32_MODULE
        use RAND32SEEDLIB_MODULE
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        integer::hFileOut
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::ICFROM
        integer::ICTO
        integer::IC
        integer::I
        !---Body---

        MultiBox = Host_SimuCtrlParam%MultiBox

        DO IBox = 1,MultiBox

            ICFROM = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            DO IC = ICFROM,ICTO
                DO I = 1,3
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + DRAND32()*Host_Boxes%BOXSIZE(I)
                END DO

            END DO
        END DO

        call Host_Boxes%PutoutToFile(Host_SimuCtrlParam,Record,hFileOut)


        return
    end subroutine HomogenizeBox

end module MC_HomogenizeBox


program Main_MC_HomogenizeBox
    use MC_HomogenizeBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    !---Local Vars
    integer::arg_Num
    character*1000::SampleFile
    character*1000::ConfigFile
    character*1000::ARG
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParamList)::Host_SimuCtrlParamList
    type(MigCoalClusterRecord)::Record
    character*1000::OutFolder
    character*1000::pathOut
    integer::hFileOut
    character*30::TheVersion
    integer::processid
    integer::ISEED0,ISEED(2)
    !--Body---
    processid = 0

    arg_Num = Command_Argument_Count()

    if(arg_Num .LT. 2) then
        write(*,*) "MCPSCUERROR: You must special the sample file, configuration file"
        pause
        stop
    end if

    call Get_Command_Argument(0,ARG)

    call Get_Command_Argument(1,ARG)
    Read(ARG,fmt="(A256)") SampleFile
    write(*,*) "The Sample file is: ",SampleFile

    call Get_Command_Argument(2,ARG)
    Read(ARG,fmt="(A256)") ConfigFile
    write(*,*) "The configuration file is: ",ConfigFile

    !*********Create/Open log file********************
    call OpenLogFile(m_hFILELOG)

    !********Load Global vars from input file**************
    call Initialize_Global_Variables(Host_SimuCtrlParamList,Host_Boxes)

    ISEED0 = Host_SimuCtrlParamList%theSimulationCtrlParam%RANDSEED(1)
    call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
    ISEED0 = ISEED0 + processid - 1
    call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
    call DRAND32_PUTSEED(ISEED)

    call Print_Global_Variables(6,Host_SimuCtrlParamList,Host_Boxes)

    OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath))//"HomogenizeBox/")

    pathOut = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"HomogenizeBox.dat"

    hFileOut = CreateNewFile(pathOut)

    !*******Init the simulation boxes*****************
    call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

    call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

    call Host_Boxes%PutinCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record,ConfigFile,m_FREESURDIFPRE,m_GBSURDIFPRE,TheVersion,AsInitial=.true.)

    write(*,*) "The KMC Configuration version is: ",TheVersion

    call HomogenizeBox(Host_Boxes,Host_SimuCtrlParamList%theSimulationCtrlParam,Record,hFileOut)

    close(hFileOut)

    return

end program Main_MC_HomogenizeBox
