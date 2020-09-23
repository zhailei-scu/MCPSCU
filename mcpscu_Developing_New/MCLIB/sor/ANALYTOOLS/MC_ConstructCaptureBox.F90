module MC_ConstructCaptureBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE
    implicit none

    integer,parameter::CaptureGenWay_ByCentUniform_Locally = 0
    integer,parameter::CaptureGenWay_ByMCConfig_Locally_Directly = 1

    contains
    !*****************************************************
    subroutine Generate_Capture_Locally_CentUniform(hFile)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile

    end subroutine Generate_Capture_Locally_CentUniform


    !***************************************************************
    subroutine ResloveCaptureControlFile_FormMCConfig(hFile,NSIA,RSIADistributeToCent,ROutAbsorbToCent,MCCfgPath)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        integer,intent(out)::NSIA
        real(kind=KINDDF),intent(out)::RSIADistributeToCent
        real(kind=KINDDF),intent(out)::ROutAbsorbToCent
        character*(*),intent(out)::MCCfgPath
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*30::KEYWORD
        character*200::STRTMP(10)
        integer::N
        logical::Finded

        !---Body---
        LINE = 0
        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&SIANUMBER")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the cascade number in one box."
                        pause
                        stop
                    end if
                    NSIA = ISTR(STRTMP(1))

                    if(NSIA .LE. 0) then
                        write(*,*) "MCPSCUERROR: The SIA number in one box cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the SIA number in one box."
           pause
           stop
        end if


        LINE = 0
        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&RSIATOCENTER")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the distance radius from cascade center to SIA distributed sphere."
                        pause
                        stop
                    end if
                    RSIADistributeToCent = DRSTR(STRTMP(1))

                    if(RSIADistributeToCent .LE. 0) then
                        write(*,*) "MCPSCUERROR: The SIA distance radius from cascade center to SIA distributed sphere cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the distance radius from cascade center to SIA distributed sphere."
           pause
           stop
        end if


        LINE = 0
        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ROUTABSORB")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the out absorb sphere radius (from center)."
                        pause
                        stop
                    end if
                    ROutAbsorbToCent = DRSTR(STRTMP(1))

                    if(ROutAbsorbToCent .LE. 0) then
                        write(*,*) "MCPSCUERROR: The out absorb sphere radius (from center) cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the out absorb sphere radius (from center)."
           pause
           stop
        end if


        LINE = 0
        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&MCCFGPATH")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the MC Configuration path."
                        pause
                        stop
                    end if
                    MCCfgPath = adjustl(trim(STRTMP(1)))
                    write(*,*) MCCfgPath
                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the MC Configuration path."
           pause
           stop
        end if


        close(hFile)
        return
    end subroutine ResloveCaptureControlFile_FormMCConfig




    !***************************************************************
    subroutine Generate_Capture_Locally_FormMCConfig_Directly(hFile)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        !---Local Vars---
        integer::NSIA
        real(kind=KINDDF)::RSIADistributeToCent
        real(kind=KINDDF)::ROutAbsorbToCent
        character*1000::MCCfgPath
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        character*1000::OutFolder
        character*1000::InfoOut
        character*30::TheVersion
        integer::hOut
        integer::processid
        integer::ISEED0,ISEED(2)
        !-----------Body--------------
        call ResloveCaptureControlFile_FormMCConfig(hFile,NSIA,RSIADistributeToCent,ROutAbsorbToCent,MCCfgPath)

        processid = 0

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

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath))//"CaptureBox/")

        Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        InfoOut = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"CaptureInfo.dat"

        hOut = CreateNewFile(InfoOut)

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call Host_Boxes%PutinCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record,MCCfgPath,m_FREESURDIFPRE,m_GBSURDIFPRE,TheVersion,AsInitial=.true.)

        write(*,*) "The KMC Configuration version is: ",TheVersion

        if(Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox .LE. 0) then
            write(*,*) "MCPSCUERROR: The box number less than 1"
            pause
            stop
        end if


        call Host_Boxes%PutoutCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record)

        call Host_Boxes%Clean()

        return
    end subroutine Generate_Capture_Locally_FormMCConfig_Directly















end module MC_ConstructCaptureBox


program Main_MC_ConstructCaptureBox
    use MC_ConstructCaptureBox
    use MCLIB_GLOBAL
    use MCLIB_UTILITIES
    !---Local Vars
    integer::arg_Num
    character*1000::SampleFile
    character*1000::CaptureCtrlFile
    character*1000::ARG
    character*1000::STR
    integer::hFile
    integer::LINE
    character*32::KEYWORD
    character*1000::STRTMP(10)
    integer::N
    integer::CaptureGenerateWay
    integer::FinededCaptureGenWay
    !--Body---
    arg_Num = Command_Argument_Count()

    if(arg_Num .LT. 2) then
        write(*,*) "MCPSCUERROR: You must special the sample file,capture control file"
        pause
        stop
    end if

    call Get_Command_Argument(0,ARG)

    call Get_Command_Argument(1,ARG)
    Read(ARG,fmt="(A256)") SampleFile
    write(*,*) "The Sample file is: ",SampleFile

    call Get_Command_Argument(2,ARG)
    Read(ARG,fmt="(A256)") CaptureCtrlFile
    write(*,*) "The capture control file is: ",CaptureCtrlFile


    hFile = OpenExistedFile(CaptureCtrlFile)
    LINE = 0

    FinededCaptureGenWay = 0
    Do while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
        LINE = LINE + 1
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        if(LENTRIM(STR) .LE. 0) then
            cycle
        end if

        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&GENERATEWAY")
                call EXTRACT_NUMB(STR,1,N,STRTMP)

                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: You must special the capture generate way."
                    pause
                    stop
                end if
                CaptureGenerateWay = ISTR(STRTMP(1))
                FinededCaptureGenWay = 1
        end select
    End Do

    if(FinededCaptureGenWay .LE. 0) then
        write(*,*) "You must special the capture generate way."
        write(*,*) "By the way: '&GENERATEWAY  the capture generate way = "
        write(*,*)  "0 by Locally ,center uniform way; 1 by from MC configuration,Directly)"
        pause
        stop
    end if


    select case(CaptureGenerateWay)
        case(CaptureGenWay_ByCentUniform_Locally)
            write(*,*) "The capture generate way is by Locally ,center uniform way"

            call Generate_Capture_Locally_CentUniform(hFile)

        case(CaptureGenWay_ByMCConfig_Locally_Directly)
            write(*,*) "The capture generate way is by MC Configuration, directly"

            call Generate_Capture_Locally_FormMCConfig_Directly(hFile)

        case default
            write(*,*) "MCPSCUERROR: Unknown way to generate capture(0 by uniform way, 1 by from MC configuration,Directly)"
            write(*,*) CascadeGenWay
            pause
            stop
    end select

    return

end program Main_MC_ConstructCaptureBox
