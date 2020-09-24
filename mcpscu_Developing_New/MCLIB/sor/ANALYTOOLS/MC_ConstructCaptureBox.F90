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
    subroutine ResloveCaptureControlFile_FormMCConfig(hFile,Host_Boxes,NSIA,SIASIZE,RSIADistributeToCent,ROutAbsorbToCent,MCCfgPath)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::Host_Boxes
        integer,intent(out)::NSIA
        integer,intent(out)::SIASIZE
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
                        write(*,*) "MCPSCUERROR: You must special the SIA number in one box."
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
                case("&SIASIZE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the SIA size in one box."
                        pause
                        stop
                    end if
                    SIASIZE = ISTR(STRTMP(1))

                    if(SIASIZE .LE. 0) then
                        write(*,*) "MCPSCUERROR: The SIA size cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the SIA size in one box."
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
                    RSIADistributeToCent = DRSTR(STRTMP(1))*Host_Boxes%LatticeLength

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
                    ROutAbsorbToCent = DRSTR(STRTMP(1))*Host_Boxes%LatticeLength

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
        character*1000::CfgOut
        character*1000::InfoOut
        character*30::TheVersion
        integer::hOutInfo
        integer::hCfgOut
        integer::processid
        integer::ISEED0,ISEED(2)
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFrom
        integer::ICTo
        integer::SIAIndex
        integer::VacancyIndex
        real(kind=KINDDF),dimension(:,:),allocatable::CascadeCenter
        real(kind=KINDDF)::SEP(3)
        real(kind=KINDDF)::Distance
        real(kind=KINDDF),dimension(:),allocatable::maxDistance
        integer,dimension(:),allocatable::NVACInEachBox
        integer::I
        integer::SIASIZE
        type(DiffusorValue)::TheDiffusorValue
        integer::NCUsed
        integer::NC
        integer::RecordIndex
        real(kind=KINDDF)::ZDirection
        real(kind=KINDDF)::XDirection
        !-----------Body--------------

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

        CfgOut = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"CaptureBox.dat"
        InfoOut = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"CaptureInfo.dat"

        hCfgOut = CreateNewFile(CfgOut)
        hOutInfo = CreateNewFile(InfoOut)


        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)


        call ResloveCaptureControlFile_FormMCConfig(hFile,Host_Boxes,NSIA,SIASIZE,RSIADistributeToCent,ROutAbsorbToCent,MCCfgPath)

        if(RSIADistributeToCent .GT. ROutAbsorbToCent) then
            write(*,*) "MCPSCUERROR: The SIA distribution to cascade center distance cannot greater than OutSide Absorb radius",RSIADistributeToCent,ROutAbsorbToCent
            pause
            stop
        end if


        call Host_Boxes%PutinCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record,MCCfgPath,m_FREESURDIFPRE,m_GBSURDIFPRE,TheVersion,AsInitial=.true.)

        write(*,*) "The KMC Configuration version is: ",TheVersion

        MultiBox = Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
        if(MultiBox .LE. 0) then
            write(*,*) "MCPSCUERROR: The box number less than 1"
            pause
            stop
        end if

        !********Sweep the SIA and inactive clusters***************
        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        DO IBox = 1,MultiBox
            ICFrom = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTo = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ABSORBED_STATU
                end if

            END DO
        END DO

        call Host_Boxes%SweepUnActiveMemory_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam)


        !*******Put in SIA ****************************************
        call AllocateArray_Host(maxDistance,MultiBox,"maxDistance")
        call AllocateArray_Host(CascadeCenter,MultiBox,3,"CascadeCenter")
        call AllocateArray_Host(NVACInEachBox,MultiBox,"NVACInEachBox")

        DO IBox = 1,MultiBox
            ICFrom = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTo = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            NVACInEachBox(IBox) = 0
            CascadeCenter(IBox,1:3) = 0.D0
            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO I = 1,3
                        CascadeCenter(IBox,I) = CascadeCenter(IBox,I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                    END DO

                    NVACInEachBox(IBox) = NVACInEachBox(IBox) + 1
                end if

            END DO

            if(NVACInEachBox(IBox) .GT. 0) then
                CascadeCenter(IBox,1:3) = CascadeCenter(IBox,1:3)/NVACInEachBox(IBox)
            end if


            DO I = 1,3
                if((CascadeCenter(IBox,I) - RSIADistributeToCent) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                   (CascadeCenter(IBox,I) + RSIADistributeToCent) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                    write(*,*) "the SIA distribution sphere had existed the box "
                    write(*,*) "cascade center: ",CascadeCenter(IBox,I)
                    write(*,*) "SIA distribution sphere radius: ",RSIADistributeToCent
                    write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                    pause
                    stop
                end if

                if((CascadeCenter(IBox,I) - ROutAbsorbToCent) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                   (CascadeCenter(IBox,I) + ROutAbsorbToCent) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                    write(*,*) "the outer absorb sphere had existed the box "
                    write(*,*) "cascade center: ",CascadeCenter(IBox,I)
                    write(*,*) " outer absorb sphere radius: ",ROutAbsorbToCent
                    write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                    pause
                    stop
                end if
            END DO

            maxDistance(IBox) = -1
            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - CascadeCenter(IBox,1:3)

                    Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                    Distance = DSQRT(Distance)

                    if(Distance .GT. maxDistance(IBox)) then
                        maxDistance(IBox) = Distance
                    end if
                end if
            END DO


            if(maxDistance(IBox) .GT. RSIADistributeToCent) then
                write(*,*) "MCPSCUERROR: The mostly out vac had exist the SIA distribution position",maxDistance(IBox),RSIADistributeToCent
                pause
                stop
            end if

            if(maxDistance(IBox) .GT. ROutAbsorbToCent) then
                write(*,*) "MCPSCUERROR: The mostly out vac had exist the out absorb",maxDistance(IBox),ROutAbsorbToCent
                pause
                stop
            end if

        END DO

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,NSIA)

        DO IBox = 1,MultiBox

            if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .LE. 0) then
                NCUsed = 0
            else
                NCUsed = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            end if

            RecordIndex = 0
            if(NCUsed .GT. 0) then
                RecordIndex = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2))%m_Record(1)
            end if

            if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .LE. 0) then
                NC = 0
            else
                NC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            end if

            if((NC - NCUsed) .LT. NSIA) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
                write(*,*) "The waiting to be implanted clusters number is:",NSIA
                pause
                stop
            end if

            ICFrom = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - NSIA + 1
            ICTo = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

            if(ICFrom .LT. Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are too small to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "It would occupy other free clusters for id: ",IC
                pause
                stop
            end if

            DO IC=ICFROM,ICTO
                call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = SIASIZE
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                ZDirection = DRAND32()*CP_PI
                XDirection = DRAND32()*2*CP_PI
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = CascadeCenter(IBox,1) + RSIADistributeToCent*sin(ZDirection)*cos(XDirection)
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = CascadeCenter(IBox,2) + RSIADistributeToCent*sin(ZDirection)*sin(XDirection)
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = CascadeCenter(IBox,3) + RSIADistributeToCent*cos(ZDirection)


                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = RecordIndex + 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0


                TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))
                !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                !---you should init the distribution by external file---
                select case(TheDiffusorValue%ECRValueType_Free)
                    case(p_ECR_ByValue)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                    case default
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,           &
                                                                                                   Host_Boxes%LatticeLength)
                end select

                DO I = 1,3
                    if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                       (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                            write(*,*) "MCPSCUERROR: The new added SIA had exited the box boundary"
                            write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                            write(*,*) "SIA position: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                            write(*,*) "SIA radius: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
                            pause
                            stop
                    end if
                END DO


                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1
            END DO

        END DO

        !*************Out the configuration**************
        call Host_Boxes%PutoutToFile(Host_SimuCtrlParamList%theSimulationCtrlParam,Record,hCfgOut)

        !************Out the Capture info***************
        write(hOutInfo, fmt="(130(A30,1x))") "IBox",                      &
                                             "NSIA",                      &
                                             "NVAC",                      &
                                             "CascadeCenter_X",           &
                                             "CascadeCenter_Y",           &
                                             "CascadeCenter_Z",           &
                                             "maxVACDistanceToCent"

        DO IBox = 1,MultiBox
            write(hOutInfo,fmt="(3(I30,1x),4(1PE30.10,1x))") IBox,                      &
                                                             NSIA,                      &
                                                             NVACInEachBox(IBox),       &
                                                             CascadeCenter(IBox,1:3),   &
                                                             maxDistance(IBox)
        END DO


        call Host_Boxes%Clean()
        call DeAllocateArray_Host(CascadeCenter,"CascadeCenter")
        call DeAllocateArray_Host(maxDistance,"maxDistance")
        call DeAllocateArray_Host(NVACInEachBox,"NVACInEachBox")

        close(hCfgOut)
        close(hOutInfo)

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
