module MC_ConstructCaptureBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE
    use MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    implicit none

    contains

   !*****************************************************
   subroutine Generate_Capture(TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)
        implicit none
        !---Dummy Vars---
        type(CaptureCal)::TheCaptureCal
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        character*1000::OutFolder
        character*1000::CfgOut
        character*30::TheVersion
        integer::hOutInfo
        integer::hCfgOut
        integer::processid
        integer::ISEED0,ISEED(2)
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

        hCfgOut = CreateNewFile(CfgOut)

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call TheCaptureCal%Init(Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call TheCaptureCal%ResolveCapCtrlFile(Host_Boxes,Host_SimuCtrlParamList%theSimulationCtrlParam)
        hOutInfo = CreateNewFile(TheCaptureCal%CapCalInfoPath)

        select case(TheCaptureCal%CaptureGenerateWay)
            case(CaptureGenWay_ByCentUniform_Locally)
                write(*,*) "The capture generate way is by Locally ,center uniform way"

                call Generate_Capture_Locally_CentUniform(hCfgOut,hOutInfo,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)

            case(CaptureGenWay_ByMCConfig_Locally_Directly)
                write(*,*) "The capture generate way is by MC Configuration, directly"

                call Generate_Capture_Locally_FormMCConfig_Directly(hCfgOut,hOutInfo,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)

            case default
                write(*,*) "MCPSCUERROR: Unknown way to generate capture(0 by uniform way, 1 by from MC configuration,Directly)"
                write(*,*) TheCaptureCal%CaptureGenerateWay
                pause
                stop
        end select

        return
   end subroutine

   !*****************************************************
   subroutine Generate_Capture_Locally_CentUniform(hCfgOut,hOutInfo,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hCfgOut
        integer,intent(in)::hOutInfo
        type(CaptureCal)::TheCaptureCal
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFrom
        integer::ICTo
        integer::SIAIndex
        integer::VacancyIndex
        real(kind=KINDDF)::SEP(3)
        real(kind=KINDDF)::Distance
        integer::I
        type(DiffusorValue)::TheDiffusorValue
        real(kind=KINDDF)::RR
        real(kind=KINDDF)::ArrowLen
        real(kind=KINDDF)::Vector(3)
        integer::IIC
        integer::NCUSed
        integer::RecordIndex
        integer::NC
        logical::exitFlag
        integer::ICase
        !-----------Body--------------
        if(.not. allocated(TheCaptureCal%m_CascadeCenter) .or. &
           .not. allocated(TheCaptureCal%m_maxDistance) .or.   &
           .not. allocated(TheCaptureCal%m_NVACInEachBox) .or. &
           .not. allocated(TheCaptureCal%m_RSIADistributeToCent) .or. &
           .not. allocated(TheCaptureCal%m_ROutAbsorbToCent)) then
           write(*,*) "MCPSCUERROR: You must initial the CaptureCal object before Generate the capture configuration file"
           pause
           stop
        end if

        MultiBox = Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
        if(MultiBox .LE. 0) then
            write(*,*) "MCPSCUERROR: The box number less than 1"
            pause
            stop
        end if

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        !*******Put in VAC ****************************************
        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,sum(TheCaptureCal%UDef_NVAC))
        IC = 0

        DO IBox = 1,1
            !---VAC---
            DO ICase = 1,1

                DO IIC = 1,TheCaptureCal%UDef_NVAC(ICase)

                    IC = IC + 1

                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = TheCaptureCal%UDef_VACSIZE(ICase)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                    RR = TheCaptureCal%UDef_RVACINCLUDE(ICase)*DRAND32()
                    ArrowLen = 0.D0
                    DO I = 1,3
                        Vector(I) = DRAND32() - 0.5D0
                        ArrowLen = ArrowLen + Vector(I)*Vector(I)
                    END DO
                    ArrowLen = DSQRT(ArrowLen)

                    DO I = 1,3
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%UDef_CascadeCent(ICase,I) + RR*Vector(I)/ArrowLen
                    END DO

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
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
                                write(*,*) "MCPSCUERROR: The new added VAC had exited the box boundary"
                                write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                                write(*,*) "VAC position: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                                write(*,*) "VAC radius: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
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


            DO ICase = 2,TheCaptureCal%NCascade

                DO IIC = 1,TheCaptureCal%UDef_NVAC(ICase)

                    IC = IC + 1

                    if(TheCaptureCal%VACDistSameBetweenCasese .eq. .true.) then
                        call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC-(ICase-1)*TheCaptureCal%UDef_NVAC(1))

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - TheCaptureCal%UDef_CascadeCent(ICase,I) + TheCaptureCal%UDef_CascadeCent(1,I)
                        END DO

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    else
                        call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = TheCaptureCal%UDef_VACSIZE(ICase)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                        RR = TheCaptureCal%UDef_RVACINCLUDE(ICase)*DRAND32()
                        ArrowLen = 0.D0
                        DO I = 1,3
                            Vector(I) = DRAND32() - 0.5D0
                            ArrowLen = ArrowLen + Vector(I)*Vector(I)
                        END DO
                        ArrowLen = DSQRT(ArrowLen)

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%UDef_CascadeCent(ICase,I) + RR*Vector(I)/ArrowLen
                        END DO

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
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
                                write(*,*) "MCPSCUERROR: The new added VAC had exited the box boundary"
                                write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                                write(*,*) "VAC position: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                                write(*,*) "VAC radius: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
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


        END DO

        DO IBox = 2,MultiBox
            !---VAC---

            if(TheCaptureCal%VACDistSameBetweenBoxes .eq. .true.) then
                DO ICase = 1,TheCaptureCal%NCascade

                    DO IIC = 1,TheCaptureCal%UDef_NVAC(ICase)

                        IC = IC + 1

                        call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox-1,2))

                        Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                        Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                        Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                        Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                        Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                        Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1
                    END DO
                END DO
            else

                DO ICase = 1,1

                    DO IIC = 1,TheCaptureCal%UDef_NVAC(ICase)

                        IC = IC + 1

                        call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = TheCaptureCal%UDef_VACSIZE(ICase)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                        RR = TheCaptureCal%UDef_RVACINCLUDE(ICase)*DRAND32()
                        ArrowLen = 0.D0
                        DO I = 1,3
                            Vector(I) = DRAND32() - 0.5D0
                            ArrowLen = ArrowLen + Vector(I)*Vector(I)
                        END DO
                        ArrowLen = DSQRT(ArrowLen)

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%UDef_CascadeCent(ICase,I) + RR*Vector(I)/ArrowLen
                        END DO

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
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
                                    write(*,*) "MCPSCUERROR: The new added VAC had exited the box boundary"
                                    write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                                    write(*,*) "VAC position: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                                    write(*,*) "VAC radius: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
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


                DO ICase = 2,TheCaptureCal%NCascade

                    DO IIC = 1,TheCaptureCal%UDef_NVAC(ICase)

                        IC = IC + 1

                        if(TheCaptureCal%VACDistSameBetweenCasese .eq. .true.) then
                            call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC-(ICase-1)*TheCaptureCal%UDef_NVAC(1))

                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - TheCaptureCal%UDef_CascadeCent(ICase,I) + TheCaptureCal%UDef_CascadeCent(1,I)
                            END DO

                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) =  &
                                Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                        else
                            call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = TheCaptureCal%UDef_VACSIZE(ICase)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                            RR = TheCaptureCal%UDef_RVACINCLUDE(ICase)*DRAND32()
                            ArrowLen = 0.D0
                            DO I = 1,3
                                Vector(I) = DRAND32() - 0.5D0
                                ArrowLen = ArrowLen + Vector(I)*Vector(I)
                            END DO
                            ArrowLen = DSQRT(ArrowLen)

                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%UDef_CascadeCent(ICase,I) + RR*Vector(I)/ArrowLen
                            END DO

                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = &
                                Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                        end if

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
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
                                write(*,*) "MCPSCUERROR: The new added VAC had exited the box boundary"
                                write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                                write(*,*) "VAC position: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                                write(*,*) "VAC radius: ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
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

            end if

        END DO

        !*******Get Infor mation from configuration ****************************************
        DO IBox = 1,MultiBox
            ICFrom = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTo = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            TheCaptureCal%m_NVACInEachBox(IBox) = 0
            TheCaptureCal%m_CascadeCenter(IBox,1:3) = 0.D0
            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO I = 1,3
                        TheCaptureCal%m_CascadeCenter(IBox,I) = TheCaptureCal%m_CascadeCenter(IBox,I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                    END DO

                    TheCaptureCal%m_NVACInEachBox(IBox) = TheCaptureCal%m_NVACInEachBox(IBox) + 1
                end if

            END DO

            if(TheCaptureCal%m_NVACInEachBox(IBox) .GT. 0) then
                TheCaptureCal%m_CascadeCenter(IBox,1:3) = TheCaptureCal%m_CascadeCenter(IBox,1:3)/TheCaptureCal%m_NVACInEachBox(IBox)
            end if

            TheCaptureCal%m_maxDistance(IBox) = -1
            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - TheCaptureCal%m_CascadeCenter(IBox,1:3)

                    Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                    Distance = DSQRT(Distance)

                    if(Distance .GT. TheCaptureCal%m_maxDistance(IBox)) then
                        TheCaptureCal%m_maxDistance(IBox) = Distance
                    end if
                end if
            END DO

            !***************The SIA distribution radius and outer absorb radius**************************************
            select case(TheCaptureCal%RSIADistributeToCent_Type)
                case(Capture_RSIADistributeToCent_Type_FixedValue)
                    TheCaptureCal%m_RSIADistributeToCent(IBox) = TheCaptureCal%RSIADistributeToCent_Value
                case(Capture_RSIADistributeToCent_Type_OutFromMostOutVAC)
                    TheCaptureCal%m_RSIADistributeToCent(IBox) = TheCaptureCal%m_maxDistance(IBox) + TheCaptureCal%RSIADistributeToCent_Value
                case default
                    write(*,*) "MCPSCUERROR: Unknown SIA distribution to center radius type: ",TheCaptureCal%RSIADistributeToCent_Type
                    pause
                    stop
            end select

            select case(TheCaptureCal%ROutAbsorbToCent_Type)
                case(Capture_ROutAbsorbToCent_Type_FixedValue)
                    TheCaptureCal%m_ROutAbsorbToCent(IBox) = TheCaptureCal%ROutAbsorbToCent_Value
                case(Capture_ROutAbsorbToCent_Type_OutFromMostOutVAC)
                    TheCaptureCal%m_ROutAbsorbToCent(IBox) = TheCaptureCal%m_maxDistance(IBox) + TheCaptureCal%ROutAbsorbToCent_Value
                case default
                    write(*,*) "MCPSCUERROR: Unknown SIA distribution to center radius type: ",TheCaptureCal%ROutAbsorbToCent_Type
                    pause
                    stop
            end select


            DO I = 1,3
                if((TheCaptureCal%m_CascadeCenter(IBox,I) - TheCaptureCal%m_RSIADistributeToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                    (TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_RSIADistributeToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                    write(*,*) "the SIA distribution sphere had existed the box "
                    write(*,*) "cascade center: ",TheCaptureCal%m_CascadeCenter(IBox,I)
                    write(*,*) "SIA distribution sphere radius: ",TheCaptureCal%m_RSIADistributeToCent(IBox)
                    write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                    pause
                    stop
                end if

                if(TheCaptureCal%CheckOutAbsorb .eq. .true.) then
                    if((TheCaptureCal%m_CascadeCenter(IBox,I) - TheCaptureCal%m_ROutAbsorbToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                        (TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_ROutAbsorbToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                        write(*,*) "the outer absorb sphere had existed the box "
                        write(*,*) "cascade center: ",TheCaptureCal%m_CascadeCenter(IBox,I)
                        write(*,*) " outer absorb sphere radius: ",TheCaptureCal%m_ROutAbsorbToCent(IBox)
                        write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                        pause
                        stop
                    end if
                end if
            END DO


            if(TheCaptureCal%CheckSIARange .eq. .true.) then
                if(TheCaptureCal%m_maxDistance(IBox) .GT. TheCaptureCal%m_RSIADistributeToCent(IBox)) then
                    write(*,*) "MCPSCUERROR: The mostly out vac had exist the SIA distribution position",TheCaptureCal%m_maxDistance(IBox),TheCaptureCal%m_RSIADistributeToCent(IBox)
                    pause
                    stop
                end if
            end if

            if(TheCaptureCal%m_maxDistance(IBox) .GT. TheCaptureCal%m_ROutAbsorbToCent(IBox)) then
                write(*,*) "MCPSCUERROR: The mostly out vac had exist the out absorb",TheCaptureCal%m_maxDistance(IBox),TheCaptureCal%m_ROutAbsorbToCent(IBox)
                pause
                stop
            end if

            if(TheCaptureCal%m_RSIADistributeToCent(IBox) .GT. TheCaptureCal%m_ROutAbsorbToCent(IBox)) then
                write(*,*) "MCPSCUERROR: The SIA distribution position had existed the out absorb",TheCaptureCal%m_RSIADistributeToCent(IBox),TheCaptureCal%m_ROutAbsorbToCent(IBox)
                pause
                stop
            end if

        END DO

        !*******Put in SIA ****************************************
        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,TheCaptureCal%NSIA)

        DO IBox = 1,1

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

            if((NC - NCUsed) .LT. TheCaptureCal%NSIA) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
                write(*,*) "The waiting to be implanted clusters number is:",TheCaptureCal%NSIA
                pause
                stop
            end if

            ICFrom = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - TheCaptureCal%NSIA + 1
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
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = TheCaptureCal%SIASIZE
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

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


                select case(TheCaptureCal%SIASPaceModel)
                    case(Capture_SIASPaceModel_Type_UnderSphereFace)
                        ArrowLen = 0.D0
                        DO I = 1,3
                            Vector(I) = DRAND32() - 0.5D0
                            ArrowLen = ArrowLen + Vector(I)*Vector(I)
                        END DO
                        ArrowLen = DSQRT(ArrowLen)

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_RSIADistributeToCent(IBox)*Vector(I)/ArrowLen
                        END DO

                    case(Capture_SIASPaceModel_Type_UniformOutRSIA)

                        exitFlag = .false.
                        DO While(exitFlag .eq. .false.)
                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                            END DO

                            SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - TheCaptureCal%m_CascadeCenter(IBox,1:3)

                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            exitFlag = .true.

                            if(Distance .LE. TheCaptureCal%m_RSIADistributeToCent(IBox)) then
                                exitFlag = .false.
                            end if

                            DO I = 1,3
                                if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                                    (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                                    exitFlag = .false.
                                end if

                            END DO

                        END DO


                    case(Capture_SIASPaceModel_Type_UniformWholeBox)

                        exitFlag = .false.
                        DO While(exitFlag .eq. .false.)
                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                            END DO

                            exitFlag = .true.

                            DO I = 1,3
                                if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                                    (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                                    exitFlag = .false.
                                end if

                            END DO

                        END DO

                    case default
                        write(*,*) "MCPSCUERROR: Unknown SIA space distribution model: ",TheCaptureCal%SIASPaceModel
                        pause
                        stop
                end select

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

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


        DO IBox = 2,MultiBox

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

            if((NC - NCUsed) .LT. TheCaptureCal%NSIA) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
                write(*,*) "The waiting to be implanted clusters number is:",TheCaptureCal%NSIA
                pause
                stop
            end if

            ICFrom = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - TheCaptureCal%NSIA + 1
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

                if(TheCaptureCal%SIADistSameBetweenBoxes .eq. .true.) then
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC-Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox-1,2))
                else
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = TheCaptureCal%SIASIZE
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

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


                    select case(TheCaptureCal%SIASPaceModel)
                        case(Capture_SIASPaceModel_Type_UnderSphereFace)
                            ArrowLen = 0.D0
                            DO I = 1,3
                                Vector(I) = DRAND32() - 0.5D0
                                ArrowLen = ArrowLen + Vector(I)*Vector(I)
                            END DO
                            ArrowLen = DSQRT(ArrowLen)

                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_RSIADistributeToCent(IBox)*Vector(I)/ArrowLen
                            END DO

                        case(Capture_SIASPaceModel_Type_UniformOutRSIA)

                            exitFlag = .false.
                            DO While(exitFlag .eq. .false.)
                                DO I = 1,3
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                                END DO

                                SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - TheCaptureCal%m_CascadeCenter(IBox,1:3)

                                Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                                Distance = DSQRT(Distance)

                                exitFlag = .true.

                                if(Distance .LE. TheCaptureCal%m_RSIADistributeToCent(IBox)) then
                                    exitFlag = .false.
                                end if

                                DO I = 1,3
                                    if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                                        (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                                        exitFlag = .false.
                                    end if

                                END DO

                            END DO


                        case(Capture_SIASPaceModel_Type_UniformWholeBox)

                            exitFlag = .false.
                            DO While(exitFlag .eq. .false.)
                                DO I = 1,3
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                                END DO

                                exitFlag = .true.

                                DO I = 1,3
                                    if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                                        (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                                        exitFlag = .false.
                                    end if

                                END DO

                            END DO

                        case default
                            write(*,*) "MCPSCUERROR: Unknown SIA space distribution model: ",TheCaptureCal%SIASPaceModel
                            pause
                            stop
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = &
                        Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = 1
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

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

                end if

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
        write(hOutInfo, fmt="(130(A30,1x))") "IBox",                        &
                                             "NSIA",                        &
                                             "NVAC",                        &
                                             "CascadeCenter_X(LU)",         &
                                             "CascadeCenter_Y(LU)",         &
                                             "CascadeCenter_Z(LU)",         &
                                             "maxVACDistanceToCent(LU)",    &
                                             "RSIADistributeToCent(LU)",    &
                                             "ROutAbsorbToCent(LU)"

        DO IBox = 1,MultiBox
            write(hOutInfo,fmt="(3(I30,1x),6(1PE30.10,1x))") IBox,                                                              &
                                                             TheCaptureCal%NSIA,                                                &
                                                             TheCaptureCal%m_NVACInEachBox(IBox),                               &
                                                             TheCaptureCal%m_CascadeCenter(IBox,1:3)/Host_Boxes%LatticeLength,  &
                                                             TheCaptureCal%m_maxDistance(IBox)/Host_Boxes%LatticeLength,        &
                                                             TheCaptureCal%m_RSIADistributeToCent(IBox)/Host_Boxes%LatticeLength,     &
                                                             TheCaptureCal%m_ROutAbsorbToCent(IBox)/Host_Boxes%LatticeLength
        END DO


        call Host_Boxes%Clean()

        close(hCfgOut)
        close(hOutInfo)

        return








    end subroutine Generate_Capture_Locally_CentUniform


    !***************************************************************
    subroutine Generate_Capture_Locally_FormMCConfig_Directly(hCfgOut,hOutInfo,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hCfgOut
        integer,intent(in)::hOutInfo
        type(CaptureCal)::TheCaptureCal
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFrom
        integer::ICTo
        integer::SIAIndex
        integer::VacancyIndex
        real(kind=KINDDF)::SEP(3)
        real(kind=KINDDF)::Distance
        integer::I
        type(DiffusorValue)::TheDiffusorValue
        integer::NCUsed
        integer::NC
        integer::RecordIndex
        character*30::TheVersion
        real(kind=KINDDF)::ArrowLen
        real(kind=KINDDF)::Vector(3)
        logical::exitFlag
        !-----------Body--------------
        if(.not. allocated(TheCaptureCal%m_CascadeCenter) .or. &
           .not. allocated(TheCaptureCal%m_maxDistance) .or.   &
           .not. allocated(TheCaptureCal%m_NVACInEachBox) .or. &
           .not. allocated(TheCaptureCal%m_RSIADistributeToCent) .or. &
           .not. allocated(TheCaptureCal%m_ROutAbsorbToCent)) then
           write(*,*) "MCPSCUERROR: You must initial the CaptureCal object before Generate the capture configuration file"
           pause
           stop
        end if

        call resolveAddOnData(Host_Boxes,Host_SimuCtrlParamList%theSimulationCtrlParam)
        call resolveModelRelativeData(Host_SimuCtrlParamList%theSimulationCtrlParam%ModelData,Host_Boxes%Atoms_list)

        call Host_Boxes%PutinCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record,TheCaptureCal%MCCfgPath,m_FREESURDIFPRE,m_GBSURDIFPRE,TheVersion,AsInitial=.true.)

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


        !*******Get Infor mation from configuration ****************************************
        DO IBox = 1,MultiBox
            ICFrom = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTo = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            TheCaptureCal%m_NVACInEachBox(IBox) = 0
            TheCaptureCal%m_CascadeCenter(IBox,1:3) = 0.D0
            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO I = 1,3
                        TheCaptureCal%m_CascadeCenter(IBox,I) = TheCaptureCal%m_CascadeCenter(IBox,I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                    END DO

                    TheCaptureCal%m_NVACInEachBox(IBox) = TheCaptureCal%m_NVACInEachBox(IBox) + 1
                end if

            END DO

            if(TheCaptureCal%m_NVACInEachBox(IBox) .GT. 0) then
                TheCaptureCal%m_CascadeCenter(IBox,1:3) = TheCaptureCal%m_CascadeCenter(IBox,1:3)/TheCaptureCal%m_NVACInEachBox(IBox)
            end if

            TheCaptureCal%m_maxDistance(IBox) = -1
            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - TheCaptureCal%m_CascadeCenter(IBox,1:3)

                    Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                    Distance = DSQRT(Distance)

                    if(Distance .GT. TheCaptureCal%m_maxDistance(IBox)) then
                        TheCaptureCal%m_maxDistance(IBox) = Distance
                    end if
                end if
            END DO

            !***************The SIA distribution radius and outer absorb radius**************************************
            select case(TheCaptureCal%RSIADistributeToCent_Type)
                case(Capture_RSIADistributeToCent_Type_FixedValue)
                    TheCaptureCal%m_RSIADistributeToCent(IBox) = TheCaptureCal%RSIADistributeToCent_Value
                case(Capture_RSIADistributeToCent_Type_OutFromMostOutVAC)
                    TheCaptureCal%m_RSIADistributeToCent(IBox) = TheCaptureCal%m_maxDistance(IBox) + TheCaptureCal%RSIADistributeToCent_Value
                case default
                    write(*,*) "MCPSCUERROR: Unknown SIA distribution to center radius type: ",TheCaptureCal%RSIADistributeToCent_Type
                    pause
                    stop
            end select

            select case(TheCaptureCal%ROutAbsorbToCent_Type)
                case(Capture_ROutAbsorbToCent_Type_FixedValue)
                    TheCaptureCal%m_ROutAbsorbToCent(IBox) = TheCaptureCal%ROutAbsorbToCent_Value
                case(Capture_ROutAbsorbToCent_Type_OutFromMostOutVAC)
                    TheCaptureCal%m_ROutAbsorbToCent(IBox) = TheCaptureCal%m_maxDistance(IBox) + TheCaptureCal%ROutAbsorbToCent_Value
                case default
                    write(*,*) "MCPSCUERROR: Unknown SIA distribution to center radius type: ",TheCaptureCal%ROutAbsorbToCent_Type
                    pause
                    stop
            end select

            DO I = 1,3
                if((TheCaptureCal%m_CascadeCenter(IBox,I) - TheCaptureCal%m_RSIADistributeToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                   (TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_RSIADistributeToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                    write(*,*) "the SIA distribution sphere had existed the box "
                    write(*,*) "cascade center: ",TheCaptureCal%m_CascadeCenter(IBox,I)
                    write(*,*) "SIA distribution sphere radius: ",TheCaptureCal%m_RSIADistributeToCent(IBox)
                    write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                    pause
                    stop
                end if

                if(TheCaptureCal%CheckOutAbsorb .eq. .true.) then
                    if((TheCaptureCal%m_CascadeCenter(IBox,I) - TheCaptureCal%m_ROutAbsorbToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                        (TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_ROutAbsorbToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                        write(*,*) "the outer absorb sphere had existed the box "
                        write(*,*) "cascade center: ",TheCaptureCal%m_CascadeCenter(IBox,I)
                        write(*,*) " outer absorb sphere radius: ",TheCaptureCal%m_ROutAbsorbToCent(IBox)
                        write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                        pause
                        stop
                    end if
                end if
            END DO

            if(TheCaptureCal%CheckSIARange .eq. .true.) then
                if(TheCaptureCal%m_maxDistance(IBox) .GT. TheCaptureCal%m_RSIADistributeToCent(IBox)) then
                    write(*,*) "MCPSCUERROR: The mostly out vac had exist the SIA distribution position",TheCaptureCal%m_maxDistance(IBox),TheCaptureCal%m_RSIADistributeToCent(IBox)
                    pause
                    stop
                end if
            end if

            if(TheCaptureCal%m_maxDistance(IBox) .GT. TheCaptureCal%m_ROutAbsorbToCent(IBox)) then
                write(*,*) "MCPSCUERROR: The mostly out vac had exist the out absorb",TheCaptureCal%m_maxDistance(IBox),TheCaptureCal%m_ROutAbsorbToCent(IBox)
                pause
                stop
            end if

            if(TheCaptureCal%m_RSIADistributeToCent(IBox) .GT. TheCaptureCal%m_ROutAbsorbToCent(IBox)) then
                write(*,*) "MCPSCUERROR: The SIA distribution position had existed the out absorb",TheCaptureCal%m_RSIADistributeToCent(IBox),TheCaptureCal%m_ROutAbsorbToCent(IBox)
                pause
                stop
            end if

        END DO

        !*******Put in SIA ****************************************
        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,TheCaptureCal%NSIA)

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

            if((NC - NCUsed) .LT. TheCaptureCal%NSIA) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
                write(*,*) "The waiting to be implanted clusters number is:",TheCaptureCal%NSIA
                pause
                stop
            end if

            ICFrom = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - TheCaptureCal%NSIA + 1
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
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = TheCaptureCal%SIASIZE
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU


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

                select case(TheCaptureCal%SIASPaceModel)
                    case(Capture_SIASPaceModel_Type_UnderSphereFace)
                        ArrowLen = 0.D0
                        DO I = 1,3
                            Vector(I) = DRAND32() - 0.5D0
                            ArrowLen = ArrowLen + Vector(I)*Vector(I)
                        END DO
                        ArrowLen = DSQRT(ArrowLen)

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = TheCaptureCal%m_CascadeCenter(IBox,I) + TheCaptureCal%m_RSIADistributeToCent(IBox)*Vector(I)/ArrowLen
                        END DO

                    case(Capture_SIASPaceModel_Type_UniformOutRSIA)

                        exitFlag = .false.
                        DO While(exitFlag .eq. .false.)
                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                            END DO

                            SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - TheCaptureCal%m_CascadeCenter(IBox,1:3)

                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            exitFlag = .true.

                            if(Distance .LE. TheCaptureCal%m_RSIADistributeToCent(IBox)) then
                                exitFlag = .false.
                            end if

                            DO I = 1,3
                                if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                                    (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                                    exitFlag = .false.
                                end if

                            END DO

                        END DO


                    case(Capture_SIASPaceModel_Type_UniformWholeBox)

                        exitFlag = .false.
                        DO While(exitFlag .eq. .false.)
                            DO I = 1,3
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                            END DO

                            exitFlag = .true.

                            DO I = 1,3
                                if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .LT. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                                    (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD) .GT. Host_Boxes%BOXBOUNDARY(I,2)) then
                                    exitFlag = .false.
                                end if

                            END DO

                        END DO

                    case default
                        write(*,*) "MCPSCUERROR: Unknown SIA space distribution model: ",TheCaptureCal%SIASPaceModel
                        pause
                        stop
                end select


                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = RecordIndex + 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

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
        write(hOutInfo, fmt="(130(A30,1x))") "IBox",                        &
                                             "NSIA",                        &
                                             "NVAC",                        &
                                             "CascadeCenter_X(LU)",         &
                                             "CascadeCenter_Y(LU)",         &
                                             "CascadeCenter_Z(LU)",         &
                                             "maxVACDistanceToCent(LU)",    &
                                             "RSIADistributeToCent(LU)",    &
                                             "ROutAbsorbToCent(LU)"

        DO IBox = 1,MultiBox
            write(hOutInfo,fmt="(3(I30,1x),6(1PE30.10,1x))") IBox,                                                              &
                                                             TheCaptureCal%NSIA,                                                &
                                                             TheCaptureCal%m_NVACInEachBox(IBox),                               &
                                                             TheCaptureCal%m_CascadeCenter(IBox,1:3)/Host_Boxes%LatticeLength,  &
                                                             TheCaptureCal%m_maxDistance(IBox)/Host_Boxes%LatticeLength,        &
                                                             TheCaptureCal%m_RSIADistributeToCent(IBox)/Host_Boxes%LatticeLength,     &
                                                             TheCaptureCal%m_ROutAbsorbToCent(IBox)/Host_Boxes%LatticeLength
        END DO


        call Host_Boxes%Clean()

        close(hCfgOut)
        close(hOutInfo)

        return
    end subroutine Generate_Capture_Locally_FormMCConfig_Directly

end module MC_ConstructCaptureBox


program Main_MC_ConstructCaptureBox
    use MC_ConstructCaptureBox
    use MCLIB_GLOBAL
    use MCLIB_UTILITIES
    use MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    !---Local Vars
    integer::arg_Num
    character*1000::SampleFile
    character*1000::ARG
    type(CaptureCal)::m_CaptureCal
    type(SimulationBoxes)::m_Boxes
    type(SimulationCtrlParamList)::m_SimuCtrlParamList
    type(MigCoalClusterRecord)::m_Record
    !--Body---
    arg_Num = Command_Argument_Count()

    if(arg_Num .LT. 1) then
        write(*,*) "MCPSCUERROR: You must special the sample file"
        pause
        stop
    end if

    call Get_Command_Argument(0,ARG)

    call Get_Command_Argument(1,ARG)
    Read(ARG,fmt="(A256)") SampleFile
    write(*,*) "The Sample file is: ",SampleFile

    call Generate_Capture(m_CaptureCal,m_Boxes,m_SimuCtrlParamList,m_Record)


    return

end program Main_MC_ConstructCaptureBox
