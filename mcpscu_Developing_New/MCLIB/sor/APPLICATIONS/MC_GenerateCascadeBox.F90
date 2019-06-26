program Main_MC_GenerateCascadeBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE
    implicit none
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MigCoalClusterRecord)::Record
    character*256::OutFolder
    integer::err
    !---Parameters---
    integer::MultiBox
    integer::CascadeNum
    integer::ClusterNumOneCase
    real(kind=KMCDF),dimension(:,:),allocatable::Sphere_Central
    real(kind=KMCDF),dimension(:),allocatable::Sphere_Radius
    integer::I
    integer::IBox
    integer::ICase
    integer::IC
    integer::IIC
    integer::processid
    integer::ISEED0,ISEED(2)
    integer::SIAIndex
    integer::VacancyIndex
    !-----------Body--------------
    processid = 0

    CascadeNum = 4

    ClusterNumOneCase = 80

    call AllocateArray_Host(Sphere_Central,CascadeNum,3,"Sphere_Central")
    call AllocateArray_Host(Sphere_Radius,CascadeNum,"Sphere_Radius")

    !*********Create/Open log file********************
    call OpenLogFile(m_hFILELOG)

    !********Load Global vars from input file**************
    call Initialize_Global_Variables(Host_SimuCtrlParam,Host_Boxes)


    ISEED0 = Host_SimuCtrlParam%RANDSEED(1)
    call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
    ISEED0 = ISEED0 + processid - 1
    call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
    call DRAND32_PUTSEED(ISEED)

    call Print_Global_Variables(6,Host_SimuCtrlParam,Host_Boxes)

    OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParam%OutFilePath))//"CascadeBox/")

    Host_SimuCtrlParam%OutFilePath = trim(adjustl(OutFolder))

    call Host_Boxes%m_ClustersInfo_CPU%Clean()

    call Host_Boxes%InitSimulationBox(Host_SimuCtrlParam)

    call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParam,2*CascadeNum*ClusterNumOneCase)

    SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
    VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

    Sphere_Radius = 80*Host_Boxes%LatticeLength

    IC = 0
    DO IBox = 1,Host_SimuCtrlParam%MultiBox
        DO ICase = 1,CascadeNum
            Sphere_Central(ICase,1) = Host_Boxes%BOXBOUNDARY(1,1) + DRAND32()*Host_Boxes%BOXSIZE(1)
            Sphere_Central(ICase,2) = Host_Boxes%BOXBOUNDARY(2,1) + DRAND32()*Host_Boxes%BOXSIZE(2)
            Sphere_Central(ICase,3) = Host_Boxes%BOXBOUNDARY(3,1) + DRAND32()*Host_Boxes%BOXSIZE(3)

            DO IIC = 1,ClusterNumOneCase

                IC = IC + 1
                call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                DO I = 1,3
                    DO While(.true.)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Sphere_Central(ICase,I) + (2*DRAND32()-1.D0)*Sphere_Radius(ICase)

                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                        else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                        end if

                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GE. Host_Boxes%BOXBOUNDARY(I,1) .AND. &
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LE. Host_Boxes%BOXBOUNDARY(I,2)) then
                            exit
                        end if
                    END DO
                END DO

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1


                IC = IC + 1
                call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU
                DO I = 1,3
                    DO While(.true.)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Sphere_Central(ICase,I) + (2*DRAND32()-1.D0)*Sphere_Radius(ICase)

                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                        else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                        end if

                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GE. Host_Boxes%BOXBOUNDARY(I,1) .AND. &
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LE. Host_Boxes%BOXBOUNDARY(I,2)) then
                            exit
                        end if
                    END DO
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

    call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

    call DeAllocateArray_Host(Sphere_Central,"Sphere_Central")
    call DeAllocateArray_Host(Sphere_Radius,"Sphere_Radius")

    call Host_Boxes%Clean()

    return
end program Main_MC_GenerateCascadeBox
