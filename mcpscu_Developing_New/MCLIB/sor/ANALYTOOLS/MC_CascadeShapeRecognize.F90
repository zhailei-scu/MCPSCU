module MC_CascadeShapeRecognize
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
    subroutine CascadeShapeRecognize(ConfigFile,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)
        implicit none
        !---Dummy Vars---
        character*(*)::ConfigFile
        type(CaptureCal)::TheCaptureCal
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        character*1000::OutFolder
        character*1000::InfoOut
        character*30::TheVersion
        integer::hOutInfo
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

        InfoOut = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"CaptureBoxShapeRecognize.dat"

        hOutInfo = CreateNewFile(InfoOut)

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call TheCaptureCal%Init(Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call TheCaptureCal%ResolveCapCtrlFile(Host_Boxes,Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Cal_CascadeShapeRecognize(ConfigFile,hOutInfo,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)

        return
    end subroutine CascadeShapeRecognize

    !***************************************************************
    subroutine Cal_CascadeShapeRecognize(ConfigFile,hOutInfo,TheCaptureCal,Host_Boxes,Host_SimuCtrlParamList,Record)
        implicit none
        !---Dummy Vars---
        character*(*)::ConfigFile
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
        integer::J
        real(kind=KINDDF)::Sep_X
        real(kind=KINDDF)::Sep_Y
        real(kind=KINDDF)::Sep_Z
        real(kind=KINDDF)::RadSum
        real(kind=KINDDF)::Dist
        type(DiffusorValue)::TheDiffusorValue
        integer::NCUsed
        integer::NC
        integer::RecordIndex
        character*30::TheVersion
        real(kind=KINDDF)::ArrowLen
        real(kind=KINDDF)::Vector(3)
        logical::exitFlag
        real(kind=KINDDF)::maxSEP_Dim3(3)
        real(kind=KINDDF)::maxSEP_Dim2(3)
        real(kind=KINDDF)::maxSEP_Dim1(3)
        real(kind=KINDDF)::maxLenPW2(3)
        real(kind=KINDDF)::maxLen(3)
        real(kind=KINDDF)::tempSep(3)
        real(kind=KINDDF),dimension(:,:),allocatable::projectPos_Dim2
        real(kind=KINDDF),dimension(:,:),allocatable::projectPos_Dim1
        real(kind=KINDDF)::Ratio
        real(kind=KINDDF)::maxDistance_Dim3
        integer::IDLeftMaxDist_Dim3
        integer::IDRightMaxDist_Dim3
        real(kind=KINDDF)::maxDistance_Dim2
        integer::IDLeftMaxDist_Dim2
        integer::IDRightMaxDist_Dim2
        real(kind=KINDDF)::maxDistance_Dim1
        integer::IDLeftMaxDist_Dim1
        integer::IDRightMaxDist_Dim1
        integer::JC
        real(kind=KINDDF)::threshold
        integer::TotalEffectDim
        real(kind=KINDDF)::tempValue
        real(kind=KINDDF)::gap
        character*256::TheFormat
        !-----------Body--------------

        threshold = 0.2D0

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

        call Host_Boxes%Putin_OKMC_OUTCFG_FORMAT18(adjustl(trim(ConfigFile)),Host_SimuCtrlParamList%theSimulationCtrlParam,Record,TheVersion,m_FREESURDIFPRE,m_GBSURDIFPRE,AsInitial=.true.,&
                                                   CheckBoxSize = .false.)

        write(*,*) "The KMC Configuration version is: ",TheVersion

        MultiBox = Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
        if(MultiBox .LE. 0) then
            write(*,*) "MCPSCUERROR: The box number less than 1"
            pause
            stop
        end if


        write(hOutInfo, fmt="(130(A30,1x))") "IBox",                    &
                                             "DimLength1(LU)",          &
                                             "DimLength2(LU)",          &
                                             "DimLength3(LU)",          &
                                             "Num_EffectDim",           &
                                             "threshold"



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


        !*******Get Information from configuration ****************************************
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
        END DO

        !***************************************************************

        DO IBox = 1,MultiBox

            !************************Dim3**************************
            maxDistance_Dim3 = -1.D0
            IDLeftMaxDist_Dim3 = 0
            IDRightMaxDist_Dim3 = 0

            ICFrom = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTo = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            if((ICTo - ICFrom) .GE. 0) then
                call DeAllocateArray_Host(projectPos_Dim2,"projectPos_Dim2")
                call AllocateArray_Host(projectPos_Dim2,ICTo - ICFrom + 1,3,"projectPos_Dim2")

                call DeAllocateArray_Host(projectPos_Dim1,"projectPos_Dim1")
                call AllocateArray_Host(projectPos_Dim1,ICTo - ICFrom + 1,3,"projectPos_Dim1")
            end if

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO JC = IC+1,ICTo
                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                            (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                             Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                            SEP = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS
                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            if(Distance .GT. maxDistance_Dim3) then
                                maxDistance_Dim3 = Distance
                                IDLeftMaxDist_Dim3 = IC
                                IDRightMaxDist_Dim3 = JC
                            end if

                        end if

                    END DO

                end if
            END DO

            maxSEP_Dim3 = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IDLeftMaxDist_Dim3)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IDRightMaxDist_Dim3)%m_POS

            maxLenPW2(3) = sum(maxSEP_Dim3**2)

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    tempSep = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IDLeftMaxDist_Dim3)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS

                    Ratio = sum(maxSEP_Dim3*tempSep)/maxLenPW2(3)

                    projectPos_Dim2(IC-ICFrom+1,1:3) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS + Ratio*maxSEP_Dim3
                end if

            END DO


            !************************Dim2**************************
            maxDistance_Dim2 = -1.D0
            IDLeftMaxDist_Dim2 = 0
            IDRightMaxDist_Dim2 = 0

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO JC = IC+1,ICTo
                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                            (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                             Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                            SEP = projectPos_Dim2(IC-ICFrom+1,1:3) - projectPos_Dim2(JC-ICFrom+1,1:3)
                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            if(Distance .GT. maxDistance_Dim2) then
                                maxDistance_Dim2 = Distance
                                IDLeftMaxDist_Dim2 = IC - ICFrom + 1
                                IDRightMaxDist_Dim2 = JC - ICFrom + 1
                            end if

                        end if

                    END DO

                end if
            END DO

            maxSEP_Dim2 = projectPos_Dim2(IDLeftMaxDist_Dim2,1:3) - projectPos_Dim2(IDRightMaxDist_Dim2,1:3)

            maxLenPW2(2) = sum(maxSEP_Dim2**2)

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    tempSep = projectPos_Dim2(IDLeftMaxDist_Dim2,1:3) - projectPos_Dim2(IC - ICFrom + 1,1:3)

                    Ratio = sum(maxSEP_Dim2*tempSep)/maxLenPW2(2)

                    projectPos_Dim1(IC-ICFrom+1,1:3) = projectPos_Dim2(IC - ICFrom + 1,1:3) + Ratio*maxSEP_Dim2
                end if

            END DO


            !************************Dim1**************************
            maxDistance_Dim1 = -1.D0
            IDLeftMaxDist_Dim1 = 0
            IDRightMaxDist_Dim1 = 0

            DO IC = ICFrom,ICTo
                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                   (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                    DO JC = IC+1,ICTo
                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Atoms(VacancyIndex)%m_NA .GT. 0 .AND. &
                            (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                             Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_Statu .eq. p_ACTIVEINGB_STATU)) then

                            SEP = projectPos_Dim1(IC-ICFrom+1,1:3) - projectPos_Dim1(JC-ICFrom+1,1:3)
                            Distance = SEP(1)*SEP(1) + SEP(2)*SEP(2) + SEP(3)*SEP(3)
                            Distance = DSQRT(Distance)

                            if(Distance .GT. maxDistance_Dim1) then
                                maxDistance_Dim1 = Distance
                                IDLeftMaxDist_Dim1 = IC - ICFrom + 1
                                IDRightMaxDist_Dim1 = JC - ICFrom + 1
                            end if

                        end if

                    END DO

                end if
            END DO

            maxSEP_Dim1 = projectPos_Dim1(IDLeftMaxDist_Dim1,1:3) - projectPos_Dim1(IDRightMaxDist_Dim1,1:3)

            maxLenPW2(1) = sum(maxSEP_Dim1**2)

            maxLen = DSQRT(maxLenPW2)


            DO I = 1,size(maxLen)

                DO J = 1,size(maxLen) - 1

                    if(maxLen(J) .LT. maxLen(J+1)) then
                        tempValue = maxLen(J+1)
                        maxLen(J+1) = maxLen(J)
                        maxLen(J) = tempValue
                    end if
                END DO
            END DO

            TotalEffectDim = 1
            DO I = 1,size(maxLen) - 1
                gap = maxLen(I) - maxLen(I+1)

                if(abs(gap)/maxLen(I) .GT. threshold) then
                    TotalEffectDim = TotalEffectDim + 1
                end if
            END DO


            TheFormat = "(I30,1x,3(1PE30.10,1x),I30,1x,1(1PE30.10,1x))"
            TheFormat = adjustl(TheFormat)
            write(hOutInfo,fmt=TheFormat(1:LENTRIM(TheFormat))) IBox,                                        &
                                                                maxLen(1:3)/Host_Boxes%LatticeLength,        &
                                                                TotalEffectDim,                              &
                                                                threshold

            call DeAllocateArray_Host(projectPos_Dim2,"projectPos_Dim2")
            call DeAllocateArray_Host(projectPos_Dim1,"projectPos_Dim1")
        END DO


        !************Out the Capture info***************


        DO IBox = 1,MultiBox

        END DO


        call Host_Boxes%Clean()


        close(hOutInfo)

        return
    end subroutine Cal_CascadeShapeRecognize

end module MC_CascadeShapeRecognize



program Main_MC_CascadeShapeRecognize
    use MC_CascadeShapeRecognize
    use MCLIB_GLOBAL
    use MCLIB_UTILITIES
    use MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    !---Local Vars
    integer::arg_Num
    character*1000::SampleFile
    character*1000::ConfigFile
    character*1000::ARG
    type(CaptureCal)::m_CaptureCal
    type(SimulationBoxes)::m_Boxes
    type(SimulationCtrlParamList)::m_SimuCtrlParamList
    type(MigCoalClusterRecord)::m_Record
    !--Body---
    arg_Num = Command_Argument_Count()

    if(arg_Num .LT. 2) then
        write(*,*) "MCPSCUERROR: You must special the sample file and MC configuration file."
        pause
        stop
    end if

    call Get_Command_Argument(0,ARG)

    call Get_Command_Argument(1,ARG)

    Read(ARG,fmt="(A256)") SampleFile
    write(*,*) "The Sample file is: ",SampleFile

    call Get_Command_Argument(2,ARG)
    Read(ARG,fmt="(A256)") ConfigFile
    write(*,*) "The Configuration file is: ",ConfigFile

    call CascadeShapeRecognize(ConfigFile,m_CaptureCal,m_Boxes,m_SimuCtrlParamList,m_Record)


    return

end program Main_MC_CascadeShapeRecognize
