module MCRT_Method_MIGCOALE_CLUSTER_CPU
    !--- Description:                                                                 ---!
    !--- This module is created for the KMC method based on migration-coalesence model---!
    !--- for CLUSTER object                                                           ---!
    !--- Creator: Zhai Lei, 2018-05-23, in Sichuan University                         ---!
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE
    use MCLIB_CAL_NEIGHBOR_LIST_GPU
    use MIGCOALE_EVOLUTION_GPU
    use MIGCOALE_TIMECTL
    use MCLIB_GLOBAL_GPU
    use MIGCOALE_STATISTIC_GPU
    use MIGCOALE_STATISTIC_CPU
    use MIGCOALE_TYPEDEF_SIMRECORD
    use INLET_TYPEDEF_IMPLANTLIST
    use HYBRIDMCRT_TYPEDEF_COLLECTIONS
    implicit none

    integer, parameter, private::p_ClusterIniConfig_Simple = 0
    integer, parameter, private::p_ClusterIniConfig_SpecialDistFromFile = 1
    integer, parameter, private::p_ClusterIniConfig_SpecialDistFromExteFunc = 2

    character(len=25),  private:: m_INIFSTARTFLAG = "&INITINPUTF"

    !--------------------
    type,public::InitBoxSimCfg

        integer::InitType = -1

        integer,dimension(:),allocatable::NClusters

        character*1000::InitCfgFileName = ""

        character*10::Elemets(p_ATOMS_GROUPS_NUMBER) = ""

        real(kind=KINDDF)::NAINI = 0.D0

        real(kind=KINDDF)::NASDINI = 0.D0

        real(kind=KINDDF)::NACUT(2) = 0.D0

        real(kind=KINDDF)::CompositWeight(p_ATOMS_GROUPS_NUMBER) = 0.D0

        integer::InitDepthDistType = -1

        real(kind=KINDDF)::DepthINI = 0.D0

        real(kind=KINDDF)::DepthSDINI = 0.D0

        real(kind=KINDDF)::SUBBOXBOUNDARY(3,2) = 0.D0

        real(kind=KINDDF),dimension(:),allocatable::LayerThick

        real(kind=KINDDF),dimension(:),allocatable::PNCLayers

        contains
        procedure,non_overridable,public,pass::CopyInitBoxSimCfgFromOther
        procedure,non_overridable,public,pass::Clean_InitBoxSimCfg
        Generic::Assignment(=)=>CopyInitBoxSimCfgFromOther
        Final::CleanInitBoxSimCfg
    end type

    !--------------------
    type,public::InitBoxSimCfgList
        type(InitBoxSimCfg)::TheValue
        integer::ListCount = 0
        type(InitBoxSimCfgList),pointer::next=>null()

        contains
        procedure,non_overridable,public,pass::AppendOne_InintSimBoxCfg
        procedure,non_overridable,public,pass::GetList_Count=>GetInitBoxSimCfgList_Count
        procedure,non_overridable,public,pass::Clean_InitBoxSimCfgList
        Final::CleanInitBoxSimCfgList
    end type



    type(InitBoxSimCfgList),target::m_InitBoxSimCfgList
    type(ImplantList)::m_ImplantList
    type(MigCoalClusterRecord)::m_MigCoalClusterRecord
    type(MigCoaleStatInfoWrap),private::m_MigCoaleStatInfoWrap
    type(MigCoale_GVarsDev),private::m_MigCoale_GVarsDev
    type(HybridCollections),private::m_MCRTHybridCollections

    private::CopyInitBoxSimCfgFromOther
    private::Clean_InitBoxSimCfg
    private::CleanInitBoxSimCfg
    private::AppendOne_InintSimBoxCfg
    private::GetInitBoxSimCfgList_Count
    private::ReadInitBoxSimCfgList
    private::ReadInitBoxSimRecord
    private::Clean_InitBoxSimCfgList
    private::CleanInitBoxSimCfgList

    contains

    !*****************************************************************
    subroutine For_One_Test(Host_SimBoxes,Host_SimuCtrlParamList,Dev_Boxes,JobIndex)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_SimBoxes
        type(SimulationCtrlParamList),target::Host_SimuCtrlParamList
        type(SimulationBoxes_GPU)::Dev_Boxes
        integer,intent(in)::JobIndex
        !---Local Vars---
        type(SimulationCtrlParamList),pointer::cursor=>null()
        type(ImplantSection),pointer::PImplantSection=>null()
        type(InitBoxSimCfg)::tempInitBoxSimCfg
        type(MigCoalClusterRecord)::tempMigCoalClusterRecord
        integer::ITEST
        integer::TestLoop0,TestLoop1
        integer::I
        integer::SEEDBefore(2)
        integer::ISEED0
        integer::ISEED(2)
        logical::exitflag
        !---Body---
        call tempMigCoalClusterRecord%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        if(Host_SimuCtrlParamList%theSimulationCtrlParam%RESTARTAT .GT. 0) then
            call m_InitBoxSimCfgList%Clean_InitBoxSimCfgList()
            call tempInitBoxSimCfg%Clean_InitBoxSimCfg()
            tempInitBoxSimCfg%InitType = p_ClusterIniConfig_SpecialDistFromFile
            tempInitBoxSimCfg%InitCfgFileName = Host_SimuCtrlParamList%theSimulationCtrlParam%RestartCfg
            call m_InitBoxSimCfgList%AppendOne_InintSimBoxCfg(tempInitBoxSimCfg)
        else
            call ReadInitBoxSimCfgList(Host_SimBoxes,Host_SimuCtrlParamList%theSimulationCtrlParam,m_InitBoxSimCfgList,tempMigCoalClusterRecord)
        end if

        call ReadInitBoxSimRecord(Host_SimBoxes,m_InitBoxSimCfgList,tempMigCoalClusterRecord)

        if(Host_SimuCtrlParamList%theSimulationCtrlParam%RESTARTAT .GT. 0 .or. &
          tempMigCoalClusterRecord%GetSimuSteps() .GT. 0) then
            !---For restart, we should use the random number in the sequence before last running---
            !---However, it too hard to realize, currently, we should at least that the random number in---
            !---the restart job is not same with last running, so, it is necessary to change the random seed again---
            call DRAND32_GETSEED(SEEDBefore)
            DO While(.true.)
                exitflag = .true.

                ISEED0 = Host_SimuCtrlParamList%theSimulationCtrlParam%RANDSEED(1) + tempMigCoalClusterRecord%GetSimuSteps() + DRAND32()*RAND32SEEDLIB_SIZE

                call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
                ISEED0 = ISEED0 + JobIndex + tempMigCoalClusterRecord%GetTimeSections() - 1
                call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))

                DO I = 1,size(ISEED)
                    if(ISEED(I) .eq. SEEDBefore(I)) then
                        exitflag = .false.
                        exit
                    end if
                END DO

                if(exitflag .eq. .true.) then
                    call DRAND32_PUTSEED(ISEED)
                    exit
                end if
            END DO
        end if

        if(Host_SimuCtrlParamList%theSimulationCtrlParam%INDEPBOX) then
            if(JobIndex .LT. tempMigCoalClusterRecord%GetSimuPatch()) then
                return
            end if

            TestLoop0 = 1
            TestLoop1 = 1
        else
            TestLoop0 = tempMigCoalClusterRecord%GetSimuPatch()
            TestLoop1 = Host_SimuCtrlParamList%theSimulationCtrlParam%TOTALBOX/Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
        end if

        DO ITEST = TestLoop0,TestLoop1

            !---The Assignment had been override----
            m_MigCoalClusterRecord = tempMigCoalClusterRecord

            cursor=>Host_SimuCtrlParamList%Get_P(m_MigCoalClusterRecord%GetTimeSections())

            if(Host_SimuCtrlParamList%theSimulationCtrlParam%INDEPBOX) then
                call m_MigCoalClusterRecord%SetSimuPatch(JobIndex)
            else
                call m_MigCoalClusterRecord%SetSimuPatch(ITEST)
            end if

            if(ITEST .eq. 1) then

                call m_MigCoaleStatInfoWrap%Init(cursor%theSimulationCtrlParam%MultiBox)

                call resolveAddOnData(Host_SimBoxes,cursor%theSimulationCtrlParam)
                call resolveModelRelativeData(cursor%theSimulationCtrlParam%ModelData,Host_SimBoxes%Atoms_list)
                call InitSimulationBoxesConfig(Host_SimBoxes,cursor%theSimulationCtrlParam,m_InitBoxSimCfgList,m_MigCoaleStatInfoWrap,m_MigCoalClusterRecord)

                call TransformMCToRT(m_MCRTHybridCollections,Host_SimBoxes,Host_SimuCtrlParamList)
            end if

            DO While(.true.)

                write(*,*) "Start to evolution for time section ",m_MigCoalClusterRecord%GetTimeSections()

                call m_MigCoalClusterRecord%SetStartImplantTime(m_MigCoalClusterRecord%GetSimuTimes())

                call copyInPhyParamsConstant(cursor%theSimulationCtrlParam)

                call resolveAddOnData(Host_SimBoxes,cursor%theSimulationCtrlParam)

                call resolveModelRelativeData(cursor%theSimulationCtrlParam%ModelData,Host_SimBoxes%Atoms_list)

                call m_ImplantList%Init(Host_SimBoxes,cursor%theSimulationCtrlParam)

                call For_One_TimeSect(Host_SimBoxes,cursor%theSimulationCtrlParam,Dev_Boxes,m_MigCoale_GVarsDev,m_ImplantList,m_MigCoaleStatInfoWrap,m_MigCoalClusterRecord)

                call m_MigCoalClusterRecord%SetLastRecordImplantNum(m_MigCoalClusterRecord%GetImplantedEntitiesNum())

                cursor=>cursor%next

                if(.not. associated(cursor)) then
                    exit
                end if

                call m_MigCoalClusterRecord%IncreaseOneTimeSection()

            END DO

        END DO

        return

    end subroutine For_One_Test

    !************************************************
    subroutine TransformMCToRT(Host_HybridCollections,Host_SimBoxes,Host_SimuCtrlParamList)
        implicit none
        !--Dummy Vars---
        Class(HybridCollections)::Host_HybridCollections
        type(SimulationBoxes)::Host_SimBoxes
        type(SimulationCtrlParamList),target::Host_SimuCtrlParamList
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::ICFROM
        integer::ICTO
        integer::IC
        real(kind=KINDDF)::BoxVolum
        logical::Finded
        type(SecondOrder_AClusterLists),pointer::cursor=>null()
        type(AClusterList),pointer::cursorClusterList=>null()
        type(AClusterList)::newOne
        integer::cascadeID
        integer::SIAIndex
        integer::VacancyIndex
        !---Body---

        MultiBox = Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox

        BoxVolum = Host_SimBoxes%BOXSIZE(1)*Host_SimBoxes%BOXSIZE(2)*Host_SimBoxes%BOXSIZE(3)

        SIAIndex = Host_SimBoxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_SimBoxes%Atoms_list%FindIndexBySymbol("VC")

        call Host_HybridCollections%Clean_HybridCollections()
        allocate(Host_HybridCollections%Collections(MultiBox))

        DO IBox = 1,MultiBox

            call Host_HybridCollections%Collections(IBox)%Clean_Hybrid_ClusterListsFold()

            ICFROM = Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_SimBoxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            DO IC = ICFROM,ICTO

                if(Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. &
                   Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU) then

                    Finded = .false.

                    cascadeID = Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1)

                    cursor=>Host_HybridCollections%Collections(IBox)%Find(cascadeID)

                    if(.not. associated(cursor)) then
                        call newOne%Clean_ClusterList()
                        cursor=>Host_HybridCollections%Collections(IBox)%AppendOneClusterList(newOne,cascadeID)
                    end if

                    if(cursor%TheList%GetList_Count() .GT. 0) then
                        cursorClusterList=>cursor%TheList
                        DO While(associated(cursorClusterList))
                            if(cursorClusterList%TheCluster%IsSameKindCluster((Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC))) .eq. .true.) then
                                cursorClusterList%quantififyValue = cursorClusterList%quantififyValue + 1.D0/BoxVolum
                                Finded = .true.
                            end if

                            cursorClusterList=>cursorClusterList%next
                        END DO
                    end if

                    if(Finded .eq. .false.) then
                        call cursor%TheList%AppendOneCluster(Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC),1.D0/BoxVolum)
                    end if

                end if

            END DO

        END DO

        Nullify(cursor)
        cursor=>null()

        return
    end subroutine TransformMCToRT


    !***************************************************
    subroutine NucleationSimu_NonSpaceDist_Balance_SimpleHybrid(Host_SimBoxes,Host_SimuCtrlParam,Host_MFCollections,TheMigCoaleStatInfoWrap,Record,TheImplantSection)
        use RAND32_MODULE
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_SimBoxes
        type(SimulationCtrlParam),target::Host_SimuCtrlParam
        type(HybridCollections)::Host_HybridCollections
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        type(ImplantSection)::TheImplantSection
        !---Local Vars---
        integer::MultiBox
        real(kind=KINDDF)::TSTEP
        real(kind=KINDDF)::deta
        type(SecondOrder_AClusterLists),dimension(:),pointer::tempNBPVChangeRate
        real(kind=KINDDF)::NPOWER0Ave
        real(kind=KINDDF)::NPOWER1DIV2Ave
        real(kind=KINDDF)::NPOWER1Ave
        real(kind=KINDDF)::NPOWER3DIV2Ave
        real(kind=KINDDF)::N1
        real(kind=KINDDF)::N2
        real(kind=KINDDF)::N3
        real(kind=KINDDF)::Rave
        integer::I
        integer::INode
        integer::NNodes
        real(kind=KINDDF)::Factor
        real(kind=KINDDF)::tempTimeStep
        real(kind=KINDDF)::DiffGradient1
        real(kind=KINDDF)::DiffGradient2
        type(DiffusorValue)::TheDiffusorValue
        integer::AtomuNumbSubject
        integer::AtomuNumbObject
        integer::AtomuNumbProductor
        type(ReactionValue)::TheReactionValue
        real(kind=KINDDF)::ReactionCoeff
        real(kind=KINDDF)::SFlux
        real(kind=KINDDF)::MaxConcent
        real(kind=KINDDF)::MaxChangeRate
        integer::SIAIndex
        integer::VacancyIndex
        type(AClusterList),pointer::IKindCursor=>null()
        type(AClusterList),pointer::JKindCursor=>null()
        type(AClusterList),pointer::IChangeRateCursor=>null()
        type(AClusterList),pointer::JChangeRateCursor=>null()
        type(ACluster)::generatedCluster
        type(AClusterList),pointer::visitCursor=>null()
        real(kind=KINDDF)::deta_Final
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        SIAIndex = Host_SimBoxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_SimBoxes%Atoms_list%FindIndexBySymbol("VC")

        allocate(tempNBPVChangeRate(MultiBox))

        DO IBox = 1,MultiBox
            !--The Assignment(=) had been overrided---
            tempNBPVChangeRate(IBox) = Host_MFCollections%Collections(IBox)
        END DO

        TSTEP = 0.01

        DO While(.true.)

          Associate(Collections=>Host_MFCollections%Collections)

            call Record%IncreaseOneSimuStep()

            DO IBox = 1,MultiBox

                IKindCursor=>Collections(IBox)

                IChangeRateCursor=>tempNBPVChangeRate(IBox)

                tempCount = tempNBPVChangeRate(IBox)%GetList_Count()

                DO ILoop = 1,tempCount

                    if(IKindCursor%TheCluster%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                        JKindCursor=>IKindCursor

                        JChangeRateCursor=>IChangeRateCursor

                        DO JLoop = ILoop,tempCount
                            if(JKindCursor%TheCluster%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                                TheReactionValue = Host_SimBoxes%m_ReactionsMap%get(IKindCursor%TheCluster,JKindCursor%TheCluster)

                                ReactionCoeff = 0.D0
                                select case(TheReactionValue%ReactionCoefficientType)
                                    case(p_ReactionCoefficient_ByValue)
                                        ReactionCoeff = TheReactionValue%ReactionCoefficient_Value
                                    case(p_ReactionCoefficient_ByArrhenius)
                                        ReactionCoeff = TheReactionValue%PreFactor*exp(-C_EV2ERG*TheReactionValue%ActEnergy/Host_SimuCtrlParam%TKB)
                                end select

                                if(ReactionCoeff .GE. DRAND32()) then

                                    deta = Dumplicate*4*PI*IKindCursor%quantififyValue*JKindCursor%quantififyValue* &
                                             (IKindCursor%TheCluster%m_RAD + JKindCursor%TheCluster%m_RAD)*         &
                                             (IKindCursor%TheCluster%m_DiffCoeff + JKindCursor%TheCluster%m_DiffCoeff)

                                    if(IKind .eq. JKind) then

                                        Factor = 0.5D0

                                        IChangeRateCursor%quantififyValue =  IChangeRateCursor%quantififyValue - deta
                                    else
                                        Factor = 1.D0

                                        IChangeRateCursor%quantififyValue =  IChangeRateCursor%quantififyValue - deta

                                        JChangeRateCursor%quantififyValue =  JChangeRateCursor%quantififyValue - deta
                                    end if

                                    call generatedCluster%Clean_Cluster()

                                    select case(TheReactionValue%ProductionType)
                                        case(p_ProductionType_BySimplePlus)
                                            generatedCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA =  IKindCursor%TheCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA + &
                                                                                                      JKindCursor%TheCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA

                                            !---Should consider atomic number conservation
                                            deta_Final = deta*Factor

                                        case(p_ProductionType_BySubtract)

                                            SubjectElementIndex = TheReactionValue%ElemetIndex_Subject
                                            ObjectElementIndex = TheReactionValue%ElemetIndex_Object

                                            generatedCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA = IKindCursor%TheCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA + &
                                                                                                     JKindCursor%TheCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA

                                            SubjectNANum = generatedCluster%m_Atoms(SubjectElementIndex)%m_NA
                                            ObjectNANum  = generatedCluster%m_Atoms(ObjectElementIndex)%m_NA

                                            generatedCluster%m_Atoms(SubjectElementIndex)%m_NA = max(SubjectNANum - ObjectNANum,0)

                                            generatedCluster%m_Atoms(ObjectElementIndex)%m_NA = max(ObjectNANum - SubjectNANum,0)

                                            if(sum(generatedCluster%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1) .EQ. 0) then
                                                generatedCluster%m_Statu = p_ANNIHILATE_STATU
                                            end if

                                            !---Not consider atomic number conservation
                                            deta_Final = deta
                                        case default
                                            write(*,*) "MCPSCUERROR: Unknown reaction type",TheReactionValue%ProductionType
                                            pause
                                            stop

                                    end select

                                    TheDiffusorValue = Host_SimBoxes%m_DiffusorTypesMap%Get(generatedCluster)

                                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                                    !---you should init the distribution by external file---
                                    select case(TheDiffusorValue%ECRValueType_Free)
                                        case(p_ECR_ByValue)
                                            generatedCluster%m_RAD = TheDiffusorValue%ECR_Free
                                        case default
                                            generatedCluster%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,        &
                                                                     Host_SimBoxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA, &
                                                                     Host_SimuCtrlParam%TKB,               &
                                                                     Host_SimBoxes%LatticeLength)
                                    end select

                                    select case(TheDiffusorValue%DiffusorValueType_Free)
                                        case(p_DiffuseCoefficient_ByValue)
                                            generatedCluster%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                                        case(p_DiffuseCoefficient_ByArrhenius)
                                            generatedCluster%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                                        case(p_DiffuseCoefficient_ByBCluster)
                                            ! Here we adopt a model that D=D0*(1/R)**Gama
                                            generatedCluster%m_DiffCoeff = m_FREESURDIFPRE*(generatedCluster%m_RAD**(-p_GAMMA))
                                        case(p_DiffuseCoefficient_BySIACluster)
                                            generatedCluster%m_DiffCoeff = (sum(generatedCluster%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                                        case(p_DiffuseCoefficient_ByVcCluster)
                                            generatedCluster%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(generatedCluster%m_Atoms(:)%m_NA)))* &
                                                                                                         TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                                    end select

                                    generatedCluster%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

                                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                                        DO TheDim = 1,3
                                            generatedCluster%m_DiffuseDirection(TheDim) = generatedCluster%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                                        END DO
                                        generatedCluster%m_DiffCoeff = generatedCluster%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                                    end if

                                    generatedCluster%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParam%TKB)

                                    visitCursor=>tempNBPVChangeRate(IBox)%Find(generatedCluster)

                                    if(associated(visitCursor)) then
                                        visitCursor%quantififyValue = visitCursor%quantififyValue  + deta_Final
                                    else
                                        call tempNBPVChangeRate(IBox)%AppendOneCluster(generatedCluster,deta_Final)
                                    end if

!                                    AtomuNumbSubject = sum(ClustersKind(IKind)%m_Atoms(:)%m_NA)
!                                    AtomuNumbObject = sum(ClustersKind(JKind)%m_Atoms(:)%m_NA)
!                                    AtomuNumbProductor = sum(ClustersKind(IKind+JKind)%m_Atoms(:)%m_NA)
!
!                                    tempNBPVChangeRate(INode,IKind + JKind) = tempNBPVChangeRate(INode,IKind + JKind) + &
!                                                                            Factor*deta*(AtomuNumbSubject+AtomuNumbObject)/AtomuNumbProductor
                                end if
                            end if

                            JKindCursor=>JKindCursor%next

                            JChangeRateCursor=>JChangeRateCursor%Next
                        END DO

                    end if

                    IKindCursor=>IKindCursor%next
                    IChangeRateCursor=>IChangeRateCursor%Next
                END DO
            END DO

            tempNBPVChangeRate = tempNBPVChangeRate + ImplantedRate

            MaxConcent = maxval(Concent)
            MaxChangeRate = maxval(dabs(tempNBPVChangeRate))

            if(MaxConcent .GT. 0.D0 .AND. MaxChangeRate .GT. 0.D0) then
                TSTEP = Host_SimuCtrlParam%MaxReactChangeRate*MaxConcent/MaxChangeRate
            else
                TSTEP = 1.D-10
            end if

            write(*,*) "!---------------------"
            write(*,*) "TSTEP1",TSTEP
            write(*,*) "maxval(Concent)",maxval(Concent)
            write(*,*) "maxval(dabs(tempNBPVChangeRate))",maxval(dabs(tempNBPVChangeRate))
            write(*,*) "maxloc(Concent)",maxloc(Concent)
            write(*,*) "maxloc(dabs(tempNBPVChangeRate))",maxloc(dabs(tempNBPVChangeRate))
            write(*,*) "maxloc(ImplantedRate)",maxloc(ImplantedRate)
            write(*,*) "maxval(ImplantedRate)",maxval(ImplantedRate)

            DO IKind = 1,CKind
                DO INode = 1,NNodes
                    tempTimeStep = Concent(INode,IKind)/dabs(tempNBPVChangeRate(INode,IKind))
                    if(tempNBPVChangeRate(INode,IKind) .LT. 0.D0 .AND. Concent(INode,IKind) .GT. 0.D0) then
                        TSTEP = min(TSTEP,tempTimeStep)
                    end if

                    if(INode .eq. 1) then  ! upper surface

                        DiffGradient1 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                        if(NNodes .LE. 1) then
                            DiffGradient2 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                            SFlux = DiffGradient1*Concent(INode,IKind) + DiffGradient2*Concent(INode,IKind)
                            if(SFlux .GT. 0.D0 .AND. Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind) .GT. 0) then
                                TSTEP = min(TSTEP,Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind)/(dabs(SFlux)/NodeSpace(INode)))
                            end if
                        else
                            DiffGradient2 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode) + NodeSpace(INode+1))

                            SFlux = DiffGradient1*Concent(INode,IKind) - DiffGradient2*(Concent(INode+1,IKind) - Concent(INode,IKind))
                            if(SFlux .GT. 0.D0 .AND. Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind) .GT. 0) then
                                TSTEP = min(TSTEP,Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind)/(dabs(SFlux)/NodeSpace(INode)))
                            end if

                        end if

                    else if(INode .eq. NNodes) then  ! Low surface

                        DiffGradient2 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                        if(NNodes .LE. 1) then
                            DiffGradient1 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                            SFlux = DiffGradient1*Concent(INode,IKind) + DiffGradient2*Concent(INode,IKind)
                            if(SFlux .GT. 0.D0 .AND. Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind) .GT. 0) then
                                TSTEP = min(TSTEP,Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind)/(dabs(SFlux)/NodeSpace(INode)))
                            end if
                        else
                            DiffGradient1 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode-1) + NodeSpace(INode))

                            SFlux = DiffGradient1*(Concent(INode,IKind) - Concent(INode-1,IKind)) + DiffGradient2*Concent(INode,IKind)
                            if(SFlux .GT. 0.D0 .AND. Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind) .GT. 0) then
                                TSTEP = min(TSTEP,Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind)/(dabs(SFlux)/NodeSpace(INode)))
                            end if
                        end if

                    else
                        DiffGradient1 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode-1) + NodeSpace(INode))
                        DiffGradient2 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode) + NodeSpace(INode+1))

                        SFlux = DiffGradient1*(Concent(INode,IKind) - Concent(INode-1,IKind)) - DiffGradient2*(Concent(INode+1,IKind) - Concent(INode,IKind))
                        if(SFlux .GT. 0.D0 .AND. Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind) .GT. 0) then
                            TSTEP = min(TSTEP,Host_SimuCtrlParam%MaxDiffuseChangeRate*Concent(INode,IKind)/(dabs(SFlux)/NodeSpace(INode)))
                        end if
                    end if

                END DO
            END DO

            DO IKind = 1,CKind

                MatA = 0.D0
                MatB = 0.D0
                MatC = 0.D0
                MatD = 0.D0

                DO INode = 1,NNodes
                    if(INode .eq. 1) then  ! upper surface

                        DiffGradient1 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                        MatA(INode) = 0.D0
                        if(NNodes .LE. 1) then
                            DiffGradient2 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                            select case(Host_SimuCtrlParam%BDCTYPE(3,1))
                                case(p_Dirichlet_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP + DiffGradient1 + DiffGradient2
                                case(p_Neumann_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP
                                case default
                                    write(*,*) "MFPSCUERROR: Unknown boundary condition ",Host_SimuCtrlParam%BDCTYPE(3,1)
                                    pause
                                    stop
                            end select

                            MatC(INode) = 0.D0
                        else
                            DiffGradient2 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode) + NodeSpace(INode+1))

                            select case(Host_SimuCtrlParam%BDCTYPE(3,1))
                                case(p_Dirichlet_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP + DiffGradient1 + DiffGradient2
                                case(p_Neumann_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP + DiffGradient2
                                case default
                                    write(*,*) "MFPSCUERROR: Unknown boundary condition ",Host_SimuCtrlParam%BDCTYPE(3,1)
                                    pause
                                    stop
                            end select

                            MatC(INode) = -DiffGradient2
                        end if

                        MatD(INode) = Concent(INode,IKind)*NodeSpace(INode)/TSTEP + tempNBPVChangeRate(INode,IKind)*NodeSpace(INode)
                    else if(INode .eq. NNodes) then  ! Low surface

                        DiffGradient2 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                        if(NNodes .LE. 1) then
                            DiffGradient1 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(INode)

                            MatA(INode) = 0.D0

                            select case(Host_SimuCtrlParam%BDCTYPE(3,2))
                                case(p_Dirichlet_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP + DiffGradient1 + DiffGradient2
                                case(p_Neumann_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP
                                case default
                                    write(*,*) "MFPSCUERROR: Unknown boundary condition ",Host_SimuCtrlParam%BDCTYPE(3,2)
                                    pause
                                    stop
                            end select
                        else
                            DiffGradient1 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode-1) + NodeSpace(INode))

                            MatA(INode) = -DiffGradient1

                            select case(Host_SimuCtrlParam%BDCTYPE(3,2))
                                case(p_Dirichlet_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP + DiffGradient1 + DiffGradient2
                                case(p_Neumann_BDC)
                                    MatB(INode) = NodeSpace(INode)/TSTEP + DiffGradient1
                                case default
                                    write(*,*) "MFPSCUERROR: Unknown boundary condition ",Host_SimuCtrlParam%BDCTYPE(3,2)
                                    pause
                                    stop
                            end select

                        end if

                        MatC(INode) = 0.D0
                        MatD(INode) = Concent(INode,IKind)*NodeSpace(INode)/TSTEP + tempNBPVChangeRate(INode,IKind)*NodeSpace(INode)
                    else
                        DiffGradient1 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode-1) + NodeSpace(INode))
                        DiffGradient2 = (ClustersKind(IKind)%m_DiffCoeff + ClustersKind(IKind)%m_DiffCoeff)/(NodeSpace(INode) + NodeSpace(INode+1))
                        MatA(INode) = -DiffGradient1
                        MatB(INode) = NodeSpace(INode)/TSTEP + (DiffGradient1 + DiffGradient2)
                        MatC(INode) = -DiffGradient2
                        MatD(INode) = Concent(INode,IKind)*NodeSpace(INode)/TSTEP + tempNBPVChangeRate(INode,IKind)*NodeSpace(INode)
                    end if

                END DO

                call SolveTridag(IKind,MatA,MatB,MatC,MatD,Concent,NNodes,MatW,MatH)

!                DiffGradient2 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(NNodes)
!                FOutEachStep(IKind) = DiffGradient2*Concent(NNodes,IKind)
!                COutEachStep(IKind) = DiffGradient2*Concent(NNodes,IKind)*TSTEP/NodeSpace(NNodes)
!                FOutAccum(IKind) = FOutAccum(IKind) + FOutEachStep(IKind)
!                COutAccum(IKind) = COutAccum(IKind) + COutEachStep(IKind)
!
!                DiffGradient1 = ClustersKind(IKind)%m_DiffCoeff/NodeSpace(1)
!                FSurfEachStep(IKind) = DiffGradient1*Concent(1,IKind)
!                CSurfEachStep(IKind) = DiffGradient1*Concent(1,IKind)*TSTEP/NodeSpace(1)
!                FSurfAccum(IKind) = FSurfAccum(IKind) + FSurfEachStep(IKind)
!                CSurfAccum(IKind) = CSurfAccum(IKind) + CSurfEachStep(IKind)

                if(IKind .eq. 1) then

!                    write(*,*) "Accumulated out flux from up surface",FSurfAccum(IKind)
!                    write(*,*) "Accumulated out concentrate from up surface",CSurfAccum(IKind)
!                    write(*,*) "Out flux from up surface in current step",FSurfEachStep(IKind)
!                    write(*,*) "Out concentrate from up surface in current step",CSurfEachStep(IKind)
!                    DO INode = 1,NNodes
!                        write(*,*) "INode",INode,"Concent(INode,1)",Concent(INode,1)
!                    END DO
!                    write(*,*) "Accumulated out flux from lower surface",FOutAccum(IKind)
!                    write(*,*) "Accumulated out concentrate from lower surface",COutAccum(IKind)
!                    write(*,*) "Out flux from lower surface in current step",FOutEachStep(IKind)
!                    write(*,*) "Out concentrate from lower surface in current step",COutEachStep(IKind)
!
!                    write(*,*) "----------------------------------------"
!                    write(*,*) "sum(Concent(:,1))",sum(Concent(:,1))
!                    write(*,*) "sum(Concent(:,1)) + CSurfAccum(1) + COutAccum(1)",sum(Concent(:,1)) + CSurfAccum(1) + COutAccum(1)
!                    write(*,*) "(sum(Concent(:,1)) + CSurfAccum(1) + COutAccum(1))/ConCentrat0",(ConCentrat0 - (sum(Concent(:,1)) + CSurfAccum(1) + COutAccum(1)))/ConCentrat0
!                    write(*,*) "----------------------------------------"
                end if


            END DO

            call Record%AddSimuTimes(TSTEP)

            call OutPutCurrent(Host_SimBoxes,Host_SimuCtrlParam,Record)

            if(mod(Record%GetSimuSteps(),1) .eq. 0) then

                call Cal_Statistic_IMPLANT(Host_SimBoxes,Host_SimuCtrlParam,NPOWER0Ave,NPOWER1DIV2Ave,NPOWER1Ave,NPOWER3DIV2Ave)

                call Put_Out_IMPLANT(Host_SimBoxes,Host_SimuCtrlParam,Record%GetSimuSteps(),Record%GetSimuTimes(),TSTEP,NPOWER0Ave,NPOWER1DIV2Ave,NPOWER1Ave,NPOWER3DIV2Ave,N1,N2,N3,Rave)
            end if

            !if(Concent(CKind) .GT. 1.D-10) then
            if(DSQRT(dble(sum(ClustersKind(CKind)%m_Atoms(:)%m_NA)))*sum(Concent(1:NNodes,CKind)) .GT. &
               NPOWER1DIV2Ave*Host_SimuCtrlParam%DumplicateFactor) then

               write(*,*) "---Expand Clusters kind---"

                if(TheImplantSection%ImplantFlux .GT. 0.D0) then
                    call Host_SimBoxes%ReSizeClusterKind_CPU(Host_SimuCtrlParam,m_RNFACTOR,m_FREESURDIFPRE,CKind*2)

                    CKind = CKind*2
                    NNodes = Host_SimBoxes%NNodes

                    call DeAllocateArray_Host(tempNBPVChangeRate,"tempNBPVChangeRate")
                    call AllocateArray_Host(tempNBPVChangeRate,NNodes,CKind,"tempNBPVChangeRate")
                    tempNBPVChangeRate = 0.D0

                    call DeAllocateArray_Host(ImplantedRate,"ImplantedRate")
                    call AllocateArray_Host(ImplantedRate,NNodes,CKind,"ImplantedRate")
                    ImplantedRate = 0.D0

                    call TheImplantSection%Cal_ImplantClustersRate(Host_SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap,Record,ImplantedRate)

                    write(*,*) "Max clusters kind number: ",CKind
                else

                    DO INode = 1,NNodes

                        DO I = 1,(CKind -1)/2 + 1
                            if(2*I .LE. CKind) then
                                Concent(INode,I) = (Concent(INode,2*I-1)*sum(ClustersKind(2*I-1)%m_Atoms(:)%m_NA)+Concent(INode,2*I)*sum(ClustersKind(2*I)%m_Atoms(:)%m_NA)) &
                                                    /(2.D0*sum(ClustersKind(2*I)%m_Atoms(:)%m_NA))
                            else
                                Concent(INode,I) = Concent(INode,2*I - 1)
                            end if
                        END DO

                        Concent(INode,(CKind -1)/2+2:CKind) = 0.D0

                    END DO

                    DO I = 1,(CKind -1)/2 + 1
                            if(2*I .LE. CKind) then
                                ClustersKind(I) = ClustersKind(2*I)
                            else
                                ClustersKind(I) = ClustersKind(2*I - 1)
                            end if
                    END DO

                    DO I = (CKind -1)/2 + 2,CKind
                            ClustersKind(I)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA = 2*ClustersKind(I)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
                            TheDiffusorValue = Host_SimBoxes%m_DiffusorTypesMap%get(ClustersKind(I))

                            select case(TheDiffusorValue%ECRValueType_Free)
                                case(p_ECR_ByValue)
                                    ClustersKind(I)%m_RAD = TheDiffusorValue%ECR_Free
                                case(p_ECR_ByBCluster)
                                    ClustersKind(I)%m_RAD = DSQRT(sum(ClustersKind(I)%m_Atoms(:)%m_NA)/m_RNFACTOR)
                            end select

                            select case(TheDiffusorValue%DiffusorValueType_Free)
                                case(p_DiffuseCoefficient_ByValue)
                                    ClustersKind(I)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                                case(p_DiffuseCoefficient_ByArrhenius)
                                    ClustersKind(I)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                                case(p_DiffuseCoefficient_ByBCluster)
                                    ! Here we adopt a model that D=D0*(1/R)**Gama
                                    ClustersKind(I)%m_DiffCoeff = m_FREESURDIFPRE*(ClustersKind(I)%m_RAD**(-p_GAMMA))
                            end select
                    END DO


                    Dumplicate = Dumplicate*2
                    write(*,*) "Dumplicate",Dumplicate
                end if
            end if

            if(Record%GetSimuTimes() .GT. Host_SimuCtrlParam%TermTValue) then
                exit
            end if

            TSTEP = TSTEP*1.5

          END Associate
        END DO

    end subroutine NucleationSimu_NonSpaceDist_Balance_SimpleHybrid

























    !****************************************************************
    subroutine For_One_TimeSect(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,ImplantSectionList,TheMigCoaleStatInfoWrap,Record)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
        type(ImplantList)::ImplantSectionList
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        integer::TotalSize
        integer:: NCUT, DUP, DUPXYZ(3)
        !---Body---
        if(m_DumplicateBox .eq. .true.) then

            DUP = 1
            DUPXYZ = 0
            if(Host_SimuCtrlParam%PERIOD(1)) then
                DUP = DUP*2
                DUPXYZ(1) = DUPXYZ(1)+1
            end if

            if(Host_SimuCtrlParam%PERIOD(2)) then
                DUP = DUP*2
                DUPXYZ(2) = DUPXYZ(2)+1
            end if

            if(Host_SimuCtrlParam%PERIOD(3) ) then
                DUP = DUP*2
                DUPXYZ(3) = DUPXYZ(3)+1
            end if

            if(Record%GetNCUT() .LE. 0) then
                NCUT = (Host_SimBoxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + Host_SimBoxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEINGB_STATU))/DUP+1
                call Record%SetNCUT(NCUT)
            else
                NCUT = Record%GetNCUT()
            end if

            DO While(.true.)

                if(Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
                    TotalSize = Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
                else
                    TotalSize = 0
                end if

                call GetBoxesMigCoaleStat_Used_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)
                if(Record%GetSimuSteps() .eq. 0) then
                    call TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%ConverFromUsed(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used)
                    call TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual%ConverFromUsed(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used)
                else
                    call GetBoxesMigCoaleStat_Expd_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)
                    call GetBoxesMigCoaleStat_Virtual_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual,Record)
                end if

                call Growth_FixBox(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,ImplantSectionList,TheMigCoaleStatInfoWrap,Record,TotalSize, NCUT)

                call Record%IncreaseOneRescaleCount()
                call Host_SimBoxes%PutoutCfg(Host_SimuCtrlParam,Record,RescaleCount=Record%GetRescaleCount())

                call GetBoxesMigCoaleStat_Used_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

                call PutOut_Instance_Statistic_IntegralBox(Host_SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record,Model=0)
                call PutOut_Instance_Statistic_EachBox(Host_SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

                if(Record%GetSimuTimes() .gt. Host_SimuCtrlParam%TermTValue) then
                    exit
                end if

                write(*,*) "Start to rescale box..."

                !dumplicate the simulation boxes

                call Record%RecordNC_ForSweepOut(Host_SimuCtrlParam%MultiBox,Host_SimBoxes%m_BoxesBasicStatistic)
                call Dev_Boxes%RescaleBoxes_GPUToCPU(Host_SimBoxes, Host_SimuCtrlParam,DUPXYZ)

                if(Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
                    TotalSize = Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
                else
                    TotalSize = 0
                end if

                if(TotalSize*3 .GT. size(Dev_MigCoaleGVars%dm_MigCoale_RandDev%dm_RandArray_Walk)) then
                    call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeWalkRandNum(TotalSize)
                end if

                if(TotalSize .GT. size(Dev_MigCoaleGVars%dm_MigCoale_RandDev%dm_DevRandRecord)) then
                    if(Host_SimuCtrlParam%UPDATETSTEPSTRATEGY .eq. mp_SelfAdjustlStep_NNDR_LastPassage_Integer) then
                        call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeDevRandRecord(TotalSize,Record%RandSeed_InnerDevWalk(1),Record%GetSimuSteps()*(3 + (Host_SimuCtrlParam%LastPassageFactor+2)*3 + 2))
                        ! 3 is for three random boundary condition for 1-D diffusion , (Host_SimuCtrlParam%LastPassageFactor+2)*3 is for random walk , 2 is for the random 1-D direction for new generated cluster in pre and back merge
                    else
                        call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeDevRandRecord(TotalSize,Record%RandSeed_InnerDevWalk(1),Record%GetSimuSteps()*(3 + 2))
                        ! 3 is for three random boundary condition for 1-D diffusion ,2 is for the random 1-D direction for new generated cluster in pre and back merge
                    end if


                end if

                if(TotalSize .GT. size(Dev_MigCoaleGVars%dm_MigCoale_RandDev%dm_RandArray_Reaction)) then
                    call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeReactionRandNum(TotalSize)
                end if

                NCUT = (Host_SimBoxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + Host_SimBoxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEINGB_STATU))/DUP+1

                call Record%SetNCUT(NCUT)

                write(*,fmt="(A20,I10,A20)") "Rescale for ",Record%GetRescaleCount()," times ."

                call GetBoxesMigCoaleStat_Used_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

                call PutOut_Instance_Statistic_IntegralBox(Host_SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record,Model=0)
                call PutOut_Instance_Statistic_EachBox(Host_SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

            END DO
        else

            if(Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
                TotalSize = Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_SimBoxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
            else
                TotalSize = 0
            end if

            call GetBoxesMigCoaleStat_Used_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)
            if(Record%GetSimuSteps() .eq. 0) then
                call TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%ConverFromUsed(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used)
                call TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual%ConverFromUsed(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used)
            else
                call GetBoxesMigCoaleStat_Expd_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)
                call GetBoxesMigCoaleStat_Virtual_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual,Record)
            end if

            call Growth_FixBox(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,ImplantSectionList,TheMigCoaleStatInfoWrap,Record,TotalSize)

        end if

        call GetBoxesMigCoaleStat_Used_GPU(Host_SimBoxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

        call PutOut_Instance_Statistic_IntegralBox(Host_SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record,Model=1)

        return
    end subroutine For_One_TimeSect

    !*****************************************************
    subroutine Growth_FixBox(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,ImplantSectionList,TheMigCoaleStatInfoWrap, Record, NC0, NCUT)
        ! To start growth
        implicit none
        !---Dummy vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
        type(ImplantList)::ImplantSectionList
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        integer, intent(in)::NC0
        integer, optional::NCUT
        !---local vars---
        real(kind=KINDDF):: TSTEP, RCUT
        integer::NAct
        integer::IBox
        logical::HasUpdateStatis
        integer::MultiBox
        integer::NSIZE
        !---Body---

        Associate(Host_ClustesInfo=>Host_Boxes%m_ClustersInfo_CPU,Dev_ClustesInfo=>Dev_Boxes%dm_ClusterInfo_GPU, &
              TBasicInfo=>Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral)

            call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                       max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                           TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)),&
                                           MaxDiffuse=max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEFREE_STATU), &
                                                          TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEINGB_STATU)))

            DO WHILE(.TRUE.)


                call For_One_Step(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,ImplantSectionList,TheMigCoaleStatInfoWrap,Record,TSTEP)

                if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalSteps) then
                    if ((Record%GetSimuSteps() - Record%GetLastUpdateStatisTime()) .GE. Host_SimuCtrlParam%TUpdateStatisValue) then
                        call GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)

                        call Record%SetLastUpdateStatisTime(dble(Record%GetSimuSteps()))

                    end if

                else if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalRealTime) then
                    if((Record%GetSimuTimes() - Record%GetLastUpdateStatisTime()) .GE. Host_SimuCtrlParam%TUpdateStatisValue) then

                        call GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)

                        call Record%SetLastUpdateStatisTime(Record%GetSimuTimes())
                    end if
                end if

                call OutPutCurrent(Host_Boxes,Dev_Boxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

                call SweepOutMemory(Host_Boxes,Dev_Boxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap,Record)

                NAct = TBasicInfo%NC(p_ACTIVEFREE_STATU)+TBasicInfo%NC(p_ACTIVEINGB_STATU)

                call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.false.,RMAX= &
                                           max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                           TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)),&
                                           MaxDiffuse=max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEFREE_STATU), &
                                                          TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEINGB_STATU)))


                !Check if need to duplicate the box
                if(present(NCUT)) then
                    if(NAct .LE. NCUT) then
                        exit
                    end if
                end if

                if(Record%GetSimuTimes() .GE. Host_SimuCtrlParam%TermTValue) then
                    exit
                end if

            END DO

        END Associate

        return
    end subroutine Growth_FixBox

    !*********************************************************************
    subroutine For_One_Step(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,ImplantSectionList,TheMigCoaleStatInfoWrap,Record,TSTEP)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
        type(ImplantList),target::ImplantSectionList
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF)::TSTEP
        !---Local Vars---
        type(ImplantList),pointer::curosr=>null()
        integer::I
        !---Body---

        call UpdateTimeStep_MigCoal(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record,TSTEP)

        curosr=>ImplantSectionList
        DO I = 1,ImplantSectionList%ListCount
            if(curosr%TheImplantSection%InsertCountOneBatch .GT. 0.D0) then
                call curosr%TheImplantSection%Implant(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TheMigCoaleStatInfoWrap,Record,TSTEP,m_FREESURDIFPRE,m_GBSURDIFPRE)
            end if

            curosr=>curosr%next
        END DO

        call WalkOneStep(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,Record,TSTEP)

        if(Host_SimuCtrlParam%FreeDiffusion .ne. .true.) then
            call MergeClusters(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,Record,TSTEP)
        end if

        call Record%IncreaseOneSimuStep()

        call Record%AddSimuTimes(TSTEP)

        call Record%SetTimeSteps(TSTEP)

        Nullify(curosr)
        curosr=>null()

        return
    end subroutine For_One_Step

    !*************************************************************
    subroutine CopyInitBoxSimCfgFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(InitBoxSimCfg),intent(out)::this
        type(InitBoxSimCfg),intent(in)::other
        !---Local Vars---
        integer::I
        !---Body---
        this%InitType = other%InitType

        if(allocated(this%NClusters)) then
            deallocate(this%NClusters)
        end if
        if(size(other%NClusters) .GT. 0) then
            allocate(this%NClusters(size(other%NClusters)))
            this%NClusters = other%NClusters
        end if

        this%InitCfgFileName = other%InitCfgFileName

        DO I = 1,p_ATOMS_GROUPS_NUMBER
            this%Elemets(I) = other%Elemets(I)
            this%CompositWeight(I) = other%CompositWeight(I)
        END DO

        this%NAINI = other%NAINI

        this%NASDINI = other%NASDINI

        this%NACUT = other%NACUT

        this%InitDepthDistType = other%InitDepthDistType

        this%DepthINI = other%DepthINI

        this%DepthSDINI = other%DepthSDINI

        this%SUBBOXBOUNDARY = other%SUBBOXBOUNDARY

        call DeAllocateArray_Host(this%LayerThick,"LayerThick")
        call AllocateArray_Host(this%LayerThick,size(other%LayerThick),"LayerThick")
        this%LayerThick = other%LayerThick

        call DeAllocateArray_Host(this%PNCLayers,"PNCLayers")
        call AllocateArray_Host(this%PNCLayers,size(other%PNCLayers),"PNCLayers")
        this%PNCLayers = other%PNCLayers

        return
    end subroutine CopyInitBoxSimCfgFromOther

    !*************************************************************
    subroutine Clean_InitBoxSimCfg(this)
        implicit none
        !---Dummy Vars---
        CLASS(InitBoxSimCfg),intent(out)::this
        !---Body---
        this%InitType = -1

        if(allocated(this%NClusters)) then
            deallocate(this%NClusters)
        end if

        this%InitCfgFileName = ""

        this%Elemets = ""

        this%NAINI = 0.D0

        this%NASDINI = 0.D0

        this%NACUT = 0.D0

        this%CompositWeight = 0.D0

        this%InitDepthDistType = -1

        call DeAllocateArray_Host(this%LayerThick,"LayerThick")

        call DeAllocateArray_Host(this%PNCLayers,"PNCLayers")

        this%DepthINI = 0.D0

        this%DepthSDINI = 0.D0

        this%SUBBOXBOUNDARY = 0.D0

        return
    end subroutine

    !*************************************************
    subroutine CleanInitBoxSimCfg(this)
        implicit none
        !---Dummy Vars---
        type(InitBoxSimCfg)::this
        !---Body---
        call this%Clean_InitBoxSimCfg()

        return
    end subroutine CleanInitBoxSimCfg

    !*************************************************************
    subroutine AppendOne_InintSimBoxCfg(this,newOne)
        implicit none
        !---Dummy Vars---
        CLASS(InitBoxSimCfgList),target::this
        type(InitBoxSimCfg)::newOne
        !---Local Vars---
        type(InitBoxSimCfgList),pointer::cursor=>null(),cursorP=>null()
        !---Body---
        if(this%GetList_Count() .LE. 0) then
            this%ListCount = 1
            this%TheValue = newOne
        else
            cursor=>this%next
            cursorP=>this

            DO while(associated(cursor))
                cursor=>cursor%next
                cursorP=>cursorP%next

            END DO

            this%ListCount = this%ListCount + 1

            allocate(cursor)
            NUllify(cursor%next)
            cursor%next=>null()
            ! The assignment(=) had been overrided
            cursor%TheValue = newOne
            cursorP%next=>cursor
        end if

        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()
        return
    end subroutine AppendOne_InintSimBoxCfg

    !**************************************
    integer function GetInitBoxSimCfgList_Count(this)
        implicit none
        !---Dummy Vars---
        CLASS(InitBoxSimCfgList)::this
        !---Body---
        GetInitBoxSimCfgList_Count = this%ListCount

        return
    end function

    !**************************************
    subroutine Clean_InitBoxSimCfgList(this)
        implicit none
        !---Dummy Vars---
        CLASS(InitBoxSimCfgList),target::this
        !---Local Vars---
        type(InitBoxSimCfgList),pointer::cursor=>null()
        type(InitBoxSimCfgList),pointer::next=>null()
        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        call this%TheValue%Clean_InitBoxSimCfg()

        DO While(associated(cursor))
            next=>cursor%next
            call cursor%TheValue%Clean_InitBoxSimCfg()
            cursor%next=>null()
            deallocate(cursor)
            Nullify(cursor)
            cursor=>next
        END DO


        this%next=>null()

        this%ListCount = 0

        Nullify(cursor)
        Nullify(next)
        cursor=>null()
        next=>null()

        return
    end subroutine Clean_InitBoxSimCfgList

    !************************************
    subroutine CleanInitBoxSimCfgList(this)
        implicit none
        !---Dummy Vars---
        type(InitBoxSimCfgList)::this
        !---Body---

        call this%Clean_InitBoxSimCfgList()

        return
    end subroutine CleanInitBoxSimCfgList

    !*****************************************************************
    subroutine InitSimulationBoxesConfig(SimBoxes,Host_SimuCtrlParam,InitBoxCfgList,TheMigCoaleStatInfoWrap,Record)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfgList)::InitBoxCfgList
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        character*1000::path
        character*18,dimension(:),allocatable::CRMin
        character*18,dimension(:),allocatable::CRMax
        character*18,dimension(:),allocatable::CCNum
        character*18,dimension(:),allocatable::CAcumNum
        integer::I
        integer::length,trueLength
        !---Body---

        call DOInitSimulationBoxesConfig(SimBoxes,Host_SimuCtrlParam,Record,InitBoxCfgList)

        call GetBoxesMigCoaleStat_Used_CPU(SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used)

        call GetBoxesMigCoaleStat_Expd_CPU(SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd)

        !---The final one is used for the total boxes
        allocate(CRMin(p_NUMBER_OF_STATU))
        allocate(CRMax(p_NUMBER_OF_STATU))
        allocate(CCNum(p_NUMBER_OF_STATU))
        allocate(CAcumNum(p_NUMBER_OF_STATU))

        DO I = 1,p_NUMBER_OF_STATU
            CRMin(I) = "RMIN_"//trim(p_CStatu(I))
            length = len(CRMin(I))
            trueLength = LENTRIM(CRMin(I))
            CRMin(I)(length-trueLength+1:length) = CRMin(I)(1:trueLength)
            CRMin(I)(1:length-trueLength) = ""

            CRMax(I) = "RMAX_"//trim(p_CStatu(I))
            length = len(CRMax(I))
            trueLength = LENTRIM(CRMax(I))
            CRMax(I)(length-trueLength+1:length) = CRMax(I)(1:trueLength)
            CRMax(I)(1:length-trueLength) = ""

            CCNum(I) = "NUM_"//trim(p_CStatu(I))
            length = len(CCNum(I))
            trueLength = LENTRIM(CCNum(I))
            CCNum(I)(length-trueLength+1:length) = CCNum(I)(1:trueLength)
            CCNum(I)(1:length-trueLength) = ""

            CAcumNum(I) = "ACUM_"//trim(p_CStatu(I))
            length = len(CAcumNum(I))
            trueLength = LENTRIM(CAcumNum(I))
            CAcumNum(I)(length-trueLength+1:length) = CAcumNum(I)(1:trueLength)
            CAcumNum(I)(1:length-trueLength) = ""

        END DO

        path = Host_SimuCtrlParam%OutFilePath(1:LENTRIM(Host_SimuCtrlParam%OutFilePath))//FolderSpe//"RTStatistic_EachBox_.out"

        Record%HSizeStatistic_EachBox = CreateNewFile(path)

        write(Record%HSizeStatistic_EachBox, fmt="(130(A30,1x))") "Step",                              &
                                                                  "IBox",                              &
                                                                  "Time(s)",                           &
                                                                  "TStep(s)",                          &
                                                                  "NACTClusters",                      &
                                                                  "TotalCluster",                      &
                                                                  "TotalAtoms",                        &
                                                                  CCNum(1:p_NUMBER_OF_STATU),          &
                                                                  CRMin(1:p_NUMBER_OF_STATU),          &
                                                                  CRMax(1:p_NUMBER_OF_STATU),          &
                                                                  "RAVE",                              &
                                                                  "NAVA",                              &
                                                                  "Concentrate",                       &
                                                                  "TotalImplant",                      &
                                                                  CAcumNum(1:p_NUMBER_OF_STATU)

        FLUSH(Record%HSizeStatistic_EachBox)

        path = Host_SimuCtrlParam%OutFilePath(1:LENTRIM(Host_SimuCtrlParam%OutFilePath))//FolderSpe//"RTStatistic_TotalBox_.out"

        Record%HSizeStatistic_TotalBox = CreateNewFile(path)

        write(Record%HSizeStatistic_TotalBox, fmt="(130(A30,1x))")  "Step",                              &
                                                                    "Time(s)",                           &
                                                                    "TStep(s)",                          &
                                                                    "NACTClusters",                      &
                                                                    "TotalCluster",                      &
                                                                    "TotalAtoms",                        &
                                                                    CCNum(1:p_NUMBER_OF_STATU),          &
                                                                    CRMin(1:p_NUMBER_OF_STATU),          &
                                                                    CRMax(1:p_NUMBER_OF_STATU),          &
                                                                    "RAVE",                              &
                                                                    "NAVA",                              &
                                                                    "Concentrate",                       &
                                                                    "TotalImplant",                      &
                                                                    CAcumNum(1:p_NUMBER_OF_STATU)


        FLUSH(Record%HSizeStatistic_TotalBox)

        deallocate(CRMin)
        deallocate(CRMax)
        deallocate(CCNum)
        deallocate(CAcumNum)

        call SimBoxes%PutoutCfg(Host_SimuCtrlParam,Record)

        call PutOut_Instance_Statistic_IntegralBox(SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record,Model=0)
        call PutOut_Instance_Statistic_EachBox(SimBoxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

        return
    end subroutine InitSimulationBoxesConfig

    !*****************************************************************
    subroutine ReadInitBoxSimCfgList(SimBoxes,Host_SimuCtrlParam,InitBoxCfgList,SimuRecord)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfgList),target::InitBoxCfgList
        CLASS(SimulationRecord)::SimuRecord
        !---Local Vars---
        logical::existed
        integer::hFile
        character*1000::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::LINE
        integer::N
        !---Body---
        existed = .false.

        LINE = 0
        call InitBoxCfgList%Clean_InitBoxSimCfgList()

        INQUIRE(File=Host_SimuCtrlParam%IniConfig(1:LENTRIM(Host_SimuCtrlParam%IniConfig)),exist=existed)

        if(.not. existed) then
            write(*,*) "MCPSCUERROR: The box initial file do not existed!"
            write(*,*) Host_SimuCtrlParam%IniConfig
            pause
            stop
        end if

        hFile = openExistedFile(Host_SimuCtrlParam%IniConfig)

        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        if(KEYWORD(1:LENTRIM(KEYWORD)) .ne. m_INIFSTARTFLAG) then
            write(*,*) "MCPSCUERROR: The Start Flag of Init box Parameters is Illegal: ",KEYWORD(1:LENTRIM(KEYWORD))
            pause
            stop
        end if

        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE, "!", *100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDINITINPUTF")
                    exit
                case("&GROUPSUBCTL")
                    call ReadInitBoxSimCfg_OneGroup(hFile,SimBoxes,Host_SimuCtrlParam,InitBoxCfgList,LINE,*100)

                case default
                    write(*,*) "MCPSCUERROR: You must speical the initial input group by group"
                    write(*,*) "By the way: &GROUPSUBCTL 'TYPE' "
                    write(*,*) "However, the words you input is: ",STR
                    write(*,*) "At LINE: ",LINE
                    pause
                    stop
            end select
        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load init config file"//Host_SimuCtrlParam%IniConfig(1:LENTRIM(Host_SimuCtrlParam%IniConfig))//"failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadInitBoxSimCfgList

    !*****************************************************************
    subroutine ReadInitBoxSimRecord(SimBoxes,InitBoxCfgList,SimuRecord)
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(InitBoxSimCfgList),target::InitBoxCfgList
        CLASS(SimulationRecord)::SimuRecord
        !---Local Vars---
        type(InitBoxSimCfgList),pointer::cursor=>null()
        integer::hFile
        character*1000::STR
        character*32::KEYWORD
        character*1000::fileName
        integer::LINE
        integer::RecordNum
        character*30::TheVersion
        !---Body---

        RecordNum = 0

        cursor=>InitBoxCfgList

        DO While(associated(cursor))

            if(cursor%TheValue%InitType .eq. p_ClusterIniConfig_SpecialDistFromFile) then
                fileName = cursor%TheValue%InitCfgFileName
                hFile = OpenExistedFile(fileName)

                LINE = 0

                call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
                call RemoveComments(STR,"!")

                STR = adjustl(STR)

                call GETKEYWORD("&",STR,KEYWORD)

                call UPCASE(KEYWORD)

                if(ISSTREQUAL(adjustl(trim(KEYWORD)),OKMC_OUTCFG_FORMAT18)) then
                    if(RecordNum .GE. 1) then
                        write(*,*) "MCPSCUERROR: Do not allow two records for initialization."
                        pause
                        stop
                    end if

                    call SimBoxes%Putin_OKMC_OUTCFG_FORMAT18_SimRecord(hFile,SimuRecord,TheVersion,LINE)
                    RecordNum = RecordNum + 1
                end if

                close(hFile)

            end if

            cursor=>cursor%next
        END DO

        Nullify(cursor)
        cursor=>null()

        return

        100 write(*,*) "MCPSCUERROR : Load init config file"//fileName//"failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine

    !*****************************************************************
    subroutine ReadInitBoxSimCfg_OneGroup(hFile,SimBoxes,Host_SimuCtrlParam,InitBoxCfgList,LINE,*)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfgList)::InitBoxCfgList
        integer::LINE
        !---Local Vars---
        type(InitBoxSimCfg)::tempInitBoxSimCfg
        character*1000::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::N
        !---Body---

        call tempInitBoxSimCfg%Clean_InitBoxSimCfg()

        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE, "!", *100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&TYPE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for bubbles initial type : "
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way : &TYPE The bubble initial type =  "
                        pause
                        stop
                    end if
                    tempInitBoxSimCfg%InitType = ISTR(STRTMP(1))
                    exit
                case default
                    write(*,*) "MCPSCUERROR: You must special the bubble init type first!"
                    write(*,*) "By the way: &TYPE The initial type = "
                    write(*,*) "However, the words you input is: ",STR
                    pause
                    stop
            end select
        END DO

        select case(tempInitBoxSimCfg%InitType)
            case(p_ClusterIniConfig_Simple)
                call ReadInitSimulationBoxesConfig_Simple(hFile,SimBoxes,Host_SimuCtrlParam,tempInitBoxSimCfg,LINE)
            case(p_ClusterIniConfig_SpecialDistFromFile)
                call ReadInitSimulationBoxesConfig_SpecialDistFromFile(hFile,SimBoxes,Host_SimuCtrlParam,tempInitBoxSimCfg,LINE)
            case(p_ClusterIniConfig_SpecialDistFromExteFunc)
                call ReadInitSimulationBoxesConfig_SpecialDistFromExteFunc(hFile,SimBoxes,Host_SimuCtrlParam,tempInitBoxSimCfg,LINE)
            case default
                write(*,*) "MCPSCUERROR: Unknown strategy for the box initialization:",tempInitBoxSimCfg%InitType
                pause
                stop
        end select

        call InitBoxCfgList%AppendOne_InintSimBoxCfg(tempInitBoxSimCfg)

        return
        100 return 1
    end subroutine ReadInitBoxSimCfg_OneGroup

    !*****************************************************************
    subroutine ReadInitSimulationBoxesConfig_Simple(hFile,SimBoxes,Host_SimuCtrlParam,InitBoxCfg,LINE)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfg)::InitBoxCfg
        integer::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::N
        integer::IBox
        integer::MultiBox
        integer::SNC0
        !---Body---

        MultiBox = Host_SimuCtrlParam%MultiBox

        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE, "!", *100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&NUMBER")

                    allocate(InitBoxCfg%NClusters(MultiBox))

                    call EXTRACT_NUMB(STR,MultiBox,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the bubble number"
                        write(*,*) "You shoud special: &NCLUSTER THE INITIAL NUMBER OF ENTRIES = "
                        pause
                        stop
                    else if(N .GT. 1 .AND. N .LT. MultiBox) then
                        write(*,*) "MCPSCUERROR: If you want to special the initial clusters number for each box"
                        write(*,*) "You must list the initial clusters number for all boxes."
                        write(*,*) "However, the simulation boxes number is :",MultiBox
                        write(*,*) "The initial boxes clusters numbers are listed are: ",N
                        write(*,*) "If you don not want to special the initial clusters number for each box,"
                        write(*,*) "you can only special the initial clusters number in each box is same and you need only"
                        write(*,*) "special one common clusters number."
                        pause
                        stop
                    else if(N .eq. 1) then
                        InitBoxCfg%NClusters = ISTR(STRTMP(1))
                    else if(N .eq. MultiBox) then
                        DO IBox = 1,MultiBox
                            InitBoxCfg%NClusters(IBox) = ISTR(STRTMP(IBox))
                        END DO
                    end if

                case("&SIZESUBCTL")
                    call ReadClusterSizeDist_Simple(hFile,SimBoxes,InitBoxCfg,LINE)
                case("&DEPTHSUBCTL")
                    call ReadClusterDepthDist_Simple(hFile,SimBoxes,InitBoxCfg,LINE)
                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD
                    pause
                    stop
            end select

        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load init config file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadInitSimulationBoxesConfig_Simple

    !****************************************************************
    subroutine ReadInitSimulationBoxesConfig_SpecialDistFromFile(hFile,SimBoxes,Host_SimuCtrlParam,InitBoxCfg,LINE)
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfg)::InitBoxCfg
        integer::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*200::STRTMP(10)
        integer::N
        !---Body---

        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE, "!", *100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&INITFILE")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the clusters initialize file path"
                        write(*,*) "You should special: &INITFILE The initialize file path = "
                        pause
                        stop
                    end if

                    InitBoxCfg%InitCfgFileName = INQUIREFILE(STRTMP(1),Host_SimuCtrlParam%InputFilePath)

                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD
                    pause
                    stop
            end select
        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load init config file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadInitSimulationBoxesConfig_SpecialDistFromFile

     !*****************************************************************
    subroutine ReadInitSimulationBoxesConfig_SpecialDistFromExteFunc(hFile,SimBoxes,Host_SimuCtrlParam,InitBoxCfg,LINE)
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfg)::InitBoxCfg
        integer::LINE

        ! @todo (zhail#1#):


        return
    end subroutine ReadInitSimulationBoxesConfig_SpecialDistFromExteFunc

    !***************************************************************
    subroutine ReadClusterSizeDist_Simple(hFile,SimBoxes,InitBoxCfg,LINE)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(InitBoxSimCfg)::InitBoxCfg
        integer::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::N
        integer::NElements
        integer::I
        integer::TheIndex
        !---Body---
        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            SELECT CASE(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                CASE("&NATOMDIST")
                    call EXTRACT_NUMB(STR,4,N,STRTMP)
                    if(N .LT. 4) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the cluster size distribution"
                        write(*,*) "You should special: &NATOMDIST The atoms number in each cluster distribution as Gauss that central = , distribution half width = , left cut = ,right cut ="
                        pause
                        stop
                    end if
                    InitBoxCfg%NAINI = DRSTR(STRTMP(1))
                    InitBoxCfg%NASDINI  = DRSTR(STRTMP(2))
                    InitBoxCfg%NACUT(1) = DRSTR(STRTMP(3))
                    InitBoxCfg%NACUT(2) = DRSTR(STRTMP(4))

                    if(InitBoxCfg%NACUT(1) .GE. InitBoxCfg%NACUT(2)) then
                        write(*,*) "MCPSCUERROR: The right cut cannot less than left cut."
                        write(*,*) "LCut",InitBoxCfg%NACUT(1)
                        write(*,*) "RCut",InitBoxCfg%NACUT(2)
                        pause
                        stop
                    end if
                case("&ELEMENTCOMPOSIT")
                    call EXTRACT_SUBSTR(STR,p_ATOMS_GROUPS_NUMBER,NElements,STRTMP)
                    if(NElements .LE. 0) then
                        write(*,*) "MCPSCUERROR: None of atoms kind (Elements) are specialized "
                        write(*,*) "You should special like that : &ELEMENTCOMPOSIT The included element = 'A', 'B' ."
                        pause
                        stop
                    else if(NElements .GT. p_ATOMS_GROUPS_NUMBER) then
                        write(*,*) "MCPSCUERROR: the specialized elements kinds is : ",N
                        write(*,*) "which is great than the max permitted elements kinds :",p_ATOMS_GROUPS_NUMBER
                        pause
                        stop
                    else
                        DO I = 1,NElements
                            InitBoxCfg%Elemets(I) = adjustl(trim(STRTMP(I)))
                            call UPCASE(InitBoxCfg%Elemets(I))
                        END DO
                    end if

                    call EXTRACT_NUMB(STR,p_ATOMS_GROUPS_NUMBER,N,STRTMP)
                    if(N .ne. NElements) then
                        write(*,*) "MCPSCUERROR: The elements weights number is not equal with the elements kinds which given."
                        write(*,*) "The elements kinds number is :",NElements
                        write(*,*) "But the weights number is :",N
                        pause
                        stop
                    else
                        InitBoxCfg%CompositWeight = 0.D0

                        DO I = 1,N
                            TheIndex = SimBoxes%Atoms_list%FindIndexBySymbol(InitBoxCfg%Elemets(I))
                            InitBoxCfg%CompositWeight(TheIndex) = DRSTR(STRTMP(I))
                        END DO

                        if(sum(InitBoxCfg%CompositWeight) .LE. 0.D0) then
                            write(*,*) "MCPSCUERROR: The sum of elements weights must great than 0 ."
                            write(*,*) STR
                            write(*,*) "At Line :",LINE
                            pause
                            stop
                        end if

                        InitBoxCfg%CompositWeight = InitBoxCfg%CompositWeight/sum(InitBoxCfg%CompositWeight)
                    end if
                CASE default
                    write(*,*) "MCPSCUERROR: Illegal Symbol: ", KEYWORD
                    pause
                    stop
            END SELECT

        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load init config file failed for cluster size!"
            write(*,*) "At line :",LINE
            write(*,*) STR
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadClusterSizeDist_Simple

    !***************************************************************
    subroutine ReadClusterDepthDist_Simple(hFile,Host_SimBoxes,InitBoxCfg,LINE)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        type(SimulationBoxes)::Host_SimBoxes
        type(InitBoxSimCfg)::InitBoxCfg
        integer::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::N
        integer::LayerNum
        integer::I
        real(kind=KINDDF)::TotalLayerThick
        real(kind=KINDDF)::TotalPNC
        !---Body---

        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            SELECT CASE(KEYWORD(1:LENTRIM(KEYWORD)))
                CASE("&ENDSUBCTL")
                    exit
                CASE("&DEPTH_LAYER")
                    InitBoxCfg%InitDepthDistType = p_DEPT_DIS_Layer

                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the bubble depth distribution layer type"
                        write(*,*) "You should special: &DEPTH_LAYER THE NUMBER OF DEPTH DISTRIBUTION LAYER = , THE ENTRIES DISTRIBUTION ="
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if
                    LayerNum = ISTR(STRTMP(1))
                    if(LayerNum .LT. 1) then
                        write(*,*) "MCPSCUERROR: The layer number should greater than 1"
                        write(*,*) "At line :",LINE
                        write(*,*) STR
                        pause
                        stop
                    end if

                    call EXTRACT_NUMB(STR,LayerNum*2+1,N,STRTMP)

                    if((N-1) .NE. LayerNum*2) then
                        write(*,*) "MCPSCUERROR: the specialeld layer is not equal with your setting"
                        write(*,*) STR
                        write(*,*) "At line :", LINE
                        pause
                        stop
                    end if

                    call AllocateArray_Host(InitBoxCfg%LayerThick,LayerNum,"LayerThick")
                    InitBoxCfg%LayerThick = 0.D0
                    call AllocateArray_Host(InitBoxCfg%PNCLayers,LayerNum,"PNCLayers")
                    InitBoxCfg%PNCLayers = 0.D0

                    DO I = 1,LayerNum
                        InitBoxCfg%LayerThick(I) = DRSTR(STRTMP(I+1))
                    END DO

                    if(sum(InitBoxCfg%LayerThick) .LE. 0) then
                        InitBoxCfg%LayerThick(1) = 1.D0
                    end if

                    TotalLayerThick =  sum(InitBoxCfg%LayerThick)

                    DO I = 1,LayerNum
                        InitBoxCfg%LayerThick(I) = Host_SimBoxes%BOXSIZE(3)*InitBoxCfg%LayerThick(I)/TotalLayerThick
                    END DO

                    DO I = 1,LayerNum
                        InitBoxCfg%PNCLayers(I) = DRSTR(STRTMP(I + LayerNum + 1))
                    END DO

                    TotalPNC = sum(InitBoxCfg%PNCLayers)
                    if(TotalPNC .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total percent cannot less than 0"
                        pause
                        stop
                    end if
                    InitBoxCfg%PNCLayers = InitBoxCfg%PNCLayers/TotalPNC


                CASE("&DEPTH_SUBBOX")

                    InitBoxCfg%InitDepthDistType = p_DEPT_DIS_BOX

                    call EXTRACT_NUMB(STR,6,N,STRTMP)
                    if(N .LT. 6) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the bubble depth distribution subbox type"
                        write(*,*) "You shoud special: &DEPTH_SUBBOX THE SUBOX SHAPE IS THAT: X1 = , x2= , Y1 = , Y2 =, Z1=, Z2= in (LU) "
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if
                    DO I=1, 3
                        InitBoxCfg%SUBBOXBOUNDARY(I,1) = DRSTR(STRTMP((I-1)*2 + 1))*Host_SimBoxes%LatticeLength
                        InitBoxCfg%SUBBOXBOUNDARY(I,2) = DRSTR(STRTMP((I-1)*2 + 2))*Host_SimBoxes%LatticeLength

                        if(InitBoxCfg%SUBBOXBOUNDARY(I,1) .LT. Host_SimBoxes%BOXBOUNDARY(I,1) .or. InitBoxCfg%SUBBOXBOUNDARY(I,1) .GT. Host_SimBoxes%BOXBOUNDARY(I,2)) then
                            write(*,*) "MCPSCUERROR: The subbox boundary should not beyond the origin box.",InitBoxCfg%SUBBOXBOUNDARY(I,1)
                            write(*,*) Host_SimBoxes%BOXBOUNDARY(I,1),Host_SimBoxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if

                        if(InitBoxCfg%SUBBOXBOUNDARY(I,2) .LT. Host_SimBoxes%BOXBOUNDARY(I,1) .or. InitBoxCfg%SUBBOXBOUNDARY(I,2) .GT. Host_SimBoxes%BOXBOUNDARY(I,2)) then
                            write(*,*) "MCPSCUERROR: The subbox boundary should not beyond the origin box.",InitBoxCfg%SUBBOXBOUNDARY(I,2)
                            write(*,*) Host_SimBoxes%BOXBOUNDARY(I,1),Host_SimBoxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if
                    END DO

                CASE("&DEPTH_GAUSS")

                    InitBoxCfg%InitDepthDistType = p_DEPT_DIS_GAS

                    call EXTRACT_NUMB(STR,2,N,STRTMP)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the bubble depth distribution gauss type"
                        write(*,*) "You shoud special: &DEPTH_GAUSS THE GAUSS DISTRIBUTION CENTRAL = , THE HALF WIDTH = (in LU) "
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if
                    InitBoxCfg%DepthINI = DRSTR(STRTMP(1))*Host_SimBoxes%LatticeLength
                    InitBoxCfg%DepthSDINI = DRSTR(STRTMP(2))*Host_SimBoxes%LatticeLength
                CASE default
                    write(*,*) "MCPSCUERROR: Illegal Symbol: ", KEYWORD
                    pause
                    stop
            END SELECT

        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load init config file failed for bubble depth distribution!"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadClusterDepthDist_Simple

    !*****************************************************************
    subroutine DOInitSimulationBoxesConfig(SimBoxes,Host_SimuCtrlParam,Record,InitBoxCfgList)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        type(InitBoxSimCfgList),target::InitBoxCfgList
        !---Local Vars---
        integer::MultiBox
        type(InitBoxSimCfgList),pointer::cursor=>null()
        integer::IBox
        integer::TNC0
        !---Body---

        MultiBox = Host_SimuCtrlParam%MultiBox

        cursor=>InitBoxCfgList

        call SimBoxes%m_ClustersInfo_CPU%Clean()

        call SimBoxes%m_BoxesBasicStatistic%Init(MultiBox)

        call SimBoxes%m_BoxesInfo%Init(MultiBox)

        DO While(associated(cursor))

            select case(cursor%TheValue%InitType)
                case(p_ClusterIniConfig_Simple)
                    call DoInitSimulationBoxesConfig_Simple(SimBoxes,Host_SimuCtrlParam,cursor%TheValue)
                case(p_ClusterIniConfig_SpecialDistFromFile)
                    call DoInitSimulationBoxesConfig_SpecialDistFromFile(SimBoxes,Host_SimuCtrlParam,Record,cursor%TheValue)
                case(p_ClusterIniConfig_SpecialDistFromExteFunc)
                    call DoInitSimulationBoxesConfig_SpecialDistFromExteFunc(SimBoxes,Host_SimuCtrlParam,cursor%TheValue)
                case default
                    write(*,*) "MCPSCUERROR: Unknown strategy for the box initialization:",cursor%TheValue%InitType
                    pause
                    stop
            end select

            cursor=>cursor%next
        END DO

        Nullify(cursor)
        cursor=>null()

        return
    end subroutine

    !****************************************************************
    subroutine DoInitSimulationBoxesConfig_Simple(SimBoxes,Host_SimuCtrlParam,InitBoxCfg)
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfg)::InitBoxCfg
        !---Body--
        select case(InitBoxCfg%InitDepthDistType)
            case(p_DEPT_DIS_Layer)
                call Init_Depth_Dis_LAY(SimBoxes,Host_SimuCtrlParam,InitBoxCfg)
            case(p_DEPT_DIS_BOX)
                call Init_Depth_Dis_SubBox(SimBoxes,Host_SimuCtrlParam,InitBoxCfg)
            case(p_DEPT_DIS_GAS)
                call Init_Depth_Dis_Gauss(SimBoxes,Host_SimuCtrlParam,InitBoxCfg)
            case default
                write(*,*) "MCPSCUERROR : Unknown way to initial the simulation box conifuration :",InitBoxCfg%InitDepthDistType
                pause
                stop
        end select

        return
    end subroutine DoInitSimulationBoxesConfig_Simple

    !****************************************************************
    subroutine DoInitSimulationBoxesConfig_SpecialDistFromFile(SimBoxes,Host_SimuCtrlParam,Record,InitBoxCfg)
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        type(InitBoxSimCfg)::InitBoxCfg
        character*30::TheVersion
        !---Body---

        call SimBoxes%PutinCfg(Host_SimuCtrlParam,Record,InitBoxCfg%InitCfgFileName,m_FREESURDIFPRE,m_GBSURDIFPRE,TheVersion,AsInitial=.true.)
        write(*,*) "The KMC Configuration version is: ",TheVersion

        return
    end subroutine DoInitSimulationBoxesConfig_SpecialDistFromFile

    !*****************************************************************
    subroutine DoInitSimulationBoxesConfig_SpecialDistFromExteFunc(SimBoxes,Host_SimuCtrlParam,InitBoxCfg)
        !---Dummy Vars---
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfg)::InitBoxCfg
        ! @todo (zhail#1#):


        return
    end subroutine DoInitSimulationBoxesConfig_SpecialDistFromExteFunc

    !**************************************************************
    subroutine Init_Depth_Dis_LAY(Host_Boxes,Host_SimuCtrlParam,InitBoxCfg)
      !*** Purpose: To initialize the system (clusters distributed as the form of layer)
      ! Host_Boxes: the boxes information in host
      implicit none
      !---Dummy Vars---
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(InitBoxSimCfg)::InitBoxCfg
      !-----local variables---
      integer::MultiBox
      real(kind=KINDDF)::POS(3)
      real(kind=KINDDF)::Z0
      real(kind=KINDDF)::BOXBOUNDARY(3,2)
      real(kind=KINDDF)::BOXSIZE(3)
      integer::IBox, II, IC, LAY, PNC
      integer::J
      integer::SNC0,SNC
      integer::LayerNum
      integer::NAtoms
      type(DiffusorValue)::TheDiffusorValue
      integer::TheDim
      !---Body---
      MultiBox = Host_SimuCtrlParam%MultiBox

      BOXBOUNDARY = Host_Boxes%BOXBOUNDARY
      BOXSIZE = Host_Boxes%BOXSIZE

      LayerNum = size(InitBoxCfg%LayerThick)

      call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParam,InitBoxCfg%NClusters)

      DO IBox = 1,MultiBox

        SNC0 = InitBoxCfg%NClusters(IBox)
        SNC  = 0
        Z0  = 0.D0

        IC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - SNC0

        DO LAY=1, LayerNum

            if(LAY .EQ. LayerNum) THEN
                PNC = SNC0 - SNC
            else
                PNC = SNC0*InitBoxCfg%PNCLayers(LAY)
            end if

            SNC = SNC + PNC
            Z0 = BOXBOUNDARY(3,1) + sum(InitBoxCfg%LayerThick(1:LAY-1))

            DO II = 1, PNC

                IC = IC + 1

                call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                !Initialize the position of clusters
                POS(1) = DRAND32()*BOXSIZE(1)+BOXBOUNDARY(1,1)
                POS(2) = DRAND32()*BOXSIZE(2)+BOXBOUNDARY(2,1)
                POS(3) = DRAND32()*InitBoxCfg%LayerThick(LAY) + Z0
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS

                !Give the cluster an type(layer) ID for the convenience of visualization
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = LAY

                !*** Initialize the size of the clusters
                NAtoms = RGAUSS0_WithCut(InitBoxCfg%NAINI, InitBoxCfg%NASDINI,InitBoxCfg%NACUT(1),InitBoxCfg%NACUT(2))

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA = NAtoms*InitBoxCfg%CompositWeight

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

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
                                                                                                   Host_SimuCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                end select

                select case(TheDiffusorValue%DiffusorValueType_Free)
                    case(p_DiffuseCoefficient_ByValue)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                    case(p_DiffuseCoefficient_ByArrhenius)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                    case(p_DiffuseCoefficient_ByBCluster)
                        ! Here we adopt a model that D=D0*(1/R)**Gama
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                    case(p_DiffuseCoefficient_BySIACluster)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                    case(p_DiffuseCoefficient_ByVcCluster)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                end select

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                    DO TheDim = 1,3
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                    END DO
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                end if

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParam%TKB)

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

            END DO
        END DO

      END DO

      return
    end subroutine Init_Depth_Dis_LAY

    !**************************************************************
    subroutine Init_Depth_Dis_SubBox(Host_Boxes,Host_SimuCtrlParam,InitBoxCfg)
      !*** Purpose: To initialize the system (clusters distributed as the form of layer)
      ! Host_Boxes: the boxes information in host
      implicit none
      !---Dummy Vars---
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(InitBoxSimCfg)::InitBoxCfg
      !-----local variables---
      integer::MultiBox
      real(kind=KINDDF)::POS(3)
      real(kind=KINDDF)::SUBBOXSIZE(3)
      integer::IBox, II, IC
      integer::SNC0
      integer::I
      integer::NAtoms
      type(DiffusorValue)::TheDiffusorValue
      integer::TheDim
      !---Body---
      MultiBox = Host_SimuCtrlParam%MultiBox

      DO I = 1,3
        SUBBOXSIZE(I) = InitBoxCfg%SUBBOXBOUNDARY(I,2) - InitBoxCfg%SUBBOXBOUNDARY(I,1)
      END DO

      call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParam,InitBoxCfg%NClusters)

      DO IBox = 1,MultiBox

        SNC0 = InitBoxCfg%NClusters(IBox)

        IC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - SNC0

        DO II = 1, SNC0

            IC = IC + 1
            !Initialize the position of clusters
            call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

            DO I = 1,3
                POS(I) = DRAND32()*SUBBOXSIZE(I) + InitBoxCfg%SUBBOXBOUNDARY(I,1)
            END DO

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS
            !Give the cluster an type(layer) ID for the convenience of visualization
            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = 1

            NAtoms = RGAUSS0_WithCut(InitBoxCfg%NAINI, InitBoxCfg%NASDINI,InitBoxCfg%NACUT(1),InitBoxCfg%NACUT(2))

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA = NAtoms*InitBoxCfg%CompositWeight

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

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
                                                                                               Host_SimuCtrlParam%TKB,                                      &
                                                                                               Host_Boxes%LatticeLength)
            end select

            select case(TheDiffusorValue%DiffusorValueType_Free)
                case(p_DiffuseCoefficient_ByValue)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                case(p_DiffuseCoefficient_ByArrhenius)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                case(p_DiffuseCoefficient_ByBCluster)
                    ! Here we adopt a model that D=D0*(1/R)**Gama
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                case(p_DiffuseCoefficient_BySIACluster)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                case(p_DiffuseCoefficient_ByVcCluster)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            end select

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
            if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                DO TheDim = 1,3
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                END DO

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
            end if

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParam%TKB)

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = 1
            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

            Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
            Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

            Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
            Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

            Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
            Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1
        END DO

      END DO

      return
    end subroutine Init_Depth_Dis_SubBox

    !**************************************************************
    subroutine Init_Depth_Dis_Gauss(Host_Boxes,Host_SimuCtrlParam,InitBoxCfg)
        !*** Purpose: To initialize the system (clusters distributed as the form of gauss in depth)
        ! Host_Boxes: the boxes information in host
        implicit none
        !-------Dummy Vars------
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(InitBoxSimCfg)::InitBoxCfg
        !---local variables---
        integer::MultiBox
        real(kind=KINDDF)::POS(3)
        real(kind=KINDDF)::SEP
        real(kind=KINDDF)::BOXBOUNDARY(3,2)
        real(kind=KINDDF)::BOXSIZE(3)
        integer::IBox,II,IC
        integer::SNC0
        integer::NAtoms
        type(DiffusorValue)::TheDiffusorValue
        integer::TheDim
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox
        BOXBOUNDARY = Host_Boxes%BOXBOUNDARY
        BOXSIZE = Host_Boxes%BOXSIZE

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParam,InitBoxCfg%NClusters)

        DO IBox = 1,MultiBox

            SNC0 = InitBoxCfg%NClusters(IBox)

            IC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - SNC0

            DO II = 1, SNC0

                IC = IC + 1
                !Initialize the position of clusters
                call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                POS(1) = DRAND32()*BOXSIZE(1) + BOXBOUNDARY(1,1)
                POS(2) = DRAND32()*BOXSIZE(2) + BOXBOUNDARY(2,1)
                POS(3) = RGAUSS0_WithCut(InitBoxCfg%DepthINI, InitBoxCfg%DepthSDINI,BOXBOUNDARY(3,1),BOXBOUNDARY(3,2))

                if(POS(3) .LT. BOXBOUNDARY(3,1)) then
                    POS(3) = BOXBOUNDARY(3,1)
                end if

                if(POS(3) .GT. BOXBOUNDARY(3,2)) then
                    POS(3) = BOXBOUNDARY(3,2)
                end if

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS

                !Give the cluster an type(layper) ID for the convenience of visualization
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = 1

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                !*** Initialize the size of the clusters
                NAtoms = RGAUSS0_WithCut(InitBoxCfg%NAINI, InitBoxCfg%NASDINI,InitBoxCfg%NACUT(1),InitBoxCfg%NACUT(2))

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA = NAtoms*InitBoxCfg%CompositWeight

                TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                !---you should init the distribution by external file---
                select case(TheDiffusorValue%ECRValueType_Free)
                    case(p_ECR_ByValue)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                    case default
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                           &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                end select

                select case(TheDiffusorValue%DiffusorValueType_Free)
                    case(p_DiffuseCoefficient_ByValue)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                    case(p_DiffuseCoefficient_ByArrhenius)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                    case(p_DiffuseCoefficient_ByBCluster)
                        ! Here we adopt a model that D=D0*(1/R)**Gama
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                    case(p_DiffuseCoefficient_BySIACluster)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                    case(p_DiffuseCoefficient_ByVcCluster)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
                end select

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                    DO TheDim = 1,3
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                    END DO

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                end if

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParam%TKB)

                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1
            END DO

        END DO

        return
   end subroutine Init_Depth_Dis_Gauss

    !*****************************************************************
    subroutine OutPutCurrent(Host_Boxes,Dev_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        logical::OutIntegralBoxStatistic
        logical::OutEachBoxStatistic
        integer::NC0
        !---Body---

        OutIntegralBoxStatistic = .false.
        OutEachBoxStatistic = .true.

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
            NC0 = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            NC0 = 0
        end if

        OutIntegralBoxStatistic = Record%WhetherOutSizeDist_IntegralBox(Host_SimuCtrlParam)

        OutEachBoxStatistic = Record%WhetherOutSizeDist_EachBox(Host_SimuCtrlParam)


        if(OutIntegralBoxStatistic .eq. .true. .or. OutEachBoxStatistic .eq. .true. .or. Record%GetStatusTriggerFocusedTimePoints() .eq. .true.) then
            call GetBoxesMigCoaleStat_Used_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatisticInfo,Record)

            if(OutIntegralBoxStatistic .eq. .true.) then
                call PutOut_Instance_Statistic_IntegralBox(Host_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record,Model=0)

                if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalSteps) then
                    call Record%SetLastOutSizeDistTime_IntegralBox(dble(Record%GetSimuSteps()))
                else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalRealTime) then
                    call Record%SetLastOutSizeDistTime_IntegralBox(Record%GetSimuTimes())
                else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalTimeMagnification) then
                    call Record%SetLastOutSizeDistTime_IntegralBox(Record%GetSimuTimes())
                end if
            end if

            if(OutEachBoxStatistic .eq. .true.) then
                call PutOut_Instance_Statistic_EachBox(Host_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record)

                if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalSteps) then
                    call Record%SetLastOutSizeDistTime_EachBox(dble(Record%GetSimuSteps()))
                else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalRealTime) then
                    call Record%SetLastOutSizeDistTime_EachBox(Record%GetSimuTimes())
                else if(Host_SimuCtrlParam%OutPutSCFlag .eq. mp_OutTimeFlag_ByIntervalTimeMagnification) then
                    call Record%SetLastOutSizeDistTime_EachBox(Record%GetSimuTimes())
                end if
            end if

            if(Record%GetStatusTriggerFocusedTimePoints() .eq. .true.) then
                call PutOut_Instance_Statistic_IntegralBox(Host_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record,Model=0)
                call PutOut_Instance_Statistic_EachBox(Host_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record)
            end if

        end if

        ! check if need to output intermediate configure
        if(Host_SimuCtrlParam%OutPutConfFlag .eq. mp_OutTimeFlag_ByIntervalSteps) then
            if((Record%GetSimuSteps() - Record%GetLastRecordOutConfigTime()) .GE. Host_SimuCtrlParam%OutPutConfValue .OR. &
                Record%GetSimuTimes() .GE. Host_SimuCtrlParam%TermTValue) then

                call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

                call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

                call Record%SetLastRecordOutConfigTime(dble(Record%GetSimuSteps()))

            end if
        else if(Host_SimuCtrlParam%OutPutConfFlag .eq. mp_OutTimeFlag_ByIntervalRealTime) then
            if((Record%GetSimuTimes() - Record%GetLastRecordOutConfigTime()) .GE. Host_SimuCtrlParam%OutPutConfValue .OR. &
                Record%GetSimuTimes() .GE. Host_SimuCtrlParam%TermTValue) then

                call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

                call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

                call Record%SetLastRecordOutConfigTime(Record%GetSimuTimes())
            end if

        else if(Host_SimuCtrlParam%OutPutConfFlag .eq. mp_OutTimeFlag_ByIntervalTimeMagnification) then
            if((Record%GetSimuTimes()/Host_SimuCtrlParam%OutPutConfValue) .GE. Record%GetLastRecordOutConfigTime() .OR. &
                Record%GetSimuTimes() .GE. Host_SimuCtrlParam%TermTValue) then

                call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

                call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

                call Record%SetLastRecordOutConfigTime(Record%GetSimuTimes())
            end if

        else if(Record%GetStatusTriggerFocusedTimePoints() .eq. .true.) then
                call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

                call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)
        end if

    end subroutine

    !*****************************************************************
    subroutine PutOut_Instance_Statistic_IntegralBox(Host_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record,Model)
        implicit none
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::Model                           ! 1 would output ot file
        !---Local Vars---
        integer::MultiBox
        real(kind=KINDDF)::RMIN
        real(kind=KINDDF)::Concentrate
        integer::NCAct
        real(kind=KINDDF)::RAVA
        real(kind=KINDDF)::NAVA
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        ASSOCIATE(TBasicInfo=>Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral,TMigStatInfo=>TheMigCoaleStatisticInfo%statistic_IntegralBox)

            if(Model .eq. 0) then

                NCAct = TBasicInfo%NC(p_ACTIVEFREE_STATU) + TBasicInfo%NC(p_ACTIVEINGB_STATU)

                RAVA = (TBasicInfo%NC(p_ACTIVEFREE_STATU)*TMigStatInfo%RAVA(p_ACTIVEFREE_STATU) + TBasicInfo%NC(p_ACTIVEINGB_STATU)*TMigStatInfo%RAVA(p_ACTIVEINGB_STATU))/NCAct

                if(NCAct .GT. 0) then
                    NAVA = dble(TBasicInfo%NA(p_ACTIVEFREE_STATU) +  TBasicInfo%NA(p_ACTIVEINGB_STATU))/dble(NCAct)
                else
                    NAVA = 0.D0
                end if

                Concentrate = NCAct/(MultiBox*Host_Boxes%BOXVOLUM)

                write(Record%HSizeStatistic_TotalBox, fmt="(I30,1x,2(1PE30.10,1x),10(I30,1x),17(1PE30.10,1x),8(I30,1x))") Record%GetSimuSteps(),             &
                                                                                                           Record%GetSimuTimes(),                          &
                                                                                                           Record%GetTimeSteps(),                          &
                                                                                                           NCAct,                                          &
                                                                                                           sum(TBasicInfo%NC),                             &
                                                                                                           sum(TBasicInfo%NA),                             &
                                                                                                           TBasicInfo%NC(1:p_NUMBER_OF_STATU),             &
                                                                                                           TMigStatInfo%RMIN(1:p_NUMBER_OF_STATU)*C_CM2NM, &
                                                                                                           TMigStatInfo%RMAX(1:p_NUMBER_OF_STATU)*C_CM2NM, &
                                                                                                           RAVA*C_CM2NM,                                   &
                                                                                                           NAVA,                                           &
                                                                                                           Concentrate,                                    &
                                                                                                           Record%GetImplantedEntitiesNum(),                &
                                                                                                           TBasicInfo%NC(p_ACTIVEFREE_STATU),               &
                                                                                                           TBasicInfo%NC(p_ACTIVEINGB_STATU),               &
                                                                                                           TBasicInfo%NC(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU) + &
                                                                                                           Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU)
                call flush(Record%HSizeStatistic_TotalBox)

                if(m_CheckNClusters .eq. .true.) then
                    if((TBasicInfo%NC(p_ACTIVEFREE_STATU) + TBasicInfo%NC(p_ACTIVEINGB_STATU) + &
                        sum(TBasicInfo%NC(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU) + Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU)) - &
                        Record%GetImplantedEntitiesNum() - sum(TBasicInfo%NC0) - TBasicInfo%NCDumpAdded) .ne. 0) then

                        write(*,*) "MCPSCUERROR: The clusters number is not conservation."
                        write(*,*) "The accumulated clusters for all kinds =",TBasicInfo%NC(p_ACTIVEFREE_STATU) + TBasicInfo%NC(p_ACTIVEINGB_STATU) + &
                                    sum(TBasicInfo%NC(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU) + Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU))
                        write(*,*) "The total implanted cluster number = ",Record%GetImplantedEntitiesNum()
                        write(*,*) "The initial cluster number plus rescale added number = ",TBasicInfo%NC0(p_ACTIVEFREE_STATU) + TBasicInfo%NC0(p_ACTIVEINGB_STATU) + TBasicInfo%NCDumpAdded
                        pause
                    end if
                end if
            end if

            NCAct = TBasicInfo%NC(p_ACTIVEFREE_STATU) + TBasicInfo%NC(p_ACTIVEINGB_STATU)

            RAVA = (TBasicInfo%NC(p_ACTIVEFREE_STATU)*TMigStatInfo%RAVA(p_ACTIVEFREE_STATU) + TBasicInfo%NC(p_ACTIVEINGB_STATU)*TMigStatInfo%RAVA(p_ACTIVEINGB_STATU))/NCAct

            if(NCAct .GT. 0) then
                NAVA = dble(TBasicInfo%NA(p_ACTIVEFREE_STATU) +  TBasicInfo%NA(p_ACTIVEINGB_STATU))/dble(NCAct)
            else
                NAVA = 0.D0
            end if

            Concentrate = NCAct/(MultiBox*Host_Boxes%BOXVOLUM)

            write(6, fmt= "(130(A15,1x))")   "Step",            &
                                             "Time",            &
                                             "TStep",           &
                                             "NC(ACTIVEFREE)",  &
                                             "NC(ACTIVEINGB)",  &
                                             "TNCAct",          &
                                             "TNC",             &
                                             "RMin",            &
                                             "RMax",            &
                                             "Rava",            &
                                              "NAVA",           &
                                             "MaxDiff",         &
                                             "Concentrate"
            RMIN = 1.D32
            if(TBasicInfo%NC(p_ACTIVEFREE_STATU) .GT. 0) then
                RMIN = min(RMIN,TMigStatInfo%RMIN(p_ACTIVEFREE_STATU))
            else if(TBasicInfo%NC(p_ACTIVEINGB_STATU) .GT. 0) then
                RMIN = min(RMIN,TMigStatInfo%RMIN(p_ACTIVEINGB_STATU))
            end if

            write(6, fmt= "(I15,1x,2(1PE16.8,1x),3(I15,1x),I15,1x,130(1PE16.8,1x))")   Record%GetSimuSteps(),                                                                &
                                                                                    Record%GetSimuTimes(),                                                                   &
                                                                                    Record%GetTimeSteps(),                                                                   &
                                                                                    TBasicInfo%NC(p_ACTIVEFREE_STATU),                                                       &
                                                                                    TBasicInfo%NC(p_ACTIVEINGB_STATU),                                                       &
                                                                                    NCAct,                                                                                   &
                                                                                    sum(TBasicInfo%NC),                                                                      &
                                                                                    RMIN*C_CM2NM,                                                                            &
                                                                                    max(TMigStatInfo%RMAX(p_ACTIVEFREE_STATU),TMigStatInfo%RMAX(p_ACTIVEINGB_STATU))*C_CM2NM,&
                                                                                    RAVA*C_CM2NM,                                                                            &
                                                                                    NAVA,                                                                                    &
                                                                                    max(TMigStatInfo%DiffusorValueMax(p_ACTIVEFREE_STATU),TMigStatInfo%DiffusorValueMax(p_ACTIVEINGB_STATU)), &
                                                                                    Concentrate


        END ASSOCIATE

        return
    end subroutine PutOut_Instance_Statistic_IntegralBox

    !*****************************************************************
    subroutine PutOut_Instance_Statistic_EachBox(Host_Boxes,Host_SimuCtrlParam,TheMigCoaleStatisticInfo,Record)
        implicit none
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoaleStatisticInfo_Used)::TheMigCoaleStatisticInfo
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        integer::IBox
        integer::MultiBox
        real(kind=KINDDF)::RMIN
        real(kind=KINDDF)::Concentrate
        integer::NCAct
        real(kind=KINDDF)::RAVA
        real(kind=KINDDF)::NAVA
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        DO IBox = 1,MultiBox

            ASSOCIATE(SBasicInfo=>Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox),SMigStatInfo=>TheMigCoaleStatisticInfo%statistic_SingleBoxes(IBox))

                NCAct = SBasicInfo%NC(p_ACTIVEFREE_STATU) + SBasicInfo%NC(p_ACTIVEINGB_STATU)

                RAVA = (SBasicInfo%NC(p_ACTIVEFREE_STATU)*SMigStatInfo%RAVA(p_ACTIVEFREE_STATU) + SBasicInfo%NC(p_ACTIVEINGB_STATU)*SMigStatInfo%RAVA(p_ACTIVEINGB_STATU))/NCAct

                if(NCAct .GT. 0) then
                    NAVA = dble(SBasicInfo%NA(p_ACTIVEFREE_STATU) +  SBasicInfo%NA(p_ACTIVEINGB_STATU))/dble(NCAct)
                else
                    NAVA = 0.D0
                end if

                Concentrate = NCAct/Host_Boxes%BOXVOLUM

                write(Record%HSizeStatistic_EachBox,fmt="(2(I30,1x),2(1PE30.10,1x),10(I30,1x),17(1PE30.10,1x),8(I30,1x))") Record%GetSimuSteps(),               &
                                                                                                             IBox,                                            &
                                                                                                             Record%GetSimuTimes(),                           &
                                                                                                             Record%GetTimeSteps(),                           &
                                                                                                             NCAct,                                           &
                                                                                                             sum(SBasicInfo%NC),                              &
                                                                                                             sum(SBasicInfo%NA(1:p_NUMBER_OF_STATU)),         &
                                                                                                             SBasicInfo%NC(1:p_NUMBER_OF_STATU),              &
                                                                                                             SMigStatInfo%RMIN(1:p_NUMBER_OF_STATU)*C_CM2NM,  &
                                                                                                             SMigStatInfo%RMAX(1:p_NUMBER_OF_STATU)*C_CM2NM,  &
                                                                                                             RAVA*C_CM2NM,                                    &
                                                                                                             NAVA,                                            &
                                                                                                             Concentrate,                                     &
                                                                                                             Record%GetImplantedEntitiesNum(),                &
                                                                                                             SBasicInfo%NC(p_ACTIVEFREE_STATU),               &
                                                                                                             SBasicInfo%NC(p_ACTIVEINGB_STATU),               &
                                                                                                             SBasicInfo%NC(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU) + &
                                                                                                             Record%RecordNCBeforeSweepOut_SingleBox(IBox,p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU)

            END ASSOCIATE

            call flush(Record%HSizeStatistic_EachBox)
        END DO

        return
    end subroutine PutOut_Instance_Statistic_EachBox

    !*************************************************************
    subroutine SweepOutMemory(Host_Boxes,Dev_Boxes,Host_SimuCtrlParam,TheMigCoaleStatInfoWrap,Record)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        !---Local Vars---
        integer::MultiBox
        integer::NC0
        !---Body---

        MultiBox = Host_SimuCtrlParam%MultiBox


        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
            NC0 = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            NC0 = 0
        end if

        if(Host_SimuCtrlParam%SweepOutMemory .eq. .true.) then

            if(Host_SimuCtrlParam%SweepOutFlag .eq. mp_SweepOutFlag_ByIntervalSteps) then
                if((Record%GetSimuSteps() - Record%GetLastSweepOutTime()) .GE. Host_SimuCtrlParam%SweepOutValue) then

                    call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)
                    call Record%RecordNC_ForSweepOut(MultiBox,Host_Boxes%m_BoxesBasicStatistic)

                    call Record%IncreaseOneSweepOutCount()

                    call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

                    call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record,SweepOutCount=Record%GetSweepOutCount())

                    call Dev_Boxes%SweepUnActiveMemory_GPUToCPU(Host_Boxes,Host_SimuCtrlParam)

                    call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)

                    call Record%SetLastSweepOutTime(dble(Record%GetSimuSteps()))


                    call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                      max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                          TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)),&
                                          MaxDiffuse=max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEFREE_STATU), &
                                                         TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEINGB_STATU)))

                end if

            else if(Host_SimuCtrlParam%SweepOutFlag .eq. mp_SweepOutFlag_ByIntervalRealTime) then
                if((Record%GetSimuTimes() - Record%GetLastSweepOutTime()) .GE. Host_SimuCtrlParam%SweepOutValue) then

                    call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)
                    call Record%RecordNC_ForSweepOut(MultiBox,Host_Boxes%m_BoxesBasicStatistic)

                    call Record%IncreaseOneSweepOutCount()

                    call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

                    call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record,SweepOutCount=Record%GetSweepOutCount())

                    call Dev_Boxes%SweepUnActiveMemory_GPUToCPU(Host_Boxes,Host_SimuCtrlParam)

                    call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)

                    call Record%SetLastSweepOutTime(Record%GetSimuTimes())

                    call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                      max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                          TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)),&
                                          MaxDiffuse=max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEFREE_STATU), &
                                                         TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%DiffusorValueMax(p_ACTIVEINGB_STATU)))
                end if
            end if

        end if

        return
    end subroutine


end module MCRT_Method_MIGCOALE_CLUSTER_CPU
