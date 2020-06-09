module INLET_TYPEDEF_IMPLANTSECTION
    use cudafor
    use MCLIB_CONSTANTS_GPU
    use MCLIB_TYPEDEF_BASICRECORD
    use MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_TYPEDEF_ClustersInfo_CPU
    use MCLIB_GLOBAL_GPU
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY_GPU
    use MCLIB_CAL_NEIGHBOR_LIST_GPU
    use MIGCOALE_TIMECTL
    use MIGCOALE_TYPEDEF_STATISTICINFO
    use MIGCOALE_STATISTIC_GPU
    use MIGCOALE_STATISTIC_CPU
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MIGCOALE_GLOBALVARS_DEV
    use MODEL_ECR_GPU
    implicit none

    ! Note: This is DISTOKMC18 different with OKMC_OUTCFG_FORMAT18, the main different is cause that for implant,
    !       what we need for the implant source is a discrete distribution(size and space), not a special configuration.
    !       So, to use the OKMC configuration for implant, we need to convert the OKMC configuration to a discrete distribution.
    !       However, for the mean-filed result, it is just the distribution that we need, so we need not to do any other things.
    character(len=9),private,parameter::SRIM_DIST = "&DISTSRIM"
    character(len=10),private,parameter::PANDA_DIST = "&DISTPANDA"

    character(len=10),parameter::BATCHCONFIG = "&BATCHCONFIG"

    integer,private,parameter::p_Implant_Hunger = 0
    integer,private,parameter::p_Implant_MemSaving = 1

    integer, parameter, private::p_ImplantConfig_Simple = 0
    integer, parameter, private::p_ImplantConfig_SpecialDistFromFile = 1
    integer, parameter, private::p_ImplantConfig_SpecialDistFromExteFunc = 2

    integer,parameter,private::p_InsertCountModel_ByConfigNum = 0
    integer,parameter,private::p_InsertCountModel_ByClusterNum = 1

    integer,parameter,private::p_InsertConfig_ByAlphaBeta = 0
    integer,parameter,private::p_InsertConfig_Random = 1

    #ifdef MAXBATCHINSERTTP
    integer,parameter,private::p_MAX_BATCHINSERTTIMEPOINTS = MAXBATCHINSERTTP
    #else
    integer,parameter,private::p_MAX_BATCHINSERTTIMEPOINTS = 10
    #endif

    type,public::ImplantSection

        integer::ImplantConfigType = -1

        integer::ExpandFactor = 1

        integer::MemoryOccupyFactor = 100

        real(kind=KINDDF)::InsertTimeInterval = -1.0

        integer::NInsertTimePoint = 0
        real(kind=KINDDF),dimension(:),allocatable::InsertTimePoint

        integer::InsertCountModel = p_InsertCountModel_ByConfigNum

        integer::InsertCountOneBatch = 0

        character*1000::ConfigFolder = ""

        integer::InsetSequence = p_InsertConfig_ByAlphaBeta

        type(STRList),pointer::ImplantCfgFileList=>null()

        character*20::ImplantCfgFileType = ""

        character*10::Elemets(p_ATOMS_GROUPS_NUMBER) = ""

        real(kind=KINDDF)::NAINI = 0.D0

        real(kind=KINDDF)::NASDINI = 0.D0

        real(kind=KINDDF)::NACUT(2) = 0.D0

        real(kind=KINDDF)::CompositWeight(p_ATOMS_GROUPS_NUMBER) = 0.D0

        integer::ImplantDepthDistType = -1

        real(kind=KINDDF)::SUBBOXBOUNDARY(3,2) = 0.D0

        real(kind=KINDDF)::DepthINI = 0.D0

        real(kind=KINDDF)::DepthSDINI = 0.D0

        real(kind=KINDDF),dimension(:),allocatable::LayerThick

        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleRate

        type(ACluster),dimension(:,:),allocatable::ClustersSample

        contains

        procedure,non_overridable,public,pass::LoadOne_ImplantSection
        procedure,non_overridable,public,pass::ReadImplantCommonCtl
        procedure,non_overridable,public,pass::ReadRateCtrl
        procedure,non_overridable,public,pass::ReadImplantSource
        procedure,non_overridable,public,pass::ReadImplantSource_Simple
        procedure,non_overridable,public,pass::ReadImplantClusterSizeDist_Simple
        procedure,non_overridable,public,pass::ReadImplantClusterDepthDist_Simple
        procedure,non_overridable,public,pass::ReadImplantSource_SpecialDistFromFile
        procedure,non_overridable,public,pass::Putin_PANDA_OUTCFG_Distribution
        procedure,non_overridable,public,pass::Putin_SRIM2003_OUTCFG_Distribution
        procedure,non_overridable,public,pass::ReadImplantSection_SpecialDistFromExteFunc

        procedure,non_overridable,public,pass::Implant
        !---Continue Implant
        procedure,non_overridable,public,pass::ImplantClusters_Contiune
        procedure,non_overridable,public,pass::AdjustTimeStep_ImplantContiune
        procedure,non_overridable,public,pass::Cal_ExpandSize_ImplantContiune
        procedure,non_overridable,public,pass::DoImplantTillVirtualBoundary_CPUTOGPU_ImplantContiune
        procedure,non_overridable,public,pass::DoImplantTillVirtualBoundary_CPU_ImplantContiune
        procedure,non_overridable,public,pass::FillVirtualBoundary_CPU_Depth_LAY_ImplantContiune
        procedure,non_overridable,public,pass::FillVirtualBoundary_CPU_Depth_SubBox_ImplantContiune
        procedure,non_overridable,public,pass::FillVirtualBoundary_CPU_Depth_Gauss_ImplantContiune
        procedure,non_overridable,public,pass::FillVirtualBoundary_CPU_Simple_ImplantContiune
        procedure,non_overridable,public,pass::FillVirtualBoundary_CPU_FromFile_ImplantContiune
        procedure,non_overridable,public,pass::FillVirtualBoundary_CPU_FromExteFunc_ImplantContiune

        !---Batch Implant---
        procedure,non_overridable,public,pass::ImplantClusters_BatchFromConfig
        procedure,non_overridable,public,pass::AdjustTimeStep_ImplantBatchFromConfig

        procedure,non_overridable,public,pass::CopyImplantSectionFromOther
        procedure,non_overridable,public,pass::Clean_ImplantSection
        Generic::ASSIGNMENT(=)=>CopyImplantSectionFromOther
        Final::CleanImplantSection
    end type ImplantSection

    private::LoadOne_ImplantSection
    private::ReadImplantCommonCtl
    private::ReadRateCtrl
    private::ReadImplantSource
    private::ReadImplantSource_Simple
    private::ReadImplantClusterSizeDist_Simple
    private::ReadImplantClusterDepthDist_Simple
    private::ReadImplantSource_SpecialDistFromFile
    private::Putin_PANDA_OUTCFG_Distribution
    private::Putin_SRIM2003_OUTCFG_Distribution
    private::ReadImplantSection_SpecialDistFromExteFunc
    private::Implant
    private::ImplantClusters_Contiune
    private::AdjustTimeStep_ImplantContiune
    private::Cal_ExpandSize_ImplantContiune
    private::DoImplantTillVirtualBoundary_CPUTOGPU_ImplantContiune
    private::DoImplantTillVirtualBoundary_CPU_ImplantContiune
    private::FillVirtualBoundary_CPU_Depth_LAY_ImplantContiune
    private::FillVirtualBoundary_CPU_Depth_SubBox_ImplantContiune
    private::FillVirtualBoundary_CPU_Depth_Gauss_ImplantContiune
    private::FillVirtualBoundary_CPU_Simple_ImplantContiune
    private::FillVirtualBoundary_CPU_FromFile_ImplantContiune
    private::FillVirtualBoundary_CPU_FromExteFunc_ImplantContiune
    private::ImplantClusters_BatchFromConfig
    private::AdjustTimeStep_ImplantBatchFromConfig
    private::CopyImplantSectionFromOther
    private::Clean_ImplantSection
    private::CleanImplantSection
    contains

    !********************************************************************
    subroutine CopyImplantSectionFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection),intent(out)::this
        CLASS(ImplantSection),intent(in)::other
        !---Local Vars---
        integer::I
        !---Body---

        !---CommonCtl-----
        this%ImplantConfigType =other%ImplantConfigType

        this%ExpandFactor = other%ExpandFactor

        this%MemoryOccupyFactor = other%MemoryOccupyFactor

        this%InsertTimeInterval = other%InsertTimeInterval

        this%NInsertTimePoint = other%NInsertTimePoint
        if(allocated(other%InsertTimePoint)) then
            if(size(other%InsertTimePoint) .GT. 0) then
                call AllocateArray_Host(this%InsertTimePoint,size(other%InsertTimePoint),"this%InsertTimePoint")
            end if
        end if

        this%InsertCountModel = other%InsertCountModel

        this%InsertCountOneBatch = other%InsertCountOneBatch

        this%ConfigFolder = other%ConfigFolder

        this%InsetSequence = other%InsetSequence

        !---The Assignment(=)=> had been override
        this%ImplantCfgFileList = other%ImplantCfgFileList

        this%ImplantCfgFileType = other%ImplantCfgFileType

        DO I = 1,p_ATOMS_GROUPS_NUMBER
            this%Elemets(I) = other%Elemets(I)
        END DO

        this%NAINI = other%NAINI

        this%NASDINI = other%NASDINI

        this%NACUT = other%NACUT

        this%CompositWeight = other%CompositWeight

        this%ImplantDepthDistType = other%ImplantDepthDistType

        this%SUBBOXBOUNDARY = other%SUBBOXBOUNDARY

        this%DepthINI = other%DepthINI

        this%DepthSDINI = other%DepthSDINI

        call DeAllocateArray_Host(this%LayerThick,"LayerThick")
        call AllocateArray_Host(this%LayerThick,size(other%LayerThick),"LayerThick")
        this%LayerThick = other%LayerThick

        !---The assignment(=) had been override
        call DeAllocateArray_Host(this%ClustersSample,"ClustersSample")
        call AllocateArray_Host(this%ClustersSample,size(other%ClustersSample,dim=1),size(other%ClustersSample,dim=2),"ClustersSample")
        this%ClustersSample = other%ClustersSample

        call DeAllocateArray_Host(this%ClustersSampleRate,"ClustersSampleRate")
        call AllocateArray_Host(this%ClustersSampleRate,size(other%ClustersSampleRate,dim=1),size(other%ClustersSampleRate,dim=2),"ClustersSampleRate")
        this%ClustersSampleRate = other%ClustersSampleRate

        return
    end subroutine CopyImplantSectionFromOther

    !*********************************************************************
    subroutine Clean_ImplantSection(this)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        !---Body---

        this%ImplantConfigType = -1

        this%ExpandFactor = 1

        this%MemoryOccupyFactor = 100

        this%InsertTimeInterval = 0.D0

        this%NInsertTimePoint = 0
        call DeAllocateArray_Host(this%InsertTimePoint,"this%InsertTimePoint")

        this%InsertCountModel = p_InsertCountModel_ByConfigNum

        this%InsertCountOneBatch = 0

        this%ConfigFolder = ""

        this%InsetSequence = p_InsertConfig_ByAlphaBeta

        call this%ImplantCfgFileList%Clean_STRList()
        Nullify(this%ImplantCfgFileList)
        this%ImplantCfgFileList=>null()

        this%ImplantCfgFileType = ""

        this%Elemets = ""

        this%NAINI = 0.D0

        this%NASDINI = 0.D0

        this%NACUT = 0.D0

        this%CompositWeight = 0.D0

        this%ImplantDepthDistType = -1

        this%SUBBOXBOUNDARY = 0.D0

        this%DepthINI = 0.D0

        this%DepthSDINI = 0.D0

        call DeAllocateArray_Host(this%LayerThick,"LayerThick")

        call DeAllocateArray_Host(this%ClustersSample,"ClustersSample")

        call DeAllocateArray_Host(this%ClustersSampleRate,"ClustersSampleRate")

        return
    end subroutine Clean_ImplantSection


    !**************************************************************
    subroutine CleanImplantSection(this)
        !---Dummy Vars---
        type(ImplantSection)::this
        !---Body---
        call this%Clean_ImplantSection()

        return
    end subroutine

    !**************************************************************
    subroutine LoadOne_ImplantSection(this,hFile,SimBoxes,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer, intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Local Vars---
        character*256::STR
        character*32::KEYWORD
        character*20::STRTMP(10)
        integer::N
        !---Body---
        Do While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit

                case("&IMPCOMMONSUBCTL")
                    call this%ReadImplantCommonCtl(hFile,LINE)
                case("&RATESUBCTL")
                    call this%ReadRateCtrl(hFile,Host_SimuCtrlParam,LINE)

                case("&SIZESUBCTL","&DEPTHSUBCTL","&EXTFSUBCTL")
                    call this%ReadImplantSource(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)

                case default
                    write(*,*) "MCPSCUERROR: Unknown Flag: ",KEYWORD
                    write(*,*) "At LINE: ",LINE
                    pause
                    stop
            end select
        End Do

        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine


    !*****************************************************************
    subroutine ReadRateCtrl(this,hFile,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer, intent(in)::hFile
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Local Vars---
        character*256::STR
        character*32::KEYWORD
        character*20::STRTMP(20)
        integer::N
        integer::I
        !---Body---
        DO while(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit

                case("&TIMEINTERVAL")
                    call EXTRACT_SUBNUM(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the insert time interval"
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way: &TIMEINTERVAL The time interval between batches ="
                        pause
                        stop
                    end if
                    this%InsertTimeInterval = DRSTR(STRTMP(1))

                case("&INSERTTIMEPOINT")
                    call EXTRACT_SUBNUM(STR,p_MAX_BATCHINSERTTIMEPOINTS,N,STRTMP)

                    if(N .GT. 0) then
                        call AllocateOneDimd_Host(this%InsertTimePoint,N,"InsertTimePoint")
                    else
                        write(*,*) "MCPSCUERROR: Too few parameters are specialized for &INSERTTIMEPOINT"
                        write(*,*) "At LINE: ",LINE
                        write(*,*) "You should use &INSERTTIMEPOINT The batch insert time point ="
                        pause
                        stop
                    end if

                    this%NInsertTimePoint = N

                    DO I = 1,N
                        this%InsertTimePoint(I) = DRSTR(STRTMP(I))

                        if(I .GT. 1) then
                            if(this%InsertTimePoint(I) .LE. this%InsertTimePoint(I-1)) then
                                write(*,*) "MCPSCU ERROR: You should align the batch insert time-points from smaller to bigger"
                                write(*,*) "At control file line: ",LINE
                                write(*,*) STR
                                pause
                                stop
                            end if
                        end if

                        if(this%InsertTimePoint(I) .GT. Host_SimuCtrlParam%TermTValue) then
                            write(*,*) "MCPSCU ERROR: the insert time-point should less than terminate time "
                            write(*,*) "Chosen insert time point is: ", this%InsertTimePoint(I)
                            write(*,*) "The terminate time point is: ",Host_SimuCtrlParam%TermTValue
                            pause
                            stop
                        end if

                    END DO

                case("&INSERTCOUNTMODEL")
                    call EXTRACT_SUBNUM(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the insert count model"
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way: The count model for each batch ="
                        pause
                        stop
                    end if
                    this%InsertCountModel = ISTR(STRTMP(1))

                case("&INSERTCOUNT")
                    call EXTRACT_SUBNUM(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the insert count for each batch"
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way: &INSERTCOUNT  The count in each batch = "
                        pause
                        stop
                    end if
                    this%InsertCountOneBatch = ISTR(STRTMP(1))

                case default
                    write(*,*) "MCPSCUERROR: Unknown Flag: ",KEYWORD
                    write(*,*) "At LINE: ",LINE
                    pause
                    stop
            end select

        END DO


        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine

    !***************************************************************
    subroutine ReadImplantCommonCtl(this,hFile,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        integer::LINE
        !---Local Vars---
        character*256::STR
        character*32::KEYWORD
        character*20::STRTMP(10)
        integer::N
        !---Body---
        DO while(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)

            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit

                case("&SOURCETYPE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for implantation distribution type."
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way : &SOURCETYPE The implantation cluster distribution type =  "
                        pause
                        stop
                    end if
                    this%ImplantConfigType = ISTR(STRTMP(1))

                case("&FEXPAND")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the implantation expand factor ."
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way: &FEXPAND The expand size factor = "
                        pause
                        stop
                    end if
                    this%ExpandFactor = ISTR(STRTMP(1))

                case("&FMEMOCCUP")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the memory occupy factor."
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way: &FMEMOCCUP TThe memory occupied factor ="
                        pause
                        stop
                    end if
                    this%MemoryOccupyFactor = ISTR(STRTMP(1))

                    if(this%MemoryOccupyFactor .LE. 1) then
                        write(*,*) "MCPSCUERROR: The MemoryOccupyFactor cannot less than 1"
                        pause
                        stop
                    end if

                case default
                    write(*,*) "MCPSCUERROR: Unknown keyword: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
            end select

        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At Line: ",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine

    !***************************************************************
    subroutine ReadImplantSource(this,hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        character*(*)::KEYWORD
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !--Body---
        select case(this%ImplantConfigType)
            case(p_ImplantConfig_Simple)
                call this%ReadImplantSource_Simple(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
            case(p_ImplantConfig_SpecialDistFromFile)
                call this%ReadImplantSource_SpecialDistFromFile(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
            case(p_ImplantConfig_SpecialDistFromExteFunc)
                call this%ReadImplantSection_SpecialDistFromExteFunc(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
            case default
                write(*,*) "MCPSCUERROR: Unknown strategy for the implantation configuration:",this%ImplantConfigType
                pause
                stop
        end select

        return
    end subroutine ReadImplantSource

    !*****************************************************************
    subroutine ReadImplantSource_Simple(this,hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        character*(*)::KEYWORD
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Body---

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                return
            case("&SIZESUBCTL")
                    call this%ReadImplantClusterSizeDist_Simple(hFile,SimBoxes,LINE)
            case("&DEPTHSUBCTL")
                    call this%ReadImplantClusterDepthDist_Simple(hFile,SimBoxes,LINE)
            case default
                write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD
                pause
                stop
        end select

        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadImplantSource_Simple

    !****************************************************************
    subroutine ReadImplantSource_SpecialDistFromFile(this,hFile,PreKEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        character*(*)::PreKEYWORD
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Local Vars---
        character*256::STR
        character*32::KEYWORD
        character*256::STRTEMP(10)
        integer::N
        type(MigCoalClusterRecord)::tempRecord
        real(kind=KINDDF)::TotalSampleRate
        character*256::ConfigPath
        integer::LayerNum
        type(STRList),pointer::cursor=>null()
        !---Body---

        if(.not. ISSTREQUAL(PreKEYWORD,"&EXTFSUBCTL")) then
            write(*,*) "MCPSCUERROR: You must special the &EXTFSUBCTL when the implant strategy is chosen by outer file ."
            write(*,*) "However, you had special the key word :",KEYWORD
            write(*,*) "At line: ",LINE
            pause
            stop
        end if

        DO While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&DISTFILETYPE")
                    call EXTRACT_SUBSTR(STR,1,N,STRTEMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the implantation configuration file type if you had chosen the file model."
                        write(*,*) "By the way: &DISTFILETYPE The distribution file type = ! 'DISTOKMC18','BOXMF18','BOXSPMF18','DISTSRIM','DISTPANDA' "
                        pause
                        stop
                    end if

                    if(LENTRIM(STRTEMP(1)) .LE. 0) then
                        write(*,*) "MCPSCUERROR: The implant configuration file type is null."
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if

                    call UPCASE(STRTEMP(1))

                    this%ImplantCfgFileType = adjustl(trim(KEYWORD_HEAD))//adjustl(trim(STRTEMP(1)))

                    if(ISSTREQUAL(trim(this%ImplantCfgFileType),SRIM_DIST) .or.  ISSTREQUAL(trim(this%ImplantCfgFileType),PANDA_DIST)) then
                        call EXTRACT_SUBSTR(STR,2,N,STRTEMP)

                        if(N .LT. 2) then
                            write(*,*) "MCPSCUERROR: when the specialized distribution type is 'DISTSRIM' or 'DISTPANDA'"
                            write(*,*) "MCPSCUERROR: you must special the implantation ion type."
                            pause
                            stop
                        end if

                        ! For both of PANDA and SRIM, only one kind of ion are injected to matrix
                        call UPCASE(STRTEMP(2))
                        this%Elemets(1) = adjustl(trim((STRTEMP(2))))
                    end if

                    if(ISSTREQUAL(trim(this%ImplantCfgFileType),SRIM_DIST)) then
                        call EXTRACT_NUMB(STR,1,N,STRTEMP)

                        if(N .LT. 1) then
                            write(*,*) "MCPSCUERROR: when the specialized distribution type is 'DISTSRIM'"
                            write(*,*) "MCPSCUERROR: you must special the layer number that you want to divide."
                            pause
                            stop
                        end if

                        LayerNum = ISTR(STRTEMP(1))

                        if(LayerNum .LE. 0) then
                            write(*,*) "MCPSCUERROR: the total layer number cannot be less than 0 when it is set for SRIM distribution"
                            pause
                            stop
                        end if
                    end if

                case("&FOLDER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTEMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for  batch input configuration folder setting"
                        write(*,*) "At Line: ",LINE
                        write(*,*) STR
                        write(*,*) "You should special &FOLDER The candiacate batches configurations folder = "
                        pause
                        stop
                    end if

                    ConfigPath = STRTEMP(1)
                    ConfigPath = adjustl(ConfigPath)
                    if(IsAbsolutePath(ConfigPath)) then
                        this%ConfigFolder = adjustl(trim(ConfigPath))
                    else
                        if(LENTRIM(adjustl(Host_SimuCtrlParam%InputFilePath)) .GT. 0) then
                            this%ConfigFolder = adjustl(trim(Host_SimuCtrlParam%InputFilePath))//FolderSpe//adjustl(trim(ConfigPath))
                        else
                            this%ConfigFolder = adjustl(trim(ConfigPath))
                        end if
                    endif

                    if(.not. associated(this%ImplantCfgFileList)) then
                        allocate(this%ImplantCfgFileList)
                    end if
                    call ListFilesInFolder(this%ConfigFolder,this%ImplantCfgFileList)

                    if(this%ImplantCfgFileList%GetSTRList_Count() .LE. 0) then
                        write(*,*) "MCPSCUERROR: There are not any configuration within folder: ",this%ConfigFolder
                        pause
                        stop
                    end if

                case("&SELECTSEQUENCE")
                    call EXTRACT_NUMB(STR,1,N,STRTEMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for  batch input configuration read sequence"
                        write(*,*) "At Line: ",LINE
                        write(*,*) STR
                        write(*,*) "You should special &SELECTSEQUENCE select sequence is by = "
                        pause
                        stop
                    end if

                    this%InsetSequence = ISTR(STRTEMP(1))
                case default
                    write(*,*) "MCPSCUERROR: Illegal flag: ",KEYWORD
                    pause
                    stop
            end select
        END DO

        cursor=>this%ImplantCfgFileList
        if(associated(cursor)) then
            select case(adjustl(trim(this%ImplantCfgFileType)))
                case(MF_OUTCFG_FORMAT18)
                    call SimBoxes%Putin_MF_OUTCFG_FORMAT18_Distribution(Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),this%LayerThick,this%ClustersSampleRate,this%ClustersSample,tempRecord,m_FREESURDIFPRE)

                    TotalSampleRate = sum(this%ClustersSampleRate)
                    if(TotalSampleRate .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
                        write(*,*) "In file: ",cursor%TheValue
                        pause
                        stop
                    end if
                    this%ClustersSampleRate = this%ClustersSampleRate/TotalSampleRate

                    if(this%ImplantCfgFileList%GetSTRList_Count() .GT. 1) then
                        write(*,*) "MCPSCUERROR: Currently, mcpscu only support single MF file"
                        write(*,*) "However, the number of MF file in folder is : ",this%ImplantCfgFileList%GetSTRList_Count()
                        pause
                        stop
                    end if

                    if(this%InsertCountModel .eq. p_InsertCountModel_ByConfigNum) then
                        write(*,*) "MCPSCUERROR: the configuration files you specialed is MF file"
                        write(*,*) "It is not suit for insert configuration by configuration"
                        pause
                        stop
                    end if


                case(SPMF_OUTCFG_FORMAT18)
                    call SimBoxes%Putin_SPMF_OUTCFG_FORMAT18_Distribution(Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),this%LayerThick,this%ClustersSampleRate,this%ClustersSample,tempRecord,m_FREESURDIFPRE,m_GBSURDIFPRE)
                    TotalSampleRate = sum(this%ClustersSampleRate)
                    if(TotalSampleRate .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
                        write(*,*) "In file: ",cursor%TheValue
                        pause
                        stop
                    end if
                    this%ClustersSampleRate = this%ClustersSampleRate/TotalSampleRate

                    if(this%ImplantCfgFileList%GetSTRList_Count() .GT. 1) then
                        write(*,*) "MCPSCUERROR: Currently, mcpscu only support single SPMF file"
                        write(*,*) "However, the number of SPMF file in folder is : ",this%ImplantCfgFileList%GetSTRList_Count()
                        pause
                        stop
                    end if

                    if(this%InsertCountModel .eq. p_InsertCountModel_ByConfigNum) then
                        write(*,*) "MCPSCUERROR: the configuration files you specialed is SPMF file"
                        write(*,*) "It is not suit for insert configuration by configuration"
                        pause
                        stop
                    end if

                case(SRIM_DIST)
                    call this%Putin_SRIM2003_OUTCFG_Distribution(SimBoxes,Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),LayerNum,this%LayerThick,this%ClustersSampleRate,this%ClustersSample)
                    TotalSampleRate = sum(this%ClustersSampleRate)
                    if(TotalSampleRate .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
                        write(*,*) "In file: ",cursor%TheValue
                        pause
                        stop
                    end if
                    this%ClustersSampleRate = this%ClustersSampleRate/TotalSampleRate

                    if(this%ImplantCfgFileList%GetSTRList_Count() .GT. 1) then
                        write(*,*) "MCPSCUERROR: Currently, mcpscu only support single SRIM DIST file"
                        write(*,*) "However, the number of SRIM DIST  file in folder is : ",this%ImplantCfgFileList%GetSTRList_Count()
                        pause
                        stop
                    end if

                    if(this%InsertCountModel .eq. p_InsertCountModel_ByConfigNum) then
                        write(*,*) "MCPSCUERROR: the configuration files you specialed is SRIM Dist file"
                        write(*,*) "It is not suit for insert configuration by configuration"
                        pause
                        stop
                    end if

                case(PANDA_DIST)
                    call this%Putin_PANDA_OUTCFG_Distribution(SimBoxes,Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),this%LayerThick,this%ClustersSampleRate,this%ClustersSample)
                    TotalSampleRate = sum(this%ClustersSampleRate)
                    if(TotalSampleRate .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
                        write(*,*) "In file: ",cursor%TheValue
                        pause
                        stop
                    end if
                    this%ClustersSampleRate = this%ClustersSampleRate/TotalSampleRate

                    if(this%ImplantCfgFileList%GetSTRList_Count() .GT. 1) then
                        write(*,*) "MCPSCUERROR: Currently, mcpscu only support single PANDA DIST file"
                        write(*,*) "However, the number of PANDA DIST  file in folder is : ",this%ImplantCfgFileList%GetSTRList_Count()
                        pause
                        stop
                    end if

                    if(this%InsertCountModel .eq. p_InsertCountModel_ByConfigNum) then
                        write(*,*) "MCPSCUERROR: the configuration files you specialed is PANDA Dist file"
                        write(*,*) "It is not suit for insert configuration by configuration"
                        pause
                        stop
                    end if

                case(BATCHCONFIG)
                    if(this%InsertCountModel .eq. p_InsertCountModel_ByClusterNum) then
                        write(*,*) "MCPSCUERROR: the configuration files you specialed is batch Config file"
                        write(*,*) "It is not suit for insert contiune"
                        pause
                        stop
                    end if

                case default
                    write(*,*) "MCPSCUERROR: Unknown Implant Configuration file type : ",this%ImplantCfgFileType
                    write(*,*) "In current version, only the ", &
                                MF_OUTCFG_FORMAT18," ",         &
                                SPMF_OUTCFG_FORMAT18," ",       &
                                SRIM_DIST," ",                  &
                                PANDA_DIST," ",                 &
                                BATCHCONFIG," ",                &
                                "are supported. "
                    pause
                    stop
            end select

        end if

        return
        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadImplantSource_SpecialDistFromFile

    !*****************************************************************
    subroutine Putin_PANDA_OUTCFG_Distribution(this,SimBoxes,Host_SimuCtrlParam,cfgFile,LayersThick,ClustersSampleConcentrate,ClustersSample)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes),intent(in)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        character*256,intent(in)::cfgFile
        real(kind=KINDDF),dimension(:),allocatable::LayersThick
        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleConcentrate
        type(ACluster),dimension(:,:),allocatable::ClustersSample
        !---Local Vars---
        integer::hFile
        integer::LINE
        integer::N
        character*256::STR
        character*32::KEYWORD
        character*32::STRTMP(20)
        integer::IElement
        integer::LayerNum
        integer::ILayer
        integer::Layer
        real(kind=KINDDF)::SumOfThick
        type(ACluster)::ImplantIon
        type(DiffusorValue)::TheDiffusorValue
        !---Body---

        LINE = 0

        hFile = OpenExistedFile(adjustl(trim(cfgFile)))

        LayerNum = 0

        LINE = 0

        DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")

            if(LENTRIM(adjustl(STR)) .LE. 0) then
                cycle
            end if

            LINE = LINE + 1

            LayerNum = LayerNum + 1

        END DO

        if(LayerNum .LE. 0) then
            write(*,*) "MCPSCUERROR: The layers number in panda distribution is less than 1, that is impossible."
            pause
            stop
        end if

        call AllocateArray_Host(LayersThick,LayerNum,"LayersThick")
        call AllocateArray_Host(ClustersSampleConcentrate,LayerNum,1,"ClustersSampleConcentrate")
        call AllocateArray_Host(ClustersSample,LayerNum,1,"ClustersSample")

        call ImplantIon%Clean_Cluster()

        !---For SRIM2003 and PANDA, only one kind of ions would be implanted
        DO IElement = 1,p_ATOMS_GROUPS_NUMBER
            ImplantIon%m_Atoms(IElement)%m_ID = IElement
            ImplantIon%m_Atoms(IElement)%m_NA = 0
        END DO
        IElement = SimBoxes%Atoms_list%FindIndexBySymbol(this%Elemets(1))
        ImplantIon%m_Atoms(IElement)%m_ID = IElement
        ImplantIon%m_Atoms(IElement)%m_NA = 1

        TheDiffusorValue = SimBoxes%m_DiffusorTypesMap%Get(ImplantIon)

        !---In PANDA, the matrix is amorphous---
        select case(TheDiffusorValue%ECRValueType_Free)
            case(p_ECR_ByValue)
                ImplantIon%m_RAD = TheDiffusorValue%ECR_Free
            case default
                ImplantIon%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,    &
                                                         ImplantIon%m_Atoms(:)%m_NA,            &
                                                         Host_SimuCtrlParam%TKB,                &
                                                         SimBoxes%LatticeLength)
        end select

        select case(TheDiffusorValue%DiffusorValueType_Free)
            case(p_DiffuseCoefficient_ByValue)
                ImplantIon%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
            case(p_DiffuseCoefficient_ByArrhenius)
                ImplantIon%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                ImplantIon%m_DiffCoeff = m_FREESURDIFPRE*(ImplantIon%m_RAD**(-p_GAMMA))
            case(p_DiffuseCoefficient_BySIACluster)
                ImplantIon%m_DiffCoeff = (sum(ImplantIon%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByVcCluster)
                ImplantIon%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(ImplantIon%m_Atoms(:)%m_NA)))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
        end select

        ImplantIon%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

        ImplantIon%m_Statu = p_ACTIVEFREE_STATU

        DO Layer = 1,LayerNum
            ClustersSample(Layer,1) = ImplantIon
        END DO


        ReWind(hFile)

        ILayer = 1

        SumOfThick = 0.D0

        LINE = 0

        DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")

            if(LENTRIM(adjustl(STR)) .LE. 0) then
                cycle
            end if

            LINE = LINE + 1

            call EXTRACT_NUMB(STR,2,N,STRTMP)

            if(N .LT. 2) then
                write(*,*) "MCPSCUERROR: The panda distribution file data cannot be recognized in line: ",LINE
                write(*,*) STR
                write(*,*) "At file: ",cfgFile
                pause
                stop
            end if

            LayersThick(ILayer) = 2*(DRSTR(STRTMP(1))*C_UM2CM - SumOfThick)
            SumOfThick = SumOfThick + LayersThick(ILayer)

            if(SumOfThick .GT. SimBoxes%BOXSIZE(3)) then
                write(*,*) "MCPSCUERROR: The PANDA depth distribution is greater than simulation box depth."
                write(*,*) "The PANDA depth is: ",SumOfThick
                write(*,*) "The simulation box depth is : ",SimBoxes%BOXSIZE(3)
                pause
                stop
            end if

            ClustersSampleConcentrate(ILayer,1) = DRSTR(STRTMP(2))

            ClustersSample(ILayer,1)%m_Statu = p_ACTIVEFREE_STATU

            ClustersSample(ILayer,1)%m_Layer = ILayer

            ILayer =  ILayer + 1

        END DO

        close(hFile)

        return
    end subroutine Putin_PANDA_OUTCFG_Distribution

    !******************************************************************
    subroutine Putin_SRIM2003_OUTCFG_Distribution(this,SimBoxes,Host_SimuCtrlParam,cfgFile,LayerNum,LayersThick,ClustersSampleConcentrate,ClustersSample)
                !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes),intent(in)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        character*256,intent(in)::cfgFile
        integer,intent(in)::LayerNum
        real(kind=KINDDF),dimension(:),allocatable::LayersThick
        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleConcentrate
        type(ACluster),dimension(:,:),allocatable::ClustersSample
        !---Local Vars---
        integer::hFile
        integer::LINE
        integer::N
        character*256::STR
        character*32::KEYWORD
        character*32::STRTMP(20)
        integer::IElement
        integer::StoppedNum
        integer::IIon
        integer::ILayer
        real(kind=KINDDF)::SumOfThick
        type(ACluster)::ImplantIon
        type(DiffusorValue)::TheDiffusorValue
        real(kind=KINDDF),dimension(:,:),allocatable::StoppedPosition
        real(kind=KINDDF)::Thickness
        !---Body---
        LINE = 0

        hFile = OpenExistedFile(adjustl(trim(cfgFile)))

        StoppedNum = 0

        DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")

            if(LENTRIM(adjustl(STR)) .LE. 0) then
                cycle
            end if

            LINE = LINE + 1

            if((iachar(STR(1:1)) .GE. iachar('0') .AND. iachar(STR(1:1)) .LE. iachar('9'))) then
                StoppedNum = StoppedNum + 1
            end if

        END DO

        if(StoppedNum .LE. 0) then
            write(*,*) "MCPSCUERROR: The bins number in SRIM2003 distribution is less than 1, that is impossible."
            pause
            stop
        end if

        call AllocateArray_Host(StoppedPosition,StoppedNum,3,"StoppedPosition")

        ReWind(hFile)

        LINE = 0

        IIon = 1

        DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")

            if(LENTRIM(adjustl(STR)) .LE. 0) then
                cycle
            end if

            LINE = LINE + 1

            if((iachar(STR(1:1)) .GE. iachar('0') .AND. iachar(STR(1:1)) .LE. iachar('9'))) then

                call EXTRACT_NUMB(STR,4,N,STRTMP)

                if(N .LT. 4) then
                    write(*,*) "MCPSCUERROR: The SRIM2003 distribution file data cannot be recognized in line: ",LINE
                    write(*,*) STR
                    write(*,*) "At file: ",cfgFile
                    pause
                    stop
                end if

                StoppedPosition(IIon,1) = DRSTR(STRTMP(2))*C_AM2CM ! depth   X
                StoppedPosition(IIon,2) = DRSTR(STRTMP(3))*C_AM2CM ! lateral Y
                StoppedPosition(IIon,3) = DRSTR(STRTMP(4))*C_AM2CM ! lateral Z

                if(StoppedPosition(IIon,2) .LT. SimBoxes%BOXBOUNDARY(1,1) .or. StoppedPosition(IIon,2) .GT. SimBoxes%BOXBOUNDARY(1,2)) then
                    write(*,*) "MCPSCUERROR: The SRIM2003 distribution is out of the simulation box in lateral X."
                    write(*,*) STR
                    write(*,*) "Current position in lateral X is (cm) : ",StoppedPosition(IIon,2)
                    write(*,*) "However, the box boundary range from ",SimBoxes%BOXBOUNDARY(1,1)," To ",SimBoxes%BOXBOUNDARY(1,2)
                    pause
                    stop
                end if

                if(StoppedPosition(IIon,3) .LT. SimBoxes%BOXBOUNDARY(2,1) .or. StoppedPosition(IIon,3) .GT. SimBoxes%BOXBOUNDARY(2,2)) then
                    write(*,*) "MCPSCUERROR: The SRIM2003 distribution is out of the simulation box in lateral Y."
                    write(*,*) STR
                    write(*,*) "Current position in lateral Y is (cm) : ",StoppedPosition(IIon,3)
                    write(*,*) "However, the box boundary range from ",SimBoxes%BOXBOUNDARY(2,1)," To ",SimBoxes%BOXBOUNDARY(2,2)
                    pause
                    stop
                end if

                if(StoppedPosition(IIon,1) .LT. SimBoxes%BOXBOUNDARY(3,1) .or. StoppedPosition(IIon,1) .GT. SimBoxes%BOXBOUNDARY(3,2)) then
                    write(*,*) "MCPSCUERROR: The SRIM2003 distribution is out of the simulation box in depth."
                    write(*,*) STR
                    write(*,*) "Current depth is (cm) : ",StoppedPosition(IIon,1)
                    write(*,*) "However, the box boundary range from ",SimBoxes%BOXBOUNDARY(3,1)," To ",SimBoxes%BOXBOUNDARY(3,2)
                    pause
                    stop
                end if

                IIon = IIon + 1
            end if

        END DO

        call AllocateArray_Host(LayersThick,LayerNum,"LayersThick")
        call AllocateArray_Host(ClustersSampleConcentrate,LayerNum,1,"ClustersSampleConcentrate")
        call AllocateArray_Host(ClustersSample,LayerNum,1,"ClustersSample")

        Thickness = (maxval(StoppedPosition(:,1)) - minval(StoppedPosition(:,1)))/LayerNum

        LayersThick = Thickness

        ClustersSampleConcentrate = 0

        DO IIon = 1,StoppedNum
            ILayer = max(floor(StoppedPosition(IIon,1)/Thickness),1)
            ClustersSampleConcentrate(ILayer,1) = ClustersSampleConcentrate(ILayer,1) + 1
        END DO

        call ImplantIon%Clean_Cluster()

        !---For SRIM2003 and PANDA, only one kind of ions would be implanted
        DO IElement = 1,p_ATOMS_GROUPS_NUMBER
            ImplantIon%m_Atoms(IElement)%m_ID = IElement
            ImplantIon%m_Atoms(IElement)%m_NA = 0
        END DO
        IElement = SimBoxes%Atoms_list%FindIndexBySymbol(this%Elemets(1))
        ImplantIon%m_Atoms(IElement)%m_ID = IElement
        ImplantIon%m_Atoms(IElement)%m_NA = 1

        TheDiffusorValue = SimBoxes%m_DiffusorTypesMap%Get(ImplantIon)

        !---In SRIM2003, the matrix is amorphous---
        select case(TheDiffusorValue%ECRValueType_Free)
            case(p_ECR_ByValue)
                ImplantIon%m_RAD = TheDiffusorValue%ECR_Free
            case default
                ImplantIon%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,    &
                                                         ImplantIon%m_Atoms(:)%m_NA,            &
                                                         Host_SimuCtrlParam%TKB,                &
                                                         SimBoxes%LatticeLength)
        end select

        select case(TheDiffusorValue%DiffusorValueType_Free)
            case(p_DiffuseCoefficient_ByValue)
                ImplantIon%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
            case(p_DiffuseCoefficient_ByArrhenius)
                ImplantIon%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                ImplantIon%m_DiffCoeff = m_FREESURDIFPRE*(ImplantIon%m_RAD**(-p_GAMMA))
            case(p_DiffuseCoefficient_BySIACluster)
                ImplantIon%m_DiffCoeff = (sum(ImplantIon%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByVcCluster)
                ImplantIon%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(ImplantIon%m_Atoms(:)%m_NA)))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
        end select

        ImplantIon%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

        ImplantIon%m_Statu = p_ACTIVEFREE_STATU

        ClustersSample(:,:) = ImplantIon

        call DeAllocateArray_Host(StoppedPosition,"StoppedPosition")

        close(hFile)

        return
    end subroutine Putin_SRIM2003_OUTCFG_Distribution

    !******************************************************************
    subroutine Putin_OKMC_FORMAT18_Distribution(this,SimBoxes,Host_SimuCtrlParam,cfgFile,LayerNum,LayersThick,ClustersSampleConcentrate,ClustersSample)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        character*256,intent(in)::cfgFile
        integer,intent(in)::LayerNum
        real(kind=KINDDF),dimension(:),allocatable::LayersThick
        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleConcentrate
        type(ACluster),dimension(:,:),allocatable::ClustersSample
        !---Body---

    end subroutine Putin_OKMC_FORMAT18_Distribution

    !*****************************************************************
    subroutine ReadImplantSection_SpecialDistFromExteFunc(this,hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        character*(*)::KEYWORD
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE

        ! @todo (zhail#1#):


        return
    end subroutine ReadImplantSection_SpecialDistFromExteFunc


    !***************************************************************
    subroutine ReadImplantClusterSizeDist_Simple(this,hFile,SimBoxes,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        integer::LINE
        !---Local Vars---
        character*512::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::N
        integer::NElements
        integer::I
        integer::TheIndex
        real(kind=KINDDF)::TotalSampleRate
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
                case("&NATOMDIST")
                    call EXTRACT_NUMB(STR,4,N,STRTMP)
                    if(N .LT. 4) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the cluster size distribution"
                        write(*,*) "You should special: &NATOMDIST The atoms number in each cluster distribution as Gauss that central = , distribution half width = , left cut = ,right cut = "
                        pause
                        stop
                    end if
                    this%NAINI = DRSTR(STRTMP(1))
                    this%NASDINI  = DRSTR(STRTMP(2))
                    this%NACUT(1) = DRSTR(STRTMP(3))
                    this%NACUT(2) = DRSTR(STRTMP(4))
                    if(this%NACUT(1) .GE. this%NACUT(2)) then
                        write(*,*) "MCPSCUERROR: The right cut cannot less than left cut."
                        write(*,*) "LCut",this%NACUT(1)
                        write(*,*) "RCut",this%NACUT(2)
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
                            this%Elemets(I) = adjustl(trim(STRTMP(I)))
                            call UPCASE(this%Elemets(I))
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
                        this%CompositWeight = 0.D0

                        DO I = 1,N
                            TheIndex = SimBoxes%Atoms_list%FindIndexBySymbol(this%Elemets(I))
                            this%CompositWeight(TheIndex) = DRSTR(STRTMP(I))
                        END DO

                        if(sum(this%CompositWeight) .LE. 0.D0) then
                            write(*,*) "MCPSCUERROR: The sum of elements weights must great than 0 ."
                            write(*,*) STR
                            write(*,*) "At Line :",LINE
                            pause
                            stop
                        end if

                        this%CompositWeight = this%CompositWeight/sum(this%CompositWeight)
                    end if
                CASE default
                    write(*,*) "MCPSCUERROR: Illegal Symbol: ", KEYWORD
                    pause
                    stop
            END SELECT

        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed for cluster size!"
            write(*,*) "At line :",LINE
            write(*,*) STR
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadImplantClusterSizeDist_Simple

    !***************************************************************
    subroutine ReadImplantClusterDepthDist_Simple(this,hFile,Host_SimBoxes,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        type(SimulationBoxes)::Host_SimBoxes
        integer::LINE
        !---Local Vars---
        character*256::STR
        character*32::KEYWORD
        character*32::STRTMP(10)
        integer::N
        integer::I
        integer::LayerNum
        real(kind=KINDDF)::SumOfLayer
        real(kind=KINDDF)::TotalSampleRate
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
                    this%ImplantDepthDistType = p_DEPT_DIS_Layer

                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the clusters depth distribution layer type"
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

                    call AllocateArray_Host(this%LayerThick,LayerNum,"LayerThick")
                    this%LayerThick = 0.D0
                    call AllocateArray_Host(this%ClustersSampleRate,LayerNum,1,"ClustersSampleRate")
                    this%ClustersSampleRate = 0.D0

                    DO I = 1,LayerNum
                        this%LayerThick(I) = DRSTR(STRTMP(I+1))
                    END DO

                    if(sum(this%LayerThick) .LE. 0) then
                        this%LayerThick(1) = 1.D0
                    end if

                    SumOfLayer = sum(this%LayerThick)

                    DO I = 1,LayerNum
                        this%LayerThick(I) = Host_SimBoxes%BOXSIZE(3)*this%LayerThick(I)/SumOfLayer
                    END DO

                    DO I = 1,LayerNum
                        this%ClustersSampleRate(I,1) = DRSTR(STRTMP(I + LayerNum + 1))
                    END DO

                    ! Note, the out put SampleRate may be the concentrate, we need to convert it to rate now.
                    TotalSampleRate = sum(this%ClustersSampleRate)
                    if(TotalSampleRate .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
                        pause
                        stop
                    end if
                    this%ClustersSampleRate = this%ClustersSampleRate/TotalSampleRate


                CASE("&DEPTH_SUBBOX")

                    this%ImplantDepthDistType = p_DEPT_DIS_BOX

                    call EXTRACT_NUMB(STR,3,N,STRTMP)
                    if(N .LT. 3) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the clusters depth distribution subbox type"
                        write(*,*) "You shoud special: &DEPTH_SUBBOX THE SUBOX SHAPE IS THAT: X =, Y =, Z ="
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if
                    DO I=1, 3
                        this%SUBBOXBOUNDARY(I,1) = Host_SimBoxes%BOXBOUNDARY(I,1) - DRSTR(STRTMP(I))*C_NM2CM/2
                        this%SUBBOXBOUNDARY(I,2) = Host_SimBoxes%BOXBOUNDARY(I,2) + DRSTR(STRTMP(I))*C_NM2CM/2
                    END DO

                CASE("&DEPTH_GAUSS")

                    this%ImplantDepthDistType = p_DEPT_DIS_GAS

                    call EXTRACT_NUMB(STR,2,N,STRTMP)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: Too few parameters for the clusters depth distribution gauss type"
                        write(*,*) "You shoud special: &DEPTH_GAUSS THE GAUSS DISTRIBUTION CENTRAL = , THE HALF WIDTH = "
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if
                    this%DepthINI = DRSTR(STRTMP(1))*C_NM2CM
                    this%DepthSDINI = DRSTR(STRTMP(2))*C_NM2CM
                CASE default
                    write(*,*) "MCPSCUERROR: Illegal Symbol: ", KEYWORD
                    pause
                    stop
            END SELECT

        END DO

        return

        100 write(*,*) "MCPSCUERROR : Load Implantation configuration file failed for clusters depth distribution!"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadImplantClusterDepthDist_Simple

    !*********************************************************************
    subroutine Implant(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TheMigCoaleStatInfoWrap,Record,TSTEP,SURDIFPRE_FREE,SURDIFPRE_INGB)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF)::TSTEP
        real(kind=KINDDF),intent(in)::SURDIFPRE_FREE
        real(kind=KINDDF),intent(in)::SURDIFPRE_INGB
        !---Body---

        select case(this%InsertCountModel)
            case(p_InsertCountModel_ByClusterNum)
                call this%ImplantClusters_Contiune(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TheMigCoaleStatInfoWrap,Record,TSTEP)
            case(p_InsertCountModel_ByConfigNum)
                call this%ImplantClusters_BatchFromConfig(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TheMigCoaleStatInfoWrap,Record,TSTEP,SURDIFPRE_FREE,SURDIFPRE_INGB)
            case default
                write(*,*) "MCPSCUERROR: Unknown insert count model : ", this%InsertCountModel
                pause
                stop
        end select

        return
    end subroutine

    !*********************************************************************
    subroutine ImplantClusters_BatchFromConfig(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TheMigCoaleStatInfoWrap,Record,TSTEP,SURDIFPRE_FREE,SURDIFPRE_INGB)
        use RAND32_MODULE
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF)::TSTEP
        real(kind=KINDDF),intent(in)::SURDIFPRE_FREE
        real(kind=KINDDF),intent(in)::SURDIFPRE_INGB
        !---Local Vars---
        integer::I
        type(MigCoalClusterRecord)::tempRecord
        integer::ISelected
        character*1000::cfgFile
        character*30::TheVersion
        integer::TotalImplantNum
        integer::NC0
        integer::NCAfter
        integer::NCBefore
        integer::MultiBox
        integer::NewTotalSize
        integer::IBox
        !---Body---
        if(Record%GetStatu_InsertOneBatchInNextStep() .eq. .true.) then

            TotalImplantNum = 0

            MultiBox = Host_SimuCtrlParam%MultiBox

            if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
                NC0 = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
            else
                NC0 = 0
            end if

            call GetBoxesMigCoaleStat_Used_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

            NCBefore = sum(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC)

            call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

            call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

            DO I = 1,this%InsertCountOneBatch
                select case(this%InsetSequence)
                case(p_InsertConfig_ByAlphaBeta)
                    ISelected = mod(Record%Get_InsertBatchNum()*this%InsertCountOneBatch + I,this%ImplantCfgFileList%GetSTRList_Count())

                    if(ISelected .eq. 0) then
                        ISelected = this%ImplantCfgFileList%GetSTRList_Count()
                    end if

                case(p_InsertConfig_Random)
                    ISelected = nint(DRAND32()*this%ImplantCfgFileList%GetSTRList_Count()) + 1
                    ISelected = min(ISelected,this%ImplantCfgFileList%GetSTRList_Count())

                case default
                    write(*,*) "MCPSCUERROR: Unknown insert sequence model: ",this%InsetSequence
                    pause
                    stop
                end select

                cfgFile = this%ImplantCfgFileList%GetValueBySTRListIndex(ISelected)
                call Host_Boxes%PutinCfg(Host_SimuCtrlParam,tempRecord,cfgFile,SURDIFPRE_FREE,SURDIFPRE_INGB,TheVersion)

            END DO

            call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

            if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
                NewTotalSize = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
            else
                NewTotalSize = 0
            end if

            call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeWalkRandNum(NewTotalSize)

            call CleanSimulationBoxes_GPU(Dev_Boxes)

            call Dev_Boxes%InitSimulationBoxes_Dev(Host_Boxes,Host_SimuCtrlParam)

            call Dev_Boxes%CopyInBoxesArrayFromHost(Host_Boxes,NewTotalSize,Record,IfCpyNL=.false.)

            call GetBoxesMigCoaleStat_Virtual_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual,Record)
            call GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)
            call GetBoxesMigCoaleStat_Used_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Used,Record)

            NCAfter = sum(Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC)

            DO IBox = 1,MultiBox
                write(*,*) "The virtual range for box ",IBox, " is ",Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)
            END DO

            if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalSteps) then
                call Record%SetLastUpdateStatisTime(Record%GetSimuSteps() + 1.D0)
            else if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalRealTime) then
                call Record%SetLastUpdateStatisTime(Record%GetSimuTimes() + TSTEP)
            end if

            call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                      max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                          TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)))


            Dev_Boxes%dm_SEUsedIndexBox = Host_Boxes%m_BoxesInfo%SEUsedIndexBox

            TotalImplantNum = NCAfter - NCBefore

            call Record%AddImplantedEntitiesNum(TotalImplantNum)

            call Record%InCrease_OneInsertBatchNum()
        end if

        call this%AdjustTimeStep_ImplantBatchFromConfig(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap,Record,TSTEP)

        return
    end subroutine


    !*********************************************
    subroutine AdjustTimeStep_ImplantBatchFromConfig(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap,Record,TSTEP)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF)::TSTEP
        !---Local Vars---
        integer::I
        !---Body---

        if(this%NInsertTimePoint .GT. 0) then
            DO I = 1,this%NInsertTimePoint
                if(this%InsertTimePoint(I) .GE. Record%GetSimuTimes() .AND. this%InsertTimePoint(I) .LE. (Record%GetSimuTimes() + TSTEP) ) then
                    TSTEP = this%InsertTimePoint(I) - Record%GetSimuTimes()
                    call Record%SetTrue_InsertOneBatchInNextStep()
                else
                    call Record%SetFalse_InsertOneBatchInNextStep()
                end if

            END DO
        else
            if((Record%GetSimuTimes() + TSTEP)/this%InsertTimeInterval .GT. Record%Get_InsertBatchNum()) then
                TSTEP = Record%Get_InsertBatchNum()*this%InsertTimeInterval - Record%GetSimuTimes()
                call Record%SetTrue_InsertOneBatchInNextStep()
            else
                call Record%SetFalse_InsertOneBatchInNextStep()
            end if
        end if

        return
    end subroutine AdjustTimeStep_ImplantBatchFromConfig

    !*********************************************************************
    subroutine ImplantClusters_Contiune(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,TheMigCoaleStatInfoWrap,Record,TSTEP)
        use RAND32_MODULE
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF)::TSTEP
        !---Local Vars---
        integer::err
        integer::MultiBox
        integer::IBox
        integer::ImplantNumEachBox_Ceiling
        logical::NeedAddVirtualRange
        logical::NeedAddExpdRange
        integer::NewAllocateNCEachBox
        integer::NewTotalSize
        real(kind=KINDDF)::tempTSTEP
        integer::tempImplantNumEachBox
        integer::NSIZE
        integer::ImplantNumEachBox
        integer::TotalImplantNum
        integer::NC0
        !---Body---

        TotalImplantNum = 0

        NeedAddVirtualRange = .false.

        NeedAddExpdRange = .false.

        MultiBox = Host_SimuCtrlParam%MultiBox

        call this%AdjustTimeStep_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap,Record,TSTEP,ImplantNumEachBox_Ceiling)

        DO IBox = 1,MultiBox
            if((Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + ImplantNumEachBox_Ceiling) .GT. Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)) then
                NeedAddVirtualRange = .true.
                exit
            end if
        END DO

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) .GT. 0) then
            NC0 = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(Host_SimuCtrlParam%MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            NC0 = 0
        end if

        if(NeedAddVirtualRange .eq. .true.) then

            call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)
            call Record%RecordNC_ForSweepOut(MultiBox,Host_Boxes%m_BoxesBasicStatistic)

            call Record%IncreaseOneSweepOutCount()

            call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NC0,IfCpyNL=.false.)

            call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record,SweepOutCount=Record%GetSweepOutCount())

            call Dev_Boxes%SweepUnActiveMemory_GPUToCPU(Host_Boxes,Host_SimuCtrlParam,Record)

            call Dev_Boxes%GetBoxesBasicStatistic_AllStatu_GPU(Host_Boxes,Host_SimuCtrlParam)

            if(this%Cal_ExpandSize_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,TSTEP,ImplantNumEachBox_Ceiling,NewAllocateNCEachBox) .eq. .false.) then
                write(*,*) "MCPSCUInfo: There are no enough memory to do the future implant job, so the time step is deduced."
            end if

            write(*,*) "NewAllocateNCEachBox",NewAllocateNCEachBox

            if(NewAllocateNCEachBox .GT. 0) then ! need to allocate new memory for new added cluster

                write(*,*) ".....Add virtual range...."

                call Dev_Boxes%ExpandClustersInfo_GPUToCPU_EqualNum(Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)

                if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
                    NewTotalSize = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
                else
                    NewTotalSize = 0
                end if

                call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeWalkRandNum(NewTotalSize)
                call Dev_MigCoaleGVars%dm_MigCoale_RandDev%ReSizeImplantRandNum(MultiBox*NewAllocateNCEachBox)

                call this%DoImplantTillVirtualBoundary_CPUTOGPU_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Record,Dev_Boxes,NewAllocateNCEachBox)

                call GetBoxesMigCoaleStat_Virtual_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual,Record)

                DO IBox = 1,MultiBox
                    write(*,*) "The virtual range for box ",IBox, " is ",Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)
                END DO

            end if

            DO IBox = 1,MultiBox
                Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = min(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + ImplantNumEachBox_Ceiling*this%ExpandFactor,Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2))
            END DO
            Dev_Boxes%dm_SEExpdIndexBox = Host_Boxes%m_BoxesInfo%SEExpdIndexBox

            call GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)

            if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalSteps) then
                call Record%SetLastUpdateStatisTime(Record%GetSimuSteps() + 1.D0)
            else if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalRealTime) then
                call Record%SetLastUpdateStatisTime(Record%GetSimuTimes() + TSTEP)
            end if

            call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                      max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                          TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)))

        else

            DO IBox = 1,MultiBox
                if((Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + ImplantNumEachBox_Ceiling) .GT. Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2)) then
                    NeedAddExpdRange = .true.
                    exit
                end if
            END DO

            if(NeedAddExpdRange .eq. .true.) then

                write(*,*) ".....Add expand range...."

                DO IBox = 1,MultiBox
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = min(Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + ImplantNumEachBox_Ceiling*this%ExpandFactor,Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2))

                    write(*,*) "The expanded range for box ",IBox, " is ",Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2)
                END DO
                Dev_Boxes%dm_SEExpdIndexBox = Host_Boxes%m_BoxesInfo%SEExpdIndexBox

                call GetBoxesMigCoaleStat_Expd_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd,Record)

                if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalSteps) then
                    call Record%SetLastUpdateStatisTime(Record%GetSimuSteps() + 1.D0)
                else if(Host_SimuCtrlParam%TUpdateStatisFlag .eq. mp_UpdateStatisFlag_ByIntervalRealTime) then
                    call Record%SetLastUpdateStatisTime(Record%GetSimuTimes() + TSTEP)
                end if

                call Cal_Neighbor_List_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,IfDirectly=.true.,RMAX= &
                                            max(TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEFREE_STATU), &
                                                TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Expd%statistic_IntegralBox%RMAX(p_ACTIVEINGB_STATU)))

            end if

        end if


        DO IBox = 1,MultiBox

            ImplantNumEachBox = floor(this%InsertCountOneBatch*TSTEP/this%InsertTimeInterval)

            if(DRAND32() .LE. (this%InsertCountOneBatch*TSTEP/this%InsertTimeInterval - ImplantNumEachBox)) then
                ImplantNumEachBox = ImplantNumEachBox + 1
            end if

            Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) =  Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + ImplantNumEachBox

            if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .GT. Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2)) then
                write(*,*) "MCPSCUERROR: The expand size for box ,",IBox," is not enough!"
                write(*,*) "Implant number is: ",ImplantNumEachBox
                write(*,*) "End of the used clusters index is, ",Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                write(*,*) "End of the expd clusters index is, ",Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2)
                pause
                stop
            end if

            if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .GT. Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)) then
                write(*,*) "MCPSCUERROR: The virtual size for box ,",IBox," is not enough!"
                write(*,*) "Implant number is: ",ImplantNumEachBox
                write(*,*) "End of the used clusters index is, ",Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                write(*,*) "End of the virtual clusters index is, ",Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)
                pause
                stop
            end if

            TotalImplantNum = TotalImplantNum + ImplantNumEachBox
        END DO

        Dev_Boxes%dm_SEUsedIndexBox = Host_Boxes%m_BoxesInfo%SEUsedIndexBox

        call Record%AddImplantedEntitiesNum(TotalImplantNum)

        return
    end subroutine ImplantClusters_Contiune

    !*********************************************
    subroutine AdjustTimeStep_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap,Record,TSTEP,ImplantNumEachBox_Ceiling)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoaleStatInfoWrap)::TheMigCoaleStatInfoWrap
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF)::TSTEP
        integer::ImplantNumEachBox_Ceiling
        !---Local Vars---
        integer::ImplantNumEachBox
        real(kind=KINDDF)::VerifyTime
        !---Body---

        DO While(.true.)

            ImplantNumEachBox_Ceiling = ceiling(this%InsertCountOneBatch*TSTEP/this%InsertTimeInterval)

            VerifyTime = Cal_VerifyTime_Implant(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,TheMigCoaleStatInfoWrap%m_MigCoaleStatisticInfo_Virtual,Record,ImplantNumEachBox_Ceiling)

            if(VerifyTime .LT. TSTEP) then
                TSTEP = TSTEP*0.95
                cycle
            else
                exit
            end if
        END DO

        return
    end subroutine AdjustTimeStep_ImplantContiune

    !*********************************************
    function Cal_ExpandSize_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Record,TSTEP,ImplantNumEachBox_Ceiling,NewAllocateNCEachBox) result(TheStatu)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoalClusterRecord)::Record
        real(kind=KINDDF), intent(inout)::TSTEP
        integer, intent(inout)::ImplantNumEachBox_Ceiling
        integer, intent(inout)::NewAllocateNCEachBox
        logical, intent(inout)::TheStatu
        !---Local Vars---
        integer::err
        real(kind=KINDDF)::newSimulatedTime
        integer::MultiBox
        integer::IBox
        integer(kind=cuda_count_kind)::FreeMemSize,TotalMemSize
        integer::FreeMemSize2NCEachBox
        integer::TotalMemSize2NCEachBox
        integer::AllocatedFreeNCEachBox
        integer::newFreeNCEachBox
        integer::PreAllocatedNCEachBox
        integer::PreFreeNCEachBox
        integer::RestoreImplantNumEachBox
        real::ImplantPersistTime
        integer,dimension(:),allocatable::NCFree
        !---Body---

        RestoreImplantNumEachBox = ImplantNumEachBox_Ceiling

        FreeMemSize2NCEachBox = 0
        TotalMemSize2NCEachBox = 0
        NewAllocateNCEachBox = 0

        MultiBox = Host_SimuCtrlParam%MultiBox

        ImplantPersistTime = Record%GetSimuTimes() - Record%GetStartImplantTime()

        call AllocateArray_Host(NCFree,MultiBox,"NCFree")

        DO IBox=1,MultiBox
            PreAllocatedNCEachBox = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            if(PreAllocatedNCEachBox .LE. 0) then
                PreAllocatedNCEachBox = 0
            end if

            PreFreeNCEachBox = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            if(PreFreeNCEachBox .LE. 0) then
                PreFreeNCEachBox = 0
            end if

            NCFree(IBox) = PreAllocatedNCEachBox - PreFreeNCEachBox

            if(NCFree(IBox) .LE. 0) then
                NCFree(IBox) = 0
            end if
        END DO
        AllocatedFreeNCEachBox = minval(NCFree)

        err = cudaMemGetInfo(FreeMemSize,TotalMemSize)

        !The 8*3 is for the random walking random number
        if(Host_SimuCtrlParam%FreeDiffusion .eq. .false.) then
            FreeMemSize2NCEachBox  = (FreeMemSize/(Dev_Boxes%dm_ClusterInfo_GPU%GetMemConsumOneClusterInfo(Host_SimuCtrlParam%MAXNEIGHBORNUM) + 8*3))/MultiBox
            TotalMemSize2NCEachBox = (TotalMemSize/(Dev_Boxes%dm_ClusterInfo_GPU%GetMemConsumOneClusterInfo(Host_SimuCtrlParam%MAXNEIGHBORNUM)+ 8*3))/MultiBox
        else
            FreeMemSize2NCEachBox  = (FreeMemSize/(Dev_Boxes%dm_ClusterInfo_GPU%GetMemConsumOneClusterInfo(0) + 8*3))/MultiBox
            TotalMemSize2NCEachBox = (TotalMemSize/(Dev_Boxes%dm_ClusterInfo_GPU%GetMemConsumOneClusterInfo(0)+ 8*3))/MultiBox
        end if

        DO while(.true.)

            NewAllocateNCEachBox = 0

            newFreeNCEachBox = AllocatedFreeNCEachBox

            if(newFreeNCEachBox .GE. ImplantNumEachBox_Ceiling*this%ExpandFactor) then
                exit
            end if

            NewAllocateNCEachBox = min(FreeMemSize2NCEachBox,TotalMemSize2NCEachBox/this%MemoryOccupyFactor)

            newFreeNCEachBox = AllocatedFreeNCEachBox + NewAllocateNCEachBox

            if(newFreeNCEachBox .LT. ImplantNumEachBox_Ceiling*this%ExpandFactor .or. (FreeMemSize2NCEachBox - NewAllocateNCEachBox) .LE. 0) then
                TSTEP = TSTEP/2.D0
                ImplantNumEachBox_Ceiling = ceiling(this%InsertCountOneBatch*TSTEP/this%InsertTimeInterval)
            else
                exit
            end if

            if(ImplantNumEachBox_Ceiling .LE. 0) then
                exit
            end if
        END DO

        if(RestoreImplantNumEachBox .GT. 0 .AND. ImplantNumEachBox_Ceiling .LE. 0) then
            TheStatu = .false.
        else
            TheStatu = .true.
        end if

        call DeAllocateArray_Host(NCFree,"NCFree")

        return
    end function Cal_ExpandSize_ImplantContiune

    !*************************************************************
    subroutine DoImplantTillVirtualBoundary_CPU_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::NewAllocateNCEachBox
        !---Body---
        select case(this%ImplantConfigType)
            case(p_ImplantConfig_Simple)
                call this%FillVirtualBoundary_CPU_Simple_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
            case(p_ImplantConfig_SpecialDistFromFile)
                call this%FillVirtualBoundary_CPU_FromFile_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
            case(p_ImplantConfig_SpecialDistFromExteFunc)
                call this%FillVirtualBoundary_CPU_FromExteFunc_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
            case default
                write(*,*) "MCPSCUERROR: Unknown strategy for the implantation configuration:",this%ImplantConfigType
                pause
                stop
        end select
        return
    end subroutine DoImplantTillVirtualBoundary_CPU_ImplantContiune

    !*************************************************************
    subroutine FillVirtualBoundary_CPU_Simple_ImplantContiune(this,SimBoxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::NewAllocateNCEachBox
        !---Body--
        select case(this%ImplantDepthDistType)
            case(p_DEPT_DIS_Layer)
                call this%FillVirtualBoundary_CPU_Depth_LAY_ImplantContiune(SimBoxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
            case(p_DEPT_DIS_BOX)
                call this%FillVirtualBoundary_CPU_Depth_SubBox_ImplantContiune(SimBoxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
            case(p_DEPT_DIS_GAS)
                call this%FillVirtualBoundary_CPU_Depth_Gauss_ImplantContiune(SimBoxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
            case default
                write(*,*) "MCPSCUERROR : Unknown way to Unknown strategy for the simple implantation configuration: ",this%ImplantDepthDistType
                pause
                stop
        end select

        return
    end subroutine FillVirtualBoundary_CPU_Simple_ImplantContiune

    !*************************************************************
    subroutine FillVirtualBoundary_CPU_FromFile_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
        use RAND32_MODULE
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::NewAllocateNCEachBox
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFROM
        integer::ICTO
        type(DiffusorValue)::TheDiffusorValue
        integer::NC
        integer::NCUsed
        real(kind=KINDDF)::POS(3)
        integer::MaxGroups
        real(kind=KINDDF)::tempRand
        real(kind=KINDDF)::GroupRateTemp
        integer::ILayer
        integer::IGroup
        logical::exitFlag
        integer::LayerNum
        integer::NCAccum
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        LayerNum = size(this%LayerThick)

        DO IBox = 1,MultiBox

            if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .LE. 0) then
                NCUsed = 0
            else
                NCUsed = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            end if

            if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .LE. 0) then
                NC = 0
            else
                NC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            end if

            if((NC - NCUsed) .LT. NewAllocateNCEachBox) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
                write(*,*) "The waiting to be implanted clusters number is:",NewAllocateNCEachBox
                pause
                stop
            end if

            ICFROM = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox + 1
            ICTO = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

            if(NewAllocateNCEachBox .LE. 0) then
                cycle
            end if

            if(ICFROM .LT. Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are too small to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "It would occupy other free clusters for id: ",ICFROM
                pause
                stop
            end if

            if(size(this%ClustersSample) .LE. 0) then
                write(*,*) "MCPSCUERROR: The number of sample clusters is less than 1: "
                write(*,*) "There would be no clusters to be implanted from the sample distribution."
                pause
                stop
            end if

            MaxGroups = size(this%ClustersSampleRate,dim=2)

            NCAccum = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) + sum(Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU))

            DO IC = ICFROM,ICTO

                tempRand = DRAND32()

                GroupRateTemp = 0.D0

                exitFlag = .false.
                DO ILayer = 1,LayerNum

                    if(exitFlag .eq. .true.) then
                        exit
                    end if

                     DO IGroup = 1,MaxGroups
                        GroupRateTemp = GroupRateTemp + this%ClustersSampleRate(ILayer,IGroup)
                        if(GroupRateTemp .GE. tempRand) then

                            call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                            !---The assignment(=) had been override
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms = this%ClustersSample(ILayer,IGroup)%m_Atoms

                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = ILayer

                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = this%ClustersSample(ILayer,IGroup)%m_Statu

                            POS(1) = DRAND32()*Host_Boxes%BOXSIZE(1) + Host_Boxes%BOXBOUNDARY(1,1)
                            POS(2) = DRAND32()*Host_Boxes%BOXSIZE(2) + Host_Boxes%BOXBOUNDARY(2,1)
                            POS(3) = DRAND32()*this%LayerThick(ILayer) + sum(this%LayerThick(1:ILayer-1)) + Host_Boxes%BOXBOUNDARY(3,1)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS

                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .GT. 0.D0) then
                                write(*,*) "MCPSCUERROR: the implant position had been occupied in memory",IC
                                pause
                            end if

                            TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

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

                            else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU) then

                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = this%ClustersSample(ILayer,IGroup)%m_GrainID(1)

                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) .GT. Host_Boxes%m_GrainBoundary%GrainNum) then
                                    write(*,*) "MCPSCUERROR: The grain number is greater than the seeds number in system."
                                    write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1)
                                    pause
                                    stop
                                end if

                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(2) = this%ClustersSample(ILayer,IGroup)%m_GrainID(2)

                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(2) .GT. Host_Boxes%m_GrainBoundary%GrainNum) then
                                    write(*,*) "MCPSCUERROR: The grain number is greater than the seeds number in system."
                                    write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(2)
                                    pause
                                    stop
                                end if

                                select case(TheDiffusorValue%ECRValueType_InGB)
                                    case(p_ECR_ByValue)
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_InGB
                                    case default
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_InGB,                          &
                                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                                   Host_SimuCtrlParam%TKB,                                      &
                                                                                                                   Host_Boxes%LatticeLength)
                                end select

                                select case(TheDiffusorValue%DiffusorValueType_InGB)
                                    case(p_DiffuseCoefficient_ByValue)
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_InGB_Value
                                    case(p_DiffuseCoefficient_ByArrhenius)
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/Host_SimuCtrlParam%TKB)
                                    case(p_DiffuseCoefficient_ByBCluster)
                                        ! Here we adopt a model that D=D0*(1/R)**Gama
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_GBSURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                                    case(p_DiffuseCoefficient_BySIACluster)
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_InGB))* &
                                                                                                    TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/Host_SimuCtrlParam%TKB)
                                    case(p_DiffuseCoefficient_ByVcCluster)
                                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_InGB)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                                   TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/Host_SimuCtrlParam%TKB)
                                end select
                            end if

                            NCAccum = NCAccum + 1
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = NCAccum
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                            exitFlag = .true.

                            exit

                        end if

                    END DO
                END DO

            END DO

        END DO

        return
    end subroutine FillVirtualBoundary_CPU_FromFile_ImplantContiune

    !*************************************************************
    subroutine FillVirtualBoundary_CPU_FromExteFunc_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::NewAllocateNCEachBox
        !---Body---

    end subroutine FillVirtualBoundary_CPU_FromExteFunc_ImplantContiune


    !*************************************************************
    subroutine DoImplantTillVirtualBoundary_CPUTOGPU_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,Dev_Boxes,NewAllocateNCEachBox)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(SimulationBoxes_GPU)::Dev_Boxes
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::NewAllocateNCEachBox
        !---Local Vars---
        integer::MultiBox
        integer::NSIZE
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox

        call this%DoImplantTillVirtualBoundary_CPU_ImplantContiune(Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
            NSIZE = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            NSIZE = 0
        end if

        if(NSIZE .ne. size(Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters)) then

            call Dev_Boxes%dm_ClusterInfo_GPU%ReleaseClustersInfo_GPU()

            if(Host_SimuCtrlParam%FreeDiffusion .eq. .false.) then
                call Dev_Boxes%dm_ClusterInfo_GPU%AllocateClustersInfo_GPU(NSIZE,Host_SimuCtrlParam%MAXNEIGHBORNUM)
            else
                call Dev_Boxes%dm_ClusterInfo_GPU%AllocateClustersInfo_GPU(NSIZE,0)
            end if
        end if

        call Dev_Boxes%dm_ClusterInfo_GPU%CopyInFromHost(Host_Boxes%m_ClustersInfo_CPU,NSIZE,IfCpyNL=.false.)

        return
    end subroutine DoImplantTillVirtualBoundary_CPUTOGPU_ImplantContiune

    !**************************************************************
    subroutine FillVirtualBoundary_CPU_Depth_LAY_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
      !*** Purpose: To initialize the system (clusters distributed as the form of layer)
      ! Host_Boxes: the boxes information in host
      use RAND32_MODULE
      implicit none
      !---Dummy Vars---
      CLASS(ImplantSection)::this
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(MigCoalClusterRecord)::Record
      integer,intent(in)::NewAllocateNCEachBox
      !-----local variables---
      integer::MultiBox
      real(kind=KINDDF)::POS(3)
      integer::IBox
      integer::NC
      integer::NCUsed
      integer::IC
      integer::ICFROM
      integer::ICTO
      integer::MaxGroups
      real(kind=KINDDF)::tempRand
      real(kind=KINDDF)::GroupRateTemp
      integer::ILayer
      integer::IGroup
      integer::IElement
      integer::NAtoms
      type(DiffusorValue)::TheDiffusorValue
      logical::exitFlag
      integer::LayerNum
      integer::NCAccum
      !---Body---
      MultiBox = Host_SimuCtrlParam%MultiBox

      LayerNum = size(this%LayerThick)

      DO IBox = 1,MultiBox

        if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .LE. 0) then
            NCUsed = 0
        else
            NCUsed = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
        end if

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .LE. 0) then
            NC = 0
        else
            NC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
        end if

        if((NC - NCUsed) .LT. NewAllocateNCEachBox) then
            write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
            write(*,*) "For box :",IBox
            write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
            write(*,*) "The waiting to be implanted clusters number is:",NewAllocateNCEachBox
            pause
            stop
        end if

        ICFROM = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox + 1
        ICTO = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

        if(ICFROM .LT. Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)) then
            write(*,*) "MCPSCUERROR: The allocated  memory space are too small to implant the clusters"
            write(*,*) "For box :",IBox
            write(*,*) "It would occupy other free clusters for id: ",IC
            pause
            stop
        end if

        MaxGroups = size(this%ClustersSampleRate,dim=2)

        NCAccum = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) + sum(Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU))

        DO IC=ICFROM,ICTO

            tempRand = DRAND32()

            GroupRateTemp = 0.D0

            exitFlag = .false.
            DO ILayer = 1,LayerNum
                if(exitFlag .eq. .true.) then
                    exit
                end if

                DO IGroup = 1,MaxGroups
                    GroupRateTemp = GroupRateTemp + this%ClustersSampleRate(ILayer,IGroup)
                    if(GroupRateTemp .GE. tempRand) then

                        call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                        !*** Initialize the size of the clusters
                        NAtoms = RGAUSS0_WithCut(this%NAINI, this%NASDINI,this%NACUT(1),this%NACUT(2))

                        DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(IElement)%m_NA = floor(NAtoms*this%CompositWeight(IElement)+0.5D0)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(IElement)%m_ID = IElement
                        END DO

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = ILayer

                        POS(1) = DRAND32()*Host_Boxes%BOXSIZE(1) + Host_Boxes%BOXBOUNDARY(1,1)
                        POS(2) = DRAND32()*Host_Boxes%BOXSIZE(2) + Host_Boxes%BOXBOUNDARY(2,1)
                        POS(3) = DRAND32()*this%LayerThick(ILayer) + sum(this%LayerThick(1:ILayer-1)) + Host_Boxes%BOXBOUNDARY(3,1)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

                        TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                        !-- In Current application, the simple implant distribution is only considered in free matrix, if you want to init the clusters in GB---
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

                        NCAccum = NCAccum + 1
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = NCAccum
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                        exitFlag = .true.
                        exit

                    end if
                END DO
            END DO

        END DO

      END DO

      return
    end subroutine FillVirtualBoundary_CPU_Depth_LAY_ImplantContiune

    !**************************************************************
    subroutine FillVirtualBoundary_CPU_Depth_SubBox_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
      !*** Purpose: To initialize the system (clusters distributed as the form of layer)
      ! Host_Boxes: the boxes information in host
      use RAND32_MODULE
      implicit none
      !---Dummy Vars---
      CLASS(ImplantSection)::this
      type(SimulationBoxes)::Host_Boxes
      type(SimulationCtrlParam)::Host_SimuCtrlParam
      type(MigCoalClusterRecord)::Record
      integer,intent(in)::NewAllocateNCEachBox
      !-----local variables---
      integer::MultiBox
      real(kind=KINDDF)::POS(3)
      real(kind=KINDDF)::SUBBOXSIZE(3)
      integer::IBox, II, IC
      integer::I
      integer::NC
      integer::NCUsed
      integer::NAtoms
      integer::IElement
      type(DiffusorValue)::TheDiffusorValue
      integer::NCAccum
      !---Body---
      MultiBox = Host_SimuCtrlParam%MultiBox

      DO I = 1,3
        SUBBOXSIZE(I) = this%SUBBOXBOUNDARY(I,2) - this%SUBBOXBOUNDARY(I,1)
      END DO

      DO IBox = 1,MultiBox

        if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .LE. 0) then
            NCUsed = 0
        else
            NCUsed = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
        end if

        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .LE. 0) then
            NC = 0
        else
            NC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
        end if

        if((NC - NCUsed) .LT. NewAllocateNCEachBox) then
            write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
            write(*,*) "For box :",IBox
            write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
            write(*,*) "The waiting to be implanted clusters number is:",NewAllocateNCEachBox
            pause
            stop
        end if

        IC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox

        if(IC .LT. Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)) then
            write(*,*) "MCPSCUERROR: The allocated  memory space are too small to implant the clusters"
            write(*,*) "For box :",IBox
            write(*,*) "It would occupy other free clusters for id: ",IC
            pause
            stop
        end if

        NCAccum = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) + sum(Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU))

        DO II = 1, NewAllocateNCEachBox

            IC = IC + 1
            !Initialize the position of clusters

            call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

            DO I = 1,3
                POS(I) = DRAND32()*SUBBOXSIZE(I) + this%SUBBOXBOUNDARY(I,1)
            END DO

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS
            !Give the cluster an type(layer) ID for the convenience of visualization
            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = 1

            NAtoms = RGAUSS0_WithCut(this%NAINI, this%NASDINI,this%NACUT(1),this%NACUT(2))

            DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(IElement)%m_NA = floor(NAtoms*this%CompositWeight(IElement)+0.5D0)
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(IElement)%m_ID = IElement
            END DO

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

            TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

            !-- In Current application, the simple implant distribution is only considered in free matrix, if you want to init the clusters in GB---
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

            NCAccum = NCAccum + 1
            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = NCAccum
            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

        END DO

      END DO

      return
    end subroutine FillVirtualBoundary_CPU_Depth_SubBox_ImplantContiune

    !**************************************************************
    subroutine FillVirtualBoundary_CPU_Depth_Gauss_ImplantContiune(this,Host_Boxes,Host_SimuCtrlParam,Record,NewAllocateNCEachBox)
        !*** Purpose: To initialize the system (clusters distributed as the form of gauss in depth)
        ! Host_Boxes: the boxes information in host
        use RAND32_MODULE
        implicit none
        !-------Dummy Vars------
        CLASS(ImplantSection)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        integer,intent(in)::NewAllocateNCEachBox
        !---local variables---
        integer::MultiBox
        real(kind=KINDDF)::POS(3)
        real(kind=KINDDF)::SEP
        real(kind=KINDDF)::BOXBOUNDARY(3,2)
        real(kind=KINDDF)::BOXSIZE(3)
        integer::IBox,II,IC
        integer::NAtoms
        integer::NC
        integer::NCUsed
        integer::IElement
        type(DiffusorValue)::TheDiffusorValue
        integer::NCAccum
        !---Body---
        MultiBox = Host_SimuCtrlParam%MultiBox
        BOXBOUNDARY = Host_Boxes%BOXBOUNDARY
        BOXSIZE = Host_Boxes%BOXSIZE

        DO IBox = 1,MultiBox

            if(Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) .LE. 0) then
                NCUsed = 0
            else
                NCUsed = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            end if

            if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .LE. 0) then
                NC = 0
            else
                NC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            end if

            if((NC - NCUsed) .LT. NewAllocateNCEachBox) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "The free of allocated allocated  memory space is: ",NC - NCUsed
                write(*,*) "The waiting to be implanted clusters number is:",NewAllocateNCEachBox
                pause
                stop
            end if

            IC = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox

            if(IC .LT. Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)) then
                write(*,*) "MCPSCUERROR: The allocated  memory space are too small to implant the clusters"
                write(*,*) "For box :",IBox
                write(*,*) "It would occupy other free clusters for id: ",IC
                pause
                stop
            end if

            NCAccum = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(IBox,2) + sum(Record%RecordNCBeforeSweepOut_Integal(p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU))

            DO II = 1, NewAllocateNCEachBox

                IC = IC + 1
                !Initialize the position of clusters

                call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                POS(1) = DRAND32()*BOXSIZE(1) + BOXBOUNDARY(1,1)
                POS(2) = DRAND32()*BOXSIZE(2) + BOXBOUNDARY(2,1)
                POS(3) = RGAUSS0_WithCut(this%DepthINI, this%DepthSDINI,BOXBOUNDARY(3,1),BOXBOUNDARY(3,2))

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
                NAtoms = RGAUSS0_WithCut(this%NAINI, this%NASDINI,this%NACUT(1),this%NACUT(2))

                DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(IElement)%m_NA = floor(NAtoms*this%CompositWeight(IElement)+0.5D0)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(IElement)%m_ID = IElement
                END DO

                TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                !-- In Current application, the simple implant distribution is only considered in free matrix, if you want to init the clusters in GB---
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

                NCAccum = NCAccum + 1
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = NCAccum
                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

            END DO

        END DO

        return
    end subroutine FillVirtualBoundary_CPU_Depth_Gauss_ImplantContiune


!    !*************************************************************
!    subroutine DoImplantTillVirtualBoundary_GPUTOCPU(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Local Vars---
!        integer::MultiBox
!        integer::NSIZE
!        !---Body---
!
!        MultiBox = Host_SimuCtrlParam%MultiBox
!
!        call this%DoImplantTillVirtualBoundary_GPU(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!
!        if(Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
!            NSIZE = Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - Host_Boxes%m_BoxesInfo%SEVirtualIndexBox(MultiBox,1) + 1
!        else
!            NSIZE = 0
!        end if
!
!        call Host_Boxes%Clean()
!
!        call Dev_Boxes%dm_ClusterInfo_GPU%CopyOutToHost(Host_Boxes%m_ClustersInfo_CPU,NSIZE,IfCpyNL=.false.)
!
!        return
!    end subroutine DoImplantTillVirtualBoundary_GPUTOCPU
!
!
!    !*************************************************************
!    subroutine DoImplantTillVirtualBoundary_GPU(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Body---
!
!        call this%InitImplantInfo_DevPart()
!
!        select case(this%ImplantConfigType)
!            case(p_ImplantConfig_Simple)
!                call this%FillVirtualBoundary_GPU_Simple(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!            case(p_ImplantConfig_SpecialDistFromFile)
!                call this%FillVirtualBoundary_GPU_FromFile(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!            case(p_ImplantConfig_SpecialDistFromExteFunc)
!                call this%FillVirtualBoundary_GPU_FromExteFunc(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!            case default
!                write(*,*) "MCPSCUERROR: Unknown strategy for the implantation configuration:",this%ImplantConfigType
!                pause
!                stop
!        end select
!
!        return
!    end subroutine DoImplantTillVirtualBoundary_GPU
!
!
!    !*************************************************************
!    subroutine FillVirtualBoundary_GPU_Simple(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Body---
!        select case(this%ImplantDepthDistType)
!            case(p_DEPT_DIS_Layer)
!                call this%FillVirtualBoundary_GPU_Depth_LAY(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!            case(p_DEPT_DIS_BOX)
!                call this%FillVirtualBoundary_GPU_Depth_SubBox(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!            case(p_DEPT_DIS_GAS)
!                call this%FillVirtualBoundary_GPU_Depth_Gauss(Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!            case default
!                write(*,*) "MCPSCUERROR : Unknown way to Unknown strategy for the simple implantation configuration: ",this%ImplantDepthDistType
!                pause
!                stop
!        end select
!
!        return
!    end subroutine FillVirtualBoundary_GPU_Simple
!
!    !*************************************************************
!    subroutine FillVirtualBoundary_GPU_Depth_LAY(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Local Vars---
!        integer::MultiBox
!        integer::TotalAllocateNC
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::NB
!        integer::NBX,NBY
!        Integer::BX
!        integer::BY
!        integer::err
!        !---Body---
!
!        ASSOCIATE(ImplantRand=>Dev_MigCoaleGVars%dm_MigCoale_RandDev)
!
!            if(NewAllocateNCEachBox .GT. 0) then
!
!                MultiBox = Host_SimuCtrlParam%MultiBox
!
!                TotalAllocateNC = MultiBox*NewAllocateNCEachBox
!
!                NB = (TotalAllocateNC - 1)/p_BLOCKSIZE + 1
!                NBX  = min(NB,p_BLOCKDIMX)
!                NBY = (NB - 1)/NBX + 1
!
!                !*** to determine the block size
!                BX = p_BLOCKSIZE
!                BY = 1
!                !*** to determine the dimension of blocks
!
!                blocks  = dim3(NBX, NBY, 1)
!                threads = dim3(BX,  BY,  1)
!
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Layer,ImplantRand%dm_SpaceDist_Implant(1:TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_X,ImplantRand%dm_SpaceDist_Implant(TotalAllocateNC+1:2*TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Y,ImplantRand%dm_SpaceDist_Implant(2*TotalAllocateNC+1:3*TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Z,ImplantRand%dm_SpaceDist_Implant(3*TotalAllocateNC+1:4*TotalAllocateNC),TotalAllocateNC)
!
!                err = curandGenerateNormal(ImplantRand%m_ranGen_ClustersSizeDist,ImplantRand%dm_SizeDist_Implant,TotalAllocateNC,this%NAINI,this%NASDINI)
!
!                call Kernel_ImplantClusters_Depth_Layer<<<blocks,threads>>>(TotalAllocateNC,                                          &
!                                                                            NewAllocateNCEachBox,                                     &
!                                                                            Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                 &
!                                                                            Dev_Boxes%dm_DiffusorTypesMap%Dev_TypesEntities,          &
!                                                                            Dev_Boxes%dm_DiffusorTypesMap%Dev_SingleAtomsDivideArrays,&
!                                                                            Host_Boxes%m_GrainBoundary%GrainNum,                      &
!                                                                            Dev_Boxes%dm_GrainBoundary%dm_GrainSeeds,                 &
!                                                                            ImplantRand%dm_SpaceDist_Implant,                         &
!                                                                            ImplantRand%dm_SizeDist_Implant,                          &
!                                                                            this%NACUT(1),                                            &
!                                                                            this%NACUT(2),                                            &
!                                                                            Dev_Boxes%dm_SEVirtualIndexBox,                           &
!                                                                            Dev_Boxes%dm_RecordNCBeforeSweepOut_SingleBox,            &
!                                                                            this%dm_ImplantInfo_DevPart%Dev_LayerThick,               &
!                                                                            this%dm_ImplantInfo_DevPart%Dev_ClustersSampleRate,       &
!                                                                            this%dm_ImplantInfo_DevPart%Dev_CompositWeight)
!            end if
!
!        END ASSOCIATE
!
!        return
!    end subroutine FillVirtualBoundary_GPU_Depth_LAY
!
!    !**********************************************
!    attributes(global) subroutine Kernel_ImplantClusters_Depth_Layer(TotalAllocateNC,            &
!                                                                    NewAllocateNCEachBox,        &
!                                                                    Dev_Clusters,                &
!                                                                    Dev_TypesEntities,           &
!                                                                    Dev_SingleAtomsDivideArrays, &
!                                                                    Nseeds,                      &
!                                                                    Dev_GrainSeeds,              &
!                                                                    Dev_RandArray_SpaceDist,     &
!                                                                    Dev_RandArray_SizeDist,      &
!                                                                    LNACUT,                      &
!                                                                    RNACUT,                      &
!                                                                    Dev_SEVirtualIndexBox,       &
!                                                                    Dev_RecordNCBeforeSweepOut_SingleBox, &
!                                                                    Dev_LayerThick,              &
!                                                                    Dev_ClustersSampleRate,      &
!                                                                    Dev_CompositWeight)
!        implicit none
!        !---Dummy Vars---
!        integer, value::TotalAllocateNC
!        integer, value::NewAllocateNCEachBox
!        type(ACluster), device::Dev_Clusters(:)
!        type(DiffusorTypeEntity),device::Dev_TypesEntities(:)
!        integer,device::Dev_SingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
!        integer,value::Nseeds
!        type(GrainSeed),device::Dev_GrainSeeds(:)
!        real(kind=KINDDF),device::Dev_RandArray_SpaceDist(:)
!        real(kind=KINDDF),device::Dev_RandArray_SizeDist(:)
!        real(kind=KINDDF),value::LNACUT
!        real(kind=KINDDF),value::RNACUT
!        integer, device::Dev_SEVirtualIndexBox(:,:)
!        integer, device::Dev_RecordNCBeforeSweepOut_SingleBox(:,:)
!        real(kind=KINDDF),device::Dev_LayerThick(:)
!        real(kind=KINDDF),device::Dev_ClustersSampleRate(:,:)
!        real(kind=KINDDF),device::Dev_CompositWeight(:)
!        !---Local Vars---
!        integer::tid
!        integer::bid
!        integer::cid
!        integer::IBox
!        integer::cid0
!        integer::ICTRUE
!        real(kind=KINDDF)::POS(3)
!        integer::NLayer
!        integer::MaxGroups
!        integer::ILayer
!        integer::IGroup
!        logical::exitFlag
!        real(kind=KINDDF)::tempRand
!        real(kind=KINDDF)::GroupRateTemp
!        integer::IElement
!        type(DiffusorValue)::TheDiffusorValue
!        real(kind=KINDDF)::randSize
!        integer::ATOMS(p_ATOMS_GROUPS_NUMBER)
!        !---Body---
!        tid = (threadidx%y - 1)*blockdim%x + threadidx%x
!        bid = (blockidx%y - 1)*griddim%x + blockidx%x
!        cid = (bid - 1)*blockdim%x*blockdim%y + tid
!
!        IBox = (cid - 1)/NewAllocateNCEachBox + 1
!        cid0 = (IBox - 1)*NewAllocateNCEachBox + 1
!
!        if(cid .LE. TotalAllocateNC) then
!            NLayer = size(Dev_ClustersSampleRate,dim=1)
!            MaxGroups = size(Dev_ClustersSampleRate,dim=2)
!
!            tempRand = Dev_RandArray_SpaceDist(cid)
!
!            GroupRateTemp = 0.D0
!            exitFlag = .false.
!            DO ILayer = 1,NLayer
!
!                if(exitFlag .eq. .true.) then
!                    exit
!                end if
!
!                DO IGroup = 1,MaxGroups
!                    GroupRateTemp = GroupRateTemp + Dev_ClustersSampleRate(ILayer,IGroup)
!                    if(GroupRateTemp .GE. tempRand) then
!
!                        ICTRUE = Dev_SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox + 1 + (cid - cid0)
!
!                        !Initialize the position of clusters
!                        POS(1) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC)*dm_BOXSIZE(1)+dm_BOXBOUNDARY(1,1)
!                        POS(2) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*2)*dm_BOXSIZE(2)+dm_BOXBOUNDARY(2,1)
!                        POS(3) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*3)*Dev_LayerThick(ILayer) + sum(Dev_LayerThick(1:ILayer-1),dim=1) + dm_BOXBOUNDARY(3,1)
!                        Dev_Clusters(ICTRUE)%m_POS = POS
!
!                        !Give the cluster an type(layer) ID for the convenience of visualization
!                        Dev_Clusters(ICTRUE)%m_Layer = ILayer
!
!                        !*** Initialize the size of the clusters
!                        randSize = Dev_RandArray_SizeDist(cid)
!                        if(randSize .GT. RNACUT) then
!                            randSize = 2*RNACUT - randSize - (RNACUT-LNACUT)*floor((randSize-RNACUT)/(RNACUT-LNACUT))
!                        else if(randSize .LT. LNACUT) then
!                            randSize = 2*LNACUT - randSize - (RNACUT-LNACUT)*floor((LNACUT - randSize)/(RNACUT-LNACUT))
!                        end if
!                        DO IElement = 1,p_ATOMS_GROUPS_NUMBER
!                            Dev_Clusters(ICTRUE)%m_Atoms(IElement)%m_NA = floor(randSize*Dev_CompositWeight(IElement)+0.5D0)
!                            Dev_Clusters(ICTRUE)%m_Atoms(IElement)%m_ID = IElement
!                        END DO
!
!                        Dev_Clusters(ICTRUE)%m_GrainID(1) = GrainBelongsTo_Dev(Nseeds,Dev_GrainSeeds,POS)
!
!                        Dev_Clusters(ICTRUE)%m_Statu = p_ACTIVEFREE_STATU
!
!                        call Dev_GetValueFromDiffusorsMap(Dev_Clusters(ICTRUE),Dev_TypesEntities,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!                        !-- In Current application, the simple implant distribution is only considered in free matrix, if you want to init the clusters in GB---
!                        !---you should init the distribution by external file---
!                        select case(TheDiffusorValue%ECRValueType_Free)
!                            case(p_ECR_ByValue)
!                                Dev_Clusters(ICTRUE)%m_RAD = TheDiffusorValue%ECR_Free
!                            case default
!                                ATOMS = Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
!                                Dev_Clusters(ICTRUE)%m_RAD = Cal_ECR_ModelDataBase_Dev(TheDiffusorValue%ECRValueType_Free,                        &
!                                                                                       ATOMS,                                                     &
!                                                                                       dm_TKB,                                                    &
!                                                                                       dm_LatticeLength)
!                        end select
!
!                        select case(TheDiffusorValue%DiffusorValueType_Free)
!                            case(p_DiffuseCoefficient_ByValue)
!                                Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
!                            case(p_DiffuseCoefficient_ByArrhenius)
!                                Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                            case(p_DiffuseCoefficient_ByBCluster)
!                                ! Here we adopt a model that D=D0*(1/R)**Gama
!                                Dev_Clusters(ICTRUE)%m_DiffCoeff = dm_FREESURDIFPRE*(Dev_Clusters(ICTRUE)%m_RAD**(-p_GAMMA))
!                            case(p_DiffuseCoefficient_BySIACluster)
!                                Dev_Clusters(ICTRUE)%m_DiffCoeff = (sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_Free))* &
!                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                            case(p_DiffuseCoefficient_ByVcCluster)
!                                Dev_Clusters(ICTRUE)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
!                                                                   TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                        end select
!
!                        Dev_Clusters(ICTRUE)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
!
!                        Dev_Clusters(ICTRUE)%m_Record(1) = cid + sum(Dev_RecordNCBeforeSweepOut_SingleBox(IBox,p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU),dim=1) + &
!                                                           Dev_SEVirtualIndexBox(IBox,2)
!                        Dev_Clusters(ICTRUE)%m_Record(2) = 0
!
!
!                        exitFlag = .true.
!                        exit
!
!
!                    end if
!                END DO
!            END DO
!
!        end if
!
!        return
!    end subroutine Kernel_ImplantClusters_Depth_Layer
!
!    !*************************************************************
!    subroutine FillVirtualBoundary_GPU_Depth_SubBox(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Local Vars---
!        integer::MultiBox
!        integer::TotalAllocateNC
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::NB
!        integer::NBX,NBY
!        Integer::BX
!        integer::BY
!        integer::err
!        !---Body---
!
!        ASSOCIATE(ImplantRand=>Dev_MigCoaleGVars%dm_MigCoale_RandDev)
!
!            if(NewAllocateNCEachBox .GT. 0) then
!
!                MultiBox = Host_SimuCtrlParam%MultiBox
!
!                TotalAllocateNC = MultiBox*NewAllocateNCEachBox
!
!                NB = (TotalAllocateNC - 1)/p_BLOCKSIZE + 1
!                NBX  = min(NB,p_BLOCKDIMX)
!                NBY = (NB - 1)/NBX + 1
!
!                !*** to determine the block size
!                BX = p_BLOCKSIZE
!                BY = 1
!                !*** to determine the dimension of blocks
!
!                blocks  = dim3(NBX, NBY, 1)
!                threads = dim3(BX,  BY,  1)
!
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_X,ImplantRand%dm_SpaceDist_Implant(1:TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Y,ImplantRand%dm_SpaceDist_Implant(TotalAllocateNC+1:2*TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Z,ImplantRand%dm_SpaceDist_Implant(2*TotalAllocateNC+1:3*TotalAllocateNC),TotalAllocateNC)
!
!                err = curandGenerateNormal(ImplantRand%m_ranGen_ClustersSizeDist,ImplantRand%dm_SizeDist_Implant,TotalAllocateNC,this%NAINI,this%NASDINI)
!
!                call Kernel_ImplantClusters_Depth_SubBox<<<blocks,threads>>>(TotalAllocateNC,                                         &
!                                                                            NewAllocateNCEachBox,                                     &
!                                                                            Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                 &
!                                                                            Dev_Boxes%dm_DiffusorTypesMap%Dev_TypesEntities,          &
!                                                                            Dev_Boxes%dm_DiffusorTypesMap%Dev_SingleAtomsDivideArrays,&
!                                                                            Host_Boxes%m_GrainBoundary%GrainNum,                      &
!                                                                            Dev_Boxes%dm_GrainBoundary%dm_GrainSeeds,                 &
!                                                                            ImplantRand%dm_SpaceDist_Implant,                         &
!                                                                            ImplantRand%dm_SizeDist_Implant,                          &
!                                                                            this%NACUT(1),                                            &
!                                                                            this%NACUT(2),                                            &
!                                                                            Dev_Boxes%dm_SEVirtualIndexBox,                           &
!                                                                            Dev_Boxes%dm_RecordNCBeforeSweepOut_SingleBox,            &
!                                                                            this%dm_ImplantInfo_DevPart%Dev_SUBBOXBOUNDARY,           &
!                                                                            this%dm_ImplantInfo_DevPart%Dev_CompositWeight)
!            end if
!
!        END ASSOCIATE
!
!
!        return
!    end subroutine FillVirtualBoundary_GPU_Depth_SubBox
!
!    !**********************************************
!    attributes(global) subroutine Kernel_ImplantClusters_Depth_SubBox(TotalAllocateNC,           &
!                                                                    NewAllocateNCEachBox,        &
!                                                                    Dev_Clusters,                &
!                                                                    Dev_TypesEntities,           &
!                                                                    Dev_SingleAtomsDivideArrays, &
!                                                                    Nseeds,                      &
!                                                                    Dev_GrainSeeds,              &
!                                                                    Dev_RandArray_SpaceDist,     &
!                                                                    Dev_RandArray_SizeDist,      &
!                                                                    LNACUT,                      &
!                                                                    RNACUT,                      &
!                                                                    Dev_SEVirtualIndexBox,       &
!                                                                    Dev_RecordNCBeforeSweepOut_SingleBox, &
!                                                                    Dev_SUBBOXBOUNDARY,          &
!                                                                    Dev_CompositWeight)
!        implicit none
!        !---Dummy Vars---
!        integer, value::TotalAllocateNC
!        integer, value::NewAllocateNCEachBox
!        type(ACluster), device::Dev_Clusters(:)
!        type(DiffusorTypeEntity),device::Dev_TypesEntities(:)
!        integer,device::Dev_SingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
!        integer,value::Nseeds
!        type(GrainSeed),device::Dev_GrainSeeds(:)
!        real(kind=KINDDF),device::Dev_RandArray_SpaceDist(:)
!        real(kind=KINDDF),device::Dev_RandArray_SizeDist(:)
!        real(kind=KINDDF),value::LNACUT
!        real(kind=KINDDF),value::RNACUT
!        integer, device::Dev_SEVirtualIndexBox(:,:)
!        integer, device::Dev_RecordNCBeforeSweepOut_SingleBox(:,:)
!        real(kind=KINDDF),device::Dev_SUBBOXBOUNDARY(:,:)
!        real(kind=KINDDF),device::Dev_CompositWeight(:)
!        !---Local Vars---
!        integer::tid
!        integer::bid
!        integer::cid
!        integer::IBox
!        integer::cid0
!        integer::ICTRUE
!        integer::I
!        real(kind=KINDDF)::POS(3)
!        integer::IElement
!        type(DiffusorValue)::TheDiffusorValue
!        real(kind=KINDDF)::randSize
!        integer::ATOMS(p_ATOMS_GROUPS_NUMBER)
!        !---Body---
!        tid = (threadidx%y - 1)*blockdim%x + threadidx%x
!        bid = (blockidx%y - 1)*griddim%x + blockidx%x
!        cid = (bid - 1)*blockdim%x*blockdim%y + tid
!
!        IBox = (cid - 1)/NewAllocateNCEachBox + 1
!        cid0 = (IBox - 1)*NewAllocateNCEachBox + 1
!
!        if(cid .LE. TotalAllocateNC) then
!            ICTRUE = Dev_SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox + 1 + (cid - cid0)
!
!            call Clean_Cluster_Dev(Dev_Clusters(ICTRUE))
!
!            DO I = 1,3
!                POS(I) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*(I - 1))*(Dev_SUBBOXBOUNDARY(I,2) - Dev_SUBBOXBOUNDARY(I,1)) + Dev_SUBBOXBOUNDARY(I,1)
!            END DO
!            !Initialize the position of clusters
!            Dev_Clusters(ICTRUE)%m_POS = POS
!
!            !Give the cluster an type(layer) ID for the convenience of visualization
!            Dev_Clusters(ICTRUE)%m_Layer = 1
!
!            !*** Initialize the size of the clusters
!            randSize = Dev_RandArray_SizeDist(cid)
!            if(randSize .GT. RNACUT) then
!                randSize = 2*RNACUT - randSize - (RNACUT-LNACUT)*floor((randSize-RNACUT)/(RNACUT-LNACUT))
!            else if(randSize .LT. LNACUT) then
!                randSize = 2*LNACUT - randSize - (RNACUT-LNACUT)*floor((LNACUT - randSize)/(RNACUT-LNACUT))
!            end if
!            DO IElement = 1,p_ATOMS_GROUPS_NUMBER
!                Dev_Clusters(ICTRUE)%m_Atoms(IElement)%m_NA = floor(randSize*Dev_CompositWeight(IElement)+0.5D0)
!                Dev_Clusters(ICTRUE)%m_Atoms(IElement)%m_ID = IElement
!            END DO
!
!            Dev_Clusters(ICTRUE)%m_GrainID(1) = GrainBelongsTo_Dev(Nseeds,Dev_GrainSeeds,POS)
!
!            Dev_Clusters(ICTRUE)%m_Statu = p_ACTIVEFREE_STATU
!
!            call Dev_GetValueFromDiffusorsMap(Dev_Clusters(ICTRUE),Dev_TypesEntities,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!            !-- In Current application, the simple implant distribution is only considered in free matrix, if you want to init the clusters in GB---
!            !---you should init the distribution by external file---
!            select case(TheDiffusorValue%ECRValueType_Free)
!                case(p_ECR_ByValue)
!                    Dev_Clusters(ICTRUE)%m_RAD = TheDiffusorValue%ECR_Free
!                case default
!                    ATOMS = Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
!                    Dev_Clusters(ICTRUE)%m_RAD = Cal_ECR_ModelDataBase_Dev(TheDiffusorValue%ECRValueType_Free,                        &
!                                                                           ATOMS,                                                     &
!                                                                           dm_TKB,                                                    &
!                                                                           dm_LatticeLength)
!            end select
!
!            select case(TheDiffusorValue%DiffusorValueType_Free)
!                case(p_DiffuseCoefficient_ByValue)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
!                case(p_DiffuseCoefficient_ByArrhenius)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                case(p_DiffuseCoefficient_ByBCluster)
!                    ! Here we adopt a model that D=D0*(1/R)**Gama
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = dm_FREESURDIFPRE*(Dev_Clusters(ICTRUE)%m_RAD**(-p_GAMMA))
!                case(p_DiffuseCoefficient_BySIACluster)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = (sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_Free))* &
!                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                case(p_DiffuseCoefficient_ByVcCluster)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
!                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!            end select
!
!            Dev_Clusters(ICTRUE)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
!
!            Dev_Clusters(ICTRUE)%m_Record(1) = cid + sum(Dev_RecordNCBeforeSweepOut_SingleBox(IBox,p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU),dim=1) + &
!                                               Dev_SEVirtualIndexBox(IBox,2)
!            Dev_Clusters(ICTRUE)%m_Record(2) = 0
!
!        end if
!
!        return
!    end subroutine Kernel_ImplantClusters_Depth_SubBox
!
!    !*************************************************************
!    subroutine FillVirtualBoundary_GPU_Depth_Gauss(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Local Vars---
!        integer::MultiBox
!        integer::TotalAllocateNC
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::NB
!        integer::NBX,NBY
!        Integer::BX
!        integer::BY
!        integer::err
!        !---Body---
!        ASSOCIATE(ImplantRand=>Dev_MigCoaleGVars%dm_MigCoale_RandDev)
!
!            if(NewAllocateNCEachBox .GT. 0) then
!
!                MultiBox = Host_SimuCtrlParam%MultiBox
!
!                TotalAllocateNC = MultiBox*NewAllocateNCEachBox
!
!                NB = (TotalAllocateNC - 1)/p_BLOCKSIZE + 1
!                NBX  = min(NB,p_BLOCKDIMX)
!                NBY = (NB - 1)/NBX + 1
!
!                !*** to determine the block size
!                BX = p_BLOCKSIZE
!                BY = 1
!                !*** to determine the dimension of blocks
!
!                blocks  = dim3(NBX, NBY, 1)
!                threads = dim3(BX,  BY,  1)
!
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_X,ImplantRand%dm_SpaceDist_Implant(1:TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Y,ImplantRand%dm_SpaceDist_Implant(TotalAllocateNC+1:2*TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateNormal(ImplantRand%m_ranGen_ClustersSpaceDist_Z,ImplantRand%dm_SpaceDist_Implant(2*TotalAllocateNC+1:3*TotalAllocateNC),TotalAllocateNC,this%DepthINI,this%DepthSDINI)
!
!                err = curandGenerateNormal(ImplantRand%m_ranGen_ClustersSizeDist,ImplantRand%dm_SizeDist_Implant,TotalAllocateNC,this%NAINI,this%NASDINI)
!
!                call Kernel_ImplantClusters_Depth_Gauss<<<blocks,threads>>>(TotalAllocateNC,                                          &
!                                                                            NewAllocateNCEachBox,                                     &
!                                                                            Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                 &
!                                                                            Dev_Boxes%dm_DiffusorTypesMap%Dev_TypesEntities,          &
!                                                                            Dev_Boxes%dm_DiffusorTypesMap%Dev_SingleAtomsDivideArrays,&
!                                                                            Host_Boxes%m_GrainBoundary%GrainNum,                      &
!                                                                            Dev_Boxes%dm_GrainBoundary%dm_GrainSeeds,                 &
!                                                                            ImplantRand%dm_SpaceDist_Implant,                         &
!                                                                            ImplantRand%dm_SizeDist_Implant,                          &
!                                                                            this%NACUT(1),                                            &
!                                                                            this%NACUT(2),                                            &
!                                                                            Dev_Boxes%dm_SEVirtualIndexBox,                           &
!                                                                            Dev_Boxes%dm_RecordNCBeforeSweepOut_SingleBox,            &
!                                                                            this%dm_ImplantInfo_DevPart%Dev_CompositWeight)
!            end if
!
!        END ASSOCIATE
!
!        return
!    end subroutine FillVirtualBoundary_GPU_Depth_Gauss
!
!    !**********************************************
!    attributes(global) subroutine Kernel_ImplantClusters_Depth_Gauss(TotalAllocateNC,            &
!                                                                    NewAllocateNCEachBox,        &
!                                                                    Dev_Clusters,                &
!                                                                    Dev_TypesEntities,           &
!                                                                    Dev_SingleAtomsDivideArrays, &
!                                                                    Nseeds,                      &
!                                                                    Dev_GrainSeeds,              &
!                                                                    Dev_RandArray_SpaceDist,     &
!                                                                    Dev_RandArray_SizeDist,      &
!                                                                    LNACUT,                      &
!                                                                    RNACUT,                      &
!                                                                    Dev_SEVirtualIndexBox,       &
!                                                                    Dev_RecordNCBeforeSweepOut_SingleBox, &
!                                                                    Dev_CompositWeight)
!        implicit none
!        !---Dummy Vars---
!        integer, value::TotalAllocateNC
!        integer, value::NewAllocateNCEachBox
!        type(ACluster), device::Dev_Clusters(:)
!        type(DiffusorTypeEntity),device::Dev_TypesEntities(:)
!        integer,device::Dev_SingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
!        integer,value::Nseeds
!        type(GrainSeed),device::Dev_GrainSeeds(:)
!        real(kind=KINDDF),device::Dev_RandArray_SpaceDist(:)
!        real(kind=KINDDF),device::Dev_RandArray_SizeDist(:)
!        real(kind=KINDDF),value::LNACUT
!        real(kind=KINDDF),value::RNACUT
!        integer, device::Dev_SEVirtualIndexBox(:,:)
!        integer, device::Dev_RecordNCBeforeSweepOut_SingleBox(:,:)
!        real(kind=KINDDF),device::Dev_CompositWeight(:)
!        !---Local Vars---
!        integer::tid
!        integer::bid
!        integer::cid
!        integer::IBox
!        integer::cid0
!        integer::ICTRUE
!        integer::I
!        real(kind=KINDDF)::POS(3)
!        integer::IElement
!        type(DiffusorValue)::TheDiffusorValue
!        real(kind=KINDDF)::randSize
!        real(kind=KINDDF)::randDepth
!        integer::ATOMS(p_ATOMS_GROUPS_NUMBER)
!        !---Body---
!        tid = (threadidx%y - 1)*blockdim%x + threadidx%x
!        bid = (blockidx%y - 1)*griddim%x + blockidx%x
!        cid = (bid - 1)*blockdim%x*blockdim%y + tid
!
!        IBox = (cid - 1)/NewAllocateNCEachBox + 1
!        cid0 = (IBox - 1)*NewAllocateNCEachBox + 1
!
!        if(cid .LE. TotalAllocateNC) then
!            ICTRUE = Dev_SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox + 1 + (cid - cid0)
!
!            call Clean_Cluster_Dev(Dev_Clusters(ICTRUE))
!
!            POS(1) = Dev_RandArray_SpaceDist(cid)*dm_BOXSIZE(1)+dm_BOXBOUNDARY(1,1)
!            POS(2) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC)*dm_BOXSIZE(2)+dm_BOXBOUNDARY(2,1)
!
!            randDepth = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*2)
!            if(randDepth .GT. dm_BOXBOUNDARY(3,2)) then
!                randDepth = 2*dm_BOXBOUNDARY(3,2) - randDepth - (dm_BOXBOUNDARY(3,2)-dm_BOXBOUNDARY(3,1))*floor((randDepth-dm_BOXBOUNDARY(3,2))/(dm_BOXBOUNDARY(3,2)-dm_BOXBOUNDARY(3,1)))
!            else if(randDepth .LT. dm_BOXBOUNDARY(3,1)) then
!                randDepth = 2*dm_BOXBOUNDARY(3,1) - randDepth - (dm_BOXBOUNDARY(3,2)-dm_BOXBOUNDARY(3,1))*floor((dm_BOXBOUNDARY(3,1)-randDepth)/(dm_BOXBOUNDARY(3,2)-dm_BOXBOUNDARY(3,1)))
!            end if
!
!            POS(3) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*2)*dm_BOXSIZE(3) + dm_BOXBOUNDARY(3,1)
!            !Initialize the position of clusters
!            Dev_Clusters(ICTRUE)%m_POS = POS
!
!            !Give the cluster an type(layer) ID for the convenience of visualization
!            Dev_Clusters(ICTRUE)%m_Layer = 1
!
!            !*** Initialize the size of the clusters
!            randSize = Dev_RandArray_SizeDist(cid)
!            if(randSize .GT. RNACUT) then
!                randSize = 2*RNACUT - randSize - (RNACUT-LNACUT)*floor((randSize-RNACUT)/(RNACUT-LNACUT))
!            else if(randSize .LT. LNACUT) then
!                randSize = 2*LNACUT - randSize - (RNACUT-LNACUT)*floor((LNACUT - randSize)/(RNACUT-LNACUT))
!            end if
!            DO IElement = 1,p_ATOMS_GROUPS_NUMBER
!                Dev_Clusters(ICTRUE)%m_Atoms(IElement)%m_NA = floor(randSize*Dev_CompositWeight(IElement)+0.5D0)
!                Dev_Clusters(ICTRUE)%m_Atoms(IElement)%m_ID = IElement
!            END DO
!
!            Dev_Clusters(ICTRUE)%m_GrainID(1) = GrainBelongsTo_Dev(Nseeds,Dev_GrainSeeds,POS)
!
!            Dev_Clusters(ICTRUE)%m_Statu = p_ACTIVEFREE_STATU
!
!            call Dev_GetValueFromDiffusorsMap(Dev_Clusters(ICTRUE),Dev_TypesEntities,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!            !-- In Current application, the simple implant distribution is only considered in free matrix, if you want to init the clusters in GB---
!            !---you should init the distribution by external file---
!            select case(TheDiffusorValue%ECRValueType_Free)
!                case(p_ECR_ByValue)
!                    Dev_Clusters(ICTRUE)%m_RAD = TheDiffusorValue%ECR_Free
!                case default
!                    ATOMS = Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
!                    Dev_Clusters(ICTRUE)%m_RAD = Cal_ECR_ModelDataBase_Dev(TheDiffusorValue%ECRValueType_Free,                        &
!                                                                           ATOMS,                                                     &
!                                                                           dm_TKB,                                                    &
!                                                                           dm_LatticeLength)
!            end select
!
!            select case(TheDiffusorValue%DiffusorValueType_Free)
!                case(p_DiffuseCoefficient_ByValue)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
!                case(p_DiffuseCoefficient_ByArrhenius)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                case(p_DiffuseCoefficient_ByBCluster)
!                    ! Here we adopt a model that D=D0*(1/R)**Gama
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = dm_FREESURDIFPRE*(Dev_Clusters(ICTRUE)%m_RAD**(-p_GAMMA))
!                case(p_DiffuseCoefficient_BySIACluster)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = (sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_Free))* &
!                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                case(p_DiffuseCoefficient_ByVcCluster)
!                    Dev_Clusters(ICTRUE)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
!                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!            end select
!
!            Dev_Clusters(ICTRUE)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
!
!            Dev_Clusters(ICTRUE)%m_Record(1) = cid + sum(Dev_RecordNCBeforeSweepOut_SingleBox(IBox,p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU),dim=1) + &
!                                               Dev_SEVirtualIndexBox(IBox,2)
!            Dev_Clusters(ICTRUE)%m_Record(2) = 0
!
!        end if
!
!        return
!    end subroutine Kernel_ImplantClusters_Depth_Gauss
!
!    !*************************************************************
!    subroutine FillVirtualBoundary_GPU_FromFile(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Local Vars---
!        integer::MultiBox
!        integer::TotalAllocateNC
!        type(dim3)::blocks
!        type(dim3)::threads
!        integer::NB
!        integer::NBX,NBY
!        Integer::BX
!        integer::BY
!        integer::err
!        !---Body---
!        ASSOCIATE(ImplantRand=>Dev_MigCoaleGVars%dm_MigCoale_RandDev)
!
!            if(NewAllocateNCEachBox .GT. 0) then
!
!                MultiBox = Host_SimuCtrlParam%MultiBox
!
!                TotalAllocateNC = MultiBox*NewAllocateNCEachBox
!
!                NB = (TotalAllocateNC - 1)/p_BLOCKSIZE + 1
!                NBX  = min(NB,p_BLOCKDIMX)
!                NBY = (NB - 1)/NBX + 1
!
!                !*** to determine the block size
!                BX = p_BLOCKSIZE
!                BY = 1
!                !*** to determine the dimension of blocks
!
!                blocks  = dim3(NBX, NBY, 1)
!                threads = dim3(BX,  BY,  1)
!
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_Layer,ImplantRand%dm_SpaceDist_Implant(1:TotalAllocateNC),TotalAllocateNC)
!                err = curandGenerateUniformDouble(ImplantRand%m_ranGen_ClustersSpaceDist_X,ImplantRand%dm_SpaceDist_Implant(TotalAllocateNC+1:2*TotalAllocateNC),TotalAllocateNC)
!
!                call Kernel_ImplantClusters_FromFile<<<blocks,threads>>>(TotalAllocateNC,                                         &
!                                                                        NewAllocateNCEachBox,                                     &
!                                                                        Dev_Boxes%dm_ClusterInfo_GPU%dm_Clusters,                 &
!                                                                        Dev_Boxes%dm_DiffusorTypesMap%Dev_TypesEntities,          &
!                                                                        Dev_Boxes%dm_DiffusorTypesMap%Dev_SingleAtomsDivideArrays,&
!                                                                        Host_Boxes%m_GrainBoundary%GrainNum,                      &
!                                                                        Dev_Boxes%dm_GrainBoundary%dm_GrainSeeds,                 &
!                                                                        ImplantRand%dm_SpaceDist_Implant,                         &
!                                                                        Dev_Boxes%dm_SEVirtualIndexBox,                           &
!                                                                        this%dm_ImplantInfo_DevPart%Dev_LayerThick,               &
!                                                                        this%dm_ImplantInfo_DevPart%Dev_ClustersSample,           &
!                                                                        this%dm_ImplantInfo_DevPart%Dev_ClustersSampleRate,       &
!                                                                        Dev_Boxes%dm_RecordNCBeforeSweepOut_SingleBox)
!            end if
!
!        END ASSOCIATE
!
!        return
!    end subroutine FillVirtualBoundary_GPU_FromFile
!
!
!    !**********************************************
!    attributes(global) subroutine Kernel_ImplantClusters_FromFile(TotalAllocateNC,             &
!                                                                  NewAllocateNCEachBox,        &
!                                                                  Dev_Clusters,                &
!                                                                  Dev_TypesEntities,                &
!                                                                  Dev_SingleAtomsDivideArrays, &
!                                                                  Nseeds,                      &
!                                                                  Dev_GrainSeeds,              &
!                                                                  Dev_RandArray_SpaceDist,     &
!                                                                  Dev_SEVirtualIndexBox,       &
!                                                                  Dev_LayerThick,              &
!                                                                  Dev_ClustersSample,          &
!                                                                  Dev_ClustersSampleRate,      &
!                                                                  Dev_RecordNCBeforeSweepOut_SingleBox)
!        implicit none
!        !---Dummy Vars---
!        integer, value::TotalAllocateNC
!        integer, value::NewAllocateNCEachBox
!        type(ACluster), device::Dev_Clusters(:)
!        type(DiffusorTypeEntity),device::Dev_TypesEntities(:)
!        integer,device::Dev_SingleAtomsDivideArrays(p_ATOMS_GROUPS_NUMBER,*) ! If the two dimension array would be delivered to attributes(device), the first dimension must be known
!        integer,value::Nseeds
!        type(GrainSeed),device::Dev_GrainSeeds(:)
!        real(kind=KINDDF),device::Dev_RandArray_SpaceDist(:)
!        integer, device::Dev_SEVirtualIndexBox(:,:)
!        real(kind=KINDDF),device::Dev_LayerThick(:)
!        type(ACluster),device::Dev_ClustersSample(:,:)
!        real(kind=KINDDF),device::Dev_ClustersSampleRate(:,:)
!        integer, device::Dev_RecordNCBeforeSweepOut_SingleBox(:,:)
!        !---Local Vars---
!        integer::tid
!        integer::bid
!        integer::cid
!        integer::IBox
!        integer::cid0
!        integer::ICTRUE
!        real(kind=KINDDF)::POS(3)
!        integer::NLayer
!        integer::MaxGroups
!        integer::ILayer
!        integer::IGroup
!        logical::exitFlag
!        real(kind=KINDDF)::tempRand
!        real(kind=KINDDF)::GroupRateTemp
!        type(DiffusorValue)::TheDiffusorValue
!        integer::ATOMS(p_ATOMS_GROUPS_NUMBER)
!        !---Body---
!        tid = (threadidx%y - 1)*blockdim%x + threadidx%x
!        bid = (blockidx%y - 1)*griddim%x + blockidx%x
!        cid = (bid - 1)*blockdim%x*blockdim%y + tid
!
!        IBox = (cid - 1)/NewAllocateNCEachBox + 1
!        cid0 = (IBox - 1)*NewAllocateNCEachBox + 1
!
!        if(cid .LE. TotalAllocateNC) then
!            NLayer = size(Dev_ClustersSampleRate,dim=1)
!            MaxGroups = size(Dev_ClustersSampleRate,dim=2)
!
!            tempRand = Dev_RandArray_SpaceDist(cid)
!
!            GroupRateTemp = 0.D0
!            exitFlag = .false.
!            DO ILayer = 1,NLayer
!
!                if(exitFlag .eq. .true.) then
!                    exit
!                end if
!
!                DO IGroup = 1,MaxGroups
!                    GroupRateTemp = GroupRateTemp + Dev_ClustersSampleRate(ILayer,IGroup)
!                    if(GroupRateTemp .GE. tempRand) then
!
!                        ICTRUE = Dev_SEVirtualIndexBox(IBox,2) - NewAllocateNCEachBox + 1 + (cid - cid0)
!
!                        call Clean_Cluster_Dev(Dev_Clusters(ICTRUE))
!
!                        Dev_Clusters(ICTRUE)%m_Atoms = Dev_ClustersSample(ILayer,IGroup)%m_Atoms
!
!                        !Initialize the position of clusters
!                        POS(1) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC)*dm_BOXSIZE(1)+dm_BOXBOUNDARY(1,1)
!                        POS(2) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*2)*dm_BOXSIZE(2)+dm_BOXBOUNDARY(2,1)
!                        POS(3) = Dev_RandArray_SpaceDist(cid + TotalAllocateNC*3)*Dev_LayerThick(ILayer) + sum(Dev_LayerThick(1:ILayer-1),dim=1) + dm_BOXBOUNDARY(3,1)
!                        Dev_Clusters(ICTRUE)%m_POS = POS
!
!                        !Give the cluster an type(layer) ID for the convenience of visualization
!                        Dev_Clusters(ICTRUE)%m_Layer = ILayer
!
!                        Dev_Clusters(ICTRUE)%m_Statu = Dev_ClustersSample(ILayer,IGroup)%m_Statu
!
!                        call Dev_GetValueFromDiffusorsMap(Dev_Clusters(ICTRUE),Dev_TypesEntities,Dev_SingleAtomsDivideArrays,TheDiffusorValue)
!
!                        if(Dev_Clusters(ICTRUE)%m_Statu .eq. p_ACTIVEFREE_STATU) then
!
!                            Dev_Clusters(ICTRUE)%m_GrainID(1) = GrainBelongsTo_Dev(Nseeds,Dev_GrainSeeds,POS)
!
!                            select case(TheDiffusorValue%ECRValueType_Free)
!                                case(p_ECR_ByValue)
!                                    Dev_Clusters(ICTRUE)%m_RAD = TheDiffusorValue%ECR_Free
!                                case default
!                                    ATOMS = Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
!                                    Dev_Clusters(ICTRUE)%m_RAD = Cal_ECR_ModelDataBase_Dev(TheDiffusorValue%ECRValueType_Free,                        &
!                                                                                           ATOMS,                                                     &
!                                                                                           dm_TKB,                                                    &
!                                                                                           dm_LatticeLength)
!                            end select
!
!                            select case(TheDiffusorValue%DiffusorValueType_Free)
!                                case(p_DiffuseCoefficient_ByValue)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
!                                case(p_DiffuseCoefficient_ByArrhenius)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                                case(p_DiffuseCoefficient_ByBCluster)
!                                    ! Here we adopt a model that D=D0*(1/R)**Gama
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = dm_FREESURDIFPRE*(Dev_Clusters(ICTRUE)%m_RAD**(-p_GAMMA))
!                                case(p_DiffuseCoefficient_BySIACluster)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = (sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_Free))* &
!                                                                       TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                                case(p_DiffuseCoefficient_ByVcCluster)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
!                                                                       TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/dm_TKB)
!                            end select
!
!                            Dev_Clusters(ICTRUE)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
!
!                        else if(Dev_Clusters(ICTRUE)%m_Statu .eq. p_ACTIVEINGB_STATU) then
!
!                            Dev_Clusters(ICTRUE)%m_GrainID = Dev_ClustersSample(ILayer,IGroup)%m_GrainID
!
!                            select case(TheDiffusorValue%ECRValueType_InGB)
!                                case(p_ECR_ByValue)
!                                    Dev_Clusters(ICTRUE)%m_RAD = TheDiffusorValue%ECR_InGB
!                                case default
!                                    ATOMS = Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
!                                    Dev_Clusters(ICTRUE)%m_RAD = Cal_ECR_ModelDataBase_Dev(TheDiffusorValue%ECRValueType_InGB,                        &
!                                                                                           ATOMS,                                                     &
!                                                                                           dm_TKB,                                                    &
!                                                                                           dm_LatticeLength)
!                            end select
!
!                            select case(TheDiffusorValue%DiffusorValueType_InGB)
!                                case(p_DiffuseCoefficient_ByValue)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_InGB_Value
!                                case(p_DiffuseCoefficient_ByArrhenius)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
!                                case(p_DiffuseCoefficient_ByBCluster)
!                                    ! Here we adopt a model that D=D0*(1/R)**Gama
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = dm_GBSURDIFPRE*(Dev_Clusters(ICTRUE)%m_RAD**(-p_GAMMA))
!                                case(p_DiffuseCoefficient_BySIACluster)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = (sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)**(-TheDiffusorValue%PreFactorParameter_InGB))* &
!                                                                       TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
!                                case(p_DiffuseCoefficient_ByVcCluster)
!                                    Dev_Clusters(ICTRUE)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_InGB)**(1-sum(Dev_Clusters(ICTRUE)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA,dim=1)))* &
!                                                                       TheDiffusorValue%PreFactor_InGB*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_InGB/dm_TKB)
!                            end select
!                        end if
!
!                        Dev_Clusters(ICTRUE)%m_Record(1) = cid + sum(Dev_RecordNCBeforeSweepOut_SingleBox(IBox,p_OUT_DESTROY_STATU:p_ANNIHILATE_STATU),dim=1) + &
!                                                           Dev_SEVirtualIndexBox(IBox,2)
!                        Dev_Clusters(ICTRUE)%m_Record(2) = 0
!
!                        exitFlag = .true.
!                        exit
!
!                    end if
!                END DO
!            END DO
!
!        end if
!
!        return
!    end subroutine Kernel_ImplantClusters_FromFile
!
!    !*************************************************************
!    subroutine FillVirtualBoundary_GPU_FromExteFunc(this,Host_Boxes,Host_SimuCtrlParam,Dev_Boxes,Dev_MigCoaleGVars,NewAllocateNCEachBox)
!        implicit none
!        !---Dummy Vars---
!        CLASS(ImplantSection)::this
!        type(SimulationBoxes)::Host_Boxes
!        type(SimulationCtrlParam)::Host_SimuCtrlParam
!        type(SimulationBoxes_GPU)::Dev_Boxes
!        type(MigCoale_GVarsDev)::Dev_MigCoaleGVars
!        integer,intent(in)::NewAllocateNCEachBox
!        !---Body---
!
!        return
!    end subroutine FillVirtualBoundary_GPU_FromExteFunc

end module INLET_TYPEDEF_IMPLANTSECTION
