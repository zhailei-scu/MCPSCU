module MC_TYPEDEF_IMPLANTATIONSECTION
    use cudafor
    use MCLIB_CONSTANTS_GPU
    use MCLIB_TYPEDEF_BASICRECORD
    use MCLIB_TYPEDEF_ACLUSTER
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

    integer,parameter,private::p_ImplantModelType_Continue = 0      ! the implant cluster number is a line function of evolution time
    integer,parameter,private::p_ImplantModelType_Batch = 1         ! the implant cluster number is a step function of evolution time



    character(len=11),private,parameter::m_IMPFINPUTF = "&IMPFINPUTF"

    ! Note: This is DISTOKMC18 different with OKMC_OUTCFG_FORMAT18, the main different is cause that for implant,
    !       what we need for the implant source is a discrete distribution(size and space), not a special configuration.
    !       So, to use the OKMC configuration for implant, we need to convert the OKMC configuration to a discrete distribution.
    !       However, for the mean-filed result, it is just the distribution that we need, so we need not to do any other things.
    character(len=11),parameter::OKMC_DIST_FORMAT18 = "&DISTOKMC18"
    character(len=9),private,parameter::SRIM_DIST = "&DISTSRIM"
    character(len=10),private,parameter::PANDA_DIST = "&DISTPANDA"

    character(len=10),parameter::OKMC_CFG_FORMAT18 = "&CFGOKMC18"
    character(len=8), parameter::SRIM_CFG = "&CFGSRIM"
    character(len=12),parameter::MARLOWER_CFG = "&CFGMARLOWER"
    character(len=6), parameter::MD_CFG = "&CFGMD"

    integer,private,parameter::p_Implant_Hunger = 0
    integer,private,parameter::p_Implant_MemSaving = 1

    integer, parameter, private::p_ImplantConfig_Simple = 0
    integer, parameter, private::p_ImplantConfig_SpecialDistFromFile = 1
    integer, parameter, private::p_ImplantConfig_SpecialDistFromExteFunc = 2

    type,public::ImplantInfo_DevPart

        logical::InitFlag = .false.    ! the flag to record if the data structure had been initialization

        real(kind=KINDDF),device,allocatable,dimension(:)::Dev_CompositWeight

        real(kind=KINDDF),device,allocatable,dimension(:,:)::Dev_SUBBOXBOUNDARY

        real(kind=KINDDF),device,dimension(:),allocatable::Dev_LayerThick

        type(ACluster),device,dimension(:,:),allocatable::Dev_ClustersSample

        real(kind=KINDDF),device,dimension(:,:),allocatable::Dev_ClustersSampleRate

        contains
        procedure,non_overridable,public,pass::CopyImplantInfo_DevPartFromOther
        procedure,non_overridable,public,pass::Clean=>Clean_ImplantInfo_DevPart
        Generic::ASSIGNMENT(=)=>CopyImplantInfo_DevPartFromOther
        Final::CleanImplantInfo_DevPart
    end type ImplantInfo_DevPart


    type,public::ImplantSection

        integer::MemoryOccupyFactor = 10
        integer::ExpandFactor = 10

        integer::ImplantConfigType = -1

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

        type(ImplantInfo_DevPart)::dm_ImplantInfo_DevPart

        contains
        procedure,non_overridable,public,pass::Load_ImplantSection
        procedure,non_overridable,public,pass::InitImplantInfo_DevPart
        procedure,non_overridable,public,pass::ReadImplantSection
        procedure,non_overridable,public,pass::ReadImplantSection_Simple
        procedure,non_overridable,public,pass::ReadImplantClusterSizeDist_Simple
        procedure,non_overridable,public,pass::ReadImplantClusterDepthDist_Simple
        procedure,non_overridable,public,pass::ReadImplantSection_SpecialDistFromFile
        procedure,non_overridable,public,pass::Putin_PANDA_OUTCFG_Distribution
        procedure,non_overridable,public,pass::Putin_SRIM2003_OUTCFG_Distribution
        procedure,non_overridable,public,pass::Putin_OKMC_FORMAT18_Distribution
        procedure,non_overridable,public,pass::ReadImplantSection_SpecialDistFromExteFunc
        procedure,non_overridable,public,pass::CopyImplantSectionFromOther
        procedure,non_overridable,public,pass::Clean=>Clean_ImplantSection
        Generic::ASSIGNMENT(=)=>CopyImplantSectionFromOther
        Final::CleanImplantSection
    end type ImplantSection

    type,public::ImplantList
        type(ImplantSection)::TheImplantSection

        type(ImplantList),pointer::next=>null()

        integer::ListCount = 0

        contains
        procedure,non_overridable,public,pass::Init=>Init_ImplantList
        procedure,non_overridable,private,pass::Load_ImplantList
        procedure,non_overridable,private,pass::CheckImplantList
        procedure,non_overridable,public,pass::AppendOneSection=>AppendOne_ImplantSection
        procedure,non_overridable,public,pass::Get_P=>GetImplantSection_P
        procedure,non_overridable,public,pass::Clean=>Clean_ImplantList
        Final::CleanImplantList
    end type

    private::CopyImplantInfo_DevPartFromOther
    private::Clean_ImplantInfo_DevPart
    private::CleanImplantInfo_DevPart
    private::Load_ImplantSection
    private::InitImplantInfo_DevPart
    private::ReadImplantSection
    private::ReadImplantSection_Simple
    private::ReadImplantClusterSizeDist_Simple
    private::ReadImplantClusterDepthDist_Simple
    private::ReadImplantSection_SpecialDistFromFile
    private::Putin_PANDA_OUTCFG_Distribution
    private::Putin_SRIM2003_OUTCFG_Distribution
    private::Putin_OKMC_FORMAT18_Distribution
    private::ReadImplantSection_SpecialDistFromExteFunc
    private::CopyImplantSectionFromOther
    private::Clean_ImplantSection
    private::CleanImplantSection
    private::Init_ImplantList
    private::Load_ImplantList
    private::CheckImplantList
    private::AppendOne_ImplantSection
    private::GetImplantSection_P
    private::Clean_ImplantList
    private::CleanImplantList


    contains

    !********************************************************************
    subroutine CopyImplantSectionFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection),intent(out)::this
        type(ImplantSection),intent(in)::other
        !---Local Vars---
        integer::I
        !---Body---

        this%MemoryOccupyFactor = other%MemoryOccupyFactor

        this%ExpandFactor = other%ExpandFactor

        this%ImplantConfigType =other%ImplantConfigType

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

        !---The assignment(=) had been override
        this%dm_ImplantInfo_DevPart = other%dm_ImplantInfo_DevPart

        return
    end subroutine CopyImplantSectionFromOther

    !*********************************************************************
    subroutine Clean_ImplantSection(this)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        !---Body---
        this%MemoryOccupyFactor = 100
        this%ExpandFactor = 10

        this%ImplantConfigType = -1

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

        call this%dm_ImplantInfo_DevPart%Clean()
        return
    end subroutine Clean_ImplantSection

    !*********************************************************************
    subroutine CleanImplantSection(this)
        implicit none
        !---Dummy Vars---
        type(ImplantSection)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanImplantSection

    !********************For type ImplantInfo_DevPart**********************
    subroutine CopyImplantInfo_DevPartFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantInfo_DevPart),intent(out)::this
        TYPE(ImplantInfo_DevPart),intent(in)::other
        !---Local Vars--
        integer::err
        !---Body---

        call DeAllocateArray_GPU(this%Dev_CompositWeight,"Dev_CompositWeight")
        call AllocateArray_GPU(this%Dev_CompositWeight,p_ATOMS_GROUPS_NUMBER,"Dev_CompositWeight")
        err = cudaMemcpy(this%Dev_CompositWeight,other%Dev_CompositWeight,size(other%Dev_CompositWeight),cudaMemcpyDeviceToDevice)

        call DeAllocateArray_GPU(this%Dev_SUBBOXBOUNDARY,"Dev_SUBBOXBOUNDARY")
        call AllocateArray_GPU(this%Dev_SUBBOXBOUNDARY,3,2,"Dev_SUBBOXBOUNDARY")
        err = cudaMemcpy(this%Dev_SUBBOXBOUNDARY,other%Dev_SUBBOXBOUNDARY,size(other%Dev_SUBBOXBOUNDARY),cudaMemcpyDeviceToDevice)

        call DeAllocateArray_GPU(this%Dev_LayerThick,"Dev_LayerThick")
        call AllocateArray_GPU(this%Dev_LayerThick,size(other%Dev_LayerThick),"Dev_LayerThick")
        err = cudaMemcpy(this%Dev_LayerThick,other%Dev_LayerThick,size(other%Dev_LayerThick),cudaMemcpyDeviceToDevice)

        call DeAllocateArray_GPU(this%Dev_ClustersSample,"Dev_ClustersSample")
        call AllocateArray_GPU(this%Dev_ClustersSample,size(other%Dev_ClustersSample,dim=1),size(other%Dev_ClustersSample,dim=2),"Dev_ClustersSample")
        call copyClustersDevToDevSync2D(other%Dev_ClustersSample,this%Dev_ClustersSample,size(other%Dev_ClustersSample))

        call DeAllocateArray_GPU(this%Dev_ClustersSampleRate,"Dev_ClustersSampleRate")
        call AllocateArray_GPU(this%Dev_ClustersSampleRate,size(other%Dev_ClustersSampleRate,dim=1),size(other%Dev_ClustersSampleRate,dim=2),"Dev_ClustersSampleRate")
        err = cudaMemcpy(this%Dev_ClustersSampleRate,other%Dev_ClustersSampleRate,size(other%Dev_ClustersSampleRate),cudaMemcpyDeviceToDevice)

        return
    end subroutine

    !**********************************************************************
    subroutine Clean_ImplantInfo_DevPart(this)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantInfo_DevPart)::this
        !---Body---

        call DeAllocateArray_GPU(this%Dev_CompositWeight,"Dev_CompositWeight")

        call DeAllocateArray_GPU(this%Dev_SUBBOXBOUNDARY,"Dev_SUBBOXBOUNDARY")

        call DeAllocateArray_GPU(this%Dev_LayerThick,"Dev_LayerThick")

        call DeAllocateArray_GPU(this%Dev_ClustersSample,"Dev_ClustersSample")

        call DeAllocateArray_GPU(this%Dev_ClustersSampleRate,"Dev_ClustersSampleRate")

    end subroutine

    !**********************************************************************
    subroutine CleanImplantInfo_DevPart(this)
        implicit none
        !---Dummy Vars---
        TYPE(ImplantInfo_DevPart)::this
        !---Body---

        call this%Clean()
        return
    end subroutine

    !***************For type ImplantList************************************
    subroutine Init_ImplantList(this,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantList)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Body---

        call this%Clean()

        call this%Load_ImplantList(Host_Boxes,Host_SimuCtrlParam)

        call this%CheckImplantList(Host_SimuCtrlParam)

        return
    end subroutine

    !***********************************************************************
    subroutine CheckImplantList(this,Host_SimuCtrlParam)
         implicit none
        !---Dummy Vars---
        CLASS(ImplantList),target::this
        type(SimulationCtrlParam),target::Host_SimuCtrlParam
        !---Local Vars---
        integer::I
        !---Body---
!        DO I = 1,Host_SimuCtrlParam%NImplantSection
!            if(Host_SimuCtrlParam%ImplantSectIDs(I) .GT. this%ListCount) then
!                write(*,*) "MCPSCUERROR: The implantation section is not special :",Host_SimuCtrlParam%ImplantSectIDs(I)
!                pause
!                stop
!            end if
!
!        END DO

        return
    end subroutine

    !***********************************************************************
    subroutine Load_ImplantList(this,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantList)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        type(ImplantSection)::tempImplantSection
        character*1000::truePath
        character*1000::STR
        character*32::KEYWORD
        integer::hFile
        integer::LINE
        !---Body---
        LINE = 0

        truePath = INQUIREFILE(Host_SimuCtrlParam%ImpFile)

        hFile = OpenExistedFile(truePath)

        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        if(.not. ISSTREQUAL(KEYWORD,m_IMPFINPUTF)) then
            write(*,*) "MCPSCUERROR: Unknown file header: ",KEYWORD
            write(*,*) "In file: ",truePath
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
                case("&ENDIMPFINPUTF")
                    exit
                case("&GROUPSUBCTL")
                    call tempImplantSection%Clean()
                    call tempImplantSection%Load_ImplantSection(hFile,Host_Boxes,Host_SimuCtrlParam,LINE)

                    call this%AppendOneSection(tempImplantSection)
                case default
                    write(*,*) "MCPSCUERROR: Unknown flag: ",KEYWORD
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
            end select

        END DO

        return

        100 write(*,*) "MCPSCUERROR: Fail to read the file: ",truePath
            write(*,*) "At Line: ",LINE
            pause
            stop
    end subroutine Load_ImplantList

    !***********************************************************************
    subroutine AppendOne_ImplantSection(this,TheImplantSection)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantList),target::this
        type(ImplantSection)::TheImplantSection
        !---Local Vars---
        type(ImplantList),pointer::cursor=>null()
        type(ImplantList),pointer::next=>null()
        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: You should allocate the ImplantList first!"
            pause
            stop
        end if

        if(this%ListCount .eq. 0) then
            ! The assignment(=) had been override
            this%TheImplantSection = TheImplantSection
        else
            cursor=>this
            next=>cursor%next

            Do While(associated(next))
                cursor=>next
                next=>cursor%next
            End Do

            allocate(next)
            ! The assignment(=) had been override
            next%TheImplantSection = TheImplantSection
            Nullify(next%next)
            cursor%next=>next
        end if

        this%ListCount = this%ListCount + 1

        return
    end subroutine

    !***********************************************************************
    function GetImplantSection_P(this,TheIndex) result(TheResult)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantList),target::this
        integer,intent(in)::TheIndex
        type(ImplantSection),intent(out),pointer::TheResult
        !---Local Vars---
        type(ImplantList),pointer::cursor=>null()
        integer::CountTemp
        !---Body---

        TheResult=>null()

        cursor=>this

        CountTemp = 0

        DO While(associated(cursor))


            CountTemp = CountTemp + 1

            if(CountTemp .eq. TheIndex) then
                TheResult=>cursor%TheImplantSection
                exit
            end if

            cursor=>cursor%next
        END DO

        Nullify(cursor)

        cursor=>null()

        if(.not. associated(TheResult)) then
            write(*,*) "MCPSCUERROR: Cannot find the Implantation section by the id: ",TheIndex
            pause
            stop
        end if

        return
    end function GetImplantSection_P


    !***********************************************************************
    subroutine Clean_ImplantList(this)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantList),target::this
        !---Local Vars---
        type(ImplantList),pointer::cursor=>null()
        type(ImplantList),pointer::next=>null()
        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        cursor=>this%next

        call this%TheImplantSection%Clean()

        Do while(associated(cursor))
            next=>cursor%next
            call cursor%TheImplantSection%Clean()
            cursor%next=>null()
            deallocate(cursor)
            Nullify(cursor)
            cursor=>next
        End Do

        this%next=>null()

        this%ListCount = 0
        Nullify(cursor)
        cursor=>null()
        Nullify(next)
        next=>null()

        return
    end subroutine Clean_ImplantList

    !***********************************************************************
    subroutine CleanImplantList(this)
        implicit none
        !---Dummy Vars---
        type(ImplantList)::this
        !---Body---

        call this%Clean()
        return
    end subroutine

    !*****************For Type ImplantSection****************************
    subroutine Load_ImplantSection(this,hFile,SimBoxes,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer, intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*20::STRTMP(10)
        integer::N
        real(kind=KINDDF)::ReflectRatio
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
                case("&TYPE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for implantation distribution type."
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way : &TYPE The implantation cluster distribution type =  "
                        pause
                        stop
                    end if
                    this%ImplantConfigType = ISTR(STRTMP(1))
                    exit
                case default
                    write(*,*) "MCPSCUERROR: You must special the implantation distribution type first!"
                    write(*,*) "By the way: &TYPE The implantation cluster distribution type = "
                    write(*,*) "However, the words you input is: ",STR
                    pause
                    stop
            end select
        End Do

        Do While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit

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

                case("&SIZESUBCTL","&DEPTHSUBCTL","&EXTFSUBCTL")
                    call this%ReadImplantSection(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)

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

    !****************************************************************
    subroutine InitImplantInfo_DevPart(this)
        implicit none
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        !---Body---
        if(this%dm_ImplantInfo_DevPart%InitFlag .eq. .false.) then

            this%dm_ImplantInfo_DevPart%InitFlag = .true.

            call AllocateArray_GPU(this%dm_ImplantInfo_DevPart%Dev_CompositWeight,size(this%CompositWeight),"Dev_CompositWeight")
            this%dm_ImplantInfo_DevPart%Dev_CompositWeight = this%CompositWeight

            call AllocateArray_GPU(this%dm_ImplantInfo_DevPart%Dev_SUBBOXBOUNDARY,size(this%SUBBOXBOUNDARY,DIM=1),size(this%SUBBOXBOUNDARY,DIM=2),"Dev_SUBBOXBOUNDARY")
            this%dm_ImplantInfo_DevPart%Dev_SUBBOXBOUNDARY = this%SUBBOXBOUNDARY

            call AllocateArray_GPU(this%dm_ImplantInfo_DevPart%Dev_LayerThick,size(this%LayerThick),"Dev_LayerThick")
            this%dm_ImplantInfo_DevPart%Dev_LayerThick = this%LayerThick

            call AllocateArray_GPU(this%dm_ImplantInfo_DevPart%Dev_ClustersSample,size(this%ClustersSample,dim=1),size(this%ClustersSample,dim=2),"Dev_ClustersSample")
            call copyInClustersSync2D(this%ClustersSample,this%dm_ImplantInfo_DevPart%Dev_ClustersSample,size(this%ClustersSample))

            call AllocateArray_GPU(this%dm_ImplantInfo_DevPart%Dev_ClustersSampleRate,size(this%ClustersSampleRate,dim=1),size(this%ClustersSampleRate,dim=2),"Dev_ClustersSampleRate")
            this%dm_ImplantInfo_DevPart%Dev_ClustersSampleRate = this%ClustersSampleRate

        end if

        return
    end subroutine InitImplantInfo_DevPart

    !***************************************************************
    subroutine ReadImplantSection(this,hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
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
                call this%ReadImplantSection_Simple(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
            case(p_ImplantConfig_SpecialDistFromFile)
                call this%ReadImplantSection_SpecialDistFromFile(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
            case(p_ImplantConfig_SpecialDistFromExteFunc)
                call this%ReadImplantSection_SpecialDistFromExteFunc(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
            case default
                write(*,*) "MCPSCUERROR: Unknown strategy for the implantation configuration:",this%ImplantConfigType
                pause
                stop
        end select

        return
    end subroutine ReadImplantSection

    !*****************************************************************
    subroutine ReadImplantSection_Simple(this,hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
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
    end subroutine ReadImplantSection_Simple

    !****************************************************************
    subroutine ReadImplantSection_SpecialDistFromFile(this,hFile,PreKEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        integer,intent(in)::hFile
        character*(*)::PreKEYWORD
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Local Vars---
        character*1000::STR
        character*32::KEYWORD
        character*1000::STRTEMP(1)
        integer::N
        type(MigCoalClusterRecord)::tempRecord
        real(kind=KINDDF)::TotalSampleRate
        character*1000::FolderPath
        integer::LayerNum
        real(kind=KINDDF),dimension(:),allocatable::LayersThick
        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleConcentrate
        type(ACluster),dimension(:,:),allocatable::ClustersSample
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

                    if(ISSTREQUAL(trim(this%ImplantCfgFileType),SRIM_DIST) .or. ISSTREQUAL( trim(this%ImplantCfgFileType),OKMC_DIST_FORMAT18)) then
                        call EXTRACT_NUMB(STR,1,N,STRTEMP)

                        if(N .LT. 1) then
                            write(*,*) "MCPSCUERROR: when the specialized distribution type is 'DISTSRIM' or 'DISTOKMC18'"
                            write(*,*) "MCPSCUERROR: you must special the layer number that you want to divide."
                            pause
                            stop
                        end if

                        LayerNum = ISTR(STRTEMP(1))

                        if(LayerNum .LE. 0) then
                            write(*,*) "MCPSCUERROR: the total layer number cannot be less than 0 when it is set for SRIM or OKMC18 distribution"
                            pause
                            stop
                        end if
                    end if


                case("&DISTFOLDER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTEMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the implantation configuration file if you had chosen the file model."
                        write(*,*) "By the way: &DISTFILE The distribution file path = "
                        pause
                        stop
                    end if

                    if(LENTRIM(STRTEMP(1)) .LE. 0) then
                        write(*,*) "MCPSCUERROR: The implant configuration file name is null."
                        write(*,*) "At line: ",LINE
                        pause
                        stop
                    end if

                    FolderPath = INQUIREFILE(STRTEMP(1),Host_SimuCtrlParam%InputFilePath)

                    if(.not. associated(this%ImplantCfgFileList)) then
                        allocate(this%ImplantCfgFileList)
                    end if
                    call ListFilesInFolder(FolderPath,this%ImplantCfgFileList)

                case default
                    write(*,*) "MCPSCUERROR: Illegal flag: ",KEYWORD
                    pause
                    stop
            end select
        END DO

        cursor=>this%ImplantCfgFileList
        select case(adjustl(trim(this%ImplantCfgFileType)))
            case(MF_OUTCFG_FORMAT18)
                DO While(associated(cursor))
                    call SimBoxes%Putin_MF_OUTCFG_FORMAT18_Distribution(Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),LayersThick,ClustersSampleConcentrate,ClustersSample,tempRecord,m_FREESURDIFPRE)

                    TotalSampleRate = sum(ClustersSampleConcentrate)
                    if(TotalSampleRate .LE. 0) then
                        write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
                        write(*,*) "In file: ",cursor%TheValue
                        pause
                        stop
                    end if
                    ClustersSampleConcentrate = ClustersSampleConcentrate/TotalSampleRate

                    cursor=>cursor%Next
                END DO

            case(SPMF_OUTCFG_FORMAT18)
                DO While(associated(cursor))
                    call SimBoxes%Putin_SPMF_OUTCFG_FORMAT18_Distribution(Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),LayersThick,ClustersSampleConcentrate,ClustersSample,tempRecord,m_FREESURDIFPRE,m_GBSURDIFPRE)
                    cursor=>cursor%Next
                END DO

            case(SRIM_DIST)
                DO While(associated(cursor))
                    call this%Putin_SRIM2003_OUTCFG_Distribution(SimBoxes,Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),LayerNum,LayersThick,ClustersSampleConcentrate,ClustersSample)
                    cursor=>cursor%Next
                END DO

            case(PANDA_DIST)
                DO While(associated(cursor))
                    call this%Putin_PANDA_OUTCFG_Distribution(SimBoxes,Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),LayersThick,ClustersSampleConcentrate,ClustersSample)
                    cursor=>cursor%Next
                END DO

            case(OKMC_DIST_FORMAT18)
                DO While(associated(cursor))
                    call this%Putin_OKMC_FORMAT18_Distribution(SimBoxes,Host_SimuCtrlParam,adjustl(trim(cursor%TheValue)),LayerNum,LayersThick,ClustersSampleConcentrate,ClustersSample)
                    cursor=>cursor%Next
                END DO

            case default
                write(*,*) "MCPSCUERROR: Unknown Implant Configuration file type : ",this%ImplantCfgFileType
                write(*,*) "In current version, only the ", &
                            OKMC_DIST_FORMAT18," ",         &
                            MF_OUTCFG_FORMAT18," ",         &
                            SPMF_OUTCFG_FORMAT18," ",       &
                            SRIM_DIST," ",                  &
                            PANDA_DIST," ",                 &
                            "are supported. "
                pause
                stop
        end select

        ! Note, the out put SampleRate may be the concentrate, we need to convert it to rate now.
        TotalSampleRate = sum(this%ClustersSampleRate)
        if(TotalSampleRate .LE. 0) then
            write(*,*) "MCPSCUERROR: The total concentrate cannot less equal with 0"
            pause
            stop
        end if
        this%ClustersSampleRate = this%ClustersSampleRate/TotalSampleRate

        return
        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadImplantSection_SpecialDistFromFile

    !*****************************************************************
    subroutine Putin_PANDA_OUTCFG_Distribution(this,SimBoxes,Host_SimuCtrlParam,cfgFile,LayersThick,ClustersSampleConcentrate,ClustersSample)
        !---Dummy Vars---
        CLASS(ImplantSection)::this
        type(SimulationBoxes),intent(in)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        character*(*),intent(in)::cfgFile
        real(kind=KINDDF),dimension(:),allocatable::LayersThick
        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleConcentrate
        type(ACluster),dimension(:,:),allocatable::ClustersSample
        !---Local Vars---
        integer::hFile
        integer::LINE
        integer::N
        character*1000::STR
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
                ImplantIon%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-CP_EVERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                ImplantIon%m_DiffCoeff = m_FREESURDIFPRE*(ImplantIon%m_RAD**(-p_GAMMA))
            case(p_DiffuseCoefficient_BySIACluster)
                ImplantIon%m_DiffCoeff = (sum(ImplantIon%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-CP_EVERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByVcCluster)
                ImplantIon%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(ImplantIon%m_Atoms(:)%m_NA)))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-CP_EVERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
        end select

        ImplantIon%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

        if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
            ImplantIon%m_DiffCoeff =  ImplantIon%m_DiffCoeff*1.D0/3.D0     ! All Diffusion coeff would be changed to 3-D formation
        end if

        ImplantIon%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-CP_EVERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParam%TKB)

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

            LayersThick(ILayer) = 2*(DRSTR(STRTMP(1))*CP_UM2CM - SumOfThick)
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
        character*(*),intent(in)::cfgFile
        integer,intent(in)::LayerNum
        real(kind=KINDDF),dimension(:),allocatable::LayersThick
        real(kind=KINDDF),dimension(:,:),allocatable::ClustersSampleConcentrate
        type(ACluster),dimension(:,:),allocatable::ClustersSample
        !---Local Vars---
        integer::hFile
        integer::LINE
        integer::N
        character*1000::STR
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

                StoppedPosition(IIon,1) = DRSTR(STRTMP(2))*CP_A2CM ! depth   X
                StoppedPosition(IIon,2) = DRSTR(STRTMP(3))*CP_A2CM ! lateral Y
                StoppedPosition(IIon,3) = DRSTR(STRTMP(4))*CP_A2CM ! lateral Z

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
                ImplantIon%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-CP_EVERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                ImplantIon%m_DiffCoeff = m_FREESURDIFPRE*(ImplantIon%m_RAD**(-p_GAMMA))
            case(p_DiffuseCoefficient_BySIACluster)
                ImplantIon%m_DiffCoeff = (sum(ImplantIon%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-CP_EVERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByVcCluster)
                ImplantIon%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(ImplantIon%m_Atoms(:)%m_NA)))* &
                                         TheDiffusorValue%PreFactor_Free*exp(-CP_EVERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParam%TKB)
        end select

        ImplantIon%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection

        if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
            ImplantIon%m_DiffCoeff =  ImplantIon%m_DiffCoeff*1.D0/3.D0     ! All Diffusion coeff would be changed to 3-D formation
        end if

        ImplantIon%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-CP_EVERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParam%TKB)

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
        character*(*),intent(in)::cfgFile
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
        character*1000::STR
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
                        this%SUBBOXBOUNDARY(I,1) = Host_SimBoxes%BOXBOUNDARY(I,1) - DRSTR(STRTMP(I))*CP_NM2CM/2
                        this%SUBBOXBOUNDARY(I,2) = Host_SimBoxes%BOXBOUNDARY(I,2) + DRSTR(STRTMP(I))*CP_NM2CM/2
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
                    this%DepthINI = DRSTR(STRTMP(1))*CP_NM2CM
                    this%DepthSDINI = DRSTR(STRTMP(2))*CP_NM2CM
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

end module MC_TYPEDEF_IMPLANTATIONSECTION
