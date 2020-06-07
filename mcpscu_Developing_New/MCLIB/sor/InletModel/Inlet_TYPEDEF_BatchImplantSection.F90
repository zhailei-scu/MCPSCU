module INLET_BATCHIMPLANTATION_GPU
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none

    character(len=14),private,parameter::OKMC_CASCADE_FORMAT18 = "&CASCADEOKMC18"
    character(len=12),private,parameter::SRIM_CASCADE = "&CASCADESRIM"
    character(len=10),private,parameter::MD_CASCADE = "&CASCADEMD"
    character(len=16),private,parameter::MARLOWER_CASCADE = "&CASCADEMARLOWER"

    integer,parameter,private::p_InsertCountModel_ByConfigNum = 0
    integer,parameter,private::p_InsertCountModel_ByClusterNum = 1

    integer,parameter,private::p_InsertConfig_ByAlphaBeta = 0
    integer,parameter,private::p_InsertConfig_Random = 1

    #ifdef MAXBATCHINSERTTP
    integer,parameter,private::p_MAX_BATCHINSERTTIMEPOINTS = MAXBATCHINSERTTP
    #else
    integer,parameter,private::p_MAX_BATCHINSERTTIMEPOINTS = 10
    #endif

    TYPE,public,extends(ImplantSection)::BatchImplantSection
        real(kind=KINDDF)::InsertTimeInterval = -1.0

        integer::NInsertTimePoint = 0
        real(kind=KINDDF),dimension(:),allocatable::InsertTimePoint

        integer::InsertCountModel = p_InsertCountModel_ByConfigNum

        integer::InsertCountOneBatch = 0

        character*256::ConfigFolder = ""
        character*256,dimension(:),allocatable::ConfigFiles

        logical::WhetherReadToMemory = .false.

        integer::InsetSequence = p_InsertConfig_ByAlphaBeta

        contains

        procedure,non_overridable,public,pass::LoadOne_ImplantSection

        procedure,non_overridable,public,pass::ReadRateCtrl
        procedure,non_overridable,public,pass::ReadBatchConfigCtrl

    END TYPE

    TYPE,public::BatchImplantList
        type(BatchImplantSection)::TheBatchImplantSection

        type(BatchImplantList),pointer::next=>null()

        integer::ListCount=0

    END TYPE

    private::LoadOne_ImplantSection
    private::ReadRateCtrl
    private::ReadBatchConfigCtrl

    contains

    !*****The declare for abstract method***************
    !*****Note: this is not same with the "Fortran 95/2003 For Scientists and Engineers, Third Edition (chinese version)(P668)"
    !*****Because the abstract useage way in this book is not depended on PGFORTRAN compiler. The  following is based on our test
    !*****and verified to suit for pgfortran.
    subroutine LoadOne_ImplantSection(this,hFile,SimBoxes,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(BatchImplantSection)::this
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
                    call this%ImplantSection%ReadImplantCommonCtl(hFile,LINE)
                case("&RATESUBCTL")
                    call this%ReadRateCtrl(hFile,Host_SimuCtrlParam,LINE)
                case("&BATCHCONFIGSUBCTL")
                    call this%ReadBatchConfigCtrl(hFile,Host_SimuCtrlParam,LINE)

                case default
                    write(*,*) "MCPSCUERROR: Unknown Flag: ",KEYWORD
                    write(*,*) "At LINE: ",LINE
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


    !*****************************************************************
    subroutine ReadRateCtrl(this,hFile,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(BatchImplantSection)::this
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


    !******************************************************************
    subroutine ReadBatchConfigCtrl(this,hFile,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(BatchImplantSection)::this
        integer, intent(in)::hFile
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !----Dummy Vars---
        character*256::STR
        character*32::KEYWORD
        character*20::STRTMP(20)
        integer::N
        integer::I
        character*256::ConfigPath
        !---Body---


        Do while(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&FOLDER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for  batch input configuration folder setting"
                        write(*,*) "At Line: ",LINE
                        write(*,*) STR
                        write(*,*) "You should special &FOLDER The candiacate batches configurations folder = "
                        pause
                        stop
                    end if

                    ConfigPath = STRTMP(1)
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

                case("&READTOMEMORY")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for  batch input configuration whether read to memory"
                        write(*,*) "At Line: ",LINE
                        write(*,*) STR
                        write(*,*) "You should special &READTOMEMORY Whether store all configurations to memory ="
                        pause
                        stop
                    end if

                    call UPCASE(STRTMP(1))
                    STRTMP(1) = adjustl(STRTMP(1))
                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        this%WhetherReadToMemory = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        this%WhetherReadToMemory = .false.
                    else
                        write(*,*) "MCPSCUERROR: You can only special YES or NO for Whether store all configurations to memory"
                        write(*,*) "However, what you used is: ",STR
                        pause
                        stop
                    end if

                case("&SELECTSEQUENCE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for  batch input configuration read sequence"
                        write(*,*) "At Line: ",LINE
                        write(*,*) STR
                        write(*,*) "You should special &SELECTSEQUENCE select sequence is by = "
                        pause
                        stop
                    end if

                    this%InsetSequence = ISTR(STRTMP(1))
            end select

        End Do

        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine ReadBatchConfigCtrl

end module INLET_BATCHIMPLANTATION_GPU
