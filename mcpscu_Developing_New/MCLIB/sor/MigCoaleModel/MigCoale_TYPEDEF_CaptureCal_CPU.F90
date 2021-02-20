module MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    use MCLIB_CONSTANTS
    use MCLIB_UTILITIES
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    implicit none

    integer,parameter::CaptureGenWay_ByCentUniform_Locally = 0
    integer,parameter::CaptureGenWay_ByMCConfig_Locally_Directly = 1

    integer,parameter::Capture_SIASPaceModel_Type_UnderSphereFace = 0
    integer,parameter::Capture_SIASPaceModel_Type_UniformOutRSIA = 1
    integer,parameter::Capture_SIASPaceModel_Type_UniformWholeBox = 2
    integer,parameter::Capture_SIASPaceModel_Type_UnderSphereFaceAndSpecialCenter = 3

    integer,parameter::Capture_RSIADistributeToCent_Type_FixedValue = 0
    integer,parameter::Capture_RSIADistributeToCent_Type_OutFromMostOutVAC = 1

    integer,parameter::Capture_ROutAbsorbToCent_Type_FixedValue = 0
    integer,parameter::Capture_ROutAbsorbToCent_Type_OutFromMostOutVAC = 1

    type,public::CaptureCal
        integer::CaptureGenerateWay = CaptureGenWay_ByMCConfig_Locally_Directly
        integer::NSIA = 0
        integer::SIASIZE = 1
        integer::NCascade = 1
        integer,dimension(:),allocatable::UDef_NVAC
        integer,dimension(:),allocatable::UDef_VACSIZE

        real(kind=KINDDF),dimension(:,:),allocatable::UDef_CascadeCent

        character*20::VACDist_Shape
        real(kind=KINDDF),dimension(:,:),allocatable::UDef_Shape_Data

        logical::SIADistSameBetweenBoxes = .false.
        logical::VACDistSameBetweenBoxes = .false.
        logical::VACDistSameBetweenCasese = .false.

        integer::SIASPaceModel = Capture_SIASPaceModel_Type_UnderSphereFace
        integer::SIASPaceNum = 1
        real(kind=KINDDF),dimension(:,:),allocatable::m_SIASPaceCenter

        integer::RSIADistributeToCent_Type = Capture_RSIADistributeToCent_Type_FixedValue
        real(kind=KINDDF)::RSIADistributeToCent_Value = 0.D0


        integer::ROutAbsorbToCent_Type = Capture_ROutAbsorbToCent_Type_FixedValue
        real(kind=KINDDF)::ROutAbsorbToCent_Value = 0.D0

        logical::CheckSIARange = .true.
        logical::CheckOutAbsorb = .true.

        real(kind=KINDDF),dimension(:),allocatable::m_RSIADistributeToCent
        real(kind=KINDDF),dimension(:),allocatable::m_ROutAbsorbToCent
        character*1000::MCCfgPath
        integer::TargetTotalBoxNum
        integer::StartBoxIdx
        Integer::EndBoxIdx
        character*1000::CapCalInfoPath
        real(kind=KINDDF),dimension(:,:),allocatable::m_CascadeCenter
        real(kind=KINDDF),dimension(:),allocatable::m_maxDistance
        integer,dimension(:),allocatable::m_NVACInEachBox

        contains
        procedure,public,non_overridable,pass::Init=>InitCaptureCal
        procedure,public,non_overridable,pass::ResolveCapCtrlFile
        procedure,public,non_overridable,pass::ResolveCapCtrlFile_ByCentUniform
        procedure,public,non_overridable,pass::ResolveCapCtrlFile_FormMCConfig
        procedure,public,non_overridable,pass::ResolveCapInfoFile
        procedure,public,non_overridable,pass::copyCaptureCalFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_CaptureCal
        Generic::Assignment(=)=>copyCaptureCalFromOther
        Final::CleanCaptureCal
    end type CaptureCal


    private::InitCaptureCal
    private::ResolveCapCtrlFile
    private::ResolveCapCtrlFile_ByCentUniform
    private::ResolveCapCtrlFile_FormMCConfig
    private::ResolveCapInfoFile
    private::copyCaptureCalFromOther
    private::Clean_CaptureCal
    private::copyCaptureCalFromOther
    private::CleanCaptureCal
    contains

    subroutine InitCaptureCal(this,MultiBox)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        integer,intent(in)::MultiBox
        !---Body---
        this%CaptureGenerateWay = CaptureGenWay_ByMCConfig_Locally_Directly
        this%NSIA = 0
        this%SIASIZE = 1
        this%NCascade = 0

        call DeAllocateArray_Host(this%UDef_NVAC,"UDef_NVAC")
        call DeAllocateArray_Host(this%UDef_VACSIZE,"UDef_NVAC")
        call DeAllocateArray_Host(this%UDef_CascadeCent,"UDef_CascadeCent")

        this%VACDist_Shape = ""
        call DeAllocateArray_Host(this%UDef_Shape_Data,"this%UDef_Shape_Data")

        this%SIASPaceModel = Capture_SIASPaceModel_Type_UnderSphereFace
        this%RSIADistributeToCent_Type = Capture_RSIADistributeToCent_Type_FixedValue
        this%RSIADistributeToCent_Value = 0.D0

        this%ROutAbsorbToCent_Type = Capture_ROutAbsorbToCent_Type_FixedValue
        this%ROutAbsorbToCent_Value = 0.D0

        this%CheckSIARange = .true.
        this%CheckOutAbsorb = .true.

        this%MCCfgPath = ""
        this%TargetTotalBoxNum = 1
        this%StartBoxIdx = 1
        this%EndBoxIdx = 1

        this%CapCalInfoPath = ""

        call DeAllocateArray_Host(this%m_CascadeCenter,"m_CascadeCenter")
        call AllocateArray_Host(this%m_CascadeCenter,MultiBox,3,"m_CascadeCenter")

        call DeAllocateArray_Host(this%m_maxDistance,"m_maxDistance")
        call AllocateArray_Host(this%m_maxDistance,MultiBox,"m_maxDistance")

        call DeAllocateArray_Host(this%m_NVACInEachBox,"m_NVACInEachBox")
        call AllocateArray_Host(this%m_NVACInEachBox,MultiBox,"m_NVACInEachBox")

        call DeAllocateArray_Host(this%m_RSIADistributeToCent,"m_RSIADistributeToCent")
        call AllocateArray_Host(this%m_RSIADistributeToCent,MultiBox,"m_RSIADistributeToCent")

        call DeAllocateArray_Host(this%m_ROutAbsorbToCent,"m_ROutAbsorbToCent")
        call AllocateArray_Host(this%m_ROutAbsorbToCent,MultiBox,"m_ROutAbsorbToCent")

        return
    end subroutine


    subroutine ResolveCapCtrlFile(this,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        character*1000::STR
        integer::hFile
        integer::LINE
        character*32::KEYWORD
        character*1000::STRTMP(10)
        integer::N
        integer::CaptureGenerateWay
        logical::Finded
        integer::I
        integer::J
        character*20,dimension(:),allocatable::STRTMPPos
        !---Body---
        if(.not. allocated(this%m_CascadeCenter) .or. &
           .not. allocated(this%m_maxDistance) .or.   &
           .not. allocated(this%m_NVACInEachBox) .or. &
           .not. allocated(this%m_RSIADistributeToCent) .or. &
           .not. allocated(this%m_ROutAbsorbToCent)) then
           write(*,*) "MCPSCUERROR: You must initial the CaptureCal object before resolve the capture info file"
           pause
           stop
        end if

        hFile = OpenExistedFile(Host_SimuCtrlParam%CapCtrlFile)


        LINE = 0
        Finded = .false.
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
                    this%CaptureGenerateWay = ISTR(STRTMP(1))
                    Finded = 1
            end select
        End Do

        if(Finded .LE. 0) then
            write(*,*) "You must special the capture generate way."
            write(*,*) "By the way: '&GENERATEWAY  the capture generate way = "
            write(*,*)  "0 by Locally ,center uniform way; 1 by from MC configuration,Directly,2 by special shape)"
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
                case("&SIANUMBER")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the SIA number in one box."
                        pause
                        stop
                    end if
                    this%NSIA = ISTR(STRTMP(1))

                    if(this%NSIA .LE. 0) then
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
                    this%SIASIZE = ISTR(STRTMP(1))

                    if(this%SIASIZE .LE. 0) then
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
                case("&SIASPACEMODEL")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the SIA distribution space model."
                        pause
                        stop
                    end if
                    this%SIASPACEMODEL = ISTR(STRTMP(1))

                    if(this%SIASPACEMODEL .ne. Capture_SIASPaceModel_Type_UnderSphereFace .AND.  &
                       this%SIASPACEMODEL .ne. Capture_SIASPaceModel_Type_UniformOutRSIA  .AND.  &
                       this%SIASPACEMODEL .ne. Capture_SIASPaceModel_Type_UniformWholeBox .AND.  &
                       this%SIASPaceModel .ne. Capture_SIASPaceModel_Type_UnderSphereFaceAndSpecialCenter) then
                        write(*,*) "MCPSCUERROR: Unknown SIA distribution space model: ",this%SIASPACEMODEL
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the SIA distribution space model."
           pause
           stop
        end if


        if(this%SIASPaceModel .eq. Capture_SIASPaceModel_Type_UnderSphereFaceAndSpecialCenter) then

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
                    case("&SIASPACENUMANDCENTER")
                        call EXTRACT_NUMB(STR,1,N,STRTMP)
                        if(N .LT. 1) then
                            write(*,*) "MCPSCUERROR: You must special the SIA sphere face number and center when SIA distribution type is ",Capture_SIASPaceModel_Type_UnderSphereFaceAndSpecialCenter
                            pause
                            stop
                        end if
                        this%SIASPACENUM = ISTR(STRTMP(1))

                        if(this%SIASPACENUM .LT. 1) then
                            write(*,*) "MCPSCUERROR: the SIA sphere face number cannot less than 1: ",this%SIASPACENUM
                            write(*,*) STR
                            pause
                            stop
                        end if

                        if(allocated(STRTMPPos)) deallocate(STRTMPPos)
                        allocate(STRTMPPos(3*this%SIASPACENUM+1))
                        STRTMPPos = ""

                        call AllocateArray_Host(this%m_SIASPaceCenter,this%SIASPACENUM,3,"this%m_SIASPaceCenter")

                        call EXTRACT_NUMB(STR,3*this%SIASPACENUM+1,N,STRTMPPos)
                        if((N-1) .LT. 3*this%SIASPACENUM) then
                            write(*,*) "MCPSCUERROR: You must special ",3*this%SIASPACENUM," SIA sphere face for ",this%SIASPACENUM," SIA sphere face number ."
                            write(*,*) "However, you only specialed ",N-1," position."
                            pause
                            stop
                        end if

                        DO I = 1,this%SIASPACENUM
                            DO J = 1,3
                                this%m_SIASPaceCenter(I,J) = DRSTR(STRTMPPos((I - 1)*3 + J + 1))*Host_Boxes%BOXSIZE(J) +  Host_Boxes%BOXBOUNDARY(J,1)

                                if(this%m_SIASPaceCenter(I,J) .LT.  Host_Boxes%BOXBOUNDARY(J,1) .or. this%m_SIASPaceCenter(I,J) .GT.  Host_Boxes%BOXBOUNDARY(J,2)) then
                                    write(*,*) "MCPSCUERROR: the SIASpaceCenter existed box boundary: ",this%m_SIASPaceCenter(I,J)
                                    write(*,*) Host_Boxes%BOXBOUNDARY(J,1),Host_Boxes%BOXBOUNDARY(J,2)
                                    pause
                                    stop
                                end if
                            END DO
                        END DO

                        if(allocated(STRTMPPos)) deallocate(STRTMPPos)

                        Finded = .true.
                end select
            END DO

            if(Finded .eq. .false.) then
                write(*,*) "MCPSCUERROR:  You must special the SIA sphere face number and center."
                pause
                stop
            end if

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
                    call EXTRACT_NUMB(STR,2,N,STRTMP)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: You must special the distance radius from cascade center to SIA distributed sphere type and value."
                        pause
                        stop
                    end if
                    this%RSIADistributeToCent_Type = ISTR(STRTMP(1))
                    this%RSIADistributeToCent_Value = DRSTR(STRTMP(2))*Host_Boxes%LatticeLength

                    if(this%RSIADistributeToCent_Value .LE. 0) then
                        write(*,*) "MCPSCUERROR: The SIA distance radius from cascade center to SIA distributed sphere cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the distance radius from cascade center to SIA distributed sphere type and value."
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
                    call EXTRACT_NUMB(STR,2,N,STRTMP)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: You must special the out absorb sphere radius (from center) type and value."
                        pause
                        stop
                    end if
                    this%ROutAbsorbToCent_Type = ISTR(STRTMP(1))
                    this%ROutAbsorbToCent_Value = DRSTR(STRTMP(2))*Host_Boxes%LatticeLength

                    if(this%ROutAbsorbToCent_Value .LE. 0) then
                        write(*,*) "MCPSCUERROR: The out absorb sphere radius (from center) cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the out absorb sphere radius (from center) type and value."
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
                case("&SIASAMEBETWEENBOX")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special YES or NO for whether SIA dist same between different boxes"
                        pause
                        stop
                    end if
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"YES")) then
                        this%SIADistSameBetweenBoxes = .true.
                    else if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"NO")) then
                        this%SIADistSameBetweenBoxes = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether SIA dist same between different boxes."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special YES or NO for whether SIA dist same between different boxes"
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
                case("&CHECKSIARANGE")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special YES or NO for whether Check SIA Range"
                        pause
                        stop
                    end if
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"YES")) then
                        this%CheckSIARange = .true.
                    else if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"NO")) then
                        this%CheckSIARange = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether Check SIA Range."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special YES or NO for whether Check SIA Range"
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
                case("&CHECKOUTABSORB")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special YES or NO for whether Check out absorb range"
                        pause
                        stop
                    end if
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"YES")) then
                        this%CheckOutAbsorb = .true.
                    else if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"NO")) then
                        this%CheckOutAbsorb = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether Check out absorb range."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special YES or NO for whether Check out absorb range."
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
                case("&CALCAPINFOPATH")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the capture info path."
                        pause
                        stop
                    end if
                    this%CapCalInfoPath = adjustl(trim(STRTMP(1)))

                    if(IsAbsolutePath(this%CapCalInfoPath)) then
                        this%CapCalInfoPath = adjustl(trim(this%CapCalInfoPath))
                    else
                        if(LENTRIM(adjustl(Host_SimuCtrlParam%InputFilePath)) .GT. 0) then
                            this%CapCalInfoPath = adjustl(trim(Host_SimuCtrlParam%InputFilePath))//FolderSpe//adjustl(trim(this%CapCalInfoPath))
                        else
                            this%CapCalInfoPath = adjustl(trim(this%CapCalInfoPath))
                        end if
                    endif


                    write(*,*) this%CapCalInfoPath
                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the capture info path."
           pause
           stop
        end if

        select case(this%CaptureGenerateWay)
            case(CaptureGenWay_ByCentUniform_Locally)
                write(*,*) "The capture generate way is by Locally ,center uniform way"

                call this%ResolveCapCtrlFile_ByCentUniform(hFile,Host_Boxes,Host_SimuCtrlParam)

            case(CaptureGenWay_ByMCConfig_Locally_Directly)
                write(*,*) "The capture generate way is by MC Configuration, directly"

                call this%ResolveCapCtrlFile_FormMCConfig(hFile,Host_Boxes,Host_SimuCtrlParam)

            case default
                write(*,*) "MCPSCUERROR: Unknown way to generate capture(0 by uniform way, 1 by from MC configuration,Directly, 2 by special shape)"
                write(*,*) this%CaptureGenerateWay
                pause
                stop
        end select

        return

    end subroutine


    subroutine ResolveCapCtrlFile_ByCentUniform(this,hFile,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        integer,intent(in)::hFile
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        character*1000::STR
        integer::LINE
        character*32::KEYWORD
        character*1000::STRTMP(10)
        integer::N
        logical::Finded
        integer::ICase
        integer::I
        integer::J
        !---Body---

        LINE = 0
        Finded = .false.
        rewind(hFile)
        DO while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&NCASCADE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the Cascade number in one box."
                        pause
                        stop
                    end if
                    this%NCascade = ISTR(STRTMP(1))

                    if(this%NCascade .LE. 0) then
                        write(*,*) "MCPSCUERROR: the cascade number cannot less than 1."
                        write(*,*) this%NCascade
                        pause
                        stop
                    end if

                    Finded = .true.
            end select

        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the Cascade number in one box."
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
                case("&VACSAMEBETWEENCASES")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special YES or NO for whether VAC dist same between different cascades"
                        pause
                        stop
                    end if
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"YES")) then
                        this%VACDistSameBetweenCasese = .true.
                    else if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"NO")) then
                        this%VACDistSameBetweenCasese = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether VAC dist same between different cascades."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special YES or NO for whether VAC dist same between different cascades"
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
                case("&VACNUMBER")
                    call EXTRACT_NUMB(STR,this%NCascade,N,STRTMP)
                    if(N .LT. this%NCascade) then
                        write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC number in one box."
                        pause
                        stop
                    end if
                    call AllocateArray_Host(this%UDef_NVAC,this%NCascade,"this%NCascade")

                    DO ICase=1,this%NCascade
                        this%UDef_NVAC(ICase) = ISTR(STRTMP(ICase))

                        if(this%UDef_NVAC(ICase) .LE. 0) then
                            write(*,*) "MCPSCUERROR: The VAC number cannot less than 0"
                            pause
                            stop
                        end if

                        if(ICase .GT. 1) then
                            if(this%UDef_NVAC(ICase) .ne. this%UDef_NVAC(ICase-1)) then
                                write(*,*) "MCPSCUERROR: Cascade number is different between Case: ",ICase-1,ICase
                                write(*,*) this%UDef_NVAC(ICase-1),this%UDef_NVAC(ICase)
                                write(*,*) "However ,you had set that cascades are same in on box"
                                pause
                                stop
                            end if
                        end if
                    END DO

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the VAC number in one box."
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
                case("&VACSIZE")
                    call EXTRACT_NUMB(STR,this%NCascade,N,STRTMP)
                    if(N .LT. this%NCascade) then
                        write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC size in one box."
                        pause
                        stop
                    end if
                    call AllocateArray_Host(this%UDef_VACSIZE,this%NCascade,"this%NCascade")

                    DO ICase = 1,this%NCascade
                        this%UDef_VACSIZE(ICase) = ISTR(STRTMP(ICase))

                        if(this%UDef_VACSIZE(ICase) .LE. 0) then
                            write(*,*) "MCPSCUERROR: The VAC size cannot less than 0"
                            pause
                            stop
                        end if


                        if(ICase .GT. 1) then
                            if(this%UDef_VACSIZE(ICase) .ne. this%UDef_VACSIZE(ICase-1) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                write(*,*) "MCPSCUERROR: Cascade size is different between Case: ",ICase-1,ICase
                                write(*,*) this%UDef_VACSIZE(ICase-1),this%UDef_VACSIZE(ICase)
                                write(*,*) "However ,you had set that cascades are same in on box"
                                pause
                                stop
                            end if
                        end if
                    END DO

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the VAC number in one box."
           pause
           stop
        end if


        LINE = 0
        Finded = .false.
        rewind(hFile)
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
                case("&VACSPECIALSHAPE")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the shape of VAC dist if you chosen the mode: ",this%CaptureGenerateWay
                        write(*,*) "You can choose SPHERE or LINE or ROUND or SQUARE or CYLINDER or ELLIPSOID "
                        pause
                        stop
                    end if

                    call UPCASE(STRTMP(1))

                    this%VACDist_Shape = adjustl(trim(STRTMP(1)))

                    Finded = .true.
            end select
        End Do

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the shape of VAC dist if you chosen the mode: ",this%CaptureGenerateWay
           pause
           stop
        end if


        LINE = 0
        Finded = .false.
        rewind(hFile)
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
                case("&VACSPECIALSHAPEDATA")

                    call DeAllocateArray_Host(this%UDef_Shape_Data,"this%UDef_Shape_Data")

                    select case(this%VACDist_Shape(1:LENTRIM(this%VACDist_Shape)))
                        case("SPHERE")
                            call EXTRACT_NUMB(STR,this%NCascade,N,STRTMP)
                            if(N .LT. this%NCascade) then
                                write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC distribution radius for SPHERE shape in one box."
                                pause
                                stop
                            end if
                            call AllocateArray_Host(this%UDef_Shape_Data,this%NCascade,1,"this%UDef_Shape_Data")

                            DO ICase = 1,this%NCascade
                                this%UDef_Shape_Data(ICase,1) = DRSTR(STRTMP(ICase))*Host_Boxes%LatticeLength

                                if(this%UDef_Shape_Data(ICase,1) .LE. 0) then
                                    write(*,*) "MCPSCUERROR: The VAC distribution radius cannot less than 0"
                                    pause
                                    stop
                                end if

                                if(ICase .GT. 1) then
                                    if(this%UDef_Shape_Data(ICase,1) .ne. this%UDef_Shape_Data(ICase-1,1) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                        write(*,*) "MCPSCUERROR: VAC distributions are different between Cases: ",ICase-1,ICase
                                        write(*,*) this%UDef_Shape_Data(ICase-1,1),this%UDef_Shape_Data(ICase,1)
                                        write(*,*) "However ,you had set that cascades are same in on box"
                                        pause
                                        stop
                                    end if
                                end if

                            END DO

                        case("LINE")
                            call EXTRACT_NUMB(STR,this%NCascade,N,STRTMP)
                            if(N .LT. this%NCascade) then
                                write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC distribution separation distance for LINE shape in one box."
                                pause
                                stop
                            end if
                            call AllocateArray_Host(this%UDef_Shape_Data,this%NCascade,1,"this%UDef_Shape_Data")

                            DO ICase = 1,this%NCascade
                                this%UDef_Shape_Data(ICase,1) = DRSTR(STRTMP(ICase))*Host_Boxes%LatticeLength

                                if(this%UDef_Shape_Data(ICase,1) .LE. 0) then
                                    write(*,*) "MCPSCUERROR: The VAC distribution radius cannot less than 0"
                                    pause
                                    stop
                                end if

                                if(ICase .GT. 1) then
                                    if(this%UDef_Shape_Data(ICase,1) .ne. this%UDef_Shape_Data(ICase-1,1) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                        write(*,*) "MCPSCUERROR: VAC distribution separation distance are different between Cases: ",ICase-1,ICase
                                        write(*,*) this%UDef_Shape_Data(ICase-1,1),this%UDef_Shape_Data(ICase,1)
                                        write(*,*) "However ,you had set that cascades are same in on box"
                                        pause
                                        stop
                                    end if
                                end if

                            END DO

                        case("ROUND")
                            call EXTRACT_NUMB(STR,this%NCascade,N,STRTMP)
                            if(N .LT. this%NCascade) then
                                write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC distribution round surface radius for ROUND shape in one box."
                                pause
                                stop
                            end if
                            call AllocateArray_Host(this%UDef_Shape_Data,this%NCascade,1,"this%UDef_Shape_Data")

                            DO ICase = 1,this%NCascade
                                this%UDef_Shape_Data(ICase,1) = DRSTR(STRTMP(ICase))*Host_Boxes%LatticeLength

                                if(this%UDef_Shape_Data(ICase,1) .LE. 0) then
                                    write(*,*) "MCPSCUERROR: The VAC distribution radius cannot less than 0"
                                    pause
                                    stop
                                end if

                                if(ICase .GT. 1) then
                                    if(this%UDef_Shape_Data(ICase,1) .ne. this%UDef_Shape_Data(ICase-1,1) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                        write(*,*) "MCPSCUERROR:VAC distribution round surface radius for ROUND shape are different between Cases: ",ICase-1,ICase
                                        write(*,*) this%UDef_Shape_Data(ICase-1,1),this%UDef_Shape_Data(ICase,1)
                                        write(*,*) "However ,you had set that cascades are same in on box"
                                        pause
                                        stop
                                    end if
                                end if

                            END DO

                        case("SQUARE")
                            call EXTRACT_NUMB(STR,this%NCascade*2,N,STRTMP)
                            if(N .LT. this%NCascade) then
                                write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC distribution two sides length for SQUARE shape in one box."
                                pause
                                stop
                            end if
                            call AllocateArray_Host(this%UDef_Shape_Data,this%NCascade,2,"this%UDef_Shape_Data")

                            DO ICase = 1,this%NCascade
                                this%UDef_Shape_Data(ICase,1) = DRSTR(STRTMP((ICase-1)*2+1))*Host_Boxes%LatticeLength
                                this%UDef_Shape_Data(ICase,2) = DRSTR(STRTMP((ICase-1)*2+2))*Host_Boxes%LatticeLength

                                DO I = 1,2
                                    if(this%UDef_Shape_Data(ICase,I) .LE. 0) then
                                        write(*,*) "MCPSCUERROR: The VAC distribution side length cannot less than 0"
                                        pause
                                        stop
                                    end if

                                    if(ICase .GT. 1) then
                                        if(this%UDef_Shape_Data(ICase,I) .ne. this%UDef_Shape_Data(ICase-1,I) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                            write(*,*) "MCPSCUERROR: Cascades VAC distribution two sides length for SQUARE shape data are different between Cases: ",ICase-1,ICase
                                            write(*,*) this%UDef_Shape_Data(ICase-1,I),this%UDef_Shape_Data(ICase,I)
                                            write(*,*) "However ,you had set that cascades are same in on box"
                                            pause
                                            stop
                                        end if
                                    end if
                                END DO

                            END DO

                        case("CYLINDER")
                            call EXTRACT_NUMB(STR,this%NCascade*2,N,STRTMP)
                            if(N .LT. this%NCascade*2) then
                                write(*,*) "MCPSCUERROR: You must special ",this%NCascade," Cascades VAC distribution bottom radius and high for CYLINDER shape in one box."
                                pause
                                stop
                            end if
                            call AllocateArray_Host(this%UDef_Shape_Data,this%NCascade,2,"this%UDef_Shape_Data")

                            DO ICase = 1,this%NCascade
                                this%UDef_Shape_Data(ICase,1) = DRSTR(STRTMP((ICase-1)*2+1))*Host_Boxes%LatticeLength
                                this%UDef_Shape_Data(ICase,2) = DRSTR(STRTMP((ICase-1)*2+2))*Host_Boxes%LatticeLength

                                DO I = 1,2
                                    if(this%UDef_Shape_Data(ICase,I) .LE. 0) then
                                        write(*,*) "MCPSCUERROR: The VAC distribution radius cannot less than 0"
                                        pause
                                        stop
                                    end if

                                    if(ICase .GT. 1) then
                                        if(this%UDef_Shape_Data(ICase,I) .ne. this%UDef_Shape_Data(ICase-1,I) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                            write(*,*) "MCPSCUERROR: VAC distribution shape data are different between Cases: ",ICase-1,ICase
                                            write(*,*) this%UDef_Shape_Data(ICase-1,I),this%UDef_Shape_Data(ICase,I)
                                            write(*,*) "However ,you had set that cascades are same in on box"
                                            pause
                                            stop
                                        end if
                                    end if
                                END DO

                            END DO

                        case("ELLIPSOID")
                            call EXTRACT_NUMB(STR,this%NCascade*3,N,STRTMP)
                            if(N .LT. this%NCascade*3) then
                                write(*,*) "MCPSCUERROR: You must special ",this%NCascade," a,b,c for x ,y and z axis when the ELLIPSOID shape is chosen."
                                pause
                                stop
                            end if
                            call AllocateArray_Host(this%UDef_Shape_Data,this%NCascade,3,"this%UDef_Shape_Data")

                            DO ICase = 1,this%NCascade
                                this%UDef_Shape_Data(ICase,1) = DRSTR(STRTMP((ICase-1)*2+1))*Host_Boxes%LatticeLength
                                this%UDef_Shape_Data(ICase,2) = DRSTR(STRTMP((ICase-1)*2+2))*Host_Boxes%LatticeLength
                                this%UDef_Shape_Data(ICase,3) = DRSTR(STRTMP((ICase-1)*2+3))*Host_Boxes%LatticeLength

                                DO I = 1,3
                                    if(this%UDef_Shape_Data(ICase,I) .LE. 0) then
                                        write(*,*) "MCPSCUERROR: The VAC distribution radius cannot less than 0"
                                        pause
                                        stop
                                    end if

                                    if(ICase .GT. 1) then
                                        if(this%UDef_Shape_Data(ICase,I) .ne. this%UDef_Shape_Data(ICase-1,I) .AND. this%VACDistSameBetweenCasese .eq. .true.) then
                                            write(*,*) "MCPSCUERROR: VAC distribution shape data are different between Cases: ",ICase-1,ICase
                                            write(*,*) this%UDef_Shape_Data(ICase-1,I),this%UDef_Shape_Data(ICase,I)
                                            write(*,*) "However ,you had set that cascades are same in on box"
                                            pause
                                            stop
                                        end if
                                    end if
                                END DO

                            END DO

                        case default
                            write(*,*) "MCPSCUERROR:Unknown VAC dist shape: ",this%VACDist_Shape(1:LENTRIM(this%VACDist_Shape))
                            write(*,*) "You can choose SPHERE or LINE or CYLINDER or ELLIPSOID "
                            pause
                            stop
                    end select

                    Finded = .true.
            end select
        End Do

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the shape data of VAC dist if you chosen the mode: ",this%CaptureGenerateWay
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
                case("&CASCADECENTERPOS")
                    call EXTRACT_NUMB(STR,3*this%NCascade,N,STRTMP)
                    if(N .LT. 3*this%NCascade) then
                        write(*,*) "MCPSCUERROR: You must special 3*",this%NCascade," position of user defined cascade center."
                        pause
                        stop
                    end if
                    call AllocateArray_Host(this%UDef_CascadeCent,this%NCascade,3,"this%UDef_CascadeCent")

                    DO ICase = 1,this%NCascade

                        this%UDef_CascadeCent(ICase,1) = Host_Boxes%BOXBOUNDARY(1,1) + DRSTR(STRTMP((ICase-1)*3 + 1))*Host_Boxes%BOXSIZE(1)
                        this%UDef_CascadeCent(ICase,2) = Host_Boxes%BOXBOUNDARY(2,1) + DRSTR(STRTMP((ICase-1)*3 + 2))*Host_Boxes%BOXSIZE(2)
                        this%UDef_CascadeCent(ICase,3) = Host_Boxes%BOXBOUNDARY(3,1) + DRSTR(STRTMP((ICase-1)*3 + 3))*Host_Boxes%BOXSIZE(3)

                        DO J = 1,3
                            if(this%UDef_CascadeCent(ICase,J) .LT. Host_Boxes%BOXBOUNDARY(J,1) .or. &
                               this%UDef_CascadeCent(ICase,J) .GT. Host_Boxes%BOXBOUNDARY(J,2)) then
                                write(*,*) "MCPSCUERROR: The user defined cascade center cannot exit the box boundary"
                                write(*,*) "cascade center: ",this%UDef_CascadeCent(ICase,J)
                                write(*,*) "box boundary: ",Host_Boxes%BOXBOUNDARY(J,1),Host_Boxes%BOXBOUNDARY(J,2)
                                pause
                                stop
                            end if
                        END DO
                    END DO
                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the three position of user defined cascade center."
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
                case("&VACSAMEBETWEENBOX")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special YES or NO for whether VAC dist same between different boxes"
                        pause
                        stop
                    end if
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"YES")) then
                        this%VACDistSameBetweenBoxes = .true.
                    else if(IsStrEqual(STRTMP(1)(1:LENTRIM(STRTMP(1))),"NO")) then
                        this%VACDistSameBetweenBoxes = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether VAC dist same between different boxes."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special YES or NO for whether VAC dist same between different boxes"
           pause
           stop
        end if


        return
    end subroutine ResolveCapCtrlFile_ByCentUniform

    subroutine ResolveCapCtrlFile_FormMCConfig(this,hFile,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        integer,intent(in)::hFile
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
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
                case("&MCCFGPATH")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the MC Configuration path."
                        pause
                        stop
                    end if
                    this%MCCfgPath = adjustl(trim(STRTMP(1)))

                    if(IsAbsolutePath(this%MCCfgPath)) then
                        this%MCCfgPath = adjustl(trim(this%MCCfgPath))
                    else
                        if(LENTRIM(adjustl(Host_SimuCtrlParam%InputFilePath)) .GT. 0) then
                            this%MCCfgPath = adjustl(trim(Host_SimuCtrlParam%InputFilePath))//FolderSpe//adjustl(trim(this%MCCfgPath))
                        else
                            this%MCCfgPath = adjustl(trim(this%MCCfgPath))
                        end if
                    endif


                    write(*,*) this%MCCfgPath
                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the MC Configuration path."
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
                case("&TARGETBOXNUM")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special total box number in MC configuration used to construct captureBox."
                        pause
                        stop
                    end if

                    this%TargetTotalBoxNum = ISTR(STRTMP(1))

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special total box number in MC configuration used to construct captureBox."
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
                case("&SEIDUSEDBOX")
                    call EXTRACT_NUMB(STR,2,N,STRTMP)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: You must special the start and end box index in MC configuration used to construct captureBox."
                        pause
                        stop
                    end if

                    this%StartBoxIdx = ISTR(STRTMP(1))
                    this%EndBoxIdx = ISTR(STRTMP(2))

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the start and end box index in MC configuration used to construct captureBox."
           pause
           stop
        end if

        close(hFile)
        return
    end subroutine ResolveCapCtrlFile_FormMCConfig

    subroutine ResolveCapInfoFile(this,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        integer::hFile
        integer::FindTrueLine
        character*1000::STR
        integer::LINE
        character*100::STRTMP(10)
        integer::N
        integer::MultiBox
        integer::IBox
        integer::I
        !---Body---
        if(.not. allocated(this%m_CascadeCenter) .or. &
           .not. allocated(this%m_maxDistance) .or.   &
           .not. allocated(this%m_NVACInEachBox) .or. &
           .not. allocated(this%m_RSIADistributeToCent) .or. &
           .not. allocated(this%m_ROutAbsorbToCent)) then
           write(*,*) "MCPSCUERROR: You must initial the CaptureCal object before resolve the capture info file"
           pause
           stop
        end if

        MultiBox = Host_SimuCtrlParam%MultiBox

        hFile = OpenExistedFile(this%CapCalInfoPath)

        FindTrueLine = 0

        LINE = 0
        DO while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))

            LINE = LINE + 1
            STR = adjustl(trim(STR))
            call RemoveComments(STR,"!")

            if(LENTRIM(adjustl(STR)) .LE. 0) then
                cycle
            end if

            if(ichar(STR(1:1)) .GE. ichar('0') .AND. ichar(STR(1:1)) .LE. ichar('9')) then
                FindTrueLine = FindTrueLine + 1
            end if

        END DO

        if(FindTrueLine .LT. MultiBox) then
            write(*,*) "MCPSCUERROR: The total box info line number is less than box number"
            write(*,*) "Total line number : ",FindTrueLine
            write(*,*) "Total box number: ",MultiBox
            pause
            stop
        end if

        rewind(hFile)

        LINE = 0

        DO while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))

            LINE = LINE + 1
            STR = adjustl(trim(STR))
            call RemoveComments(STR,"!")

            if(LENTRIM(adjustl(STR)) .LE. 0) then
                cycle
            end if

            if(ichar(STR(1:1)) .GE. ichar('0') .AND. ichar(STR(1:1)) .LE. ichar('9')) then
                call EXTRACT_NUMB(STR,9,N,STRTMP)

                if(N .LT. 9) then
                    write(*,*) "MCPSCUERROR: The info number is less than 9 in LINE: ",LINE
                    write(*,*) STR
                    write(*,*) "At file: ",this%CapCalInfoPath
                    pause
                    stop
                end if

                IBox = ISTR(STRTMP(1))

                if(IBox .LE. 0 .or. IBox .GT. MultiBox) then
                    write(*,*) "MCPSCUERROR: The index of Box is out of range: ",IBox
                    pause
                    stop
                end if

                this%m_NVACInEachBox(IBox) = ISTR(STRTMP(3))

                this%m_CascadeCenter(IBox,1) = DRSTR(STRTMP(4))*Host_Boxes%LatticeLength
                this%m_CascadeCenter(IBox,2) = DRSTR(STRTMP(5))*Host_Boxes%LatticeLength
                this%m_CascadeCenter(IBox,3) = DRSTR(STRTMP(6))*Host_Boxes%LatticeLength

                this%m_maxDistance(IBox) = DRSTR(STRTMP(7))*Host_Boxes%LatticeLength

                this%m_RSIADistributeToCent(IBox) = DRSTR(STRTMP(8))*Host_Boxes%LatticeLength
                this%m_ROutAbsorbToCent(IBox) = DRSTR(STRTMP(9))*Host_Boxes%LatticeLength

                DO I = 1,3
                    if((this%m_CascadeCenter(IBox,I) - this%m_RSIADistributeToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                       (this%m_CascadeCenter(IBox,I) + this%m_RSIADistributeToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                        write(*,*) "the SIA distribution sphere had existed the box "
                        write(*,*) "cascade center: ",this%m_CascadeCenter(IBox,I)
                        write(*,*) "SIA distribution sphere radius: ",this%m_RSIADistributeToCent(IBox)
                        write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                        pause
                        stop
                    end if

                    if(this%CheckOutAbsorb .eq. .true.) then
                        if((this%m_CascadeCenter(IBox,I) - this%m_ROutAbsorbToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                            (this%m_CascadeCenter(IBox,I) + this%m_ROutAbsorbToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                            write(*,*) "the outer absorb sphere had existed the box "
                            write(*,*) "cascade center: ",this%m_CascadeCenter(IBox,I)
                            write(*,*) " outer absorb sphere radius: ",this%m_ROutAbsorbToCent(IBox)
                            write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if
                    end if
                END DO

                if(this%CheckSIARange .eq. .true.) then
                    if(this%m_maxDistance(IBox) .GT. this%m_RSIADistributeToCent(IBox)) then
                        write(*,*) "MCPSCUERROR: The mostly out vac had exist the SIA distribution position",this%m_maxDistance(IBox),this%m_RSIADistributeToCent(IBox)
                        pause
                        stop
                    end if
                end if

                if(this%m_maxDistance(IBox) .GT. this%m_ROutAbsorbToCent(IBox)) then
                    write(*,*) "MCPSCUERROR: The mostly out vac had exist the out absorb",this%m_maxDistance(IBox),this%m_ROutAbsorbToCent(IBox)
                    pause
                    stop
                end if

                if(this%m_RSIADistributeToCent(IBox) .GT. this%m_ROutAbsorbToCent(IBox)) then
                    write(*,*) "MCPSCUERROR: The SIA distribution radius greater than that for out absorb",this%m_RSIADistributeToCent(IBox),this%m_ROutAbsorbToCent(IBox)
                    pause
                    stop
                end if

            end if

        END DO


        close(hFile)
        return
    end subroutine



    subroutine copyCaptureCalFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal),intent(out)::this
        type(CaptureCal),intent(in)::other
        !---Body---
        this%CaptureGenerateWay = other%CaptureGenerateWay
        this%NSIA = other%NSIA
        this%SIASIZE = other%SIASIZE

        this%NCascade = other%NCascade

        call DeAllocateArray_Host(this%UDef_NVAC,"this%UDef_NVAC")
        call AllocateArray_Host(this%UDef_NVAC,size(other%UDef_NVAC),"this%UDef_NVAC")
        this%UDef_NVAC = other%UDef_NVAC

        call DeAllocateArray_Host(this%UDef_VACSIZE,"this%UDef_VACSIZE")
        call AllocateArray_Host(this%UDef_VACSIZE,size(other%UDef_VACSIZE),"this%UDef_VACSIZE")
        this%UDef_VACSIZE = other%UDef_VACSIZE

        call DeAllocateArray_Host(this%UDef_CascadeCent,"this%UDef_CascadeCent")
        call AllocateArray_Host(this%UDef_CascadeCent,size(other%UDef_CascadeCent,dim=1),size(other%UDef_CascadeCent,dim=2),"this%UDef_CascadeCent")
        this%UDef_CascadeCent = other%UDef_CascadeCent

        this%VACDist_Shape = other%VACDist_Shape
        call DeAllocateArray_Host(this%UDef_Shape_Data,"this%UDef_Shape_Data")
        call AllocateArray_Host(this%UDef_Shape_Data,size(other%UDef_Shape_Data,dim=1),size(other%UDef_Shape_Data,dim=2),"this%UDef_Shape_Data")
        this%UDef_Shape_Data = other%UDef_Shape_Data


        this%SIADistSameBetweenBoxes = other%SIADistSameBetweenBoxes
        this%VACDistSameBetweenBoxes = other%VACDistSameBetweenBoxes
        this%VACDistSameBetweenCasese = other%VACDistSameBetweenCasese

        this%SIASPaceModel = other%SIASPaceModel

        this%SIASPaceNum = other%SIASPaceNum
        call DeAllocateArray_Host(this%m_SIASPaceCenter,"this%m_SIASPaceCenter")
        call AllocateArray_Host(this%m_SIASPaceCenter,size(other%m_SIASPaceCenter,dim=1),size(other%m_SIASPaceCenter,dim=2),"this%m_SIASPaceCenter")
        this%m_SIASPaceCenter = other%m_SIASPaceCenter


        this%RSIADistributeToCent_Type = other%RSIADistributeToCent_Type
        this%RSIADistributeToCent_Value = other%RSIADistributeToCent_Value

        this%ROutAbsorbToCent_Type = other%ROutAbsorbToCent_Type
        this%ROutAbsorbToCent_Value = other%ROutAbsorbToCent_Value

        this%CheckSIARange = other%CheckSIARange
        this%CheckOutAbsorb = other%CheckOutAbsorb

        this%MCCfgPath = other%MCCfgPath
        this%TargetTotalBoxNum = other%TargetTotalBoxNum
        this%StartBoxIdx = other%StartBoxIdx
        this%EndBoxIdx = other%EndBoxIdx

        this%CapCalInfoPath = other%CapCalInfoPath

        call DeAllocateArray_Host(this%m_CascadeCenter,"m_CascadeCenter")
        call AllocateArray_Host(this%m_CascadeCenter,size(other%m_CascadeCenter,dim=1),size(other%m_CascadeCenter,dim=2),"m_CascadeCenter")
        this%m_CascadeCenter = other%m_CascadeCenter

        call DeAllocateArray_Host(this%m_maxDistance,"m_maxDistance")
        call AllocateArray_Host(this%m_maxDistance,size(other%m_maxDistance),"m_maxDistance")
        this%m_maxDistance = other%m_maxDistance

        call DeAllocateArray_Host(this%m_NVACInEachBox,"m_NVACInEachBox")
        call AllocateArray_Host(this%m_NVACInEachBox,size(other%m_NVACInEachBox),"m_NVACInEachBox")
        this%m_NVACInEachBox = other%m_NVACInEachBox

        call DeAllocateArray_Host(this%m_RSIADistributeToCent,"m_RSIADistributeToCent")
        call AllocateArray_Host(this%m_RSIADistributeToCent,size(other%m_RSIADistributeToCent),"m_RSIADistributeToCent")
        this%m_RSIADistributeToCent = other%m_RSIADistributeToCent

        call DeAllocateArray_Host(this%m_ROutAbsorbToCent,"m_ROutAbsorbToCent")
        call AllocateArray_Host(this%m_ROutAbsorbToCent,size(other%m_ROutAbsorbToCent),"m_ROutAbsorbToCent")
        this%m_ROutAbsorbToCent = other%m_ROutAbsorbToCent

        return
    end subroutine



    subroutine Clean_CaptureCal(this)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        !---Body---
        this%CaptureGenerateWay = CaptureGenWay_ByMCConfig_Locally_Directly
        this%NSIA = 0
        this%SIASIZE = 1
        this%NCascade = 0
        call DeAllocateArray_Host(this%UDef_NVAC,"UDef_NVAC")
        call DeAllocateArray_Host(this%UDef_VACSIZE,"UDef_NVAC")
        call DeAllocateArray_Host(this%UDef_CascadeCent,"UDef_CascadeCent")

        this%VACDist_Shape = ""
        call DeAllocateArray_Host(this%UDef_Shape_Data,"this%UDef_Shape_Data")

        this%SIADistSameBetweenBoxes = .false.
        this%VACDistSameBetweenBoxes = .false.
        this%VACDistSameBetweenCasese = .false.

        this%SIASPaceModel = Capture_SIASPaceModel_Type_UnderSphereFace

        this%SIASPaceNum = 1
        call DeAllocateArray_Host(this%m_SIASPaceCenter,"this%m_SIASPaceCenter")


        this%RSIADistributeToCent_Type = Capture_RSIADistributeToCent_Type_FixedValue
        this%RSIADistributeToCent_Value = 0.D0

        this%ROutAbsorbToCent_Type = Capture_ROutAbsorbToCent_Type_FixedValue
        this%ROutAbsorbToCent_Value = 0.D0

        this%CheckSIARange = .true.
        this%CheckOutAbsorb = .true.

        this%MCCfgPath = ""
        this%TargetTotalBoxNum = 1
        this%StartBoxIdx = 1
        this%EndBoxIdx = 1

        this%CapCalInfoPath = ""

        call DeAllocateArray_Host(this%m_CascadeCenter,"m_CascadeCenter")

        call DeAllocateArray_Host(this%m_maxDistance,"m_maxDistance")

        call DeAllocateArray_Host(this%m_NVACInEachBox,"m_NVACInEachBox")

        call DeAllocateArray_Host(this%m_RSIADistributeToCent,"m_RSIADistributeToCent")

        call DeAllocateArray_Host(this%m_ROutAbsorbToCent,"m_ROutAbsorbToCent")

        return
    end subroutine Clean_CaptureCal


    subroutine CleanCaptureCal(this)
        implicit none
        !---Dummy Vars---
        type(CaptureCal)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanCaptureCal


end module
