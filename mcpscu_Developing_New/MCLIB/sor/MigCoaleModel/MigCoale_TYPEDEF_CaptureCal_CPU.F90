module MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    use MCLIB_CONSTANTS
    use MCLIB_UTILITIES
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    implicit none

    integer,parameter::CaptureGenWay_ByCentUniform_Locally = 0
    integer,parameter::CaptureGenWay_ByMCConfig_Locally_Directly = 1

    integer,parameter::Capture_RSIADistributeToCent_Type_FixedValue = 0
    integer,parameter::Capture_RSIADistributeToCent_Type_OutFromMostOutVAC = 1

    integer,parameter::Capture_ROutAbsorbToCent_Type_FixedValue = 0
    integer,parameter::Capture_ROutAbsorbToCent_Type_OutFromMostOutVAC = 1

    type,public::CaptureCal
        integer::CaptureGenerateWay = CaptureGenWay_ByMCConfig_Locally_Directly
        integer::NSIA = 0
        integer::SIASIZE = 1

        integer::RSIADistributeToCent_Type = Capture_RSIADistributeToCent_Type_FixedValue
        real(kind=KINDDF)::RSIADistributeToCent_Value = 0.D0

        integer::ROutAbsorbToCent_Type = Capture_ROutAbsorbToCent_Type_FixedValue
        real(kind=KINDDF)::ROutAbsorbToCent_Value = 0.D0

        real(kind=KINDDF),dimension(:),allocatable::m_RSIADistributeToCent
        real(kind=KINDDF),dimension(:),allocatable::m_ROutAbsorbToCent
        character*1000::MCCfgPath
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

        this%RSIADistributeToCent_Type = Capture_RSIADistributeToCent_Type_FixedValue
        this%RSIADistributeToCent_Value = 0.D0

        this%ROutAbsorbToCent_Type = Capture_ROutAbsorbToCent_Type_FixedValue
        this%ROutAbsorbToCent_Value = 0.D0

        this%MCCfgPath = ""

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


    subroutine ResolveCapCtrlFile(this,CtrlFile,Host_Boxes,Host_SimuCtrlParam)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this
        character*(*),intent(in)::CtrlFile
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        character*1000::ARG
        character*1000::STR
        integer::hFile
        integer::LINE
        character*32::KEYWORD
        character*1000::STRTMP(10)
        integer::N
        integer::CaptureGenerateWay
        integer::FinededCaptureGenWay
        !---Body---
        hFile = OpenExistedFile(CtrlFile)
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
                    this%CaptureGenerateWay = ISTR(STRTMP(1))
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


        select case(this%CaptureGenerateWay)
            case(CaptureGenWay_ByCentUniform_Locally)
                write(*,*) "The capture generate way is by Locally ,center uniform way"

                call this%ResolveCapCtrlFile_ByCentUniform(hFile,Host_Boxes,Host_SimuCtrlParam)

            case(CaptureGenWay_ByMCConfig_Locally_Directly)
                write(*,*) "The capture generate way is by MC Configuration, directly"

                call this%ResolveCapCtrlFile_FormMCConfig(hFile,Host_Boxes,Host_SimuCtrlParam)

            case default
                write(*,*) "MCPSCUERROR: Unknown way to generate capture(0 by uniform way, 1 by from MC configuration,Directly)"
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
        if(.not. allocated(this%m_CascadeCenter) .or. &
           .not. allocated(this%m_maxDistance) .or.   &
           .not. allocated(this%m_NVACInEachBox) .or. &
           .not. allocated(this%m_RSIADistributeToCent) .or. &
           .not. allocated(this%m_ROutAbsorbToCent)) then
           write(*,*) "MCPSCUERROR: You must initial the CaptureCal object before resolve the capture info file"
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

        hFile = OpenExistedFile(Host_SimuCtrlParam%CapInfoFile)

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
                    write(*,*) "At file: ",Host_SimuCtrlParam%CapInfoFile
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

                    if((this%m_CascadeCenter(IBox,I) - this%m_ROutAbsorbToCent(IBox)) .LE. Host_Boxes%BOXBOUNDARY(I,1) .or. &
                       (this%m_CascadeCenter(IBox,I) + this%m_ROutAbsorbToCent(IBox)) .GE. Host_Boxes%BOXBOUNDARY(I,2)) then
                        write(*,*) "the outer absorb sphere had existed the box "
                        write(*,*) "cascade center: ",this%m_CascadeCenter(IBox,I)
                        write(*,*) " outer absorb sphere radius: ",this%m_ROutAbsorbToCent(IBox)
                        write(*,*) "Box boundary: ",Host_Boxes%BOXBOUNDARY(I,1),Host_Boxes%BOXBOUNDARY(I,2)
                        pause
                        stop
                    end if
                END DO


                if(this%m_maxDistance(IBox) .GT. this%m_RSIADistributeToCent(IBox)) then
                    write(*,*) "MCPSCUERROR: The mostly out vac had exist the SIA distribution position",this%m_maxDistance(IBox),this%m_RSIADistributeToCent(IBox)
                    pause
                    stop
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

        this%RSIADistributeToCent_Type = other%RSIADistributeToCent_Type
        this%RSIADistributeToCent_Value = other%RSIADistributeToCent_Value

        this%ROutAbsorbToCent_Type = other%ROutAbsorbToCent_Type
        this%ROutAbsorbToCent_Value = other%ROutAbsorbToCent_Value

        this%MCCfgPath = other%MCCfgPath

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

        this%RSIADistributeToCent_Type = Capture_RSIADistributeToCent_Type_FixedValue
        this%RSIADistributeToCent_Value = 0.D0

        this%ROutAbsorbToCent_Type = Capture_ROutAbsorbToCent_Type_FixedValue
        this%ROutAbsorbToCent_Value = 0.D0

        this%MCCfgPath = ""

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
