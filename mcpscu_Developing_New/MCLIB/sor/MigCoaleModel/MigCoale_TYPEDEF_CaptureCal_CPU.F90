module MIGCOALE_TYPEDEF_CAPTURECAL_CPU
    use MCLIB_CONSTANTS
    use MCLIB_UTILITIES
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    implicit none

    integer,parameter::CaptureGenWay_ByCentUniform_Locally = 0
    integer,parameter::CaptureGenWay_ByMCConfig_Locally_Directly = 1

    type,public::CaptureCal
        integer::CaptureGenerateWay = CaptureGenWay_ByMCConfig_Locally_Directly
        integer::NSIA = 0
        integer::SIASIZE = 1
        real(kind=KINDDF)::RSIADistributeToCent = 0.D0
        real(kind=KINDDF)::ROutAbsorbToCent = 0.D0
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
        this%RSIADistributeToCent = 0.D0
        this%ROutAbsorbToCent = 0.D0

        this%MCCfgPath = ""

        call DeAllocateArray_Host(this%m_CascadeCenter,"m_CascadeCenter")
        call AllocateArray_Host(this%m_CascadeCenter,MultiBox,3,"m_CascadeCenter")

        call DeAllocateArray_Host(this%m_maxDistance,"m_maxDistance")
        call AllocateArray_Host(this%m_maxDistance,MultiBox,"m_maxDistance")

        call DeAllocateArray_Host(this%m_NVACInEachBox,"m_NVACInEachBox")
        call AllocateArray_Host(this%m_NVACInEachBox,MultiBox,"m_NVACInEachBox")

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
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the distance radius from cascade center to SIA distributed sphere."
                        pause
                        stop
                    end if
                    this%RSIADistributeToCent = DRSTR(STRTMP(1))*Host_Boxes%LatticeLength

                    if(this%RSIADistributeToCent .LE. 0) then
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
                    this%ROutAbsorbToCent = DRSTR(STRTMP(1))*Host_Boxes%LatticeLength

                    if(this%ROutAbsorbToCent .LE. 0) then
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


        if(this%RSIADistributeToCent .GT. this%ROutAbsorbToCent) then
            write(*,*) "MCPSCUERROR: The SIA distribution to cascade center distance cannot greater than OutSide Absorb radius",this%RSIADistributeToCent,this%ROutAbsorbToCent
            pause
            stop
        end if


        close(hFile)
        return
    end subroutine ResolveCapCtrlFile_FormMCConfig

    subroutine ResolveCapInfoFile(this)
        implicit none
        !---Dummy Vars---
        CLASS(CaptureCal)::this

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
        this%RSIADistributeToCent = other%RSIADistributeToCent
        this%ROutAbsorbToCent = other%ROutAbsorbToCent

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
        this%RSIADistributeToCent = 0.D0
        this%ROutAbsorbToCent = 0.D0

        this%MCCfgPath = ""

        call DeAllocateArray_Host(this%m_CascadeCenter,"m_CascadeCenter")

        call DeAllocateArray_Host(this%m_maxDistance,"m_maxDistance")

        call DeAllocateArray_Host(this%m_NVACInEachBox,"m_NVACInEachBox")

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
