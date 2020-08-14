module INLET_TYPEDEF_IMPLANTLIST
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none


    character(len=11),private,parameter::m_IMPFINPUTF = "&IMPFINPUTF"


    type,public::ImplantList
        type(ImplantSection)::TheImplantSection

        type(ImplantList),pointer::next=>null()

        integer::ListCount = 0

        contains
        procedure,non_overridable,public,pass::Init=>Init_ImplantList
        procedure,non_overridable,private,pass::Load_ImplantList
        procedure,non_overridable,private,pass::CheckImplantList
        procedure,non_overridable,public,pass::AppendOneSection=>AppendOne_ImplantSection
        procedure,non_overridable,public,pass::Clean=>Clean_ImplantList
        Final::CleanImplantList
    end type

    private::Init_ImplantList
    private::Load_ImplantList
    private::CheckImplantList
    private::AppendOne_ImplantSection
    private::Clean_ImplantList
    private::CleanImplantList

    contains

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
        type(ImplantList),pointer::cursor=>null()
        integer::I
        !---Body---

        cursor=>this

        DO I = 1,Host_SimuCtrlParam%NImplantSection

            if(.not. associated(cursor)) then
                write(*,*) "MCPSCUERROR: cannot load specialed section: ",Host_SimuCtrlParam%ImplantSectIDs(I)
                pause
                stop
            end if

            if(Host_SimuCtrlParam%ImplantSectIDs(I) .GE. this%ListCount) then
                write(*,*) "MCPSCUERROR: The implantation section is not special :",Host_SimuCtrlParam%ImplantSectIDs(I)
                pause
                stop
            end if


            if(cursor%TheImplantSection%InsertCountOneBatch .GT. 0.D0 .AND. Host_SimuCtrlParam%NEIGHBORUPDATESTRATEGY .eq. mp_NEIGHBORUPDATEBYNCREMIND) then
                write(*,*) "MCPSCUERROR: You cannot use the neighbor-list update strategy by clusters number remind percent when the implantation"
                write(*,*) "flux exist."
                pause
                stop
            end if


            cursor=>cursor%next

        END DO

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
        integer::CountSection
        integer::I
        !---Body---
        LINE = 0

        CountSection = 0

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
                    call tempImplantSection%Clean_ImplantSection()
                    call tempImplantSection%LoadOne_ImplantSection(hFile,Host_Boxes,Host_SimuCtrlParam,LINE)

                    CountSection = CountSection + 1

                    DO I = 1,Host_SimuCtrlParam%NImplantSection
                        if(CountSection .eq. Host_SimuCtrlParam%ImplantSectIDs(I)) then
                            call this%AppendOneSection(tempImplantSection)
                        end if
                    END DO

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

        call this%TheImplantSection%Clean_ImplantSection()

        Do while(associated(cursor))
            next=>cursor%next
            call cursor%TheImplantSection%Clean_ImplantSection()
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

end module INLET_TYPEDEF_IMPLANTLIST
