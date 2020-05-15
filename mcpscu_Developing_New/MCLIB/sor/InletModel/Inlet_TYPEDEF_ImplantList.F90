module INLET_TYPEDEF_IMPLANTLIST
    use INLET_TYPEDEF_IMPLANTSECTION
    use INLET_BATCHIMPLANTATION_GPU
    use INLET_CONTINUEIMPLANTATION_GPU
    implicit none

    integer,parameter,private::p_ImplantModelType_Continue = 0      ! the implant cluster number is a line function of evolution time
    integer,parameter,private::p_ImplantModelType_Batch = 1         ! the implant cluster number is a step function of evolution time


    type(ContinueImplantList),private,target::m_ContinueImplantList
    type(BatchImplantList),private,target::m_BatchImplantList


    character(len=11),private,parameter::m_IMPFINPUTF = "&IMPFINPUTF"


    type,public::ImplantList
        CLASS(ImplantSection),pointer::p_ImplantSection=>null()

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

    private::Init_ImplantList
    private::Load_ImplantList
    private::CheckImplantList
    private::AppendOne_ImplantSection
    private::GetImplantSection_P
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
        type(SimulationCtrlParam),pointer::PSimuCtrlParamCursor=>Null()
        CLASS(ImplantSection),pointer::PImplantSection=>null()
        integer::ICount
        !---Body---
        PSimuCtrlParamCursor=>Host_SimuCtrlParam

        ICount = 0
        DO While(associated(PSimuCtrlParamCursor))
            ICount = ICount + 1

            if(PSimuCtrlParamCursor%ImplantSectID .GE. 1) then
                PImplantSection=>this%Get_P(PSimuCtrlParamCursor%ImplantSectID)

                if(.not. associated(PImplantSection)) then
                    write(*,*) "MCPSCUERROR: The implantation section is not special :",PSimuCtrlParamCursor%ImplantSectID
                    write(*,*) "For the simulation section :",ICount
                    pause
                    stop
                end if

            end if

            PSimuCtrlParamCursor=>PSimuCtrlParamCursor%next
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
        type(ContinueImplantSection)::tempContinueImplantSection
        type(BatchImplantSection)::tempBatchImplantSection
        character*256::truePath
        character*256::STR
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
                    call tempContinueImplantSection%Clean()

                    tempContinueImplantSection%ImplantModel = p_ImplantModelType_Continue
                    call tempContinueImplantSection%Load_ImplantSection(hFile,Host_Boxes,Host_SimuCtrlParam,LINE)
!
!                    call m_ContinueImplantList%AppendOneSection(tempContinueImplantSection)
!
!                    call this%AppendOneSection(m_ContinueImplantList%GetP_LastSection())
!
                case("&CONTINUESUBCTL")
                    call tempContinueImplantSection%Clean()

                    tempContinueImplantSection%ImplantModel = p_ImplantModelType_Continue
                    call tempContinueImplantSection%Load_ImplantSection(hFile,Host_Boxes,Host_SimuCtrlParam,LINE)
!
!                    call m_ContinueImplantList%AppendOneSection(tempContinueImplantSection)
!
!                    call this%AppendOneSection(m_ContinueImplantList%GetP_LastSection())
!
                case("&BATCHSUBCTL")
                    call tempBatchImplantSection%Clean()

                    tempContinueImplantSection%ImplantModel = p_ImplantModelType_Batch
                    call tempBatchImplantSection%Load_ImplantSection(hFile,Host_Boxes,Host_SimuCtrlParam,LINE)
!
!                    call m_BatchImplantList%AppendOneSection(tempBatchImplantSection)
!
!                    call this%AppendOneSection(m_BatchImplantList%GetP_LastSection())

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
        CLASS(ImplantSection),pointer::TheImplantSection
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
            this%p_ImplantSection => TheImplantSection
        else
            cursor=>this
            next=>cursor%next

            Do While(associated(next))
                cursor=>next
                next=>cursor%next
            End Do

            allocate(next)
            ! The assignment(=) had been override
            next%p_ImplantSection => TheImplantSection
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
        CLASS(ImplantSection),intent(out),pointer::TheResult
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
                TheResult=>cursor%p_ImplantSection
                exit
            end if

            cursor=>cursor%next
        END DO

        Nullify(cursor)

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

        if(associated(this%p_ImplantSection)) then
            call this%p_ImplantSection%Clean()
        end if

        Do while(associated(cursor))
            next=>cursor%next

            if(associated(cursor%p_ImplantSection)) then
                call cursor%p_ImplantSection%Clean()
            end if

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
