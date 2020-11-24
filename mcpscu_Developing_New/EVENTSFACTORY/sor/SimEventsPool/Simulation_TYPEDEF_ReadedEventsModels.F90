#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_READEDEVENTSMODELS
    use MiniUtilities,only:EXTRACT_SUBSTR,ISTR,GETKEYWORD
    use COMMONLIB_UTILITIES
    implicit none
    character*11,private,parameter::m_MODELFILEFLAG = "&MODELNPUTF"

    type,public::UserDefEventModels
        character(len=100),private::EventModelSymbol PREASSIGN ""
        integer,private::EventModelCode PREASSIGN -1

        contains
        procedure,non_overridable,public,pass::GetEventModelSymbol
        procedure,non_overridable,public,pass::SetEventModelSymbol
        procedure,non_overridable,public,pass::GetEventModelCode
        procedure,non_overridable,public,pass::SetEventModelCode
        procedure,non_overridable,public,pass::CopyFormOther=>CopyUserDefEventModelsFormOther
        procedure,non_overridable,public,pass::Clean=>CleanUserDefEventModels
        Generic::Assignment(=)=>CopyUserDefEventModelsFormOther
        Final::Clean_UserDefEventModels
    end type UserDefEventModels

    !****Define a list UserDefEventModelsList with value type(UserDefEventModels)*********
    DefGeneralList(UserDefEventModelsList,type(UserDefEventModels))

    !*************************************************************************************
    type,public::EventsModelsPair
        type(UserDefEventModels)::SubjectEventsModelDef
        type(UserDefEventModels)::ObjectEventsModelDef
        type(UserDefEventModels)::CrossEventsModelDef

        contains
        procedure,non_overridable,public,pass::CopyFormOther=>CopyEventsModelsPairFormOther
        procedure,non_overridable,public,pass::Clean=>CleanEventsModelsPair
        Generic::Assignment(=)=>CopyEventsModelsPairFormOther
        Final::Clean_EventsModelsPair
    end type EventsModelsPair

    !****Define a list EventsModelsPairList with value type(EventsModelsPair)**************
    DefGeneralList(EventsModelsPairList,type(EventsModelsPair))

    !**************************************************************************************
    type,public::ReadedEventsModels
        type(UserDefEventModelsList),private::TheUserDefEventModelsList

        type(EventsModelsPairList),public::TheEventsModelsPairList

        contains

        procedure,non_overridable,public,pass::Load_ReadedEventsModels
        procedure,non_overridable,private,pass::Load_UDefEventsModels
        procedure,non_overridable,private,pass::LoadOne_ReadedEventsModelsActions
        procedure,non_overridable,private,pass::Load_OneRowEventsModelsActions
        procedure,non_overridable,public,pass::CopyFormOther=>CopyReadedEventsModelsFormOther
        procedure,non_overridable,public,pass::Clean=>CleanReadedEventsModels
        Generic::Assignment(=)=>CopyReadedEventsModelsFormOther
        Final::Clean_ReadedEventsModels
    end type ReadedEventsModels

    private::GetEventModelSymbol
    private::SetEventModelSymbol
    private::GetEventModelCode
    private::SetEventModelCode
    private::CopyUserDefEventModelsFormOther
    private::CleanUserDefEventModels
    private::Clean_UserDefEventModels
    private::CopyEventsModelsPairFormOther
    private::CleanEventsModelsPair
    private::Clean_EventsModelsPair
    private::Load_ReadedEventsModels
    private::Load_UDefEventsModels
    private::LoadOne_ReadedEventsModelsActions
    private::Load_OneRowEventsModelsActions
    private::CopyReadedEventsModelsFormOther
    private::CleanReadedEventsModels
    private::Clean_ReadedEventsModels

    contains
    !**********************************************************
    function GetEventModelSymbol(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModels),intent(in)::this
        character(len=100),intent(out)::TheResult
        !---Body---

        TheResult = this%EventModelSymbol
        return
    end function GetEventModelSymbol

    !**********************************************************
    subroutine SetEventModelSymbol(this,TheSymbol)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModels)::this
        character*(*),intent(in)::TheSymbol
        !---Body---

        this%EventModelSymbol = TheSymbol
        return
    end subroutine SetEventModelSymbol

    !**********************************************************
    function GetEventModelCode(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModels),intent(in)::this
        integer,intent(out)::TheResult
        !---Body---

        TheResult = this%EventModelCode
        return
    end function GetEventModelCode

    !**********************************************************
    subroutine SetEventModelCode(this,TheCode)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModels)::this
        integer,intent(in)::TheCode
        !---Body---

        this%EventModelCode = TheCode
        return
    end subroutine SetEventModelCode

    !******************************************************************
    subroutine CopyUserDefEventModelsFormOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(UserDefEventModels),intent(out)::this
        CLASS(UserDefEventModels),intent(in)::other
        !---Body---
        this%EventModelSymbol = other%EventModelSymbol
        this%EventModelCode = other%EventModelCode
        return
    end subroutine CopyUserDefEventModelsFormOther

    !***************************************************
    subroutine CleanUserDefEventModels(this)
        implicit none
        !---Dummy Vars---
        CLASS(UserDefEventModels)::this
        !---Body---
        this%EventModelSymbol = ""
        this%EventModelCode = -1
        return
    end subroutine CleanUserDefEventModels

    !***************************************************
    subroutine Clean_UserDefEventModels(this)
        implicit none
        !---Dummy Vars---
        type(UserDefEventModels)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine Clean_UserDefEventModels

    !****The member function of list UserDefEventModelsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(UserDefEventModelsList,type(UserDefEventModels),Clean)

    !******************************************************************
    subroutine CopyEventsModelsPairFormOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(EventsModelsPair),intent(out)::this
        CLASS(EventsModelsPair),intent(in)::other
        !---Body---
        this%SubjectEventsModelDef = other%SubjectEventsModelDef
        this%ObjectEventsModelDef = other%ObjectEventsModelDef
        this%CrossEventsModelDef = other%CrossEventsModelDef
        return
    end subroutine CopyEventsModelsPairFormOther

    !***************************************************
    subroutine CleanEventsModelsPair(this)
        implicit none
        !---Dummy Vars---
        CLASS(EventsModelsPair)::this
        !---Body---
        call this%SubjectEventsModelDef%Clean()
        call this%ObjectEventsModelDef%Clean()
        call this%CrossEventsModelDef%Clean()
        return
    end subroutine CleanEventsModelsPair

    !***************************************************
    subroutine Clean_EventsModelsPair(this)
        implicit none
        !---Dummy Vars---
        type(EventsModelsPair)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine Clean_EventsModelsPair

    !****The member function of list CollectionEventsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(EventsModelsPairList,type(EventsModelsPair),Clean)

    !*****************************************************
    subroutine Load_ReadedEventsModels(this,hFile)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedEventsModels)::this
        integer,intent(in)::hFile
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*100::KEYWORD
        !---Body---

        call this%TheEventsModelsPairList%CleanList()

        call GETINPUTSTRLINE(hFile,STR,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        if(KEYWORD(1:LENTRIM(KEYWORD)) .ne. m_MODELFILEFLAG) then
            write(*,*) "MCPSCUERROR: The start flag of model input file must be: ",m_MODELFILEFLAG
            write(*,*) "However, what you specialed is: ",KEYWORD(1:LENTRIM(KEYWORD))
            pause
            stop
        end if

        DO while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")
            STR = adjustl(STR)

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDMODELNPUTF")
                    exit
                case("&UDEFMODELSUBCTL")
                    call this%Load_UDefEventsModels(hFile,*100)
                case("&MODELACTIONSSUBCTL")
                    call this%LoadOne_ReadedEventsModelsActions(hFile,*100)
                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "Please check model ctrl file"
                    pause
                    stop
            end select
        END DO

        return
        100 write(*,*) "MCPSCUERROR: Error when read the event models file, at LINE: ",LINE
            write(*,*) STR
            pause
            stop
    end subroutine Load_ReadedEventsModels

    !******************************************************************
    subroutine Load_UDefEventsModels(this,hFile,*)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedEventsModels)::this
        integer,intent(in)::hFile
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*100::KEYWORD
        character*100::STRTMP(10)
        type(UserDefEventModelsList),pointer::cursor=>null()
        type(UserDefEventModels)::tempUserDefEventModels
        type(UserDefEventModels)::newUserDefEventModels
        integer::I
        integer::N
        !---Body---

        call this%TheUserDefEventModelsList%CleanList()

        DO while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")
            STR = adjustl(STR)

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&MODELDEF")

                    call newUserDefEventModels%Clean()

                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode."
                        write(*,*) "By the way:  &MODELDEF       Name =   , unicode = "
                        pause
                        stop
                    end if

                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))
                    
                    call newUserDefEventModels%SetEventModelSymbol(STRTMP(1)(1:LENTRIM(STRTMP(1))))

                    call EXTRACT_NUMB(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode."
                        write(*,*) "By the way:  &MODELDEF       Name =   , unicode = "
                        pause
                        stop
                    end if

                    call newUserDefEventModels%SetEventModelCode(ISTR(STRTMP(1)))

                    DO I = 1,this%TheUserDefEventModelsList%GetListCount()
                        !---Use safe way to access the list---
                        tempUserDefEventModels = this%TheUserDefEventModelsList%GetValueByListIndex(I)

                        if(ISSTREQUAL(tempUserDefEventModels%GetEventModelSymbol(),newUserDefEventModels%GetEventModelSymbol()) .or. &
                           tempUserDefEventModels%GetEventModelCode() .eq. newUserDefEventModels%GetEventModelCode()) then
                            write(*,*) "MCPSCUERROR: The event model had been defined , cannot be redefineded."
                            write(*,*) tempUserDefEventModels%GetEventModelSymbol(),tempUserDefEventModels%GetEventModelCode()
                            write(*,*) newUserDefEventModels%GetEventModelSymbol(),newUserDefEventModels%GetEventModelCode()
                            pause
                            stop
                        end if
                    END DO

                    call this%TheUserDefEventModelsList%AppendOne(newUserDefEventModels)

                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag, for model define: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "Please check model ctrl file"
                    pause
                    stop
            end select
        END DO

        return

        100 return 1
    end  subroutine Load_UDefEventsModels

    !******************************************************************
    subroutine LoadOne_ReadedEventsModelsActions(this,hFile,*)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedEventsModels)::this
        integer,intent(in)::hFile
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*100::KEYWORD
        character*100::STRTMP(10)
        character*100,dimension(:),allocatable::ModelsName
        integer::N
        integer::NCol
        integer::I
        integer::J
        type(UserDefEventModels)::tempUserDefEventModels
        type(UserDefEventModels)::newUserDefEventModels
        type(UserDefEventModelsList),pointer::HeadEventModels
        type(UserDefEventModelsList),pointer::cursor=>null()
        logical::Finded
        !---Body---
        DO while(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            call RemoveComments(STR,"!")
            STR = adjustl(STR)

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&MODELSNAMEHEAD")
                    NCol = GetSubStr_Count(STR)

                    if(NCol .LE. 0) then
                        write(*,*) "MCPSCUERROR: you must special at least one model name in &MODELSNAMEHEAD"
                        write(*,*) STR
                        pause
                        stop
                    end if

                    allocate(ModelsName(NCol))

                    call EXTRACT_SUBSTR(STR,NCol,N,ModelsName)

                    if(N .ne. NCol) then
                        write(*,*) "MCPSCUERROR: The discrepancy appeared between function EXTRACT_SUBSTR and GetSubStr_Count to get substr number"
                        write(*,*) NCol
                        write(*,*) N
                        pause
                        stop
                    end if

                    call HeadEventModels%CleanList()
                    
                    DO I = 1,NCol
                        call newUserDefEventModels%Clean()

                        Finded = .false.

                        ModelsName(I) = adjustl(ModelsName(I))

                        call UPCASE(ModelsName(I)(1:LENTRIM(ModelsName(I))))

                        DO J = 1,this%TheUserDefEventModelsList%GetListCount()
                            !---Use safe way to access the list---
                            tempUserDefEventModels = this%TheUserDefEventModelsList%GetValueByListIndex(J)

                            if(ISSTREQUAL(tempUserDefEventModels%GetEventModelSymbol(),ModelsName(I)(1:LENTRIM(ModelsName(I))))) then
                                !---The Assignment(=) had been overridable---
                                newUserDefEventModels = tempUserDefEventModels
                                Finded = .true.
                                exit
                            end if
                        END DO

                        if(.not. Finded) then
                            write(*,*) "MCPSCUERROR: The Model used had not been defineded: ",ModelsName(I)(1:LENTRIM(ModelsName(I)))
                            pause
                            stop
                        end if

                        !---Check dumplicate----
                        cursor=>HeadEventModels
                        DO J = 1,HeadEventModels%GetListCount()
                            if(cursor%TheValue%GetEventModelCode() .eq. newUserDefEventModels%GetEventModelCode()) then
                                write(*,*) "MCPSCUERROR: The model name is used , cannot be used again."
                                write(*,*) cursor%TheValue%GetEventModelCode()
                                write(*,*) newUserDefEventModels%GetEventModelCode()
                                pause
                                stop
                            end if
                            cursor=>cursor%Next
                        END DO

                        call HeadEventModels%AppendOne(newUserDefEventModels)
                    END DO

                    if(allocated(ModelsName)) then
                        deallocate(ModelsName)
                    end if

                    DO I = 1,NCol
                        call this%Load_OneRowEventsModelsActions(hFile,HeadEventModels,*100)
                    END DO

                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "Please check model ctrl file"
                    pause
                    stop
            end select
        END DO

        Nullify(cursor)
        cursor=>null()

        return

        100 return 1
    end subroutine LoadOne_ReadedEventsModelsActions

    !******************************************************************
    subroutine Load_OneRowEventsModelsActions(this,hFile,HeadCodeList,*)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedEventsModels)::this
        integer,intent(in)::hFile
        type(UserDefEventModelsList),target::HeadCodeList
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*100::KEYWORD
        character*100::STRTMP(10)
        integer,dimension(:),allocatable::CodeArray
        character*100,dimension(:),allocatable::CodeSTRArray
        integer::N
        integer::I
        integer::J
        type(UserDefEventModels)::tempUserDefEventModels
        type(EventsModelsPair)::newEventsModelsPair
        type(EventsModelsPair)::tempEventsModelsPair
        logical::Finded
        type(UserDefEventModelsList),pointer::cursor=>null()
        integer::tempCode
        !---Body---
        call newEventsModelsPair%Clean()

        if(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!")) then
            call RemoveComments(STR,"!")
            STR = adjustl(STR)

            if(LENTRIM(STR) .LE. 0) then
                return
            end if

            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&MODELACTIONSSUBCTL")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the Model Name for &MODELACTIONSSUBCTL setting."
                        write(*,*) STR
                        pause
                        stop
                    end if

                    STRTMP(1) = adjustl(STRTMP(1))

                    call UPCASE(STRTMP(1)(1:LENTRIM(STRTMP(1))))

                    Finded = .false.

                    DO I = 1,this%TheUserDefEventModelsList%GetListCount()
                        !---Use safe way to access the list---
                        tempUserDefEventModels = this%TheUserDefEventModelsList%GetValueByListIndex(I)

                        if(ISSTREQUAL(tempUserDefEventModels%GetEventModelSymbol(),STRTMP(1)(1:LENTRIM(STRTMP(1))))) then
                            newEventsModelsPair%SubjectEventsModelDef = tempUserDefEventModels
                            tempCode = tempUserDefEventModels%GetEventModelCode()
                            Finded = .true.
                            exit
                        end if
                    END DO

                    if(.not. Finded) then
                        write(*,*) "MCPSCUERROR: The Model used had not been defineded: ",STRTMP(1)(1:LENTRIM(STRTMP(1)))
                        write(*,*) STR
                        pause
                        stop
                    end if

                    !---Check whether is located in MODELSNAMEHEAD----
                    Finded = .false.

                    cursor=>HeadCodeList
                    DO I = 1,HeadCodeList%GetListCount()
                        if(cursor%TheValue%GetEventModelCode() .eq. tempCode) then
                            Finded = .true.
                            exit
                        end if
                        cursor=>cursor%Next
                    END DO

                    if(.not. Finded) then
                        write(*,*) "MCPSCUERROR: The Model used in row had not been used in head: ",STRTMP(1)(1:LENTRIM(STRTMP(1)))
                        write(*,*) STR
                        pause
                        stop
                    end if

                    if(HeadCodeList%GetListCount() .GT. 0) then
                        allocate(CodeSTRArray(HeadCodeList%GetListCount()))
                        call AllocateArray_Host(CodeArray,HeadCodeList%GetListCount(),"CodeArray")
                        CodeArray = 0
                    else
                        write(*,*) "MCPSCUERROR: The Head code List less than 0."
                        pause
                        stop
                    end if

                    call EXTRACT_NUMB(STR,HeadCodeList%GetListCount(),N,CodeSTRArray)

                    if(N .LT. HeadCodeList%GetListCount()) then
                        write(*,*) "MCPSCUERROR: In each row ,you must special same event code number with head."
                        write(*,*) "Head event number is : ",HeadCodeList%GetListCount()
                        write(*,*) "Event number in this row: ",N
                        write(*,*) STR
                        pause
                        stop
                    end if

                    DO I = 1,HeadCodeList%GetListCount()

                        CodeArray(I) = ISTR(CodeSTRArray(I))

                        !---Check whether is located in Defined Model code----
                        Finded = .false.

                        DO J = 1,this%TheUserDefEventModelsList%GetListCount()
                            !---Use safe way to access the list---
                            tempUserDefEventModels = this%TheUserDefEventModelsList%GetValueByListIndex(J)

                            if(tempUserDefEventModels%GetEventModelCode() .eq. CodeArray(I)) then
                                newEventsModelsPair%CrossEventsModelDef = tempUserDefEventModels
                                Finded = .true.
                                exit
                            end if
                        END DO

                        if(.not. Finded) then
                            write(*,*) "MCPSCUERROR: The Model code used had not been defineded: ",CodeArray(I)
                            write(*,*) STR
                            pause
                            stop
                        end if

                        newEventsModelsPair%ObjectEventsModelDef = HeadCodeList%GetValueByListIndex(I)

                        call this%TheEventsModelsPairList%AppendOne(newEventsModelsPair)
                    END DO

                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "Please check model ctrl file"
                    write(*,*) "The reason is caused by that the spceialed rows count is less than colum number."
                    pause
                    stop
            end select
        end if

        call DeAllocateArray_Host(CodeArray, "CodeArray") 
        deallocate(CodeSTRArray)
       
        Nullify(cursor)
        cursor=>null()

        return
        100 return 1
    end subroutine Load_OneRowEventsModelsActions

    !******************************************************************
    subroutine CopyReadedEventsModelsFormOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedEventsModels),intent(out)::this
        CLASS(ReadedEventsModels),intent(in)::other
        !---Body---
        !---Assignment(=) had been overrided-------
        this%TheUserDefEventModelsList = other%TheUserDefEventModelsList
        this%TheEventsModelsPairList = other%TheEventsModelsPairList
        return
    end subroutine CopyReadedEventsModelsFormOther

    !***************************************************
    subroutine CleanReadedEventsModels(this)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedEventsModels)::this
        !---Body---
        call this%TheUserDefEventModelsList%CleanList()
        call this%TheEventsModelsPairList%CleanList()
        return
    end subroutine CleanReadedEventsModels

    !***************************************************
    subroutine Clean_ReadedEventsModels(this)
        implicit none
        !---Dummy Vars---
        type(ReadedEventsModels)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine Clean_ReadedEventsModels

end module SIMULATION_TYPEDEF_READEDEVENTSMODELS