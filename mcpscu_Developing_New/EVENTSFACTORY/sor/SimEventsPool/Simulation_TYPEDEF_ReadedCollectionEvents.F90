#include "../../../MACRO/Macro"
module SIMULATION_TYPEDEF_READEDCOLLECTIONEVENTS
    use MiniUtilities,only:EXTRACT_SUBSTR,ISTR,GETKEYWORD
    use COMMONLIB_UTILITIES
    implicit none
    character*11,private,parameter::m_MODELFILEFLAG = "&MODELNPUTF"

    integer,public,parameter::p_ModelType_Single = 0
    integer,public,parameter::p_ModelType_Cross = 1

    type,public::UserDefObjectCollection
        character(len=100),private::ObjectCollectionSymbol PREASSIGN ""
        character(len=100),private::ObjectCollectionCode PREASSIGN ""

        contains
        procedure,non_overridable,public,pass::GetObjectCollectionSymbol
        procedure,non_overridable,public,pass::SetObjectCollectionSymbol
        procedure,non_overridable,public,pass::GetObjectCollectionCode
        procedure,non_overridable,public,pass::SetObjectCollectionCode
        procedure,non_overridable,public,pass::CopyFormOther=>CopyUserDefObjectCollection
        procedure,non_overridable,public,pass::Clean=>CleanUserDefObjectCollection
        Generic::Assignment(=)=>CopyUserDefObjectCollection
        Final::Clean_UserDefObjectCollection
    end type UserDefObjectCollection

    !****Define a list UserDefObjectCollectionsList with value type(UserDefObjectCollection)*********
    DefGeneralList(UserDefObjectCollectionsList,type(UserDefObjectCollection))

    type,public::UserDefEventModel
        character(len=100),private::EventModelSymbol PREASSIGN ""
        integer,private::EventModelCode PREASSIGN -1
        integer,private::ModelType PREASSIGN p_ModelType_Single
        type(UserDefObjectCollection)::RelativeData(2)
        contains
        procedure,non_overridable,public,pass::GetEventModelSymbol
        procedure,non_overridable,public,pass::SetEventModelSymbol
        procedure,non_overridable,public,pass::GetEventModelCode
        procedure,non_overridable,public,pass::SetEventModelCode
        procedure,non_overridable,public,pass::GetEventModelType
        procedure,non_overridable,public,pass::SetEventModelType
        procedure,non_overridable,public,pass::CopyFormOther=>CopyUserDefEventModelFormOther
        procedure,non_overridable,public,pass::Clean=>CleanUserDefEventModel
        Generic::Assignment(=)=>CopyUserDefEventModelFormOther
        Final::Clean_UserDefEventModel
    end type UserDefEventModel

    !****Define a list UserDefEventModelsList with value type(UserDefEventModel)*********
    DefGeneralList(UserDefEventModelsList,type(UserDefEventModel))

    !*************************************************************************************
    type,public::EventsModelsPair
        type(UserDefEventModel)::SubjectEventsModelDef
        type(UserDefEventModel)::ObjectEventsModelDef
        type(UserDefEventModel)::CrossEventsModelDef

        contains
        procedure,non_overridable,public,pass::CopyFormOther=>CopyEventsModelsPairFormOther
        procedure,non_overridable,public,pass::Clean=>CleanEventsModelsPair
        Generic::Assignment(=)=>CopyEventsModelsPairFormOther
        Final::Clean_EventsModelsPair
    end type EventsModelsPair

    !****Define a list EventsModelsPairList with value type(EventsModelsPair)**************
    DefGeneralList(EventsModelsPairList,type(EventsModelsPair))

    !**************************************************************************************
    type,public::ReadedCollectionEvents
        type(UserDefObjectCollectionsList),private::TheUserDefObjectCollectionsList
        type(UserDefEventModelsList),private::TheUserDefEventModelsList

        type(EventsModelsPairList),public::TheEventsModelsPairList

        contains

        procedure,non_overridable,public,pass::Load_ReadedCollectionEvents
        procedure,non_overridable,private,pass::Load_UDefObjectCollections
        procedure,non_overridable,private,pass::Load_UDefEventsModels
        procedure,non_overridable,private,pass::LoadOne_ReadedEventsModelsActions
        procedure,non_overridable,private,pass::Load_OneRowEventsModelsActions
        procedure,non_overridable,public,pass::CopyFormOther=>CopyReadedCollectionEventsFormOther
        procedure,non_overridable,public,pass::Clean=>CleanReadedCollectionEvents
        Generic::Assignment(=)=>CopyReadedCollectionEventsFormOther
        Final::Clean_ReadedCollectionEvents
    end type ReadedCollectionEvents


    private::GetObjectCollectionSymbol
    private::SetObjectCollectionSymbol
    private::GetObjectCollectionCode
    private::SetObjectCollectionCode
    private::CopyUserDefObjectCollection
    private::CleanUserDefObjectCollection
    private::Clean_UserDefObjectCollection
    private::GetEventModelSymbol
    private::SetEventModelSymbol
    private::GetEventModelCode
    private::SetEventModelCode
    private::GetEventModelType
    private::SetEventModelType
    private::CopyUserDefEventModelFormOther
    private::CleanUserDefEventModel
    private::Clean_UserDefEventModel
    private::CopyEventsModelsPairFormOther
    private::CleanEventsModelsPair
    private::Clean_EventsModelsPair
    private::Load_ReadedCollectionEvents
    private::Load_UDefObjectCollections
    private::Load_UDefEventsModels
    private::LoadOne_ReadedEventsModelsActions
    private::Load_OneRowEventsModelsActions
    private::CopyReadedCollectionEventsFormOther
    private::CleanReadedCollectionEvents
    private::Clean_ReadedCollectionEvents

    contains

    !**************************************************************************
    function GetObjectCollectionSymbol(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefObjectCollection),intent(in)::this
        character(len=100),intent(out)::TheResult
        !---Body---

        TheResult = this%ObjectCollectionSymbol
        return
    end function GetObjectCollectionSymbol

    !**************************************************************************
    subroutine SetObjectCollectionSymbol(this,TheSymbol)
        implicit none
        !---Dummy Vars---
        Class(UserDefObjectCollection)::this
        character*(*),intent(in)::TheSymbol
        !---Body---

        this%ObjectCollectionSymbol = TheSymbol
        return
    end subroutine SetObjectCollectionSymbol


    !**************************************************************************
    function GetObjectCollectionCode(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefObjectCollection),intent(in)::this
        character(len=100),intent(out)::TheResult
        !---Body---

        TheResult = this%ObjectCollectionCode
        return
    end function GetObjectCollectionCode

    !**************************************************************************
    subroutine SetObjectCollectionCode(this,TheSymbol)
        implicit none
        !---Dummy Vars---
        Class(UserDefObjectCollection)::this
        character*(*),intent(in)::TheSymbol
        !---Body---

        this%ObjectCollectionCode = TheSymbol
        return
    end subroutine SetObjectCollectionCode


    !***************************************************************************
    subroutine CopyUserDefObjectCollection(this,other)
        implicit none
        !---Dummy Vars---
        Class(UserDefObjectCollection),intent(out)::this
        Class(UserDefObjectCollection),intent(in)::other
        !---Body---
        this%ObjectCollectionSymbol = other%ObjectCollectionSymbol
        this%ObjectCollectionCode = other%ObjectCollectionCode
        return
    end subroutine CopyUserDefObjectCollection

    !*************************************************************************
    subroutine CleanUserDefObjectCollection(this)
        implicit none
        !---Dummy Vars---
        Class(UserDefObjectCollection)::this
        !---Body---
        this%ObjectCollectionSymbol = ""
        this%ObjectCollectionCode = ""
        return
    end subroutine CleanUserDefObjectCollection
    
    !*************************************************************************
    subroutine Clean_UserDefObjectCollection(this)
        implicit none
        !---Dummy Vars---
        type(UserDefObjectCollection)::this
        !---Body---
        call this%Clean()
        return
    end subroutine Clean_UserDefObjectCollection

    !****The member function of list UserDefObjectCollectionsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(UserDefObjectCollectionsList,type(UserDefObjectCollection),Clean)

    !**********************************************************
    function GetEventModelSymbol(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModel),intent(in)::this
        character(len=100),intent(out)::TheResult
        !---Body---

        TheResult = this%EventModelSymbol
        return
    end function GetEventModelSymbol

    !**********************************************************
    subroutine SetEventModelSymbol(this,TheSymbol)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModel)::this
        character*(*),intent(in)::TheSymbol
        !---Body---

        this%EventModelSymbol = TheSymbol
        return
    end subroutine SetEventModelSymbol

    !**********************************************************
    function GetEventModelCode(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModel),intent(in)::this
        integer,intent(out)::TheResult
        !---Body---

        TheResult = this%EventModelCode
        return
    end function GetEventModelCode

    !**********************************************************
    subroutine SetEventModelCode(this,TheCode)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModel)::this
        integer,intent(in)::TheCode
        !---Body---

        this%EventModelCode = TheCode
        return
    end subroutine SetEventModelCode

    !**********************************************************
    function GetEventModelType(this) result(TheResult)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModel),intent(in)::this
        integer,intent(out)::TheResult
        !---Body---

        TheResult = this%ModelType
        return
    end function GetEventModelType

    !**********************************************************
    subroutine SetEventModelType(this,TheType)
        implicit none
        !---Dummy Vars---
        Class(UserDefEventModel)::this
        integer,intent(in)::TheType
        !---Body---

        this%ModelType = TheType
        return
    end subroutine SetEventModelType

    !******************************************************************
    subroutine CopyUserDefEventModelFormOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(UserDefEventModel),intent(out)::this
        CLASS(UserDefEventModel),intent(in)::other
        !---Local Vars---
        integer::I
        !---Body---
        this%EventModelSymbol = other%EventModelSymbol
        this%EventModelCode = other%EventModelCode
        this%ModelType = other%ModelType

        DO I = 1,size(other%RelativeData)
            this%RelativeData(I) = other%RelativeData(I)
        END DO
        return
    end subroutine CopyUserDefEventModelFormOther

    !***************************************************
    subroutine CleanUserDefEventModel(this)
        implicit none
        !---Dummy Vars---
        CLASS(UserDefEventModel)::this
        !---Local Vars---
        integer::I
        !---Body---
        this%EventModelSymbol = ""
        this%EventModelCode = -1
        this%ModelType = p_ModelType_Single

        DO I = 1,size(this%RelativeData)
            call this%RelativeData(I)%Clean()
        END DO
        return
    end subroutine CleanUserDefEventModel

    !***************************************************
    subroutine Clean_UserDefEventModel(this)
        implicit none
        !---Dummy Vars---
        type(UserDefEventModel)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine Clean_UserDefEventModel

    !****The member function of list UserDefEventModelsList**************
    DefGeneralListFuncSpan_WithValueCleanMethod(UserDefEventModelsList,type(UserDefEventModel),Clean)

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
    subroutine Load_ReadedCollectionEvents(this,hFile)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedCollectionEvents)::this
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
                case("&UDEFDATASUBCTL")
                    call this%Load_UDefObjectCollections(hFile,*100)
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
    end subroutine Load_ReadedCollectionEvents


    !******************************************************************
    subroutine Load_UDefObjectCollections(this,hFile,*)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedCollectionEvents)::this
        integer,intent(in)::hFile
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*100::KEYWORD
        character*100::STRTMP(10)
        type(UserDefObjectCollection)::tempUserDefObjectCollection
        type(UserDefObjectCollection)::newUserDefObjectCollection
        integer::I
        integer::N
        logical::Finded
        !---Body---

        call this%TheUserDefObjectCollectionsList%CleanList()

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
                case("&DATADEF")

                    call newUserDefObjectCollection%Clean()

                    call EXTRACT_SUBSTR(STR,2,N,STRTMP)

                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: Too few parameters for &DATADEF setting, you must special a model name and unicode."
                        write(*,*) "By the way:  &DATADEF       Name =   , unicode = "
                        pause
                        stop
                    end if

                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))
                    
                    call newUserDefObjectCollection%SetObjectCollectionSymbol(STRTMP(1)(1:LENTRIM(STRTMP(1))))

                    call newUserDefObjectCollection%SetObjectCollectionCode(adjustl(trim(STRTMP(2))))

                    DO I = 1,this%TheUserDefObjectCollectionsList%GetListCount()
                        !---Use safe way to access the list---
                        tempUserDefObjectCollection = this%TheUserDefObjectCollectionsList%GetValueByListIndex(I)

                        if(ISSTREQUAL(tempUserDefObjectCollection%GetObjectCollectionSymbol(),newUserDefObjectCollection%GetObjectCollectionSymbol()) .or. &
                           ISSTREQUAL(tempUserDefObjectCollection%GetObjectCollectionCode(),newUserDefObjectCollection%GetObjectCollectionCode())) then
                            write(*,*) "MCPSCUERROR: The data had been defined , cannot be redefineded."
                            write(*,*) tempUserDefObjectCollection%GetObjectCollectionSymbol(),newUserDefObjectCollection%GetObjectCollectionSymbol()
                            write(*,*) tempUserDefObjectCollection%GetObjectCollectionCode(),newUserDefObjectCollection%GetObjectCollectionCode()
                            pause
                            stop
                        end if
                    END DO

                    call this%TheUserDefObjectCollectionsList%AppendOne(newUserDefObjectCollection)

                case default
                    write(*,*) "MCPSCUERROR: The Illegal flag, for model define: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "Please check model ctrl file"
                    pause
                    stop
            end select
        END DO

        return

        100 return 1
    end  subroutine Load_UDefObjectCollections

    !******************************************************************
    subroutine Load_UDefEventsModels(this,hFile,*)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedCollectionEvents)::this
        integer,intent(in)::hFile
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*100::KEYWORD
        character*100::STRTMP(10)
        type(UserDefEventModel)::tempUserDefEventModel
        type(UserDefEventModel)::newUserDefEventModel
        type(UserDefObjectCollection)::tempUserDefObjectCollection
        integer::I
        integer::N
        logical::Finded
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

                    call newUserDefEventModel%Clean()

                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode."
                        write(*,*) "By the way:  &MODELDEF       Name =   , unicode = "
                        pause
                        stop
                    end if

                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))
                    
                    call newUserDefEventModel%SetEventModelSymbol(STRTMP(1)(1:LENTRIM(STRTMP(1))))

                    call EXTRACT_NUMB(STR,1,N,STRTMP)

                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode."
                        write(*,*) "By the way:  &MODELDEF       Name =   , unicode = "
                        pause
                        stop
                    end if

                    call newUserDefEventModel%SetEventModelCode(ISTR(STRTMP(1)))

                    DO I = 1,this%TheUserDefEventModelsList%GetListCount()
                        !---Use safe way to access the list---
                        tempUserDefEventModel = this%TheUserDefEventModelsList%GetValueByListIndex(I)

                        if(ISSTREQUAL(tempUserDefEventModel%GetEventModelSymbol(),newUserDefEventModel%GetEventModelSymbol()) .or. &
                           tempUserDefEventModel%GetEventModelCode() .eq. newUserDefEventModel%GetEventModelCode()) then
                            write(*,*) "MCPSCUERROR: The event model had been defined , cannot be redefineded."
                            write(*,*) tempUserDefEventModel%GetEventModelSymbol(),tempUserDefEventModel%GetEventModelCode()
                            write(*,*) newUserDefEventModel%GetEventModelSymbol(),newUserDefEventModel%GetEventModelCode()
                            pause
                            stop
                        end if
                    END DO


                    call EXTRACT_SUBSTR(STR,2,N,STRTMP) 

                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode ,type and data "
                        write(*,*) STR
                        pause
                        stop
                    end if

                    STRTMP(2) = adjustl(trim(STRTMP(2)))
                    call UPCASE(STRTMP(2))

                    select case(STRTMP(2)(1:LENTRIM(STRTMP(2))))
                        case("SINGLE")
                            call newUserDefEventModel%SetEventModelType(p_ModelType_Single)
                        case("CROSS")
                            call newUserDefEventModel%SetEventModelType(p_ModelType_Cross)
                        case default
                            write(*,*) "MCPSCUERROR: The evnet type can only be single or cross"
                            write(*,*) "However, what you specialed is: ",STRTMP(2)(1:LENTRIM(STRTMP(2)))
                            pause
                            stop
                    end select

                    select case(newUserDefEventModel%GetEventModelType())
                        case(p_ModelType_Single)
                            call EXTRACT_SUBSTR(STR,3,N,STRTMP) 

                            if(N .LT. 3) then
                                write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode ,type and data "
                                write(*,*) STR
                                pause
                                stop
                            end if

                            STRTMP(3) = adjustl(trim(STRTMP(3)))
                            call UPCASE(STRTMP(3))
                            Finded = .false.
                            DO I = 1,this%TheUserDefObjectCollectionsList%GetListCount()
                                tempUserDefObjectCollection = this%TheUserDefObjectCollectionsList%GetValueByListIndex(I)

                                if(ISSTREQUAL(STRTMP(3),tempUserDefObjectCollection%GetObjectCollectionSymbol()) .or. &
                                    ISSTREQUAL(STRTMP(3),tempUserDefObjectCollection%GetObjectCollectionCode())) then
                                    Finded = .true.
                                    !---The Assignment(=) had been overrided---
                                    newUserDefEventModel%RelativeData(1) = tempUserDefObjectCollection
                                    exit
                                end if
                            END DO

                            if(.not. Finded) then
                                write(*,*) "MCPSCUERROR: The ObjectCollection used had not been defineded: ",STRTMP(3)
                                pause
                                stop
                            end if

                        case(p_ModelType_Cross)
                            call EXTRACT_SUBSTR(STR,4,N,STRTMP) 

                            if(N .LT. 4) then
                                write(*,*) "MCPSCUERROR: Too few parameters for &MODELDEF setting, you must special a model name and unicode ,type and data(left) and dataright when cross event type are specialed "
                                write(*,*) STR
                                pause
                                stop
                            end if

                            STRTMP(3) = adjustl(trim(STRTMP(3)))
                            call UPCASE(STRTMP(3))
                            Finded = .false.
                            DO I = 1,this%TheUserDefObjectCollectionsList%GetListCount()
                                tempUserDefObjectCollection = this%TheUserDefObjectCollectionsList%GetValueByListIndex(I)

                                if(ISSTREQUAL(STRTMP(3),tempUserDefObjectCollection%GetObjectCollectionSymbol()) .or. &
                                    ISSTREQUAL(STRTMP(3),tempUserDefObjectCollection%GetObjectCollectionCode())) then
                                    Finded = .true.
                                    !---The Assignment(=) had been overrided---
                                    newUserDefEventModel%RelativeData(1) = tempUserDefObjectCollection
                                    exit
                                end if
                            END DO

                            if(.not. Finded) then
                                write(*,*) "MCPSCUERROR: The ObjectCollection used had not been defineded: ",STRTMP(3)
                                pause
                                stop
                            end if

                            STRTMP(4) = adjustl(trim(STRTMP(4)))
                            call UPCASE(STRTMP(4))
                            Finded = .false.
                            DO I = 1,this%TheUserDefObjectCollectionsList%GetListCount()
                                tempUserDefObjectCollection = this%TheUserDefObjectCollectionsList%GetValueByListIndex(I)

                                if(ISSTREQUAL(STRTMP(4),tempUserDefObjectCollection%GetObjectCollectionSymbol()) .or. &
                                    ISSTREQUAL(STRTMP(4),tempUserDefObjectCollection%GetObjectCollectionCode())) then
                                    Finded = .true.
                                    !---The Assignment(=) had been overrided---
                                    newUserDefEventModel%RelativeData(2) = tempUserDefObjectCollection
                                    exit
                                end if
                            END DO

                            if(.not. Finded) then
                                write(*,*) "MCPSCUERROR: The ObjectCollection used had not been defineded: ",STRTMP(4)
                                pause
                                stop
                            end if

                            if(ISSTREQUAL(newUserDefEventModel%RelativeData(1)%GetObjectCollectionCode(),newUserDefEventModel%RelativeData(2)%GetObjectCollectionCode())) then
                                write(*,*) "MCPSCUERROR: The two relative data for corss event is same."
                                write(*,*) STR
                                pause
                                stop
                            end if
                        case default
                            write(*,*) "MCPSCUERROR: Unknown event type: ",newUserDefEventModel%GetEventModelType()
                            pause
                            stop
                    end select

                    call this%TheUserDefEventModelsList%AppendOne(newUserDefEventModel)

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
        CLASS(ReadedCollectionEvents)::this
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
        type(UserDefEventModel)::tempUserDefEventModel
        type(UserDefEventModel)::newUserDefEventModel
        type(UserDefEventModelsList),target::HeadEventModels
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
                        call newUserDefEventModel%Clean()

                        Finded = .false.

                        ModelsName(I) = adjustl(ModelsName(I))

                        call UPCASE(ModelsName(I)(1:LENTRIM(ModelsName(I))))

                        DO J = 1,this%TheUserDefEventModelsList%GetListCount()
                            !---Use safe way to access the list---
                            tempUserDefEventModel = this%TheUserDefEventModelsList%GetValueByListIndex(J)

                            if(ISSTREQUAL(tempUserDefEventModel%GetEventModelSymbol(),ModelsName(I)(1:LENTRIM(ModelsName(I))))) then
                                !---The Assignment(=) had been overridable---
                                newUserDefEventModel = tempUserDefEventModel
                                Finded = .true.
                                exit
                            end if
                        END DO

                        if(.not. Finded) then
                            write(*,*) "MCPSCUERROR: The Model used had not been defineded: ",ModelsName(I)(1:LENTRIM(ModelsName(I)))
                            pause
                            stop
                        end if

                        if(newUserDefEventModel%GetEventModelType() .eq. p_ModelType_Cross) then
                            write(*,*) "MCPSCUERROR: The models names head cannot include cross event."
                            write(*,*) newUserDefEventModel%GetEventModelSymbol()
                            pause
                            stop
                        end if

                        !---Check dumplicate----
                        cursor=>HeadEventModels
                        DO J = 1,HeadEventModels%GetListCount()
                            if(cursor%TheValue%GetEventModelCode() .eq. newUserDefEventModel%GetEventModelCode()) then
                                write(*,*) "MCPSCUERROR: The model name is used , cannot be used again."
                                write(*,*) cursor%TheValue%GetEventModelCode()
                                write(*,*) newUserDefEventModel%GetEventModelCode()
                                pause
                                stop
                            end if
                            cursor=>cursor%Next
                        END DO

                        call HeadEventModels%AppendOne(newUserDefEventModel)
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
        CLASS(ReadedCollectionEvents)::this
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
        type(UserDefEventModel)::tempUserDefEventModel
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
                        tempUserDefEventModel = this%TheUserDefEventModelsList%GetValueByListIndex(I)

                        if(ISSTREQUAL(tempUserDefEventModel%GetEventModelSymbol(),STRTMP(1)(1:LENTRIM(STRTMP(1))))) then
                            newEventsModelsPair%SubjectEventsModelDef = tempUserDefEventModel
                            tempCode = tempUserDefEventModel%GetEventModelCode()
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

                    if(newEventsModelsPair%SubjectEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross) then
                        write(*,*) "The subject event model cannot be type of cross event."
                        write(*,*) newEventsModelsPair%SubjectEventsModelDef%GetEventModelType()
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
                            tempUserDefEventModel = this%TheUserDefEventModelsList%GetValueByListIndex(J)

                            if(tempUserDefEventModel%GetEventModelCode() .eq. CodeArray(I)) then
                                newEventsModelsPair%CrossEventsModelDef = tempUserDefEventModel
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

                        if(newEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .eq. newEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()) then
                            if(newEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .ne. newEventsModelsPair%CrossEventsModelDef%GetEventModelCode() .or. &
                               newEventsModelsPair%CrossEventsModelDef%GetEventModelType() .eq. p_ModelType_Cross) then
                                write(*,*) "MCPSCUERROR: It is impossible that the cross event is not same with subject and object events when subject and object events is same."
                                write(*,*) "Subject: ",adjustl(trim(newEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), newEventsModelsPair%SubjectEventsModelDef%GetEventModelCode()
                                write(*,*) "Object: ",adjustl(trim(newEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)), newEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()
                                write(*,*) "Cross: ",adjustl(trim(newEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)), newEventsModelsPair%CrossEventsModelDef%GetEventModelCode()
                                pause
                                stop
                            end if
                        end if
                        !---Filter some dumplicated pairs---
                        Finded = .false.
                        DO J = 1,this%TheEventsModelsPairList%GetListCount()
                            tempEventsModelsPair = this%TheEventsModelsPairList%GetValueByListIndex(J)
                            if( (newEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .and. &
                                 newEventsModelsPair%ObjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode()) .or.   &   
                                (newEventsModelsPair%ObjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .and.  &
                                 newEventsModelsPair%SubjectEventsModelDef%GetEventModelCode() .eq. tempEventsModelsPair%ObjectEventsModelDef%GetEventModelCode())) then

                                Finded = .true.

                                if(newEventsModelsPair%CrossEventsModelDef%GetEventModelCode() .ne. tempEventsModelsPair%CrossEventsModelDef%GetEventModelCode()) then
                                    write(*,*) "MCPSCUERROR: It seems that that two same event model pairs have different cross event."
                                    write(*,*) "Subject: ",adjustl(trim(newEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), &
                                               "Object: ",adjustl(trim(newEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)),   &
                                               "Cross: ",adjustl(trim(newEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol)) 
                                    write(*,*) "Subject: ",adjustl(trim(tempEventsModelsPair%SubjectEventsModelDef%GetEventModelSymbol)), &
                                               "Object: ",adjustl(trim(tempEventsModelsPair%ObjectEventsModelDef%GetEventModelSymbol)),   &
                                               "Cross: ",adjustl(trim(tempEventsModelsPair%CrossEventsModelDef%GetEventModelSymbol))
                                    pause
                                    stop
                                end if

                            end if
                        END DO

                        if(.not. Finded) then
                            call this%TheEventsModelsPairList%AppendOne(newEventsModelsPair)
                        end if
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
        if(allocated(CodeSTRArray)) deallocate(CodeSTRArray)
       
        Nullify(cursor)
        cursor=>null()

        return
        100 return 1
    end subroutine Load_OneRowEventsModelsActions

    !******************************************************************
    subroutine CopyReadedCollectionEventsFormOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedCollectionEvents),intent(out)::this
        CLASS(ReadedCollectionEvents),intent(in)::other
        !---Body---
        !---Assignment(=) had been overrided-------
        this%TheUserDefObjectCollectionsList = other%TheUserDefObjectCollectionsList
        this%TheUserDefEventModelsList = other%TheUserDefEventModelsList
        this%TheEventsModelsPairList = other%TheEventsModelsPairList
        return
    end subroutine CopyReadedCollectionEventsFormOther

    !***************************************************
    subroutine CleanReadedCollectionEvents(this)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedCollectionEvents)::this
        !---Body---
        call this%TheUserDefObjectCollectionsList%CleanList()
        call this%TheUserDefEventModelsList%CleanList()
        call this%TheEventsModelsPairList%CleanList()
        return
    end subroutine CleanReadedCollectionEvents

    !***************************************************
    subroutine Clean_ReadedCollectionEvents(this)
        implicit none
        !---Dummy Vars---
        type(ReadedCollectionEvents)::this
        !---Local Vars---
        call this%Clean()

        return
    end subroutine Clean_ReadedCollectionEvents

end module SIMULATION_TYPEDEF_READEDCOLLECTIONEVENTS