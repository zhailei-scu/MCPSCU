#include "../../../Macro"

module MCLIB_TYPEDEF_DiffusorPropList
    use MCLIB_CONSTANTS
    use MCLIB_TYPEDEF_ATOMSLIST
    use MCLIB_TYPEDEF_DiffusorsValue
    use MCLIB_UTILITIES
    use MiniUtilities, only:ISTR
    implicit none

    character(len=1),parameter,private::p_ElementsTypeSpe = "@"
    character(len=1),parameter,private::p_ElementsNumSpe = "#"
    character(len=1),parameter,private::p_NumRangeSpe = "-"
    character(len=3),parameter,private::p_InfStr = "INF"

    type,public::ReadDiffusorPropList

        type(ReadedDiffusorValue)::Diffusor

        type(ReadDiffusorPropList),pointer::next=>null()

        integer,private::ListCount = 0

        contains
        procedure,non_overridable,pass,public::AppendOne_ReadDiffusorPropList
        procedure,non_overridable,pass,public::AppendArray_ReadDiffusorPropList
        procedure,non_overridable,pass,public::GetReadDiffusorByListIndex
        procedure,non_overridable,pass,public::ConvertToDiffusorsTypesMap
        procedure,non_overridable,pass,public::GetList_Count=>GetReadDiffusorPropList_Count
        procedure,non_overridable,pass,private::CopyReadDiffusorPropListFromOther
        procedure,non_overridable,pass,public::Clean_ReadDiffusorPropList
        procedure,non_overridable,pass,public::PrintOutCheckingResult
        Generic::Assignment(=)=>CopyReadDiffusorPropListFromOther
        Final::CleanReadDiffusorPropList

    end type ReadDiffusorPropList

    interface InterpCScript

      function InterpCScript(scriptStr) result(ArraySize) bind(c,name="InterpCScript")
        use iso_c_binding
        implicit none
        character(kind=c_char,len=10000)::scriptStr
        integer(kind=c_int)::ArraySize
      end function InterpCScript

    end interface

    interface GetInterpedArray
      subroutine GetInterpedArray(FDiffusorDefArray) bind(c,name="GetInterpedArray")
        use iso_c_binding
        use MCLIB_TYPEDEF_DiffusorsValue
        implicit none
        type(ReadedDiffusorValue)::FDiffusorDefArray(*)
      end subroutine GetInterpedArray

    end interface GetInterpedArray

    private::AppendOne_ReadDiffusorPropList
    private::AppendArray_ReadDiffusorPropList
    private::GetReadDiffusorByListIndex
    private::ConvertToDiffusorsTypesMap
    private::GetReadDiffusorPropList_Count
    private::CopyReadDiffusorPropListFromOther
    private::Clean_ReadDiffusorPropList
    private::CleanReadDiffusorPropList

    contains

    !*************************************************
    function ResolveSymbol2AtomsSetRange(symbol,BasicAtomsList) result(TheAtomsSetRange)
        implicit none
        !---Dummy Vars---
        character(*)::symbol
        type(AtomsList),intent(in)::BasicAtomsList
        type(AtomsSetRange)::TheAtomsSetRange
        !---Local Vars---
        character(len=20)::ElementsStrs(10)
        character(len=20)::symbolANDNumRangeStr(2)
        character(len=10)::NumRangeStr(2)
        integer::ElemtsGroup
        integer::SepNum
        integer::ElementIndex
        integer::I
        !---Body---

        ElementsStrs = ''

        call TheAtomsSetRange%ReleaseSetsRange()

        call separateStrByString(symbol,p_ElementsTypeSpe,ElementsStrs,ElemtsGroup)

        if(ElemtsGroup .GT. p_ATOMS_GROUPS_NUMBER) then
            write(*,*) "MCPSCUERROR: The kinds of atoms in cluster "//adjustl(trim(symbol))//" is ",ElemtsGroup
            write(*,*) "MCPSCUERROR: Which has been greater than defined max atoms kinds: ",p_ATOMS_GROUPS_NUMBER
            pause
            stop
        end if

        DO I = 1,p_ATOMS_GROUPS_NUMBER
            TheAtomsSetRange%m_SetsRange(I)%m_ID = I
        END DO

        DO I = 1,ElemtsGroup

            symbolANDNumRangeStr = ""

            NumRangeStr = ""

            call separateStrByString(ElementsStrs(I),p_ElementsNumSpe,symbolANDNumRangeStr,SepNum)
            if(SepNum .NE. 2) then
                write(*,*) "MCPSCUERROR: The Element "//ElementsStrs(I)//" define is not correct"
                write(*,*) "In cluster :",symbol
                write(*,*) symbolANDNumRangeStr
                pause
                stop
            end if

            ElementIndex = BasicAtomsList%FindIndexBySymbol(symbolANDNumRangeStr(1))

            if(ElementIndex .LE. 0) then
                write(*,*) "MCPSCUERROR: The element symbol is not defineded: ",symbolANDNumRangeStr(1)
                write(*,*) "In cluster: ",symbol
                pause
                stop
            end if

            if(TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From .GT. 0) then
                write(*,*) "MCPSCUERROR: Cannot define two ranges for one same element in one cluster symbol: ",symbol
                pause
                stop
            end if

            if(ElementIndex .ne. TheAtomsSetRange%m_SetsRange(ElementIndex)%m_ID) then
                write(*,*) "The pre-putted element index is not true"
                write(*,*) "For cluster: ",symbol
                write(*,*) "In position: ",ElementIndex
                write(*,*) "The pre-putted element index is: ",TheAtomsSetRange%m_SetsRange(ElementIndex)%m_ID
                pause
                stop
            end if

            call separateStrByString(symbolANDNumRangeStr(2),p_NumRangeSpe,NumRangeStr,SepNum)

            if(SepNum .eq. 1) then
                TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From = ISTR(NumRangeStr(1))
                TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_To = ISTR(NumRangeStr(1))
            else if(SepNum .LE. 0) then
                write(*,*) "MCPSCUERROR: you must special the atoms compents number for cluster: ",symbol
                pause
                stop
            else if(SepNum .GE. 3) then
                write(*,*) "MCPSCUERROR: only up and down limits can be accepted, you have specialied too much limit for cluster :",symbol
                pause
                stop
            else if(SepNum .eq. 2) then
                if(IsStrEqual(NumRangeStr(2),p_InfStr)) then
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_To = 1.D32
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From = ISTR(NumRangeStr(1))
                else if(IsStrEqual(NumRangeStr(1),p_InfStr)) then
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_To = 1.D32
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From = ISTR(NumRangeStr(2))
                else
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From = min(ISTR(NumRangeStr(1)),ISTR(NumRangeStr(2)))
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_To = max(ISTR(NumRangeStr(1)),ISTR(NumRangeStr(2)))
                end if
            end if

            if(TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From .LE. 0 .AND. TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_To .GT. 0) then
                write(*,*) "MCPSCUERROR: The atoms number down limit cannot less than 1 in cluster: ",symbol
                write(*,*) NumRangeStr
                pause
                stop
            end if

        END DO

        return
    end function ResolveSymbol2AtomsSetRange

    !*******************************************************
    subroutine ConvertToDiffusorsTypesMap(this,BasicAtomsList,TheDiffusorTypesMap)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),intent(in),target::this
        type(AtomsList),intent(in)::BasicAtomsList
        type(DiffusorTypesMap)::TheDiffusorTypesMap
        !---Local Vars---
        type(AtomsSetRange),dimension(:),allocatable::TheAtomsSetsRangesArray
        type(ReadDiffusorPropList),pointer::cursor=>null()
        integer,dimension(:,:),allocatable::RangesArray
        integer,dimension(:,:),allocatable::tempSingleAtomsDivideArrays
        integer,dimension(:,:),allocatable::SingleAtomsDivideArrays
        integer,dimension(:,:),allocatable::AtomsSetsRangesMarkArray
        type(AClusterList),dimension(:),pointer::ConstructClusterListsArray=>null()
        type(AClusterList),pointer::ClusterListCursor=>null()
        integer::UsedAtomsType(p_ATOMS_GROUPS_NUMBER)
        type(ReadedDiffusorValue)::Diffusor
        integer::temp_MaxDivideGroups
        integer::tempMax
        integer::tempMin
        integer::tempMinLoc
        integer::DivideGroups_SingleElement(p_ATOMS_GROUPS_NUMBER)
        integer::tempDivideGroups_SingleElement(p_ATOMS_GROUPS_NUMBER)
        integer::tempIndex
        integer::ListCount
        integer::IElement
        integer::I
        integer::J
        integer::TheID
        integer::coverageCount
        integer::Maplength
        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: You should allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if

        ListCount = this%GetList_Count()

        if(ListCount .LE. 0) then
            return
        else
            allocate(TheAtomsSetsRangesArray(ListCount))
        end if

        tempIndex = 0

        DO While(associated(cursor))

            tempIndex = tempIndex + 1

            call TheAtomsSetsRangesArray(tempIndex)%ReleaseSetsRange()

            TheAtomsSetsRangesArray(tempIndex) = ResolveSymbol2AtomsSetRange(cursor%Diffusor%symbol,BasicAtomsList)

            cursor=>cursor%next

        END DO

        if(ListCount .ne. tempIndex) then
            write(*,*) "MCPSCUERROR: The diffusor define List count is error."
            write(*,*) "The record list count is ",ListCount
            write(*,*) "In fact, the actual list count is: ",tempIndex
            pause
            stop
        end if

        call AllocateArray_Host(RangesArray,p_ATOMS_GROUPS_NUMBER,2*ListCount,"RangesArray")
        RangesArray = 0

        call AllocateArray_Host(tempSingleAtomsDivideArrays,p_ATOMS_GROUPS_NUMBER,2*ListCount,"tempSingleAtomsDivideArrays")
        tempSingleAtomsDivideArrays = 0

        UsedAtomsType = 0

        DO I = 1,ListCount
            DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                TheID = TheAtomsSetsRangesArray(I)%m_SetsRange(IElement)%m_ID
                if(TheID .GT. 0) then
                    RangesArray(TheID,(I -1)*2 + 1) = TheAtomsSetsRangesArray(I)%m_SetsRange(TheID)%m_NA_From
                    RangesArray(TheID,(I -1)*2 + 2) = TheAtomsSetsRangesArray(I)%m_SetsRange(TheID)%m_NA_To
                    UsedAtomsType(TheID) = 1
                end if
            END DO
        END DO

        !---Check the coverage---
        DO I = 1,ListCount
            DO J = I+1,ListCount
                coverageCount = 0
                DO IElement = 1,p_ATOMS_GROUPS_NUMBER

                    if(UsedAtomsType(IElement) .GT. 0) then

                        if(IsRangeCoverage(RangesArray(IElement,(I -1)*2 + 1),RangesArray(IElement,(I -1)*2 + 2), &
                                           RangesArray(IElement,(J -1)*2 + 1),RangesArray(IElement,(J -1)*2 + 2))) then
                            coverageCount = coverageCount + 1
                        end if
                    else
                        coverageCount = coverageCount + 1
                    end if

                END DO

                if(coverageCount .GE. p_ATOMS_GROUPS_NUMBER) then
                    Diffusor = this%GetReadDiffusorByListIndex(I)

                    write(*,*) "MCPSCUERROR: The diffusor define is overlapping between diffusor ",Diffusor%symbol
                    Diffusor = this%GetReadDiffusorByListIndex(J)
                    write(*,*) "and diffusor: ",Diffusor%symbol
                    pause
                    stop
                end if
            END DO
        END DO

        !---Construct the range array---
        DO IElement = 1,p_ATOMS_GROUPS_NUMBER

            temp_MaxDivideGroups = 0

            tempMax = maxval(RangesArray(IElement,:))

            if(tempMax .GT. 0) then

                DO I = 1,2*ListCount

                    tempMin = minval(RangesArray(IElement,:),MASK=(RangesArray(IElement,:) .GT. 0))

                    tempMinLoc = minloc(RangesArray(IElement,:),DIM=1,mask=(RangesArray(IElement,:) .GT. 0))

                    RangesArray(IElement,tempMinLoc) = tempMax

                    if(temp_MaxDivideGroups .eq. 0) then
                            temp_MaxDivideGroups = temp_MaxDivideGroups + 1
                            tempSingleAtomsDivideArrays(IElement,temp_MaxDivideGroups) = tempMin
                    else if(tempMin .GT. tempSingleAtomsDivideArrays(IElement,temp_MaxDivideGroups)) then
                            temp_MaxDivideGroups = temp_MaxDivideGroups + 1
                            tempSingleAtomsDivideArrays(IElement,temp_MaxDivideGroups) = tempMin
                    end if
                END DO

            end if

        END DO

        DivideGroups_SingleElement = 0

        DO IElement = 1,p_ATOMS_GROUPS_NUMBER
            DivideGroups_SingleElement(IElement) = count(tempSingleAtomsDivideArrays(IElement,:) .GT. 0)
        END DO

        !----Start to set barrier for the void range ---------------------
        if(maxval(DivideGroups_SingleElement) .GT. 1) then
            call AllocateArray_Host(AtomsSetsRangesMarkArray,p_ATOMS_GROUPS_NUMBER,maxval(DivideGroups_SingleElement)-1,"AtomsSetsRangesMarkArray")

            AtomsSetsRangesMarkArray = 0

            DO tempIndex = 1,ListCount

                DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                    DO J = 1,DivideGroups_SingleElement(IElement)-1
                        if((tempSingleAtomsDivideArrays(IElement,J) + 1) .LT. tempSingleAtomsDivideArrays(IElement,J+1)) then
                            if(TheAtomsSetsRangesArray(tempIndex)%m_SetsRange(IElement)%m_NA_From .LT. (tempSingleAtomsDivideArrays(IElement,J) + 1) .AND. &
                                TheAtomsSetsRangesArray(tempIndex)%m_SetsRange(IElement)%m_NA_To .GT. (tempSingleAtomsDivideArrays(IElement,J) + 1)) then
                                AtomsSetsRangesMarkArray(IElement,J) = 1
                            end if
                        else
                            AtomsSetsRangesMarkArray(IElement,J) = 1
                        end if
                    END DO

                END DO

            END DO

            tempDivideGroups_SingleElement = DivideGroups_SingleElement

            DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                DO J = 1,DivideGroups_SingleElement(IElement)-1
                    if(AtomsSetsRangesMarkArray(IElement,J) .LE. 0) then
                        if((tempSingleAtomsDivideArrays(IElement,J) + 2) .LT. tempSingleAtomsDivideArrays(IElement,J+1)) then
                            tempDivideGroups_SingleElement(IElement) = tempDivideGroups_SingleElement(IElement) + 2
                        else
                            tempDivideGroups_SingleElement(IElement) = tempDivideGroups_SingleElement(IElement) + 1
                        end if
                    end if
                END DO

            END DO

            !---The left start of the range array should be considered----
            tempDivideGroups_SingleElement = tempDivideGroups_SingleElement + 1

            if(maxval(DivideGroups_SingleElement) .GT. 0) then

                call AllocateArray_Host(SingleAtomsDivideArrays,p_ATOMS_GROUPS_NUMBER,maxval(tempDivideGroups_SingleElement) ,"SingleAtomsDivideArrays")

                DO IElement = 1,p_ATOMS_GROUPS_NUMBER

                    tempIndex = 1
                    !---The left start of the range array should be considered----
                    SingleAtomsDivideArrays(IElement,tempIndex) = max(tempSingleAtomsDivideArrays(IElement,1)-1,0)

                    DO I = 1,DivideGroups_SingleElement(IElement)-1

                        if(tempSingleAtomsDivideArrays(IElement,I) .LE. 0) then
                            exit
                        end if
                        tempIndex = tempIndex + 1
                        SingleAtomsDivideArrays(IElement,tempIndex) = tempSingleAtomsDivideArrays(IElement,I)

                        if(AtomsSetsRangesMarkArray(IElement,I) .LE. 0) then
                            if((tempSingleAtomsDivideArrays(IElement,I) + 2) .LT. tempSingleAtomsDivideArrays(IElement,I+1)) then
                                SingleAtomsDivideArrays(IElement,tempIndex+1) = tempSingleAtomsDivideArrays(IElement,I) + 1
                                SingleAtomsDivideArrays(IElement,tempIndex+2) = tempSingleAtomsDivideArrays(IElement,I+1) - 1
                                tempIndex = tempIndex + 2
                            else
                                SingleAtomsDivideArrays(IElement,tempIndex+1) = tempSingleAtomsDivideArrays(IElement,I) + 1
                                tempIndex = tempIndex + 1
                            end if
                        end if
                    END DO

                    SingleAtomsDivideArrays(IElement,tempIndex+1:maxval(tempDivideGroups_SingleElement)) = maxval(tempSingleAtomsDivideArrays(IElement,:))
                END DO
            end if

        else if(maxval(DivideGroups_SingleElement) .eq. 1) then
            call AllocateArray_Host(SingleAtomsDivideArrays,p_ATOMS_GROUPS_NUMBER,2,"SingleAtomsDivideArrays")

            DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                SingleAtomsDivideArrays(IElement,1) = max(tempSingleAtomsDivideArrays(IElement,1)-1,0)
                SingleAtomsDivideArrays(IElement,2) = tempSingleAtomsDivideArrays(IElement,1)
            END DO
        else
            call AllocateArray_Host(SingleAtomsDivideArrays,p_ATOMS_GROUPS_NUMBER,1,"SingleAtomsDivideArrays")
            SingleAtomsDivideArrays = 0
        end if

        !---Start to construct the diffusors map---
        allocate(ConstructClusterListsArray(ListCount))

        Maplength = 0

        DO tempIndex = 1,ListCount

            call ConstructClusterListsArray(tempIndex)%Clean_ClusterList()

            ConstructClusterListsArray(tempIndex) = TheAtomsSetsRangesArray(tempIndex)%AtomsSetRange2ClusterList(SingleAtomsDivideArrays)

            Maplength = Maplength + ConstructClusterListsArray(tempIndex)%GetList_Count()
        END DO

        call TheDiffusorTypesMap%constructor(SingleAtomsDivideArrays,Maplength)

        !*********Note: The array TheAtomsSetsRangesArray need to response to this(ReadDiffusorList) one by one, so, please ensure*********
        !*********The ConstructClusterListsArray is not modified after resolved from ReadDiffusorList***********
        cursor=>this
        tempIndex = 0
        DO While(associated(cursor))

            tempIndex = tempIndex + 1

            if(ConstructClusterListsArray(tempIndex)%GetList_Count() .GT. 0) then
                ClusterListCursor=>ConstructClusterListsArray(tempIndex)
                DO While(associated(ClusterListCursor))
                    call TheDiffusorTypesMap%put(ClusterListCursor%TheCluster,cursor%Diffusor%Convert2DiffusorValue())
                    ClusterListCursor=>ClusterListCursor%next
                END DO
            end if

            cursor=>cursor%next
        END DO

        call DeAllocateArray_Host(RangesArray,"RangesArray")

        call DeAllocateArray_Host(tempSingleAtomsDivideArrays,"tempSingleAtomsDivideArrays")

        call DeAllocateArray_Host(SingleAtomsDivideArrays,"SingleAtomsDivideArrays")

        call DeAllocateArray_Host(AtomsSetsRangesMarkArray,"AtomsSetsRangesMarkArray")

        if(allocated(TheAtomsSetsRangesArray)) then
            deallocate(TheAtomsSetsRangesArray)
        end if

        if(allocated(ConstructClusterListsArray)) then
            deallocate(ConstructClusterListsArray)
        end if
        Nullify(ConstructClusterListsArray)
        ConstructClusterListsArray=>null()

        Nullify(cursor)
        cursor=>null()
        Nullify(ClusterListCursor)
        ClusterListCursor=>null()

        return
    end subroutine

    !***************************************
    subroutine CopyReadDiffusorPropListFromOther(this,otherOne)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),intent(out),target::this
        type(ReadDiffusorPropList),target,intent(in)::otherOne
        !---Local Vars---
        type(ReadDiffusorPropList),pointer::cursorOfOthers=>null()
        type(ReadDiffusorPropList),pointer::cursorOfSelf=>null()
        type(ReadDiffusorPropList),pointer::cursorOfSelfP=>null()
        !---Body---
        cursorOfSelf=>this
        if(.not. associated(cursorOfSelf)) then
            write(*,*) "MCPSCUERROR: You need to allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if

        cursorOfOthers=>otherOne
        if(.not. associated(cursorOfOthers)) then
            Nullify(cursorOfSelf)
            return
        end if

        call this%Clean_ReadDiffusorPropList()

        this%Diffusor = otherOne%Diffusor

        cursorOfOthers=>otherOne%next
        cursorOfSelfP=>this
        cursorOfSelf=>this%next
        DO While(associated(cursorOfOthers))
            allocate(cursorOfSelf)
            cursorOfSelf%Diffusor = cursorOfOthers%Diffusor
            cursorOfSelfP%next=>cursorOfSelf

            cursorOfOthers=>cursorOfOthers%next
            cursorOfSelfP=>cursorOfSelfP%next
            cursorOfSelf=>cursorOfSelf%next
        END DO
        this%ListCount = otherOne%GetList_Count()

        Nullify(cursorOfSelfP)
        Nullify(cursorOfSelf)
        Nullify(cursorOfOthers)
        return
    end subroutine CopyReadDiffusorPropListFromOther

    !***************************************
    subroutine AppendOne_ReadDiffusorPropList(this,newOne)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),target::this
        type(ReadedDiffusorValue)::newOne
        !---Local Vars---
        type(ReadDiffusorPropList),pointer::cursor=>null(),cursorP=>null()
        !---Body---
        cursorP=>this
        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: You need to allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if

        if(this%GetList_Count() .LE. 0) then
            this%ListCount = 1
            this%Diffusor = newOne
        else
            cursor=>this%next
            cursorP=>this
            if(IsStrEqual(cursorP%Diffusor%symbol,newOne%symbol)) then
                write(*,*) "MCPSCUERROR: The Diffusor is dumplicated:",newOne%symbol
                pause
                stop
            end if

            DO while(associated(cursor))
                cursor=>cursor%next
                cursorP=>cursorP%next

                if(IsStrEqual(cursorP%Diffusor%symbol,newOne%symbol)) then
                    write(*,*) "MCPSCUERROR: The Diffusor is dumplicated:",newOne%symbol
                    pause
                    stop
                end if
            END DO

            this%ListCount = this%ListCount + 1

            allocate(cursor)
            NUllify(cursor%next)
            ! The assignment(=) had been overrided
            cursor%Diffusor = newOne
            cursorP%next=>cursor
        end if

        Nullify(cursorP)
        cursorP=>null()
        Nullify(cursor)
        cursor=>null()
        return
    end subroutine AppendOne_ReadDiffusorPropList


    !***************************************
    subroutine AppendArray_ReadDiffusorPropList(this,DiffusorsArray,ArraySize)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),target::this
        type(ReadedDiffusorValue),allocatable::DiffusorsArray(:)
        integer,intent(in)::ArraySize
        !---Local Vars---
        type(ReadDiffusorPropList),pointer::cursor=>null()
        type(ReadDiffusorPropList),pointer::cursorP=>null()
        integer::I
        !---Body---
        cursorP=>this
        if(.not. associated(cursorP)) then
            write(*,*) "MCPSCUERROR: You need to allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if

        if(ArraySize  .LE. 0 .or. size(DiffusorsArray) .LE. 0) then
            write(*,*) "MCPSCUWARNING: No elements would be appended to diffusorList"
            return
        end if

        if(ArraySize .GT. size(DiffusorsArray)) then
            write(*,*) "MCPSCUERROR: The aimmed size to appended to the diffusorList is greater than the Array size",ArraySize,size(DiffusorsArray)
            pause
            stop
        end if


        DO I=1,ArraySize
            call this%AppendOne_ReadDiffusorPropList(DiffusorsArray(I))
        END DO

        return
    end subroutine AppendArray_ReadDiffusorPropList

    !**************************************
    function GetReadDiffusorByListIndex(this,ListIndex) result(Diffusor)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),target::this
        integer,intent(in)::ListIndex
        type(ReadedDiffusorValue),intent(out)::Diffusor
        !---Local Vars---
        integer::tempIndex
        type(ReadDiffusorPropList),pointer::cursor=>null()
        !---Body---
        cursor=>this
        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: You need to allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if

        tempIndex = 0

        DO while(associated(cursor))

            tempIndex = tempIndex + 1

            if(tempIndex .eq. ListIndex) then
                Diffusor = cursor%Diffusor
                exit
            end if

            cursor=>cursor%next

        END DO

        if(ListIndex .ne. tempIndex) then
            write(*,*) "MCPSCUERROR: Cannot get the diffusor form diffusor list by index: ",ListIndex
            pause
            stop
        end if

        Nullify(cursor)
        cursor=>null()
        return
    end function GetReadDiffusorByListIndex


    !**************************************
    integer function GetReadDiffusorPropList_Count(this)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),target::this
        !---Local Vars---
        type(ReadDiffusorPropList),pointer::cursor=>null()
        !---Body---
        cursor=>this
        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: You need to allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if


        GetReadDiffusorPropList_Count = this%ListCount

        return
    end function

    !**************************************
    subroutine PrintOutCheckingResult(this,hFile,BasicAtomsList,TheDiffusorTypesMap)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),intent(in),target::this
        integer,intent(in)::hFile
        type(AtomsList),intent(in)::BasicAtomsList
        type(DiffusorTypesMap),intent(in)::TheDiffusorTypesMap
        !---Local Vars---
        type(AtomsSetRange),dimension(:),allocatable::TheAtomsSetsRangesArray
        type(ReadDiffusorPropList),pointer::cursor=>null()
        type(AClusterList),target::ConstructClusterList
        type(AClusterList),pointer::ClusterListCursor=>null()
        type(DiffusorValue)::TheValue
        integer::ListCount
        integer::tempIndex
        character*32::symbol
        character*32::CNum
        character*32::CElements
        integer::IAtomsGroup
        integer::I
        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            write(*,*) "MCPSCUERROR: You should allocate the ReadDiffusorPropList first!"
            pause
            stop
        end if

        ListCount = this%GetList_Count()

        if(ListCount .LE. 0) then
            return
        else
            allocate(TheAtomsSetsRangesArray(ListCount))
        end if

        tempIndex = 0

        DO While(associated(cursor))

            tempIndex = tempIndex + 1

            call TheAtomsSetsRangesArray(tempIndex)%ReleaseSetsRange()

            TheAtomsSetsRangesArray(tempIndex) = ResolveSymbol2AtomsSetRange(cursor%Diffusor%symbol,BasicAtomsList)

            cursor=>cursor%next
        END DO

        cursor=>this
        tempIndex = 0
        DO While(associated(cursor))

            tempIndex = tempIndex + 1

            call ConstructClusterList%Clean_ClusterList()

            !---Construct the diffusor to determine the maps
            ConstructClusterList = TheAtomsSetsRangesArray(tempIndex)%AtomsSetRange2ClusterList(TheDiffusorTypesMap%SingleAtomsDivideArrays)

            write(*,*) "######################################################################"

            write(hFile,fmt="('!','The diffusor symbol =',A20,2x,  &
                              '!','CoefficentsGenerate way =',I1,2x, &
                              '!','DiffusionCiefficents value =',1PE10.4,2x, &
                              '!','PreFactor = ',1PE10.4,2x, &
                              '!','ActEnergy = ',1PE10.4,2x, &
                              '!','ECR Generate way =',I1,2x, &
                              '!','ECR Value =',1PE10.4)")                     cursor%Diffusor%symbol, &
                                                                               cursor%Diffusor%DiffusorValueType, &
                                                                               cursor%Diffusor%DiffuseCoefficient_Value,  &
                                                                               cursor%Diffusor%PreFactor, &
                                                                               cursor%Diffusor%ActEnergy, &
                                                                               cursor%Diffusor%ECRValueType, &
                                                                               cursor%Diffusor%ECR

            if(ConstructClusterList%GetList_Count() .GT. 0) then
                ClusterListCursor=>ConstructClusterList
                DO While(associated(ClusterListCursor))
                    TheValue  = TheDiffusorTypesMap%get(ClusterListCursor%TheCluster)

                    symbol = ""
                    DO IAtomsGroup = 1,p_ATOMS_GROUPS_NUMBER
                        CNum = ""
                        call BasicAtomsList%GetSymbolByIndex(IAtomsGroup,CElements)
                        CElements = adjustl(CElements)
                        write(CNum,*) ClusterListCursor%TheCluster%m_Atoms(IAtomsGroup)%m_NA
                        CNum = adjustl(CNum)
                        symbol = symbol(1:LENTRIM(symbol))//CElements(1:LENTRIM(CElements))//"#"//CNum(1:LENTRIM(CNum))

                        if(IAtomsGroup .LT. p_ATOMS_GROUPS_NUMBER) then
                            symbol = symbol(1:LENTRIM(symbol))//"@"
                        end if
                    END DO

                    symbol = adjustl(symbol)

                    write(hFile,fmt="('! |--','The diffusor symbol =',A20,2x,  &
                                      '!','CoefficentsGenerate way =',I1,2x, &
                                      '!','DiffusionCiefficents value =',1PE10.4,2x, &
                                      '!','PreFactor = ',1PE10.4,2x, &
                                      '!','ActEnergy = ',1PE10.4,2x, &
                                      '!','ECR Generate way =',I1,2x, &
                                      '!','ECR Value =',1PE10.4)")             symbol, &
                                                                               TheValue%DiffusorValueType, &
                                                                               TheValue%DiffuseCoefficient_Value,  &
                                                                               TheValue%PreFactor,          &
                                                                               TheValue%ActEnergy,          &
                                                                               TheValue%ECRValueType, &
                                                                               TheValue%ECR

                    ClusterListCursor=>ClusterListCursor%next
                END DO
            end if

            cursor=>cursor%next
        END DO

        if(allocated(TheAtomsSetsRangesArray)) then
            deallocate(TheAtomsSetsRangesArray)
        end if

        call ConstructClusterList%Clean_ClusterList()

        Nullify(cursor)
        Nullify(ClusterListCursor)
        return
    end subroutine PrintOutCheckingResult

    !**************************************
    subroutine Clean_ReadDiffusorPropList(this)
        implicit none
        !---Dummy Vars---
        CLASS(ReadDiffusorPropList),target::this
        !---Local Vars---
        type(ReadDiffusorPropList),pointer::cursor=>null()
        type(ReadDiffusorPropList),pointer::next=>null()
        !---Body---
        cursor=>this

        if(.not. associated(cursor)) then
            return
        end if

        if(cursor%GetList_Count() .LE. 0) then
            return
        end if

        cursor=>this%next

        call CleanReadedDiffusorValue(this%Diffusor)

        DO While(associated(cursor))
            next=>cursor%next
            call CleanReadedDiffusorValue(cursor%Diffusor)
            deallocate(cursor)
            Nullify(cursor)
            cursor=>next
        END DO

        call CleanReadedDiffusorValue(this%Diffusor)

        this%next=>null()

        this%ListCount = 0

        Nullify(cursor)
        Nullify(next)
        cursor=>null()
        next=>null()

        return
    end subroutine Clean_ReadDiffusorPropList

    !************************************
    subroutine CleanReadDiffusorPropList(this)
        implicit none
        !---Dummy Vars---
        type(ReadDiffusorPropList)::this
        !---Body---

        call this%Clean_ReadDiffusorPropList()

        return
    end subroutine CleanReadDiffusorPropList

    !**************************************************
    subroutine ResloveDiffusorsValueFromCScript(scriptStr,DiffusorProp_List)
        implicit none
        !---Dummy Vars---
        character(kind=c_char,len=10000)::scriptStr
        type(ReadDiffusorPropList)::DiffusorProp_List
        !---Local Vars---
        type(ReadedDiffusorValue),allocatable::FDiffusorDefArray(:)
        integer(kind=c_int)::ArraySize
        !---Body---

        ArraySize = InterpCScript(scriptStr)

        if(allocated(FDiffusorDefArray)) then
            deallocate(FDiffusorDefArray)
        end if
        allocate(FDiffusorDefArray(ArraySize))

        call GetInterpedArray(FDiffusorDefArray)


        call AppendArray_ReadDiffusorPropList(DiffusorProp_List,FDiffusorDefArray,ArraySize)


        return
    end subroutine ResloveDiffusorsValueFromCScript


end module MCLIB_TYPEDEF_DiffusorPropList
