module MCLIB_UTILITIES
  USE MCLIB_CONSTANTS
  USE MCLIB_TYPEDEF_ACLUSTER
  use MODEL_TYPEDEF_ATOMSLIST
  use COMMONLIB_UTILITIES
  use MiniUtilities,only:LENTRIM,ISTR
  #ifdef MC_PROFILING
  USE MCLIB_TimeProfile
  #endif

  implicit none

  !-----------------------
  interface AllocateArray_Host
    MODULE PROCEDURE AllocateOneDimACluster_Host
    MODULE PROCEDURE AllocateTwoDimACluster_Host
  end interface AllocateArray_Host

  !------------------
  interface DeAllocateArray_Host
    MODULE PROCEDURE DeAllocateOneDimACluster_Host
    MODULE PROCEDURE DeAllocateTwoDimACluster_Host
  end interface DeAllocateArray_Host

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
                if(ISSTREQUAL(NumRangeStr(2),p_InfStr)) then
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_To = 1.D32
                    TheAtomsSetRange%m_SetsRange(ElementIndex)%m_NA_From = ISTR(NumRangeStr(1))
                else if(ISSTREQUAL(NumRangeStr(1),p_InfStr)) then
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

  !****************************************************************
  real(kind=KINDDF) function Calc_RCUT_Old(MultiBox,CUTREGIONEXTEND,BOXVOLUM,NCAct,RMAX)
    !---Purpose: To calculate the radium while searching nerighbor list
    !---Dummy Vars---
    integer,intent(in)::MultiBox
    real(kind=KINDDF),intent(in)::CUTREGIONEXTEND
    real(kind=KINDDF),intent(in)::BOXVOLUM
    integer,intent(in)::NCAct
    real(kind=KINDDF),intent(in)::RMAX
    !---Local Vars---
    real(kind=KINDDF)::SEP

    #ifdef MC_PROFILING
    call Time_Start(T_Calc_RCUT_Old_Start)
    #endif
    !---Body---
    SEP = MultiBox*BOXVOLUM/dble(NCAct)
    SEP = SEP**(0.33333333333333D0)
    SEP = SEP+2*RMAX
    Calc_RCUT_Old = CUTREGIONEXTEND*SEP

    #ifdef MC_PROFILING
    call Time_Accumulate(T_Calc_RCUT_Old_Start,T_Calc_RCUT_Old)
    #endif
    return
  end function Calc_RCUT_Old

  !*************************************************************
  subroutine AllocateOneDimACluster_Host(Array,Length,Name)
    implicit none
    !---Dummy Vars---
    type(ACluster),dimension(:),allocatable::Array
    integer,intent(in)::Length
    character(*)::Name
    !---Dummy Vars---
    integer::istat
    !---Body---
    call DeAllocateOneDimACluster_Host(Array,Name)

    if(Length .GT. 0) then
        allocate(Array(Length),STAT=istat)
        if(istat /=0) then
            write(*,*) "MCPSCUERROR: The Array :",Name,"allocate Failed !"
            pause
            stop
        end if
    end if

    return
  end subroutine AllocateOneDimACluster_Host

  !*************************************************************
  subroutine AllocateTwoDimACluster_Host(Array,LengthX,LengthY,Name)
    implicit none
    !---Dummy Vars---
    type(ACluster),dimension(:,:),allocatable::Array
    integer,intent(in)::LengthX
    integer,intent(in)::LengthY
    character(*)::Name
    !---Dummy Vars---
    integer::istat
    !---Body---
    call DeAllocateTwoDimACluster_Host(Array,Name)

    if(LengthX .GT. 0 .AND. LengthY .GT. 0) then
        allocate(Array(LengthX,LengthY),STAT=istat)
        if(istat /=0) then
            write(*,*) "MCPSCUERROR: The Array :",Name,"allocate Failed !"
            pause
            stop
        end if
    end if

    return
  end subroutine AllocateTwoDimACluster_Host

  !*************************************************************
  subroutine DeAllocateOneDimACluster_Host(Array,Name)
    implicit none
    !---Dummy Vars---
    type(ACluster),dimension(:),allocatable::Array
    character(*)::Name
    !---Local Vars---
    integer::istat
    !---Body---

    if(allocated(Array)) then
        deallocate(Array,STAT=istat)
        if(istat /=0) then
            write(*,*) "MCPSCUERROR: The Array :",Name,"Deallocate Failed !"
            pause
            stop
        end if
    end if

    return
  end subroutine DeAllocateOneDimACluster_Host

  !*************************************************************
  subroutine DeAllocateTwoDimACluster_Host(Array,Name)
    implicit none
    !---Dummy Vars---
    type(ACluster),dimension(:,:),allocatable::Array
    character(*)::Name
    !---Local Vars---
    integer::istat
    !---Body---

    if(allocated(Array)) then
        deallocate(Array,STAT=istat)
        if(istat /=0) then
            write(*,*) "MCPSCUERROR: The Array :",Name,"Deallocate Failed !"
            pause
            stop
        end if
    end if

    return
  end subroutine DeAllocateTwoDimACluster_Host

  !*******************************************************
  subroutine ResizeClustersArray_OneDim(TheArray,NewSize)
        implicit none
        !---Dummy Vars---
        type(ACluster), dimension(:), allocatable,intent(inout)::TheArray
        integer,intent(in)::NewSize
        !---Local Vars---
        integer::OldSize(1)
        type(ACluster), dimension(:), allocatable::tempArray
        type(ACluster)::zero_Cluster
        integer::istat
        !---Body----

        OldSize = shape(TheArray)

        if(OldSize(1) .ne. NewSize) then
            zero_Cluster%m_POS = 0
            zero_Cluster%m_Statu = p_Empty
            zero_Cluster%m_Layer = 1
            zero_Cluster%m_RAD = 0
            zero_Cluster%m_DiffCoeff = 0.D0
            zero_Cluster%m_DiffuseDirection = 0.D0
            zero_Cluster%m_DiffuseRotateCoeff = 0.D0
            ! note: in fortran2003, while the compiler option -Mallocatable=03 is used
            !       the tempArray need not need to be allocated, the compiler would
            !       allocate the array automatic that is marked as "allocatable" based on the
            !       assigned array's size
            call AllocateArray_Host(tempArray,OldSize(1),"tempArray")

            tempArray = reshape(SOURCE=[TheArray],SHAPE=[OldSize(1)])

            call AllocateArray_Host(TheArray,NewSize,"TheArray")

            TheArray = reshape(SOURCE=[tempArray],SHAPE=[NewSize],PAD=[zero_Cluster])

            call DeAllocateArray_Host(tempArray,"tempArray")

        end if

        return
    end subroutine ResizeClustersArray_OneDim

    !*******************************************
    subroutine DumplicateClustersArray_OneDim(TheArray,DumplicateNum)
        implicit none
        !---Dummy Vars---
        type(ACluster), dimension(:), allocatable,intent(inout)::TheArray
        integer,intent(in)::DumplicateNum
        !---Local Vars---
        integer::OldSize(1)
        type(ACluster), dimension(:), allocatable::tempArray
        integer::NewSize
        integer::istat
        !---Body----

        OldSize = shape(TheArray)

        NewSize = DumplicateNum*OldSize(1)

        if(DumplicateNum .GT. 1) then
            ! note: in fortran2003, while the compiler option -Mallocatable=03 is used
            !       the tempArray need not need to be allocated, the compiler would
            !       allocate the array automatic that is marked as "allocatable" based on the
            !       assigned array's size
            call AllocateArray_Host(tempArray,OldSize(1),"tempArray")

            tempArray = reshape(SOURCE=[TheArray],SHAPE=[OldSize(1)])

            call AllocateArray_Host(TheArray,NewSize,"TheArray")

            TheArray = reshape(SOURCE=[tempArray],SHAPE=[NewSize],PAD=[tempArray])

            call DeAllocateArray_Host(tempArray,"tempArray")

        end if

        return

    end subroutine DumplicateClustersArray_OneDim


end module MCLIB_UTILITIES
