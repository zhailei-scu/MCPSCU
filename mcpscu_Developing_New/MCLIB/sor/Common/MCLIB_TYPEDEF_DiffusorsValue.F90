#include "../../../Macro"

module MCLIB_TYPEDEF_DiffusorsValue
    USE MCLIB_CONSTANTS
    USE MCLIB_TYPEDEF_ACLUSTER
    use MCLIB_UTILITIES
    USE iso_c_binding
    implicit none



    TYPE,PUBLIC::ReadedDiffusorValue

        character(kind=c_char,len=20)::symbol = ""

        integer(c_int)::DiffusorValueType = p_DiffuseCoefficient_ByValue

        ! If the DiffuseCoefficient type is by value,use this
        real(c_double)::DiffuseCoefficient_Value = 0.D0

        ! If the DiffuseCoefficient type is by Arrhenius or BCluster(bigger cluster),use this
        real(c_double)::PreFactor = 0.D0
        real(c_double)::ActEnergy = 0.D0

        integer(c_int)::ECRValueType = p_ECR_ByValue

        real(c_double)::ECR = 0.D0

        contains
        procedure,private,non_overridable,pass::CopyReadedDiffusorValueFromOther
        procedure,public,non_overridable,pass::Convert2DiffusorValue
        GENERIC::Assignment(=)=>CopyReadedDiffusorValueFromOther
        Final::CleanReadedDiffusorValue
    END TYPE ReadedDiffusorValue

    !**********Based by our test, if we want to combine C, the same data structure need to be defined in fortran and C
    !**********More important, we can not use the inherit in fotran types define, or it would meeting mismatch while running the code
    !**********So we must define the type ReadedDiffusorValue in fortran to match C type to reslove the diffusor from C.
    !**********Howeve, the member "symbol" is not required after the diffusors is used for calculation, so we re-define type DiffusorValue
    !**********Which contains all same members except the "symbol".
    TYPE,PUBLIC::DiffusorValue

        integer::DiffusorValueType PREASSIGN p_DiffuseCoefficient_ByValue

        ! If the DiffuseCoefficient type is by value,use this
        real(kind=KMCDF)::DiffuseCoefficient_Value PREASSIGN 0.D0

        ! If the DiffuseCoefficient type is by Arrhenius or BCluster(bigger cluster),use this
        real(kind=KMCDF)::PreFactor PREASSIGN 0.D0
        real(kind=KMCDF)::ActEnergy PREASSIGN 0.D0

        integer::ECRValueType PREASSIGN p_ECR_ByValue

        real(kind=KMCDF)::ECR PREASSIGN 0.D0

        contains
        procedure,private,non_overridable,pass::CopyDiffusorValueFromOther
        GENERIC::Assignment(=)=>CopyDiffusorValueFromOther
        !*********Important note: The PGI CUDA Fortran not support the Final symbol, the final symbol would cause the
        !*********Compiler error************
        !Final::CleanDiffusorValue
    END TYPE DiffusorValue

    ! This Entity is not same as the define in standard hashMap, because
    ! 1: One Code is mapped to range of diffusors(key), so we use Code to replace
    !    the diffusor(key)
    ! 2: We can ensure that the diffusors(key) in different ranges are mapped to different
    !    Code(in other words, the Code would not conflict with each other), thus we can use
    !    Code to replace the diffusors(key) while checking whether the generated index is conflicted
    !---
    TYPE,PUBLIC::DiffusorTypeEntity
        integer(kind=KMCLINT)::Code = 0

        type(DiffusorValue)::TheValue

        integer::NextIndex = 0

        contains
        procedure,public,non_overridable,pass::CopyDiffusorTypeEntityFromOther
        Generic::Assignment(=)=>CopyDiffusorTypeEntityFromOther
        !---Similarly, the final procedure cannot be used here---
    END TYPE DiffusorTypeEntity


    TYPE,PUBLIC::DiffusorTypesMap

        integer::MaxDivideGroups_SingleElement = ISHFT(1,8) - 1

        integer::MapBitLength = 16
        integer::MapLength = ISHFT(1,16)

        integer,dimension(:,:),allocatable::SingleAtomsDivideArrays

        type(DiffusorTypeEntity),dimension(:),allocatable::TypesMap

        contains

        procedure,public,non_overridable,pass::put=>putToDiffusorsMap
        procedure,public,non_overridable,pass::get=>getValueFromDiffusorsMap
        procedure,public,non_overridable,pass::constructor=>DiffusorTypesMapConstructor

        procedure,private,non_overridable,pass::getCode
        procedure,private,non_overridable,pass::hash
        procedure,private,non_overridable,pass::GetIndexFor
        procedure,private,non_overridable,pass::CopyDiffusorTypesMapFromOther
        procedure,public,non_overridable,pass::Clean=>Clean_DiffusorTypesMap
        Generic::Assignment(=)=>CopyDiffusorTypesMapFromOther
        Final::CleanDiffusorTypesMap
    END TYPE DiffusorTypesMap

    private::CopyReadedDiffusorValueFromOther
    private::Convert2DiffusorValue
    !private::CleanReadedDiffusorValue
    private::CopyDiffusorValueFromOther
    !private::CleanDiffusorValue
    private::CopyDiffusorTypeEntityFromOther
    private::putToDiffusorsMap
    private::getValueFromDiffusorsMap
    private::DiffusorTypesMapConstructor
    private::getCode
    private::hash
    private::GetIndexFor
    private::CopyDiffusorTypesMapFromOther
    private::Clean_DiffusorTypesMap
    private::CleanDiffusorTypesMap

    contains

    !*************************************
    subroutine CopyReadedDiffusorValueFromOther(this,Others)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedDiffusorValue),intent(out)::this
        type(ReadedDiffusorValue),intent(in)::Others
        !---Body---

        this%symbol = others%symbol

        this%DiffusorValueType = Others%DiffusorValueType

        this%DiffuseCoefficient_Value = Others%DiffuseCoefficient_Value

        this%PreFactor = Others%PreFactor

        this%ActEnergy = Others%ActEnergy

        this%ECRValueType = Others%ECRValueType

        this%ECR  = others%ECR

        return
    end subroutine

    !*******************************************
    function Convert2DiffusorValue(this) result(TheDiffusorValue)
        implicit none
        !---Dummy Vars---
        CLASS(ReadedDiffusorValue),intent(in)::this
        type(DiffusorValue),intent(out)::TheDiffusorValue
        !---Body---

        TheDiffusorValue%DiffusorValueType = this%DiffusorValueType

        TheDiffusorValue%DiffuseCoefficient_Value = this%DiffuseCoefficient_Value

        TheDiffusorValue%PreFactor = this%PreFactor

        TheDiffusorValue%ActEnergy = this%ActEnergy

        TheDiffusorValue%ECRValueType = this%ECRValueType

        TheDiffusorValue%ECR = this%ECR

        return
    end function

    !*******************************************
    subroutine CleanReadedDiffusorValue(this)
        implicit none
        !---Dummy Vars---
        type(ReadedDiffusorValue)::this
        !---Body---
        this%symbol = ""
        this%DiffusorValueType = p_DiffuseCoefficient_ByValue
        this%DiffuseCoefficient_Value = 0.D0
        this%PreFactor = 0.D0
        this%ActEnergy = 0.D0
        this%ECRValueType = p_ECR_ByValue
        this%ECR = 0.D0
        return
    end subroutine

    !*********************************************
    subroutine CopyDiffusorValueFromOther(this,Others)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorValue),intent(out)::this
        type(DiffusorValue),intent(in)::Others
        !---Body---

        this%DiffusorValueType = Others%DiffusorValueType

        this%DiffuseCoefficient_Value = Others%DiffuseCoefficient_Value

        this%PreFactor = Others%PreFactor

        this%ActEnergy = Others%ActEnergy

        this%ECRValueType = Others%ECRValueType

        this%ECR = Others%ECR

        return
    end subroutine


    !*******************************************
    subroutine CleanDiffusorValue(this)
        implicit none
        !---Dummy Vars---
        type(DiffusorValue)::this
        !---Body---
        this%DiffusorValueType = p_DiffuseCoefficient_ByValue
        this%DiffuseCoefficient_Value = 0.D0
        this%PreFactor = 0.D0
        this%ActEnergy = 0.D0
        this%ECRValueType = p_ECR_ByValue
        this%ECR = 0.D0
        return
    end subroutine

    !*******************************************
    subroutine CopyDiffusorTypeEntityFromOther(this,others)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypeEntity),intent(out)::this
        type(DiffusorTypeEntity),intent(in)::others
        !---Body---

        this%Code = others%Code

        !---The (=) hand been overrided
        this%TheValue = others%TheValue

        this%NextIndex = others%NextIndex

        return
    end subroutine

    !********************************************
    subroutine DiffusorTypesMapConstructor(this,SingleAtomsDivideArrays,MapLength)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        integer,dimension(:,:),allocatable::SingleAtomsDivideArrays
        integer::MapLength
        !---Local Vars---
        integer::tempLength
        integer::MapBitLength
        !---Body---

        MapBitLength = 0

        tempLength = MapLength

        if(size(SingleAtomsDivideArrays,DIM=1) .ne. p_ATOMS_GROUPS_NUMBER) then
            write(*,*) "MCPSCUERROR: In current version, the max elements group is ",p_ATOMS_GROUPS_NUMBER
            write(*,*) "However, the SingleAtomsDivideArrays own elements group: ",size(SingleAtomsDivideArrays,DIM=1)
            pause
            stop
        end if

        this%MaxDivideGroups_SingleElement = size(SingleAtomsDivideArrays,DIM=2)


        this%MapLength = 0


        DO While(tempLength .GT. 0)
            this%MapLength = ISHFT(this%MapLength,1)
            this%MapLength = this%MapLength + 1
            tempLength = ISHFT(tempLength,-1)
            MapBitLength = MapBitLength + 1

        END DO

        this%MapBitLength = MapBitLength

        call AllocateArray_Host(this%SingleAtomsDivideArrays,p_ATOMS_GROUPS_NUMBER,this%MaxDivideGroups_SingleElement,"SingleAtomsDivideArrays")

        this%SingleAtomsDivideArrays = SingleAtomsDivideArrays

        allocate(this%TypesMap(this%MapLength))

        return
    end subroutine DiffusorTypesMapConstructor

    !********************************************
    subroutine CopyDiffusorTypesMapFromOther(this,Others)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap),intent(out)::this
        type(DiffusorTypesMap),intent(in)::Others
        !---Body---
        this%MaxDivideGroups_SingleElement = Others%MaxDivideGroups_SingleElement
        this%MapLength = Others%MapLength
        this%MapBitLength = Others%MapBitLength

        this%SingleAtomsDivideArrays = Others%SingleAtomsDivideArrays
        this%TypesMap = Others%TypesMap

        return
    end subroutine

    !**********************************************
    subroutine Clean_DiffusorTypesMap(this)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        !---Body---
        this%MaxDivideGroups_SingleElement = 0

        this%MapBitLength = 0

        this%MapLength = 0

        call DeAllocateArray_Host(this%SingleAtomsDivideArrays,"SingleAtomsDivideArrays")

        if(allocated(this%TypesMap)) then
            deallocate(this%TypesMap)
        end if

        return
    end subroutine Clean_DiffusorTypesMap

    !**********************************************
    subroutine CleanDiffusorTypesMap(this)
        implicit none
        !---Dummy Vars---
        type(DiffusorTypesMap)::this
        !---Body---

        call this%Clean()

        return
    end subroutine

    !**********************************
    subroutine putToDiffusorsMap(this,Key,TheValue)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        type(ACluster)::Key
        CLASS(DiffusorValue)::TheValue
        !---Local Vars---
        integer(kind=KMCLINT)::Code
        integer(kind=KMCLINT)::reSparedCode
        integer(kind=KMCLINT)::IndexFor
        integer(kind=KMCLINT)::NextIndex
        integer::ICount
        !---Body---
        Code = this%getCode(Key%m_Atoms)

        reSparedCode = this%hash(Code)

        IndexFor = this%GetIndexFor(reSparedCode)

        ! Handle the conflictions
        if(this%TypesMap(IndexFor)%Code .NE. 0 .AND. Code .NE. this%TypesMap(IndexFor)%Code) then

            NextIndex = this%TypesMap(IndexFor)%NextIndex

            ICount = 1
            DO While(NextIndex .GT. 0 .AND. Code .NE. this%TypesMap(IndexFor)%Code)

                if(Code .eq. this%TypesMap(IndexFor)%Code) then
                    write(*,*) "MCPSCUERROR: The diffusor is redefined !"
                    write(*,*) "Diffusor : ",Key%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA
                    pause
                    stop
                end if

                ICount = ICount + 1

                if(ICount .GT. this%MapLength) then
                    write(*,*) "MCPSCUERROR: The difussor map is not sufficient to store all kinds of diffusor."
                    pause
                    stop
                end if

                IndexFor = NextIndex

                NextIndex = this%TypesMap(IndexFor)%NextIndex

            END DO

            ICount = 1
            DO While(.true.)
                ICount = ICount + 1
                if(ICount .GT. this%MapLength) then
                    write(*,*) "MCPSCUERROR: The difussor map is not sufficient to store all kinds of diffusor."
                    pause
                    stop
                end if

                reSparedCode = reSparedCode + 1

                NextIndex = this%GetIndexFor(reSparedCode)

                if(NextIndex .LE. 0 .or. NextIndex .GT. this%MapLength) then
                    reSparedCode = 1
                else if(this%TypesMap(NextIndex)%Code .eq. 0) then
                    exit
                end if

            END DO

            this%TypesMap(IndexFor)%NextIndex = NextIndex

            IndexFor = NextIndex
        end if

        this%TypesMap(IndexFor)%Code = Code
        this%TypesMap(IndexFor)%TheValue = TheValue

        return
    end subroutine putToDiffusorsMap

    !**********************************
    function getValueFromDiffusorsMap(this,Key) result(TheValue)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        type(ACluster)::Key
        type(DiffusorValue)::TheValue
        !---Local Vars---
        integer(kind=KMCLINT)::Code
        integer(kind=KMCLINT)::reSparedCode
        integer(kind=KMCLINT)::IndexFor
        integer(kind=KMCLINT)::NextIndex
        !---Body---
        Code = this%getCode(Key%m_Atoms)

        reSparedCode = this%hash(Code)

        IndexFor = this%GetIndexFor(reSparedCode)

        DO While(IndexFor .GT. 0)

            if(this%TypesMap(IndexFor)%Code .eq. Code) then
                TheValue = this%TypesMap(IndexFor)%TheValue
                exit
            end if

            IndexFor = this%TypesMap(IndexFor)%NextIndex
        END DO


        return
    end function getValueFromDiffusorsMap

    !**********************************
    function getCode(this,Atoms) result(Code)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        type(Single_AtomsSet)::Atoms(:)
        integer(kind=KMCLINT)::Code
        !---Local Vars---
        integer::I
        integer::J
        !---Body---
        Code = 0

        DO I = 1,p_ATOMS_GROUPS_NUMBER

            J = BinarySearch_GE(Atoms(I)%m_NA,this%SingleAtomsDivideArrays(I,:),1,this%MaxDivideGroups_SingleElement)

            Code = ISHFT(Code,this%MapBitLength) + J

        END DO

        return
    end function getCode

    !**********************************
    function hash(this,Code) result(reSparedCode)
        implicit none
        ! Purpose: to spare the code to be more uniform
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        integer(kind=KMCLINT)::Code
        integer(kind=KMCLINT)::reSparedCode
        !---Local Vars---
        integer(kind=KMCLINT)::TempCode
        !---Body---
        reSparedCode = Code
        TempCode = Code

        TempCode = ISHFT(TempCode,-this%MapBitLength)

        DO While(TempCode .GT. 0)

            reSparedCode = IOR(reSparedCode,IBITS(TempCode,0,this%MapBitLength-1))

            TempCode = ISHFT(TempCode,-this%MapBitLength)

        END DO


        return
    end function hash

    !************************************
    function GetIndexFor(this,Code) result(IndexFor)
        implicit none
        !---Dummy Vars---
        CLASS(DiffusorTypesMap)::this
        integer(kind=KMCLINT)::Code
        integer(kind=KMCLINT)::IndexFor
        !---Body---
        IndexFor = IAND(Code,this%MapLength)

        return
    end function GetIndexFor



end module MCLIB_TYPEDEF_DiffusorsValue
