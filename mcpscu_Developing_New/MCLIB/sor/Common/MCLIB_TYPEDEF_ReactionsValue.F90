#include "../../../Macro"

module MCLIB_TYPEDEF_REACTIONSVALUE
    use MCLIB_CONSTANTS
    use iso_c_binding
    implicit none

    integer,parameter::p_ReactionCoefficientTypesNum = 2
    integer,parameter::p_ReactionCoefficient_ByValue = 1
    integer,parameter::p_ReactionCoefficient_ByArrhenius = 2

    integer,parameter::p_ECRTypesNum = 2
    integer,parameter::p_ECR_ByValue = 1
    integer,parameter::p_ECR_ByBCluster = 2

    type,public::ReadReactionPair
        character(kind=c_char,len=20)::SubjectSymbol = ""
        character(kind=c_char,len=20)::ObjectSymbol = ""

        integer(c_int)::ReactionCoefficientType = p_ReactionCoefficient_ByValue

        real(c_double)::ReactionCoefficient_Value = -1.D0 ! < 0 means not occur, >=1 means must occur
        real(c_double)::PreFactor = 0.D0
        real(c_double)::ActEnergy = 0.D0

        integer(c_int)::ECRValueType = p_ECR_ByValue
        real(c_double)::ECR = 0.D0

    end type ReadReactionPair

    type,public::ReactionValue
        integer::ReactionCoefficientType PREASSIGN p_ReactionCoefficient_ByValue

        real(kind=KMCDF)::ReactionCoefficient_Value PREASSIGN -1.D0 ! < 0 means not occur, >=1 means must occur
        real(kind=KMCDF)::PreFactor PREASSIGN 0.D0
        real(kind=KMCDF)::ActEnergy PREASSIGN 0.D0

        integer::ECRValueType PREASSIGN p_ECR_ByValue
        real(kind=KMCDF)::ECR PREASSIGN 0.D0

    end type ReactionValue

    type,public::ReactionEntity
        integer(kind=KMCLINT)::Code PREASSIGN 0

        type(ReactionValue)::TheValue

        integer::NextIndex PREASSIGN 0
    end type ReactionEntity

    type,public::ReactionsMap
        integer::MapBitLength = 16
        integer::MapLength = ISHFT(1,16)

        integer,dimension(:,:),allocatable::SingleAtomsDivideArrays

        type(ReactionEntity),dimension(:),allocatable::RecordsMap

    end type

    contains



end module MCLIB_TYPEDEF_REACTIONSVALUE
