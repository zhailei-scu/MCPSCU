module INLET_CONTINUEIMPLANTATION_GPU
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none


    TYPE,public,extends(ImplantSection)::ContinueImplantSection

        contains

    END TYPE

    TYPE,public::ContinueImplantList
        type(ContinueImplantSection)::TheContinueImplantSection

        type(ContinueImplantList),pointer::next=>null()

        integer::ListCount=0

    END TYPE

    contains




end module INLET_CONTINUEIMPLANTATION_GPU
