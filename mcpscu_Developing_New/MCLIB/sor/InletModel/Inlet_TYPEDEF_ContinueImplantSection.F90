module INLET_CONTINUEIMPLANTATION_GPU
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none


    TYPE,public,extends(ImplantSection)::ContinueImplantSection

        contains

        procedure,non_overridable,public,pass::LoadOne_ImplantSection
    END TYPE

    TYPE,public::ContinueImplantList
        type(ContinueImplantSection)::TheContinueImplantSection

        type(ContinueImplantList),pointer::next=>null()

        integer::ListCount=0

    END TYPE

    contains

    !*****The declare for abstract method***************
    !*****Note: this is not same with the "Fortran 95/2003 For Scientists and Engineers, Third Edition (chinese version)(P668)"
    !*****Because the abstract useage way in this book is not depended on PGFORTRAN compiler. The  following is based on our test
    !*****and verified to suit for pgfortran.
    subroutine LoadOne_ImplantSection(this,hFile,SimBoxes,Host_SimuCtrlParam,LINE)
            implicit none
            !---Dummy Vars---
            CLASS(ContinueImplantSection)::this
            integer, intent(in)::hFile
            type(SimulationBoxes)::SimBoxes
            type(SimulationCtrlParam)::Host_SimuCtrlParam
            integer::LINE
    end subroutine


end module INLET_CONTINUEIMPLANTATION_GPU
