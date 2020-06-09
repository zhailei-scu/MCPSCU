module INLET_BATCHIMPLANTATION_GPU
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none




    TYPE,public,extends(ImplantSection)::BatchImplantSection


        contains

    END TYPE

    TYPE,public::BatchImplantList
        type(BatchImplantSection)::TheBatchImplantSection

        type(BatchImplantList),pointer::next=>null()

        integer::ListCount=0

    END TYPE

    contains



end module INLET_BATCHIMPLANTATION_GPU
