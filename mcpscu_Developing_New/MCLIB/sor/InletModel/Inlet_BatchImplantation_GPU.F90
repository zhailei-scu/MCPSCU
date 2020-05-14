module INLET_BATCHIMPLANTATION_GPU
    use MC_TYPEDEF_IMPLANTATIONSECTION
    implicit none

    character(len=14),private,parameter::OKMC_CASCADE_FORMAT18 = "&CASCADEOKMC18"
    character(len=12),private,parameter::SRIM_CASCADE = "&CASCADESRIM"
    character(len=10),private,parameter::MD_CASCADE = "&CASCADEMD"
    character(len=16),private,parameter::MARLOWER_CASCADE = "&CASCADEMARLOWER"


    TYPE,public,extends(ImplantSection)::CascadeImplantSection
        integer::InletClustersNumOneCase = 0


    END TYPE

    TYPE,public::CascadeImplantList
        type(CascadeImplantSection)::TheCascadeImplantSection

        type(CascadeImplantList),pointer::next=>null()

        integer::ListCount=0

    END TYPE


    type(CascadeImplantList),target::m_CascadeImplantList

    contains



end module INLET_BATCHIMPLANTATION_GPU
