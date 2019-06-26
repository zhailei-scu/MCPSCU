module INLET_CASCADEIMPLANTATION_GPU
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none

    character(len=14),private,parameter::OKMC_CASCADE_FORMAT18 = "&CASCADEOKMC18"
    character(len=12),private,parameter::SRIM_CASCADE = "&CASCADESRIM"
    character(len=10),private,parameter::MD_CASCADE = "&CASCADEMD"
    character(len=16),private,parameter::MARLOWER_CASCADE = "&CASCADEMARLOWER"


    TYPE,public,extends(ImplantSection)::CascadeImplantSection
        integer::InletClustersNumOneCase = 0





    END TYPE


    contains



end module INLET_CASCADEIMPLANTATION_GPU
