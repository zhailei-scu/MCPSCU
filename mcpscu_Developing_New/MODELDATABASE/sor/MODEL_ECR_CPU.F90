module MODEL_ECR_CPU
    use MSM_CONSTANTS
    use MSM_TYPEDEF_InputPaser
    implicit none

    integer,parameter::p_ECRTypesNum = 7
    integer,parameter::p_ECR_ByValue = 1
    integer,parameter::p_ECR_ByBCluster = 2
    integer,parameter::p_ECR_SIACluster_W = 3
    integer,parameter::p_ECR_VacancyCluster_W = 4
    integer,parameter::p_ECR_PureHeCluster_W = 5
    integer,parameter::p_ECR_HeVacancyCluster_W = 6
    integer,parameter::p_ECR_HeSIACluster_W = 7


    !----------------------------------------------
    real(kind=KINDDF)::m_SURFE                                ! the surface energy. In ERG/cm**2


    contains

    function Cal_ECR_ByBCluster(NAtoms,Temp_TKB) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NAtoms
        real(kind=KINDDF),intent(in)::Temp_TKB
        real(kind=KINDDF),intent(out)::TheECR
        !---Local Vars---
        real(kind=KINDDF)::RNFACTOR
        !---Body---
        RNFACTOR = 8.D0*CP_PI*m_SURFE/(3.D0*Temp_TKB)

        TheECR = DSQRT(TheECR/RNFACTOR)

        return
    end function Cal_ECR_ByBCluster

    !*************************************************************
    function Cal_ECR_SIACluster_W(NSIAs,LatticeLength,BiasFactor,Corrections) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NSIAs
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(in)::BiasFactor
        real(kind=KINDDF),intent(in)::Corrections
        real(kind=KINDDF),intent(out)::TheECR
        !---Dummy Vars---
        real(kind=KINDDF)::R0
        !---Body---
        R0 = CP_RSQ3*LatticeLength/4.D0

        TheECR = BiasFactor*( (R0 + Corrections) +                              &
                              (NSIAs*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                              (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH )

        return
    end function Cal_ECR_SIACluster_W

    !*************************************************************
    function Cal_ECR_VacancyCluster_W(NVacs,LatticeLength,Corrections) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NVacs
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(in)::Corrections
        real(kind=KINDDF),intent(out)::TheECR
        !---Dummy Vars---
        real(kind=KINDDF)::R0
        !---Body---
        R0 = CP_RSQ3*LatticeLength/4.D0

        TheECR = (R0 + Corrections) +                              &
                 (NVacs*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                 (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH

        return
    end function Cal_ECR_VacancyCluster_W

    !*************************************************************
    function Cal_ECR_PureHeCluster_W(NHe,R1,LatticeLength,Corrections) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NHe
        real(kind=KINDDF)::R1
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(in)::Corrections
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (R1 + Corrections) +                              &
                 (NHe*CP_3_4PI*(LatticeLength**3)/10.D0)**C_UTH - &
                 (CP_3_4PI*(LatticeLength**3)/10.D0)**C_UTH

        return
    end function Cal_ECR_PureHeCluster_W


    !*************************************************************
    function Cal_ECR_HeVacancyCluster_W(NHe,NVacs,LatticeLength,Corrections) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NHe
        integer,intent(in)::NVacs
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(in)::Corrections
        real(kind=KINDDF),intent(out)::TheECR
        !---Dummy Vars---
        real(kind=KINDDF)::R0
        !---Body---
        R0 = CP_RSQ3*LatticeLength/4.D0

        TheECR = (R0 + Corrections) +                              &
                 (NVacs*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                 (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH

        return
    end function Cal_ECR_HeVacancyCluster_W

    !*************************************************************
    function Cal_ECR_HeSIACluster_W(NHe,NSIAs,LatticeLength,BiasFactor,Corrections) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NHe
        integer,intent(in)::NSIAs
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(in)::BiasFactor
        real(kind=KINDDF),intent(in)::Corrections
        real(kind=KINDDF),intent(out)::TheECR
        !---Dummy Vars---
        real(kind=KINDDF)::R0
        !---Body---
        R0 = CP_RSQ3*LatticeLength/4.D0

        TheECR = BiasFactor*( (R0 + Corrections) +                              &
                              (NSIAs*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                              (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH )

        return
    end function Cal_ECR_HeSIACluster_W

    !*************************************************************
    subroutine resolveModelRelativeData(AddOnData)
        !---Dummy Vars---
        type(Statementlist),pointer::AddOnData
        !---Local Vars---
        integer::LINE
        character*256::STR
        character*32::KEYWORD
        character*20::STRTEMP(10)
        integer::N
        integer::I
        !---Body---

        KEYWORD = "&SURENG"
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), AddOnData, STR, LINE)
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for surface energy at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &SURENG THE SURFACE ENERGY OF A BUBBLE = ! (ERG/CM^2))"
            pause
            stop
        end if
        m_SURFE = DRSTR(STRTEMP(1))

        return
    end subroutine resolveModelRelativeData


end module
