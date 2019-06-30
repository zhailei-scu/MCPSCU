module MODEL_ECR_CPU
    use MSM_CONSTANTS
    use MODEL_TYPEDEF_ATOMSLIST
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

    integer,public::m_H_Index
    integer,public::m_He_Index
    integer,public::m_SIA_Index_W
    integer,public::m_Vac_Index_W


    !----------------------------------------------
    real(kind=KINDDF)::m_SURFE                                ! the surface energy. In ERG/cm**2
    real(kind=KINDDF)::m_BiasFactor
    real(kind=KINDDF)::m_Corrections
    real(kind=KINDDF)::m_R0
    real(kind=KINDDF)::m_R1


    contains

    function Cal_ECR_ModelDataBase(ECR_CalModelType,NAtoms,TKB,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars--
        integer,intent(in)::ECR_CalModelType
        integer,intent(in)::NAtoms(:)
        real(kind=KINDDF),intent(in)::TKB
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = 0.D0

        select case(ECR_CalModelType)
            case(p_ECR_ByBCluster)
                TheECR = Cal_ECR_ByBCluster(sum(NAtoms),TKB)
            case(p_ECR_SIACluster_W)
                TheECR = Cal_ECR_SIACluster_W(NAtoms,LatticeLength)
            case(p_ECR_VacancyCluster_W)
                TheECR = Cal_ECR_VacancyCluster_W(NAtoms,LatticeLength)
            case(p_ECR_PureHeCluster_W)
                TheECR = Cal_ECR_PureHeCluster_W(NAtoms,LatticeLength)
            case(p_ECR_HeSIACluster_W)
                TheECR = Cal_ECR_HeSIACluster_W(NAtoms,LatticeLength)
            case(p_ECR_HeVacancyCluster_W)
                TheECR = Cal_ECR_HeVacancyCluster_W(NAtoms,LatticeLength)
        end select

        return
    end function Cal_ECR_ModelDataBase

    ! Convert the number of atoms to radius
    ! Ref. Modelling Simul. Mater. Sci. Eng.16(2008)055003
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

    ! Ref.Journal of Nuclear Materials 403 (2010) 75–88
    !*************************************************************
    function Cal_ECR_SIACluster_W(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = m_BiasFactor*( (m_R0 + m_Corrections) +                                        &
                              (ATOMS(m_SIA_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH -  &
                              (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH )

        return
    end function Cal_ECR_SIACluster_W

    !*************************************************************
    function Cal_ECR_VacancyCluster_W(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (m_R0 + m_Corrections) +                                          &
                 (ATOMS(m_Vac_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH -  &
                 (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH

        return
    end function Cal_ECR_VacancyCluster_W

    !*************************************************************
    function Cal_ECR_PureHeCluster_W(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (m_R1 + m_Corrections) +                                        &
                 (ATOMS(m_He_Index)*CP_3_4PI*(LatticeLength**3)/10.D0)**C_UTH -  &
                 (CP_3_4PI*(LatticeLength**3)/10.D0)**C_UTH

        return
    end function Cal_ECR_PureHeCluster_W


    !*************************************************************
    function Cal_ECR_HeVacancyCluster_W(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (m_R0 + m_Corrections) +                                         &
                 (ATOMS(m_Vac_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                 (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH

        return
    end function Cal_ECR_HeVacancyCluster_W

    !*************************************************************
    function Cal_ECR_HeSIACluster_W(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = m_BiasFactor*((m_R0 + m_Corrections) +                                        &
                              (ATOMS(m_SIA_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                              (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH )

        return
    end function Cal_ECR_HeSIACluster_W

    !*************************************************************
    subroutine resolveModelRelativeData(MODELData,TheAtomsList)
        !---Dummy Vars---
        type(Statementlist),pointer::MODELData
        type(AtomsList)::TheAtomsList
        !---Local Vars---
        integer::LINE
        character*256::STR
        character*32::KEYWORD
        character*20::STRTEMP(10)
        integer::N
        integer::I
        integer::hFile
        !---Body---
        hFile = 6

        m_H_Index = TheAtomsList%FindIndexBySymbol("H")
        m_He_Index = TheAtomsList%FindIndexBySymbol("He")
        m_SIA_Index_W = TheAtomsList%FindIndexBySymbol("W")
        m_Vac_Index_W = TheAtomsList%FindIndexBySymbol("Vc")

        KEYWORD = "&SURFE"
        call UPCASE(KEYWORD)
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), MODELData, STR, LINE)
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for surface energy at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &SURFE THE SURFACE ENERGY OF A BUBBLE = ! (ERG/CM^2))"
            pause
            stop
        end if
        m_SURFE = DRSTR(STRTEMP(1))

        KEYWORD = "&BiasFactor"
        call UPCASE(KEYWORD)
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), MODELData, STR, LINE)
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for BiasFactor at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &BiasFactor value"
            pause
            stop
        end if
        m_BiasFactor = DRSTR(STRTEMP(1))

        KEYWORD = "&Corrections"
        call UPCASE(KEYWORD)
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), MODELData, STR, LINE)
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for Corrections at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &Corrections value"
            pause
            stop
        end if
        m_Corrections = DRSTR(STRTEMP(1))

        KEYWORD = "&R0"
        call UPCASE(KEYWORD)
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), MODELData, STR, LINE)
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for R0 at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &R0 value"
            pause
            stop
        end if
        m_R0 = DRSTR(STRTEMP(1))

        KEYWORD = "&R1"
        call UPCASE(KEYWORD)
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), MODELData, STR, LINE)
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for R1 at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &R1 value"
            pause
            stop
        end if
        m_R1 = DRSTR(STRTEMP(1))

        write(*,*) "***************The Model Relative Data****************"
        write(hFile,fmt="('!',A70,'!',2x,I10)") "Element H Index = ",m_H_Index
        write(hFile,fmt="('!',A70,'!',2x,I10)") "Element He Index = ",m_He_Index
        write(hFile,fmt="('!',A70,'!',2x,I10)") "Element SIA of W matrix Index = ",m_SIA_Index_W
        write(hFile,fmt="('!',A70,'!',2x,I10)") "Element Vacancy of W matrix Index = ",m_Vac_Index_W
        write(hFile,fmt="('!',A70,'!',2x,I10)") "Element Vacancy of W matrix Index = ",m_Vac_Index_W
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "Surface energy of = ",m_SURFE
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "Bias Factor = ",m_BiasFactor
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "Correction factor (cm) = ",m_Corrections
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "R0 value (cm) = ",m_R0
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "R1 value (cm) = ",m_R1

        return
    end subroutine resolveModelRelativeData


end module
