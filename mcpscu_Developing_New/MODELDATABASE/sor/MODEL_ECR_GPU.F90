module MODEL_ECR_GPU
    use cudafor
    use MODEL_ECR_CPU
    implicit none

    real(kind=KINDDF),constant::dm_SURFE
    real(kind=KINDDF),constant::dm_BiasFactor
    real(kind=KINDDF),constant::dm_Corrections
    real(kind=KINDDF),constant::dm_R0
    real(kind=KINDDF),constant::dm_R1


    integer,constant::dm_H_Index
    integer,constant::dm_He_Index
    integer,constant::dm_SIA_Index_W
    integer,constant::dm_Vac_Index_W

    contains

    subroutine CopyModelRelativeDataToDev()
        implicit none

        dm_H_Index = m_H_Index
        dm_He_Index = m_He_Index
        dm_SIA_Index_W = m_SIA_Index_W
        dm_Vac_Index_W = m_Vac_Index_W
        !-----------------

        dm_SURFE = m_SURFE

        dm_BiasFactor = m_BiasFactor

        dm_Corrections = m_Corrections

        dm_R0 = m_R0

        dm_R1 = m_R1

        return
    end subroutine CopyModelRelativeDataToDev


    attributes(device) function Cal_ECR_ModelDataBase_Dev(ECR_CalModelType,NAtoms,TKB,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars--
        integer,intent(in)::ECR_CalModelType
        integer,intent(in),dimension(:)::NAtoms
        real(kind=KINDDF),intent(in)::TKB
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = 0.D0

        select case(ECR_CalModelType)
            case(p_ECR_ByBCluster)
                TheECR = Cal_ECR_ByBCluster_Dev(sum(NAtoms,dim=1),TKB)
            case(p_ECR_SIACluster_W)
                TheECR = Cal_ECR_SIACluster_W_Dev(NAtoms,LatticeLength)
            case(p_ECR_VacancyCluster_W)
                TheECR = Cal_ECR_VacancyCluster_W_Dev(NAtoms,LatticeLength)
            case(p_ECR_PureHeCluster_W)
                TheECR = Cal_ECR_PureHeCluster_W_Dev(NAtoms,LatticeLength)
            case(p_ECR_HeSIACluster_W)
                TheECR = Cal_ECR_HeSIACluster_W_Dev(NAtoms,LatticeLength)
            case(p_ECR_HeVacancyCluster_W)
                TheECR = Cal_ECR_HeVacancyCluster_W_Dev(NAtoms,LatticeLength)
        end select

        return
    end function Cal_ECR_ModelDataBase_Dev



    !****************************************************
    attributes(device) function Cal_ECR_ByBCluster_Dev(NAtoms,Temp_TKB) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NAtoms
        real(kind=KINDDF),intent(in)::Temp_TKB
        real(kind=KINDDF),intent(out)::TheECR
        !---Local Vars---
        real(kind=KINDDF)::RNFACTOR
        !---Body---
        RNFACTOR = 8.D0*CP_PI*dm_SURFE/(3.D0*Temp_TKB)

        TheECR = DSQRT(TheECR/RNFACTOR)

        return
    end function Cal_ECR_ByBCluster_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_SIACluster_W_Dev(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = dm_BiasFactor*( (dm_R0 + dm_Corrections) +                                     &
                              (ATOMS(dm_SIA_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                              (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH )

        return
    end function Cal_ECR_SIACluster_W_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_VacancyCluster_W_Dev(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (dm_R0 + dm_Corrections) +                                         &
                 (ATOMS(dm_Vac_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH -  &
                 (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH

        return
    end function Cal_ECR_VacancyCluster_W_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_PureHeCluster_W_Dev(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (dm_R1 + dm_Corrections) +                                      &
                 (ATOMS(dm_He_Index)*CP_3_4PI*(LatticeLength**3)/10.D0)**C_UTH - &
                 (CP_3_4PI*(LatticeLength**3)/10.D0)**C_UTH

        return
    end function Cal_ECR_PureHeCluster_W_Dev


    !*************************************************************
    attributes(device) function Cal_ECR_HeVacancyCluster_W_Dev(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = (dm_R0 + dm_Corrections) +                                        &
                 (ATOMS(dm_Vac_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                 (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH

        return
    end function Cal_ECR_HeVacancyCluster_W_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_HeSIACluster_W_Dev(ATOMS,LatticeLength) result(TheECR)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::ATOMS(:)
        real(kind=KINDDF),intent(in)::LatticeLength
        real(kind=KINDDF),intent(out)::TheECR
        !---Body---

        TheECR = dm_BiasFactor*((dm_R0 + dm_Corrections) +                                      &
                              (ATOMS(dm_SIA_Index_W)*CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH - &
                              (CP_3_4PI*(LatticeLength**3)/2.D0)**C_UTH )

        return
    end function Cal_ECR_HeSIACluster_W_Dev


end module
