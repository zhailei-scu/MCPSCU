module MODEL_ECR_GPU
    use cudafor
    use MODEL_ECR_CPU
    implicit none

    real(kind=KINDDF),constant::dm_SURFE

    contains

    subroutine CopyModelRelativeDataToDev()
        implicit none

        dm_SURFE = m_SURFE

        return
    end subroutine CopyModelRelativeDataToDev

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
    attributes(device) function Cal_ECR_SIACluster_W_Dev(NSIAs,LatticeLength,BiasFactor,Corrections) result(TheECR)
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
    end function Cal_ECR_SIACluster_W_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_VacancyCluster_W_Dev(NVacs,LatticeLength,Corrections) result(TheECR)
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
    end function Cal_ECR_VacancyCluster_W_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_PureHeCluster_W_Dev(NHe,R1,LatticeLength,Corrections) result(TheECR)
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
    end function Cal_ECR_PureHeCluster_W_Dev


    !*************************************************************
    attributes(device) function Cal_ECR_HeVacancyCluster_W_Dev(NHe,NVacs,LatticeLength,Corrections) result(TheECR)
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
    end function Cal_ECR_HeVacancyCluster_W_Dev

    !*************************************************************
    attributes(device) function Cal_ECR_HeSIACluster_W_Dev(NHe,NSIAs,LatticeLength,BiasFactor,Corrections) result(TheECR)
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
    end function Cal_ECR_HeSIACluster_W_Dev


end module
