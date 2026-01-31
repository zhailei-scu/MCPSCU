!*********************************************************************************!
!--- Description:
!--- Author : Lei Zhai, Insti. of Nucle. Sci. and Tech., Sichuan University
!--- Email : zhaileiytp@163.com
!--- data: From 2017-09 to 2021-12
!--- License: MIT License (There are no limitation for anyone to use/modify/sale of this code, be happy to use this code)
!--- Please ref "Lei Zhai, Chaoqiong Ma, Jiechao Cui and Qing Hou, GPU-based acceleration of Monte Carlo simulations for migration-coalescence evolution of gas bubbles in materials
!---                        Modelling Simul. Mater. Sci. Eng. 2019. 27 055008,https://iopscience.iop.org/article/10.1088/1361-651X/ab1d14
!*********************************************************************************!
module MC_GenerateCascadeBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE

    implicit none

    integer,parameter::CascadeGenWay_ByCentUniform_Locally = 0
    integer,parameter::CascadeGenWay_ByMDDataBase_Locally_Resample = 1
    integer,parameter::CascadeGenWay_ByMDDataBase_Uniform_Resample = 2
    integer,parameter::CascadeGenWay_ByMDDataBase_Locally_Directly = 3

    integer,parameter::CascadePosModel_ByVolumeAverage = 0
    integer,parameter::CascadePosModel_ByUserSpeicaled = 1
    integer,parameter::CascadePosModel_ByRandom = 2

    type,public::ClusterAtom
        real(kind=KINDDF)::POS(3)
    end type


    type,public::MDStatistic
        integer::MaxSIANumEachCluster = -1.D0
        integer::MinSIANumEachCluster = 1.D32
        integer::MaxVACNumEachCluster = -1.D0
        integer::MinVACNumEachCluster = 1.D32
        integer::MaxMIXNumEachCluster = -1.D0
        integer::MinMIXNumEachCluster = 1.D32
        real(kind=KINDDF)::MaxDistanceSIA_ToCent = -1.D0
        real(kind=KINDDF)::MaxDistanceVAC_ToCent = -1.D0
        real(kind=KINDDF)::MaxDistanceMIX_ToCent = -1.D0
        real(kind=KINDDF)::MaxDistanceSIA_BetweenCluster = -1.D0
        real(kind=KINDDF)::MaxDistanceVAC_BetweenCluster = -1.D0
        real(kind=KINDDF)::MaxDistanceMIX_BetweenCluster = -1.D0
        real(kind=KINDDF)::MaxDistanceSIAToVAC_BetweenCluster = -1.D0
        real(kind=KINDDF)::MinDistanceSIA_ToCent = 1.D32
        real(kind=KINDDF)::MinDistanceVAC_ToCent = 1.D32
        real(kind=KINDDF)::MinDistanceMIX_ToCent = 1.D32
        real(kind=KINDDF)::MinDistanceSIA_BetweenCluster = 1.D32
        real(kind=KINDDF)::MinDistanceVAC_BetweenCluster = 1.D32
        real(kind=KINDDF)::MinDistanceMIX_BetweenCluster = 1.D32
        real(kind=KINDDF)::MinDistanceSIAToVAC_BetweenCluster = 1.D32
        real(kind=KINDDF)::BinWidthSIA_ToCenter = 0.D0
        real(kind=KINDDF)::BinWidthVAC_ToCenter = 0.D0
        real(kind=KINDDF)::BinWidthMIX_ToCenter = 0.D0
        real(kind=KINDDF)::BinWidthSIA_BetweenCluster = 0.D0
        real(kind=KINDDF)::BinWidthVAC_BetweenCluster = 0.D0
        real(kind=KINDDF)::BinWidthMIX_BetweenCluster = 0.D0
        real(kind=KINDDF)::BinWidthSIAToVAC_BetweenCluster = 0.D0
        real(kind=KINDDF),dimension(:),allocatable::ToCent_DistSIA
        real(kind=KINDDF),dimension(:),allocatable::ToCent_DistVAC
        real(kind=KINDDF),dimension(:),allocatable::ToCent_DistMIX
        real(kind=KINDDF),dimension(:),allocatable::ClusterGap_DistSIA
        real(kind=KINDDF),dimension(:),allocatable::ClusterGap_DistVAC
        real(kind=KINDDF),dimension(:),allocatable::ClusterGap_DistMIX
        real(kind=KINDDF),dimension(:),allocatable::ClusterGap_SIAToVAC
        real(kind=KINDDF),dimension(:),allocatable::NAtomSIA
        real(kind=KINDDF),dimension(:),allocatable::NAtomVAC
        real(kind=KINDDF),dimension(:),allocatable::NAtomMIX
        real(kind=KINDDF),dimension(:),allocatable::BinSIA_ToCenterArray
        real(kind=KINDDF),dimension(:),allocatable::BinSIA_BetweenClusterArray
        real(kind=KINDDF),dimension(:),allocatable::BinVAC_ToCenterArray
        real(kind=KINDDF),dimension(:),allocatable::BinVAC_BetweenClusterArray
        real(kind=KINDDF),dimension(:),allocatable::BinSIAToVAC_BetweenClusterArray
        real(kind=KINDDF),dimension(:),allocatable::BinMIX_ToCenterArray
        real(kind=KINDDF),dimension(:),allocatable::BinMIX_BetweenClusterArray
        integer,dimension(:),allocatable::BinSIA_NAtomArray
        integer,dimension(:),allocatable::BinVAC_NAtomArray
        integer,dimension(:),allocatable::BinMIX_NAtomArray
        integer::NFrankelParisEachBox_AVE = 0
        real(kind=KINDDF)::TotalCountSIA_GapCToC = 0
        real(kind=KINDDF)::TotalCountVAC_GapCToC = 0
        real(kind=KINDDF)::TotalCount_GapSIAToVAC = 0
        real(kind=KINDDF)::TotalCountMIX_GapCToC = 0
        real(kind=KINDDF)::TotalCountSIA_ToCent = 0
        real(kind=KINDDF)::TotalCountVAC_ToCent = 0
        real(kind=KINDDF)::TotalCountMIX_ToCent = 0
        real(kind=KINDDF)::TotalCountSIA_NAtom = 0
        real(kind=KINDDF)::TotalCountVAC_NAtom = 0
        real(kind=KINDDF)::TotalCountMIX_NAtom = 0

        contains
        procedure,non_overridable,public,pass::CopyMDStatisticFromOther
        procedure,non_overridable,public,pass::Clean_MDStatistic
        Generic::Assignment(=)=>CopyMDStatisticFromOther
        Final::CleanMDStatistic
    end type MDStatistic

    INTERFACE CopyArray
        MODULE PROCEDURE CopyArray_D
        MODULE PROCEDURE CopyArray_I
    END INTERFACE

    integer,parameter::MaxNumLen = 4

    integer,parameter::BinNum = 100

    integer,parameter::SIAIndex_MD = 1
    integer,parameter::SIAIndex2_MD = 2
    integer,parameter::SIAIndex3_MD = 3
    integer,parameter::VACIndex_MD = 4
    integer,parameter::RefSiteIndex_MD = 8

    real(kind=KINDDF),parameter::p_RoundOff = 1.D-8


    private::CopyMDStatisticFromOther
    private::Clean_MDStatistic
    private::CleanMDStatistic

    contains

    subroutine CopyArray_D(this,others)
        implicit none
        !---Dummy Vars---
        real(kind=KINDDF),dimension(:),allocatable,intent(out)::this
        real(kind=KINDDF),dimension(:),allocatable,intent(in)::others
        !---Body--
        if(allocated(this)) deallocate(this)

        if(allocated(others)) then
            if(size(others) .GT. 0) then
                allocate(this(size(others)))
                this = others
            end if
        end if

        return
    end subroutine CopyArray_D


    subroutine CopyArray_I(this,others)
        implicit none
        !---Dummy Vars---
        integer,dimension(:),allocatable,intent(out)::this
        integer,dimension(:),allocatable,intent(in)::others
        !---Body--
        if(allocated(this)) deallocate(this)

        if(allocated(others)) then
            if(size(others) .GT. 0) then
                allocate(this(size(others)))
                this = others
            end if
        end if

        return
    end subroutine CopyArray_I

    !******************For type MDStatistic*******************************
    subroutine CopyMDStatisticFromOther(this,others)
        implicit none
        !---Dummy Vars---
        CLASS(MDStatistic),intent(out)::this
        CLASS(MDStatistic),intent(in)::others
        !---Body---
        this%MaxSIANumEachCluster = others%MaxSIANumEachCluster
        this%MaxVACNumEachCluster = others%MaxVACNumEachCluster
        this%MaxMIXNumEachCluster = others%MaxMIXNumEachCluster
        this%MinSIANumEachCluster = others%MinSIANumEachCluster
        this%MinVACNumEachCluster = others%MinVACNumEachCluster
        this%MinMIXNumEachCluster = others%MinMIXNumEachCluster

        this%MaxDistanceSIA_ToCent = others%MaxDistanceSIA_ToCent
        this%MaxDistanceVAC_ToCent = others%MaxDistanceVAC_ToCent
        this%MaxDistanceMIX_ToCent = others%MaxDistanceMIX_ToCent
        this%MaxDistanceSIA_BetweenCluster = others%MaxDistanceSIA_BetweenCluster
        this%MaxDistanceVAC_BetweenCluster = others%MaxDistanceVAC_BetweenCluster
        this%MaxDistanceMIX_BetweenCluster = others%MaxDistanceMIX_BetweenCluster
        this%MaxDistanceSIAToVAC_BetweenCluster = others%MaxDistanceSIAToVAC_BetweenCluster
        this%MinDistanceSIA_ToCent = others%MinDistanceSIA_ToCent
        this%MinDistanceVAC_ToCent = others%MinDistanceVAC_ToCent
        this%MinDistanceMIX_ToCent = others%MinDistanceMIX_ToCent
        this%MinDistanceSIA_BetweenCluster = others%MinDistanceSIA_BetweenCluster
        this%MinDistanceVAC_BetweenCluster = others%MinDistanceVAC_BetweenCluster
        this%MinDistanceMIX_BetweenCluster = others%MinDistanceMIX_BetweenCluster
        this%MinDistanceSIAToVAC_BetweenCluster = others%MinDistanceSIAToVAC_BetweenCluster

        this%BinWidthSIA_ToCenter = others%BinWidthSIA_ToCenter
        this%BinWidthVAC_ToCenter = others%BinWidthVAC_ToCenter
        this%BinWidthMIX_ToCenter = others%BinWidthMIX_ToCenter
        this%BinWidthSIA_BetweenCluster = others%BinWidthSIA_BetweenCluster
        this%BinWidthVAC_BetweenCluster = others%BinWidthVAC_BetweenCluster
        this%BinWidthMIX_BetweenCluster = others%BinWidthMIX_BetweenCluster
        this%BinWidthSIAToVAC_BetweenCluster = others%BinWidthSIAToVAC_BetweenCluster

        call CopyArray(this%ToCent_DistSIA,others%ToCent_DistSIA)

        call CopyArray(this%BinSIA_ToCenterArray,others%BinSIA_ToCenterArray)

        call CopyArray(this%ToCent_DistVAC,others%ToCent_DistVAC)

        call CopyArray(this%BinVAC_ToCenterArray,others%BinVAC_ToCenterArray)

        call CopyArray(this%ToCent_DistMIX,others%ToCent_DistMIX)

        call CopyArray(this%BinMIX_ToCenterArray,others%BinMIX_ToCenterArray)

        call CopyArray(this%ClusterGap_DistSIA,others%ClusterGap_DistSIA)

        call CopyArray(this%BinSIA_BetweenClusterArray,others%BinSIA_BetweenClusterArray)

        call CopyArray(this%ClusterGap_DistVAC,others%ClusterGap_DistVAC)

        call CopyArray(this%BinVAC_BetweenClusterArray,others%BinVAC_BetweenClusterArray)

        call CopyArray(this%ClusterGap_DistMIX,others%ClusterGap_DistMIX)

        call CopyArray(this%BinMIX_BetweenClusterArray,others%BinMIX_BetweenClusterArray)

        call CopyArray(this%BinMIX_BetweenClusterArray,others%BinMIX_BetweenClusterArray)

        call CopyArray(this%BinSIAToVAC_BetweenClusterArray,others%BinSIAToVAC_BetweenClusterArray)

        call CopyArray(this%NAtomSIA,others%NAtomSIA)

        call CopyArray(this%BinSIA_NAtomArray,others%BinSIA_NAtomArray)

        call CopyArray(this%NAtomVAC,others%NAtomVAC)

        call CopyArray(this%BinVAC_NAtomArray,others%BinVAC_NAtomArray)

        call CopyArray(this%NAtomMIX,others%NAtomMIX)

        call CopyArray(this%BinMIX_NAtomArray,others%BinMIX_NAtomArray)

        this%NFrankelParisEachBox_AVE = others%NFrankelParisEachBox_AVE

        this%TotalCountSIA_GapCToC = others%TotalCountSIA_GapCToC
        this%TotalCountVAC_GapCToC = others%TotalCountVAC_GapCToC
        this%TotalCount_GapSIAToVAC = others%TotalCount_GapSIAToVAC
        this%TotalCountMIX_GapCToC = others%TotalCountMIX_GapCToC
        this%TotalCountSIA_ToCent = others%TotalCountSIA_ToCent
        this%TotalCountVAC_ToCent = others%TotalCountVAC_ToCent
        this%TotalCountMIX_ToCent = others%TotalCountMIX_ToCent
        this%TotalCountSIA_NAtom = others%TotalCountSIA_NAtom
        this%TotalCountVAC_NAtom = others%TotalCountVAC_NAtom
        this%TotalCountMIX_NAtom = others%TotalCountMIX_NAtom

        return
    end subroutine CopyMDStatisticFromOther

    !******************************************************************
    subroutine Clean_MDStatistic(this)
        implicit none
        !---Dummy Vars---
        CLASS(MDStatistic)::this
        !---Body---
        this%MaxSIANumEachCluster = -1
        this%MaxVACNumEachCluster = -1
        this%MaxMIXNumEachCluster = -1
        this%MinSIANumEachCluster = 1.D32
        this%MinVACNumEachCluster = 1.D32
        this%MinMIXNumEachCluster = 1.D32

        this%MaxDistanceSIA_ToCent = -1.D0
        this%MaxDistanceVAC_ToCent = -1.D0
        this%MaxDistanceMIX_ToCent = -1.D0
        this%MaxDistanceSIA_BetweenCluster = -1.D0
        this%MaxDistanceVAC_BetweenCluster = -1.D0
        this%MaxDistanceMIX_BetweenCluster = -1.D0
        this%MaxDistanceSIAToVAC_BetweenCluster = -1.D0
        this%MinDistanceSIA_ToCent = 1.D32
        this%MinDistanceVAC_ToCent = 1.D32
        this%MinDistanceMIX_ToCent = 1.D32
        this%MinDistanceSIA_BetweenCluster = 1.D32
        this%MinDistanceVAC_BetweenCluster = 1.D32
        this%MinDistanceMIX_BetweenCluster = 1.D32
        this%MinDistanceSIAToVAC_BetweenCluster = 1.D32

        this%BinWidthSIA_ToCenter = 0.D0
        this%BinWidthVAC_ToCenter = 0.D0
        this%BinWidthMIX_ToCenter = 0.D0
        this%BinWidthSIA_BetweenCluster = 0.D0
        this%BinWidthVAC_BetweenCluster = 0.D0
        this%BinWidthMIX_BetweenCluster = 0.D0
        this%BinWidthSIAToVAC_BetweenCluster = 0.D0


        if(allocated(this%ToCent_DistSIA)) deallocate(this%ToCent_DistSIA)

        if(allocated(this%BinSIA_ToCenterArray)) deallocate(this%BinSIA_ToCenterArray)


        if(allocated(this%ToCent_DistVAC)) deallocate(this%ToCent_DistVAC)

        if(allocated(this%BinVAC_ToCenterArray)) deallocate(this%BinVAC_ToCenterArray)

        if(allocated(this%ToCent_DistMIX)) deallocate(this%ToCent_DistMIX)

        if(allocated(this%BinMIX_ToCenterArray)) deallocate(this%BinMIX_ToCenterArray)


        if(allocated(this%ClusterGap_DistSIA)) deallocate(this%ClusterGap_DistSIA)

        if(allocated(this%BinSIA_BetweenClusterArray)) deallocate(this%BinSIA_BetweenClusterArray)

        if(allocated(this%ClusterGap_DistVAC)) deallocate(this%ClusterGap_DistVAC)

        if(allocated(this%BinVAC_BetweenClusterArray)) deallocate(this%BinVAC_BetweenClusterArray)

        if(allocated(this%ClusterGap_DistMIX)) deallocate(this%ClusterGap_DistMIX)

        if(allocated(this%BinMIX_BetweenClusterArray)) deallocate(this%BinMIX_BetweenClusterArray)

        if(allocated(this%ClusterGap_SIAToVAC)) deallocate(this%ClusterGap_SIAToVAC)

        if(allocated(this%BinSIAToVAC_BetweenClusterArray)) deallocate(this%BinSIAToVAC_BetweenClusterArray)

        if(allocated(this%NAtomSIA)) deallocate(this%NAtomSIA)

        if(allocated(this%BinSIA_NAtomArray)) deallocate(this%BinSIA_NAtomArray)

        if(allocated(this%NAtomVAC)) deallocate(this%NAtomVAC)

        if(allocated(this%BinVAC_NAtomArray)) deallocate(this%BinVAC_NAtomArray)

        if(allocated(this%NAtomMIX)) deallocate(this%NAtomMIX)

        if(allocated(this%BinMIX_NAtomArray)) deallocate(this%BinMIX_NAtomArray)

        this%NFrankelParisEachBox_AVE = 0

        this%TotalCountSIA_GapCToC = 0
        this%TotalCountVAC_GapCToC = 0
        this%TotalCount_GapSIAToVAC = 0
        this%TotalCountMIX_GapCToC = 0
        this%TotalCountSIA_ToCent = 0
        this%TotalCountVAC_ToCent = 0
        this%TotalCountMIX_ToCent = 0
        this%TotalCountSIA_NAtom = 0
        this%TotalCountVAC_NAtom = 0
        this%TotalCountMIX_NAtom = 0

        return
    end subroutine Clean_MDStatistic


    !********************************************************************
    subroutine CleanMDStatistic(this)
        implicit none
        !---Dummy Vars---
        type(MDStatistic)::this
        !---Body---

        call this%Clean_MDStatistic()
        return
    end subroutine


  !***********************************************************************
  subroutine CascadeDataBase_ANALYSIS_SIAANDVAC(pathIn,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig,TheMDStatistic)
    !---This routine only analysis SIA or VAC
    implicit none
    !---Dummy Vars---
    character*(*),intent(in)::pathIn
    integer,intent(in)::Index_StartBox
    integer,intent(in)::Index_EndBox
    integer,intent(in)::Index_SIAConfig
    integer,intent(in)::Index_VACConfig
    type(MDStatistic)::TheMDStatistic
    !---Local Vars---
    character*1000::OutFolder
    character*1000::pathOutSIA
    integer::hFileOutSIA
    character*1000::pathOutVAC
    integer::hFileOutVAC
    character*1000::pathOutSIAToVAC
    integer::hFileOutSIAToVAC
    character*1000::pathOutMIX
    integer::hFileOutMIX
    integer::IBox
    character*1000::C_IBOX
    character*1000::C_ICFGSIA
    character*1000::C_ICFGVAC
    character*1000::fileName
    logical::exits
    integer::RemindZeroNum
    integer::I
    character*1000::STR
    integer::LINE
    character*32::KEYWORD
    integer::STA
    integer::N
    character*32::STRTMP(30)
    integer::ClusterID
    integer::NSIAClusterEachBox
    integer::NVACClusterEachBox
    type(ClusterAtom),dimension(:),allocatable::ClustersArraySIA
    integer,dimension(:),allocatable::NAtomEachClusterSIA
    type(ClusterAtom),dimension(:),allocatable::ClustersArrayVAC
    integer,dimension(:),allocatable::NAtomEachClusterVAC
    integer::AtomType
    real(kind=KINDDF)::CentPosSIA(3)
    real(kind=KINDDF)::CentPosVAC(3)
    real(kind=KINDDF)::CentPosMIX(3)
    real(kind=KINDDF)::Distance
    integer::IC
    integer::JC
    integer::NBOX
    integer::BinID
    !---Body---

    call TheMDStatistic%Clean_MDStatistic()

    OutFolder = CreateDataFolder(adjustl(trim(pathIn)//"/ANALYSIS/"))

    pathOutSIA = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"SIA_ToCenter.show"
    pathOutVAC = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"VAC_ToCenter.show"
    pathOutMIX = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"MIX_ToCenter.show"

    hFileOutSIA = CreateNewFile(pathOutSIA)
    hFileOutVAC = CreateNewFile(pathOutVAC)
    hFileOutMIX = CreateNewFile(pathOutMIX)

    write(hFileOutSIA,fmt="(10(A14,1x))") "IBox","ICFG","ICluster","Cent_X","Cent_Y","Cent_Z","Dist_From_Cent","AtomsNum"
    write(hFileOutVAC,fmt="(10(A14,1x))") "IBox","ICFG","ICluster","Cent_X","Cent_Y","Cent_Z","Dist_From_Cent","AtomsNum"
    write(hFileOutMIX,fmt="(10(A14,1x))") "IBox","ICFG","ICluster","Cent_X","Cent_Y","Cent_Z","Dist_From_Cent","AtomsNum"

    TheMDStatistic%MaxDistanceSIA_ToCent = -1.D0
    TheMDStatistic%MaxDistanceVAC_ToCent = -1.D0
    TheMDStatistic%MaxDistanceMIX_ToCent = -1.D0
    TheMDStatistic%MaxDistanceSIA_BetweenCluster = -1.D0
    TheMDStatistic%MaxDistanceVAC_BetweenCluster = -1.D0
    TheMDStatistic%MaxDistanceMIX_BetweenCluster = -1.D0
    TheMDStatistic%MaxDistanceSIAToVAC_BetweenCluster = -1.D0
    TheMDStatistic%MinDistanceSIA_ToCent = 1.D32
    TheMDStatistic%MinDistanceVAC_ToCent = 1.D32
    TheMDStatistic%MinDistanceMIX_ToCent = 1.D32
    TheMDStatistic%MinDistanceSIA_BetweenCluster = 1.D32
    TheMDStatistic%MinDistanceVAC_BetweenCluster = 1.D32
    TheMDStatistic%MinDistanceMIX_BetweenCluster = 1.D32
    TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster = 1.D32

    TheMDStatistic%MaxSIANumEachCluster = -1
    TheMDStatistic%MaxVACNumEachCluster = -1
    TheMDStatistic%MaxMIXNumEachCluster = -1
    TheMDStatistic%MinSIANumEachCluster = 1.D32
    TheMDStatistic%MinVACNumEachCluster = 1.D32
    TheMDStatistic%MinMIXNumEachCluster = 1.D32

    write(C_ICFGSIA,*) Index_SIAConfig
    C_ICFGSIA = adjustl(C_ICFGSIA)
    RemindZeroNum = MaxNumLen - LENTRIM(C_ICFGSIA)
    DO I = 1,RemindZeroNum
        C_ICFGSIA = "0"//C_ICFGSIA
    END DO

    write(C_ICFGVAC,*) Index_VACConfig
    C_ICFGVAC = adjustl(C_ICFGVAC)
    RemindZeroNum = MaxNumLen - LENTRIM(C_ICFGVAC)
    DO I = 1,RemindZeroNum
        C_ICFGVAC = "0"//C_ICFGVAC
    END DO

    DO IBox = Index_StartBox,Index_EndBox
        write(C_IBOX,*) IBox
        C_IBOX = adjustl(C_IBOX)
        RemindZeroNum = MaxNumLen - LENTRIM(C_IBOX)

        DO I = 1,RemindZeroNum
            C_IBOX = "0"//C_IBOX
        END DO

        CentPosMIX = 0.D0

        !---SIA---
        fileName = adjustl(trim(pathIn))//"/SIA/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGSIA))
        call ReadOnConifg_SIA(fileName,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

        TheMDStatistic%MaxSIANumEachCluster = max(TheMDStatistic%MaxSIANumEachCluster,maxval(NAtomEachClusterSIA))
        TheMDStatistic%MinSIANumEachCluster = min(TheMDStatistic%MinSIANumEachCluster,minval(NAtomEachClusterSIA))

        CentPosSIA = 0.D0
        DO IC = 1,NSIAClusterEachBox
            CentPosSIA = CentPosSIA + ClustersArraySIA(IC)%POS
            CentPosMIX = CentPosMIX + ClustersArraySIA(IC)%POS
        END DO

        if(NSIAClusterEachBox .GT. 0) then
            CentPosSIA = CentPosSIA/NSIAClusterEachBox
        end if

        if(NSIAClusterEachBox .ne. size(ClustersArraySIA)) then
            write(*,*) "Opps...."
            write(*,*) "NSIAClusterEachBox",NSIAClusterEachBox
            write(*,*) "size(ClustersArraySIA)",size(ClustersArraySIA)
            pause
            stop
        end if

        DO IC = 1,NSIAClusterEachBox
            Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - CentPosSIA)**2))
            write(hFileOutSIA,fmt="(3(I14,1x),4(1PE16.8,1x),I14)") IBox,Index_SIAConfig,IC,ClustersArraySIA(IC)%POS,Distance,NAtomEachClusterSIA(IC)

            if(TheMDStatistic%MaxDistanceSIA_ToCent .LE. Distance) then
                TheMDStatistic%MaxDistanceSIA_ToCent = Distance
            end if

            if(TheMDStatistic%MinDistanceSIA_ToCent .GE. Distance) then
                TheMDStatistic%MinDistanceSIA_ToCent = Distance
            end if

            DO JC = IC+1,NSIAClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArraySIA(JC)%POS)**2))

                if(TheMDStatistic%MaxDistanceSIA_BetweenCluster .LE. Distance) then
                    TheMDStatistic%MaxDistanceSIA_BetweenCluster = Distance
                end if

                if(TheMDStatistic%MinDistanceSIA_BetweenCluster .GE. Distance) then
                    TheMDStatistic%MinDistanceSIA_BetweenCluster = Distance
                end if

            END DO
        END DO

        flush(hFileOutSIA)

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg_VAC(fileName,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

        TheMDStatistic%MaxVACNumEachCluster = max(TheMDStatistic%MaxVACNumEachCluster,maxval(NAtomEachClusterVAC))
        TheMDStatistic%MinVACNumEachCluster = min(TheMDStatistic%MinVACNumEachCluster,minval(NAtomEachClusterVAC))

        CentPosVAC = 0.D0
        DO IC = 1,NVACClusterEachBox
            CentPosVAC = CentPosVAC + ClustersArrayVAC(IC)%POS
            CentPosMIX = CentPosMIX + ClustersArrayVAC(IC)%POS
        END DO

        if(NVACClusterEachBox .GT. 0) then
            CentPosVAC = CentPosVAC/NVACClusterEachBox
        end if

        if(NVACClusterEachBox .ne. size(ClustersArrayVAC)) then
            write(*,*) "Opps...."
            write(*,*) "NVACClusterEachBox",NVACClusterEachBox
            write(*,*) "size(ClustersArrayVAC)",size(ClustersArrayVAC)
            pause
            stop
        end if

        DO IC = 1,NVACClusterEachBox
            Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - CentPosVAC)**2))
            write(hFileOutVAC,fmt="(3(I14,1x),4(1PE16.8,1x),I14)") IBox,Index_VACConfig,IC,ClustersArrayVAC(IC)%POS,Distance,NAtomEachClusterVAC(IC)

            if(TheMDStatistic%MaxDistanceVAC_ToCent .LE. Distance) then
                TheMDStatistic%MaxDistanceVAC_ToCent = Distance
            end if

            if(TheMDStatistic%MinDistanceVAC_ToCent .GE. Distance) then
                TheMDStatistic%MinDistanceVAC_ToCent = Distance
            end if

            DO JC = IC+1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - ClustersArrayVAC(JC)%POS)**2))

                if(TheMDStatistic%MaxDistanceVAC_BetweenCluster .LE. Distance) then
                    TheMDStatistic%MaxDistanceVAC_BetweenCluster = Distance
                end if

                if(TheMDStatistic%MinDistanceVAC_BetweenCluster .GE. Distance) then
                    TheMDStatistic%MinDistanceVAC_BetweenCluster = Distance
                end if
            END DO

        END DO

        flush(hFileOutVAC)

        !---MIX---
        if((NSIAClusterEachBox + NVACClusterEachBox) .GT. 0) then
            CentPosMIX = CentPosMIX/(NSIAClusterEachBox + NVACClusterEachBox)
        end if

        TheMDStatistic%MaxDistanceMIX_BetweenCluster = max(TheMDStatistic%MaxDistanceMIX_BetweenCluster,  &
                                                           max(TheMDStatistic%MaxDistanceSIA_BetweenCluster,TheMDStatistic%MaxDistanceVAC_BetweenCluster))
        TheMDStatistic%MinDistanceMIX_BetweenCluster = min(TheMDStatistic%MinDistanceSIA_BetweenCluster, &
                                                           min(TheMDStatistic%MinDistanceSIA_BetweenCluster,TheMDStatistic%MinDistanceVAC_BetweenCluster))

        DO IC = 1,NSIAClusterEachBox
            Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - CentPosMIX)**2))
            write(hFileOutMIX,fmt="(3(I14,1x),4(1PE16.8,1x),I14)") IBox,Index_SIAConfig,IC,ClustersArraySIA(IC)%POS,Distance,NAtomEachClusterSIA(IC)

            if(TheMDStatistic%MaxDistanceMIX_ToCent .LE. Distance) then
                TheMDStatistic%MaxDistanceMIX_ToCent = Distance
            end if

            if(TheMDStatistic%MinDistanceMIX_ToCent .GE. Distance) then
                TheMDStatistic%MinDistanceMIX_ToCent = Distance
            end if

            DO JC = 1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArrayVAC(JC)%POS)**2))

                if(TheMDStatistic%MaxDistanceMIX_BetweenCluster .LE. Distance) then
                    TheMDStatistic%MaxDistanceMIX_BetweenCluster = Distance
                end if

                if(TheMDStatistic%MinDistanceMIX_BetweenCluster .GE. Distance) then
                    TheMDStatistic%MinDistanceMIX_BetweenCluster = Distance
                end if

                if(TheMDStatistic%MaxDistanceSIAToVAC_BetweenCluster .LE. Distance) then
                    TheMDStatistic%MaxDistanceSIAToVAC_BetweenCluster = Distance
                end if

                if(TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster .GE. Distance) then
                    TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster = Distance
                end if

            END DO

        END DO

        DO IC = 1,NVACClusterEachBox
            Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - CentPosMIX)**2))
            write(hFileOutMIX,fmt="(3(I14,1x),4(1PE16.8,1x),I14)") IBox,Index_VACConfig,IC,ClustersArrayVAC(IC)%POS,Distance,NAtomEachClusterVAC(IC)

            if(TheMDStatistic%MaxDistanceMIX_ToCent .LE. Distance) then
                TheMDStatistic%MaxDistanceMIX_ToCent = Distance
            end if

            if(TheMDStatistic%MinDistanceMIX_ToCent .GE. Distance) then
                TheMDStatistic%MinDistanceMIX_ToCent = Distance
            end if
        END DO

        flush(hFileOutMIX)

    END DO

    TheMDStatistic%MaxMIXNumEachCluster = max(TheMDStatistic%MaxSIANumEachCluster,TheMDStatistic%MaxVACNumEachCluster)
    TheMDStatistic%MinMIXNumEachCluster = min(TheMDStatistic%MinSIANumEachCluster,TheMDStatistic%MinVACNumEachCluster)

    close(hFileOutSIA)
    close(hFileOutVAC)
    close(hFileOutMIX)

    !-------------------Gap between clusters distribution------------------------
    TheMDStatistic%BinWidthSIA_ToCenter = (TheMDStatistic%MaxDistanceSIA_ToCent - TheMDStatistic%MinDistanceSIA_ToCent)/BinNum
    TheMDStatistic%BinWidthVAC_ToCenter = (TheMDStatistic%MaxDistanceVAC_ToCent - TheMDStatistic%MinDistanceVAC_ToCent)/BinNum
    TheMDStatistic%BinWidthMIX_ToCenter = (TheMDStatistic%MaxDistanceMIX_ToCent - TheMDStatistic%MinDistanceMIX_ToCent)/BinNum

    TheMDStatistic%BinWidthSIA_BetweenCluster = (TheMDStatistic%MaxDistanceSIA_BetweenCluster - TheMDStatistic%MinDistanceSIA_BetweenCluster)/BinNum
    TheMDStatistic%BinWidthVAC_BetweenCluster = (TheMDStatistic%MaxDistanceVAC_BetweenCluster - TheMDStatistic%MinDistanceVAC_BetweenCluster)/BinNum
    TheMDStatistic%BinWidthMIX_BetweenCluster = (TheMDStatistic%MaxDistanceMIX_BetweenCluster - TheMDStatistic%MinDistanceMIX_BetweenCluster)/BinNum
    TheMDStatistic%BinWidthSIAToVAC_BetweenCluster = (TheMDStatistic%MaxDistanceSIAToVAC_BetweenCluster - TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster)/BinNum

    pathOutSIA = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"SIA_Dist.analysis"
    pathOutVAC = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"VAC_Dist.analysis"
    pathOutSIAToVAC = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"Dist.SIAToVAc"
    pathOutMIX = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"MIX_Dist.analysis"

    hFileOutSIA = CreateNewFile(pathOutSIA)
    hFileOutVAC = CreateNewFile(pathOutVAC)
    hFileOutSIAToVAC = CreateNewFile(pathOutSIAToVAC)
    hFileOutMIX = CreateNewFile(pathOutMIX)

    if(allocated(TheMDStatistic%ToCent_DistSIA)) deallocate(TheMDStatistic%ToCent_DistSIA)
    allocate(TheMDStatistic%ToCent_DistSIA(BinNum))
    TheMDStatistic%ToCent_DistSIA = 0.D0
    if(allocated(TheMDStatistic%BinSIA_ToCenterArray)) deallocate(TheMDStatistic%BinSIA_ToCenterArray)
    allocate(TheMDStatistic%BinSIA_ToCenterArray(BinNum))
    TheMDStatistic%BinSIA_ToCenterArray = 0.D0

    if(allocated(TheMDStatistic%ToCent_DistVAC)) deallocate(TheMDStatistic%ToCent_DistVAC)
    allocate(TheMDStatistic%ToCent_DistVAC(BinNum))
    TheMDStatistic%ToCent_DistVAC = 0.D0
    if(allocated(TheMDStatistic%BinVAC_ToCenterArray)) deallocate(TheMDStatistic%BinVAC_ToCenterArray)
    allocate(TheMDStatistic%BinVAC_ToCenterArray(BinNum))
    TheMDStatistic%BinVAC_ToCenterArray = 0.D0

    if(allocated(TheMDStatistic%ToCent_DistMIX)) deallocate(TheMDStatistic%ToCent_DistMIX)
    allocate(TheMDStatistic%ToCent_DistMIX(BinNum))
    TheMDStatistic%ToCent_DistMIX = 0.D0
    if(allocated(TheMDStatistic%BinMIX_ToCenterArray)) deallocate(TheMDStatistic%BinMIX_ToCenterArray)
    allocate(TheMDStatistic%BinMIX_ToCenterArray(BinNum))
    TheMDStatistic%BinMIX_ToCenterArray = 0.D0

    if(allocated(TheMDStatistic%ClusterGap_DistSIA)) deallocate(TheMDStatistic%ClusterGap_DistSIA)
    allocate(TheMDStatistic%ClusterGap_DistSIA(BinNum))
    TheMDStatistic%ClusterGap_DistSIA = 0.D0
    if(allocated(TheMDStatistic%BinSIA_BetweenClusterArray)) deallocate(TheMDStatistic%BinSIA_BetweenClusterArray)
    allocate(TheMDStatistic%BinSIA_BetweenClusterArray(BinNum))
    TheMDStatistic%BinSIA_BetweenClusterArray = 0.D0

    if(allocated(TheMDStatistic%ClusterGap_DistVAC)) deallocate(TheMDStatistic%ClusterGap_DistVAC)
    allocate(TheMDStatistic%ClusterGap_DistVAC(BinNum))
    TheMDStatistic%ClusterGap_DistVAC = 0.D0
    if(allocated(TheMDStatistic%BinVAC_BetweenClusterArray)) deallocate(TheMDStatistic%BinVAC_BetweenClusterArray)
    allocate(TheMDStatistic%BinVAC_BetweenClusterArray(BinNum))
    TheMDStatistic%BinVAC_BetweenClusterArray = 0.D0

    if(allocated(TheMDStatistic%ClusterGap_DistMIX)) deallocate(TheMDStatistic%ClusterGap_DistMIX)
    allocate(TheMDStatistic%ClusterGap_DistMIX(BinNum))
    TheMDStatistic%ClusterGap_DistMIX = 0.D0
    if(allocated(TheMDStatistic%BinMIX_BetweenClusterArray)) deallocate(TheMDStatistic%BinMIX_BetweenClusterArray)
    allocate(TheMDStatistic%BinMIX_BetweenClusterArray(BinNum))
    TheMDStatistic%BinMIX_BetweenClusterArray = 0.D0

    if(allocated(TheMDStatistic%ClusterGap_SIAToVAC)) deallocate(TheMDStatistic%ClusterGap_SIAToVAC)
    allocate(TheMDStatistic%ClusterGap_SIAToVAC(BinNum))
    TheMDStatistic%ClusterGap_SIAToVAC = 0.D0
    if(allocated(TheMDStatistic%BinSIAToVAC_BetweenClusterArray)) deallocate(TheMDStatistic%BinSIAToVAC_BetweenClusterArray)
    allocate(TheMDStatistic%BinSIAToVAC_BetweenClusterArray(BinNum))
    TheMDStatistic%BinSIAToVAC_BetweenClusterArray = 0.D0


    if(allocated(TheMDStatistic%NAtomSIA)) deallocate(TheMDStatistic%NAtomSIA)
    allocate(TheMDStatistic%NAtomSIA(TheMDStatistic%MaxSIANumEachCluster - TheMDStatistic%MinSIANumEachCluster + 1))
    TheMDStatistic%NAtomSIA = 0
    if(allocated(TheMDStatistic%BinSIA_NAtomArray)) deallocate(TheMDStatistic%BinSIA_NAtomArray)
    allocate(TheMDStatistic%BinSIA_NAtomArray(TheMDStatistic%MaxSIANumEachCluster - TheMDStatistic%MinSIANumEachCluster + 1))
    TheMDStatistic%BinSIA_NAtomArray = 0

    if(allocated(TheMDStatistic%NAtomVAC)) deallocate(TheMDStatistic%NAtomVAC)
    allocate(TheMDStatistic%NAtomVAC(TheMDStatistic%MaxVACNumEachCluster - TheMDStatistic%MinVACNumEachCluster + 1))
    TheMDStatistic%NAtomVAC = 0
    if(allocated(TheMDStatistic%BinVAC_NAtomArray)) deallocate(TheMDStatistic%BinVAC_NAtomArray)
    allocate(TheMDStatistic%BinVAC_NAtomArray(TheMDStatistic%MaxVACNumEachCluster - TheMDStatistic%MinVACNumEachCluster + 1))
    TheMDStatistic%BinVAC_NAtomArray = 0

    if(allocated(TheMDStatistic%NAtomMIX)) deallocate(TheMDStatistic%NAtomMIX)
    allocate(TheMDStatistic%NAtomMIX(TheMDStatistic%MaxMIXNumEachCluster - TheMDStatistic%MinMIXNumEachCluster + 1))
    TheMDStatistic%NAtomMIX = 0
    if(allocated(TheMDStatistic%BinMIX_NAtomArray)) deallocate(TheMDStatistic%BinMIX_NAtomArray)
    allocate(TheMDStatistic%BinMIX_NAtomArray(TheMDStatistic%MaxMIXNumEachCluster - TheMDStatistic%MinMIXNumEachCluster + 1))
    TheMDStatistic%BinMIX_NAtomArray = 0

    TheMDStatistic%NFrankelParisEachBox_AVE = 0
    NBOX = 0
    DO IBox = Index_StartBox,Index_EndBox
        write(C_IBOX,*) IBox
        C_IBOX = adjustl(C_IBOX)
        RemindZeroNum = MaxNumLen - LENTRIM(C_IBOX)

        DO I = 1,RemindZeroNum
            C_IBOX = "0"//C_IBOX
        END DO

        CentPosMIX = 0.D0

        !---SIA---
        fileName = adjustl(trim(pathIn))//"/SIA/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGSIA))
        call ReadOnConifg_SIA(fileName,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

        DO IC = 1,NSIAClusterEachBox
            if(NAtomEachClusterSIA(IC) .LE. 0) then
                write(*,*) "It is impossible that atoms size less than 0",IC
                write(*,*) NAtomEachClusterSIA(IC)
                pause
                stop
            end if

            BinID = NAtomEachClusterSIA(IC) - TheMDStatistic%MinSIANumEachCluster + 1
            if(BinID .GT. size(TheMDStatistic%NAtomSIA)) then
                write(*,*) "Opps, the bin index exceed the array."
                write(*,*) BinID
                write(*,*) size(TheMDStatistic%NAtomSIA)
            end if
            TheMDStatistic%NAtomSIA(BinID) = TheMDStatistic%NAtomSIA(BinID) + 1
        END DO

        CentPosSIA = 0.D0
        DO IC = 1,NSIAClusterEachBox
            CentPosSIA = CentPosSIA + ClustersArraySIA(IC)%POS
            CentPosMIX = CentPosMIX + ClustersArraySIA(IC)%POS
        END DO

        if(NSIAClusterEachBox .GT. 0) then
            CentPosSIA = CentPosSIA/NSIAClusterEachBox
        end if

        DO IC = 1,NSIAClusterEachBox
            Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - CentPosSIA)**2))

            BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceSIA_ToCent-p_RoundOff)/TheMDStatistic%BinWidthSIA_ToCenter) + 1
            TheMDStatistic%ToCent_DistSIA(BinID) = TheMDStatistic%ToCent_DistSIA(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) (Distance-TheMDStatistic%MinDistanceSIA_ToCent)/TheMDStatistic%BinWidthSIA_ToCenter
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        DO IC = 1,NSIAClusterEachBox
            DO JC = IC+1,NSIAClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArraySIA(JC)%POS)**2))
                BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceSIA_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthSIA_BetweenCluster) + 1
                TheMDStatistic%ClusterGap_DistSIA(BinID) = TheMDStatistic%ClusterGap_DistSIA(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) (Distance-TheMDStatistic%MinDistanceSIA_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthSIA_BetweenCluster
                    write(*,*) BinID
                    write(*,*) BinNum
                end if

                BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceMIX_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthMIX_BetweenCluster) + 1
                TheMDStatistic%ClusterGap_DistMIX(BinID) = TheMDStatistic%ClusterGap_DistMIX(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if
            END DO
        END DO

        TheMDStatistic%NFrankelParisEachBox_AVE = TheMDStatistic%NFrankelParisEachBox_AVE + sum(NAtomEachClusterSIA)

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg_VAC(fileName,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

        DO IC = 1,NVACClusterEachBox
            if(NAtomEachClusterVAC(IC) .LE. 0) then
                write(*,*) "It is impossible that atoms size less than 0",IC
                write(*,*) NAtomEachClusterVAC(IC)
                pause
                stop
            end if

            BinID = NAtomEachClusterVAC(IC) - TheMDStatistic%MinVACNumEachCluster + 1
            if(BinID .GT. size(TheMDStatistic%NAtomVAC)) then
                write(*,*) "Opps, the bin index exceed the array."
                write(*,*) BinID
                write(*,*) size(TheMDStatistic%NAtomVAC)
            end if
            TheMDStatistic%NAtomVAC(BinID) = TheMDStatistic%NAtomVAC(BinID) + 1
        END DO

        CentPosVAC = 0.D0
        DO IC = 1,NVACClusterEachBox
            CentPosVAC = CentPosVAC + ClustersArrayVAC(IC)%POS
            CentPosMIX = CentPosMIX + ClustersArrayVAC(IC)%POS
        END DO

        if(NVACClusterEachBox .GT. 0) then
            CentPosVAC = CentPosVAC/NVACClusterEachBox
        end if

        DO IC = 1,NVACClusterEachBox
            Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - CentPosVAC)**2))
            BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceVAC_ToCent-p_RoundOff)/TheMDStatistic%BinWidthVAC_ToCenter) + 1
            TheMDStatistic%ToCent_DistVAC(BinID) = TheMDStatistic%ToCent_DistVAC(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        DO IC = 1,NVACClusterEachBox
            DO JC = IC+1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - ClustersArrayVAC(JC)%POS)**2))
                BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceVAC_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthVAC_BetweenCluster) + 1
                TheMDStatistic%ClusterGap_DistVAC(BinID) = TheMDStatistic%ClusterGap_DistVAC(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if

                BinID = floor(DABS(Distance- TheMDStatistic%MinDistanceMIX_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthMIX_BetweenCluster) + 1
                TheMDStatistic%ClusterGap_DistMIX(BinID) = TheMDStatistic%ClusterGap_DistMIX(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if
            END DO
        END DO

        if(sum(NAtomEachClusterSIA) .ne. sum(NAtomEachClusterVAC)) then
            write(*,*) "MCPSCUERROR: It is impossible that SIA number not equal with VAC number"
            write(*,*) sum(NAtomEachClusterSIA),sum(NAtomEachClusterVAC)
            write(*,*) "For box: ",C_IBOX
            pause
            stop
        end if

        !---Gap Between SIA and VAC---
        DO IC = 1,NSIAClusterEachBox
            DO JC = 1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArrayVAC(JC)%POS)**2))
                BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthSIAToVAC_BetweenCluster) + 1
                TheMDStatistic%ClusterGap_SIAToVAC(BinID) = TheMDStatistic%ClusterGap_SIAToVAC(BinID) + 1
                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if

                BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceMIX_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthMIX_BetweenCluster) + 1
                TheMDStatistic%ClusterGap_DistMIX(BinID) = TheMDStatistic%ClusterGap_DistMIX(BinID) + 1
                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) (Distance-TheMDStatistic%MinDistanceMIX_BetweenCluster-p_RoundOff)/TheMDStatistic%BinWidthMIX_BetweenCluster
                    write(*,*) "Distance",Distance
                    write(*,*) "MaxDistanceMIX_BetweenCluster",TheMDStatistic%MaxDistanceMIX_BetweenCluster
                    write(*,*) BinID
                    write(*,*) BinNum
                end if
            END DO
        END DO

        !---MIX---
        DO IC = 1,NSIAClusterEachBox
            if(NAtomEachClusterSIA(IC) .LE. 0) then
                write(*,*) "It is impossible that atoms size less than 0",IC
                write(*,*) NAtomEachClusterSIA(IC)
                pause
                stop
            end if

            BinID = NAtomEachClusterSIA(IC) - TheMDStatistic%MinMIXNumEachCluster + 1
            if(BinID .GT. size(TheMDStatistic%NAtomMIX)) then
                write(*,*) "Opps, the bin index exceed the array."
                write(*,*) BinID
                write(*,*) size(TheMDStatistic%NAtomMIX)
            end if
            TheMDStatistic%NAtomMIX(BinID) = TheMDStatistic%NAtomMIX(BinID) + 1
        END DO

        DO IC = 1,NVACClusterEachBox
            if(NAtomEachClusterVAC(IC) .LE. 0) then
                write(*,*) "It is impossible that atoms size less than 0",IC
                write(*,*) NAtomEachClusterVAC(IC)
                pause
                stop
            end if

            BinID = NAtomEachClusterVAC(IC) - TheMDStatistic%MinMIXNumEachCluster + 1
            if(BinID .GT. size(TheMDStatistic%NAtomMIX)) then
                write(*,*) "Opps, the bin index exceed the array."
                write(*,*) BinID
                write(*,*) size(TheMDStatistic%NAtomMIX)
            end if
            TheMDStatistic%NAtomMIX(BinID) = TheMDStatistic%NAtomMIX(BinID) + 1
        END DO



        if((NSIAClusterEachBox + NVACClusterEachBox) .GT. 0) then
            CentPosMIX = CentPosMIX/(NSIAClusterEachBox + NVACClusterEachBox)
        end if

        DO IC = 1,NSIAClusterEachBox
            Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - CentPosMIX)**2))
            BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceMIX_ToCent-p_RoundOff)/TheMDStatistic%BinWidthMIX_ToCenter) + 1
            TheMDStatistic%ToCent_DistMIX(BinID) = TheMDStatistic%ToCent_DistMIX(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        DO IC = 1,NVACClusterEachBox
            Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - CentPosMIX)**2))
            BinID = floor(DABS(Distance-TheMDStatistic%MinDistanceMIX_ToCent-p_RoundOff)/TheMDStatistic%BinWidthMIX_ToCenter) + 1
            TheMDStatistic%ToCent_DistMIX(BinID) = TheMDStatistic%ToCent_DistMIX(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        NBOX = NBOX + 1
    END DO


    write(hFileOutSIA,fmt="(A,1x,1PE16.8)") "&MINDISTANCE_ToCent_SIA",TheMDStatistic%MinDistanceSIA_ToCent
    write(hFileOutVAC,fmt="(A,1x,1PE16.8)") "&MINDISTANCE_ToCent_VAC",TheMDStatistic%MinDistanceVAC_ToCent
    write(hFileOutMIX,fmt="(A,1x,1PE16.8)") "&MINDISTANCE_ToCent_MIX",TheMDStatistic%MinDistanceMIX_ToCent
    write(hFileOutSIA,fmt="(A,1x,1PE16.8)") "&MAXDISTANCE_ToCent_SIA",TheMDStatistic%MaxDistanceSIA_ToCent
    write(hFileOutVAC,fmt="(A,1x,1PE16.8)") "&MAXDISTANCE_ToCent_VAC",TheMDStatistic%MaxDistanceVAC_ToCent
    write(hFileOutMIX,fmt="(A,1x,1PE16.8)") "&MAXDISTANCE_ToCent_MIX",TheMDStatistic%MaxDistanceMIX_ToCent

    write(hFileOutSIA,fmt="(A,1x,1PE16.8)") "&MinDistanceSIA_BetweenCluster",TheMDStatistic%MinDistanceSIA_BetweenCluster
    write(hFileOutVAC,fmt="(A,1x,1PE16.8)") "&MinDistanceVAC_BetweenCluster",TheMDStatistic%MinDistanceVAC_BetweenCluster
    write(hFileOutMIX,fmt="(A,1x,1PE16.8)") "&MinDistanceMIX_BetweenCluster",TheMDStatistic%MinDistanceMIX_BetweenCluster
    write(hFileOutSIAToVAC,fmt="(A,1x,1PE16.8)") "&MinDistanceSIAToVAC_BetweenCluster",TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster
    write(hFileOutSIA,fmt="(A,1x,1PE16.8)") "&MaxDistanceSIA_BetweenCluster",TheMDStatistic%MaxDistanceSIA_BetweenCluster
    write(hFileOutVAC,fmt="(A,1x,1PE16.8)") "&MaxDistanceVAC_BetweenCluster",TheMDStatistic%MaxDistanceVAC_BetweenCluster
    write(hFileOutMIX,fmt="(A,1x,1PE16.8)") "&MaxDistanceMIX_BetweenCluster",TheMDStatistic%MaxDistanceMIX_BetweenCluster
    write(hFileOutSIAToVAC,fmt="(A,1x,1PE16.8)") "&MaxDistanceSIAToVAC_BetweenCluster",TheMDStatistic%MaxDistanceSIAToVAC_BetweenCluster


    write(hFileOutSIA,fmt="(A)") "&DISTANCE"
    write(hFileOutVAC,fmt="(A)") "&DISTANCE"
    write(hFileOutSIAToVAC,fmt="(A)") "&DISTANCE"
    write(hFileOutMIX,fmt="(A)") "&DISTANCE"

    write(hFileOutSIA,fmt="(10(A14,1x))") "!ToCent","Count","GapCToC","Count"
    write(hFileOutVAC,fmt="(10(A14,1x))") "!ToCent","Count","GapCToC","Count"
    write(hFileOutSIAToVAC,fmt="(10(A14,1x))") "!GapSIAToVac","Count"
    write(hFileOutMIX,fmt="(10(A14,1x))") "!ToCent","Count","GapCToC","Count"

    TheMDStatistic%TotalCountSIA_ToCent = sum(TheMDStatistic%ToCent_DistSIA)
    TheMDStatistic%TotalCountSIA_GapCToC = sum(TheMDStatistic%ClusterGap_DistSIA)
    TheMDStatistic%ToCent_DistSIA = TheMDStatistic%ToCent_DistSIA/TheMDStatistic%TotalCountSIA_ToCent
    TheMDStatistic%ClusterGap_DistSIA = TheMDStatistic%ClusterGap_DistSIA/TheMDStatistic%TotalCountSIA_GapCToC
    DO I = 1,BinNum
        TheMDStatistic%BinSIA_ToCenterArray(I) = TheMDStatistic%BinWidthSIA_ToCenter*(I - 0.5D0) + TheMDStatistic%MinDistanceSIA_ToCent
        TheMDStatistic%BinSIA_BetweenClusterArray(I) = TheMDStatistic%BinWidthSIA_BetweenCluster*(I - 0.5D0) + TheMDStatistic%MinDistanceSIA_BetweenCluster

        write(hFileOutSIA,fmt="(4(1PE16.8,1x))") TheMDStatistic%BinSIA_ToCenterArray(I), &
                                                TheMDStatistic%ToCent_DistSIA(I),&
                                                TheMDStatistic%BinSIA_BetweenClusterArray(I),&
                                                TheMDStatistic%ClusterGap_DistSIA(I)
    END DO

    TheMDStatistic%TotalCountVAC_ToCent = sum(TheMDStatistic%ToCent_DistVAC)
    TheMDStatistic%TotalCountVAC_GapCToC = sum(TheMDStatistic%ClusterGap_DistVAC)
    TheMDStatistic%ToCent_DistVAC = TheMDStatistic%ToCent_DistVAC/TheMDStatistic%TotalCountVAC_ToCent
    TheMDStatistic%ClusterGap_DistVAC = TheMDStatistic%ClusterGap_DistVAC/TheMDStatistic%TotalCountVAC_GapCToC
    DO I = 1,BinNum
        TheMDStatistic%BinVAC_ToCenterArray(I) = TheMDStatistic%BinWidthVAC_ToCenter*(I - 0.5D0) + TheMDStatistic%MinDistanceVAC_ToCent
        TheMDStatistic%BinVAC_BetweenClusterArray(I) = TheMDStatistic%BinWidthVAC_BetweenCluster*(I - 0.5D0) + TheMDStatistic%MinDistanceVAC_BetweenCluster

        write(hFileOutVAC,fmt="(4(1PE16.8,1x))") TheMDStatistic%BinVAC_ToCenterArray(I), &
                                                TheMDStatistic%ToCent_DistVAC(I),&
                                                TheMDStatistic%BinVAC_BetweenClusterArray(I),&
                                                TheMDStatistic%ClusterGap_DistVAC(I)
    END DO

    TheMDStatistic%TotalCount_GapSIAToVAC = sum(TheMDStatistic%ClusterGap_SIAToVAC)
    TheMDStatistic%ClusterGap_SIAToVAC = TheMDStatistic%ClusterGap_SIAToVAC/TheMDStatistic%TotalCount_GapSIAToVAC
    DO I = 1,BinNum
        TheMDStatistic%BinSIAToVAC_BetweenClusterArray(I) = TheMDStatistic%BinWidthSIAToVAC_BetweenCluster*(I - 0.5D0) + TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster

        write(hFileOutSIAToVAC,fmt="(2(1PE16.8,1x))") TheMDStatistic%BinSIAToVAC_BetweenClusterArray(I), &
                                                     TheMDStatistic%ClusterGap_SIAToVAC(I)
    END DO

    TheMDStatistic%TotalCountMIX_ToCent = sum(TheMDStatistic%ToCent_DistMIX)
    TheMDStatistic%TotalCountMIX_GapCToC = sum(TheMDStatistic%ClusterGap_DistMIX)
    TheMDStatistic%ToCent_DistMIX = TheMDStatistic%ToCent_DistMIX/TheMDStatistic%TotalCountMIX_ToCent
    TheMDStatistic%ClusterGap_DistMIX = TheMDStatistic%ClusterGap_DistMIX/TheMDStatistic%TotalCountMIX_GapCToC
    DO I = 1,BinNum
        TheMDStatistic%BinMIX_ToCenterArray(I) = TheMDStatistic%BinWidthMIX_ToCenter*(I - 0.5D0) + TheMDStatistic%MinDistanceMIX_ToCent
        TheMDStatistic%BinMIX_BetweenClusterArray(I) = TheMDStatistic%BinWidthMIX_BetweenCluster*(I - 0.5D0) + TheMDStatistic%MinDistanceMIX_BetweenCluster

        write(hFileOutMIX,fmt="(4(1PE16.8,1x))") TheMDStatistic%BinMIX_ToCenterArray(I), &
                                                TheMDStatistic%ToCent_DistMIX(I),&
                                                TheMDStatistic%BinMIX_BetweenClusterArray(I), &
                                                TheMDStatistic%ClusterGap_DistMIX(I)
    END DO

    close(hFileOutSIA)
    close(hFileOutVAC)
    close(hFileOutSIATOVAC)
    close(hFileOutMIX)


    pathOutSIA = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"SIACluster_NAtoms.analysis"
    pathOutVAC = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"VACCluster_NAtoms.analysis"
    pathOutMIX = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"MIX_NAtoms.analysis"

    hFileOutSIA = CreateNewFile(pathOutSIA)
    hFileOutVAC = CreateNewFile(pathOutVAC)
    hFileOutMIX = CreateNewFile(pathOutMIX)

    TheMDStatistic%NFrankelParisEachBox_AVE = ceiling(dble(TheMDStatistic%NFrankelParisEachBox_AVE)/dble(NBOX))
    write(hFileOutSIA,fmt="(A,1x,I10)") "&NFRANKELPAIRS",TheMDStatistic%NFrankelParisEachBox_AVE
    write(hFileOutVAC,fmt="(A,1x,I10)") "&NFRANKELPAIRS",TheMDStatistic%NFrankelParisEachBox_AVE
    write(hFileOutMIX,fmt="(A,1x,I10)") "&NFRANKELPAIRS",TheMDStatistic%NFrankelParisEachBox_AVE


    write(hFileOutSIA,fmt="(A,1x,I10)") "&MinSIANumEachCluster",TheMDStatistic%MinSIANumEachCluster
    write(hFileOutVAC,fmt="(A,1x,I10)") "&MinVACNumEachCluster",TheMDStatistic%MinVACNumEachCluster
    write(hFileOutMIX,fmt="(A,1x,I10)") "&MinMIXNumEachCluster",TheMDStatistic%MinMIXNumEachCluster
    write(hFileOutSIA,fmt="(A,1x,I10)") "&MaxSIANumEachCluster",TheMDStatistic%MaxSIANumEachCluster
    write(hFileOutVAC,fmt="(A,1x,I10)") "&MaxVACNumEachCluster",TheMDStatistic%MaxVACNumEachCluster
    write(hFileOutMIX,fmt="(A,1x,I10)") "&MaxMIXNumEachCluster",TheMDStatistic%MaxMIXNumEachCluster


    write(hFileOutSIA,fmt="(A)") "&NATOMS"
    write(hFileOutVAC,fmt="(A)") "&NATOMS"
    write(hFileOutMIX,fmt="(A)") "&NATOMS"

    write(hFileOutSIA,fmt="(10(A14,1x))") "!NA","Count"
    write(hFileOutVAC,fmt="(10(A14,1x))") "!NA","Count"
    write(hFileOutMIX,fmt="(10(A14,1x))") "!NA","Count"

    TheMDStatistic%TotalCountSIA_NAtom = sum(TheMDStatistic%NAtomSIA)
    TheMDStatistic%NAtomSIA = TheMDStatistic%NAtomSIA/TheMDStatistic%TotalCountSIA_NAtom
    DO IC = TheMDStatistic%MinSIANumEachCluster,TheMDStatistic%MaxSIANumEachCluster
        TheMDStatistic%BinSIA_NAtomArray(IC-TheMDStatistic%MinSIANumEachCluster+1) = IC
        write(hFileOutSIA,fmt="(I14,1x,1PE16.8,1x)") TheMDStatistic%BinSIA_NAtomArray(IC-TheMDStatistic%MinSIANumEachCluster+1),TheMDStatistic%NAtomSIA(IC-TheMDStatistic%MinSIANumEachCluster+1)
    END DO

    TheMDStatistic%TotalCountVAC_NAtom = sum(TheMDStatistic%NAtomVAC)
    TheMDStatistic%NAtomVAC = TheMDStatistic%NAtomVAC/TheMDStatistic%TotalCountVAC_NAtom
    DO IC = TheMDStatistic%MinVACNumEachCluster,TheMDStatistic%MaxVACNumEachCluster
        TheMDStatistic%BinVAC_NAtomArray(IC-TheMDStatistic%MinVACNumEachCluster+1) = IC
        write(hFileOutVAC,fmt="(I14,1x,1PE16.8,1x)") TheMDStatistic%BinVAC_NAtomArray(IC-TheMDStatistic%MinVACNumEachCluster+1),TheMDStatistic%NAtomVAC(IC-TheMDStatistic%MinVACNumEachCluster+1)
    END DO

    TheMDStatistic%TotalCountMIX_NAtom = sum(TheMDStatistic%NAtomMIX)
    TheMDStatistic%NAtomMIX = TheMDStatistic%NAtomMIX/TheMDStatistic%TotalCountMIX_NAtom
    DO IC = TheMDStatistic%MinMIXNumEachCluster,TheMDStatistic%MaxMIXNumEachCluster
        TheMDStatistic%BinMIX_NAtomArray(IC-TheMDStatistic%MinMIXNumEachCluster+1) = IC
        write(hFileOutMIX,fmt="(I14,1x,1PE16.8,1x)") TheMDStatistic%BinMIX_NAtomArray(IC-TheMDStatistic%MinMIXNumEachCluster+1),TheMDStatistic%NAtomMIX(IC-TheMDStatistic%MinMIXNumEachCluster+1)
    END DO


    close(hFileOutSIA)
    close(hFileOutVAC)
    close(hFileOutSIATOVAC)
    close(hFileOutMIX)


    if(allocated(ClustersArraySIA)) deallocate(ClustersArraySIA)
    if(allocated(ClustersArrayVAC)) deallocate(ClustersArrayVAC)

    if(allocated(NAtomEachClusterSIA)) deallocate(NAtomEachClusterSIA)
    if(allocated(NAtomEachClusterVAC)) deallocate(NAtomEachClusterVAC)


    return
    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
    write(*,*) "At line: ",LINE
    write(*,*) "STR",STR
    pause
    stop
  end subroutine CascadeDataBase_ANALYSIS_SIAANDVAC

  !***********************************************************************
  subroutine CascadeDataBase_DirectlyRead(pathIn,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig,ClusterArray,NAtomEachCluster,NSIAClusterEachBoxArray,NVACClusterEachBoxArray)
    !---This routine only analysis SIA or VAC
    implicit none
    !---Dummy Vars---
    character*(*),intent(in)::pathIn
    integer,intent(in)::Index_StartBox
    integer,intent(in)::Index_EndBox
    integer,intent(in)::Index_SIAConfig
    integer,intent(in)::Index_VACConfig
    type(ClusterAtom),intent(inout),dimension(:),allocatable::ClusterArray
    integer,intent(inout),dimension(:),allocatable::NAtomEachCluster
    integer,intent(inout),dimension(:),allocatable::NSIAClusterEachBoxArray
    integer,intent(inout),dimension(:),allocatable::NVACClusterEachBoxArray
    !---Local Vars---
    character*1000::OutFolder
    character*1000::pathOutSIA
    integer::hFileOutSIA
    character*1000::pathOutVAC
    integer::hFileOutVAC
    character*1000::pathOutSIAToVAC
    integer::hFileOutSIAToVAC
    character*1000::pathOutMIX
    integer::hFileOutMIX
    integer::IBox
    character*1000::C_IBOX
    character*1000::C_ICFGSIA
    character*1000::C_ICFGVAC
    character*1000::fileName
    logical::exits
    integer::RemindZeroNum
    integer::I
    character*1000::STR
    integer::LINE
    character*32::KEYWORD
    integer::STA
    integer::N
    character*32::STRTMP(30)
    integer::ClusterID
    integer::NSIAClusterEachBox
    integer::NVACClusterEachBox
    type(ClusterAtom),dimension(:),allocatable::ClustersArraySIA
    integer,dimension(:),allocatable::NAtomEachClusterSIA
    type(ClusterAtom),dimension(:),allocatable::ClustersArrayVAC
    integer,dimension(:),allocatable::NAtomEachClusterVAC
    integer::AtomType
    real(kind=KINDDF)::CentPosSIA(3)
    real(kind=KINDDF)::CentPosVAC(3)
    real(kind=KINDDF)::CentPosMIX(3)
    real(kind=KINDDF)::Distance
    integer::IC
    integer::JC
    integer::BinID
    integer::CurrentIBox
    integer::ICBefore
    !---Body---

    write(C_ICFGSIA,*) Index_SIAConfig
    C_ICFGSIA = adjustl(C_ICFGSIA)
    RemindZeroNum = MaxNumLen - LENTRIM(C_ICFGSIA)
    DO I = 1,RemindZeroNum
        C_ICFGSIA = "0"//C_ICFGSIA
    END DO

    write(C_ICFGVAC,*) Index_VACConfig
    C_ICFGVAC = adjustl(C_ICFGVAC)
    RemindZeroNum = MaxNumLen - LENTRIM(C_ICFGVAC)
    DO I = 1,RemindZeroNum
        C_ICFGVAC = "0"//C_ICFGVAC
    END DO

    if(Index_EndBox .GE. Index_StartBox) then
        if(allocated(NSIAClusterEachBoxArray)) deallocate(NSIAClusterEachBoxArray)
        allocate(NSIAClusterEachBoxArray(Index_EndBox - Index_StartBox + 1))
        NSIAClusterEachBoxArray = 0

        if(allocated(NVACClusterEachBoxArray)) deallocate(NVACClusterEachBoxArray)
        allocate(NVACClusterEachBoxArray(Index_EndBox - Index_StartBox + 1))
        NVACClusterEachBoxArray = 0
    else
        write(*,*) "MCPSCUERROR: The end box index should not less than start box index"
        write(*,*) "Start: " ,Index_StartBox
        write(*,*) "End: ",Index_EndBox
        pause
        stop
    end if

    DO IBox = Index_StartBox,Index_EndBox
        write(C_IBOX,*) IBox
        C_IBOX = adjustl(C_IBOX)
        RemindZeroNum = MaxNumLen - LENTRIM(C_IBOX)

        DO I = 1,RemindZeroNum
            C_IBOX = "0"//C_IBOX
        END DO

        CentPosMIX = 0.D0


        CurrentIBox = IBox - Index_StartBox + 1

        !---SIA---
        fileName = adjustl(trim(pathIn))//"/SIA/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGSIA))
        call ReadOnConifg_SIA(fileName,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

        NSIAClusterEachBoxArray(CurrentIBox) = NSIAClusterEachBox

        CentPosSIA = 0.D0
        DO IC = 1,NSIAClusterEachBox
            CentPosSIA = CentPosSIA + ClustersArraySIA(IC)%POS
            CentPosMIX = CentPosMIX + ClustersArraySIA(IC)%POS
        END DO

        if(NSIAClusterEachBox .GT. 0) then
            CentPosSIA = CentPosSIA/NSIAClusterEachBox
        end if

        if(NSIAClusterEachBox .ne. size(ClustersArraySIA)) then
            write(*,*) "Opps...."
            write(*,*) "NSIAClusterEachBox",NSIAClusterEachBox
            write(*,*) "size(ClustersArraySIA)",size(ClustersArraySIA)
            pause
            stop
        end if

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg_VAC(fileName,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

        NVACClusterEachBoxArray(CurrentIBox) = NVACClusterEachBox

        CentPosVAC = 0.D0
        DO IC = 1,NVACClusterEachBox
            CentPosVAC = CentPosVAC + ClustersArrayVAC(IC)%POS
            CentPosMIX = CentPosMIX + ClustersArrayVAC(IC)%POS
        END DO

        if(NVACClusterEachBox .GT. 0) then
            CentPosVAC = CentPosVAC/NVACClusterEachBox
        end if

        if(NVACClusterEachBox .ne. size(ClustersArrayVAC)) then
            write(*,*) "Opps...."
            write(*,*) "NVACClusterEachBox",NVACClusterEachBox
            write(*,*) "size(ClustersArrayVAC)",size(ClustersArrayVAC)
            pause
            stop
        end if

        !---MIX---
        if((NSIAClusterEachBox + NVACClusterEachBox) .GT. 0) then
            CentPosMIX = CentPosMIX/(NSIAClusterEachBox + NVACClusterEachBox)
        end if

        if(sum(NAtomEachClusterSIA) .ne. sum(NAtomEachClusterVAC)) then
            write(*,*) "MCPSCUERROR: It is impossible that SIA number not equal with VAC number"
            write(*,*) sum(NAtomEachClusterSIA),sum(NAtomEachClusterVAC)
            write(*,*) "For box: ",C_IBOX
            pause
            stop
        end if

    END DO

    if((sum(NSIAClusterEachBoxArray) + sum(NVACClusterEachBoxArray)) .GT. 0) then
        if(allocated(ClusterArray)) deallocate(ClusterArray)
        allocate(ClusterArray(sum(NSIAClusterEachBoxArray) + sum(NVACClusterEachBoxArray)))

        if(allocated(NAtomEachCluster)) deallocate(NAtomEachCluster)
        allocate(NAtomEachCluster(sum(NSIAClusterEachBoxArray) + sum(NVACClusterEachBoxArray)))

        NAtomEachCluster = 0
    else
        write(*,*) "MCPSCUERROR: That is impossible not cluster in the database"
        pause
        stop
    end if

    DO IBox = Index_StartBox,Index_EndBox
        write(C_IBOX,*) IBox
        C_IBOX = adjustl(C_IBOX)
        RemindZeroNum = MaxNumLen - LENTRIM(C_IBOX)

        DO I = 1,RemindZeroNum
            C_IBOX = "0"//C_IBOX
        END DO

        CurrentIBox = IBox - Index_StartBox + 1

        if(CurrentIBox .GT. 1) then
            ICBefore = sum(NSIAClusterEachBoxArray(1:CurrentIBox - 1)) + sum(NVACClusterEachBoxArray(1:CurrentIBox - 1))
        else
            ICBefore = 0
        end if

        !---SIA---
        fileName = adjustl(trim(pathIn))//"/SIA/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGSIA))
        call ReadOnConifg_SIA(fileName,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

        if(NSIAClusterEachBox .ne. size(ClustersArraySIA)) then
            write(*,*) "Opps...."
            write(*,*) "NSIAClusterEachBox",NSIAClusterEachBox
            write(*,*) "size(ClustersArraySIA)",size(ClustersArraySIA)
            pause
            stop
        end if

        if(NSIAClusterEachBox .LE. 0) then
            write(*,*) "MCPSSCUERROR: The SIA number less than 1 in box: ",C_IBOX
            pause
            stop
        end if

        ClusterArray(ICBefore + 1:ICBefore + NSIAClusterEachBox) = ClustersArraySIA
        NAtomEachCluster(ICBefore + 1:ICBefore + NSIAClusterEachBox) = NAtomEachClusterSIA

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg_VAC(fileName,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

        if(NVACClusterEachBox .ne. size(ClustersArrayVAC)) then
            write(*,*) "Opps...."
            write(*,*) "NVACClusterEachBox",NVACClusterEachBox
            write(*,*) "size(ClustersArrayVAC)",size(ClustersArrayVAC)
            pause
            stop
        end if

        if(NVACClusterEachBox .LE. 0) then
            write(*,*) "MCPSSCUERROR: The VAC number less than 1 in box: ",C_IBOX
            pause
            stop
        end if

        ClusterArray(ICBefore + NSIAClusterEachBox + 1:ICBefore + NSIAClusterEachBox + NVACClusterEachBox) = ClustersArrayVAC
        NAtomEachCluster(ICBefore + NSIAClusterEachBox + 1:ICBefore + NSIAClusterEachBox + NVACClusterEachBox) = NAtomEachClusterVAC

    END DO

    if(allocated(ClustersArraySIA)) deallocate(ClustersArraySIA)
    if(allocated(NAtomEachClusterSIA)) deallocate(NAtomEachClusterSIA)
    if(allocated(ClustersArrayVAC)) deallocate(ClustersArrayVAC)
    if(allocated(NAtomEachClusterVAC)) deallocate(NAtomEachClusterVAC)

    return
    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
    write(*,*) "At line: ",LINE
    write(*,*) "STR",STR
    pause
    stop
  end subroutine CascadeDataBase_DirectlyRead

  !*************************************************************************
  subroutine ReadOnConifg_SIA(fileName,ClustersArray,NAtomEachCluster,NClusterEachBox)
    !---Dummy Vars---
    character*(*),intent(in)::fileName
    type(ClusterAtom),dimension(:),allocatable::ClustersArray
    integer,dimension(:),allocatable::NAtomEachCluster
    integer::NClusterEachBox
    !---Local Vars---
    integer::hFile
    character*1000::STR
    integer::LINE
    character*32::KEYWORD
    integer::STA
    integer::N
    character*32::STRTMP(30)
    integer::LastRecordClusterID
    integer::ClusterID
    integer::AtomType
    real(kind=KINDDF)::Distance
    integer::IC
    real(kind=KINDDF)::LatticeLength
    integer,dimension(:),allocatable::NAtomsRef
    !---Body---
    hFile = OpenExistedFile(fileName)
    LINE = 0

    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&LATT")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                    LatticeLength = DRSTR(STRTMP(1))
            case("&SITETYP-SIA")
            exit
        end select
    END DO

    NClusterEachBox = 0
    LastRecordClusterID = 0

    DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!") )
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        if(LENTRIM(STR) .LE. 0) then
            cycle
        end if

        call EXTRACT_NUMB(STR,11,N,STRTMP)

        AtomType = ISTR(STRTMP(1))

        if(AtomType .eq. RefSiteIndex_MD) then

            ClusterID = ISTR(STRTMP(11))

            if(LastRecordClusterID .LT. ClusterID) then
                NClusterEachBox = NClusterEachBox + 1
                LastRecordClusterID = ClusterID
            end if
        end if

    END DO

    if(NClusterEachBox .LE. 0) then
        write(*,*) "Warning: the file: ",fileName," do not have any one of cluster."
        return
    end if

    if(allocated(ClustersArray)) deallocate(ClustersArray)
    allocate(ClustersArray(NClusterEachBox))
    DO IC = 1,NClusterEachBox
        ClustersArray(IC)%POS = 0.D0
    END DO

    if(allocated(NAtomEachCluster)) deallocate(NAtomEachCluster)
    allocate(NAtomEachCluster(NClusterEachBox))
    NAtomEachCluster = 0

    if(allocated(NAtomsRef)) deallocate(NAtomsRef)
    allocate(NAtomsRef(NClusterEachBox))
    NAtomsRef = 0

    rewind(hFile)

    LINE = 0
    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&SITETYP-SIA")
            exit
        end select
    END DO

    NClusterEachBox = 0
    LastRecordClusterID = 0
    DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!") )
        LINE = LINE + 1
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        if(LENTRIM(STR) .LE. 0) then
            cycle
        end if

        call EXTRACT_NUMB(STR,11,N,STRTMP)

        AtomType = ISTR(STRTMP(1))

        ClusterID = ISTR(STRTMP(11))

        if(AtomType .eq. RefSiteIndex_MD) then

            if(LastRecordClusterID .LT. ClusterID) then

                NClusterEachBox = NClusterEachBox + 1
                LastRecordClusterID = ClusterID

                if(NClusterEachBox .GT. 1) then
                    if(NAtomsRef(NClusterEachBox -1) .LE. 0) then
                        write(*,*) "MCPSCUERROR: The Reference cluster number cannot less than 1"
                        write(*,*) "For file: ",fileName
                        write(*,*) "Cluster ID: ",NClusterEachBox -1
                        pause
                        stop
                    end if

                    ClustersArray(NClusterEachBox -1)%POS = ClustersArray(NClusterEachBox -1)%POS/NAtomsRef(NClusterEachBox -1)
                end if

                NAtomsRef(NClusterEachBox) = 1

                ClustersArray(NClusterEachBox)%POS(1) = DRSTR(STRTMP(2))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(2) = DRSTR(STRTMP(3))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(3) = DRSTR(STRTMP(4))*LatticeLength*C_AM2CM
            else
                NAtomsRef(NClusterEachBox) = NAtomsRef(NClusterEachBox) + 1
                ClustersArray(NClusterEachBox)%POS(1) = ClustersArray(NClusterEachBox)%POS(1) + DRSTR(STRTMP(2))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(2) = ClustersArray(NClusterEachBox)%POS(2) + DRSTR(STRTMP(3))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(3) = ClustersArray(NClusterEachBox)%POS(3) + DRSTR(STRTMP(4))*LatticeLength*C_AM2CM
            end if

        end if

        if(AtomType .eq. SIAIndex_MD .or. AtomType .eq. SIAIndex2_MD .or. AtomType .eq. SIAIndex3_MD) then
            if(NClusterEachBox .LE. 0) then
                write(*,*) "You must speicial the reference site first..."
                write(*,*) "For file:",fileName
                write(*,*) "At line: ",LINE
                write(*,*) STR
                pause
                stop
            end if

            NAtomEachCluster(NClusterEachBox) = NAtomEachCluster(NClusterEachBox) + 1
        end if

    END DO

    !---Last one---
    if(NClusterEachBox .GT. 0) then
        if(NAtomsRef(NClusterEachBox) .LE. 0) then
            write(*,*) "MCPSCUERROR: The Reference cluster number cannot less than 1"
            write(*,*) "For file: ",fileName
            write(*,*) "Cluster ID: ",NClusterEachBox
            pause
            stop
        end if

        ClustersArray(NClusterEachBox)%POS = ClustersArray(NClusterEachBox)%POS/NAtomsRef(NClusterEachBox)
    end if

    NAtomEachCluster = NAtomEachCluster - NAtomsRef

    close(unit=hFile,IOSTAT=STA)


    if(STA .GT. 0) then
        write(*,*) "MCPSCUERROR: Close file:",fileName," Failed!"
        pause
        stop
    end if

    call DeAllocateArray_Host(NAtomsRef,"NAtomsRef")

    return

    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
    write(*,*) "At line: ",LINE
    write(*,*) "STR",STR
    write(*,*) "fileName",fileName
    pause
    stop
    return
  end subroutine ReadOnConifg_SIA


  !*************************************************************************
  subroutine ReadOnConifg_VAC(fileName,ClustersArray,NAtomEachCluster,NClusterEachBox)
    !---Dummy Vars---
    character*(*),intent(in)::fileName
    type(ClusterAtom),dimension(:),allocatable::ClustersArray
    integer,dimension(:),allocatable::NAtomEachCluster
    integer::NClusterEachBox
    !---Local Vars---
    integer::hFile
    character*1000::STR
    integer::LINE
    character*32::KEYWORD
    integer::STA
    integer::N
    character*32::STRTMP(30)
    integer::LastRecordClusterID
    integer::ClusterID
    integer::AtomType
    real(kind=KINDDF)::Distance
    integer::IC
    real(kind=KINDDF)::LatticeLength
    !---Body---
    hFile = OpenExistedFile(fileName)
    LINE = 0

    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&LATT")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                    LatticeLength = DRSTR(STRTMP(1))
            case("&SITETYP-SIA")
            exit
        end select
    END DO

    NClusterEachBox = 0
    LastRecordClusterID = 0

    DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!") )
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        if(LENTRIM(STR) .LE. 0) then
            cycle
        end if

        call EXTRACT_NUMB(STR,11,N,STRTMP)

        AtomType = ISTR(STRTMP(1))

        if(AtomType .eq. VACIndex_MD) then

            ClusterID = ISTR(STRTMP(11))

            if(LastRecordClusterID .LT. ClusterID) then
                NClusterEachBox = NClusterEachBox + 1
                LastRecordClusterID = ClusterID
            end if
        end if

    END DO

    if(NClusterEachBox .LE. 0) then
        write(*,*) "Warning: the file: ",fileName," do not have any one of cluster."
        return
    end if

    if(allocated(ClustersArray)) deallocate(ClustersArray)
    allocate(ClustersArray(NClusterEachBox))
    DO IC = 1,NClusterEachBox
        ClustersArray(IC)%POS = 0.D0
    END DO

    if(allocated(NAtomEachCluster)) deallocate(NAtomEachCluster)
    allocate(NAtomEachCluster(NClusterEachBox))
    NAtomEachCluster = 0

    rewind(hFile)

    LINE = 0
    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&SITETYP-SIA")
            exit
        end select
    END DO

    NClusterEachBox = 0
    LastRecordClusterID = 0
    DO While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!") )
        LINE = LINE + 1
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        if(LENTRIM(STR) .LE. 0) then
            cycle
        end if

        call EXTRACT_NUMB(STR,11,N,STRTMP)

        AtomType = ISTR(STRTMP(1))

        if(AtomType .eq. VACIndex_MD) then

            ClusterID = ISTR(STRTMP(11))

            if(LastRecordClusterID .LT. ClusterID) then

                NClusterEachBox = NClusterEachBox + 1
                LastRecordClusterID = ClusterID

                if(NClusterEachBox .GT. 1) then
                    ClustersArray(NClusterEachBox -1)%POS = ClustersArray(NClusterEachBox -1)%POS/NAtomEachCluster(NClusterEachBox - 1)
                end if

                ClustersArray(NClusterEachBox)%POS(1) = DRSTR(STRTMP(2))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(2) = DRSTR(STRTMP(3))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(3) = DRSTR(STRTMP(4))*LatticeLength*C_AM2CM
            else
                ClustersArray(NClusterEachBox)%POS(1) = ClustersArray(NClusterEachBox)%POS(1) + DRSTR(STRTMP(2))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(2) = ClustersArray(NClusterEachBox)%POS(2) + DRSTR(STRTMP(3))*LatticeLength*C_AM2CM
                ClustersArray(NClusterEachBox)%POS(3) = ClustersArray(NClusterEachBox)%POS(3) + DRSTR(STRTMP(4))*LatticeLength*C_AM2CM
            end if

            NAtomEachCluster(NClusterEachBox) = NAtomEachCluster(NClusterEachBox) + 1

        end if

    END DO

    !---Last one---
    if(NClusterEachBox .GT. 0) then
        ClustersArray(NClusterEachBox)%POS = ClustersArray(NClusterEachBox)%POS/NAtomEachCluster(NClusterEachBox)
    end if

    close(unit=hFile,IOSTAT=STA)


    if(STA .GT. 0) then
        write(*,*) "MCPSCUERROR: Close file:",fileName," Failed!"
        pause
        stop
    end if

    return

    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
    write(*,*) "At line: ",LINE
    write(*,*) "STR",STR
    write(*,*) "fileName",fileName
    pause
    stop
    return
  end subroutine ReadOnConifg_VAC



  subroutine ResloveCascadeControlFile_FormMDDataBase(hFile,WhetherIncludeSIA,WhetherIncludeVAC,CascadeNum,CascadePosModel,CascadePos,WhetherCascadeSameInOneBox, &
                                                      MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig)
        !---Dummy Vars---
        integer,intent(in)::hFile
        logical,intent(out)::WhetherIncludeSIA
        logical,intent(out)::WhetherIncludeVAC
        integer,intent(out)::CascadeNum
        integer,intent(out)::CascadePosModel
        real(kind=KINDDF),dimension(:,:),allocatable::CascadePos
        logical,intent(out)::WhetherCascadeSameInOneBox
        character*(*),intent(out)::MDDataBasePath
        integer,intent(out)::Index_StartBox
        integer,intent(out)::Index_EndBox
        integer,intent(out)::Index_SIAConfig
        integer,intent(out)::Index_VACConfig
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*30::KEYWORD
        character*200::STRTMP(10)
        character*20,dimension(:),allocatable::STRTMPCascadePos
        integer::N
        integer::ICase
        logical::Finded

        !---Body---
        LINE = 0

        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&CASCADENUMBER")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the cascade number in one box."
                        pause
                        stop
                    end if
                    CascadeNum = ISTR(STRTMP(1))

                    if(CascadeNum .LE. 0) then
                        write(*,*) "MCPSCUERROR: The cascade number in one box cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the cascade number in one box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&CASCADECENTERPOSMODEL")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the cascade center position model."
                        pause
                        stop
                    end if
                    CascadePosModel = ISTR(STRTMP(1))

                    if(CascadePosModel .eq. CascadePosModel_ByVolumeAverage) then
                        write(*,*) "The cascade center position mode is by volume average "
                    else if(CascadePosModel .eq. CascadePosModel_ByUserSpeicaled) then
                        write(*,*) "The cascade center position mode is by user special position"
                    else if(CascadePosModel .eq. CascadePosModel_ByRandom) then
                            write(*,*) "The cascade center position mode is by random position"
                    else
                        write(*,*) "MCPSCUERROR: Unknown cascade center position model"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the cascade center position model."
           pause
           stop
        end if


        if(CascadePosModel .eq. CascadePosModel_ByUserSpeicaled) then

            Finded = .false.
            rewind(hFile)
            Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
                LINE = LINE + 1
                STR = adjustl(STR)
                call RemoveComments(STR,"!")

                if(LENTRIM(STR) .LE. 0) then
                    cycle
                end if

                call GETKEYWORD("&",STR,KEYWORD)

                call UPCASE(KEYWORD)

                select case(KEYWORD(1:LENTRIM(KEYWORD)))
                    case("&CASCADECENTERPOS")

                        allocate(STRTMPCascadePos(3*CascadeNum))
                        STRTMPCascadePos = ""

                        allocate(CascadePos(CascadeNum,3))

                        call EXTRACT_NUMB(STR,3*CascadeNum,N,STRTMPCascadePos)
                        if(N .LT. 3*CascadeNum) then
                            write(*,*) "MCPSCUERROR: You must special the number cascade center position each with cascade number."
                            pause
                            stop
                        end if


                        DO ICase = 1,CascadeNum
                            CascadePos(ICase,1) = DRSTR(STRTMPCascadePos((ICase - 1)*3 + 1))
                            CascadePos(ICase,2) = DRSTR(STRTMPCascadePos((ICase - 1)*3 + 2))
                            CascadePos(ICase,3) = DRSTR(STRTMPCascadePos((ICase - 1)*3 + 3))
                        END DO

                        Finded = .true.

                        if(allocated(STRTMPCascadePos)) deallocate(STRTMPCascadePos)
                end select
            END DO

            if(Finded .eq. .false.) then
                write(*,*) "MCPSCUERROR: You must special the cascade center position."
                pause
                stop
            end if
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&CASCADESAME")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special whether cascades are same in the box."
                        pause
                        stop
                    end if
                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        WhetherCascadeSameInOneBox = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        WhetherCascadeSameInOneBox = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether cascades are same in the box."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special whether cascades are same in the box."
           pause
           stop
        end if



        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INCLUDESIACLUSTER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special whether include SIA in box."
                        pause
                        stop
                    end if
                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        WhetherIncludeSIA = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        WhetherIncludeSIA = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether include SIA in box."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special whether include SIA in box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INCLUDEVACCLUSTER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special whether include VAC in box."
                        pause
                        stop
                    end if
                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        WhetherIncludeVAC = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        WhetherIncludeVAC = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether include VAC in box."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special whether include VAC in box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&MDDATABASEPATH")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special the MD DataBase path."
                        pause
                        stop
                    end if
                    MDDataBasePath = adjustl(trim(STRTMP(1)))
                    write(*,*) MDDataBasePath

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the MD DataBase path."
           pause
           stop
        end if

        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INDEX_STARTBOX")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special start box index."
                        pause
                        stop
                    end if
                    Index_StartBox = ISTR(STRTMP(1))
                    write(*,*) Index_StartBox

                    Finded = .true.
            end select
         END DO

         if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special start box index."
           pause
           stop
         end if

         Finded = .false.
         rewind(hFile)
         Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INDEX_ENDBOX")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special end box index."
                        pause
                        stop
                    end if
                    Index_EndBox = ISTR(STRTMP(1))
                    write(*,*) Index_EndBox

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special end box index."
           pause
           stop
        end if

        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INDEX_SIACONFIG")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special SIA configuration index."
                        pause
                        stop
                    end if
                    Index_SIAConfig = ISTR(STRTMP(1))
                    write(*,*) Index_SIAConfig
                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special SIA configuration index."
           pause
           stop
        end if

        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INDEX_VACCONFIG")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special VAC configuration index."
                        pause
                        stop
                    end if
                    Index_VACConfig = ISTR(STRTMP(1))
                    write(*,*) Index_VACConfig

                    Finded = .true.
            end select

        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special VAC configuration index."
           pause
           stop
        end if

        return
    end subroutine ResloveCascadeControlFile_FormMDDataBase


    !**************************************************************
    subroutine GenerateClustersSize(TheMDStatistic,NSIACluster,NAtomEachSIACluster,NVACCluster,NAtomEachVACCluster)
        implicit none
        !---Dummy Vars---
        type(MDStatistic)::TheMDStatistic
        integer::NSIACluster
        integer,dimension(:),allocatable::NAtomEachSIACluster
        integer::NVACCluster
        integer,dimension(:),allocatable::NAtomEachVACCluster
        !---Local Vars---
        integer::NSIARemind
        integer::NVACRemind
        logical::exitTotal
        integer::I
        integer::NSIAAccum
        integer::NVACAccum
        integer::EndBin
        integer::IBin
        real(kind=KINDDF)::Accum
        real(kind=KINDDF)::RandNum
        integer::TheBin
        logical::exitflag
        !---Body---

        if(allocated(NAtomEachSIACluster)) deallocate(NAtomEachSIACluster)
        allocate(NAtomEachSIACluster(TheMDStatistic%NFrankelParisEachBox_AVE))
        NAtomEachSIACluster = 0

        if(allocated(NAtomEachVACCluster)) deallocate(NAtomEachVACCluster)
        allocate(NAtomEachVACCluster(TheMDStatistic%NFrankelParisEachBox_AVE))
        NAtomEachVACCluster = 0


        DO While(.true.)
          NSIACluster = 0
          NAtomEachSIACluster = 0

          NSIARemind = TheMDStatistic%NFrankelParisEachBox_AVE
          exitTotal = .false.

          DO I = 1,TheMDStatistic%NFrankelParisEachBox_AVE

            if(NSIARemind .eq. 0) then
                exitTotal = .true.
                exit
            else if(NSIARemind .GE. TheMDStatistic%MinSIANumEachCluster) then

                NSIAAccum = 0
                DO IBin = 1,size(TheMDStatistic%BinSIA_NAtomArray)
                    NSIAAccum = NSIAAccum + TheMDStatistic%BinSIA_NAtomArray(IBin)

                    if(NSIAAccum .GE. NSIARemind) then
                        EndBin = IBin
                        exit
                    end if
                END DO

                exitflag = .false.
                DO While(.true.)
                    Accum = 0.D0
                    RandNum = DRAND32()
                    DO IBin = 1,EndBin
                        Accum = Accum + TheMDStatistic%NAtomSIA(IBin)

                        if(Accum .GE. RandNum) then
                            TheBin = IBin
                            exitflag = .true.

                            NSIARemind = NSIARemind -TheMDStatistic%BinSIA_NAtomArray(TheBin)

                            NSIACluster = NSIACluster + 1
                            NAtomEachSIACluster(NSIACluster) = TheMDStatistic%BinSIA_NAtomArray(TheBin)
                            exit
                        end if
                    END DO

                    if(exitflag .eq. .true.) then
                        exit
                    end if
                END DO
            else if(NSIARemind .LT. TheMDStatistic%MinSIANumEachCluster) then
                exitTotal = .false.
                exit
            end if

          END DO

          if(exitTotal .eq. .true.) then
            exit
          end if
        END DO


        DO While(.true.)
          NVACCluster = 0
          NAtomEachVACCluster = 0

          NVACRemind = TheMDStatistic%NFrankelParisEachBox_AVE
          exitTotal = .false.

          DO I = 1,TheMDStatistic%NFrankelParisEachBox_AVE

            if(NVACRemind .eq. 0) then
                exitTotal = .true.
                exit
            else if(NVACRemind .GE. TheMDStatistic%MinVACNumEachCluster) then

                NVACAccum = 0
                DO IBin = 1,size(TheMDStatistic%BinVAC_NAtomArray)
                    NVACAccum = NVACAccum + TheMDStatistic%BinVAC_NAtomArray(IBin)

                    if(NVACAccum .GE. NVACRemind) then
                        EndBin = IBin
                        exit
                    end if
                END DO

                exitflag = .false.
                DO While(.true.)
                    Accum = 0.D0
                    RandNum = DRAND32()
                    DO IBin = 1,EndBin
                        Accum = Accum + TheMDStatistic%NAtomVAC(IBin)

                        if(Accum .GE. RandNum) then
                            TheBin = IBin
                            exitflag = .true.

                            NVACRemind = NVACRemind -TheMDStatistic%BinVAC_NAtomArray(TheBin)

                            NVACCluster = NVACCluster + 1
                            NAtomEachVACCluster(NVACCluster) = TheMDStatistic%BinVAC_NAtomArray(TheBin)
                            exit
                        end if
                    END DO

                    if(exitflag .eq. .true.) then
                        exit
                    end if
                END DO
            else if(NVACRemind .LT. TheMDStatistic%MinVACNumEachCluster) then
                exitTotal = .false.
                exit
            end if

          END DO

          if(exitTotal .eq. .true.) then
            exit
          end if
        END DO

        return
    end subroutine GenerateClustersSize

    !***************************************************************
    subroutine Generate_Cascade_Locally_FormMDDataBase_Resample(hFile)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        !---Local Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        character*1000::OutFolder
        logical::WhetherIncludeSIA
        logical::WhetherIncludeVAC
        integer::CascadeNum
        logical::WhetherCascadeSameInOneBox
        character*1000::MDDataBasePath
        integer::Index_StartBox
        integer::Index_EndBox
        integer::Index_SIAConfig
        integer::Index_VACConfig
        type(MDStatistic)::TheMDStatistic
        integer::NSIACluster
        integer,dimension(:),allocatable::NAtomEachSIACluster
        integer::NVACCluster
        integer,dimension(:),allocatable::NAtomEachVACCluster
        integer::err
        integer::MultiBox
        real(kind=KINDDF),dimension(:,:),allocatable::Sphere_Central
        integer::I
        integer::J
        integer::K
        integer::IBox
        integer::ICase
        integer::IC
        integer::JC
        integer::IIC
        integer::processid
        integer::ISEED0,ISEED(2)
        integer::SIAIndex
        integer::VacancyIndex
        real(kind=KINDDF)::VectorLen
        real(kind=KINDDF)::ZDirection
        real(kind=KINDDF)::XDirection
        integer::ExitCount
        integer::CellNum_OneDim
        integer::CellNum
        integer::ICell
        real(kind=KINDDF),dimension(:,:),allocatable::CellCentralPos
        real(kind=KINDDF)::ACCUM
        real(kind=KINDDF)::RandNum
        integer::IBin
        integer::TheBin
        real(kind=KINDDF)::Distance
        real(kind=KINDDF)::Gap
        integer::GapCondition
        integer::CheckSIAEachBox
        integer::CheckVACEachBox
        integer::CascadePosModel
        real(kind=KINDDF),dimension(:,:),allocatable::CascadePos
        type(DiffusorValue)::TheDiffusorValue
        integer::TheDim
        !-----------Body--------------
        WhetherIncludeSIA = .false.
        WhetherIncludeVAC = .false.
        call ResloveCascadeControlFile_FormMDDataBase(hFile,WhetherIncludeSIA,WhetherIncludeVAC,CascadeNum,CascadePosModel,CascadePos,WhetherCascadeSameInOneBox, &
                                                      MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig)

        call CascadeDataBase_ANALYSIS_SIAANDVAC(MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig,TheMDStatistic)

        call GenerateClustersSize(TheMDStatistic,NSIACluster,NAtomEachSIACluster,NVACCluster,NAtomEachVACCluster)

        if(WhetherIncludeSIA .eq. .false.) then
            NSIACluster = 0
        end if

        if(WhetherIncludeVAC .eq. .false.) then
            NVACCluster = 0
        end if

        processid = 0

        call AllocateArray_Host(Sphere_Central,CascadeNum,3,"Sphere_Central")

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(Host_SimuCtrlParamList,Host_Boxes)


        ISEED0 = Host_SimuCtrlParamList%theSimulationCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,Host_SimuCtrlParamList,Host_Boxes)

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath))//"CascadeBox/")

        Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,CascadeNum*(NSIACluster+NVACCluster))

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        select case(CascadePosModel)
            case(CascadePosModel_ByVolumeAverage)
                !---ReDraw the cells-----
                CellNum_OneDim = floor(CascadeNum**C_UTH + 0.5D0)
                CellNum = CellNum_OneDim**3

                call AllocateArray_Host(CellCentralPos,CellNum,3,"CellCentralPos")

                ICell = 0
                    DO I = 1,CellNum_OneDim
                        DO J = 1,CellNum_OneDim
                            DO K = 1,CellNum_OneDim
                                ICell = ICell + 1
                                CellCentralPos(ICell,1) = Host_Boxes%BOXBOUNDARY(1,1) + (I - 0.5D0)*Host_Boxes%BOXSIZE(1)/CellNum_OneDim
                                CellCentralPos(ICell,2) = Host_Boxes%BOXBOUNDARY(2,1) + (J - 0.5D0)*Host_Boxes%BOXSIZE(2)/CellNum_OneDim
                                CellCentralPos(ICell,3) = Host_Boxes%BOXBOUNDARY(3,1) + (K - 0.5D0)*Host_Boxes%BOXSIZE(3)/CellNum_OneDim
                            END DO
                        END DO
                    END DO

            case(CascadePosModel_ByUserSpeicaled)
                !---ReDraw the cells-----
                call AllocateArray_Host(CellCentralPos,CascadeNum,3,"CellCentralPos")

                DO I = 1,3
                    CellCentralPos(:,I) = CascadePos(:,I)*Host_Boxes%BOXSIZE(I) +  Host_Boxes%BOXBOUNDARY(I,1)
                END DO

            case(CascadePosModel_ByRandom)
                !---ReDraw the cells-----
                call AllocateArray_Host(CellCentralPos,CascadeNum,3,"CellCentralPos")

                DO ICase = 1,CascadeNum
                    DO I = 1,3
                        CellCentralPos(ICase,1) = Host_Boxes%BOXBOUNDARY(1,1) + Host_Boxes%BOXSIZE(1)*DRAND32()
                        CellCentralPos(ICase,2) = Host_Boxes%BOXBOUNDARY(2,1) + Host_Boxes%BOXSIZE(2)*DRAND32()
                        CellCentralPos(ICase,3) = Host_Boxes%BOXBOUNDARY(3,1) + Host_Boxes%BOXSIZE(3)*DRAND32()
                    END DO
                END DO

            case default
                write(*,*) "MCPSCUERROR: Unknown cascade position model"
                write(*,*) CascadePosModel
                pause
                stop
        end select
        !------------------------

        IC = 0
        DO IBox = 1,Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox

            CheckSIAEachBox = 0
            CheckVACEachBox = 0

            DO ICase = 1,CascadeNum
                Sphere_Central(ICase,:) = CellCentralPos(ICase,:)

                DO IIC = 1,NSIACluster
                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = NAtomEachSIACluster(IIC)

                    CheckSIAEachBox = CheckSIAEachBox + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .LE. 0) then
                        write(*,*) "Opps...,the SIA cluster size cannot less than 0"
                        write(*,*) "For cluster: ",IC
                        write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA
                        pause
                        stop
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU


                    DO While(.true.)

                        DO While(.true.)
                            ExitCount = 0

                            Accum = 0.D0
                            RandNum = DRAND32()
                            DO IBin = 1,size(TheMDStatistic%ToCent_DistSIA)
                                Accum = Accum + TheMDStatistic%ToCent_DistSIA(IBin)

                                if(Accum .GE. RandNum) then
                                    TheBin = IBin
                                    exit
                                end if
                            END DO
                            VectorLen = TheMDStatistic%BinSIA_ToCenterArray(TheBin)
                            ZDirection = DRAND32()*CP_PI
                            XDirection = DRAND32()*2*CP_PI

                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = Sphere_Central(ICase,1) + VectorLen*sin(ZDirection)*cos(XDirection)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = Sphere_Central(ICase,2) + VectorLen*sin(ZDirection)*sin(XDirection)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = Sphere_Central(ICase,3) + VectorLen*cos(ZDirection)

                            DO I = 1,3
                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                                else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                                end if

                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GE. Host_Boxes%BOXBOUNDARY(I,1) .AND. &
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LE. Host_Boxes%BOXBOUNDARY(I,2)) then
                                    ExitCount = ExitCount + 1
                                end if
                            END DO

                            if(ExitCount .EQ. 3) then
                                exit
                            end if
                        END DO

                        !---Consider the Gap between SIA clusters
                        Accum = 0.D0
                        RandNum = DRAND32()
                        DO IBin = 1,size(TheMDStatistic%ClusterGap_DistSIA)
                            Accum = Accum + TheMDStatistic%ClusterGap_DistSIA(IBin)

                            if(Accum .GE. RandNum) then
                                TheBin = IBin
                                exit
                            end if
                        END DO
                        Gap = TheMDStatistic%BinSIA_BetweenClusterArray(TheBin)

                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .GT. Gap) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        end if

                        !---Consider the Gap between SIA cluster and VAC Cluster
                        Accum = 0.D0
                        RandNum = DRAND32()
                        DO IBin = 1,size(TheMDStatistic%ClusterGap_SIAToVAC)
                            Accum = Accum + TheMDStatistic%ClusterGap_SIAToVAC(IBin)

                            if(Accum .GE. RandNum) then
                                TheBin = IBin
                                exit
                            end if
                        END DO
                        Gap = TheMDStatistic%BinSIAToVAC_BetweenClusterArray(TheBin)

                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .GT. Gap) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        else
                            exit
                        end if

                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

                DO IIC = 1,NVACCluster

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = NAtomEachVACCluster(IIC)

                    CheckVACEachBox = CheckVACEachBox + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .LE. 0) then
                        write(*,*) "Opps...,the VAC cluster size cannot less than 0"
                        write(*,*) "For cluster: ",IC
                        write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA
                        pause
                        stop
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                    DO While(.true.)

                        DO While(.true.)
                            ExitCount = 0

                            Accum = 0.D0
                            RandNum = DRAND32()
                            DO IBin = 1,size(TheMDStatistic%ToCent_DistVAC)
                                Accum = Accum + TheMDStatistic%ToCent_DistVAC(IBin)

                                if(Accum .GE. RandNum) then
                                    TheBin = IBin
                                    exit
                                end if
                            END DO
                            VectorLen = TheMDStatistic%BinVAC_ToCenterArray(TheBin)
                            ZDirection = DRAND32()*CP_PI
                            XDirection = DRAND32()*2*CP_PI

                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = Sphere_Central(ICase,1) + VectorLen*sin(ZDirection)*cos(XDirection)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = Sphere_Central(ICase,2) + VectorLen*sin(ZDirection)*sin(XDirection)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = Sphere_Central(ICase,3) + VectorLen*cos(ZDirection)

                            DO I = 1,3
                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                                else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                                end if

                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GE. Host_Boxes%BOXBOUNDARY(I,1) .AND. &
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LE. Host_Boxes%BOXBOUNDARY(I,2)) then
                                    ExitCount = ExitCount + 1
                                end if
                            END DO

                            if(ExitCount .EQ. 3) then
                                exit
                            end if
                        END DO

                        !---Consider the Gap between VAC clusters
                        Accum = 0.D0
                        RandNum = DRAND32()
                        DO IBin = 1,size(TheMDStatistic%ClusterGap_DistVAC)
                            Accum = Accum + TheMDStatistic%ClusterGap_DistVAC(IBin)

                            if(Accum .GE. RandNum) then
                                TheBin = IBin
                                exit
                            end if
                        END DO
                        Gap = TheMDStatistic%BinVAC_BetweenClusterArray(TheBin)

                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .GT. Gap) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        end if

                        !---Consider the Gap between SIA cluster and VAC Cluster
                        Accum = 0.D0
                        RandNum = DRAND32()
                        DO IBin = 1,size(TheMDStatistic%ClusterGap_SIAToVAC)
                            Accum = Accum + TheMDStatistic%ClusterGap_SIAToVAC(IBin)

                            if(Accum .GE. RandNum) then
                                TheBin = IBin
                                exit
                            end if
                        END DO
                        Gap = TheMDStatistic%BinSIAToVAC_BetweenClusterArray(TheBin)

                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .GT. Gap) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        else
                            exit
                        end if

                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

            END DO

            if(WhetherIncludeSIA .eq. .true. .and. WhetherIncludeVAC .eq. .true.) then
                if(CheckSIAEachBox .ne. CheckVACEachBox) then
                    write(*,*) "MCPSCUERROR: the SIA number is not equal with VAC number"
                    write(*,*) "SIA: ",CheckSIAEachBox
                    write(*,*) "VAC: ",CheckVACEachBox
                    write(*,*) "In box: ",IBox
                    pause
                    stop
                end if
            end if

        END DO

        call Host_Boxes%PutoutCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record)

        call DeAllocateArray_Host(CellCentralPos,"CellCentralPos")

        call DeAllocateArray_Host(Sphere_Central,"Sphere_Central")

        call DeAllocateArray_Host(NAtomEachSIACluster,"NAtomEachSIACluster")

        call DeAllocateArray_Host(NAtomEachVACCluster,"NAtomEachVACCluster")

        call DeAllocateArray_Host(CascadePos,"CascadePos")

        call Host_Boxes%Clean()

        call TheMDStatistic%Clean_MDStatistic()

        return
    end subroutine Generate_Cascade_Locally_FormMDDataBase_Resample

    !***************************************************************
    subroutine Generate_Cascade_Locally_FormMDDataBase_Directly(hFile)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        !---Local Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        character*1000::OutFolder
        logical::WhetherIncludeSIA
        logical::WhetherIncludeVAC
        integer::CascadeNum
        logical::WhetherCascadeSameInOneBox
        character*1000::MDDataBasePath
        integer::Index_StartBox
        integer::Index_EndBox
        integer::Index_SIAConfig
        integer::Index_VACConfig
        type(MDStatistic)::TheMDStatistic
        integer::NSIACluster
        integer::NVACCluster
        integer::err
        real(kind=KINDDF),dimension(:,:),allocatable::Sphere_Central
        integer::I
        integer::J
        integer::K
        integer::IBox
        integer::ICase
        integer::IC
        integer::JC
        integer::IIC
        integer::processid
        integer::ISEED0,ISEED(2)
        integer::SIAIndex
        integer::VacancyIndex
        integer::CellNum_OneDim
        integer::CellNum
        integer::ICell
        real(kind=KINDDF),dimension(:,:),allocatable::CellCentralPos
        integer::CheckSIAEachBox
        integer::CheckVACEachBox
        type(ClusterAtom),dimension(:),allocatable::Read_ClusterArray
        integer,dimension(:),allocatable::Read_NAtomEachCluster
        integer,dimension(:),allocatable::Read_NSIAClusterEachBox
        integer,dimension(:),allocatable::Read_NVACClusterEachBox
        integer,dimension(:),allocatable::ClusterNum_EachBox
        integer::SelectedBoxIndex
        integer::ICSIAReadFrom
        integer::ICVACReadFrom
        integer::CascadePosModel
        real(kind=KINDDF),dimension(:,:),allocatable::CascadePos
        type(DiffusorValue)::TheDiffusorValue
        integer::TheDim
        !-----------Body--------------
        WhetherIncludeSIA = .false.
        WhetherIncludeVAC = .false.
        call ResloveCascadeControlFile_FormMDDataBase(hFile,WhetherIncludeSIA,WhetherIncludeVAC,CascadeNum,CascadePosModel,CascadePos,WhetherCascadeSameInOneBox, &
                                                      MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig)

        call CascadeDataBase_DirectlyRead(MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig,Read_ClusterArray,Read_NAtomEachCluster,Read_NSIAClusterEachBox,Read_NVACClusterEachBox)

        processid = 0

        call AllocateArray_Host(Sphere_Central,CascadeNum,3,"Sphere_Central")

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(Host_SimuCtrlParamList,Host_Boxes)


        ISEED0 = Host_SimuCtrlParamList%theSimulationCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,Host_SimuCtrlParamList,Host_Boxes)

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath))//"CascadeBox/")

        Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        if(Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox .LE. 0) then
            write(*,*) "MCPSCUERROR: The box number less than 1"
            pause
            stop
        end if

        if(allocated(ClusterNum_EachBox)) deallocate(ClusterNum_EachBox)
        allocate(ClusterNum_EachBox(Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox))
        ClusterNum_EachBox = 0

        if(WhetherCascadeSameInOneBox) then
            DO IBox = 1,Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
                SelectedBoxIndex = mod(IBox,Index_EndBox - Index_StartBox + 1)

                if(SelectedBoxIndex .eq. 0) then
                    SelectedBoxIndex = Index_EndBox - Index_StartBox + 1
                end if

                if(WhetherIncludeSIA .eq. .true.) then
                    ClusterNum_EachBox(IBox) = CascadeNum*Read_NSIAClusterEachBox(SelectedBoxIndex)
                end if

                if(WhetherIncludeVAC .eq. .true.) then
                    ClusterNum_EachBox(IBox) = ClusterNum_EachBox(IBox) + CascadeNum*Read_NVACClusterEachBox(SelectedBoxIndex)
                end if
            END DO
        else
            DO IBox = 1,Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
                DO ICase = 1,CascadeNum
                    SelectedBoxIndex = mod((IBox-1)*CascadeNum + ICase,Index_EndBox - Index_StartBox + 1)

                    if(SelectedBoxIndex .eq. 0) then
                        SelectedBoxIndex = Index_EndBox - Index_StartBox + 1
                    end if

                    if(WhetherIncludeSIA .eq. .true.) then
                        ClusterNum_EachBox(IBox) = ClusterNum_EachBox(IBox) + Read_NSIAClusterEachBox(SelectedBoxIndex)
                    end if

                    if(WhetherIncludeVAC .eq. .true.) then
                        ClusterNum_EachBox(IBox) = ClusterNum_EachBox(IBox) + Read_NVACClusterEachBox(SelectedBoxIndex)
                    end if

                END DO
            END DO
        end if

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,ClusterNum_EachBox)

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        select case(CascadePosModel)
            case(CascadePosModel_ByVolumeAverage)
                !---ReDraw the cells-----
                CellNum_OneDim = floor(CascadeNum**C_UTH + 0.5D0)
                CellNum = CellNum_OneDim**3

                call AllocateArray_Host(CellCentralPos,CellNum,3,"CellCentralPos")

                ICell = 0
                    DO I = 1,CellNum_OneDim
                        DO J = 1,CellNum_OneDim
                            DO K = 1,CellNum_OneDim
                                ICell = ICell + 1
                                CellCentralPos(ICell,1) = Host_Boxes%BOXBOUNDARY(1,1) + (I - 0.5D0)*Host_Boxes%BOXSIZE(1)/CellNum_OneDim
                                CellCentralPos(ICell,2) = Host_Boxes%BOXBOUNDARY(2,1) + (J - 0.5D0)*Host_Boxes%BOXSIZE(2)/CellNum_OneDim
                                CellCentralPos(ICell,3) = Host_Boxes%BOXBOUNDARY(3,1) + (K - 0.5D0)*Host_Boxes%BOXSIZE(3)/CellNum_OneDim
                            END DO
                        END DO
                    END DO

            case(CascadePosModel_ByUserSpeicaled)
                !---ReDraw the cells-----
                call AllocateArray_Host(CellCentralPos,CascadeNum,3,"CellCentralPos")

                DO I = 1,3
                    CellCentralPos(:,I) = CascadePos(:,I)*Host_Boxes%BOXSIZE(I) +  Host_Boxes%BOXBOUNDARY(I,1)
                END DO

            case(CascadePosModel_ByRandom)
                !---ReDraw the cells-----
                call AllocateArray_Host(CellCentralPos,CascadeNum,3,"CellCentralPos")

                DO ICase = 1,CascadeNum
                    DO I = 1,3
                        CellCentralPos(ICase,1) = Host_Boxes%BOXBOUNDARY(1,1) + Host_Boxes%BOXSIZE(1)*DRAND32()
                        CellCentralPos(ICase,2) = Host_Boxes%BOXBOUNDARY(2,1) + Host_Boxes%BOXSIZE(2)*DRAND32()
                        CellCentralPos(ICase,3) = Host_Boxes%BOXBOUNDARY(3,1) + Host_Boxes%BOXSIZE(3)*DRAND32()
                    END DO
                END DO

            case default
                write(*,*) "MCPSCUERROR: Unknown cascade position model"
                write(*,*) CascadePosModel
                pause
                stop
        end select
        !------------------------

        IC = 0
        DO IBox = 1,Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox

            CheckSIAEachBox = 0
            CheckVACEachBox = 0

            DO ICase = 1,CascadeNum
                Sphere_Central(ICase,:) = CellCentralPos(ICase,:)

                if(WhetherCascadeSameInOneBox) then
                    SelectedBoxIndex = mod(IBox,Index_EndBox - Index_StartBox + 1)
                else
                    SelectedBoxIndex = mod((IBox-1)*CascadeNum + ICase,Index_EndBox - Index_StartBox + 1)
                end if

                if(SelectedBoxIndex .eq. 0) then
                    SelectedBoxIndex = Index_EndBox - Index_StartBox + 1
                end if

                if(SelectedBoxIndex .GT. 1) then
                    ICSIAReadFrom = sum(Read_NSIAClusterEachBox(1:SelectedBoxIndex - 1)) + sum(Read_NVACClusterEachBox(1:SelectedBoxIndex - 1))
                    ICVACReadFrom = sum(Read_NSIAClusterEachBox(1:SelectedBoxIndex)) + sum(Read_NVACClusterEachBox(1:SelectedBoxIndex - 1))
                else
                    ICSIAReadFrom = 0
                    ICVACReadFrom = sum(Read_NSIAClusterEachBox(1:1))
                end if

                if(WhetherIncludeSIA .eq. .true.) then
                    NSIACluster = Read_NSIAClusterEachBox(SelectedBoxIndex)
                else
                    NSIACluster = 0
                end if

                if(WhetherIncludeVAC .eq. .true.) then
                    NVACCluster = Read_NVACClusterEachBox(SelectedBoxIndex)
                else
                    NVACCluster = 0
                end if

                DO IIC = 1,NSIACluster
                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = Read_NAtomEachCluster(ICSIAReadFrom + IIC)

                    CheckSIAEachBox = CheckSIAEachBox + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .LE. 0) then
                        write(*,*) "Opps...,the SIA cluster size cannot less than 0"
                        write(*,*) "For cluster: ",IC
                        write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA
                        pause
                        stop
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = Sphere_Central(ICase,1) + Read_ClusterArray(ICSIAReadFrom + IIC)%POS(1)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = Sphere_Central(ICase,2) + Read_ClusterArray(ICSIAReadFrom + IIC)%POS(2)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = Sphere_Central(ICase,3) + Read_ClusterArray(ICSIAReadFrom + IIC)%POS(3)


                    DO I = 1,3
                        if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXBOUNDARY(I,1)) .GT. 2*Host_Boxes%BOXSIZE(I)) then
                            write(*,*) "Opps, the box is too small : "
                            write(*,*) "The cluster position is : ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                            write(*,*) "The box size is : ",Host_Boxes%BOXSIZE(I)
                            write(*,*) "The box boundary is : ",Host_Boxes%BOXBOUNDARY(I,1), "  ", Host_Boxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if

                        if((Host_Boxes%BOXBOUNDARY(I,2) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)) .GT. 2*Host_Boxes%BOXSIZE(I)) then
                            write(*,*) "Opps, the box is too small : "
                            write(*,*) "The cluster position is : ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                            write(*,*) "The box size is : ",Host_Boxes%BOXSIZE(I)
                            write(*,*) "The box boundary is : ",Host_Boxes%BOXBOUNDARY(I,1), "  ", Host_Boxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if

                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                        else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                        end if
                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

                DO IIC = 1,NVACCluster

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = Read_NAtomEachCluster(ICVACReadFrom + IIC)

                    CheckVACEachBox = CheckVACEachBox + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .LE. 0) then
                        write(*,*) "Opps...,the VAC cluster size cannot less than 0"
                        write(*,*) "For cluster: ",IC
                        write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA
                        pause
                        stop
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = Sphere_Central(ICase,1) + Read_ClusterArray(ICVACReadFrom + IIC)%POS(1)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = Sphere_Central(ICase,2) + Read_ClusterArray(ICVACReadFrom + IIC)%POS(2)
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = Sphere_Central(ICase,3) + Read_ClusterArray(ICVACReadFrom + IIC)%POS(3)

                    DO I = 1,3

                        if((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXBOUNDARY(I,1)) .GT. 2*Host_Boxes%BOXSIZE(I)) then
                            write(*,*) "Opps, the box is too small : "
                            write(*,*) "The cluster position is : ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                            write(*,*) "The box size is : ",Host_Boxes%BOXSIZE(I)
                            write(*,*) "The box boundary is : ",Host_Boxes%BOXBOUNDARY(I,1), "  ", Host_Boxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if

                        if((Host_Boxes%BOXBOUNDARY(I,2) - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)) .GT. 2*Host_Boxes%BOXSIZE(I)) then
                            write(*,*) "Opps, the box is too small : "
                            write(*,*) "The cluster position is : ",Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I)
                            write(*,*) "The box size is : ",Host_Boxes%BOXSIZE(I)
                            write(*,*) "The box boundary is : ",Host_Boxes%BOXBOUNDARY(I,1), "  ", Host_Boxes%BOXBOUNDARY(I,2)
                            pause
                            stop
                        end if


                        if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                        else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                        end if
                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

            END DO

            if(WhetherIncludeSIA .eq. .true. .and. WhetherIncludeVAC .eq. .true.) then
                if(CheckSIAEachBox .ne. CheckVACEachBox) then
                    write(*,*) "MCPSCUERROR: the SIA number is not equal with VAC number"
                    write(*,*) "SIA: ",CheckSIAEachBox
                    write(*,*) "VAC: ",CheckVACEachBox
                    write(*,*) "In box: ",IBox
                    pause
                    stop
                end if
            end if

        END DO

        call Host_Boxes%PutoutCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record)

        if(allocated(Read_ClusterArray)) deallocate(Read_ClusterArray)

        call DeAllocateArray_Host(CellCentralPos,"CellCentralPos")

        call DeAllocateArray_Host(Sphere_Central,"Sphere_Central")

        call DeAllocateArray_Host(Read_NAtomEachCluster,"Read_NAtomEachCluster")

        call DeAllocateArray_Host(Read_NSIAClusterEachBox,"Read_NSIAClusterEachBox")

        call DeAllocateArray_Host(Read_NVACClusterEachBox,"Read_NVACClusterEachBox")

        call DeAllocateArray_Host(ClusterNum_EachBox,"ClusterNum_EachBox")

        call DeAllocateArray_Host(CascadePos,"CascadePos")

        call Host_Boxes%Clean()

        return
    end subroutine Generate_Cascade_Locally_FormMDDataBase_Directly

    !***************************************************************
    subroutine Generate_Cascade_Uniform_FormMDDataBase_Resample(hFile)
        !---Dummy Vars---
        integer,intent(in)::hFile
        !---Local Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        character*1000::OutFolder
        logical::WhetherIncludeSIA
        logical::WhetherIncludeVAC
        integer::CascadeNum
        logical::WhetherCascadeSameInOneBox
        character*1000::MDDataBasePath
        integer::Index_StartBox
        integer::Index_EndBox
        integer::Index_SIAConfig
        integer::Index_VACConfig
        type(MDStatistic)::TheMDStatistic
        integer::NSIACluster
        integer,dimension(:),allocatable::NAtomEachSIACluster
        integer::NVACCluster
        integer,dimension(:),allocatable::NAtomEachVACCluster
        integer::err
        integer::MultiBox
        integer::I
        integer::IBox
        integer::ICase
        integer::IC
        integer::JC
        integer::IIC
        integer::processid
        integer::ISEED0,ISEED(2)
        integer::SIAIndex
        integer::VacancyIndex
        real(kind=KINDDF)::Distance
        integer::GapCondition
        real(kind=KINDDF)::Accum
        real(kind=KINDDF)::RandNum
        integer::IBin
        integer::TheBin
        integer::CheckSIAEachBox
        integer::CheckVACEachBox
        integer::CascadePosModel
        real(kind=KINDDF),dimension(:,:),allocatable::CascadePos
        type(DiffusorValue)::TheDiffusorValue
        integer::TheDim
        !-----------Body--------------

        WhetherIncludeSIA = .false.
        WhetherIncludeVAC = .false.
        call ResloveCascadeControlFile_FormMDDataBase(hFile,WhetherIncludeSIA,WhetherIncludeVAC,CascadeNum,CascadePosModel,CascadePos,WhetherCascadeSameInOneBox, &
                                                      MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig)

        call CascadeDataBase_ANALYSIS_SIAANDVAC(MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig,TheMDStatistic)

        call GenerateClustersSize(TheMDStatistic,NSIACluster,NAtomEachSIACluster,NVACCluster,NAtomEachVACCluster)

        if(WhetherIncludeSIA .eq. .false.) then
            NSIACluster = 0
        end if

        if(WhetherIncludeVAC .eq. .false.) then
            NVACCluster = 0
        end if

        processid = 0

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(Host_SimuCtrlParamList,Host_Boxes)


        ISEED0 = Host_SimuCtrlParamList%theSimulationCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,Host_SimuCtrlParamList,Host_Boxes)

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath))//"CascadeBox/")

        Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,CascadeNum*(NSIACluster+NVACCluster))

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        !------------------------

        IC = 0
        DO IBox = 1,Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox

            CheckSIAEachBox = 0
            CheckVACEachBox = 0

            DO ICase = 1,CascadeNum

                DO IIC = 1,NSIACluster
                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = NAtomEachSIACluster(IIC)

                    CheckSIAEachBox = CheckSIAEachBox + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .LE. 0) then
                        write(*,*) "Opps...,the SIA cluster size cannot less than 0"
                        write(*,*) "For cluster: ",IC
                        write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA
                        pause
                        stop
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU


                    DO While(.true.)

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                        END DO

                        !---Consider the minimum Gap between SIA clusters
                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .LT. TheMDStatistic%MinDistanceSIA_BetweenCluster) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        end if

                        !---Consider the minimum Gap between SIA cluster and VAC Cluster
                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .LT. TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        else
                            exit
                        end if

                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)



                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

                DO IIC = 1,NVACCluster

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = NAtomEachVACCluster(IIC)

                    CheckVACEachBox = CheckVACEachBox + Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .LE. 0) then
                        write(*,*) "Opps...,the VAC cluster size cannot less than 0"
                        write(*,*) "For cluster: ",IC
                        write(*,*) Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA
                        pause
                        stop
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU

                    DO While(.true.)

                        DO I = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%BOXBOUNDARY(I,1) + Host_Boxes%BOXSIZE(I)*DRAND32()
                        END DO

                        !---Consider the minimum Gap between VAC clusters
                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .LT. TheMDStatistic%MinDistanceVAC_BetweenCluster) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        end if

                        !---Consider the minimum Gap between SIA cluster and VAC Cluster
                        GapCondition = 0
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(NSIACluster+NVACCluster),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .LT. TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster) then
                                    GapCondition = 1
                                    exit
                                end if
                            end if
                        END DO

                        if(GapCondition .GT. 0) then
                            cycle
                        else
                            exit
                        end if

                    END DO


                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

            END DO


            if(WhetherIncludeSIA .eq. .true. .and. WhetherIncludeVAC .eq. .true.) then
                if(CheckSIAEachBox .ne. CheckVACEachBox) then
                    write(*,*) "MCPSCUERROR: the SIA number is not equal with VAC number"
                    write(*,*) "SIA: ",CheckSIAEachBox
                    write(*,*) "VAC: ",CheckVACEachBox
                    write(*,*) "In box: ",IBox
                    pause
                    stop
                end if
            end if

        END DO

        call Host_Boxes%PutoutCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record)

        call DeAllocateArray_Host(NAtomEachSIACluster,"NAtomEachSIACluster")

        call DeAllocateArray_Host(NAtomEachVACCluster,"NAtomEachVACCluster")

        call DeAllocateArray_Host(CascadePos,"CascadePos")

        call Host_Boxes%Clean()

        call TheMDStatistic%Clean_MDStatistic()

        return
    end subroutine Generate_Cascade_Uniform_FormMDDataBase_Resample

    !*************************************************************
    subroutine ResloveCascadeControlFile_Locally_CentUniform(hFile,WhetherIncludeSIA,WhetherIncludeVAC,ClusterNumOneCase,CascadeNum,CascadePosModel,CascadePos,WhetherCascadeSameInOneBox)
        !---Dummy Vars---
        integer,intent(in)::hFile
        integer,intent(out)::ClusterNumOneCase
        logical,intent(out)::WhetherIncludeSIA
        logical,intent(out)::WhetherIncludeVAC
        integer,intent(out)::CascadeNum
        integer,intent(out)::CascadePosModel
        real(kind=KINDDF),dimension(:,:),allocatable::CascadePos
        logical,intent(out)::WhetherCascadeSameInOneBox
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*30::KEYWORD
        character*200::STRTMP(10)
        character*20,dimension(:),allocatable::STRTMPCascadePos
        integer::N
        integer::ICase
        logical::Finded
        !---Body---

        LINE = 0

        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&NSIAVACPAIRSEACHCASCADE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special frankel pairs the cluster number in one cascade."
                        pause
                        stop
                    end if
                    ClusterNumOneCase = ISTR(STRTMP(1))

                    if(ClusterNumOneCase .LE. 0) then
                        write(*,*) "MCPSCUERROR: The frankel pairs in one cascade cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the frankel pairs in one box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&CASCADECENTERPOSMODEL")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the cascade center position model."
                        pause
                        stop
                    end if
                    CascadePosModel = ISTR(STRTMP(1))

                    if(CascadePosModel .eq. CascadePosModel_ByVolumeAverage) then
                        write(*,*) "The cascade center position mode is by volume average "
                    else if(CascadePosModel .eq. CascadePosModel_ByUserSpeicaled) then
                        write(*,*) "The cascade center position mode is by user special position"
                    else if(CascadePosModel .eq. CascadePosModel_ByRandom) then
                            write(*,*) "The cascade center position mode is by random position"
                    else
                        write(*,*) "MCPSCUERROR: Unknown cascade center position model"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the cascade center position model."
           pause
           stop
        end if


        if(CascadePosModel .eq. CascadePosModel_ByUserSpeicaled) then

            Finded = .false.
            rewind(hFile)
            Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
                LINE = LINE + 1
                STR = adjustl(STR)
                call RemoveComments(STR,"!")

                if(LENTRIM(STR) .LE. 0) then
                    cycle
                end if

                call GETKEYWORD("&",STR,KEYWORD)

                call UPCASE(KEYWORD)

                select case(KEYWORD(1:LENTRIM(KEYWORD)))
                    case("&CASCADECENTERPOS")

                        allocate(STRTMPCascadePos(3*CascadeNum))
                        STRTMPCascadePos = ""

                        allocate(CascadePos(CascadeNum,3))

                        call EXTRACT_NUMB(STR,3*CascadeNum,N,STRTMPCascadePos)
                        if(N .LT. 3*CascadeNum) then
                            write(*,*) "MCPSCUERROR: You must special the number cascade center position each with cascade number."
                            pause
                            stop
                        end if


                        DO ICase = 1,CascadeNum
                            CascadePos(ICase,1) = DRSTR(STRTMPCascadePos((ICase - 1)*3 + 1))
                            CascadePos(ICase,2) = DRSTR(STRTMPCascadePos((ICase - 1)*3 + 2))
                            CascadePos(ICase,3) = DRSTR(STRTMPCascadePos((ICase - 1)*3 + 3))
                        END DO

                        Finded = .true.

                        if(allocated(STRTMPCascadePos)) deallocate(STRTMPCascadePos)
                end select
            END DO

            if(Finded .eq. .false.) then
                write(*,*) "MCPSCUERROR: You must special the cascade center position."
                pause
                stop
            end if
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&CASCADENUMBER")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special the cascade number in one box."
                        pause
                        stop
                    end if
                    CascadeNum = ISTR(STRTMP(1))

                    if(CascadeNum .LE. 0) then
                        write(*,*) "MCPSCUERROR: The cascade number in one box cannot less than 0"
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special the cascade number in one box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&CASCADESAME")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special whether cascades are same in the box."
                        pause
                        stop
                    end if
                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        WhetherCascadeSameInOneBox = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        WhetherCascadeSameInOneBox = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether cascades are same in the box."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special whether cascades are same in the box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INCLUDESIACLUSTER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special whether include SIA in box."
                        pause
                        stop
                    end if
                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        WhetherIncludeSIA = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        WhetherIncludeSIA = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether include SIA in box."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special whether include SIA in box."
           pause
           stop
        end if


        Finded = .false.
        rewind(hFile)
        Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
            LINE = LINE + 1
            STR = adjustl(STR)
            call RemoveComments(STR,"!")

            if(LENTRIM(STR) .LE. 0) then
                cycle
            end if

            call GETKEYWORD("&",STR,KEYWORD)

            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))

                case("&INCLUDEVACCLUSTER")
                    call EXTRACT_SUBSTR(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: You must special whether include VAC in box."
                        pause
                        stop
                    end if
                    STRTMP(1) = adjustl(trim(STRTMP(1)))
                    call UPCASE(STRTMP(1))

                    if(IsStrEqual(adjustl(trim(STRTMP(1))),"YES")) then
                        WhetherIncludeVAC = .true.
                    else if(IsStrEqual(adjustl(trim(STRTMP(1))),"NO")) then
                        WhetherIncludeVAC = .false.
                    else
                        write(*,*) "MCPSUCERROR: You should special 'YES' or 'NO' to determine whether include VAC in box."
                        write(*,*) "However, what you used is: ",STRTMP(1)
                        pause
                        stop
                    end if

                    Finded = .true.
            end select
        END DO

        if(Finded .eq. .false.) then
           write(*,*) "MCPSCUERROR: You must special whether include VAC in box."
           pause
           stop
        end if

        return
    end subroutine ResloveCascadeControlFile_Locally_CentUniform

    !***************************************************************
    subroutine Generate_Cascade_Locally_CentUniform(hFile)
        !---Dummy Vars---
        integer,intent(in)::hFile
        !---Local Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParamList)::Host_SimuCtrlParamList
        type(MigCoalClusterRecord)::Record
        character*1000::OutFolder
        logical::WhetherIncludeSIA
        logical::WhetherIncludeVAC
        integer::NSIACluster
        integer::NVACCluster
        integer::ClusterNumOneCase
        integer::CascadeNum
        logical::WhetherCascadeSameInOneBox
        integer::err
        integer::MultiBox
        real(kind=KINDDF),dimension(:,:),allocatable::Sphere_Central
        real(kind=KINDDF),dimension(:),allocatable::Sphere_Radius
        integer::I
        integer::J
        integer::K
        integer::IBox
        integer::ICase
        integer::IC
        integer::IIC
        integer::processid
        integer::ISEED0,ISEED(2)
        integer::SIAIndex
        integer::VacancyIndex
        real(kind=KINDDF)::VectorLen
        real(kind=KINDDF)::ZDirection
        real(kind=KINDDF)::XDirection
        integer::ExitCount
        integer::CellNum_OneDim
        integer::CellNum
        integer::ICell
        real(kind=KINDDF),dimension(:,:),allocatable::CellCentralPos
        integer::CascadePosModel
        real(kind=KINDDF),dimension(:,:),allocatable::CascadePos
        type(DiffusorValue)::TheDiffusorValue
        integer::TheDim
        !-----------Body--------------

        call ResloveCascadeControlFile_Locally_CentUniform(hFile,WhetherIncludeSIA,WhetherIncludeVAC,ClusterNumOneCase,CascadeNum,CascadePosModel,CascadePos,WhetherCascadeSameInOneBox)

        if(WhetherIncludeSIA .eq. .false.) then
            NSIACluster = 0
        else
            NSIACluster = ClusterNumOneCase
        end if

        if(WhetherIncludeVAC .eq. .false.) then
            NVACCluster = 0
        else
            NVACCluster = ClusterNumOneCase
        end if

        processid = 0

        call AllocateArray_Host(Sphere_Central,CascadeNum,3,"Sphere_Central")
        call AllocateArray_Host(Sphere_Radius,CascadeNum,"Sphere_Radius")

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(Host_SimuCtrlParamList,Host_Boxes)


        ISEED0 = Host_SimuCtrlParamList%theSimulationCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,Host_SimuCtrlParamList,Host_Boxes)

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath))//"CascadeBox/")

        Host_SimuCtrlParamList%theSimulationCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParamList%theSimulationCtrlParam)

        call Record%InitMigCoalClusterRecord(MultiBox=Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox)

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParamList%theSimulationCtrlParam,CascadeNum*(NSIACluster + NVACCluster))

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        Sphere_Radius = 80*Host_Boxes%LatticeLength


        select case(CascadePosModel)
            case(CascadePosModel_ByVolumeAverage)
                !---ReDraw the cells-----
                CellNum_OneDim = floor(CascadeNum**C_UTH + 0.5D0)
                CellNum = CellNum_OneDim**3

                call AllocateArray_Host(CellCentralPos,CellNum,3,"CellCentralPos")

                ICell = 0
                    DO I = 1,CellNum_OneDim
                        DO J = 1,CellNum_OneDim
                            DO K = 1,CellNum_OneDim
                                ICell = ICell + 1
                                CellCentralPos(ICell,1) = Host_Boxes%BOXBOUNDARY(1,1) + (I - 0.5D0)*Host_Boxes%BOXSIZE(1)/CellNum_OneDim
                                CellCentralPos(ICell,2) = Host_Boxes%BOXBOUNDARY(2,1) + (J - 0.5D0)*Host_Boxes%BOXSIZE(2)/CellNum_OneDim
                                CellCentralPos(ICell,3) = Host_Boxes%BOXBOUNDARY(3,1) + (K - 0.5D0)*Host_Boxes%BOXSIZE(3)/CellNum_OneDim
                            END DO
                        END DO
                    END DO

            case(CascadePosModel_ByUserSpeicaled)
                !---ReDraw the cells-----
                call AllocateArray_Host(CellCentralPos,CascadeNum,3,"CellCentralPos")
                DO I = 1,3
                    CellCentralPos(:,I) = CascadePos(:,I)*Host_Boxes%BOXSIZE(I) +  Host_Boxes%BOXBOUNDARY(I,1)
                END DO

            case(CascadePosModel_ByRandom)
                !---ReDraw the cells-----
                call AllocateArray_Host(CellCentralPos,CascadeNum,3,"CellCentralPos")

                DO ICase = 1,CascadeNum
                    DO I = 1,3
                        CellCentralPos(ICase,1) = Host_Boxes%BOXBOUNDARY(1,1) + Host_Boxes%BOXSIZE(1)*DRAND32()
                        CellCentralPos(ICase,2) = Host_Boxes%BOXBOUNDARY(2,1) + Host_Boxes%BOXSIZE(2)*DRAND32()
                        CellCentralPos(ICase,3) = Host_Boxes%BOXBOUNDARY(3,1) + Host_Boxes%BOXSIZE(3)*DRAND32()
                    END DO
                END DO

            case default
                write(*,*) "MCPSCUERROR: Unknown cascade position model"
                write(*,*) CascadePosModel
                pause
                stop
        end select
        !------------------------

        IC = 0
        DO IBox = 1,Host_SimuCtrlParamList%theSimulationCtrlParam%MultiBox
            DO ICase = 1,CascadeNum
                Sphere_Central(ICase,:) = CellCentralPos(ICase,:)

                DO IIC = 1,NSIACluster

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = 1
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU
                    DO While(.true.)
                        ExitCount = 0

                        VectorLen = Sphere_Radius(ICase)*DRAND32()
                        ZDirection = DRAND32()*CP_PI
                        XDirection = DRAND32()*2*CP_PI

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = Sphere_Central(ICase,1) + VectorLen*sin(ZDirection)*cos(XDirection)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = Sphere_Central(ICase,2) + VectorLen*sin(ZDirection)*sin(XDirection)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = Sphere_Central(ICase,3) + VectorLen*cos(ZDirection)

                        DO I = 1,3
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                            else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                            end if

                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GE. Host_Boxes%BOXBOUNDARY(I,1) .AND. &
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LE. Host_Boxes%BOXBOUNDARY(I,2)) then
                                ExitCount = ExitCount + 1
                            end if
                        END DO

                        if(ExitCount .EQ. 3) then
                            exit
                        end if

                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)


                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1
                END DO

                DO  IIC = 1,NVACCluster

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = 1
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = p_ACTIVEFREE_STATU
                    DO While(.true.)
                        ExitCount = 0

                        VectorLen = Sphere_Radius(ICase)*DRAND32()
                        ZDirection = DRAND32()*CP_PI
                        XDirection = DRAND32()*2*CP_PI

                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) = Sphere_Central(ICase,1) + VectorLen*sin(ZDirection)*cos(XDirection)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) = Sphere_Central(ICase,2) + VectorLen*sin(ZDirection)*sin(XDirection)
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) = Sphere_Central(ICase,3) + VectorLen*cos(ZDirection)

                        DO I = 1,3
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                            else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParamList%theSimulationCtrlParam%PERIOD(I) .GT. 0) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) - Host_Boxes%BOXSIZE(I)
                            end if

                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GE. Host_Boxes%BOXBOUNDARY(I,1) .AND. &
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LE. Host_Boxes%BOXBOUNDARY(I,2)) then
                                ExitCount = ExitCount + 1
                            end if
                        END DO

                        if(ExitCount .EQ. 3) then
                            exit
                        end if

                    END DO

                    TheDiffusorValue = Host_Boxes%m_DiffusorTypesMap%Get(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC))

                    !-- In Current application, the simple init distribution is only considered in free matrix, if you want to init the clusters in GB---
                    !---you should init the distribution by external file---
                    select case(TheDiffusorValue%ECRValueType_Free)
                        case(p_ECR_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR_Free
                        case default
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = Cal_ECR_ModelDataBase(TheDiffusorValue%ECRValueType_Free,                          &
                                                                                                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA,&
                                                                                                   Host_SimuCtrlParamList%theSimulationCtrlParam%TKB,                                      &
                                                                                                   Host_Boxes%LatticeLength)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType_Free)
                        case(p_DiffuseCoefficient_ByValue)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Free_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = m_FREESURDIFPRE*(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        case(p_DiffuseCoefficient_BySIACluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = (sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)**(-TheDiffusorValue%PreFactorParameter_Free))* &
                                                                                        TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByVcCluster)
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = ((TheDiffusorValue%PreFactorParameter_Free)**(1-sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)))* &
                                                                                    TheDiffusorValue%PreFactor_Free*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy_Free/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)
                    end select

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection = TheDiffusorValue%DiffuseDirection
                    if(TheDiffusorValue%DiffuseDirectionType .eq. p_DiffuseDirection_OneDim) then
                        DO TheDim = 1,3
                            Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseDirection(TheDim)*sign(1.D0,DRAND32() - 0.5D0)
                        END DO
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff*1.D0/3.D0       ! All Diffusion coeff would be changed to 3-D formation
                    end if


                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffuseRotateCoeff = TheDiffusorValue%DiffuseRotateAttempFrequence*exp(-C_EV2ERG*TheDiffusorValue%DiffuseRotateEnerg/Host_SimuCtrlParamList%theSimulationCtrlParam%TKB)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParamList%theSimulationCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = ICase
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

            END DO

        END DO

        call Host_Boxes%PutoutCfg(Host_SimuCtrlParamList%theSimulationCtrlParam,Record)

        call DeAllocateArray_Host(CellCentralPos,"CellCentralPos")

        call DeAllocateArray_Host(Sphere_Central,"Sphere_Central")
        call DeAllocateArray_Host(Sphere_Radius,"Sphere_Radius")

        call Host_Boxes%Clean()

        return
    end subroutine Generate_Cascade_Locally_CentUniform

end module MC_GenerateCascadeBox

program Main_MC_GenerateCascadeBox
    use MC_GenerateCascadeBox
    implicit none
    integer::arg_Num
    integer::CascadeGenWay
    character*1000::ARG
    character*1000::SampleFile
    integer::CascadeNum
    integer::NClusterEachCascade
    character*1000::CascadeControlFile
    integer::hFile
    integer::LINE
    character*1000::STR
    character*1000::STRTMP(10)
    integer::FinededCascadeGenWay
    character*32::KEYWORD
    integer::N
    !---Body---
    arg_Num = Command_Argument_count()

    if(arg_Num .LT. 2) then
        write(*,*) "MCPSCUERROR: You must special the sample file, cascade control file"
        pause
        stop
    end if

    call Get_Command_Argument(0,ARG)

    call Get_Command_Argument(1,ARG)
    Read(ARG,fmt="(A256)") SampleFile
    write(*,*) "The sample file is: ",SampleFile

    call Get_Command_Argument(2,ARG)
    Read(ARG,fmt="(A256)") CascadeControlFile
    write(*,*) "The cascade control file is: ",CascadeControlFile

    hFile = OpenExistedFile(CascadeControlFile)
    LINE = 0

    FinededCascadeGenWay = 0
    Do While(.not. GETINPUTSTRLINE_New(hFile,STR,LINE,"!"))
        LINE = LINE + 1
        STR = adjustl(STR)
        call RemoveComments(STR,"!")

        if(LENTRIM(STR) .LE. 0) then
            cycle
        end if

        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&GENERATEWAY")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                if(N .LE. 0) then
                    write(*,*) "MCPSCUERROR: You must special the cascade generate way."
                    pause
                    stop
                end if
                CascadeGenWay = ISTR(STRTMP(1))
                FinededCascadeGenWay = 1
        end select
    End Do

    if(FinededCascadeGenWay .LE. 0) then
        write(*,*) "You must special the cascade generate way."
        write(*,*) "By the way: '&GENERATEWAY  the cascade generate way = "
        write(*,*)  "0 by Locally ,center uniform way; 1 by from MD database,locally;2 by from MD database, uniform)"
        pause
        stop
    end if

    select case(CascadeGenWay)
        case(CascadeGenWay_ByCentUniform_Locally)
            write(*,*) "The cascade generate way is by Locally ,center uniform way"

            call Generate_Cascade_Locally_CentUniform(hFile)

        case(CascadeGenWay_ByMDDataBase_Locally_Resample)
            write(*,*) "The cascade generate way is by MD database,locally, resample"

            call Generate_Cascade_Locally_FormMDDataBase_Resample(hFile)

        case(CascadeGenWay_ByMDDataBase_Uniform_Resample)
            write(*,*) "The cascade generate way is by MD database, uniform, resample"

            call Generate_Cascade_Uniform_FormMDDataBase_Resample(hFile)

        case(CascadeGenWay_ByMDDataBase_Locally_Directly)
            write(*,*) "The cascade generate way is by MD database,locally, directly"

            call Generate_Cascade_Locally_FormMDDataBase_Directly(hFile)

        case default
            write(*,*) "MCPSCUERROR: Unknown way to generate cascade(0 by uniform way, 1 by from MD database)"
            write(*,*) CascadeGenWay
            pause
            stop
    end select

    close(hFile)

end program Main_MC_GenerateCascadeBox

