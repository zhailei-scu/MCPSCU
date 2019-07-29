module MC_GenerateCascadeBox
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    use RAND32_MODULE
    use RAND32SEEDLIB_MODULE

    implicit none

    integer,parameter::CascadeGenWay_ByUniform = 0
    integer,parameter::CascadeGenWay_ByMDDataBase = 1

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
        integer::NSIAClusterEachBox_AVE = 0
        integer::NVACClusterEachBox_AVE = 0
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
    integer,parameter::VACIndex_MD = 4

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

        this%NSIAClusterEachBox_AVE = others%NSIAClusterEachBox_AVE
        this%NVACClusterEachBox_AVE = others%NVACClusterEachBox_AVE

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

        this%NSIAClusterEachBox_AVE = 0
        this%NVACClusterEachBox_AVE = 0

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
    character*256,intent(in)::pathIn
    integer,intent(in)::Index_StartBox
    integer,intent(in)::Index_EndBox
    integer,intent(in)::Index_SIAConfig
    integer,intent(in)::Index_VACConfig
    type(MDStatistic)::TheMDStatistic
    !---Local Vars---
    character*256::OutFolder
    character*256::pathOutSIA
    integer::hFileOutSIA
    character*256::pathOutVAC
    integer::hFileOutVAC
    character*256::pathOutSIAToVAC
    integer::hFileOutSIAToVAC
    character*256::pathOutMIX
    integer::hFileOutMIX
    integer::IBox
    character*256::C_IBOX
    character*256::C_ICFGSIA
    character*256::C_ICFGVAC
    character*256::fileName
    logical::exits
    integer::RemindZeroNum
    integer::I
    character*256::STR
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
        call ReadOnConifg(fileName,SIAIndex_MD,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

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
            write(hFileOutSIA,fmt="(3(I14,1x),4(1PE14.6,1x),I14)") IBox,Index_SIAConfig,IC,ClustersArraySIA(IC)%POS,Distance,NAtomEachClusterSIA(IC)

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
        call ReadOnConifg(fileName,VACIndex_MD,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

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
            write(hFileOutVAC,fmt="(3(I14,1x),4(1PE14.6,1x),I14)") IBox,Index_VACConfig,IC,ClustersArrayVAC(IC)%POS,Distance,NAtomEachClusterVAC(IC)

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
            write(hFileOutMIX,fmt="(3(I14,1x),4(1PE14.6,1x),I14)") IBox,Index_SIAConfig,IC,ClustersArraySIA(IC)%POS,Distance,NAtomEachClusterSIA(IC)

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
            write(hFileOutMIX,fmt="(3(I14,1x),4(1PE14.6,1x),I14)") IBox,Index_VACConfig,IC,ClustersArrayVAC(IC)%POS,Distance,NAtomEachClusterVAC(IC)

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

    TheMDStatistic%NSIAClusterEachBox_AVE = 0
    TheMDStatistic%NVACClusterEachBox_AVE = 0
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
        call ReadOnConifg(fileName,SIAIndex_MD,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

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

        TheMDStatistic%NSIAClusterEachBox_AVE = TheMDStatistic%NSIAClusterEachBox_AVE + NSIAClusterEachBox

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg(fileName,VACIndex_MD,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)



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

        TheMDStatistic%NVACClusterEachBox_AVE = TheMDStatistic%NVACClusterEachBox_AVE + NVACClusterEachBox

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


    write(hFileOutSIA,fmt="(A,1x,1PE14.6)") "&MINDISTANCE_ToCent_SIA",TheMDStatistic%MinDistanceSIA_ToCent
    write(hFileOutVAC,fmt="(A,1x,1PE14.6)") "&MINDISTANCE_ToCent_VAC",TheMDStatistic%MinDistanceVAC_ToCent
    write(hFileOutMIX,fmt="(A,1x,1PE14.6)") "&MINDISTANCE_ToCent_MIX",TheMDStatistic%MinDistanceMIX_ToCent
    write(hFileOutSIA,fmt="(A,1x,1PE14.6)") "&MAXDISTANCE_ToCent_SIA",TheMDStatistic%MaxDistanceSIA_ToCent
    write(hFileOutVAC,fmt="(A,1x,1PE14.6)") "&MAXDISTANCE_ToCent_VAC",TheMDStatistic%MaxDistanceVAC_ToCent
    write(hFileOutMIX,fmt="(A,1x,1PE14.6)") "&MAXDISTANCE_ToCent_MIX",TheMDStatistic%MaxDistanceMIX_ToCent

    write(hFileOutSIA,fmt="(A,1x,1PE14.6)") "&MinDistanceSIA_BetweenCluster",TheMDStatistic%MinDistanceSIA_BetweenCluster
    write(hFileOutVAC,fmt="(A,1x,1PE14.6)") "&MinDistanceVAC_BetweenCluster",TheMDStatistic%MinDistanceVAC_BetweenCluster
    write(hFileOutMIX,fmt="(A,1x,1PE14.6)") "&MinDistanceMIX_BetweenCluster",TheMDStatistic%MinDistanceMIX_BetweenCluster
    write(hFileOutSIAToVAC,fmt="(A,1x,1PE14.6)") "&MinDistanceSIAToVAC_BetweenCluster",TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster
    write(hFileOutSIA,fmt="(A,1x,1PE14.6)") "&MaxDistanceSIA_BetweenCluster",TheMDStatistic%MaxDistanceSIA_BetweenCluster
    write(hFileOutVAC,fmt="(A,1x,1PE14.6)") "&MaxDistanceVAC_BetweenCluster",TheMDStatistic%MaxDistanceVAC_BetweenCluster
    write(hFileOutMIX,fmt="(A,1x,1PE14.6)") "&MaxDistanceMIX_BetweenCluster",TheMDStatistic%MaxDistanceMIX_BetweenCluster
    write(hFileOutSIAToVAC,fmt="(A,1x,1PE14.6)") "&MaxDistanceSIAToVAC_BetweenCluster",TheMDStatistic%MaxDistanceSIAToVAC_BetweenCluster


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

        write(hFileOutSIA,fmt="(4(1PE14.6,1x))") TheMDStatistic%BinSIA_ToCenterArray(I), &
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

        write(hFileOutVAC,fmt="(4(1PE14.6,1x))") TheMDStatistic%BinVAC_ToCenterArray(I), &
                                                TheMDStatistic%ToCent_DistVAC(I),&
                                                TheMDStatistic%BinVAC_BetweenClusterArray(I),&
                                                TheMDStatistic%ClusterGap_DistVAC(I)
    END DO

    TheMDStatistic%TotalCount_GapSIAToVAC = sum(TheMDStatistic%ClusterGap_SIAToVAC)
    TheMDStatistic%ClusterGap_SIAToVAC = TheMDStatistic%ClusterGap_SIAToVAC/TheMDStatistic%TotalCount_GapSIAToVAC
    DO I = 1,BinNum
        TheMDStatistic%BinSIAToVAC_BetweenClusterArray(I) = TheMDStatistic%BinWidthSIAToVAC_BetweenCluster*(I - 0.5D0) + TheMDStatistic%MinDistanceSIAToVAC_BetweenCluster

        write(hFileOutSIAToVAC,fmt="(2(1PE14.6,1x))") TheMDStatistic%BinSIAToVAC_BetweenClusterArray(I), &
                                                     TheMDStatistic%ClusterGap_SIAToVAC(I)
    END DO

    TheMDStatistic%TotalCountMIX_ToCent = sum(TheMDStatistic%ToCent_DistMIX)
    TheMDStatistic%TotalCountMIX_GapCToC = sum(TheMDStatistic%ClusterGap_DistMIX)
    TheMDStatistic%ToCent_DistMIX = TheMDStatistic%ToCent_DistMIX/TheMDStatistic%TotalCountMIX_ToCent
    TheMDStatistic%ClusterGap_DistMIX = TheMDStatistic%ClusterGap_DistMIX/TheMDStatistic%TotalCountMIX_GapCToC
    DO I = 1,BinNum
        TheMDStatistic%BinMIX_ToCenterArray(I) = TheMDStatistic%BinWidthMIX_ToCenter*(I - 0.5D0) + TheMDStatistic%MinDistanceMIX_ToCent
        TheMDStatistic%BinMIX_BetweenClusterArray(I) = TheMDStatistic%BinWidthMIX_BetweenCluster*(I - 0.5D0) + TheMDStatistic%MinDistanceMIX_BetweenCluster

        write(hFileOutMIX,fmt="(4(1PE14.6,1x))") TheMDStatistic%BinMIX_ToCenterArray(I), &
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

    TheMDStatistic%NSIAClusterEachBox_AVE = ceiling(dble(TheMDStatistic%NSIAClusterEachBox_AVE)/dble(NBOX))
    TheMDStatistic%NVACClusterEachBox_AVE = ceiling(dble(TheMDStatistic%NVACClusterEachBox_AVE)/dble(NBOX))
    write(hFileOutSIA,fmt="(A,1x,I10)") "&NSIACLUSTER",TheMDStatistic%NSIAClusterEachBox_AVE
    write(hFileOutVAC,fmt="(A,1x,I10)") "&NVACCLUSTER",TheMDStatistic%NVACClusterEachBox_AVE
    write(hFileOutMIX,fmt="(A,1x,I10)") "&NSIACLUSTER",TheMDStatistic%NSIAClusterEachBox_AVE
    write(hFileOutMIX,fmt="(A,1x,I10)") "&NVACCLUSTER",TheMDStatistic%NVACClusterEachBox_AVE


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
        write(hFileOutSIA,fmt="(I14,1x,1PE14.6,1x)") TheMDStatistic%BinSIA_NAtomArray(IC-TheMDStatistic%MinSIANumEachCluster+1),TheMDStatistic%NAtomSIA(IC-TheMDStatistic%MinSIANumEachCluster+1)
    END DO

    TheMDStatistic%TotalCountVAC_NAtom = sum(TheMDStatistic%NAtomVAC)
    TheMDStatistic%NAtomVAC = TheMDStatistic%NAtomVAC/TheMDStatistic%TotalCountVAC_NAtom
    DO IC = TheMDStatistic%MinVACNumEachCluster,TheMDStatistic%MaxVACNumEachCluster
        TheMDStatistic%BinVAC_NAtomArray(IC-TheMDStatistic%MinVACNumEachCluster+1) = IC
        write(hFileOutVAC,fmt="(I14,1x,1PE14.6,1x)") TheMDStatistic%BinVAC_NAtomArray(IC-TheMDStatistic%MinVACNumEachCluster+1),TheMDStatistic%NAtomVAC(IC-TheMDStatistic%MinVACNumEachCluster+1)
    END DO

    TheMDStatistic%TotalCountMIX_NAtom = sum(TheMDStatistic%NAtomMIX)
    TheMDStatistic%NAtomMIX = TheMDStatistic%NAtomMIX/TheMDStatistic%TotalCountMIX_NAtom
    DO IC = TheMDStatistic%MinMIXNumEachCluster,TheMDStatistic%MaxMIXNumEachCluster
        TheMDStatistic%BinMIX_NAtomArray(IC-TheMDStatistic%MinMIXNumEachCluster+1) = IC
        write(hFileOutMIX,fmt="(I14,1x,1PE14.6,1x)") TheMDStatistic%BinMIX_NAtomArray(IC-TheMDStatistic%MinMIXNumEachCluster+1),TheMDStatistic%NAtomMIX(IC-TheMDStatistic%MinMIXNumEachCluster+1)
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

  !*************************************************************************
  subroutine ReadOnConifg(fileName,SpecialAtomsType,ClustersArray,NAtomEachCluster,NClusterEachBox)
    !---Dummy Vars---
    character*256,intent(in)::fileName
    integer,intent(in)::SpecialAtomsType
    type(ClusterAtom),dimension(:),allocatable::ClustersArray
    integer,dimension(:),allocatable::NAtomEachCluster
    integer::NClusterEachBox
    !---Local Vars---
    integer::hFile
    character*256::STR
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

        if(AtomType .eq. SpecialAtomsType) then

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

        if(AtomType .eq. SpecialAtomsType) then

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
  end subroutine ReadOnConifg

    subroutine ResloveMDDataBaseControlFile(MDDataBaseCtrlFile,TheMDStatistic)
        !---Dummy Vars---
        character*256,intent(in)::MDDataBaseCtrlFile
        type(MDStatistic)::TheMDStatistic
        !---Local Vars---
        integer::hFile
        integer::LINE
        character*256::STR
        character*30::KEYWORD
        character*200::STRTMP(10)
        integer::N
        character*256::MDDataBasePath
        integer::Index_StartBox
        integer::Index_EndBox
        integer::Index_SIAConfig
        integer::Index_VACConfig
        !---Body---

        hFile = OpenExistedFile(MDDataBaseCtrlFile)
        LINE = 0

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

                case("&INDEX_STARTBOX")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special start box index."
                        pause
                        stop
                    end if
                    Index_StartBox = ISTR(STRTMP(1))
                    write(*,*) Index_StartBox

                case("&INDEX_ENDBOX")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special end box index."
                        pause
                        stop
                    end if
                    Index_EndBox = ISTR(STRTMP(1))
                    write(*,*) Index_EndBox

                case("&INDEX_SIACONFIG")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special SIA configuration index."
                        pause
                        stop
                    end if
                    Index_SIAConfig = ISTR(STRTMP(1))
                    write(*,*) Index_SIAConfig

                case("&INDEX_VACCONFIG")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LE. 0) then
                        write(*,*) "MCPSCUERROR: You must special VAC configuration index."
                        pause
                        stop
                    end if
                    Index_VACConfig = ISTR(STRTMP(1))
                    write(*,*) Index_VACConfig

                case default
                    write(*,*) "MCPSCUERROR: Unknown keyword: ",KEYWORD(1:LENTRIM(KEYWORD))
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
            end select
        End Do

        call CascadeDataBase_ANALYSIS_SIAANDVAC(MDDataBasePath,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig,TheMDStatistic)

        return
    end subroutine

    !***************************************************************
    subroutine Generate_Cascade_FormMDDataBase(MDDataBaseCtrlFile,CascadeNum)
        !---Dummy Vars---
        character*256,intent(in)::MDDataBaseCtrlFile
        integer,intent(in)::CascadeNum
        !---Local Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        character*256::OutFolder
        type(MDStatistic)::TheMDStatistic
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
        !-----------Body--------------

        call ResloveMDDataBaseControlFile(MDDataBaseCtrlFile,TheMDStatistic)


        processid = 0

        call AllocateArray_Host(Sphere_Central,CascadeNum,3,"Sphere_Central")

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(Host_SimuCtrlParam,Host_Boxes)


        ISEED0 = Host_SimuCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,Host_SimuCtrlParam,Host_Boxes)

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParam%OutFilePath))//"CascadeBox/")

        Host_SimuCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParam)

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParam,CascadeNum*(TheMDStatistic%NSIAClusterEachBox_AVE + TheMDStatistic%NVACClusterEachBox_AVE))

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        !---ReDraw the cells-----
        CellNum_OneDim = floor(CascadeNum**C_UTH + 0.5D0)
        CellNum = CellNum_OneDim**3

        call AllocateArray_Host(CellCentralPos,CellNum,3,"CellCentralPos")

        ICell = 0
        DO I = 1,CellNum_OneDim
            DO J = 1,CellNum_OneDim
                DO K = 1,CellNum_OneDim
                    ICell = ICell + 1
                    CellCentralPos(ICell,1) = Host_Boxes%BOXBOUNDARY(1,1) + (I + 0.5D0)*Host_Boxes%BOXSIZE(1)/CellNum_OneDim
                    CellCentralPos(ICell,2) = Host_Boxes%BOXBOUNDARY(2,1) + (J + 0.5D0)*Host_Boxes%BOXSIZE(2)/CellNum_OneDim
                    CellCentralPos(ICell,3) = Host_Boxes%BOXBOUNDARY(3,1) + (K + 0.5D0)*Host_Boxes%BOXSIZE(3)/CellNum_OneDim
                END DO
            END DO
        END DO
        !------------------------

        IC = 0
        DO IBox = 1,Host_SimuCtrlParam%MultiBox

            DO ICase = 1,CascadeNum

                write(*,*) "ICase",ICase
                pause

                Sphere_Central(ICase,:) = CellCentralPos(ICase,:)

                DO IIC = 1,TheMDStatistic%NSIAClusterEachBox_AVE
                    write(*,*) "IC",IC

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Accum = 0.D0
                    RandNum = DRAND32()
                    DO IBin = 1,size(TheMDStatistic%NAtomSIA)
                        Accum = Accum + TheMDStatistic%NAtomSIA(IBin)

                        if(Accum .GE. RandNum) then
                            TheBin = IBin
                            exit
                        end if
                    END DO
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA = TheMDStatistic%BinSIA_NAtomArray(TheBin)

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
                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                                else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
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
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(TheMDStatistic%NSIAClusterEachBox_AVE + TheMDStatistic%NVACClusterEachBox_AVE),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .GT. Gap) then
                                    GapCondition = 1

                                    write(*,*) "DistanceBetweenSIA",Distance
                                    write(*,*) "GapBetweenSIA",Gap

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
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(TheMDStatistic%NSIAClusterEachBox_AVE + TheMDStatistic%NVACClusterEachBox_AVE),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA .GT. 0) then

                                Distance = DSQRT(sum((Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS - Host_Boxes%m_ClustersInfo_CPU%m_Clusters(JC)%m_POS)**2))

                                if(Distance .GT. Gap) then
                                    GapCondition = 1

                                    write(*,*) "DistanceSIAToVac",Distance
                                    write(*,*) "GapSIAToVac",Gap

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
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = IC - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1

                END DO

                DO IIC = 1,TheMDStatistic%NVACClusterEachBox_AVE

                    IC = IC + 1
                    call Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%Clean_Cluster()

                    Accum = 0.D0
                    RandNum = DRAND32()
                    DO IBin = 1,size(TheMDStatistic%NAtomVAC)
                        Accum = Accum + TheMDStatistic%NAtomVAC(IBin)

                        if(Accum .GE. RandNum) then
                            TheBin = IBin
                            exit
                        end if
                    END DO
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacancyIndex)%m_NA = TheMDStatistic%BinVAC_NAtomArray(TheBin)

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
                                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                                else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
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
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(TheMDStatistic%NSIAClusterEachBox_AVE + TheMDStatistic%NVACClusterEachBox_AVE),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
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
                        DO JC = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + (ICase - 1)*(TheMDStatistic%NSIAClusterEachBox_AVE + TheMDStatistic%NVACClusterEachBox_AVE),Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)
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
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = IC - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
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

        call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

        call DeAllocateArray_Host(CellCentralPos,"CellCentralPos")

        call DeAllocateArray_Host(Sphere_Central,"Sphere_Central")

        call Host_Boxes%Clean()

        return
    end subroutine Generate_Cascade_FormMDDataBase

    !***************************************************************
    subroutine Generate_Cascade_Uniform(CascadeNum,ClusterNumOneCase)
        !---Dummy Vars---
        integer,intent(in)::CascadeNum
        integer,intent(in)::ClusterNumOneCase
        !---Local Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        type(MigCoalClusterRecord)::Record
        character*256::OutFolder
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
        !-----------Body--------------
        processid = 0

        call AllocateArray_Host(Sphere_Central,CascadeNum,3,"Sphere_Central")
        call AllocateArray_Host(Sphere_Radius,CascadeNum,"Sphere_Radius")

        !*********Create/Open log file********************
        call OpenLogFile(m_hFILELOG)

        !********Load Global vars from input file**************
        call Initialize_Global_Variables(Host_SimuCtrlParam,Host_Boxes)


        ISEED0 = Host_SimuCtrlParam%RANDSEED(1)
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        ISEED0 = ISEED0 + processid - 1
        call GetSeed_RAND32SEEDLIB(ISEED0,ISEED(1),ISEED(2))
        call DRAND32_PUTSEED(ISEED)

        call Print_Global_Variables(6,Host_SimuCtrlParam,Host_Boxes)

        OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParam%OutFilePath))//"CascadeBox/")

        Host_SimuCtrlParam%OutFilePath = trim(adjustl(OutFolder))

        call Host_Boxes%m_ClustersInfo_CPU%Clean()

        call Host_Boxes%InitSimulationBox(Host_SimuCtrlParam)

        call Host_Boxes%ExpandClustersInfor_CPU(Host_SimuCtrlParam,2*CascadeNum*ClusterNumOneCase)

        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacancyIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        Sphere_Radius = 80*Host_Boxes%LatticeLength


        !---ReDraw the cells-----
        CellNum_OneDim = floor(CascadeNum**C_UTH + 0.5D0)
        CellNum = CellNum_OneDim**3

        call AllocateArray_Host(CellCentralPos,CellNum,3,"CellCentralPos")

        ICell = 0
        DO I = 1,CellNum_OneDim
            DO J = 1,CellNum_OneDim
                DO K = 1,CellNum_OneDim
                    ICell = ICell + 1
                    CellCentralPos(ICell,1) = Host_Boxes%BOXBOUNDARY(1,1) + (I + 0.5D0)*Host_Boxes%BOXSIZE(1)/CellNum_OneDim
                    CellCentralPos(ICell,2) = Host_Boxes%BOXBOUNDARY(2,1) + (J + 0.5D0)*Host_Boxes%BOXSIZE(2)/CellNum_OneDim
                    CellCentralPos(ICell,3) = Host_Boxes%BOXBOUNDARY(3,1) + (K + 0.5D0)*Host_Boxes%BOXSIZE(3)/CellNum_OneDim
                END DO
            END DO
        END DO
        !------------------------

        IC = 0
        DO IBox = 1,Host_SimuCtrlParam%MultiBox
            DO ICase = 1,CascadeNum
                Sphere_Central(ICase,:) = CellCentralPos(ICase,:)

                DO IIC = 1,ClusterNumOneCase

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
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                            else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
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
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = IC - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) = 0

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + 1
                    Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = Host_Boxes%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + 1

                    Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2) + 1
                    Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) = Host_Boxes%m_BoxesInfo%SEExpdIndexBox(IBox,2) + 1


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
                            if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .LT. Host_Boxes%BOXBOUNDARY(I,1) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
                                Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) + Host_Boxes%BOXSIZE(I)
                            else if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) .GT. Host_Boxes%BOXBOUNDARY(I,2) .AND. Host_SimuCtrlParam%PERIOD(I) .GT. 0) then
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
                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = Host_Boxes%m_GrainBoundary%GrainBelongsTo(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS,Host_Boxes%HBOXSIZE,Host_Boxes%BOXSIZE,Host_SimuCtrlParam)

                    Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) = IC - Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
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

        call Host_Boxes%PutoutCfg(Host_SimuCtrlParam,Record)

        call DeAllocateArray_Host(CellCentralPos,"CellCentralPos")

        call DeAllocateArray_Host(Sphere_Central,"Sphere_Central")
        call DeAllocateArray_Host(Sphere_Radius,"Sphere_Radius")

        call Host_Boxes%Clean()

        return
    end subroutine Generate_Cascade_Uniform

end module MC_GenerateCascadeBox

program Main_MC_GenerateCascadeBox
    use MC_GenerateCascadeBox
    implicit none
    integer::arg_Num
    integer::CascadeGenWay
    character*256::ARG
    character*256::SampleFile
    integer::CascadeNum
    integer::NClusterEachCascade
    character*256::MDDataBaseControlFile
    !---Body---
    arg_Num = Command_Argument_count()

    if(arg_Num .LT. 2) then
        write(*,*) "MCPSCUERROR: You must special the sample file, cascade generate way (0 by uniform way, 1 by from MD database)"
        pause
        stop
    end if

    call Get_Command_Argument(0,ARG)

    call Get_Command_Argument(1,ARG)
    Read(ARG,fmt="(A256)") SampleFile
    write(*,*) "The sample file is: ",SampleFile

    call Get_Command_Argument(2,ARG)
    Read(ARG,*) CascadeGenWay

    select case(CascadeGenWay)
        case(CascadeGenWay_ByUniform)

            write(*,*) "The cascade generate way is by uniform way"

            if(arg_Num .LT. 4) then
                write(*,*) "MCPSCUERROR: You must special 1: the sample file"
                write(*,*) "2: cascade generate way (0 by uniform way, 1 by from MD database)"
                write(*,*) "3: cascade number in each box"
                write(*,*) "4: cluster number in each cascade"
                pause
                stop
            end if

            call Get_Command_Argument(3,ARG)
            Read(ARG,*) CascadeNum
            write(*,*) "The cascade number in each box is: ",CascadeNum

            call Get_Command_Argument(4,ARG)
            Read(ARG,*) NClusterEachCascade
            write(*,*) "The cluster number in each cascade is: ",NClusterEachCascade

            call Generate_Cascade_Uniform(CascadeNum,NClusterEachCascade)

        case(CascadeGenWay_ByMDDataBase)
            write(*,*) "The cascade generate way is by MD DataBase way"

            if(arg_Num .LT. 4) then
                write(*,*) "MCPSCUERROR: You must special 1: the sample file"
                write(*,*) "2: cascade generate way (0 by uniform way, 1 by from MD database)"
                write(*,*) "3: MD DataBase control file"
                write(*,*) "4: Cascade number each box."
                pause
                stop
            end if

            call Get_Command_Argument(3,ARG)
            Read(ARG,fmt="(A256)") MDDataBaseControlFile
            write(*,*) "The MD DataBase path is: ",MDDataBaseControlFile

            call Get_Command_Argument(4,ARG)
            Read(ARG,*) CascadeNum
            write(*,*) "The cascade number in each box is: ",CascadeNum

            call Generate_Cascade_FormMDDataBase(MDDataBaseControlFile,CascadeNum)

        case default
            write(*,*) "MCPSCUERROR: Unknown way to generate cascade(0 by uniform way, 1 by from MD database)"
            write(*,*) CascadeGenWay
            pause
            stop
    end select


end program Main_MC_GenerateCascadeBox
