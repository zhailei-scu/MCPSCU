module CascadeDataBase_ANALYSIS
  use MCLIB_CONSTANTS
  use MiniUtilities, only:EXTRACT_NUMB,EXTRACT_SUBSTR,GETINPUTSTRLINE, GETKEYWORD, UPCASE, ISTR, DRSTR
  use MCLIB_UTILITIES
  implicit none

  type,public::ClusterAtom
    real(kind=KINDDF)::POS(3)
  end type

  integer,parameter::MaxNumLen = 4

  integer,parameter::BinNum = 100

  integer,parameter::SIAIndex = 1
  integer,parameter::VACIndex = 4

  real(kind=KINDDF),parameter::p_RoundOff = 1.D-8
  contains

  !***********************************************************************
  subroutine CascadeDataBase_ANALYSIS_SIAANDVAC(pathIn,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig)
    !---This routine only analysis SIA or VAC
    implicit none
    !---Dummy Vars---
    character*256,intent(in)::pathIn
    integer,intent(in)::Index_StartBox
    integer,intent(in)::Index_EndBox
    integer,intent(in)::Index_SIAConfig
    integer,intent(in)::Index_VACConfig
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
    integer::NSIAClusterEachBox_AVE
    integer::NVACClusterEachBox_AVE
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
    real(kind=KINDDF)::MaxDistanceSIA_ToCent
    real(kind=KINDDF)::MaxDistanceVAC_ToCent
    real(kind=KINDDF)::MaxDistanceMIX_ToCent
    real(kind=KINDDF)::MaxDistanceSIA_BetweenCluster
    real(kind=KINDDF)::MaxDistanceVAC_BetweenCluster
    real(kind=KINDDF)::MaxDistanceMIX_BetweenCluster
    real(kind=KINDDF)::MaxDistanceSIAToVAC_BetweenCluster
    real(kind=KINDDF)::MinDistanceSIA_ToCent
    real(kind=KINDDF)::MinDistanceVAC_ToCent
    real(kind=KINDDF)::MinDistanceMIX_ToCent
    real(kind=KINDDF)::MinDistanceSIA_BetweenCluster
    real(kind=KINDDF)::MinDistanceVAC_BetweenCluster
    real(kind=KINDDF)::MinDistanceMIX_BetweenCluster
    real(kind=KINDDF)::MinDistanceSIAToVAC_BetweenCluster
    real(kind=KINDDF)::BinWidthSIA_ToCenter
    real(kind=KINDDF)::BinWidthVAC_ToCenter
    real(kind=KINDDF)::BinWidthMIX_ToCenter
    real(kind=KINDDF)::BinWidthSIA_BetweenCluster
    real(kind=KINDDF)::BinWidthVAC_BetweenCluster
    real(kind=KINDDF)::BinWidthMIX_BetweenCluster
    real(kind=KINDDF)::BinWidthSIAToVAC_BetweenCluster
    integer,dimension(:),allocatable::ToCent_DistSIA
    integer,dimension(:),allocatable::ToCent_DistVAC
    integer,dimension(:),allocatable::ToCent_DistMIX
    integer,dimension(:),allocatable::ClusterGap_DistSIA
    integer,dimension(:),allocatable::ClusterGap_DistVAC
    integer,dimension(:),allocatable::ClusterGap_DistMIX
    integer,dimension(:),allocatable::ClusterGap_SIAToVAC
    integer::TotalCountSIA_GapCToC
    integer::TotalCountVAC_GapCToC
    integer::TotalCount_GapSIAToVAC
    integer::TotalCountMIX_GapCToC
    integer::TotalCountSIA_ToCent
    integer::TotalCountVAC_ToCent
    integer::TotalCountMIX_ToCent
    integer::NBOX
    integer::BinID
    !---Body---

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

    MaxDistanceSIA_ToCent = -1.D0
    MaxDistanceVAC_ToCent = -1.D0
    MaxDistanceMIX_ToCent = -1.D0
    MaxDistanceSIA_BetweenCluster = -1.D0
    MaxDistanceVAC_BetweenCluster = -1.D0
    MaxDistanceMIX_BetweenCluster = -1.D0
    MaxDistanceSIAToVAC_BetweenCluster = -1.D0
    MinDistanceSIA_ToCent = 1.D32
    MinDistanceVAC_ToCent = 1.D32
    MinDistanceMIX_ToCent = 1.D32
    MinDistanceSIA_BetweenCluster = 1.D32
    MinDistanceVAC_BetweenCluster = 1.D32
    MinDistanceMIX_BetweenCluster = 1.D32
    MinDistanceSIAToVAC_BetweenCluster = 1.D32

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
        call ReadOnConifg(fileName,SIAIndex,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

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

            if(MaxDistanceSIA_ToCent .LE. Distance) then
                MaxDistanceSIA_ToCent = Distance
            end if

            if(MinDistanceSIA_ToCent .GE. Distance) then
                MinDistanceSIA_ToCent = Distance
            end if

            DO JC = IC+1,NSIAClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArraySIA(JC)%POS)**2))

                if(MaxDistanceSIA_BetweenCluster .LE. Distance) then
                    MaxDistanceSIA_BetweenCluster = Distance
                end if

                if(MinDistanceSIA_BetweenCluster .GE. Distance) then
                    MinDistanceSIA_BetweenCluster = Distance
                end if

            END DO
        END DO

        flush(hFileOutSIA)

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg(fileName,VACIndex,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

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

            if(MaxDistanceVAC_ToCent .LE. Distance) then
                MaxDistanceVAC_ToCent = Distance
            end if

            if(MinDistanceVAC_ToCent .GE. Distance) then
                MinDistanceVAC_ToCent = Distance
            end if

            DO JC = IC+1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - ClustersArrayVAC(JC)%POS)**2))

                if(MaxDistanceVAC_BetweenCluster .LE. Distance) then
                    MaxDistanceVAC_BetweenCluster = Distance
                end if

                if(MinDistanceVAC_BetweenCluster .GE. Distance) then
                    MinDistanceVAC_BetweenCluster = Distance
                end if
            END DO

        END DO

        flush(hFileOutVAC)

        !---MIX---
        if((NSIAClusterEachBox + NVACClusterEachBox) .GT. 0) then
            CentPosMIX = CentPosMIX/(NSIAClusterEachBox + NVACClusterEachBox)
        end if

        MaxDistanceMIX_BetweenCluster = max(MaxDistanceSIA_BetweenCluster,MaxDistanceVAC_BetweenCluster)
        MinDistanceMIX_BetweenCluster = min(MinDistanceSIA_BetweenCluster,MinDistanceVAC_BetweenCluster)

        DO IC = 1,NSIAClusterEachBox
            Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - CentPosMIX)**2))
            write(hFileOutMIX,fmt="(3(I14,1x),4(1PE14.6,1x),I14)") IBox,Index_SIAConfig,IC,ClustersArraySIA(IC)%POS,Distance,NAtomEachClusterSIA(IC)

            if(MaxDistanceMIX_ToCent .LE. Distance) then
                MaxDistanceMIX_ToCent = Distance
            end if

            if(MinDistanceMIX_ToCent .GE. Distance) then
                MinDistanceMIX_ToCent = Distance
            end if

            DO JC = 1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArrayVAC(JC)%POS)**2))

                if(MaxDistanceMIX_BetweenCluster .LE. Distance) then
                    MaxDistanceMIX_BetweenCluster = Distance
                end if

                if(MinDistanceMIX_BetweenCluster .GE. Distance) then
                    MinDistanceMIX_BetweenCluster = Distance
                end if

                if(MaxDistanceSIAToVAC_BetweenCluster .LE. Distance) then
                    MaxDistanceSIAToVAC_BetweenCluster = Distance
                end if

                if(MinDistanceSIAToVAC_BetweenCluster .GE. Distance) then
                    MinDistanceSIAToVAC_BetweenCluster = Distance
                end if

            END DO

        END DO

        DO IC = 1,NVACClusterEachBox
            Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - CentPosMIX)**2))
            write(hFileOutMIX,fmt="(3(I14,1x),4(1PE14.6,1x),I14)") IBox,Index_VACConfig,IC,ClustersArrayVAC(IC)%POS,Distance,NAtomEachClusterVAC(IC)

            if(MaxDistanceMIX_ToCent .LE. Distance) then
                MaxDistanceMIX_ToCent = Distance
            end if

            if(MinDistanceMIX_ToCent .GE. Distance) then
                MinDistanceMIX_ToCent = Distance
            end if
        END DO

        flush(hFileOutMIX)

    END DO

    close(hFileOutSIA)
    close(hFileOutVAC)
    close(hFileOutMIX)

    !-------------------Gap between clusters distribution------------------------
    BinWidthSIA_ToCenter = (MaxDistanceSIA_ToCent - MinDistanceSIA_ToCent)/BinNum
    BinWidthVAC_ToCenter = (MaxDistanceVAC_ToCent - MinDistanceVAC_ToCent)/BinNum
    BinWidthMIX_ToCenter = (MaxDistanceMIX_ToCent - MinDistanceMIX_ToCent)/BinNum

    BinWidthSIA_BetweenCluster = (MaxDistanceSIA_BetweenCluster - MinDistanceSIA_BetweenCluster)/BinNum
    BinWidthVAC_BetweenCluster = (MaxDistanceVAC_BetweenCluster - MinDistanceVAC_BetweenCluster)/BinNum
    BinWidthMIX_BetweenCluster = (MaxDistanceMIX_BetweenCluster - MinDistanceMIX_BetweenCluster)/BinNum
    BinWidthSIAToVAC_BetweenCluster = (MaxDistanceSIAToVAC_BetweenCluster - MinDistanceSIAToVAC_BetweenCluster)/BinNum

    pathOutSIA = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"SIA_Dist.analysis"
    pathOutVAC = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"VAC_Dist.analysis"
    pathOutSIAToVAC = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"Dist.SIAToVAc"
    pathOutMIX = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"MIX_Dist.analysis"

    hFileOutSIA = CreateNewFile(pathOutSIA)
    hFileOutVAC = CreateNewFile(pathOutVAC)
    hFileOutSIAToVAC = CreateNewFile(pathOutSIAToVAC)
    hFileOutMIX = CreateNewFile(pathOutMIX)

    if(allocated(ToCent_DistSIA)) deallocate(ToCent_DistSIA)
    allocate(ToCent_DistSIA(BinNum))
    ToCent_DistSIA = 0.D0

    if(allocated(ToCent_DistVAC)) deallocate(ToCent_DistVAC)
    allocate(ToCent_DistVAC(BinNum))
    ToCent_DistVAC = 0.D0

    if(allocated(ToCent_DistMIX)) deallocate(ToCent_DistMIX)
    allocate(ToCent_DistMIX(BinNum))
    ToCent_DistMIX = 0.D0

    if(allocated(ClusterGap_DistSIA)) deallocate(ClusterGap_DistSIA)
    allocate(ClusterGap_DistSIA(BinNum))
    ClusterGap_DistSIA = 0.D0

    if(allocated(ClusterGap_DistVAC)) deallocate(ClusterGap_DistVAC)
    allocate(ClusterGap_DistVAC(BinNum))
    ClusterGap_DistVAC = 0.D0

    if(allocated(ClusterGap_DistMIX)) deallocate(ClusterGap_DistMIX)
    allocate(ClusterGap_DistMIX(BinNum))
    ClusterGap_DistMIX = 0.D0

    if(allocated(ClusterGap_SIAToVAC)) deallocate(ClusterGap_SIAToVAC)
    allocate(ClusterGap_SIAToVAC(BinNum))
    ClusterGap_SIAToVAC = 0.D0

    NSIAClusterEachBox_AVE = 0
    NVACClusterEachBox_AVE = 0
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
        call ReadOnConifg(fileName,SIAIndex,ClustersArraySIA,NAtomEachClusterSIA,NSIAClusterEachBox)

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

            BinID = floor(DABS(Distance-MinDistanceSIA_ToCent-p_RoundOff)/BinWidthSIA_ToCenter) + 1
            ToCent_DistSIA(BinID) = ToCent_DistSIA(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) (Distance-MinDistanceSIA_ToCent)/BinWidthSIA_ToCenter
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        DO IC = 1,NSIAClusterEachBox
            DO JC = IC+1,NSIAClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArraySIA(JC)%POS)**2))
                BinID = floor(DABS(Distance-MinDistanceSIA_BetweenCluster-p_RoundOff)/BinWidthSIA_BetweenCluster) + 1
                ClusterGap_DistSIA(BinID) = ClusterGap_DistSIA(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if

                BinID = floor(DABS(Distance-MinDistanceMIX_BetweenCluster-p_RoundOff)/BinWidthMIX_BetweenCluster) + 1
                ClusterGap_DistMIX(BinID) = ClusterGap_DistMIX(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if
            END DO
        END DO

        NSIAClusterEachBox_AVE = NSIAClusterEachBox_AVE + NSIAClusterEachBox

        !---VAC---
        fileName = adjustl(trim(pathIn))//"/VAC/"//"P0000_"//adjustl(trim(C_IBOX))//"."//adjustl(trim(C_ICFGVAC))
        call ReadOnConifg(fileName,VACIndex,ClustersArrayVAC,NAtomEachClusterVAC,NVACClusterEachBox)

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
            BinID = floor(DABS(Distance-MinDistanceVAC_ToCent-p_RoundOff)/BinWidthVAC_ToCenter) + 1
            ToCent_DistVAC(BinID) = ToCent_DistVAC(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        DO IC = 1,NVACClusterEachBox
            DO JC = IC+1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - ClustersArrayVAC(JC)%POS)**2))
                BinID = floor(DABS(Distance-MinDistanceVAC_BetweenCluster-p_RoundOff)/BinWidthVAC_BetweenCluster) + 1
                ClusterGap_DistVAC(BinID) = ClusterGap_DistVAC(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if

                BinID = floor(DABS(Distance- MinDistanceMIX_BetweenCluster-p_RoundOff)/BinWidthMIX_BetweenCluster) + 1
                ClusterGap_DistMIX(BinID) = ClusterGap_DistMIX(BinID) + 1

                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if
            END DO
        END DO

        NVACClusterEachBox_AVE = NVACClusterEachBox_AVE + NVACClusterEachBox

        !---Gap Between SIA and VAC---
        DO IC = 1,NSIAClusterEachBox
            DO JC = 1,NVACClusterEachBox
                Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - ClustersArrayVAC(JC)%POS)**2))
                BinID = floor(DABS(Distance-MinDistanceSIAToVAC_BetweenCluster-p_RoundOff)/BinWidthSIAToVAC_BetweenCluster) + 1
                ClusterGap_SIAToVAC(BinID) = ClusterGap_SIAToVAC(BinID) + 1
                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if

                BinID = floor(DABS(Distance-MinDistanceMIX_BetweenCluster-p_RoundOff)/BinWidthMIX_BetweenCluster) + 1
                ClusterGap_DistMIX(BinID) = ClusterGap_DistMIX(BinID) + 1
                if(BinID .GT. BinNum) then
                    write(*,*) "Opps..., exceeded bin number ..."
                    write(*,*) BinID
                    write(*,*) BinNum
                end if
            END DO
        END DO

        !---MIX---
        if((NSIAClusterEachBox + NVACClusterEachBox) .GT. 0) then
            CentPosMIX = CentPosMIX/(NSIAClusterEachBox + NVACClusterEachBox)
        end if

        DO IC = 1,NSIAClusterEachBox
            Distance = DSQRT(sum((ClustersArraySIA(IC)%POS - CentPosMIX)**2))
            BinID = floor(DABS(Distance-MinDistanceMIX_ToCent-p_RoundOff)/BinWidthMIX_ToCenter) + 1
            ToCent_DistMIX(BinID) = ToCent_DistMIX(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        DO IC = 1,NVACClusterEachBox
            Distance = DSQRT(sum((ClustersArrayVAC(IC)%POS - CentPosMIX)**2))
            BinID = floor(DABS(Distance-MinDistanceMIX_ToCent-p_RoundOff)/BinWidthMIX_ToCenter) + 1
            ToCent_DistMIX(BinID) = ToCent_DistMIX(BinID) + 1

            if(BinID .GT. BinNum) then
                write(*,*) "Opps..., exceeded bin number ..."
                write(*,*) BinID
                write(*,*) BinNum
            end if
        END DO

        NBOX = NBOX + 1
    END DO

    write(hFileOutSIA,fmt="(A,1x,I10)") "&NSIACLUSTER",ceiling(dble(NSIAClusterEachBox_AVE)/dble(NBOX))
    write(hFileOutVAC,fmt="(A,1x,I10)") "&NVACCLUSTER",ceiling(dble(NVACClusterEachBox_AVE)/dble(NBOX))
    write(hFileOutMIX,fmt="(A,1x,I10)") "&NSIACLUSTER",ceiling(dble(NSIAClusterEachBox_AVE)/dble(NBOX))
    write(hFileOutMIX,fmt="(A,1x,I10)") "&NVACCLUSTER",ceiling(dble(NVACClusterEachBox_AVE)/dble(NBOX))

    write(hFileOutSIA,fmt="(10(A14,1x))") "!ToCent","Count","GapCToC","Count"
    write(hFileOutVAC,fmt="(10(A14,1x))") "!ToCent","Count","GapCToC","Count"
    write(hFileOutSIAToVAC,fmt="(10(A14,1x))") "!GapSIAToVac","Count"
    write(hFileOutMIX,fmt="(10(A14,1x))") "!ToCent","Count","GapCToC","Count"

    TotalCountSIA_GapCToC = sum(ClusterGap_DistSIA)
    TotalCountSIA_ToCent = sum(ToCent_DistSIA)
    DO I = 1,BinNum
        write(hFileOutSIA,fmt="(4(1F14.6,1x))") BinWidthSIA_ToCenter*(I - 0.D50),ToCent_DistSIA(I)/float(TotalCountSIA_ToCent),BinWidthSIA_BetweenCluster*(I - 0.D50),ClusterGap_DistSIA(I)/float(TotalCountSIA_GapCToC)
    END DO
    close(hFileOutSIA)

    TotalCountVAC_GapCToC = sum(ClusterGap_DistVAC)
    TotalCountVAC_ToCent = sum(ToCent_DistVAC)
    DO I = 1,BinNum
        write(hFileOutVAC,fmt="(4(1F14.6,1x))") BinWidthVAC_ToCenter*(I - 0.D50),ToCent_DistVAC(I)/float(TotalCountVAC_ToCent),BinWidthVAC_BetweenCluster*(I - 0.D50),ClusterGap_DistVAC(I)/float(TotalCountVAC_GapCToC)
    END DO
    close(hFileOutVAC)

    TotalCount_GapSIAToVAC = sum(ClusterGap_SIAToVAC)
    DO I = 1,BinNum
        write(hFileOutSIAToVAC,fmt="(2(1F14.6,1x))") BinWidthSIAToVAC_BetweenCluster*(I - 0.D50),ClusterGap_SIAToVAC(I)/float(TotalCount_GapSIAToVAC)
    END DO
    close(hFileOutSIATOVAC)

    TotalCountMIX_ToCent = sum(ToCent_DistMIX)
    TotalCountMIX_GapCToC = sum(ClusterGap_DistMIX)
    DO I = 1,BinNum
        write(hFileOutMIX,fmt="(4(1F14.6,1x))") BinWidthMIX_ToCenter*(I - 0.D50), &
                                                ToCent_DistMIX(I)/float(TotalCountMIX_ToCent),&
                                                BinWidthMIX_BetweenCluster*(I - 0.D50), &
                                                ClusterGap_DistMIX(I)/float(TotalCountMIX_GapCToC)
    END DO
    close(hFileOutMIX)


    if(allocated(ClustersArraySIA)) deallocate(ClustersArraySIA)
    if(allocated(ClustersArrayVAC)) deallocate(ClustersArrayVAC)

    if(allocated(NAtomEachClusterSIA)) deallocate(NAtomEachClusterSIA)
    if(allocated(NAtomEachClusterVAC)) deallocate(NAtomEachClusterVAC)

    if(allocated(ToCent_DistSIA)) deallocate(ToCent_DistSIA)
    if(allocated(ToCent_DistVAC)) deallocate(ToCent_DistVAC)
    if(allocated(ToCent_DistMIX)) deallocate(ToCent_DistMIX)

    if(allocated(ClusterGap_DistSIA)) deallocate(ClusterGap_DistSIA)
    if(allocated(ClusterGap_DistVAC)) deallocate(ClusterGap_DistVAC)
    if(allocated(ClusterGap_SIAToVAC)) deallocate(ClusterGap_SIAToVAC)
    if(allocated(ClusterGap_DistMIX)) deallocate(ClusterGap_DistMIX)

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

                ClustersArray(NClusterEachBox)%POS(1) = DRSTR(STRTMP(2))
                ClustersArray(NClusterEachBox)%POS(2) = DRSTR(STRTMP(3))
                ClustersArray(NClusterEachBox)%POS(3) = DRSTR(STRTMP(4))
            else
                ClustersArray(NClusterEachBox)%POS(1) = ClustersArray(NClusterEachBox)%POS(1) + DRSTR(STRTMP(2))
                ClustersArray(NClusterEachBox)%POS(2) = ClustersArray(NClusterEachBox)%POS(2) + DRSTR(STRTMP(3))
                ClustersArray(NClusterEachBox)%POS(3) = ClustersArray(NClusterEachBox)%POS(3) + DRSTR(STRTMP(4))
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

end module CascadeDataBase_ANALYSIS


program CascadeDataBase_ANALYSIS_Main
    use CascadeDataBase_ANALYSIS
    implicit none
    !---Dummy Vars---
    integer::arg_Num
    character*256::ARG
    character*256::pathIn
    integer::Index_StartBox
    integer::Index_EndBox
    integer::Index_SIAConfig
    integer::Index_VACConfig
    !---Body---
    write(*,*) "---------Start CascadeDataBase_ANALYSIS_Main----------------------"


    arg_Num = COMMAND_ARGUMENT_COUNT()

    if(arg_Num .LT. 5) then
        write(*,*) "MCPSCUERROR: You must input the folder path, first box index ,last box index,first configuration index, last configuration index "
        pause
    end if

    call Get_Command_Argument(0,ARG)

    call get_command_argument(1,ARG)
    Read(ARG,fmt="(A256)") pathIn
    write(*,*) pathIn

    call get_command_argument(2,ARG)
    Read(ARG,*) Index_StartBox
    write(*,*) Index_StartBox

    call get_command_argument(3,ARG)
    Read(ARG,*) Index_EndBox
    write(*,*) Index_EndBox

    call get_command_argument(4,ARG)
    Read(ARG,*) Index_SIAConfig
    write(*,*) Index_SIAConfig

    call get_command_argument(5,ARG)
    Read(ARG,*) Index_VACConfig
    write(*,*) Index_VACConfig

    call CascadeDataBase_ANALYSIS_SIAANDVAC(pathIn,Index_StartBox,Index_EndBox,Index_SIAConfig,Index_VACConfig)

    write(*,*) "---------END CascadeDataBase_ANALYSIS_Main----------------------"
    return
end program CascadeDataBase_ANALYSIS_Main
