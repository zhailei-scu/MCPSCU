module MC_StatisticClusters_Offline
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none

    logical,private::FirstTimeVist = .true.

    contains

    subroutine StatisticClusters_MorethanNAtom(Host_Boxes,Host_SimuCtrlParam,NAtomGT,Record,hFileOutEachBox,hFileOutTotalBox)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer,intent(in)::NAtomGT
        CLASS(SimulationRecord)::Record
        integer::hFileOutEachBox
        integer::hFileOutTotalBox
        !---Local Vars---
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFROM
        integer::ICTO
        integer::IStatu
        integer::length
        integer::trueLength
        character*28,dimension(:),allocatable::CCNum
        character*28,dimension(:),allocatable::CCNum_GTNAtom
        character*28,dimension(:),allocatable::CNAVA
        character*28,dimension(:),allocatable::CNAVA_GTNAtom
        character*256::CNUM_GT
        real(kind=KMCDF),dimension(:),allocatable::NAVAEachBox
        real(kind=KMCDF),dimension(:),allocatable::NAVAEachBox_GTNAtom
        real(kind=KMCDF)::NAVA(p_NUMBER_OF_STATU)
        real(kind=KMCDF)::NAVA_GTNAtom(p_NUMBER_OF_STATU)
        integer,dimension(:),allocatable::NCCountEachBox
        integer,dimension(:),allocatable::NCCountEachBox_GTNAtom
        integer::NCCount(p_NUMBER_OF_STATU)
        integer::NCCount_GTNAtom(p_NUMBER_OF_STATU)
        integer::NATOMS
        !---Body---

        MultiBox = Host_SimuCtrlParam%MultiBox

        if(FirstTimeVist .eq. .true.) then
            !---The final one is used for the total boxes
            allocate(CCNum(p_NUMBER_OF_STATU),CCNum_GTNAtom(p_NUMBER_OF_STATU))
            allocate(CNAVA(p_NUMBER_OF_STATU),CNAVA_GTNAtom(p_NUMBER_OF_STATU))

            write(CNUM_GT,*) NAtomGT
            CNUM_GT = adjustl(CNUM_GT)

            DO IStatu = 1,p_NUMBER_OF_STATU
                CCNum(IStatu) = "NUM_"//trim(p_CStatu(IStatu))
                length = len(CCNum(IStatu))
                trueLength = LENTRIM(CCNum(IStatu))
                CCNum(IStatu)(length-trueLength+1:length) = CCNum(IStatu)(1:trueLength)
                CCNum(IStatu)(1:length-trueLength) = ""

                CCNum_GTNAtom(IStatu) = "NC_NAtomGT"//trim(CNUM_GT)//trim(p_CStatu(IStatu))
                length = len(CCNum_GTNAtom(IStatu))
                trueLength = LENTRIM(CCNum_GTNAtom(IStatu))
                CCNum_GTNAtom(IStatu)(length-trueLength+1:length) = CCNum_GTNAtom(IStatu)(1:trueLength)
                CCNum_GTNAtom(IStatu)(1:length-trueLength) = ""


                CNAVA(IStatu) = "NAVA_"//trim(p_CStatu(IStatu))
                length = len(CNAVA(IStatu))
                trueLength = LENTRIM(CNAVA(IStatu))
                CNAVA(IStatu)(length-trueLength+1:length) = CNAVA(IStatu)(1:trueLength)
                CNAVA(IStatu)(1:length-trueLength) = ""

                CNAVA_GTNAtom(IStatu) = "NAVA_NAtomGT"//trim(CNUM_GT)//trim(p_CStatu(IStatu))
                length = len(CNAVA_GTNAtom(IStatu))
                trueLength = LENTRIM(CNAVA_GTNAtom(IStatu))
                CNAVA_GTNAtom(IStatu)(length-trueLength+1:length) = CNAVA_GTNAtom(IStatu)(1:trueLength)
                CNAVA_GTNAtom(IStatu)(1:length-trueLength) = ""
            END DO

            write(hFileOutEachBox, fmt="(130(A30,1x))")           "IBox",                              &
                                                                  "Time(s)",                           &
                                                                  "NACTClusters",                      &
                                                                  CCNum(1:p_NUMBER_OF_STATU),          &
                                                                  CNAVA(1:p_NUMBER_OF_STATU),          &
                                                                  CCNum_GTNAtom(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_GTNAtom(1:p_NUMBER_OF_STATU)


            write(hFileOutTotalBox, fmt="(130(A30,1x))")          "Time(s)",                           &
                                                                  "NACTClusters",                      &
                                                                  CCNum(1:p_NUMBER_OF_STATU),          &
                                                                  CNAVA(1:p_NUMBER_OF_STATU),          &
                                                                  CCNum_GTNAtom(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_GTNAtom(1:p_NUMBER_OF_STATU)


            FirstTimeVist = .false.

        end if

        call AllocateArray_Host(NAVAEachBox,p_NUMBER_OF_STATU,"NAVAEachBox")
        call AllocateArray_Host(NAVAEachBox_GTNAtom,p_NUMBER_OF_STATU,"NAVAEachBox_GTNAtom")
        call AllocateArray_Host(NCCountEachBox,p_NUMBER_OF_STATU,"NCCountEachBox")
        call AllocateArray_Host(NCCountEachBox_GTNAtom,p_NUMBER_OF_STATU,"NCCountEachBox_GTNAtom")

        NAVA = 0.D0
        NAVA_GTNAtom = 0.D0
        NCCount = 0
        NCCount_GTNAtom = 0

        DO IBox = 1,MultiBox

            NAVAEachBox = 0.D0
            NAVAEachBox_GTNAtom = 0.D0
            NCCountEachBox = 0
            NCCountEachBox_GTNAtom = 0

            ICFROM = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            DO IC = ICFROM,ICTO
                DO IStatu = 1,p_NUMBER_OF_STATU
                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. IStatu) then

                        NATOMS = sum(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA)
                        NAVAEachBox(IStatu) = NAVAEachBox(IStatu) + NATOMS
                        NCCountEachBox(IStatu) = NCCountEachBox(IStatu) + 1

                        if(NATOMS .GT. NAtomGT) then
                            NAVAEachBox_GTNAtom(IStatu) = NAVAEachBox_GTNAtom(IStatu) + NATOMS
                            NCCountEachBox_GTNAtom(IStatu) = NCCountEachBox_GTNAtom(IStatu) + 1
                        end if
                    end if
                END DO
            END DO

            NAVA = NAVA + NAVAEachBox
            NAVA_GTNAtom = NAVA_GTNAtom + NAVAEachBox_GTNAtom
            NCCount = NCCount + NCCountEachBox
            NCCount_GTNAtom = NCCount_GTNAtom + NCCountEachBox_GTNAtom

            DO IStatu = 1,p_NUMBER_OF_STATU
                if(NCCountEachBox(IStatu) .GT. 0) then
                    NAVAEachBox(IStatu) = NAVAEachBox(IStatu)/dble(NCCountEachBox(IStatu))
                end if

                if(NCCountEachBox_GTNAtom(IStatu) .GT. 0) then
                    NAVAEachBox_GTNAtom(IStatu) = NAVAEachBox_GTNAtom(IStatu)/dble(NCCountEachBox_GTNAtom(IStatu))
                end if
            END DO



            write(hFileOutEachBox,fmt="(I30,1x,1PE30.8,1x,I30,1x,6(I30,1x),6(1PE30.8,1x),6(I30,1x),6(1PE30.8,1x))")    IBox,                                   &
                                                                                                                Record%GetSimuTimes(),                         &
                                                                                                                NCCountEachBox(p_ACTIVEFREE_STATU)             &
                                                                                                                + NCCountEachBox(p_ACTIVEINGB_STATU),          &
                                                                                                                NCCountEachBox(1:p_NUMBER_OF_STATU),           &
                                                                                                                NAVAEachBox(1:p_NUMBER_OF_STATU),              &
                                                                                                                NCCountEachBox_GTNAtom(1:p_NUMBER_OF_STATU),   &
                                                                                                                NAVAEachBox_GTNAtom(1:p_NUMBER_OF_STATU)

        END DO

        DO IStatu = 1,p_NUMBER_OF_STATU
            if(NCCount(IStatu) .GT. 0) then
                NAVA(IStatu) = NAVA(IStatu)/dble(NCCount(IStatu))
            end if

            if(NCCount_GTNAtom(IStatu) .GT. 0) then
                NAVA_GTNAtom(IStatu) = NAVA_GTNAtom(IStatu)/dble(NCCount_GTNAtom(IStatu))
            end if
        END DO


        write(hFileOutTotalBox,fmt="(1PE30.8,1x,,I30,1x,6(I30,1x),6(1PE30.8,1x),6(I30,1x),6(1PE30.8,1x))")  Record%GetSimuTimes(),          &
                                                                                                    NCCount(p_ACTIVEFREE_STATU)             &
                                                                                                    + NCCount(p_ACTIVEINGB_STATU),          &
                                                                                                    NCCount(1:p_NUMBER_OF_STATU),           &
                                                                                                    NAVA(1:p_NUMBER_OF_STATU),              &
                                                                                                    NCCount_GTNAtom(1:p_NUMBER_OF_STATU),   &
                                                                                                    NAVA_GTNAtom(1:p_NUMBER_OF_STATU)




        call DeAllocateArray_Host(NAVAEachBox,"NAVAEachBox")
        call DeAllocateArray_Host(NAVAEachBox_GTNAtom,"NAVAEachBox_GTNAtom")
        call DeAllocateArray_Host(NCCountEachBox,"NCCountEachBox")
        call DeAllocateArray_Host(NCCountEachBox_GTNAtom,"NCCountEachBox_GTNAtom")

        if(allocated(CCNum)) deallocate(CCNum)
        if(allocated(CCNum_GTNAtom)) deallocate(CCNum_GTNAtom)
        if(allocated(CNAVA)) deallocate(CNAVA)
        if(allocated(CNAVA_GTNAtom)) deallocate(CNAVA_GTNAtom)

        return
    end subroutine StatisticClusters_MorethanNAtom

end module

program Main_StatisticClusters_Offline
    use MC_StatisticClusters_Offline
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    implicit none
    type(SimulationBoxes)::Host_Boxes
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    type(MigCoalClusterRecord)::Record
    character*256::pathIn
    character*256::OutFolder
    character*256::pathOut
    character*256::C_JOB
    character*256::C_TIMESECTION
    character*256::C_ICFG
    character*256::pathOutEachBox
    character*256::pathOutTotalBox
    integer::hFileOutEachBox
    integer::hFileOutTotalBox
    integer::err
    integer::arg_Num
    integer::IJOB
    integer::ITSECTION
    integer::ICFG
    integer::GTNAtom
    logical::exits
    !-----------Body--------------

    !*********Create/Open log file********************
    call OpenLogFile(m_hFILELOG)

    !********Load Global vars from input file**************
    call Initialize_Global_Variables(Host_SimuCtrlParam,Host_Boxes)

    call Print_Global_Variables(6,Host_SimuCtrlParam,Host_Boxes)

    OutFolder = CreateDataFolder(adjustl(trim(Host_SimuCtrlParam%OutFilePath))//"Statistic/")

    pathOutEachBox = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"EachBox.statistic"
    pathOutTotalBox = OutFolder(1:LENTRIM(OutFolder))//FolderSpe//"TotalBox.statistic"

    hFileOutEachBox = CreateNewFile(pathOutEachBox)
    hFileOutTotalBox = CreateNewFile(pathOutTotalBox)

    open(unit=hFileOutEachBox,file=pathOutEachBox, form="formatted")
    open(unit=hFileOutTotalBox,file=pathOutTotalBox, form="formatted")

    DO IJOB = Host_SimuCtrlParam%STARTJOB,Host_SimuCtrlParam%ENDJOB,Host_SimuCtrlParam%JOBSTEP

        DO ITSECTION = Host_SimuCtrlParam%STARTTSECTION,Host_SimuCtrlParam%ENDTSECTION,Host_SimuCtrlParam%TSECTIONSTEP

            DO ICFG = Host_SimuCtrlParam%STARTCFG,Host_SimuCtrlParam%ENDCFG,Host_SimuCtrlParam%CFGSTEP

                C_JOB = ""
                write(C_JOB,*) IJOB
                C_JOB = adjustl(C_JOB)
                C_JOB = "Job"//C_JOB

                C_TIMESECTION = ""
                write(C_TIMESECTION,*) ITSECTION
                C_TIMESECTION = adjustl(C_TIMESECTION)
                C_TIMESECTION = "Section"//C_TIMESECTION

                C_ICFG = ""
                write(C_ICFG,*) ICFG
                C_ICFG = adjustl(C_ICFG)

                pathIn = Host_SimuCtrlParam%OutFilePath(1:LENTRIM(Host_SimuCtrlParam%OutFilePath))//FolderSpe//"Config_"//trim(C_JOB)//"_"//trim(C_TIMESECTION)//"_"//trim(C_ICFG)//".dat"

                INQUIRE(FILE=pathIn,EXIST=exits)
                if(.not. exits) then
                    cycle
                end if

                write(*,*) "MCPSCUInfo: The analysis file: ",pathIn

                !*******Init the simulation boxes*****************
                call Host_Boxes%InitSimulationBox(Host_SimuCtrlParam)

                call Host_Boxes%PutinCfg(Host_SimuCtrlParam,Record,pathIn,m_RNFACTOR,m_FREESURDIFPRE,m_GBSURDIFPRE)

                GTNAtom = 1

                call StatisticClusters_MorethanNAtom(Host_Boxes,Host_SimuCtrlParam,GTNAtom,Record,hFileOutEachBox,hFileOutTotalBox)

                call Host_Boxes%Clean()

            END DO

        END DO

    END DO

    close(hFileOutEachBox)
    close(hFileOutTotalBox)

    return
end program Main_StatisticClusters_Offline
