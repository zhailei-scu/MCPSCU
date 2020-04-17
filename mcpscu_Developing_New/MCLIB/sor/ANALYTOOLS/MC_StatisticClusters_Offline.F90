module MC_StatisticClusters_Offline
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MODEL_ECR_CPU
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
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_GTNAtom
        real(kind=KINDDF)::NAVA(p_NUMBER_OF_STATU)
        real(kind=KINDDF)::NAVA_GTNAtom(p_NUMBER_OF_STATU)
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

    !*******************************************************************************
    subroutine StatisticClusters_ReactionBetweenGroups(Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)
        implicit none
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
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
        character*28,dimension(:),allocatable::CCNum_SIA
        character*28,dimension(:),allocatable::CNAVA_SIA
        character*28,dimension(:),allocatable::CCNum_Vac
        character*28,dimension(:),allocatable::CNAVA_Vac
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_SIA
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_Vac
        real(kind=KINDDF)::NAVA_SIA(p_NUMBER_OF_STATU)
        real(kind=KINDDF)::NAVA_Vac(p_NUMBER_OF_STATU)
        integer,dimension(:),allocatable::NCCountEachBox_SIA
        integer,dimension(:),allocatable::NCCountEachBox_Vac
        integer::NCCount_SIA(p_NUMBER_OF_STATU)
        integer::NCCount_Vac(p_NUMBER_OF_STATU)
        integer::NATOMS_SIA
        integer::NATOMS_Vac
        integer::SIAIndex
        integer::VacIndex
        integer::NC_InterCascadeReact
        integer::NC_InterCascadeReact_EachBox
        integer::NCEachCascade
        integer::CascadeIndex
        integer::Num_AnnihilateEachBox
        integer::Num_Annihilate
        !---Body---
        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")


        MultiBox = Host_SimuCtrlParam%MultiBox

        NCEachCascade = 160

        if(FirstTimeVist .eq. .true.) then
            !---The final one is used for the total boxes
            allocate(CCNum_SIA(p_NUMBER_OF_STATU),CCNum_Vac(p_NUMBER_OF_STATU))
            allocate(CNAVA_SIA(p_NUMBER_OF_STATU),CNAVA_Vac(p_NUMBER_OF_STATU))

            DO IStatu = 1,p_NUMBER_OF_STATU
                CCNum_SIA(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))//"_SIA"
                length = len(CCNum_SIA(IStatu))
                trueLength = LENTRIM(CCNum_SIA(IStatu))
                CCNum_SIA(IStatu)(length-trueLength+1:length) = CCNum_SIA(IStatu)(1:trueLength)
                CCNum_SIA(IStatu)(1:length-trueLength) = ""

                CCNum_Vac(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))//"_VAC"
                length = len(CCNum_Vac(IStatu))
                trueLength = LENTRIM(CCNum_Vac(IStatu))
                CCNum_Vac(IStatu)(length-trueLength+1:length) = CCNum_Vac(IStatu)(1:trueLength)
                CCNum_Vac(IStatu)(1:length-trueLength) = ""

                CNAVA_SIA(IStatu) = "NAVA_"//adjustl(trim(p_CStatu(IStatu)))//"_SIA"
                length = len(CNAVA_SIA(IStatu))
                trueLength = LENTRIM(CNAVA_SIA(IStatu))
                CNAVA_SIA(IStatu)(length-trueLength+1:length) = CNAVA_SIA(IStatu)(1:trueLength)
                CNAVA_SIA(IStatu)(1:length-trueLength) = ""

                CNAVA_Vac(IStatu) = "NAVA_"//adjustl(trim(p_CStatu(IStatu)))//"_VAC"
                length = len(CNAVA_Vac(IStatu))
                trueLength = LENTRIM(CNAVA_Vac(IStatu))
                CNAVA_Vac(IStatu)(length-trueLength+1:length) = CNAVA_Vac(IStatu)(1:trueLength)
                CNAVA_Vac(IStatu)(1:length-trueLength) = ""

            END DO

            write(hFileOutEachBox, fmt="(130(A30,1x))")           "IBox",                          &
                                                                  "Time(s)",                       &
                                                                  CCNum_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CCNum_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  "NUM_IntercascadeReact",         &
                                                                  "NUM_Annihilate"


            write(hFileOutTotalBox, fmt="(130(A30,1x))")          "Time(s)",                       &
                                                                  CCNum_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CCNum_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  "NUM_IntercascadeReact",         &
                                                                  "NUM_Annihilate"


            FirstTimeVist = .false.

        end if

        call AllocateArray_Host(NAVAEachBox_SIA,p_NUMBER_OF_STATU,"NAVAEachBox_SIA")
        call AllocateArray_Host(NAVAEachBox_Vac,p_NUMBER_OF_STATU,"NAVAEachBox_Vac")
        call AllocateArray_Host(NCCountEachBox_SIA,p_NUMBER_OF_STATU,"NCCountEachBox_SIA")
        call AllocateArray_Host(NCCountEachBox_Vac,p_NUMBER_OF_STATU,"NCCountEachBox_Vac")

        NAVA_SIA = 0.D0
        NAVA_Vac = 0.D0
        NCCount_SIA = 0
        NCCount_Vac = 0
        NC_InterCascadeReact = 0

        Num_Annihilate = 0

        DO IBox = 1,MultiBox

            NAVAEachBox_SIA = 0.D0
            NCCountEachBox_SIA = 0
            NAVAEachBox_Vac = 0.D0
            NCCountEachBox_Vac = 0
            NC_InterCascadeReact_EachBox = 0

            Num_AnnihilateEachBox = 0

            ICFROM = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            DO IC = ICFROM,ICTO
                DO IStatu = 1,p_NUMBER_OF_STATU

                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. IStatu) then

                        NATOMS_SIA = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA
                        NAVAEachBox_SIA(IStatu) = NAVAEachBox_SIA(IStatu) + NATOMS_SIA
                        NCCountEachBox_SIA(IStatu) = NCCountEachBox_SIA(IStatu) + 1

                        NATOMS_Vac = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacIndex)%m_NA
                        NAVAEachBox_Vac(IStatu) = NAVAEachBox_Vac(IStatu) + NATOMS_Vac
                        NCCountEachBox_Vac(IStatu) = NCCountEachBox_Vac(IStatu) + 1

                        if(NATOMS_SIA .GT. 0 .and. NATOMS_Vac .GT. 0) then
                            write(*,*) "MCPSUCERROR: it is impossible to existence both SIA and vacancy in one cluster."
                            write(*,*) "For cluster: ",IC
                            pause
                            stop
                        end if
                    end if

                END DO

                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ABSORBED_STATU .or. &
                   Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ANNIHILATE_STATU) then
                   CascadeIndex = (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) - 1)/NCEachCascade + 1
                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) .GT. CascadeIndex*NCEachCascade .or. &
                       Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) .LT. (CascadeIndex-1)*NCEachCascade) then
                        NC_InterCascadeReact_EachBox = NC_InterCascadeReact_EachBox + 1
                    end if
                end if

                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ABSORBED_STATU) then
                    if(any(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA .LT. 0)) then
                        Num_AnnihilateEachBox = Num_AnnihilateEachBox + 1
                    end if
                end if

            END DO

            Num_AnnihilateEachBox = Num_AnnihilateEachBox + NCCountEachBox_SIA(p_ANNIHILATE_STATU) + NCCountEachBox_VAC(p_ANNIHILATE_STATU)

            NAVA_SIA = NAVA_SIA + NAVAEachBox_SIA
            NCCount_SIA = NCCount_SIA + NCCountEachBox_SIA
            NAVA_Vac = NAVA_Vac + NAVAEachBox_Vac
            NCCount_Vac = NCCount_Vac + NCCountEachBox_Vac
            NC_InterCascadeReact = NC_InterCascadeReact + NC_InterCascadeReact_EachBox
            Num_Annihilate = Num_Annihilate + Num_AnnihilateEachBox

            DO IStatu = 1,p_NUMBER_OF_STATU
                if(NCCountEachBox_SIA(IStatu) .GT. 0) then
                    NAVAEachBox_SIA(IStatu) = NAVAEachBox_SIA(IStatu)/dble(NCCountEachBox_SIA(IStatu))
                end if

                if(NCCountEachBox_Vac(IStatu) .GT. 0) then
                    NAVAEachBox_Vac(IStatu) = NAVAEachBox_Vac(IStatu)/dble(NCCountEachBox_Vac(IStatu))
                end if
            END DO

            write(hFileOutEachBox,fmt="(I30,1x,1PE30.8,1x,7(I30,1x),7(I30,1x),7(1PE30.8,1x),7(1PE30.8,1x),I30,I30)")        IBox,                              &
                                                                                                                Record%GetSimuTimes(),                         &
                                                                                                                NCCountEachBox_SIA(1:p_NUMBER_OF_STATU),       &
                                                                                                                NCCountEachBox_Vac(1:p_NUMBER_OF_STATU),       &
                                                                                                                NAVAEachBox_SIA(1:p_NUMBER_OF_STATU),          &
                                                                                                                NAVAEachBox_Vac(1:p_NUMBER_OF_STATU),          &
                                                                                                                NC_InterCascadeReact_EachBox,                  &
                                                                                                                Num_AnnihilateEachBox


        END DO

        DO IStatu = 1,p_NUMBER_OF_STATU
            if(NCCount_SIA(IStatu) .GT. 0) then
                NAVA_SIA(IStatu) = NAVA_SIA(IStatu)/dble(NCCount_SIA(IStatu))
            end if

            if(NCCount_Vac(IStatu) .GT. 0) then
                NAVA_Vac(IStatu) = NAVA_Vac(IStatu)/dble(NCCount_Vac(IStatu))
            end if
        END DO


        write(hFileOutTotalBox,fmt="(1PE30.8,1x,7(I30,1x),7(I30,1x),7(1PE30.8,1x),7(1PE30.8,1x),I30,I30)")      Record%GetSimuTimes(),              &
                                                                                                            NCCount_SIA(1:p_NUMBER_OF_STATU),       &
                                                                                                            NCCount_Vac(1:p_NUMBER_OF_STATU),       &
                                                                                                            NAVA_SIA(1:p_NUMBER_OF_STATU),          &
                                                                                                            NAVA_Vac(1:p_NUMBER_OF_STATU),          &
                                                                                                            NC_InterCascadeReact,                   &
                                                                                                            Num_Annihilate




        call DeAllocateArray_Host(NAVAEachBox_SIA,"NAVAEachBox_SIA")
        call DeAllocateArray_Host(NCCountEachBox_SIA,"NCCountEachBox_SIA")
        call DeAllocateArray_Host(NAVAEachBox_Vac,"NAVAEachBox_Vac")
        call DeAllocateArray_Host(NCCountEachBox_Vac,"NCCountEachBox_Vac")

        if(allocated(CCNum_SIA)) deallocate(CCNum_SIA)
        if(allocated(CNAVA_SIA)) deallocate(CNAVA_SIA)
        if(allocated(CCNum_Vac)) deallocate(CCNum_Vac)
        if(allocated(CNAVA_Vac)) deallocate(CNAVA_Vac)

        return
    end subroutine StatisticClusters_ReactionBetweenGroups

    !**************************************************************
    subroutine ResloveCascadeControlFile(hFile,CascadeNum)
        !---Dummy Vars---
        integer,intent(in)::hFile
        integer,intent(out)::CascadeNum
        !---Local Vars---
        integer::LINE
        character*256::STR
        character*30::KEYWORD
        character*200::STRTMP(10)
        integer::N
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


        return
    end subroutine


    !*******************************************************************************
    subroutine StatisticClusters_SIAAndVAC_Ver2019_08_16(CascadeControlFile,Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)
        implicit none
        !---Dummy Vars---
        character*256,intent(in)::CascadeControlFile
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        CLASS(SimulationRecord)::Record
        integer::hFileOutEachBox
        integer::hFileOutTotalBox
        !---Local Vars---
        integer::hFile
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFROM
        integer::ICTO
        integer::IStatu
        integer::length
        integer::trueLength
        character*28,dimension(:),allocatable::CCNum
        character*28,dimension(:),allocatable::CCNum_SIA
        character*28,dimension(:),allocatable::CNAVA_SIA
        character*28,dimension(:),allocatable::CCNum_Vac
        character*28,dimension(:),allocatable::CNAVA_Vac
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_SIA
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_Vac
        real(kind=KINDDF)::NAVA_SIA(p_NUMBER_OF_STATU)
        real(kind=KINDDF)::NAVA_Vac(p_NUMBER_OF_STATU)
        integer,dimension(:),allocatable::NCCountEachBox
        integer,dimension(:),allocatable::NCCountEachBox_SIA
        integer,dimension(:),allocatable::NCCountEachBox_Vac
        integer::NCCount(p_NUMBER_OF_STATU)
        integer::NCCount_SIA(p_NUMBER_OF_STATU)
        integer::NCCount_Vac(p_NUMBER_OF_STATU)
        integer::NATOMS_SIA
        integer::NATOMS_Vac
        integer::SIAIndex
        integer::VacIndex
        integer::NC_InterCascadeReact
        integer::NC_InterCascadeReact_EachBox
        integer::CascadeNumEachBox
        integer::NClusterEachCascade
        integer::CascadeIndex
        integer::Num_RecombineEachBox
        integer::Num_Recombine
        !---Body---
        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        hFile = OpenExistedFile(CascadeControlFile)
        call ResloveCascadeControlFile(hFile,CascadeNumEachBox)

        MultiBox = Host_SimuCtrlParam%MultiBox

        if(FirstTimeVist .eq. .true.) then
            !---The final one is used for the total boxes
            allocate(CCNum(p_NUMBER_OF_STATU))
            allocate(CCNum_SIA(p_NUMBER_OF_STATU),CCNum_Vac(p_NUMBER_OF_STATU))
            allocate(CNAVA_SIA(p_NUMBER_OF_STATU),CNAVA_Vac(p_NUMBER_OF_STATU))

            DO IStatu = 1,p_NUMBER_OF_STATU
                CCNum(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))
                length = len(CCNum(IStatu))
                trueLength = LENTRIM(CCNum(IStatu))
                CCNum(IStatu)(length-trueLength+1:length) = CCNum(IStatu)(1:trueLength)
                CCNum(IStatu)(1:length-trueLength) = ""

                CCNum_SIA(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))//"_SIA"
                length = len(CCNum_SIA(IStatu))
                trueLength = LENTRIM(CCNum_SIA(IStatu))
                CCNum_SIA(IStatu)(length-trueLength+1:length) = CCNum_SIA(IStatu)(1:trueLength)
                CCNum_SIA(IStatu)(1:length-trueLength) = ""

                CCNum_Vac(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))//"_VAC"
                length = len(CCNum_Vac(IStatu))
                trueLength = LENTRIM(CCNum_Vac(IStatu))
                CCNum_Vac(IStatu)(length-trueLength+1:length) = CCNum_Vac(IStatu)(1:trueLength)
                CCNum_Vac(IStatu)(1:length-trueLength) = ""

                CNAVA_SIA(IStatu) = "NAVA_"//adjustl(trim(p_CStatu(IStatu)))//"_SIA"
                length = len(CNAVA_SIA(IStatu))
                trueLength = LENTRIM(CNAVA_SIA(IStatu))
                CNAVA_SIA(IStatu)(length-trueLength+1:length) = CNAVA_SIA(IStatu)(1:trueLength)
                CNAVA_SIA(IStatu)(1:length-trueLength) = ""

                CNAVA_Vac(IStatu) = "NAVA_"//adjustl(trim(p_CStatu(IStatu)))//"_VAC"
                length = len(CNAVA_Vac(IStatu))
                trueLength = LENTRIM(CNAVA_Vac(IStatu))
                CNAVA_Vac(IStatu)(length-trueLength+1:length) = CNAVA_Vac(IStatu)(1:trueLength)
                CNAVA_Vac(IStatu)(1:length-trueLength) = ""

            END DO

            write(hFileOutEachBox, fmt="(130(A30,1x))")           "IBox",                          &
                                                                  "Time(s)",                       &
                                                                  CCNum(1:p_NUMBER_OF_STATU),      &
                                                                  CCNum_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CCNum_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  "NUM_IntercascadeReact",         &
                                                                  "NUM_Recombine"


            write(hFileOutTotalBox, fmt="(130(A30,1x))")          "Time(s)",                       &
                                                                  CCNum(1:p_NUMBER_OF_STATU),      &
                                                                  CCNum_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CCNum_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  "NUM_IntercascadeReact",         &
                                                                  "NUM_Recombine"


            FirstTimeVist = .false.

        end if

        call AllocateArray_Host(NAVAEachBox_SIA,p_NUMBER_OF_STATU,"NAVAEachBox_SIA")
        call AllocateArray_Host(NAVAEachBox_Vac,p_NUMBER_OF_STATU,"NAVAEachBox_Vac")
        call AllocateArray_Host(NCCountEachBox,p_NUMBER_OF_STATU,"NCCountEachBox")
        call AllocateArray_Host(NCCountEachBox_SIA,p_NUMBER_OF_STATU,"NCCountEachBox_SIA")
        call AllocateArray_Host(NCCountEachBox_Vac,p_NUMBER_OF_STATU,"NCCountEachBox_Vac")

        NAVA_SIA = 0.D0
        NAVA_Vac = 0.D0
        NCCount = 0
        NCCount_SIA = 0
        NCCount_Vac = 0
        NC_InterCascadeReact = 0
        Num_Recombine = 0

        DO IBox = 1,MultiBox

            NCCountEachBox = 0
            NAVAEachBox_SIA = 0.D0
            NCCountEachBox_SIA = 0
            NAVAEachBox_Vac = 0.D0
            NCCountEachBox_Vac = 0
            NC_InterCascadeReact_EachBox = 0
            Num_RecombineEachBox = 0

            ICFROM = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            NClusterEachCascade = (ICTO - ICFROM + 1)/CascadeNumEachBox
            if(mod(ICTO - ICFROM + 1,CascadeNumEachBox) .ne. 0) then
                write(*,*) "MCPSCUERROR: It is impossible that the cluster number in cascades is not same in box"
                write(*,*) ICTO - ICFROM + 1
                write(*,*) CascadeNumEachBox
                pause
                stop
            end if

            DO IC = ICFROM,ICTO
                IStatu = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

                NCCountEachBox(IStatu) = NCCountEachBox(IStatu) + 1

                NATOMS_SIA = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA
                NATOMS_Vac = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacIndex)%m_NA
                if(abs(NATOMS_SIA) .GT. 0) then
                    NAVAEachBox_SIA(IStatu) = NAVAEachBox_SIA(IStatu) + abs(NATOMS_SIA)
                    NCCountEachBox_SIA(IStatu) = NCCountEachBox_SIA(IStatu) + 1
                else if(abs(NATOMS_Vac) .GT. 0) then
                    NAVAEachBox_Vac(IStatu) = NAVAEachBox_Vac(IStatu) + abs(NATOMS_Vac)
                    NCCountEachBox_Vac(IStatu) = NCCountEachBox_Vac(IStatu) + 1
                end if

                if(NATOMS_SIA .ne. 0 .and. NATOMS_Vac .ne. 0) then
                    write(*,*) "MCPSUCERROR: it is impossible to existence both SIA and vacancy in one cluster."
                    write(*,*) "For cluster: ",IC
                    pause
                    stop
                end if


                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ABSORBED_STATU) then
                    CascadeIndex = (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) - 1)/NClusterEachCascade + 1
                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) .GT. CascadeIndex*NClusterEachCascade .or. &
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) .LT. (CascadeIndex-1)*NClusterEachCascade) then
                        NC_InterCascadeReact_EachBox = NC_InterCascadeReact_EachBox + 1
                    end if
                end if

                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ABSORBED_STATU) then
                    if(any(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA .LT. 0)) then
                        Num_RecombineEachBox = Num_RecombineEachBox + 1
                    end if
                end if

                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ANNIHILATE_STATU) then
                    Num_RecombineEachBox = Num_RecombineEachBox + 1
                end if

            END DO

            NCCount = NCCount + NCCountEachBox
            NAVA_SIA = NAVA_SIA + NAVAEachBox_SIA
            NCCount_SIA = NCCount_SIA + NCCountEachBox_SIA
            NAVA_Vac = NAVA_Vac + NAVAEachBox_Vac
            NCCount_Vac = NCCount_Vac + NCCountEachBox_Vac
            NC_InterCascadeReact = NC_InterCascadeReact + NC_InterCascadeReact_EachBox
            Num_Recombine = Num_Recombine + Num_RecombineEachBox

            DO IStatu = 1,p_NUMBER_OF_STATU
                if(NCCountEachBox_SIA(IStatu) .GT. 0) then
                    NAVAEachBox_SIA(IStatu) = NAVAEachBox_SIA(IStatu)/dble(NCCountEachBox_SIA(IStatu))
                end if

                if(NCCountEachBox_Vac(IStatu) .GT. 0) then
                    NAVAEachBox_Vac(IStatu) = NAVAEachBox_Vac(IStatu)/dble(NCCountEachBox_Vac(IStatu))
                end if
            END DO

            write(hFileOutEachBox,fmt="(I30,1x,1PE30.8,1x,7(I30,1x),7(I30,1x),7(I30,1x),7(1PE30.8,1x),7(1PE30.8,1x),I30,I30)")        IBox,                    &
                                                                                                                Record%GetSimuTimes(),                         &
                                                                                                                NCCountEachBox(1:p_NUMBER_OF_STATU),           &
                                                                                                                NCCountEachBox_SIA(1:p_NUMBER_OF_STATU),       &
                                                                                                                NCCountEachBox_Vac(1:p_NUMBER_OF_STATU),       &
                                                                                                                NAVAEachBox_SIA(1:p_NUMBER_OF_STATU),          &
                                                                                                                NAVAEachBox_Vac(1:p_NUMBER_OF_STATU),          &
                                                                                                                NC_InterCascadeReact_EachBox,                  &
                                                                                                                Num_RecombineEachBox

        END DO

        DO IStatu = 1,p_NUMBER_OF_STATU
            if(NCCount_SIA(IStatu) .GT. 0) then
                NAVA_SIA(IStatu) = NAVA_SIA(IStatu)/dble(NCCount_SIA(IStatu))
            end if

            if(NCCount_Vac(IStatu) .GT. 0) then
                NAVA_Vac(IStatu) = NAVA_Vac(IStatu)/dble(NCCount_Vac(IStatu))
            end if
        END DO


        write(hFileOutTotalBox,fmt="(1PE30.8,1x,7(I30,1x),7(I30,1x),7(I30,1x),7(1PE30.8,1x),7(1PE30.8,1x),I30,I30)")  Record%GetSimuTimes(),        &
                                                                                                            NCCount(1:p_NUMBER_OF_STATU),           &
                                                                                                            NCCount_SIA(1:p_NUMBER_OF_STATU),       &
                                                                                                            NCCount_Vac(1:p_NUMBER_OF_STATU),       &
                                                                                                            NAVA_SIA(1:p_NUMBER_OF_STATU),          &
                                                                                                            NAVA_Vac(1:p_NUMBER_OF_STATU),          &
                                                                                                            NC_InterCascadeReact,                   &
                                                                                                            Num_Recombine


        call DeAllocateArray_Host(NCCountEachBox,"NCCountEachBox")
        call DeAllocateArray_Host(NAVAEachBox_SIA,"NAVAEachBox_SIA")
        call DeAllocateArray_Host(NCCountEachBox_SIA,"NCCountEachBox_SIA")
        call DeAllocateArray_Host(NAVAEachBox_Vac,"NAVAEachBox_Vac")
        call DeAllocateArray_Host(NCCountEachBox_Vac,"NCCountEachBox_Vac")

        if(allocated(CCNum)) deallocate(CCNum)
        if(allocated(CCNum_SIA)) deallocate(CCNum_SIA)
        if(allocated(CNAVA_SIA)) deallocate(CNAVA_SIA)
        if(allocated(CCNum_Vac)) deallocate(CCNum_Vac)
        if(allocated(CNAVA_Vac)) deallocate(CNAVA_Vac)

        close(hFile)

        return
    end subroutine StatisticClusters_SIAAndVAC_Ver2019_08_16


    !*******************************************************************************
    subroutine StatisticClusters_SIAAndVAC_Ver2019_08_20(CascadeControlFile,Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)
        implicit none
        !---Dummy Vars---
        character*256,intent(in)::CascadeControlFile
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        CLASS(SimulationRecord)::Record
        integer::hFileOutEachBox
        integer::hFileOutTotalBox
        !---Local Vars---
        integer::hFile
        integer::MultiBox
        integer::IBox
        integer::IC
        integer::ICFROM
        integer::ICTO
        integer::IStatu
        integer::length
        integer::trueLength
        character*28,dimension(:),allocatable::CCNum
        character*28,dimension(:),allocatable::CCNum_SIA
        character*28,dimension(:),allocatable::CNAVA_SIA
        character*28,dimension(:),allocatable::CCNum_Vac
        character*28,dimension(:),allocatable::CNAVA_Vac
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_SIA
        real(kind=KINDDF),dimension(:),allocatable::NAVAEachBox_Vac
        real(kind=KINDDF)::NAVA_SIA(p_NUMBER_OF_STATU)
        real(kind=KINDDF)::NAVA_Vac(p_NUMBER_OF_STATU)
        integer,dimension(:),allocatable::NCCountEachBox
        integer,dimension(:),allocatable::NCCountEachBox_SIA
        integer,dimension(:),allocatable::NCCountEachBox_Vac
        integer::NCCount(p_NUMBER_OF_STATU)
        integer::NCCount_SIA(p_NUMBER_OF_STATU)
        integer::NCCount_Vac(p_NUMBER_OF_STATU)
        integer::NATOMS_SIA
        integer::NATOMS_Vac
        integer::SIAIndex
        integer::VacIndex
        integer::NC_InterCascadeReact
        integer::NC_InterCascadeReact_EachBox
        integer::CascadeNumEachBox
        integer::NClusterEachCascade
        integer::CascadeIndex
        integer::Num_RecombineEachBox
        integer::Num_Recombine
        !---Body---
        SIAIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("W")
        VacIndex = Host_Boxes%Atoms_list%FindIndexBySymbol("VC")

        hFile = OpenExistedFile(CascadeControlFile)
        call ResloveCascadeControlFile(hFile,CascadeNumEachBox)

        MultiBox = Host_SimuCtrlParam%MultiBox

        if(FirstTimeVist .eq. .true.) then
            !---The final one is used for the total boxes
            allocate(CCNum(p_NUMBER_OF_STATU))
            allocate(CCNum_SIA(p_NUMBER_OF_STATU),CCNum_Vac(p_NUMBER_OF_STATU))
            allocate(CNAVA_SIA(p_NUMBER_OF_STATU),CNAVA_Vac(p_NUMBER_OF_STATU))

            DO IStatu = 1,p_NUMBER_OF_STATU
                CCNum(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))
                length = len(CCNum(IStatu))
                trueLength = LENTRIM(CCNum(IStatu))
                CCNum(IStatu)(length-trueLength+1:length) = CCNum(IStatu)(1:trueLength)
                CCNum(IStatu)(1:length-trueLength) = ""

                CCNum_SIA(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))//"_SIA"
                length = len(CCNum_SIA(IStatu))
                trueLength = LENTRIM(CCNum_SIA(IStatu))
                CCNum_SIA(IStatu)(length-trueLength+1:length) = CCNum_SIA(IStatu)(1:trueLength)
                CCNum_SIA(IStatu)(1:length-trueLength) = ""

                CCNum_Vac(IStatu) = "NUM_"//adjustl(trim(p_CStatu(IStatu)))//"_VAC"
                length = len(CCNum_Vac(IStatu))
                trueLength = LENTRIM(CCNum_Vac(IStatu))
                CCNum_Vac(IStatu)(length-trueLength+1:length) = CCNum_Vac(IStatu)(1:trueLength)
                CCNum_Vac(IStatu)(1:length-trueLength) = ""

                CNAVA_SIA(IStatu) = "NAVA_"//adjustl(trim(p_CStatu(IStatu)))//"_SIA"
                length = len(CNAVA_SIA(IStatu))
                trueLength = LENTRIM(CNAVA_SIA(IStatu))
                CNAVA_SIA(IStatu)(length-trueLength+1:length) = CNAVA_SIA(IStatu)(1:trueLength)
                CNAVA_SIA(IStatu)(1:length-trueLength) = ""

                CNAVA_Vac(IStatu) = "NAVA_"//adjustl(trim(p_CStatu(IStatu)))//"_VAC"
                length = len(CNAVA_Vac(IStatu))
                trueLength = LENTRIM(CNAVA_Vac(IStatu))
                CNAVA_Vac(IStatu)(length-trueLength+1:length) = CNAVA_Vac(IStatu)(1:trueLength)
                CNAVA_Vac(IStatu)(1:length-trueLength) = ""

            END DO

            write(hFileOutEachBox, fmt="(130(A30,1x))")           "IBox",                          &
                                                                  "Time(s)",                       &
                                                                  "ISTEP",                         &
                                                                  "TStep(s)",                      &
                                                                  CCNum(1:p_NUMBER_OF_STATU),      &
                                                                  CCNum_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CCNum_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  "NUM_IntercascadeReact",         &
                                                                  "NUM_Recombine"


            write(hFileOutTotalBox, fmt="(130(A30,1x))")          "Time(s)",                       &
                                                                  "ISTEP",                         &
                                                                  "TStep(s)",                      &
                                                                  CCNum(1:p_NUMBER_OF_STATU),      &
                                                                  CCNum_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CCNum_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_SIA(1:p_NUMBER_OF_STATU),  &
                                                                  CNAVA_Vac(1:p_NUMBER_OF_STATU),  &
                                                                  "NUM_IntercascadeReact",         &
                                                                  "NUM_Recombine"


            FirstTimeVist = .false.

        end if

        call AllocateArray_Host(NAVAEachBox_SIA,p_NUMBER_OF_STATU,"NAVAEachBox_SIA")
        call AllocateArray_Host(NAVAEachBox_Vac,p_NUMBER_OF_STATU,"NAVAEachBox_Vac")
        call AllocateArray_Host(NCCountEachBox,p_NUMBER_OF_STATU,"NCCountEachBox")
        call AllocateArray_Host(NCCountEachBox_SIA,p_NUMBER_OF_STATU,"NCCountEachBox_SIA")
        call AllocateArray_Host(NCCountEachBox_Vac,p_NUMBER_OF_STATU,"NCCountEachBox_Vac")

        NAVA_SIA = 0.D0
        NAVA_Vac = 0.D0
        NCCount = 0
        NCCount_SIA = 0
        NCCount_Vac = 0
        NC_InterCascadeReact = 0
        Num_Recombine = 0

        DO IBox = 1,MultiBox

            NCCountEachBox = 0
            NAVAEachBox_SIA = 0.D0
            NCCountEachBox_SIA = 0
            NAVAEachBox_Vac = 0.D0
            NCCountEachBox_Vac = 0
            NC_InterCascadeReact_EachBox = 0
            Num_RecombineEachBox = 0

            ICFROM = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTO = Host_Boxes%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            NClusterEachCascade = (ICTO - ICFROM + 1)/CascadeNumEachBox
            if(mod(ICTO - ICFROM + 1,CascadeNumEachBox) .ne. 0) then
                write(*,*) "MCPSCUERROR: It is impossible that the cluster number in cascades is not same in box"
                write(*,*) ICTO - ICFROM + 1
                write(*,*) CascadeNumEachBox
                pause
                stop
            end if

            DO IC = ICFROM,ICTO
                IStatu = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

                NCCountEachBox(IStatu) = NCCountEachBox(IStatu) + 1

                NATOMS_SIA = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(SIAIndex)%m_NA
                NATOMS_Vac = Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(VacIndex)%m_NA
                if(abs(NATOMS_SIA) .GT. 0) then
                    NAVAEachBox_SIA(IStatu) = NAVAEachBox_SIA(IStatu) + abs(NATOMS_SIA)
                    NCCountEachBox_SIA(IStatu) = NCCountEachBox_SIA(IStatu) + 1
                else if(abs(NATOMS_Vac) .GT. 0) then
                    NAVAEachBox_Vac(IStatu) = NAVAEachBox_Vac(IStatu) + abs(NATOMS_Vac)
                    NCCountEachBox_Vac(IStatu) = NCCountEachBox_Vac(IStatu) + 1
                end if

                if(NATOMS_SIA .ne. 0 .and. NATOMS_Vac .ne. 0) then
                    write(*,*) "MCPSUCERROR: it is impossible to existence both SIA and vacancy in one cluster."
                    write(*,*) "For cluster: ",IC
                    pause
                    stop
                end if


                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ABSORBED_STATU) then
                    CascadeIndex = (Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(1) - 1)/NClusterEachCascade + 1
                    if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) .GT. CascadeIndex*NClusterEachCascade .or. &
                        Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Record(2) .LT. (CascadeIndex-1)*NClusterEachCascade) then
                        NC_InterCascadeReact_EachBox = NC_InterCascadeReact_EachBox + 1
                    end if
                end if

                if(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ABSORBED_STATU) then
                    if(any(Host_Boxes%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA .LT. 0)) then
                        Num_RecombineEachBox = Num_RecombineEachBox + 1
                    end if
                end if

            END DO

            NCCount = NCCount + NCCountEachBox
            NAVA_SIA = NAVA_SIA + NAVAEachBox_SIA
            NCCount_SIA = NCCount_SIA + NCCountEachBox_SIA
            NAVA_Vac = NAVA_Vac + NAVAEachBox_Vac
            NCCount_Vac = NCCount_Vac + NCCountEachBox_Vac
            NC_InterCascadeReact = NC_InterCascadeReact + NC_InterCascadeReact_EachBox
            Num_Recombine = Num_Recombine + Num_RecombineEachBox

            DO IStatu = 1,p_NUMBER_OF_STATU
                if(NCCountEachBox_SIA(IStatu) .GT. 0) then
                    NAVAEachBox_SIA(IStatu) = NAVAEachBox_SIA(IStatu)/dble(NCCountEachBox_SIA(IStatu))
                end if

                if(NCCountEachBox_Vac(IStatu) .GT. 0) then
                    NAVAEachBox_Vac(IStatu) = NAVAEachBox_Vac(IStatu)/dble(NCCountEachBox_Vac(IStatu))
                end if
            END DO

            write(hFileOutEachBox,fmt="(I30,1x,1PE30.8,1x,I30,1x,1PE30.8,1x,7(I30,1x),7(I30,1x),7(I30,1x),7(1PE30.8,1x),7(1PE30.8,1x),I30,I30)")               &
                                                                                                                IBox,                                          &
                                                                                                                Record%GetSimuTimes(),                         &
                                                                                                                Record%GetSimuSteps(),                         &
                                                                                                                Record%GetTimeSteps(),                         &
                                                                                                                NCCountEachBox(1:p_NUMBER_OF_STATU),           &
                                                                                                                NCCountEachBox_SIA(1:p_NUMBER_OF_STATU),       &
                                                                                                                NCCountEachBox_Vac(1:p_NUMBER_OF_STATU),       &
                                                                                                                NAVAEachBox_SIA(1:p_NUMBER_OF_STATU),          &
                                                                                                                NAVAEachBox_Vac(1:p_NUMBER_OF_STATU),          &
                                                                                                                NC_InterCascadeReact_EachBox,                  &
                                                                                                                Num_RecombineEachBox

        END DO

        DO IStatu = 1,p_NUMBER_OF_STATU
            if(NCCount_SIA(IStatu) .GT. 0) then
                NAVA_SIA(IStatu) = NAVA_SIA(IStatu)/dble(NCCount_SIA(IStatu))
            end if

            if(NCCount_Vac(IStatu) .GT. 0) then
                NAVA_Vac(IStatu) = NAVA_Vac(IStatu)/dble(NCCount_Vac(IStatu))
            end if
        END DO


        write(hFileOutTotalBox,fmt="(1PE30.8,1x,I30,1x,1PE30.8,1x,7(I30,1x),7(I30,1x),7(I30,1x),7(1PE30.8,1x),7(1PE30.8,1x),I30,I30)")              &
                                                                                                            Record%GetSimuTimes(),                  &
                                                                                                            Record%GetSimuSteps(),                  &
                                                                                                            Record%GetTimeSteps(),                  &
                                                                                                            NCCount(1:p_NUMBER_OF_STATU),           &
                                                                                                            NCCount_SIA(1:p_NUMBER_OF_STATU),       &
                                                                                                            NCCount_Vac(1:p_NUMBER_OF_STATU),       &
                                                                                                            NAVA_SIA(1:p_NUMBER_OF_STATU),          &
                                                                                                            NAVA_Vac(1:p_NUMBER_OF_STATU),          &
                                                                                                            NC_InterCascadeReact,                   &
                                                                                                            Num_Recombine


        call DeAllocateArray_Host(NCCountEachBox,"NCCountEachBox")
        call DeAllocateArray_Host(NAVAEachBox_SIA,"NAVAEachBox_SIA")
        call DeAllocateArray_Host(NCCountEachBox_SIA,"NCCountEachBox_SIA")
        call DeAllocateArray_Host(NAVAEachBox_Vac,"NAVAEachBox_Vac")
        call DeAllocateArray_Host(NCCountEachBox_Vac,"NCCountEachBox_Vac")

        if(allocated(CCNum)) deallocate(CCNum)
        if(allocated(CCNum_SIA)) deallocate(CCNum_SIA)
        if(allocated(CNAVA_SIA)) deallocate(CNAVA_SIA)
        if(allocated(CCNum_Vac)) deallocate(CCNum_Vac)
        if(allocated(CNAVA_Vac)) deallocate(CNAVA_Vac)

        close(hFile)

        return
    end subroutine StatisticClusters_SIAAndVAC_Ver2019_08_20



end module

program Main_StatisticClusters_Offline
    use MC_StatisticClusters_Offline
    use MCLIB_GLOBAL
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MIGCOALE_TYPEDEF_SIMRECORD
    use MIGCOALE_ADDONDATA_HOST
    use MCLIB_UTILITIES
    implicit none
    integer::arg_Num
    character*256::ARG
    character*256::SampleFile
    character*256::CascadeControlFile
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
    integer::IJOB
    integer::ITSECTION
    integer::ICFG
    integer::GTNAtom
    logical::exits
    character*30::TheVersion
    !-----------Body--------------
    arg_Num = Command_Argument_Count()

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

                call Host_Boxes%PutinCfg(Host_SimuCtrlParam,Record,pathIn,m_FREESURDIFPRE,m_GBSURDIFPRE,TheVersion)

                GTNAtom = 1

                !call StatisticClusters_MorethanNAtom(Host_Boxes,Host_SimuCtrlParam,GTNAtom,Record,hFileOutEachBox,hFileOutTotalBox)
                !call StatisticClusters_ReactionBetweenGroups(Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)

                select case(adjustl(trim(TheVersion)))
                    case("2019_08_16")
                        call StatisticClusters_SIAAndVAC_Ver2019_08_16(CascadeControlFile,Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)
                    case(adjustl(trim(mp_Version)))
                        call StatisticClusters_SIAAndVAC_Ver2019_08_20(CascadeControlFile,Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)
                    case default
                        call StatisticClusters_SIAAndVAC_Ver2019_08_16(CascadeControlFile,Host_Boxes,Host_SimuCtrlParam,Record,hFileOutEachBox,hFileOutTotalBox)
                end select

                call Host_Boxes%Clean()

            END DO

        END DO

    END DO

    close(hFileOutEachBox)
    close(hFileOutTotalBox)

    return
end program Main_StatisticClusters_Offline
