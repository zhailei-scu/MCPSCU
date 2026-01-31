!
!  MC_MultiBoxToOneBox_Accumulate_old_Main.F90
!
!  To Merge all box to one, the data formation is <= 2017_12_11
!
program MC_MultiBoxToOneBox_Accumulate_old_Main
  use MiniUtilities, only:EXTRACT_NUMB,GETINPUTSTRLINE, ISTR, DRSTR
  implicit none
  !-----------Vars---------------
  character*256::ARG
  character*128::filePath
  integer::arg_Num
  integer::hFILE
  integer::sequenceNum
  logical::OPENED
  integer::LINE
  integer::openError
  integer::MultiBox
  character*256::Title
  character*16::STRNUMB(10)
  integer::N
  !-----------Body--------------

  sequenceNum = 0

  arg_Num = COMMAND_ARGUMENT_COUNT()

  if(arg_NUM .GE. 2) then

    call GET_COMMAND_ARGUMENT(0,ARG)

    call GET_COMMAND_ARGUMENT(1,ARG)
    Read(ARG,*) filePath

    call GET_COMMAND_ARGUMENT(2,ARG)
    Read(ARG,*) sequenceNum

    if(sequenceNum .LT. 0) then
      write(*,*) "MCPSCU ERROR: The aimed sequence number of boxes is not true: ",sequenceNum
      stop
    end if
  else
    write(*,*) "MCPSCU ERROR: The input arguments can not be null."
    write(*,*) "The arguments should be 'Control File Name, Sequence boxes Number'."
    write(*,*) "The process would be stop."
  end if


  DO hFile = 10, 99
       INQUIRE(UNIT=hFile, OPENED=opened)
       if(.not.opened) exit
  END DO
  open(unit=hFile, file=filePath, status='old', iostat=openError)

  if(openError .ne. 0) then
    write(*,*) "MCPSCU ERROR: Load simulation file failed",filePath
    stop
  end if

  !*** Load simulation parameters
  !---Load Multi simulation boxs
  call GETINPUTSTRLINE(hFile,Title,LINE, "!",*100)
  call EXTRACT_NUMB(Title,1,N,STRNUMB)
  MultiBox = DRSTR(STRNUMB(1))

  close(hFile)

  call Load_Accumulate_FORMATTED_OLD(MultiBox,filePath,sequenceNum)

  return

  100   write(*,*) "ERROR IN READ SIMUALTION PARAMETER AT LINE: ",LINE
  stop
end program MC_MultiBoxToOneBox_Accumulate_old_Main

!**************************************************************
subroutine Load_Accumulate_FORMATTED_OLD(MultiBox,inputFilePath,sequenceNum)
    !*** Purpose: to load the previous calculation status(formated to ASCII, the data formation is <= 2017_12_11)
    !      hFile: the fileUnit
    !   MultiBox: the number of simulation box
    !       IBox: the index of simulation box
    use MCLIB_CONSTANTS
    use MCLIB_TYPEDEF_ACLUSTER
    use MiniUtilities,only:EXTRACT_NUMB,GETINPUTSTRLINE,DRSTR,ISTR
    implicit none
    !---Dummy Vars---
    integer, intent(in)::MultiBox
    character(*), intent(in)::inputFilePath
    integer, intent(in)::sequenceNum
    !---local variables---
    integer::hFile
    character*256::boxesFileName,C_IBox
    character*256::C_sequenceNum
    character*256::resultFileName
    logical::OPENED
    integer::LINE
    integer::openError
    character*256::Title
    character*16::STRNUMB(10)
    integer::IBox
    real(kind=KMCDF)::POS(3), RAD, CNA, Pre_CURTIME, CURTIME
    real(kind=KMCDF)::Pre_BOXBOUNDARY(3,2),Pre_BOXSIZE(3),BOXBOUNDARY(3,2), BOXSIZE(3)
    integer::I, IC, ICFROM, ICTO, N, LayerID, StatuID,IStatu
    integer::DEPTHDIS_NLAY
    integer::FullStatus
    integer::TNC(p_NUMBER_OF_STATU)
    integer, dimension(:,:),allocatable::SNC
    type(Acluster), dimension(:),allocatable,target::Clusters
    integer, dimension(:,:), allocatable::SEIndexBox
    integer::NC

    !---Body---

    write(C_sequenceNum,*) sequenceNum

    C_sequenceNum = adjustl(C_sequenceNum)

    resultFileName = inputFilePath(1:LEN_TRIM(inputFilePath))//"_VIS_"//trim(C_sequenceNum)//"_Box_Accumulate.dat"

    TNC = 0
    allocate(SNC(MultiBox,p_NUMBER_OF_STATU),SEIndexBox(MultiBox,2))
    SNC = 0
    SEIndexBox = 0

    DO IBox = 1,MultiBox
      write(c_IBox,*) IBox

      c_IBox = adjustl(c_IBox)

      boxesFileName = inputFilePath(1:LEN_TRIM(inputFilePath))//"_VIS_"//trim(C_sequenceNum)//"_Box_"//trim(c_IBox)//".dat"

      DO hFile = 10, 99*MultiBox
         INQUIRE(UNIT=hFile, OPENED=opened)
         if(.not.opened) exit
      END DO
      open(unit=hFile, file=boxesFileName, status='old', iostat=openError)

      if(openError .ne. 0) then
        write(*,*) "MCPSCU ERROR: Load simulation file failed",boxesFileName
        stop
      end if

      ! Load time
      call GETINPUTSTRLINE(hFile,Title,LINE,"",*100)
      call EXTRACT_NUMB(Title,1, N,STRNUMB)
      CURTIME = DRSTR(STRNUMB(1))
      ! Load the box
      call GETINPUTSTRLINE(hFile,Title,LINE,"",*100)
      call EXTRACT_NUMB(Title,6, N,STRNUMB)
      BOXBOUNDARY(1,1) = DRSTR(STRNUMB(1))
      BOXBOUNDARY(1,2) = DRSTR(STRNUMB(2))
      BOXBOUNDARY(2,1) = DRSTR(STRNUMB(3))
      BOXBOUNDARY(2,2) = DRSTR(STRNUMB(4))
      BOXBOUNDARY(3,1) = DRSTR(STRNUMB(5))
      BOXBOUNDARY(3,2) = DRSTR(STRNUMB(6))


      call GETINPUTSTRLINE(hFile,Title,LINE,"",*100)
      call EXTRACT_NUMB(Title,1, N,STRNUMB)

      DEPTHDIS_NLAY = ISTR(STRNUMB(1))

      !--- The number of clusters in all boxs
      !--- The number of clusters in all boxs
      call GETINPUTSTRLINE(hFile,Title,LINE,"",*100)
      call EXTRACT_NUMB(Title,p_NUMBER_OF_STATU, N,STRNUMB)

      TNC = 0
      if(N .GT. 1) then
        DO IStatu = 1,p_NUMBER_OF_STATU
          TNC(IStatu) = ISTR(STRNUMB(IStatu))
        END DO
        FullStatus = 1
      else if(N .EQ. 1) then
        TNC(p_ACTIVE_STATU) = ISTR(STRNUMB(1))
        FullStatus = 0
      else
        write(*,*) "MCPSCU ERROR: The number of Total bubbles cannot be null."
        stop
      end if

      if(.not. allocated(Clusters)) then
        allocate(Clusters(sum(TNC)))
      end if

      !--- The number of clusters in this boxs
      call GETINPUTSTRLINE(hFile,Title,LINE,"",*100)
      call EXTRACT_NUMB(Title,p_NUMBER_OF_STATU, N,STRNUMB)

      if(N .GT. 1) then
        DO IStatu = 1,p_NUMBER_OF_STATU
          SNC(IBox,IStatu) = ISTR(STRNUMB(IStatu))
        END DO
        FullStatus = 1
      else if(N .EQ. 1) then
        SNC(IBox,p_ACTIVE_STATU) = ISTR(STRNUMB(1))
        FullStatus = 0
      else
        write(*,*) "MCPSCU ERROR: The number of Total bubbles cannot be null."
        stop
      end if

      if(IBox .eq. 1) then
        SEIndexBox(IBox,1) = 1
      else if(IBox .GT. 1) then
        SEIndexBox(IBox,1) = SEIndexBox(IBox-1,2) + 1
      else
        write(*,*) "The Index of box is out of range:",IBox
        stop
      end if
      SEIndexBox(IBox,2) = SEIndexBox(IBox,1) + sum(SNC(IBox,p_ACTIVE_STATU:p_ABSORBED_STATU)) - 1

      ICFROM = SEIndexBox(IBox,1)
      ICTO = SEIndexBox(IBox,2)

      !---skip the title " Layer  Statu   x(nm)   y(nm)   z(nm)  radium(nm)  NATOMS"
      call GETINPUTSTRLINE(hFile,Title,LINE,"",*100)

      !--- Load the configure
      DO IC=ICFROM, ICTO
           read(hFile,*) LayerID, StatuID, POS(1:3), RAD, CNA
           Clusters(IC)%m_type = LayerID
           Clusters(IC)%m_POS(1:3) = POS(1:3)*C_NM2CM
           Clusters(IC)%m_RAD = RAD*C_NM2CM
           Clusters(IC)%m_NA = CNA
           Clusters(IC)%m_Statu = StatuID
      END DO

      close(hFile)

    END DO

    DO hFile = 10, 99*MultiBox
         INQUIRE(UNIT=hFile, OPENED=opened)
         if(.not.opened) exit
    END DO
    open(unit=hFile, file=resultFileName)

    write(hFile, FMT="('!--- Total Box ',I8)") MultiBox
    write(hFile, FMT="('!--- TIEM (in s) ', F18.7)") CURTIME
    write(hFile, FMT="('!--- BOXSIZE(in nm) ', 6(1PE14.4, 1x), I5)")      BOXBOUNDARY(1,1),BOXBOUNDARY(1,2), &
                                                                          BOXBOUNDARY(2,1),BOXBOUNDARY(2,2), &
                                                                          BOXBOUNDARY(3,1),BOXBOUNDARY(3,2)
    write(hFile, FMT="('!--- NLAY ',I8)") DEPTHDIS_NLAY

    if(FullStatus .GT. 0) then

        write(hFile, FMT="('!--- Current clusters number of all status (in all boxs) ',5(I8,1x))") TNC(p_ACTIVE_STATU:p_ABSORBED_STATU)
        write(hFile,FMT="(3(A5,1x),15(A14,1x))") "Box","Layer","Statu","x(nm)","y(nm)","z(nm)","radium(nm)","NATOMS"

        DO IBox = 1,MultiBox

            ICFROM = SEIndexBox(IBox,1)
            ICTO = SEIndexBox(IBox,2)

            DO IC = ICFROM, ICTO
              write(hFile,fmt="(3(I5, 1x), 15(1PE14.4, 1x))")      IBox,                            &
                                                                   Clusters(IC)%m_type,             &
                                                                   Clusters(IC)%m_Statu,            &
                                                                   Clusters(IC)%m_POS(1:3)*C_CM2NM, &
                                                                   Clusters(IC)%m_RAD*C_CM2NM,      &
                                                                   Clusters(IC)%m_NA
            END DO
        END DO


    else
      write(hFile, FMT="('!--- Current clusters number of independent active statu  (in all boxs) ',I8)") TNC(p_ACTIVE_STATU)
      write(hFile,FMT="(3(A5,1x),15(A14,1x))") "Box","Layer","Statu","x(nm)","y(nm)","z(nm)","radium(nm)","NATOMS"

      DO IBox = 1,MultiBox

            ICFROM = SEIndexBox(IBox,1)
            ICTO = SEIndexBox(IBox,2)

            DO IC = ICFROM, ICTO
              if(Clusters(IC)%m_Statu .eq. p_ACTIVE_STATU) then
                  write(hFile,fmt="(3(I5, 1x), 15(1PE14.4, 1x))")  IBox,                            &
                                                                   Clusters(IC)%m_type,             &
                                                                   Clusters(IC)%m_Statu,            &
                                                                   Clusters(IC)%m_POS(1:3)*C_CM2NM, &
                                                                   Clusters(IC)%m_RAD*C_CM2NM,      &
                                                                   Clusters(IC)%m_NA
              end if
            END DO
       END DO

    end if

    close(hFile)

    return
    100   write(*,*) "Error in loading configure"
    pause
    stop
end subroutine Load_Accumulate_FORMATTED_OLD
