module MCLIB_TYPEDEF_SIMULATIONBOXARRAY
  use MCLIB_CONSTANTS
  use MCLIB_TYPEDEF_ATOMSLIST
  use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
  USE MCLIB_TYPEDEF_ClustersInfo_CPU
  use MCLIB_TYPEDEF_USUAL
  use MCLIB_TYPEDEF_DiffusorPropList
  use MCLIB_TYPEDEF_ReactionPropList
  use MCLIB_UTILITIES
  use MCLIB_TYPEDEF_GEOMETRY
  use MiniUtilities, only:EXTRACT_NUMB,EXTRACT_SUBSTR,GETINPUTSTRLINE, GETKEYWORD, UPCASE, ISTR, DRSTR
  implicit none

  character(len=5), parameter, private::m_BOXSTARTFLAG = "&BOXF"

  type,public::BoxStatis
    integer::NC0(p_NUMBER_OF_STATU) = 0                            ! initial number of clusters
    integer::NC(p_NUMBER_OF_STATU)  = 0                            ! the number of all status clusters at current time intervale
    integer(kind=KMCLINT)::NA(p_NUMBER_OF_STATU) = 0               ! the number of all status atomics at current time intervale
    integer::NCDumpAdded = 0

    real(kind=KMCDF)::AveNearestSpeFreeClusters = 0.D0
    real(kind=KMCDF)::AveNearestSpeGBClusters = 0.D0
    contains
    procedure,public,non_overridable,pass::Init=>Init_BoxStatis
    procedure,public,non_overridable,pass::Clean=>Clean_BoxStatis
    procedure,private,non_overridable,pass::CopyBoxStatisFromOther
    Generic::ASSIGNMENT(=)=>CopyBoxStatisFromOther
    Final::CleanBoxStatis
  end type BoxStatis

  type,public::BoxesBasicStatistic
    type(BoxStatis)::BoxesStatis_Integral
    type(BoxStatis),dimension(:),allocatable::BoxesStatis_Single

    contains
    procedure,public,non_overridable,pass::Init=>Init_BoxesBasicStatistic
    procedure,public,non_overridable,pass::Clean=>Clean_BoxesBasicStatistic
    procedure,private,non_overridable,pass::CopyBoxesBasicStatisticFromOther
    Generic::ASSIGNMENT(=)=>CopyBoxesBasicStatisticFromOther
    Final::CleanBoxesBasicStatistic
  end type BoxesBasicStatistic


  type,public::BoxesInfo
    integer, dimension(:,:), allocatable::SEActIndexBox                ! the start and end active clusters Index in each Box
                                                                       ! (this array is got while each box is re-scanned and
                                                                       ! dm_SEActIndexBox(1,1)=1,
                                                                       ! dm_SEActIndexBox(1,2)=dm_SEActIndexBox(1,1) + m_Boxes%Box(1)%SNC(p_ActiveFree_statu) + m_Boxes%Box(1)%SNC(p_ActiveINGB_statu) - 1,
                                                                       ! dm_SEActIndexBox(2,1)=dm_SEActIndexBox(1,2) + 1,
                                                                       ! dm_SEActIndexBox(2,2)=dm_SEActIndexBox(2,1) + m_Boxes%Box(2)%SNC(p_ActiveFree_statu) + m_Boxes%Box(2)%SNC(p_ActiveINGB_statu)- 1)

    integer, dimension(:,:), allocatable::SEUsedIndexBox               ! the actual simulated start and end index cluster in each Box (CPU)

    integer, dimension(:,:), allocatable::SEVirtualIndexBox             ! while there are clusters are implanted to the box, to improve the calculation efficiency, we would pre-allocate a bigger block of
                                                                       ! array (bigger than required) to leave some free memory space for each box, and based on the implantation type, we would put lots of
                                                                       ! clusters in these free memory one-time. So we need not put clusters in each step, in each step, we need only to move the end cluster index
                                                                       ! of each box. The SEVirtualIndexBox is the start and end index for the bigger memory space for each box. The SEUsedIndexBox is the actual start
                                                                       ! and end index for each box. By other words, we need only move the SEUsedIndexBox(:,2) for each box in each step.


    integer, dimension(:,:), allocatable::SEExpdIndexBox              ! In implantation situation, as the total clusters in each box is changing in each box, it is necessary to update neighbor-list in each step,
                                                                       ! obviously, it is pretty inefficiency. To improve the calculation efficiency, as described for SEVirtualIndexBox, a block of bigger memory space
                                                                       ! is pre-allocated for each box, we can let the neighbor-list calculation to cover all of this block, but it seems like have over-calculation,
                                                                       ! because some clusters are not used. To be a compromise strategy, we use SEExpdIndexBox to indicate the clusters index range to be calculated
                                                                       ! the SEExpdIndexBox(IBox,1) = SEUsedIndexBox(IBox,1) = SEVirtualIndexBox(IBox,1),
                                                                       !  = SEUsedIndexBox(IBox,2) < SEExpdIndexBox(IBox,2) < SEVirtualIndexBox(IBox,2)
    integer, dimension(:,:), allocatable::SEAddedClustersBoxes

    contains
    procedure,public,pass,non_overridable::Init=>InitBoxesInfo
    procedure,public,pass,non_overridable::Clean=>Clean_BoxesInfo
    procedure,private,pass,non_overridable::CopyBoxesInfoFromOther
    Generic::ASSIGNMENT(=)=>CopyBoxesInfoFromOther
    Final::CleanBoxesInfo

  end type BoxesInfo

  type,public::SimulationBoxes

    !***********Diffusor list memory mapping*************
    type(DiffusorTypesMap)::m_DiffusorTypesMap

    !**********Reaction list memory mapping**************
    type(ReactionsMap)::m_ReactionsMap

    !*************Info about clusters*********
    type(ClustersInfo_CPU)::m_ClustersInfo_CPU


    !************Info about Geometry**********
    type(GrainBoundary)::m_GrainBoundary

    !************Info for boxsize*************
    real(kind=KMCDF)::LatticeLength = 3.14D-8                          ! Lattice length (cm)
    real(kind=KMCDF)::BOXBOUNDARY(3,2) = 0                             ! siumulation box boundary, in unit of atomic radiua
    real(kind=KMCDF)::BOXSIZE(3) = 0                                   ! simulation box size
    real(kind=KMCDF)::HBOXSIZE(3) = 0                                  ! half box size
    real(kind=KMCDF)::BOXVOLUM = 0                                     ! voulme of the box

    !************Info for matrix*************
    type(ATOM)::MatrixAtom

    !***********Info for atoms***************
    type(AtomsList),pointer::Atoms_list=>null()

    !************Boxes Info******************
    type(BoxesInfo)::m_BoxesInfo

    !***********Boxes Basic Statistic Info********
    type(BoxesBasicStatistic)::m_BoxesBasicStatistic

    !**************Init file****************
    character*256::IniConfig = ""

    !**********Implantation file************
    character*256::ImpFile = ""

    !***********Info for diffusor************
    type(ReadDiffusorPropList),pointer::ReadDiffusorProp_List=>null()

    !**********Info for reactions************
    type(ReactionPropList),pointer::ReadReactionProp_List=>null()

    contains

    procedure,non_overridable,public,pass::DefaultValueSimulationBoxes=>DefaultValue_SimulationBoxes
    procedure,non_overridable,public,pass::LoadParameter_SimulationBoxes=>Load_Parameter_SimulationBoxes
    procedure,non_overridable,public,pass::Print_Parameter_SimulationBoxes
    procedure,non_overridable,private,pass::Load_Box_Shape
    procedure,non_overridable,private,pass::Load_Box_AtomsDefine
    procedure,non_overridable,private,pass::Load_OneSecton_AtomDefine
    procedure,non_overridable,private,pass::Load_Box_Diffusors
    procedure,non_overridable,private,pass::LoadDiffusorsValue
    procedure,non_overridable,private,pass::LoadOneDiffusors
    procedure,non_overridable,private,pass::LoadDiffusorsValueFromScript
    procedure,non_overridable,private,pass::Load_Box_GrainBoundary
    procedure,non_overridable,private,pass::Load_GB_Simple
    procedure,non_overridable,private,pass::Load_GB_Simple_Distribution
    procedure,non_overridable,private,pass::Load_GB_Simple_Distribution_ByGSeedCtl
    procedure,non_overridable,private,pass::Load_GB_Simple_Distribution_ByGVolumCtl
    procedure,non_overridable,private,pass::Load_GB_SpecialDistFromFile
    procedure,non_overridable,private,pass::Load_GB_SpecialDistFromExteFunc
    procedure,non_overridable,public,pass::InitSimulationBox=>Init_SimulationBox
    procedure,NON_OVERRIDABLE,pass,public::RescaleBoxes_CPU=>Rescale_Boxes_CPU
    procedure,NON_OVERRIDABLE,pass,public::SweepUnActiveMemory_CPU=>Sweep_UnActiveMemory_CPU
    procedure,non_overridable,pass,public::GetBoxesBasicStatistic_AllStatu_CPU
    procedure,non_overridable,pass,public::GetOneBoxBasicStatistic_AllStatu_CPU
    procedure,non_overridable,public,pass::PutoutCfg=>Puout_Instance_Config_SimBoxArray
    procedure,non_overridable,public,pass::PutinCfg=>Putin_Instance_Config_SimBoxArray
    procedure,non_overridable,private,pass::Putin_OKMC_OUTCFG_FORMAT18
    procedure,non_overridable,private,pass::Putin_MF_OUTCFG_FORMAT18
    procedure,non_overridable,public,pass::Putin_MF_OUTCFG_FORMAT18_Distribution
    procedure,non_overridable,private,pass::Putin_SPMF_OUTCFG_FORMAT18
    procedure,non_overridable,public,pass::Putin_SPMF_OUTCFG_FORMAT18_Distribution
    procedure,non_overridable,private,pass::DoPutin_FromDistribution
    procedure,NON_OVERRIDABLE,pass,private::ExpandClustersInfor_CPU_EqualNum=>Expand_ClustersInfor_CPU_EqualNum
    procedure,NON_OVERRIDABLE,pass,private::ExpandClustersInfor_CPU_BoxByBox=>Expand_ClustersInfor_CPU_BoxByBox
    Generic,public::ExpandClustersInfor_CPU=>ExpandClustersInfor_CPU_EqualNum,ExpandClustersInfor_CPU_BoxByBox
    procedure,non_overridable,private,pass::CopySimulationBoxesFromOther
    Generic::Assignment(=)=>CopySimulationBoxesFromOther
    procedure,non_overridable,public,pass::Clean=>CleanSimulationBoxes
    Final::DestorySimulationBoxes

  end type SimulationBoxes

  private::Init_BoxStatis
  private::CopyBoxStatisFromOther
  private::Clean_BoxStatis
  private::Init_BoxesBasicStatistic
  private::CopyBoxesBasicStatisticFromOther
  private::Clean_BoxesBasicStatistic
  private::CleanBoxesBasicStatistic
  private::InitBoxesInfo
  private::CopyBoxesInfoFromOther
  private::Clean_BoxesInfo
  private::CleanBoxesInfo
  private::DefaultValue_SimulationBoxes
  private::Load_Parameter_SimulationBoxes
  private::Print_Parameter_SimulationBoxes
  private::Load_Box_Shape
  private::Load_Box_AtomsDefine
  private::Load_OneSecton_AtomDefine
  private::Load_Box_Diffusors
  private::LoadDiffusorsValue
  private::LoadOneDiffusors
  private::LoadDiffusorsValueFromScript
  private::Load_Box_GrainBoundary
  private::Load_GB_Simple
  private::Load_GB_Simple_Distribution
  private::Load_GB_Simple_Distribution_ByGSeedCtl
  private::Load_GB_Simple_Distribution_ByGVolumCtl
  private::Load_GB_SpecialDistFromFile
  private::Load_GB_SpecialDistFromExteFunc
  private::Init_SimulationBox
  private::Expand_ClustersInfor_CPU_EqualNum
  private::Expand_ClustersInfor_CPU_BoxByBox
  private::Rescale_Boxes_CPU
  private::Sweep_UnActiveMemory_CPU
  private::GetBoxesBasicStatistic_AllStatu_CPU
  private::GetOneBoxBasicStatistic_AllStatu_CPU
  private::Get_MaxClustersNum
  private::CleanSimulationBoxes
  private::DestorySimulationBoxes
  private::Puout_Instance_Config_SimBoxArray
  private::Putin_Instance_Config_SimBoxArray
  private::Putin_OKMC_OUTCFG_FORMAT18
  private::Putin_MF_OUTCFG_FORMAT18
  private::Putin_MF_OUTCFG_FORMAT18_Distribution
  private::Putin_SPMF_OUTCFG_FORMAT18
  private::Putin_SPMF_OUTCFG_FORMAT18_Distribution
  private::DoPutin_FromDistribution
  private::CopySimulationBoxesFromOther

  contains

  !*************For type BoxStatis*******************************
  subroutine Init_BoxStatis(this)
    implicit none
    !---Dummy Vars---
    CLASS(BoxStatis)::this
    !---Body---
    this%NC0 = 0
    this%NC = 0
    this%NA = 0
    this%NCDumpAdded = 0

    this%AveNearestSpeFreeClusters = 0.D0
    this%AveNearestSpeGBClusters = 0.D0

    return
  end subroutine Init_BoxStatis

  !***************************************************************
  subroutine CopyBoxStatisFromOther(this,other)
    implicit none
    !---Dummy Vars---
    CLASS(BoxStatis),intent(out)::this
    type(BoxStatis), intent(in)::other
    !---Body---
    call this%Clean()

    this%NC0 = other%NC0

    this%NC = other%NC

    this%NA = other%NA

    this%NCDumpAdded = other%NCDumpAdded

    this%AveNearestSpeFreeClusters = other%AveNearestSpeFreeClusters
    this%AveNearestSpeGBClusters = other%AveNearestSpeGBClusters

    return
  end subroutine

  !*****************************************************************
  subroutine Clean_BoxStatis(this)
    implicit none
    !---Dummy Vars---
    CLASS(BoxStatis)::this
    !---Body---
    this%NC0 = 0
    this%NC = 0
    this%NA = 0

    this%NCDumpAdded = 0

    this%AveNearestSpeFreeClusters = 0.D0
    this%AveNearestSpeGBClusters = 0.D0

    return
  end subroutine Clean_BoxStatis

    !*****************************************************************
  subroutine CleanBoxStatis(this)
    implicit none
    !---Dummy Vars---
    type(BoxStatis)::this
    !---Body---
    call this%Clean()

    return
  end subroutine CleanBoxStatis

  !*************For type BoxesBasicStatistic***************************
  subroutine Init_BoxesBasicStatistic(this,MultiBox)
    implicit none
    !---Dummy Vars---
    CLASS(BoxesBasicStatistic)::this
    integer,intent(in)::MultiBox
    !---Local Vars---
    integer::IBox
    !---Body---
    if(allocated(this%BoxesStatis_Single)) then
        deallocate(this%BoxesStatis_Single)
    end if
    allocate(this%BoxesStatis_Single(MultiBox))

    DO IBox = 1, MultiBox
        call this%BoxesStatis_Single(IBox)%Init()
    END DO
    call this%BoxesStatis_Integral%Init()

    return
  end subroutine

  !************************************************
  subroutine Clean_BoxesBasicStatistic(this)
    implicit none
    !---Dummy Vars---
    CLASS(BoxesBasicStatistic)::this

    !---Body---
    if(allocated(this%BoxesStatis_Single)) then
        deallocate(this%BoxesStatis_Single)
    end if
    call this%BoxesStatis_Integral%Clean()

    return
  end subroutine

  !*************************************************
  subroutine CopyBoxesBasicStatisticFromOther(this,other)
    implicit none
    !---Dummy Vars---
    CLASS(BoxesBasicStatistic),intent(out)::this
    type(BoxesBasicStatistic),intent(in)::other
    !---Local Vars---
    integer::IBox
    integer::MultiBox
    !---Body---
    call this%Clean()

    MultiBox = size(other%BoxesStatis_Single)
    call this%Init(MultiBox)

    ! The assignment(=) had been override
    this%BoxesStatis_Integral = other%BoxesStatis_Integral

    DO IBox = 1,MultiBox
        ! The assignment(=) had been override
        this%BoxesStatis_Single(IBox) = other%BoxesStatis_Single(IBox)
    END DO

    return
  end subroutine

  !*************************************************
  subroutine CleanBoxesBasicStatistic(this)
    implicit none
    !---Dummy Vars---
    type(BoxesBasicStatistic)::this
    !---Body---
    call this%Clean()

    return
  end subroutine


  !*************For type BoxesInfo********************************
  subroutine InitBoxesInfo(this,MultiBox)
    implicit none
    !---Dummy Vars---
    CLASS(BoxesInfo)::this
    integer,intent(in)::MultiBox
    !---Body---

    call AllocateArray_Host(this%SEActIndexBox,MULTIBOX,2,"SEActIndexBox")
    this%SEActIndexBox = 0

    call AllocateArray_Host(this%SEUsedIndexBox,MULTIBOX,2,"SEUsedIndexBox")
    this%SEUsedIndexBox = 0

    call AllocateArray_Host(this%SEExpdIndexBox,MULTIBOX,2,"SEExpdIndexBox")
    this%SEExpdIndexBox = 0

    call AllocateArray_Host(this%SEAddedClustersBoxes,MULTIBOX,2,"SEAddedClustersBoxes")
    this%SEAddedClustersBoxes = 0

    call AllocateArray_Host(this%SEVirtualIndexBox,MULTIBOX,2,"SEVirtualIndexBox")
    this%SEVirtualIndexBox = 0

    return
  end subroutine InitBoxesInfo

  !*****************************************************************
  subroutine CopyBoxesInfoFromOther(this,Other)
    implicit none
    !---Dummy Vars---
    CLASS(BoxesInfo),intent(out)::this
    type(BoxesInfo),intent(in)::Other
    !---Local Vars---
    integer::MultiBox
    !---Body---
    call this%Clean()

    MultiBox = size(Other%SEUSedIndexBox,DIM=1)

    call this%Init(MultiBox)

    this%SEActIndexBox = other%SEActIndexBox

    this%SEUsedIndexBox = other%SEUsedIndexBox

    this%SEExpdIndexBox = Other%SEExpdIndexBox

    this%SEAddedClustersBoxes = other%SEAddedClustersBoxes

    this%SEVirtualIndexBox = other%SEVirtualIndexBox
    return
  end subroutine

  !******************************************************************
  subroutine Clean_BoxesInfo(this)
    implicit none
    !---Dummy Vars---
    CLASS(BoxesInfo)::this
    !---Body---

    call DeAllocateArray_Host(this%SEActIndexBox,"SEActIndexBox")

    call DeAllocateArray_Host(this%SEUsedIndexBox,"SEUsedIndexBox")

    call DeAllocateArray_Host(this%SEAddedClustersBoxes,"SEAddedClustersBoxes")

    call DeAllocateArray_Host(this%SEVirtualIndexBox,"SEVirtualIndexBox")

    call DeAllocateArray_Host(this%SEExpdIndexBox,"SEExpdIndexBox")

    return
  end subroutine Clean_BoxesInfo

  !***************************************************************
  subroutine CleanBoxesInfo(this)
    implicit none
    !---Dummy Vars---
    type(BoxesInfo)::this
    !---Body---
    call this%Clean()

    return
  end subroutine CleanBoxesInfo

  !***************************************************************
  subroutine Init_SimulationBox(this,Host_SimuCtrlParam)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    !---Local Vars---
    !---Body---
    call this%m_BoxesInfo%Init(Host_SimuCtrlParam%MultiBox)

    call this%m_BoxesBasicStatistic%Init(Host_SimuCtrlParam%MultiBox)

    call this%m_GrainBoundary%ConstructGrainBoundary(this%BOXBOUNDARY,Host_SimuCtrlParam)

    return
  end subroutine

  !****************************************************************
  subroutine DefaultValue_SimulationBoxes(this)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this

    !***Box Shape
    this%BOXBOUNDARY = 0
    this%BOXSIZE  = 0
    this%HBOXSIZE = 0
    this%BOXVOLUM    = 0

    !***Peridic boundary
    this%IniConfig = ""

    this%ImpFile = ""

    return
  end subroutine DefaultValue_SimulationBoxes

  !*****************************************************************
  subroutine CopySimulationBoxesFromOther(this,Other)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes),intent(out)::this
    type(SimulationBoxes),intent(in)::Other
    !---Body---

    call DestorySimulationBoxes(this)

    ! The Assignment had been override
    this%m_ClustersInfo_CPU = Other%m_ClustersInfo_CPU

    ! The Assignment had been override
    this%m_DiffusorTypesMap = other%m_DiffusorTypesMap

    ! The Assignment had been override
    this%m_ReactionsMap = other%m_ReactionsMap

    ! The Assignment had been override
    this%m_GrainBoundary = Other%m_GrainBoundary

    this%LatticeLength = Other%LatticeLength
    this%BOXBOUNDARY = Other%BOXBOUNDARY
    this%BOXSIZE = Other%BOXSIZE
    this%HBOXSIZE = Other%HBOXSIZE
    this%BOXVOLUM = Other%BOXVOLUM

    ! The Assignment(=) had been override
    this%MatrixAtom = Other%MatrixAtom

    if(associated(this%Atoms_list)) then
        ! The Assignment(=) had been override
        this%Atoms_list = Other%Atoms_list
    end if

    ! The Assignment(=) had been override
    this%m_BoxesInfo = Other%m_BoxesInfo

    ! The Assignment(=) had been override
    this%m_BoxesBasicStatistic = Other%m_BoxesBasicStatistic

    this%IniConfig = Other%IniConfig

    this%ImpFile = Other%ImpFile

    ! The Assignment(=) had been override
    if(associated(this%ReadDiffusorProp_List)) then
        this%ReadDiffusorProp_List = Other%ReadDiffusorProp_List
    end if

    return
  end subroutine

  !*****************************************************************
  subroutine Load_Parameter_SimulationBoxes(this,hBoxFile)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---

    integer::LINE
    character*256::STR
    character*32::KEYWORD
    !---Body---

    call GETINPUTSTRLINE(hBoxFile,STR, LINE, "!", *100)
    call RemoveComments(STR,"!")
    STR = adjustl(STR)
    call GETKEYWORD("&", STR, KEYWORD)
    call UPCASE(KEYWORD)
    if(KEYWORD(1:LENTRIM(KEYWORD)) .ne. m_BOXSTARTFLAG) then
      write(*,*) "MCPSCUERROR: The Start Flag of simulation box Parameters is Illegal: ",KEYWORD(1:LENTRIM(KEYWORD))
      pause
      stop
    end if

    DO While(.TRUE.)
      call GETINPUTSTRLINE(hBoxFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDBOXF")
            exit
        case("&BOXSUBCTL")
          call this%Load_Box_Shape(hBoxFile,*100)
        case("&ATOMSUBCTL")
          call this%Load_Box_AtomsDefine(hBoxFile,*100)
        case("&DIFFUSORSUBCTL")
          call this%Load_Box_Diffusors(hBoxFile,*100)
        case("&GBSUBCTL")
          call this%Load_Box_GrainBoundary(hBoxFile,*100)
        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD))
          write(*,*) "Please Check Box File at Line: ",LINE
          pause
          stop
      end select

    END DO

    return
    !-----------------------------------------------------
    100 write(*,*) "MCPSCU ERROR: Failer to read Simulation box Parameters."
        write(*,*) "The process would be stop."
        stop
  end subroutine Load_Parameter_SimulationBoxes

  !****************************************
  subroutine Print_Parameter_SimulationBoxes(this,hFile)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hFile
    !---Local Vars---
    type(AtomsList),pointer::atomsListCursor=>null()
    type(ReadDiffusorPropList),pointer::diffusorListCursor=>null()
    !---Body---

    write(hFile,*) "!****************Siumation Boxes file information***********************"
    write(hFile,fmt="('!',A70,'!',2x,1PE10.4)") "Simulation Box lattice length(cm) =",this%LatticeLength
    write(hFile,fmt="('!',A70,'!',2x,3(1PE10.4,2x))") "Simulation Box size(cm) =",this%BOXSIZE

    atomsListCursor=>this%Atoms_list

    if(atomsListCursor%Get_ListCount() .GT. 0) then
        DO While(associated(atomsListCursor))
            write(hFile,fmt="('!',A30,I10,2x,'!',A30,I10)") "------The atom inner index = ",atomsListCursor%m_Atom%m_ID,&
                                                            "------The number of atoms = ",atomsListCursor%m_AtomsNumber
            write(hFile,fmt="('!',A30,A10,2x,'!',A30,I10,2(2x,A30,1PE10.4))") "The atom symbol = ",atomsListCursor%m_Atom%m_Symbol, &
                                                                             "The element index = ",atomsListCursor%m_Atom%m_ElementIndex, &
                                                                             "The element mass = ",atomsListCursor%m_Atom%m_AtomMass, &
                                                                             "The atomic m_Volum(cm^3) = ",atomsListCursor%m_Atom%m_Volum
            atomsListCursor=>atomsListCursor%next
        END DO
    end if

    write(hFile,fmt="('!',A70,'!',2x,A10)") "Simulation Box Matrix symbol =",this%MatrixAtom%m_Symbol

    write(hFile,fmt="('!',A70,'!',2x,1PE10.4)") "Simulation Box Matrix atom volum(cm^3) =",this%MatrixAtom%m_Volum


    diffusorListCursor=>this%ReadDiffusorProp_List
    if(diffusorListCursor%GetList_Count() .GT. 0) then
        Do While(associated(diffusorListCursor))

            write(hFile,fmt="('!','The diffusor symbol = ',A20,2x,  &
                              '!','CoefficentsGenerate way =',I1,2x, &
                              '!','DiffusionCiefficents value =',1PE10.4,2x, &
                              '!','PreFactor = ',1PE10.4,2x, &
                              '!','ActEnergy = ',1PE10.4,2x, &
                              '!','ECR Generate way = ',I1,2x, &
                              '!','ECR Value = ',1PE10.4)")                    diffusorListCursor%Diffusor%symbol, &
                                                                               diffusorListCursor%Diffusor%DiffusorValueType, &
                                                                               diffusorListCursor%Diffusor%DiffuseCoefficient_Value,  &
                                                                               diffusorListCursor%Diffusor%PreFactor, &
                                                                               diffusorListCursor%Diffusor%ActEnergy, &
                                                                               diffusorListCursor%Diffusor%ECRValueType, &
                                                                               diffusorListCursor%Diffusor%ECR
            diffusorListCursor=>diffusorListCursor%next
        End Do
    end if

    !---Check the diffusorList---

    write(*,*) "**************************************************************************************************"
    write(*,*) "*                                                                                                *"
    write(*,*) "***********************Start to Check The diffusors map*******************************************"
    write(*,*) "*                                                                                                *"
    write(*,*) "**************************************************************************************************"
    call this%ReadDiffusorProp_List%PrintOutCheckingResult(hFile,this%Atoms_list,this%m_DiffusorTypesMap)

    write(*,*) "**************************************************************************************************"
    write(*,*) "*                                                                                                *"
    write(*,*) "***********************Start to Check The reactions map*******************************************"
    write(*,*) "*                                                                                                *"
    write(*,*) "**************************************************************************************************"
    this%ReadReactionProp_List%PrintOutCheckingResult(hFile,this%Atoms_list,this%m_ReactionsMap)

    Nullify(atomsListCursor)
    atomsListCursor=>null()

    Nullify(diffusorListCursor)
    diffusorListCursor=>null()

    return
  end subroutine Print_Parameter_SimulationBoxes

  !*****************************************
  subroutine Load_Box_Shape(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::I
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRNUMB(10)
    real(kind=KMCDF)::BOXSIZE(3)
    !---Body---

    DO While(.TRUE.)
      call GETINPUTSTRLINE(hBoxFile,STR, LINE, "!", *100)
      call RemoveComments(STR,"!")
      STR = adjustl(STR)
      call GETKEYWORD("&", STR, KEYWORD)
      call UPCASE(KEYWORD)

      select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case("&ENDSUBCTL")
          exit
        case("&SIZE")
           call EXTRACT_NUMB(STR,3,N,STRNUMB)

           if(N .LT. 3) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for BOXSIZE Setting."
             write(*,*) "At control file line: ",LINE
             write(*,*) "Should be '&SIZE   bx(LU)= ,  by(LU) = , bz(LU) = '."
             stop
           else
             BOXSIZE(1) = DRSTR(STRNUMB(1))
             BOXSIZE(2) = DRSTR(STRNUMB(2))
             BOXSIZE(3) = DRSTR(STRNUMB(3))

             if(any(BOXSIZE .LT. 0)) then
               write(*,*) "MCPSCU ERROR: The value of BOXSIZE can not less than 0 .",this%BOXSIZE
               stop
             end if

           end if

        case("&LATT")
           call EXTRACT_NUMB(STR,1,N,STRNUMB)

           if(N .LT. 1) then
             write(*,*) "MCPSCU ERROR: Too Few Parameters for LATT Setting."
             write(*,*) "At box file line: ",LINE
             write(*,*) "Should be '&LATT latiice constant(nm) = '."
             stop
           else
             this%LatticeLength = DRSTR(STRNUMB(1))*C_NM2CM

           end if

        case default
          write(*,*) "MCPSCU ERROR: The Illegal Flag: ",KEYWORD(1:LENTRIM(KEYWORD))
          write(*,*) "Please Check box File at Line: ",LINE
          stop
      end select
    END DO

    this%BOXSIZE(1) = this%LatticeLength*BOXSIZE(1)
    this%BOXSIZE(2) = this%LatticeLength*BOXSIZE(2)
    this%BOXSIZE(3) = this%LatticeLength*BOXSIZE(3)

    DO I = 1,3
        this%HBOXSIZE(I) = 0.5*this%BOXSIZE(I)
        this%BOXBOUNDARY(I,1) = -0.5*this%BOXSIZE(I)
        this%BOXBOUNDARY(I,2) = 0.5*this%BOXSIZE(I)
    END DO

    this%BOXVOLUM = this%BOXSIZE(1)*this%BOXSIZE(2)*this%BOXSIZE(3)

    return

    100 return 1
  end subroutine Load_Box_Shape

  !*****************************************
  subroutine Load_Box_AtomsDefine(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRNUMB(10)
    !---Body---

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&GROUPSUBCTL")
                call this%Load_OneSecton_AtomDefine(hBoxFile,*100)
            case default
                write(*,*) "MCPSCUERROR: The illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "Please check box file at Line: ",LINE
                pause
                stop
        end select


    END DO

    return

    100 return 1

  end subroutine Load_Box_AtomsDefine


  !*****************************************
  subroutine Load_OneSecton_AtomDefine(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*256::STR
    character*32::KEYWORD
    character*32::STRNUMB(10)
    type(ATOM)::tempAtom
    integer::AtomNumb
    integer::N
    integer::LINE
    logical::isMatrixAtom
    !---Body---

    AtomNumb = 0

    isMatrixAtom = .FALSE.

    call tempAtom%CleanAtom()

    if(.not. associated(this%Atoms_list)) then
        allocate(this%Atoms_list)
    end if

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case ("&ENDSUBCTL")
                exit
            case("&NATOM")
                call EXTRACT_NUMB(STR,1,N,STRNUMB)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR : Too few parameters for Atoms number"
                    write(*,*) "You should special: '&NATOM    the number of atoms in the group = 1' "
                    pause
                    stop
                else
                    AtomNumb = ISTR(STRNUMB(1))
                end if
            case("&ATOMP")
                call EXTRACT_SUBSTR(STR,1,N,STRNUMB)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR : You must define the Atom symbol"
                    write(*,*) STR
                    write(*,*) "You should special: '&ATOMP atomic symbol = (symbol), element index = ,  atomic mass= ' "
                    pause
                    stop
                end if

                tempAtom%m_Symbol =  trim(adjustl(STRNUMB(1)))

                call UPCASE(tempAtom%m_Symbol)

                call EXTRACT_NUMB(STR,2,N,STRNUMB)
                if(N .LT. 2) then
                    write(*,*) "MCPSCUERROR : Too few parameters for Atoms define"
                    write(*,*) STR
                    write(*,*) "You should special: '&ATOMP    atomic symbol = (symbol), element index = ,  atomic mass= ' "
                    pause
                    stop
                end if

                tempAtom%m_ElementIndex = ISTR(STRNUMB(1))
                tempAtom%m_AtomMass = DRSTR(STRNUMB(2))

            case("&ATOMVOLUM")
                call EXTRACT_NUMB(STR,1,N,STRNUMB)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for matrix atom,you should specify the volum(in nm^3)"
                    write(*,*) STR
                    write(*,*) "You should specify : '&ATOMVOLUM   Volum of matrix atom (in nm^3) = ' "
                    pause
                    stop
                end if

                tempAtom%m_Volum = DRSTR(STRNUMB(1))*(C_NM2CM**3)
                isMatrixAtom = .true.

            case default
                write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "At box file Line: ",LINE
                pause
                stop
            case("&STAT")
                ! @todo (zhail#1#):
        end select
    END DO

    call this%Atoms_list%AppendOne(tempAtom,AtomNumb)

    if(isMatrixAtom .eq. .true.) then
        this%MatrixAtom = tempAtom
    end if

    return

    100 return 1
  end subroutine Load_OneSecton_AtomDefine



  !*********************************************
  subroutine Load_Box_Diffusors(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*256::STR
    character*32::KEYWORD
    integer::LINE
    !---Body---

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&DIFFUSORDEFSUBCTL")
                call this%LoadDiffusorsValue(hBoxFile,*100)
            case("&REACTDEFSUBCTL")
                call this%LoadReactions(hBoxFile,*100)
            case default
                write(*,*) "MCPSCUERROR: Illegal symbol:",KEYWORD
                write(*,*) "Please check box file at Line: ",LINE
                pause
                stop
        end select

    END DO

    return

    100 return 1
  end subroutine Load_Box_Diffusors


  !************************************************
  subroutine LoadDiffusorsValue(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*256::STR
    character*32::KEYWORD
    integer::LINE
    character*32::STRNUMB(10)
    type(ReadDiffusorPropList),pointer::cursor=>null()
    !---Body---
    allocate(this%ReadDiffusorProp_List)

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        SELECT CASE(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&DIFFUSOR")
                call this%LoadOneDiffusors(hBoxFile,*100)
            case("&FUNCSUBCTL")
                call this%LoadDiffusorsValueFromScript(hBoxFile,*100)
            case default
                write(*,*) "MCPSCUERROR: Illegal Keyword: ",KEYWORD
                write(*,*) "Please check box file at Line: ",LINE
                pause
                STOP

        END SELECT

    END DO

    cursor=>this%ReadDiffusorProp_List

    DO While(associated(cursor))
        call UPCASE(cursor%Diffusor%symbol)
        cursor=>cursor%next
    END DO

    Nullify(cursor)

    call this%ReadDiffusorProp_List%ConvertToDiffusorsTypesMap(this%Atoms_list,this%m_DiffusorTypesMap)

    return
    100 return 1
  end subroutine LoadDiffusorsValue

  !*******************************************
  subroutine LoadOneDiffusors(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*32::KEYWORD
    character*256::STR
    integer::LINE
    character*32::STRNUMB(10)
    type(ReadedDiffusorValue)::newDiffusor
    integer::N
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        SELECT CASE(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&SYMBOL")
                call EXTRACT_SUBSTR(STR,1,N,STRNUMB)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: You must specialize the diffusors symbol by 'Element1'#'number of Element1'@'Element2'#'number of Element2' "
                    pause
                    stop
                end if
                newDiffusor%symbol = trim(adjustl(STRNUMB(1)))
            case("&DIFFCOEFFVALUE")
                call EXTRACT_NUMB(STR,1,N,STRNUMB)

                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: You must special the diffusor value type."
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
                end if

                newDiffusor%DiffusorValueType = ISTR(STRNUMB(1))

                if(newDiffusor%DiffusorValueType .eq. p_DiffuseCoefficient_ByValue) then
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: If you had used the by-diffusionValue strategy, you should give the diffusor value."
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if
                    newDiffusor%DiffuseCoefficient_Value = DRSTR(STRNUMB(2))
                else if(newDiffusor%DiffusorValueType .eq. p_DiffuseCoefficient_ByArrhenius) then
                    call EXTRACT_NUMB(STR,3,N,STRNUMB)

                    if(N .LT. 3) then
                        write(*,*) "MCPSCUERROR: If you had used the by-Arrhenius strategy, you should give the prefacotr and active energy."
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if
                    newDiffusor%PreFactor = DRSTR(STRNUMB(2))
                    newDiffusor%ActEnergy = DRSTR(STRNUMB(3))
                else if(newDiffusor%DiffusorValueType .ne. p_DiffuseCoefficient_ByBCluster) then
                    write(*,*) "MCPSCUERROR: unknown diffusor value type :",newDiffusor%DiffusorValueType
                    write(*,*) "At line: ",LINE
                    pause
                    stop
                end if

            case("&ECR")
                call EXTRACT_NUMB(STR,1,N,STRNUMB)

                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: You must special the ECR value type."
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
                end if

                newDiffusor%ECRValueType = ISTR(STRNUMB(1))

                if(newDiffusor%ECRValueType .eq. p_ECR_ByValue) then
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: If you had used the by-ECRValue strategy, you should give the ECR value."
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if
                    newDiffusor%ECR = DRSTR(STRNUMB(2))*this%LatticeLength
                end if

            case default
                write(*,*) "MCPSCUERROR: The unknown symbol: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "Please check box file at Line: ",LINE
                pause
                stop
        END SELECT

    END DO

    call this%ReadDiffusorProp_List%AppendOne_ReadDiffusorPropList(newDiffusor)
    return

    100 return 1
  end subroutine LoadOneDiffusors

  !*******************************************
  subroutine LoadDiffusorsValueFromScript(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*256::STR
    character*32::KEYWORD
    character(kind=c_char,len=10000)::scriptStr
    integer::LINE
    !---Body---
    scriptStr = ''

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
        end select

        scriptStr = scriptStr(1:LEN_TRIM(scriptStr))//STR(1:LEN_TRIM(STR))//"\n"

    END DO

    scriptStr(LEN_TRIM(scriptStr):LEN_TRIM(scriptStr)+1) = CHAR(0)

    call ResloveDiffusorsValueFromCScript(scriptStr,this%ReadDiffusorProp_List)

    return
    100 return 1
  end subroutine LoadDiffusorsValueFromScript

  !************************************************
  subroutine LoadReactions(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*256::STR
    character*32::KEYWORD
    integer::LINE
    character*32::STRNUMB(10)
    type(ReadReactionPropList),pointer::cursor=>null()
    !---Body---
    allocate(this%ReadReactionProp_List)

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        SELECT CASE(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&REACTION")
                call this%LoadOneReaction(hBoxFile,*100)
            case("&FUNCSUBCTL")
                call this%LoadReactionsFromScript(hBoxFile,*100)
            case default
                write(*,*) "MCPSCUERROR: Illegal Keyword: ",KEYWORD
                write(*,*) "Please check box file at Line: ",LINE
                pause
                STOP

        END SELECT

    END DO

    cursor=>this%ReadReactionProp_List

    DO While(associated(cursor))
        call UPCASE(cursor%Reaction%SubjectSymbol)
        call UPCASE(cursor%Reaction%ObjectSymbol)
        cursor=>cursor%next
    END DO

    Nullify(cursor)

    call this%ReadReactionProp_List%ConvertToReactionsMap(this%Atoms_list,this%m_ReactionsMap)

    return
    100 return 1
  end subroutine LoadReactions

  !*******************************************
  subroutine LoadOneReaction(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*32::KEYWORD
    character*256::STR
    integer::LINE
    character*32::STRNUMB(10)
    type(ReadReactionPair)::newReactionPair
    integer::N
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        SELECT CASE(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&REACTPAIRS")
                call EXTRACT_SUBSTR(STR,2,N,STRNUMB)
                if(N .LT. 2) then
                    write(*,*) "MCPSCUERROR: You must specialize the &REACTPAIRS The reaction pairs by 'symbol of subject cluster', 'symbol of oubject cluster' "
                    pause
                    stop
                end if
                newReactionPair%SubjectSymbol = trim(adjustl(STRNUMB(1)))
                newReactionPair%ObjectSymbol = trim(adjustl(STRNUMB(2)))
            case("&REACTCOEFF")
                call EXTRACT_NUMB(STR,1,N,STRNUMB)

                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: You must special the reaction coefficients type."
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
                end if

                newReactionPair%ReactionCoefficientType = ISTR(STRNUMB(1))

                if(newReactionPair%ReactionCoefficientType .eq. p_ReactionCoefficient_ByValue) then
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: If you had used the reaction coefficients by-value strategy, you should give the corresponded value."
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if
                    newReactionPair%ReactionCoefficient_Value = DRSTR(STRNUMB(2))
                else if(newReactionPair%ReactionCoefficientType .eq. p_ReactionCoefficient_ByArrhenius) then
                    call EXTRACT_NUMB(STR,3,N,STRNUMB)

                    if(N .LT. 3) then
                        write(*,*) "MCPSCUERROR: If you had used reaction coefficients by-Arrhenius strategy , you should give the prefacotr and active energy."
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if
                    newReactionPair%PreFactor = DRSTR(STRNUMB(2))
                    newReactionPair%ActEnergy = DRSTR(STRNUMB(3))
                else
                    write(*,*) "MCPSCUERROR: unknown reaction coefficients type :",newReactionPair%ReactionCoefficientType
                    write(*,*) "At line: ",LINE
                    pause
                    stop
                end if

            case("&ECR")
                call EXTRACT_NUMB(STR,1,N,STRNUMB)

                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: You must special the ECR value type."
                    write(*,*) "At Line: ",LINE
                    pause
                    stop
                end if

                newReactionPair%ECRValueType = ISTR(STRNUMB(1))

                if(newReactionPair%ECRValueType .eq. p_ECR_ByValue) then
                    call EXTRACT_NUMB(STR,2,N,STRNUMB)
                    if(N .LT. 2) then
                        write(*,*) "MCPSCUERROR: If you had used the by-ECRValue strategy, you should give the ECR value (LU)."
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if
                    newReactionPair%ECR = DRSTR(STRNUMB(2))*this%LatticeLength
                end if

            case default
                write(*,*) "MCPSCUERROR: The unknown symbol: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "Please check box file at Line: ",LINE
                pause
                stop
        END SELECT

    END DO

    call this%ReadReactionProp_List%AppendOne_ReadReactionPropList(newReactionPair)
    return

    100 return 1
  end subroutine LoadOneReaction

  !*******************************************
  subroutine LoadReactionsFromScript(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    character*256::STR
    character*32::KEYWORD
    character(kind=c_char,len=10000)::scriptStr
    integer::LINE
    !---Body---
    scriptStr = ''

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)
        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
        end select

        scriptStr = scriptStr(1:LEN_TRIM(scriptStr))//STR(1:LEN_TRIM(STR))//"\n"

    END DO

    scriptStr(LEN_TRIM(scriptStr):LEN_TRIM(scriptStr)+1) = CHAR(0)

    call ResloveReactionsFromCScript(scriptStr,this%ReadReactionProp_List)

    return
    100 return 1
  end subroutine LoadReactionsFromScript

  !**********************************************
  subroutine Load_Box_GrainBoundary(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRTEMP(10)
    !---Body---

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&SIMPLEDISTSUBCTL")
                this%m_GrainBoundary%GBInitType = p_GBIniConfig_Simple
                call this%Load_GB_Simple(hBoxFile,*100)

            case("&FILEDISTSUBCTL")
                this%m_GrainBoundary%GBInitType = p_GBIniConfig_SpecialDistFromFile
                call this%Load_GB_SpecialDistFromFile(hBoxFile,*100)

            case("&EXTFUNCDISTSUBCTL")
                this%m_GrainBoundary%GBInitType = p_GBIniConfig_SpecialDistFromExteFunc
                call this%Load_GB_SpecialDistFromExteFunc(hBoxFile,*100)

            case default
                write(*,*) "MCPSCUERROR: unKnown type to for grain boundary distribution!"
                write(*,*) KEYWORD
                pause
                stop
        end select

    END DO

    return
    100 return 1
  end subroutine Load_Box_GrainBoundary

  !*********************************************
  subroutine Load_GB_Simple(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRTEMP(10)
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&GRAINSNUMBER")
                call EXTRACT_NUMB(STR,1,N,STRTEMP)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the minial (cut-off) distance between seeds."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &CUTOFF   The minial (cut-off) distance between seeds ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%GrainNum = ISTR(STRTEMP(1))
            case("&BYSEEDSUBCTL")
                this%m_GrainBoundary%GBInitSimple_Strategy = p_GBInitSimple_BySeedCtl
                call this%Load_GB_Simple_Distribution_ByGSeedCtl(hBoxFile,*100)
            case("&BYGVOLUMSUBCTL")
                this%m_GrainBoundary%GBInitSimple_Strategy = p_GBInitSimple_ByGVolumCtl
                call this%Load_GB_Simple_Distribution_ByGVolumCtl(hBoxFile,*100)
            case default
                write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "At box file Line: ",LINE
                pause
                stop
        end select

    END DO

    return

    100 return 1
  end subroutine Load_GB_Simple

  !*********************************************
  subroutine Load_GB_Simple_Distribution_ByGSeedCtl(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRTEMP(10)
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&MINCUTOFF")
                call EXTRACT_NUMB(STR,1,N,STRTEMP)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the minial (cut-off) distance between seeds."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &MINCUTOFF   The minial (cut-off) distance between seeds ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%Cutoff(1) = DRSTR(STRTEMP(1))*this%LatticeLength
            case("&MAXCUTOFF")
                call EXTRACT_NUMB(STR,1,N,STRTEMP)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the max (cut-off) distance between seeds."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &MAXCUTOFF   The max (cut-off) distance between seeds ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%Cutoff(2) = DRSTR(STRTEMP(1))*this%LatticeLength
            case("&DISTANCE_GAUSS")
                call EXTRACT_NUMB(STR,2,N,STRTEMP)
                if(N .LT. 2) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the distance distribution between seeds."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &DISTANCE_GAUSS THE GAUSS DISTRIBUTION CENTRAL =, THE HALF WIDTH ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%SeedsDistINI = DRSTR(STRTEMP(1))*this%LatticeLength
                this%m_GrainBoundary%SeedsDistSD = DRSTR(STRTEMP(2))*this%LatticeLength
            case default
                write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "At box file Line: ",LINE
                pause
                stop
        end select

    END DO

    if(this%m_GrainBoundary%Cutoff(2) .LT. this%m_GrainBoundary%Cutoff(1)) then
        write(*,*) "MCPSCUERROR: the Cut-off distance setting error."
        write(*,*) "Min cut-off: ",this%m_GrainBoundary%Cutoff(1)
        write(*,*) "Max cut-off: ",this%m_GrainBoundary%Cutoff(2)
        pause
        stop
    end if

    return
    100 return 1
  end subroutine Load_GB_Simple_Distribution_ByGSeedCtl

  !*********************************************
  subroutine Load_GB_Simple_Distribution_ByGVolumCtl(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRTEMP(10)
    !---Body---
    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&MINCUTOFF")
                call EXTRACT_NUMB(STR,1,N,STRTEMP)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the minial (cut-off) volum between grains."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &MINCUTOFF The minial(cut-off) volum for grain ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%Cutoff(1) = DRSTR(STRTEMP(1))*(this%LatticeLength**3)
            case("&MAXCUTOFF")
                call EXTRACT_NUMB(STR,1,N,STRTEMP)
                if(N .LT. 1) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the max (cut-off) volum between grains."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &MAXCUTOFF The max(cut-off) volum for grain ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%Cutoff(2) = DRSTR(STRTEMP(2))*(this%LatticeLength**3)
            case("&VOLUM_GAUSS")
                call EXTRACT_NUMB(STR,2,N,STRTEMP)
                if(N .LT. 2) then
                    write(*,*) "MCPSCUERROR: Too few parameters for the volum distribution between grains."
                    write(*,*) "At line: ",LINE
                    write(*,*) "You should special: &VOLUM_GAUSS THE GAUSS DISTRIBUTION CENTRAL =, THE HALF WIDTH ="
                    pause
                    stop
                end if
                this%m_GrainBoundary%GVolumINI = DRSTR(STRTEMP(1))*(this%LatticeLength**3)
                this%m_GrainBoundary%GVolumSD  = DRSTR(STRTEMP(2))*(this%LatticeLength**3)
            case default
                write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "At box file Line: ",LINE
                pause
                stop
        end select

    END DO

    if(this%m_GrainBoundary%Cutoff(2) .LT. this%m_GrainBoundary%Cutoff(1)) then
        write(*,*) "MCPSCUERROR: the Cut-off distance setting error."
        write(*,*) "Min cut-off: ",this%m_GrainBoundary%Cutoff(1)
        write(*,*) "Max cut-off: ",this%m_GrainBoundary%Cutoff(2)
        pause
        stop
    end if

    return
    100 return 1
  end subroutine Load_GB_Simple_Distribution_ByGVolumCtl

  !*********************************************
  subroutine Load_GB_SpecialDistFromFile(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*10::STRTMP(10)
    !---Body---

    DO While(.true.)
        call GETINPUTSTRLINE(hBoxFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")
        STR = adjustl(STR)
        call GETKEYWORD("&",STR,KEYWORD)
        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDSUBCTL")
                exit
            case("&FGBDIST")
                call EXTRACT_SUBSTR(STR,1,N,STRTMP)

                if(N. LT. 1) then
                    write(*,*) "MCPSCUERROR: You must special the grain boundary configuration file path !"
                    write(*,*) "At line : ",LINE
                    pause
                    stop
                end if

                if(LENTRIM(STRTMP(1)) .LE. 0) then
                    write(*,*) "MCPSCUERROR: The grain boundary configuration file path is null !"
                    pause
                    stop
                end if

                this%m_GrainBoundary%GBCfgFileName = adjustl((trim(STRTMP(1))))
            case default
                write(*,*) "MCPSCUERROR: The Illegal flag: ",KEYWORD(1:LENTRIM(KEYWORD))
                write(*,*) "At box file Line: ",LINE
                pause
                stop
        end select

    END DO

    return
    100 return 1
  end subroutine Load_GB_SpecialDistFromFile

  !*********************************************
  subroutine Load_GB_SpecialDistFromExteFunc(this,hBoxFile,*)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    integer,intent(in)::hBoxFile
    !---Local Vars---
    return
    100 return 1
  end subroutine Load_GB_SpecialDistFromExteFunc

  !*****************************************************************
  subroutine Expand_ClustersInfor_CPU_EqualNum(this,Host_SimuCtrlParam,ExpandNCNumEachBox)
    implicit none
    !-----Dummy Vars-------
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    integer,intent(in)::ExpandNCNumEachBox
    !---Local Vars---
    integer::MultiBox
    integer::NeighborsNum
    integer::IStatu
    integer::I
    type(ACluster), dimension(:), allocatable::tempOldClusters
    integer, dimension(:), allocatable::tempOldActiveIndex
    integer::IBox
    integer::OldTotalSize
    integer::NewTotalSize
    integer::ICFrom,ICTo
    integer::ICFromNew,ICToNew
    integer::ExpandStartNew
    integer::VirtualNum
    integer::UsedNum
    integer::ExpdNum
    !---Body-----------

    if(ExpandNCNumEachBox.LE. 0) then
        return
    end if

    MultiBox = Host_SimuCtrlParam%MultiBox

    NeighborsNum = Host_SimuCtrlParam%MAXNEIGHBORNUM

    if(this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
        OldTotalSize = this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
    else
        OldTotalSize = 0
    end if

    NewTotalSize = OldTotalSize + MultiBox*ExpandNCNumEachBox

    if(OldTotalSize .GT. 0) then

        call AllocateArray_Host(tempOldClusters,OldTotalSize,"tempOldClusters")

        call AllocateArray_Host(tempOldActiveIndex,OldTotalSize,"tempOldActiveIndex")

        tempOldClusters = this%m_ClustersInfo_CPU%m_Clusters

        tempOldActiveIndex = this%m_ClustersInfo_CPU%m_ActiveIndex

        call this%m_ClustersInfo_CPU%Clean()

        call this%m_ClustersInfo_CPU%AllocateClustersInfo_CPU(NewTotalSize,NeighborsNum)

        DO IBox = 1, MultiBox

            if(this%m_BoxesInfo%SEUsedIndexBox(IBox,2) .GT. 0) then
                UsedNum = this%m_BoxesInfo%SEUsedIndexBox(IBox,2) - this%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            else
                UsedNum = 0
            end if

            if(this%m_BoxesInfo%SEExpdIndexBox(IBox,2) .GT. 0) then
                ExpdNum = this%m_BoxesInfo%SEExpdIndexBox(IBox,2) - this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + 1
            else
                ExpdNum = 0
            end if

            if(this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .GT. 0) then
                VirtualNum = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            else
                VirtualNum = 0
            end if

            ICFrom = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            ICTo   = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

            if(IBox .ne. 1) then
                ICFromNew = this%m_BoxesInfo%SEVirtualIndexBox(IBox-1,2) + 1
            else
                ICFromNew = 1
            end if

            ExpandStartNew = ICFromNew + VirtualNum

            ICToNew = ExpandStartNew + ExpandNCNumEachBox - 1

            this%m_ClustersInfo_CPU%m_Clusters(ICFromNew:ExpandStartNew-1) = tempOldClusters(ICFrom:ICTo)

            this%m_ClustersInfo_CPU%m_ActiveIndex(ICFromNew:ExpandStartNew-1) = tempOldActiveIndex(ICFrom:ICTo)

            FORALL(I=ExpandStartNew:ICToNew)
                this%m_ClustersInfo_CPU%m_ActiveIndex(I) = I
            END FORALL

            this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = ICFromNew

            if(ICTO .GT. 0) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ICTO - ICFROM + ExpandNCNumEachBox
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ExpandNCNumEachBox - 1
            end if

            this%m_BoxesInfo%SEUsedIndexBox(IBox,1) = ICFromNew
            this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = ICFromNew + UsedNum - 1

            this%m_BoxesInfo%SEExpdIndexBox(IBox,1) = ICFromNew
            this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = ICFromNew + ExpdNum - 1

        END DO

        call DeAllocateArray_Host(tempOldClusters,"tempOldClusters")

        call DeAllocateArray_Host(tempOldActiveIndex,"tempOldActiveIndex")

    else if(NewTotalSize .GT. 0) then
        call this%m_ClustersInfo_CPU%AllocateClustersInfo_CPU(NewTotalSize,NeighborsNum)

        this%m_ClustersInfo_CPU%m_ActiveIndex = 0
        UsedNum = 0
        ExpdNum = 0

        DO IBox = 1, MultiBox

            ICFrom = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            ICTo   = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

            if(IBox .ne. 1) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox-1,2) + 1
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = 1
            end if

            if(ICTO .GT. 0) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ICTO - ICFROM + ExpandNCNumEachBox
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ExpandNCNumEachBox - 1
            end if

            this%m_BoxesInfo%SEUsedIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,1) + UsedNum - 1

            this%m_BoxesInfo%SEExpdIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + ExpdNum - 1

        END DO

    end if

    return
  end subroutine Expand_ClustersInfor_CPU_EqualNum

  !*****************************************************************
  subroutine Expand_ClustersInfor_CPU_BoxByBox(this,Host_SimuCtrlParam,ExpandNCNum)
    implicit none
    !-----Dummy Vars-------
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    integer,intent(in)::ExpandNCNum(:)
        !---Local Vars---
    integer::MultiBox
    integer::NeighborsNum
    integer::IStatu
    integer::I
    type(ACluster), dimension(:), allocatable::tempOldClusters
    integer, dimension(:), allocatable::tempOldActiveIndex
    integer::IBox
    integer::OldTotalSize
    integer::NewTotalSize
    integer::ICFrom,ICTo
    integer::ICFromNew,ICToNew
    integer::ExpandStartNew
    integer::VirtualNum
    integer::UsedNum
    integer::ExpdNum
    !---Body-----------

    if(sum(ExpandNCNum).LE. 0) then
        return
    end if

    MultiBox = Host_SimuCtrlParam%MultiBox

    NeighborsNum = Host_SimuCtrlParam%MAXNEIGHBORNUM

    if(this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
        OldTotalSize = this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
    else
        OldTotalSize = 0
    end if

    NewTotalSize = OldTotalSize + sum(ExpandNCNum)

    if(OldTotalSize .GT. 0) then

        call AllocateArray_Host(tempOldClusters,OldTotalSize,"tempOldClusters")

        call AllocateArray_Host(tempOldActiveIndex,OldTotalSize,"tempOldActiveIndex")

        tempOldClusters = this%m_ClustersInfo_CPU%m_Clusters

        tempOldActiveIndex = this%m_ClustersInfo_CPU%m_ActiveIndex

        call this%m_ClustersInfo_CPU%Clean()

        call this%m_ClustersInfo_CPU%AllocateClustersInfo_CPU(NewTotalSize,NeighborsNum)

        DO IBox = 1, MultiBox

            if(this%m_BoxesInfo%SEUsedIndexBox(IBox,2) .GT. 0) then
                UsedNum = this%m_BoxesInfo%SEUsedIndexBox(IBox,2) - this%m_BoxesInfo%SEUsedIndexBox(IBox,1) + 1
            else
                UsedNum = 0
            end if

            if(this%m_BoxesInfo%SEExpdIndexBox(IBox,2) .GT. 0) then
                ExpdNum = this%m_BoxesInfo%SEExpdIndexBox(IBox,2) - this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + 1
            else
                ExpdNum = 0
            end if

            if(this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .GT. 0) then
                VirtualNum = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            else
                VirtualNum = 0
            end if

            ICFrom = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            ICTo   = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

            if(IBox .ne. 1) then
                ICFromNew = this%m_BoxesInfo%SEVirtualIndexBox(IBox-1,2) + 1
            else
                ICFromNew = 1
            end if

            ExpandStartNew = ICFromNew + VirtualNum

            ICToNew = ExpandStartNew + ExpandNCNum(IBox) - 1

            this%m_ClustersInfo_CPU%m_Clusters(ICFromNew:ExpandStartNew-1) = tempOldClusters(ICFrom:ICTo)

            this%m_ClustersInfo_CPU%m_ActiveIndex(ICFromNew:ExpandStartNew-1) = tempOldActiveIndex(ICFrom:ICTo)

            FORALL(I=ExpandStartNew:ICToNew)
                this%m_ClustersInfo_CPU%m_ActiveIndex(I) = I
            END FORALL

            this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = ICFromNew

            if(ICTO .GT. 0) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ICTO - ICFROM + ExpandNCNum(IBox)
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ExpandNCNum(IBox) - 1
            end if

            this%m_BoxesInfo%SEUsedIndexBox(IBox,1) = ICFromNew
            this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = ICFromNew + UsedNum - 1

            this%m_BoxesInfo%SEExpdIndexBox(IBox,1) = ICFromNew
            this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = ICFromNew + ExpdNum - 1

        END DO

        call DeAllocateArray_Host(tempOldClusters,"tempOldClusters")

        call DeAllocateArray_Host(tempOldActiveIndex,"tempOldActiveIndex")

    else if(NewTotalSize .GT. 0) then
        call this%m_ClustersInfo_CPU%AllocateClustersInfo_CPU(NewTotalSize,NeighborsNum)

        this%m_ClustersInfo_CPU%m_ActiveIndex = 0
        UsedNum = 0
        ExpdNum = 0

        DO IBox = 1, MultiBox

            ICFrom = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            ICTO   = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2)

            if(IBox .ne. 1) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox-1,2) + 1
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = 1
            end if

            if(ICTO .GT. 0) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ICTO - ICFROM + ExpandNCNum(IBox)
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + ExpandNCNum(IBox) - 1
            end if

            this%m_BoxesInfo%SEUsedIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,1) + UsedNum - 1

            this%m_BoxesInfo%SEExpdIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + ExpdNum - 1

        END DO

    end if

    return
  end subroutine Expand_ClustersInfor_CPU_BoxByBox


  !****************************************************
  subroutine Rescale_Boxes_CPU(this,Host_SimuCtrlParam, DUPXYZ)
        implicit none
        !---Dummy Vars---
        CLASS(SimulationBoxes)::this
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer,intent(in)::DUPXYZ(3)
        !---Local Vars---
        integer::MultiBox
        integer::NeighborNum
        type(ClustersInfo_CPU)::temp_ClustersInfo
        real(kind=KMCDF)::tempBOXSIZE(3)
        integer::DumplicateNum
        integer::NCDUP
        integer::ICFROM,ICTO
        integer::IBox
        integer::IP
        integer::IC,I,J,K
        integer::TheStatu
        integer,dimension(:),allocatable::NActiveUsed
        integer::GrainSeedsNum
        integer::IDump
        integer::NCDumpAddEachBox
        integer::NCDumpAddTotal
        !---Body---

        MultiBox = Host_SimuCtrlParam%MultiBox

        NeighborNum = Host_SimuCtrlParam%MAXNEIGHBORNUM

        GrainSeedsNum = this%m_GrainBoundary%GrainNum

        tempBOXSIZE = this%BOXSIZE

        DumplicateNum = (DUPXYZ(1)+1)*(DUPXYZ(2)+1)*(DUPXYZ(3)+1)

        if(DumplicateNum .LE. 0) then
            write(*,*) "MCPSCUERROR: The  boxes dumplicate number less than 0 !!"
            pause
            stop
        end if

        NCDUP = DumplicateNum*(this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEINGB_STATU))

        call temp_ClustersInfo%AllocateClustersInfo_CPU(NCDUP,NeighborNum)

        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC = 0

        IP = 1

        call AllocateArray_Host(NActiveUsed,MultiBox,"NActiveUsed")

        NActiveUsed = 0

        DO IBox = 1, MultiBox

            NCDumpAddEachBox = 0

            ICFROM = this%m_BoxesInfo%SEUsedIndexBox(IBox,1)

            ICTO   = this%m_BoxesInfo%SEUsedIndexBox(IBox,2)

            this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC = 0

            if(ICTO .GT. 0) then
                    DO IC = ICFROM, ICTO
                        TheStatu = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

                        if(TheStatu .eq. p_ACTIVEFREE_STATU .or. TheStatu .eq. p_ACTIVEINGB_STATU) then

                            IDump = 0
                            DO I=0, DUPXYZ(3)
                                DO J=0, DUPXYZ(2)
                                    DO K=0, DUPXYZ(1)

                                        if(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD .LT. 3*10**-9) then
                                            write(*,*) "OPPS....."
                                            pause
                                            stop
                                        end if

                                        temp_ClustersInfo%m_Clusters(IP)%m_POS(1) = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1) + dble(K)*tempBOXSIZE(1)
                                        temp_ClustersInfo%m_Clusters(IP)%m_POS(2) = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(2) + dble(J)*tempBOXSIZE(2)
                                        temp_ClustersInfo%m_Clusters(IP)%m_POS(3) = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(3) + dble(I)*tempBOXSIZE(3)
                                        temp_ClustersInfo%m_Clusters(IP)%m_Atoms  = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms
                                        temp_ClustersInfo%m_Clusters(IP)%m_RAD    = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD
                                        temp_ClustersInfo%m_Clusters(IP)%m_Layer  = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer
                                        temp_ClustersInfo%m_Clusters(IP)%m_Statu  = TheStatu
                                        temp_ClustersInfo%m_Clusters(IP)%m_DiffCoeff = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff

                                        temp_ClustersInfo%m_ActiveIndex(IP) = IP

                                        if(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) .GT. 0) then
                                            temp_ClustersInfo%m_Clusters(IP)%m_GrainID = IDump*GrainSeedsNum + this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID
                                        end if

                                        IDump = IDump + 1
                                        IP = IP + 1

                                        NActiveUsed(IBox) = NActiveUsed(IBox) + 1

                                        this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(TheStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(TheStatu) + 1

                                        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(TheStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(TheStatu) + 1

                                        NCDumpAddEachBox = NCDumpAddEachBox + 1

                                    END DO
                                END DO
                            END DO
                        end if
                    END DO
            end if

            NCDumpAddEachBox = (DumplicateNum - 1)*NCDumpAddEachBox/DumplicateNum

            this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NCDumpAdded = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NCDumpAdded + NCDumpAddEachBox

        END DO

        if((this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) +this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEINGB_STATU) ) .ne. NCDUP) then
            write(*,*) "MCPSCUERROR: Dumplicate box failed, the clusters number is not same with goal!", &
                        (this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) +this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEINGB_STATU) ) ,&
                        NCDUP
            pause
            stop
        end if

        NCDumpAddTotal = (DumplicateNum - 1)*NCDUP/DumplicateNum

        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NCDumpAdded = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NCDumpAdded + NCDumpAddTotal

        if(sum(this%m_BoxesBasicStatistic%BoxesStatis_Single(1:MultiBox)%NCDumpAdded) .ne. this%m_BoxesBasicStatistic%BoxesStatis_Integral%NCDumpAdded) then
            write(*,*) "MCPSCUERROR: You need to check the dumplicate number ."
            write(*,*) "The sum of dumplicate number for all boxes is :",sum(this%m_BoxesBasicStatistic%BoxesStatis_Single(1:MultiBox)%NCDumpAdded)
            write(*,*) "However, the recorded dumplicate number for the integral box is ",this%m_BoxesBasicStatistic%BoxesStatis_Integral%NCDumpAdded
            pause
            stop
        end if

        DO IBox = 1,MultiBox
                if(IBox .eq. 1) then
                    this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = 1
                else
                    this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox-1,2) + 1
                end if

                this%m_BoxesInfo%SEUsedIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)

                this%m_BoxesInfo%SEExpdIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)

                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + NActiveUsed(IBox) - 1

                this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,1) + NActiveUsed(IBox) - 1

                this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + NActiveUsed(IBox) - 1
        END DO

        call this%m_ClustersInfo_CPU%Clean()

        call this%m_ClustersInfo_CPU%AllocateClustersInfo_CPU(NCDUP,NeighborNum)

        ! The Assignment(=) had been override
        this%m_ClustersInfo_CPU = temp_ClustersInfo

        !rescale the geometry size of box
        this%BOXSIZE = tempBOXSIZE*(DUPXYZ+1)
        DO I=1,3
            this%BOXBOUNDARY(I,2) =  this%BOXBOUNDARY(I,1) + this%BOXSIZE(I)
        End DO
        this%HBOXSIZE = 0.5D0*this%BOXSIZE
        this%BOXVOLUM = PRODUCT(this%BOXSIZE)

        call this%m_GrainBoundary%RescaleGrainBoundary(DUPXYZ)

        call temp_ClustersInfo%Clean()

        call DeAllocateArray_Host(NActiveUsed,"NActiveUsed")

        return
  end subroutine Rescale_Boxes_CPU

  !*************************************************************
  subroutine Sweep_UnActiveMemory_CPU(this,Host_SimuCtrlParam)
    implicit none
    !-----Dummy Vars-------
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    !---Local Vars---
    integer::MultiBox
    integer::NeighborsNum
    integer::TheStatu
    integer::I
    type(ACluster), dimension(:), allocatable::tempClusters
    integer, dimension(:), allocatable::tempActiveIndex
    integer::IBox
    integer::OldTotalSize
    integer::NewTotalSize
    integer::ICFrom
    integer::ICTo
    integer::ICTOVirtual
    integer::IP
    integer::NCUSed
    integer::NCExpd
    integer::NCVirtual
    integer::NCUnAct
    !---Body-----------

    MultiBox = Host_SimuCtrlParam%MultiBox

    NeighborsNum = Host_SimuCtrlParam%MAXNEIGHBORNUM

    if(this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
        OldTotalSize = this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
    else
        OldTotalSize = 0
    end if


    if(OldTotalSize .GE. 1) then

        call AllocateArray_Host(tempClusters,OldTotalSize,"tempClusters")

        call AllocateArray_Host(tempActiveIndex,OldTotalSize,"tempActiveIndex")

        IP = 1

        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC = 0

        DO IBox = 1, MultiBox

            ICFrom = this%m_BoxesInfo%SEUsedIndexBox(IBox,1)
            ICTo   = this%m_BoxesInfo%SEUsedIndexBox(IBox,2)
            if(ICTo .LE. 0) then
                NCUSed = 0
            else
                NCUSed = ICTo - ICFrom + 1
            end if

            if(this%m_BoxesInfo%SEExpdIndexBox(IBox,2) .GT. 0) then
                NCExpd = this%m_BoxesInfo%SEExpdIndexBox(IBox,2) - this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + 1
            else
                NCExpd = 0
            end if

            if(this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) .GT. 0) then
                NCVirtual = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + 1
            else
                NCVirtual = 0
            end if

            NCUnAct = 0

            this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC = 0

            if(ICTO .GT. 0) then
              DO I = ICFrom,ICTo
                TheStatu = this%m_ClustersInfo_CPU%m_Clusters(I)%m_Statu

                if(TheStatu .eq. p_ACTIVEFREE_STATU .or. TheStatu .eq. p_ACTIVEINGB_STATU) then

                    tempClusters(IP) = this%m_ClustersInfo_CPU%m_Clusters(I)

                    tempActiveIndex(IP) = IP

                    IP = IP + 1

                    this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(TheStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(TheStatu) + 1
                else
                    NCUnAct = NCUnAct + 1
                end if

              END DO

            end if

            if(IBox .eq. 1) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = 1
            else
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox-1,2) + 1
            end if

            if(NCVirtual .LE. NCUnAct) then
                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) - 1
            else
                ICTOVirtual = this%m_BoxesInfo%SEVirtualIndexBox(IBox,2)
                tempClusters(IP:IP + NCVirtual - NCUSed - 1) = this%m_ClustersInfo_CPU%m_Clusters(ICTO+1:ICTOVirtual)
                forall(I=IP:IP + NCVirtual - NCUSed - 1)
                    tempActiveIndex(I) = I
                end forall
                IP = IP + ICTOVirtual - ICTo

                this%m_BoxesInfo%SEVirtualIndexBox(IBox,2) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1) + NCVirtual - 1 - NCUnAct
            end if

            this%m_BoxesInfo%SEUsedIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            if(NCUSed .LE. NCUnAct) then
                this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,1) - 1
            else
                this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,1) + NCUSed - 1 - NCUnAct
            end if

            this%m_BoxesInfo%SEExpdIndexBox(IBox,1) = this%m_BoxesInfo%SEVirtualIndexBox(IBox,1)
            if(NCExpd .LE. NCUnAct) then
                this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,1) - 1
            else
                this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,1) + NCExpd - 1 - NCUnAct
            end if

            this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC + this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC

        END DO

        if(this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) .GT. 0) then
            NewTotalSize = this%m_BoxesInfo%SEVirtualIndexBox(MultiBox,2) - this%m_BoxesInfo%SEVirtualIndexBox(1,1) + 1
        else
            NewTotalSize = 0
        end if

        call this%m_ClustersInfo_CPU%Clean()

        call this%m_ClustersInfo_CPU%AllocateClustersInfo_CPU(NewTotalSize,NeighborsNum)

        this%m_ClustersInfo_CPU%m_Clusters = tempClusters(1:NewTotalSize)

        this%m_ClustersInfo_CPU%m_ActiveIndex = tempActiveIndex(1:NewTotalSize)

        call DeAllocateArray_Host(tempClusters,"tempClusters")

        call DeAllocateArray_Host(tempActiveIndex,"tempActiveIndex")

    end if

    return
  end subroutine Sweep_UnActiveMemory_CPU

  !**********************OutPut***************************
  subroutine Puout_Instance_Config_SimBoxArray(this,Host_SimuCtrlParam,SimuRecord,RescaleCount)
    !***    Purpose: to output Intermediate Status
    !           this: the boxes info in host
    !           SimuRecord: the simulation records
    !        RescaleCount : (optional)the ith rescale
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    Class(SimulationRecord)::SimuRecord
    integer, optional::RescaleCount
    !---Local Vars---
    type(AtomsList),pointer::cursor=>null()
    character*256::c_ITIME
    character*256::C_TIMESECTION
    character*256::C_JOB
    integer::MultiBox
    integer::IBox
    integer::IAKind
    character*256::path
    integer::hFile
    integer::IC, ICFROM, ICTO
    integer::ISeed
    integer::ILayer
    integer::LayerNum
    character*32::KEYWORD
    character*256::CFormat
    character*256::CNUM
    character*15::AtomsStr(p_ATOMS_GROUPS_NUMBER)
    integer::tempLen
    integer::ElementsKind
    !---Body---

    if(present(RescaleCount)) then
        write(c_ITIME,*) RescaleCount
        c_ITIME = adjustl(c_ITIME)
        c_ITIME = "BeforeRescale"//c_ITIME
    else
        write(c_ITIME,*) SimuRecord%GetOutPutIndex()

        call SimuRecord%IncreaseOneOutPutIndex()

        c_ITIME = adjustl(c_ITIME)
    end if

    write(C_TIMESECTION,*) SimuRecord%GetTimeSections()
    C_TIMESECTION = adjustl(C_TIMESECTION)
    C_TIMESECTION = "Section"//C_TIMESECTION

    write(C_JOB,*) SimuRecord%GetSimuPatch()
    C_JOB = adjustl(C_JOB)
    C_JOB = "Job"//C_JOB


    ! output the configuration(can also be for the restart)
    MultiBox = Host_SimuCtrlParam%MultiBox

    path = Host_SimuCtrlParam%OutFilePath(1:LENTRIM(Host_SimuCtrlParam%OutFilePath))//FolderSpe//"Config_"//trim(C_JOB)//"_"//trim(C_TIMESECTION)//"_"//trim(c_ITIME)//".dat"

    hFile = CreateNewFile(path)

    open(hFile,file=path, form="formatted")

    !---Start to write---
    write(hFile,FMT="(A)") OKMC_OUTCFG_FORMAT18

    KEYWORD = "&TIME"
    write(hFile, FMT="(A20,1x,A15,1x,1PE18.7)") KEYWORD(1:LENTRIM(KEYWORD)),"(in s)",SimuRecord%GetSimuTimes()

    KEYWORD = "&BOXLOW"
    write(hFile, FMT="(A20,1x,A15,1x,3(1PE14.4, 1x))") KEYWORD(1:LENTRIM(KEYWORD)),   &
                                                       "(in nm)",                     &
                                                       this%BoxBoundary(1,1)*C_CM2NM, &
                                                       this%BoxBoundary(2,1)*C_CM2NM, &
                                                       this%BoxBoundary(3,1)*C_CM2NM

    KEYWORD = "&BOXSIZE"
    write(hFile, FMT="(A20,1x,A15,1x,3(1PE14.4, 1x))") KEYWORD(1:LENTRIM(KEYWORD)),  &
                                                       "(in nm)",                    &
                                                       this%BOXSIZE(1)*C_CM2NM,      &
                                                       this%BOXSIZE(2)*C_CM2NM,      &
                                                       this%BOXSIZE(3)*C_CM2NM

    KEYWORD = "&NGRAIN"
    write(hFile,FMT="(A20,1x,I8)") KEYWORD(1:LENTRIM(KEYWORD)),this%m_GrainBoundary%GrainNum
    write(hFile,FMT="(A20,1x,4(A14,1x))")  "!","Seed ID", "x(nm)", "y(nm)", "z(nm)"
    KEYWORD = "&SEEDDATA"
    Do ISeed = 1,this%m_GrainBoundary%GrainNum
        write(hFile,fmt="(A20,1x,I14, 1x, 3(1PE14.4, 1x))") KEYWORD(1:LENTRIM(KEYWORD)),ISeed,this%m_GrainBoundary%GrainSeeds(ISeed)%m_POS(1:3)*C_CM2NM
    End Do

!    CNUM = ""
!    write(CNUM,*) p_NUMBER_OF_STATU
!
!    KEYWORD = "&NCLUSTERS"
!    CFormat = ""
!    CFormat = "(2(A20,1x),"//CNUM(1:LENTRIM(CNUM))//"(A20,1x))"
!    write(hFile, FMT=CFormat(1:LENTRIM(CFormat))) KEYWORD(1:LENTRIM(KEYWORD)),"IBox",p_CStatu
!    KEYWORD = "&NCDATA"
!    CFormat = ""
!    CFormat = "(A20,1x,I20,1x,"//CNUM(1:LENTRIM(CNUM))//"(I20,1x))"
!    DO IBox = 1,MultiBox
!        write(hFile, FMT=CFormat(1:LENTRIM(CFormat))) KEYWORD(1:LENTRIM(KEYWORD)),IBox,this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC
!    END DO

    CNUM = ""
    write(CNUM,*) p_NUMBER_OF_STATU
    KEYWORD = "&BOXSEINDEX"
    CFormat = ""
    CFormat = "(8(A20,1x))"
    write(hFile, FMT=CFormat(1:LENTRIM(CFormat))) KEYWORD(1:LENTRIM(KEYWORD)),  &
                                                  "IBox",                       &
                                                  "SEUsedIndexFrom",            &
                                                  "SEUsedIndexTo",              &
                                                  "SEExpdIndexFrom",            &
                                                  "SEExpdIndexTo",              &
                                                  "SEVirtualIndexFrom",         &
                                                  "SEVirtualIndexTo"

    KEYWORD = "&BOXSEDATA"
    CFormat = ""
    CFormat = "(A20,1x,I20,1x,"//CNUM(1:LENTRIM(CNUM))//"(I20,1x))"
    DO IBox = 1,MultiBox
        write(hFile, FMT=CFormat(1:LENTRIM(CFormat))) KEYWORD(1:LENTRIM(KEYWORD)),                  &
                                                      IBox,                                         &
                                                      this%m_BoxesInfo%SEUsedIndexBox(IBox,1:2),    &
                                                      this%m_BoxesInfo%SEExpdIndexBox(IBox,1:2),    &
                                                      this%m_BoxesInfo%SEVirtualIndexBox(IBox,1:2)
    END DO

    CNUM = ""
    ElementsKind = this%Atoms_list%Get_ListCount()
    write(CNUM,*) ElementsKind

    IAKind = 1
    tempLen = len(AtomsStr(1))
    cursor=>this%Atoms_list
    AtomsStr = " "
    DO While(associated(cursor))
        AtomsStr(IAKind)(tempLen-LENTRIM(cursor%m_Atom%m_Symbol)+1:tempLen) = cursor%m_Atom%m_Symbol
        IAKind = IAKind + 1
        cursor=>cursor%next
    END DO

    KEYWORD = "&ELEMENT"
    CFormat = ""
    CFormat = "(A20,1x,"//CNUM(1:LENTRIM(CNUM))//"(A15,1x))"
    write(hFile, FMT=CFormat(1:LENTRIM(CFormat))) KEYWORD(1:LENTRIM(KEYWORD)),AtomsStr(1:ElementsKind)

    KEYWORD = "&TYPE"
    CFormat = ""
    CFormat = "(9(A15,1x),"//CNUM(1:LENTRIM(CNUM))//"(A15,1x))"
    write(hFile,FMT=CFormat(1:LENTRIM(CFormat))) KEYWORD(1:LENTRIM(KEYWORD)),"IBox", "Layer","GBSeed1","GBSeed2","Statu","x(nm)","y(nm)","z(nm)",AtomsStr(1:ElementsKind)

    CFormat = ""
    CFormat = "(A15,1x,5(I15, 1x),3(1PE15.4, 1x),"//CNUM(1:LENTRIM(CNUM))//"(I15,1x))"
    DO IBox = 1,MultiBox
        ICFROM = this%m_BoxesInfo%SEUsedIndexBox(IBox,1)
        ICTO   = this%m_BoxesInfo%SEUsedIndexBox(IBox,2)

        if(ICTO .LE. 0) then
            cycle
        end if

        DO IC = ICFROM, ICTO

            if(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEFREE_STATU .or. this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu .eq. p_ACTIVEINGB_STATU) then

                write(hFile,fmt=CFormat(1:LENTRIM(CFormat))) "",                                                                &
                                                            IBox,                                                               &
                                                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer,                     &
                                                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID,                   &
                                                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu,                     &
                                                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1:3)*C_CM2NM,          &
                                                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(1:ElementsKind)%m_NA
            end if
        END DO
    END DO

    close(hFile)

    Nullify(cursor)

    return
  end subroutine Puout_Instance_Config_SimBoxArray

  !*****************************************************************
  subroutine Putin_Instance_Config_SimBoxArray(this,Host_SimuCtrlParam,SimuRecord,cfgFile,RNFACTOR,SURDIFPRE)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    Class(SimulationRecord)::SimuRecord
    character*256,intent(in)::cfgFile
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars--
    integer::hFile
    character*256::STR
    character*32::KEYWORD
    integer::LINE
    !---Body---

    LINE = 0

    hFile = OpenExistedFile(cfgFile)

    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
    call RemoveComments(STR,"!")

    STR = adjustl(STR)

    call GETKEYWORD("&",STR,KEYWORD)

    call UPCASE(KEYWORD)

    close(hFile)

    select case(KEYWORD(1:LENTRIM(KEYWORD)))
        case(OKMC_OUTCFG_FORMAT18)
            call this%Putin_OKMC_OUTCFG_FORMAT18(cfgFile,Host_SimuCtrlParam,SimuRecord,RNFACTOR,SURDIFPRE)
        case(MF_OUTCFG_FORMAT18)
            call this%Putin_MF_OUTCFG_FORMAT18(cfgFile,Host_SimuCtrlParam,SimuRecord,RNFACTOR,SURDIFPRE)
        case(SPMF_OUTCFG_FORMAT18)
            call this%Putin_SPMF_OUTCFG_FORMAT18(cfgFile,Host_SimuCtrlParam,SimuRecord,RNFACTOR,SURDIFPRE)
        case default
            write(*,*) "MCPSCUERROR: You must special the box file format at the beginning of the file."
            pause
            stop
    end select


    return
    100 write(*,*) "MCPSCUERROR: Fail to load the configuration at file: ",cfgFile
        write(*,*) "At line: ",LINE
        write(*,*) STR
        pause
        stop
  end subroutine Putin_Instance_Config_SimBoxArray

  !*************************************************************
  subroutine Putin_OKMC_OUTCFG_FORMAT18(this,cfgFile,Host_SimuCtrlParam,SimuRecord,RNFACTOR,SURDIFPRE)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    character*256,intent(in)::cfgFile
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    CLASS(SimulationRecord)::SimuRecord
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars---
    integer::hFile
    integer::LINE
    character*256::STR
    character*32::KEYWORD
    integer::K
    integer::IBox
    integer::IBoxTemp
    character*256::CFormat
    character*256::CNUM
    integer,dimension(:),allocatable::atomsInfo
    integer,dimension(:),allocatable::ExpandSizeArray
    integer,dimension(:),allocatable::NCEachBox
    character*256::CEmpty
    integer::state
    integer::ISeed
    integer::ISeedTemp
    integer::N
    integer::MultiBox
    character*32::STRTMP(20)
    integer::NTotalCluster
    integer::II
    integer::IC
    integer::IElement
    integer::IStatu
    type(BoxesInfo)::tempBoxesInfo
    integer::AtomsIndex(p_ATOMS_GROUPS_NUMBER)
    integer::NATomsUsed
    integer::LayerNum
    integer::ILayer
    type(DiffusorValue)::TheDiffusorValue
    character*15::CElement(p_ATOMS_GROUPS_NUMBER)
    integer::I
    integer::STA
    !---Body---

    MultiBox = Host_SimuCtrlParam%MultiBox

    NATomsUsed = 0

    AtomsIndex = 0

    LINE = 0

    hFile = OpenExistedFile(cfgFile)

    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
    call RemoveComments(STR,"!")

    STR = adjustl(STR)

    call GETKEYWORD("&",STR,KEYWORD)

    call UPCASE(KEYWORD)

    if(.not. IsStrEqual(adjustl(trim(KEYWORD)),OKMC_OUTCFG_FORMAT18)) then
        write(*,*) "MCPSCUERROR: the format of OKMC configuration file is not right at LINE: ",LINE
        write(*,*) STR
        pause
        stop
    end if

    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        call GETKEYWORD("&",STR,KEYWORD)

        STR = adjustl(STR)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&TYPE")
                exit

            case("&TIME")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                call SimuRecord%SetSimuTimes(DRSTR(STRTMP(1)))

            case("&BOXLOW")
                call EXTRACT_NUMB(STR,3,N,STRTMP)
                DO K = 1,3
                    this%BOXBOUNDARY(K,1) = DRSTR(STRTMP(K))
                END DO

            case("&BOXSIZE")
                call EXTRACT_NUMB(STR,3,N,STRTMP)
                DO K = 1,3
                    this%BOXSIZE(K) = DRSTR(STRTMP(K))
                END DO

            case("&NGRAIN")
                call this%m_GrainBoundary%Clean_Grainboundary()
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                this%m_GrainBoundary%GrainNum = ISTR(STRTMP(1))
                if(this%m_GrainBoundary%GrainNum .GT. 0) then
                    allocate(this%m_GrainBoundary%GrainSeeds(this%m_GrainBoundary%GrainNum))
                end if
                DO ISeed = 1,this%m_GrainBoundary%GrainNum
                    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
                    call RemoveComments(STR,"!")
                    read(STR,fmt="(A20,1x,I14, 1x, 3(1PE14.4, 1x))",ERR=100) CEmpty,ISeedTemp,this%m_GrainBoundary%GrainSeeds(ISeed)%m_POS(1:3)
                    if(.not. IsStrEqual(CEmpty,"&SEEDDATA")) then
                        write(*,*) "MCPSCUERROR: The grain seeds number is less than the recorded one."
                        pause
                        stop
                    end if

                    if(ISeedTemp .ne. ISeed) then
                        write(*,*) "MCPSCUERROR: The grain seeds index is not correct: ",ISeed
                        pause
                        stop
                    end if

                    this%m_GrainBoundary%GrainSeeds(ISeed)%m_POS(1:3) = this%m_GrainBoundary%GrainSeeds(ISeed)%m_POS(1:3)*C_NM2CM
                END DO

            case("&NCLUSTERS")
                write(*,*) "MCPSCUInfo: the key word &NCLUSTERS is not used anymore."
                CNUM = ""
                write(CNUM,*) p_NUMBER_OF_STATU
                CFormat = ""
                CFormat = "(A20,1x,I20,1x,"//CNUM(1:LENTRIM(CNUM))//"(I20,1x))"
                DO IBox = 1,MultiBox
                    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
                    call RemoveComments(STR,"!")
                    ! Do nothing
                END DO

            case("&BOXSEINDEX")
                call tempBoxesInfo%Init(MultiBox)

                CFormat = ""
                write(CNUM,*) p_NUMBER_OF_STATU
                CFormat = "(A20,1x,I20,1x,"//CNUM(1:LENTRIM(CNUM))//"(I20,1x))"
                DO IBox = 1,MultiBox
                    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
                    call RemoveComments(STR,"!")
                    read(STR,fmt=CFormat(1:LENTRIM(CFormat)),ERR=100)   CEmpty,                                  &
                                                                        IBoxTemp,                                &
                                                                        tempBoxesInfo%SEUsedIndexBox(IBox,1:2),  &
                                                                        tempBoxesInfo%SEExpdIndexBox(IBox,1:2),  &
                                                                        tempBoxesInfo%SEVirtualIndexBox(IBox,1:2)
                    if(.not. IsStrEqual(CEmpty,"&BOXSEDATA")) then
                        write(*,*) "MCPSCUERROR: The box clusters start and end index record is less than the control file recorded."
                        pause
                        stop
                    end if

                    if(IBoxTemp .ne. IBox) then
                        write(*,*) STR
                        write(*,*) "MCPSCUERROR: The box index is not correct: ",IBox,IBoxTemp
                        write(*,*) "At Line: ",LINE
                        pause
                        stop
                    end if

                    if((tempBoxesInfo%SEExpdIndexBox(IBox,2) - tempBoxesInfo%SEExpdIndexBox(IBox,1)) .LT. &
                       (tempBoxesInfo%SEUsedIndexBox(IBox,2) - tempBoxesInfo%SEUsedIndexBox(IBox,1))) then
                        write(*,*) "MCPSCUERROR: The recorded used clusters number is less than the expand clusters number !"
                        write(*,*) "In box: ",IBox
                        write(*,*) "The recorded start and end clusters index for used clusters is: ",tempBoxesInfo%SEUsedIndexBox(IBox,1),tempBoxesInfo%SEUsedIndexBox(IBox,2)
                        write(*,*) "The recorded start and end clusters index for expand clusters is: ",tempBoxesInfo%SEExpdIndexBox(IBox,1),tempBoxesInfo%SEExpdIndexBox(IBox,2)
                        pause
                        stop
                    end if

                    if((tempBoxesInfo%SEVirtualIndexBox(IBox,2) - tempBoxesInfo%SEVirtualIndexBox(IBox,1)) .LT. &
                       (tempBoxesInfo%SEExpdIndexBox(IBox,2) - tempBoxesInfo%SEExpdIndexBox(IBox,1))) then
                        write(*,*) "MCPSCUERROR: The recorded virtual clusters number is less than the expand clusters number !"
                        write(*,*) "In box: ",IBox
                        write(*,*) "The recorded start and end clusters index for virtual clusters is: ",tempBoxesInfo%SEVirtualIndexBox(IBox,1),tempBoxesInfo%SEVirtualIndexBox(IBox,2)
                        write(*,*) "The recorded start and end clusters index for expand clusters is: ",tempBoxesInfo%SEExpdIndexBox(IBox,1),tempBoxesInfo%SEExpdIndexBox(IBox,2)
                        pause
                        stop
                    end if

                END DO


            case("&ELEMENT")
                CNUM = ""
                write(CNUM,*) p_ATOMS_GROUPS_NUMBER
                CFormat = "(A20,1x,"//CNUM(1:LENTRIM(CNUM))//"(A15,1x))"
                read(STR,fmt=CFormat(1:LENTRIM(CFormat)),ERR=100) CEmpty,CElement

                NATomsUsed = 0
                DO IElement = 1,p_ATOMS_GROUPS_NUMBER
                    if(LENTRIM(adjustl(CElement(IElement))) .GT. 0) then
                        NATomsUsed = NATomsUsed + 1
                        AtomsIndex(NATomsUsed) = this%Atoms_list%FindIndexBySymbol(adjustl(trim(CElement(IElement))))
                    end if
                END DO

            case default
                write(*,*) "MCPSCUERROR: Illegal flag: ",KEYWORD
                write(*,*) STR
                pause
                stop
        end select

    END DO

    DO K = 1,3
        this%BOXBOUNDARY(K,2) = this%BOXBOUNDARY(K,1) + this%BOXSIZE(K)
    END DO

    call AllocateArray_Host(ExpandSizeArray,MultiBox,"ExpandSizeArray")

    ExpandSizeArray = 0

    DO IBox = 1,MultiBox
        if(tempBoxesInfo%SEVirtualIndexBox(IBox,2) .LE. 0) then
            ExpandSizeArray(IBox) = 0
        else
            ExpandSizeArray(IBox) = tempBoxesInfo%SEVirtualIndexBox(IBox,2) - tempBoxesInfo%SEVirtualIndexBox(IBox,1) + 1
        end if
    END DO

    call this%ExpandClustersInfor_CPU(Host_SimuCtrlParam,ExpandSizeArray)

    call AllocateArray_Host(atomsInfo,NATomsUsed,"atomsInfo")

    call AllocateArray_Host(NCEachBox,MultiBox,"NCEachBox")
    NCEachBox = 0

    DO While(.true.)

        read(hFile,fmt="(A)",ERR=100,IOSTAT=STA) STR

        if(STA .LT. 0) then
            exit
        end if

        call EXTRACT_NUMB(STR,8+p_ATOMS_GROUPS_NUMBER,N,STRTMP)
        atomsInfo = 0

        IBox = ISTR(STRTMP(1))

        NCEachBox(IBox) = NCEachBox(IBox) + 1

        IC = this%m_BoxesInfo%SEUsedIndexBox(IBox,2) + NCEachBox(IBox)

        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = ISTR(STRTMP(2))
        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = ISTR(STRTMP(3))
        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(2) = ISTR(STRTMP(4))
        IStatu = ISTR(STRTMP(5))
        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = IStatu

        DO I = 1,3
            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(I) = DRSTR(STRTMP(5+I))
        END DO

        DO I = 1,NATomsUsed
            atomsInfo(I) = DRSTR(STRTMP(5+3+I))
        END DO

        Do IElement = 1,NATomsUsed
            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(AtomsIndex(IElement))%m_ID = AtomsIndex(IElement)
            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(AtomsIndex(IElement))%m_NA = atomsInfo(IElement)
        End Do

        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1:3) = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS(1:3)*C_NM2CM

        TheDiffusorValue = this%m_DiffusorTypesMap%Get(this%m_ClustersInfo_CPU%m_Clusters(IC))

        select case(TheDiffusorValue%ECRValueType)
            case(p_ECR_ByValue)
                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR
            case(p_ECR_ByBCluster)
                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = DSQRT(sum(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)/RNFACTOR)
        end select

        select case(TheDiffusorValue%DiffusorValueType)
            case(p_DiffuseCoefficient_ByValue)
                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Value
            case(p_DiffuseCoefficient_ByArrhenius)
                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = SURDIFPRE*(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
        end select

        this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(IStatu) + 1
        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(IStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(IStatu) + 1

        this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(IStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(IStatu) + 1
        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(IStatu) = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(IStatu) + 1

    END DO

    DO IBox = 1,MultiBox
        this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,2) + NCEachBox(IBox)
        this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,2) + NCEachBox(IBox)
    END DO

    call DeAllocateArray_Host(atomsInfo,"atomsInfo")

    call DeAllocateArray_Host(ExpandSizeArray,"ExpandSizeArray")

    call tempBoxesInfo%Clean()

    close(hFile)

    return
    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
        write(*,*) "At line: ",LINE
        write(*,*) "STR",STR
        write(*,*) "STR(1:1)",STR(1:1)
        pause
        stop
  end subroutine Putin_OKMC_OUTCFG_FORMAT18

  !*************************************************************
  subroutine Putin_MF_OUTCFG_FORMAT18(this,cfgFile,Host_SimuCtrlParam,SimuRecord,RNFACTOR,SURDIFPRE)
    use RAND32_MODULE
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    character*256,intent(in)::cfgFile
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    CLASS(SimulationRecord)::SimuRecord
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars---
    real(kind=KMCDF),dimension(:),allocatable::LayerThick
    real(kind=KMCDF),dimension(:,:),allocatable::ClustersSampleConcentrate
    type(ACluster),dimension(:,:),allocatable::ClustersSample
    !---Body---

    call this%Putin_MF_OUTCFG_FORMAT18_Distribution(Host_SimuCtrlParam,cfgFile,LayerThick,ClustersSampleConcentrate,ClustersSample,SimuRecord,RNFACTOR,SURDIFPRE)

    call this%DoPutin_FromDistribution(Host_SimuCtrlParam,LayerThick,ClustersSampleConcentrate,ClustersSample,RNFACTOR,SURDIFPRE)

    call DeAllocateArray_Host(LayerThick,"LayerThick")
    call DeAllocateArray_Host(ClustersSampleConcentrate,"ClustersSampleConcentrate")
    call DeAllocateArray_Host(ClustersSample,"ClustersSample")

    return
  end subroutine Putin_MF_OUTCFG_FORMAT18

  !*************************************************************
  subroutine Putin_MF_OUTCFG_FORMAT18_Distribution(this,Host_SimuCtrlParam,cfgFile,LayersThick,ClustersSampleConcentrate,ClustersSample,SimuRecord,RNFACTOR,SURDIFPRE)
    use RAND32_MODULE
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    character*256,intent(in)::cfgFile
    real(kind=KMCDF),dimension(:),allocatable::LayersThick
    real(kind=KMCDF),dimension(:,:),allocatable::ClustersSampleConcentrate
    type(ACluster),dimension(:,:),allocatable::ClustersSample
    CLASS(SimulationRecord)::SimuRecord
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars---
    integer::hFile
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRTMP(20)
    integer::IElement
    integer::NCEachBox
    integer::AtomsIndex(p_ATOMS_GROUPS_NUMBER)
    integer::NATomsUsed
    integer::NClustersGroup
    type(DiffusorValue)::TheDiffusorValue
    !---Body---

    AtomsIndex = 0

    NATomsUsed = 0

    LINE = 0

    hFile = OpenExistedFile(cfgFile)

    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
    call RemoveComments(STR,"!")

    STR = adjustl(STR)

    call GETKEYWORD("&",STR,KEYWORD)

    call UPCASE(KEYWORD)

    if(.not. IsStrEqual(adjustl(trim(KEYWORD)),MF_OUTCFG_FORMAT18)) then
        write(*,*) "MCPSCUERROR: the format of mean field configuration file is not right at LINE: ",LINE
        write(*,*) STR
        pause
        stop
    end if

    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&TYPE")
                exit

            case("&TIME")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                call SimuRecord%SetSimuTimes(DRSTR(STRTMP(1)))

            case("&ELEMENT")
                call EXTRACT_SUBSTR(STR,p_ATOMS_GROUPS_NUMBER,N,STRTMP)
                if(N .LT. 1) then
                    write(*,*) "Too few elements are special!"
                    write(*,*) "At line: ",LINE
                    write(*,*) STR
                    pause
                    stop
                end if

                if(N .GT. p_ATOMS_GROUPS_NUMBER) then
                    write(*,*) "Too many elements are special!"
                    write(*,*) "At line: ",LINE
                    write(*,*) STR
                    write(*,*) "The system max atoms number is: ",p_ATOMS_GROUPS_NUMBER
                    pause
                    stop
                end if

                NATomsUsed = 0

                DO IElement = 1,N
                    AtomsIndex(IElement) = this%Atoms_list%FindIndexBySymbol(adjustl(trim(STRTMP(IElement))))
                    NATomsUsed = NATomsUsed + 1
                END DO

            case default
                write(*,*) "MCPSCUERROR: Illegal flag: ",KEYWORD
                write(*,*) STR
                pause
                stop
        end select

    END DO

    if(NATomsUsed .LE. 0) then
        write(*,*) "MCPSCUERROR: None of elements are special."
        pause
        stop
    end if

    NClustersGroup = 0

    DO While(.true.)

        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDBOXMF18")
                exit
        end select

        call EXTRACT_NUMB(STR,NATomsUsed + 1,N,STRTMP)

        if(N .LT. (NATomsUsed + 1 )) then
            write(*,*) "MCPSCUERROR: The atoms concentrations groups are less than: ",NATomsUsed
            write(*,*) "That is not correct with the elements define in previous."
            write(*,*) "At line: ",LINE
            write(*,*) STR
            pause
            stop
        end if

        NClustersGroup = NClustersGroup + 1

    END DO

    if(NClustersGroup .LE. 0) then
        write(*,*) "MCPSCUERROR: There are not any clusters group are defined in meanfield configuration file."
        pause
        stop
    end if

    !---For non-space special rate-theory, the layer number is 1.
    call AllocateArray_Host(LayersThick,1,"LayersThick")

    LayersThick(1) = this%BOXSIZE(3)

    call AllocateArray_Host(ClustersSampleConcentrate,1,NClustersGroup,"ClustersSampleConcentrate")

    call AllocateArray_Host(ClustersSample,1,NClustersGroup,"ClustersSample")

    ReWind(hFile)

    NClustersGroup = 0

    DO While(.true.)

        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDBOXMF18")
                exit
        end select

        call EXTRACT_NUMB(STR,NATomsUsed+1,N,STRTMP)

        NClustersGroup = NClustersGroup + 1

        ClustersSampleConcentrate(1,NClustersGroup) = DRSTR(STRTMP(NATomsUsed + 1))

        Do IElement = 1,NATomsUsed
            ClustersSample(1,NClustersGroup)%m_Atoms(AtomsIndex(IElement))%m_ID = AtomsIndex(IElement)
            ClustersSample(1,NClustersGroup)%m_Atoms(AtomsIndex(IElement))%m_NA = floor(DRSTR(STRTMP(IElement)) + 0.5D0)
        End Do

        TheDiffusorValue = this%m_DiffusorTypesMap%Get(ClustersSample(1,NClustersGroup))

        select case(TheDiffusorValue%ECRValueType)
            case(p_ECR_ByValue)
                ClustersSample(1,NClustersGroup)%m_RAD = TheDiffusorValue%ECR
            case(p_ECR_ByBCluster)
                ClustersSample(1,NClustersGroup)%m_RAD = DSQRT(sum(ClustersSample(1,NClustersGroup)%m_Atoms(:)%m_NA)/RNFACTOR)
        end select

        select case(TheDiffusorValue%DiffusorValueType)
            case(p_DiffuseCoefficient_ByValue)
                ClustersSample(1,NClustersGroup)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Value
            case(p_DiffuseCoefficient_ByArrhenius)
                ClustersSample(1,NClustersGroup)%m_DiffCoeff = TheDiffusorValue%PreFactor*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                ClustersSample(1,NClustersGroup)%m_DiffCoeff = SURDIFPRE*(ClustersSample(1,NClustersGroup)%m_RAD**(-p_GAMMA))
        end select

        ClustersSample(1,NClustersGroup)%m_Statu = p_ACTIVEFREE_STATU

        ClustersSample(1,NClustersGroup)%m_Layer = 1

    END DO

    close(hFile)

    return
    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
        write(*,*) "At line: ",LINE
        write(*,*) STR
        pause
        stop
  end subroutine Putin_MF_OUTCFG_FORMAT18_Distribution

  !*************************************************************
  subroutine Putin_SPMF_OUTCFG_FORMAT18(this,cfgFileName,Host_SimuCtrlParam,SimuRecord,RNFACTOR,SURDIFPRE)
    use RAND32_MODULE
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    character*256,intent(in)::cfgFileName
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    CLASS(SimulationRecord)::SimuRecord
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars---
    real(kind=KMCDF),dimension(:),allocatable::LayerThick
    real(kind=KMCDF),dimension(:,:),allocatable::ClustersSampleConcentrate
    type(ACluster),dimension(:,:),allocatable::ClustersSample
    !---Body---

    call this%Putin_SPMF_OUTCFG_FORMAT18_Distribution(Host_SimuCtrlParam,cfgFileName,LayerThick,ClustersSampleConcentrate,ClustersSample,SimuRecord,RNFACTOR,SURDIFPRE)

    call this%DoPutin_FromDistribution(Host_SimuCtrlParam,LayerThick,ClustersSampleConcentrate,ClustersSample,RNFACTOR,SURDIFPRE)

    call DeAllocateArray_Host(LayerThick,"LayerThick")
    call DeAllocateArray_Host(ClustersSampleConcentrate,"ClustersSampleConcentrate")
    call DeAllocateArray_Host(ClustersSample,"ClustersSample")

    return
  end subroutine Putin_SPMF_OUTCFG_FORMAT18

  !*************************************************************
  subroutine Putin_SPMF_OUTCFG_FORMAT18_Distribution(this,Host_SimuCtrlParam,cfgFile,LayersThick,ClustersSampleConcentrate,ClustersSample,SimuRecord,RNFACTOR,SURDIFPRE)
    use RAND32_MODULE
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    character*256,intent(in)::cfgFile
    real(kind=KMCDF),dimension(:),allocatable::LayersThick
    real(kind=KMCDF),dimension(:,:),allocatable::ClustersSampleConcentrate
    type(ACluster),dimension(:,:),allocatable::ClustersSample
    CLASS(SimulationRecord)::SimuRecord
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars---
    integer::hFile
    integer::LINE
    integer::N
    character*256::STR
    character*32::KEYWORD
    character*32::STRTMP(20)
    integer::IElement
    integer::NCEachBox
    integer::AtomsIndex(p_ATOMS_GROUPS_NUMBER)
    integer::NATomsUsed
    integer::LayerNum
    integer::tempClustersGroup
    integer::MaxGroups
    integer::ILayer
    integer::IGroup
    integer::tempLayer
    type(DiffusorValue)::TheDiffusorValue
    !---Body---

    AtomsIndex = 0

    NATomsUsed = 0

    LayerNum = 0

    LINE = 0

    hFile = OpenExistedFile(cfgFile)

    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
    call RemoveComments(STR,"!")

    STR = adjustl(STR)

    call GETKEYWORD("&",STR,KEYWORD)

    call UPCASE(KEYWORD)

    if(.not. IsStrEqual(adjustl(trim(KEYWORD)),SPMF_OUTCFG_FORMAT18)) then
        write(*,*) "MCPSCUERROR: the format of space special mean field configuration file is not right at LINE: ",LINE
        write(*,*) STR
        pause
        stop
    end if

    DO While(.true.)
        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&TYPE")
                exit

            case("&TIME")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                call SimuRecord%SetSimuTimes(DRSTR(STRTMP(1)))

            case("&ELEMENT")
                call EXTRACT_SUBSTR(STR,p_ATOMS_GROUPS_NUMBER,N,STRTMP)
                if(N .LT. 1) then
                    write(*,*) "Too few elements are special!"
                    write(*,*) "At line: ",LINE
                    write(*,*) STR
                    pause
                    stop
                end if

                if(N .GT. p_ATOMS_GROUPS_NUMBER) then
                    write(*,*) "Too many elements are special!"
                    write(*,*) "At line: ",LINE
                    write(*,*) STR
                    write(*,*) "The system max atoms number is: ",p_ATOMS_GROUPS_NUMBER
                    pause
                    stop
                end if

                NATomsUsed = 0

                DO IElement = 1,N
                    AtomsIndex(IElement) = this%Atoms_list%FindIndexBySymbol(adjustl(trim(STRTMP(IElement))))
                    NATomsUsed = NATomsUsed + 1
                END DO

            case("&NLAYER")
                call EXTRACT_NUMB(STR,1,N,STRTMP)
                if(N .LT. 1) then
                    write(*,*) "Too few parameters for number of layers!"
                    write(*,*) "At line: ",LINE
                    write(*,*) STR
                    pause
                    stop
                end if

                LayerNum = ISTR(STRTMP(1))

                if(LayerNum .LE. 0) then
                    write(*,*) "MCPSCUERROR: The layer number is less than 1."
                    write(*,*) "At line : ",LINE
                    pause
                    stop
                end if

                call AllocateArray_Host(LayersThick,LayerNum,"LayersThick")

                DO ILayer = 1,LayerNum
                    call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
                    call RemoveComments(STR,"!")

                    STR = adjustl(STR)

                    call GETKEYWORD("&",STR,KEYWORD)

                    call UPCASE(KEYWORD)

                    if(.not. IsStrEqual(KEYWORD(1:LENTRIM(KEYWORD)),"&LAYERTHICK")) then
                        write(*,*) "MCPSCUERROR: the layers number is less than the recorded layers number ."
                        pause
                        stop
                    end if

                    call EXTRACT_NUMB(STR,1,N,STRTMP)

                    LayersThick(ILayer) = DRSTR(STRTMP(1))

                END DO

                if(sum(LayersThick) .LE. 0) then
                    call DeAllocateArray_Host(LayersThick,"LayersThick")
                    call AllocateArray_Host(LayersThick,1,"LayersThick")
                    LayersThick(1) = this%BOXSIZE(3)
                end if

            case default
                write(*,*) "MCPSCUERROR: Illegal flag: ",KEYWORD
                write(*,*) STR
                pause
                stop
        end select

    END DO

    if(NATomsUsed .LE. 0) then
        write(*,*) "MCPSCUERROR: None of elements are special."
        pause
        stop
    end if

    tempClustersGroup = 0

    MaxGroups = 0

    ILayer = 1

    DO While(.true.)

        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDBOXMF18")
                exit
        end select

        call EXTRACT_NUMB(STR,NATomsUsed + 2,N,STRTMP)

        if(N .LT. (NATomsUsed + 2 )) then
            write(*,*) "MCPSCUERROR: The atoms groups are less than: ",NATomsUsed
            write(*,*) "That is not correct with the elements define in previous."
            write(*,*) "At line: ",LINE
            write(*,*) STR
            pause
            stop
        end if

        tempLayer =  ISTR(STRTMP(NATomsUsed + 2))

        if(ILayer .eq. tempLayer) then
            tempClustersGroup = tempClustersGroup + 1
        else if(ILayer .LT. tempLayer) then
            if(tempClustersGroup .GT. MaxGroups) then
                MaxGroups = tempClustersGroup
            end if
            tempClustersGroup = 0

            ILayer = tempLayer
        else
            write(*,*) "MCPSCUERROR: The layer number should be increase, but here it is decreasing."
            write(*,*) "At line :",LINE
            write(*,*) "For Layer number: ",tempLayer
            pause
            stop
        end if

    END DO

    if(MaxGroups .LE. 0) then
        write(*,*) "MCPSCUERROR: There are not any clusters group are defined in meanfield configuration file."
        pause
        stop
    end if

    if(LayerNum .LE. 0) then
        call AllocateArray_Host(LayersThick,1,"LayersThick")
        LayersThick(1) = this%BOXSIZE(3)
    end if

    if(sum(LayersThick) .GT. this%BOXSIZE(3)) then
        write(*,*) "MCPSCUERROR: The SPMF depth is greater than box depth"
        write(*,*) "The SPMF depth is: ",sum(LayersThick)
        write(*,*) "The box depth is: ",this%BOXSIZE(3)
        pause
        stop
    end if

    call AllocateArray_Host(ClustersSampleConcentrate,LayerNum,MaxGroups,"ClustersSampleConcentrate")

    call AllocateArray_Host(ClustersSample,LayerNum,MaxGroups,"ClustersSample")

    ReWind(hFile)

    IGroup = 1

    ILayer = 1

    DO While(.true.)

        call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
        call RemoveComments(STR,"!")

        STR = adjustl(STR)

        call GETKEYWORD("&",STR,KEYWORD)

        call UPCASE(KEYWORD)

        select case(KEYWORD(1:LENTRIM(KEYWORD)))
            case("&ENDBOXMF18")
                exit
        end select

        call EXTRACT_NUMB(STR,NATomsUsed+2,N,STRTMP)

        tempLayer =  ISTR(STRTMP(NATomsUsed + 2))

        if(ILayer .LT. tempLayer) then
            IGroup = IGroup + 1
            ILayer = tempLayer
        else
            write(*,*) "MCPSCUERROR: The layer number should be increase, but here it is decreasing."
            write(*,*) "At line :",LINE
            write(*,*) "For Layer number: ",tempLayer
            pause
            stop
        end if

        ClustersSampleConcentrate(ILayer,IGroup) = DRSTR(STRTMP(NATomsUsed + 1))

        Do IElement = 1,NATomsUsed
            ClustersSample(ILayer,IGroup)%m_Atoms(AtomsIndex(IElement))%m_ID = AtomsIndex(IElement)
            ClustersSample(ILayer,IGroup)%m_Atoms(AtomsIndex(IElement))%m_NA = floor(DRSTR(STRTMP(IElement)) + 0.5D0)
        End Do

        TheDiffusorValue = this%m_DiffusorTypesMap%Get(ClustersSample(ILayer,IGroup))

        select case(TheDiffusorValue%ECRValueType)
            case(p_ECR_ByValue)
                ClustersSample(ILayer,IGroup)%m_RAD = TheDiffusorValue%ECR
            case(p_ECR_ByBCluster)
                ClustersSample(ILayer,IGroup)%m_RAD = DSQRT(sum(ClustersSample(ILayer,IGroup)%m_Atoms(:)%m_NA)/RNFACTOR)
        end select

        select case(TheDiffusorValue%DiffusorValueType)
            case(p_DiffuseCoefficient_ByValue)
                ClustersSample(ILayer,IGroup)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Value
            case(p_DiffuseCoefficient_ByArrhenius)
                ClustersSample(ILayer,IGroup)%m_DiffCoeff = TheDiffusorValue%PreFactor*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy/Host_SimuCtrlParam%TKB)
            case(p_DiffuseCoefficient_ByBCluster)
                ! Here we adopt a model that D=D0*(1/R)**Gama
                ClustersSample(ILayer,IGroup)%m_DiffCoeff = SURDIFPRE*(ClustersSample(ILayer,IGroup)%m_RAD**(-p_GAMMA))
        end select

        ClustersSample(ILayer,IGroup)%m_Statu = p_ACTIVEFREE_STATU

        ClustersSample(ILayer,IGroup)%m_Layer = ILayer

    END DO

    close(hFile)

    return
    100 write(*,*) "MCPSCUERROR: Fail to load the configuration file"
        write(*,*) "At line: ",LINE
        write(*,*) STR
        pause
        stop
  end subroutine Putin_SPMF_OUTCFG_FORMAT18_Distribution

  !*************************************************************
  subroutine DoPutin_FromDistribution(this,Host_SimuCtrlParam,LayerThick,ClustersSampleConcentrate,ClustersSample,RNFACTOR,SURDIFPRE)
    use RAND32_MODULE
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    real(kind=KMCDF),dimension(:),intent(in),allocatable::LayerThick
    real(kind=KMCDF),dimension(:,:),intent(in),allocatable::ClustersSampleConcentrate
    type(ACluster),dimension(:,:),intent(in),allocatable::ClustersSample
    real(kind=KMCDF),intent(in)::RNFACTOR
    real(kind=KMCDF),intent(in)::SURDIFPRE
    !---Local Vars---
    integer::MultiBox
    integer::IBox
    integer::IC
    integer::ICFROM
    integer::ICTO
    real(kind=KMCDF)::BoxVolum
    integer::NCEachBox
    real(kind=KMCDF)::POS(3)
    integer::LastIndex
    integer::NClustersGroup
    integer::IGroup
    integer::ILayer
    integer::LayerNum
    integer::RemindedNum
    real(kind=KMCDF)::GroupRateTemp
    real(kind=KMCDF)::TotalConcentrate
    real(kind=KMCDF)::tempRand
    logical::exitFlag
    type(DiffusorValue)::TheDiffusorValue
    !---Body---

    LayerNum = size(LayerThick)

    NClustersGroup = size(ClustersSampleConcentrate,dim=2)

    if(LayerNum .ne. size(ClustersSampleConcentrate,dim=1)) then
        write(*,*) "MCPSCUERROR: The layer number is not equal between layerThick and ClustersSampleConcentrate"
        write(*,*) "The layer number in layerThick is: ",LayerNum
        write(*,*) "However: the Layer number in ClustersSampleConcentrate is ",size(ClustersSampleConcentrate,dim=1)
        pause
        stop
    end if

    MultiBox = Host_SimuCtrlParam%MultiBox

    BoxVolum = product(this%BOXSIZE)

    TotalConcentrate = sum(ClustersSampleConcentrate)

    NCEachBox = floor(TotalConcentrate*BoxVolum)

    call this%ExpandClustersInfor_CPU(Host_SimuCtrlParam,NCEachBox)

    DO IBox = 1,MultiBox

        LastIndex = this%m_BoxesInfo%SEUsedIndexBox(IBox,2)

        RemindedNum = NCEachBox

        DO ILayer = 1,LayerNum
            DO IGroup = 1,NClustersGroup

                ICFROM = LastIndex + 1
                ICTO = ICFROM + floor(ClustersSampleConcentrate(ILayer,IGroup)*BoxVolum) - 1

                DO IC = ICFROM,ICTO
                    this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms = ClustersSample(ILayer,IGroup)%m_Atoms

                    this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = ClustersSample(ILayer,IGroup)%m_Layer

                    this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = ClustersSample(ILayer,IGroup)%m_Statu

                    POS(1) = DRAND32()*this%BOXSIZE(1) + this%BOXBOUNDARY(1,1)
                    POS(2) = DRAND32()*this%BOXSIZE(2) + this%BOXBOUNDARY(2,1)
                    POS(3) = DRAND32()*this%BOXSIZE(3) + this%BOXBOUNDARY(3,1)
                    this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS

                    this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = this%m_GrainBoundary%GrainBelongsTo(POS)

                    TheDiffusorValue = this%m_DiffusorTypesMap%Get(this%m_ClustersInfo_CPU%m_Clusters(IC))

                    select case(TheDiffusorValue%ECRValueType)
                        case(p_ECR_ByValue)
                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR
                        case(p_ECR_ByBCluster)
                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = DSQRT(sum(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)/RNFACTOR)
                    end select

                    select case(TheDiffusorValue%DiffusorValueType)
                        case(p_DiffuseCoefficient_ByValue)
                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Value
                        case(p_DiffuseCoefficient_ByArrhenius)
                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy/Host_SimuCtrlParam%TKB)
                        case(p_DiffuseCoefficient_ByBCluster)
                            ! Here we adopt a model that D=D0*(1/R)**Gama
                            this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = SURDIFPRE*(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                    end select

                    RemindedNum = RemindedNum - 1

                END DO

                LastIndex = ICTO
            END DO
        END DO

        ICFROM = LastIndex + 1
        ICTO = ICFROM + RemindedNum - 1

        DO IC = ICFROM,ICTO
            tempRand = DRAND32()

            GroupRateTemp = 0.D0

            DO ILayer = 1,LayerNum
                if(exitFlag .eq. .true.) then
                    exit
                end if

                DO IGroup = 1,NClustersGroup
                    GroupRateTemp = ClustersSampleConcentrate(ILayer,IGroup)/TotalConcentrate

                    if(GroupRateTemp .GE. tempRand) then

                        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms = ClustersSample(ILayer,IGroup)%m_Atoms

                        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Layer = ClustersSample(ILayer,IGroup)%m_Layer

                        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu = ClustersSample(ILayer,IGroup)%m_Statu

                        POS(1) = DRAND32()*this%BOXSIZE(1) + this%BOXBOUNDARY(1,1)
                        POS(2) = DRAND32()*this%BOXSIZE(2) + this%BOXBOUNDARY(2,1)
                        POS(3) = DRAND32()*LayerThick(ILayer) +  sum(LayerThick(1:ILayer-1)) + this%BOXBOUNDARY(3,1)
                        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_POS = POS

                        this%m_ClustersInfo_CPU%m_Clusters(IC)%m_GrainID(1) = this%m_GrainBoundary%GrainBelongsTo(POS)

                        TheDiffusorValue = this%m_DiffusorTypesMap%Get(this%m_ClustersInfo_CPU%m_Clusters(IC))

                        select case(TheDiffusorValue%ECRValueType)
                            case(p_ECR_ByValue)
                                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = TheDiffusorValue%ECR
                            case(p_ECR_ByBCluster)
                                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD = DSQRT(sum(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(:)%m_NA)/RNFACTOR)
                        end select

                        select case(TheDiffusorValue%DiffusorValueType)
                            case(p_DiffuseCoefficient_ByValue)
                                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%DiffuseCoefficient_Value
                            case(p_DiffuseCoefficient_ByArrhenius)
                                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = TheDiffusorValue%PreFactor*exp(-C_EV2ERG*TheDiffusorValue%ActEnergy/Host_SimuCtrlParam%TKB)
                            case(p_DiffuseCoefficient_ByBCluster)
                                ! Here we adopt a model that D=D0*(1/R)**Gama
                                this%m_ClustersInfo_CPU%m_Clusters(IC)%m_DiffCoeff = SURDIFPRE*(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_RAD**(-p_GAMMA))
                        end select

                        exitFlag = .true.
                        exit

                    end if

                END DO
            END DO
        END DO

    END DO

    DO IBox = 1,MultiBox

        if(NCEachBox .GT. 0) then
            this%m_BoxesInfo%SEExpdIndexBox(IBox,2) = this%m_BoxesInfo%SEExpdIndexBox(IBox,2) + NCEachBox
            this%m_BoxesInfo%SEUsedIndexBox(IBox,2) = this%m_BoxesInfo%SEUsedIndexBox(IBox,2) + NCEachBox
        end if

        this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC(p_ACTIVEFREE_STATU) + NCEachBox
        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC(p_ACTIVEFREE_STATU) + NCEachBox

        this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) = this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC0(p_ACTIVEFREE_STATU) + NCEachBox
        this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) = this%m_BoxesBasicStatistic%BoxesStatis_Integral%NC0(p_ACTIVEFREE_STATU) + NCEachBox
    END DO

    return
  end subroutine DoPutin_FromDistribution


  !*************************************************************
  subroutine GetBoxesBasicStatistic_AllStatu_CPU(this,Host_SimuCtrlParam)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    !---Local Vars---
    integer::MultiBox
    integer::IBox
    integer::IStatu
    integer::ITYPE
    !---Body---

    ASSOCIATE(TBasicInfo=>this%m_BoxesBasicStatistic%BoxesStatis_Integral)

    TBasicInfo%NC = 0
    TBasicInfo%NA = 0

    MultiBox = Host_SimuCtrlParam%MultiBox

    DO IBox = 1, MultiBox
        ASSOCIATE(SBasicInfo=>this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox))
            call this%GetOneBoxBasicStatistic_AllStatu_CPU(IBox)

            TBasicInfo%NC = TBasicInfo%NC + SBasicInfo%NC
            TBasicInfo%NA = TBasicInfo%NA + SBasicInfo%NA

            if(IBox .GT. 1) then
                this%m_BoxesInfo%SEActIndexBox(IBox,1) = this%m_BoxesInfo%SEActIndexBox(IBox-1,2) + 1
            else
                this%m_BoxesInfo%SEActIndexBox(IBox,1) = 1
            end if

            this%m_BoxesInfo%SEActIndexBox(IBox,2) = this%m_BoxesInfo%SEActIndexBox(IBox,1) + (SBasicInfo%NC(p_ACTIVEFREE_STATU) + SBasicInfo%NC(p_ACTIVEINGB_STATU)) - 1

        END ASSOCIATE

    END DO

    END ASSOCIATE

    return
  end subroutine GetBoxesBasicStatistic_AllStatu_CPU

  !**************************************************************
  subroutine GetOneBoxBasicStatistic_AllStatu_CPU(this,IBox)
    implicit none
    !---Dummy Vars---
    Class(SimulationBoxes)::this
    integer, intent(in)::IBox
    !---Local Vars---
    integer::IC, ICFROM, ICTO
    integer::IStatu
    integer::ActiveFlag,DisappearFlag
    !---Body---

    ASSOCIATE(SBasicInfo=>this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox))

        SBasicInfo%NC = 0
        SBasicInfo%NA = 0

        ICFROM = this%m_BoxesInfo%SEUsedIndexBox(IBox,1)
        ICTO   = this%m_BoxesInfo%SEUsedIndexBox(IBox,2)

        ActiveFlag = ICFROM
        DisappearFlag = ICTO

        If(ICTO .GT. 0) then
          Do IC = ICFROM, ICTO
            IStatu = this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Statu

            if(IStatu .eq. p_Empty) then
                cycle
            end if

            if(IStatu .eq. p_ACTIVEFREE_STATU .or. IStatu .eq. p_ACTIVEINGB_STATU) then
                ! The index for active clusters
                this%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag) = IC
                ActiveFlag = ActiveFlag + 1
            else
                ! The index for unactive clusters
                this%m_ClustersInfo_CPU%m_ActiveIndex(DisappearFlag) = IC
                DisappearFlag = DisappearFlag - 1
            end if

            SBasicInfo%NC(IStatu) = SBasicInfo%NC(IStatu) + 1
            SBasicInfo%NA(IStatu) = SBasicInfo%NA(IStatu) + INT(sum(this%m_ClustersInfo_CPU%m_Clusters(IC)%m_Atoms(1:p_ATOMS_GROUPS_NUMBER)%m_NA),kind=KMCLINT)

          End Do

          ! move the unactive clusters indexes array position and inverse direction
          this%m_ClustersInfo_CPU%m_ActiveIndex(ActiveFlag:ActiveFlag + ICTO - DisappearFlag - 1) = this%m_ClustersInfo_CPU%m_ActiveIndex(ICTO:DisappearFlag+1:-1)

          ! The index for empty clusters space
          !FORALL(IC=DisappearFlag:ActiveFlag:-1)
          FORALL(IC=ActiveFlag + ICTO - DisappearFlag:ICTO)
            this%m_ClustersInfo_CPU%m_ActiveIndex(IC) = IC
          END FORALL

        End if

        ! Do some check
        if((DisappearFlag + 1 - ActiveFlag) .LT. 0) then
            write(*,*)  "MCPSCUERROR: The statistic error. The active and unactive clusters index would overlap."
            pause
            stop
        end if

    END ASSOCIATE

    return
  end subroutine GetOneBoxBasicStatistic_AllStatu_CPU

  !**********************************************
  integer function Get_MaxClustersNum(this,Host_SimuCtrlParam,ActiveStatu)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    type(SimulationCtrlParam)::Host_SimuCtrlParam
    integer,intent(in)::ActiveStatu
    !---Local Vars---
    integer::MultiBox
    integer::MaxNum
    integer::IBox
    integer, dimension(:), allocatable::tempArray
    integer::IStat
    !---Body---

    MultiBox = Host_SimuCtrlParam%MultiBox

    MaxNum = 0

    if(ActiveStatu .GT. 0 .AND. ActiveStatu .LE. p_NUMBER_OF_STATU) then

        MaxNum = maxval(this%m_BoxesBasicStatistic%BoxesStatis_Single(:)%NC(ActiveStatu))

    else if(ActiveStatu .EQ. 0) then

        call AllocateArray_Host(tempArray,MultiBox,"tempArray")

        FORALL(IBox=1:MultiBox)
            tempArray(IBox) = sum(this%m_BoxesBasicStatistic%BoxesStatis_Single(IBox)%NC)
        END FORALL

        MaxNum = maxval(tempArray)

        call DeAllocateArray_Host(tempArray,"tempArray")

    else
        write(*,*) "MCPSCUERROR: The active status is not defineded yet, the Get_MaxClustersNum would be wrong: ",ActiveStatu
        pause
        stop
    end if

    Get_MaxClustersNum = MaxNum

    return
  end function Get_MaxClustersNum

  !**********************************************
  subroutine CleanSimulationBoxes(this)
    implicit none
    !---Dummy Vars---
    CLASS(SimulationBoxes)::this
    !---Body---

    call this%m_ClustersInfo_CPU%Clean()

    call this%m_BoxesInfo%Clean()

    call this%m_BoxesBasicStatistic%Clean()

    return
  end subroutine CleanSimulationBoxes

  !**********************************************
  subroutine DestorySimulationBoxes(this)
    implicit none
    !---Dummy Vars---
    type(SimulationBoxes)::this
    !---Body---

    call this%Clean()

    call this%m_DiffusorTypesMap%Clean()

    call this%m_ReactionsMap%Clean()

    call this%m_GrainBoundary%Clean_Grainboundary()

    this%LatticeLength = 3.14D-8
    this%BOXBOUNDARY = 0.D0
    this%BOXSIZE = 0.D0
    this%HBOXSIZE = 0.D0
    this%BOXVOLUM = 0.D0

    call this%MatrixAtom%CleanAtom()

    call this%Atoms_list%CleanAtomsList()

    this%IniConfig = ""

    this%ImpFile = ""

    call this%ReadDiffusorProp_List%Clean_ReadDiffusorPropList()

    call this%ReadReactionProp_List%Clean_ReadReactionPropList()

    return
  end subroutine DestorySimulationBoxes

end module MCLIB_TYPEDEF_SIMULATIONBOXARRAY

