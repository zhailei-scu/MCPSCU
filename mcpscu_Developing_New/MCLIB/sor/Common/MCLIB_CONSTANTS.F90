module MCLIB_CONSTANTS
    use MSM_CONSTANTS
    implicit none

    !---This macro function used to translate the symbol x to string (but not unfold the macro)
    #define __S(x) #x
    !---This macro function used to translate the symbol x to string (and unfold the macro)
    #define _S(x) __S(x)

    #ifdef MCVERSION
    character(LEN = 30),parameter::mp_Version = _S(MCVERSION)
    #else
    !As the code::block cannot identify the macro string, so in code::block IDE
    !we fix the following macro. While we public the package, these should be remove because when we
    !use the makefile instead of code::block IDE, we can define these macro while installing and building
    character(LEN = 30),parameter::mp_Version = "Developing_New"
    #endif

     !Date conversion
    character(len=1),parameter::KEYWORD_HEAD = "&"
    ! Data File type
    character(len=10),parameter::OKMC_OUTCFG_FORMAT18 = "&BOXOKMC18"
    character(len=8),parameter::MF_OUTCFG_FORMAT18 = "&BOXMF18"
    character(len=10),parameter::SPMF_OUTCFG_FORMAT18 = "&BOXSPMF18"

    ! Data type
    integer,parameter::KMCLINT = 8

    integer,parameter::KINDDF_SIZE_PGI = 8             ! 8 bit for a KINDDF kind
    integer,parameter::DEFAULT_SIZE_PGI = 4


    integer,parameter::mp_CalcNeighborList_NNEAREST = 1
    integer,parameter::mp_CalcNeighborList_RCUT = 2

    integer,parameter::mp_NEIGHBORUPDATEBYSTEP    =  0
    integer,parameter::mp_NEIGHBORUPDATEBYNCREMIND = 1

    integer,parameter::mp_TermTimeFlag_ByStep = 0
    integer,parameter::mp_TermTimeFlag_ByRealTime = 1

    integer,parameter::mp_SelfAdjustlStep_NearestSep = 0   ! the time step is determined by average distance of nearest cluster
    integer,parameter::mp_FixedTimeStep = 1
    integer,parameter::mp_SelfAdjustlStep_AveSep = 2       ! the time step is determined by volume average distance and suppose the clusters distribute uniform in the box

    integer,parameter::mp_UpdateStatisFlag_ByIntervalSteps = 0
    integer,parameter::mp_UpdateStatisFlag_ByIntervalRealTime = 1

    integer,parameter::mp_OutTimeFlag_ByIntervalSteps = 0
    integer,parameter::mp_OutTimeFlag_ByIntervalRealTime = 1
    integer,parameter::mp_OutTimeFlag_ByIntervalTimeMagnification = 2


    integer, parameter::p_DEPT_DIS_Layer = 0                            ! uniform distribution in the box
    integer, parameter::p_DEPT_DIS_BOX = 1                              ! boxed uniform distribution. The box is a subbox of simulated box
    integer, parameter::p_DEPT_DIS_GAS = 2                              ! Gauss distribution in Z-depth
    integer, parameter::p_SPHERE_DIST = 3                               ! within a sphere volum


    !*** data structure defining an cluster
    #ifdef ATOMSGROUP
    integer, parameter::p_ATOMS_GROUPS_NUMBER = ATOMSGROUP
    #else
    integer, parameter::p_ATOMS_GROUPS_NUMBER = 4
    #endif

    !***Some pre-define parameters for dynamic groups
    #ifdef MAXFOCUSETIME
    integer, parameter::p_MAX_FOCUSEDTIMEPOINTS = MAXFOCUSETIME
    #else
    integer, parameter::p_MAX_FOCUSEDTIMEPOINTS = 10
    #endif

    character(len=1),parameter::p_ElementsTypeSpe = "@"
    character(len=1),parameter::p_ElementsNumSpe = "#"
    character(len=1),parameter::p_NumRangeSpe = "-"
    character(len=3),parameter::p_InfStr = "INF"


    integer,parameter::p_ReactionCoefficientTypesNum = 2
    integer,parameter::p_ReactionCoefficient_ByValue = 1
    integer,parameter::p_ReactionCoefficient_ByArrhenius = 2

    integer,parameter::p_ProductionTypesNum = 2
    integer,parameter::p_ProductionType_BySimplePlus = 1
    integer,parameter::p_ProductionType_BySubtract = 2

    integer,parameter::p_DiffuseCoefficientTypesNum = 5
    integer,parameter::p_DiffuseCoefficient_ByValue = 1
    integer,parameter::p_DiffuseCoefficient_ByArrhenius = 2
    integer,parameter::p_DiffuseCoefficient_ByBCluster = 3
    integer,parameter::p_DiffuseCoefficient_BySIACluster = 4
    integer,parameter::p_DiffuseCoefficient_ByVcCluster = 5

    integer,parameter::p_DiffuseDirectionTypeNum = 2
    integer,parameter::p_DiffuseDirection_ThreeDim = 1
    integer,parameter::p_DiffuseDirection_OneDim = 2

    !*** The clusters type
    integer,parameter::p_NUMBER_OF_STATU = 7
    integer,parameter::p_ACTIVEFREE_STATU = 1
    integer,parameter::p_ACTIVEINGB_STATU = 2
    integer,parameter::p_OUT_DESTROY_STATU = 3
    integer,parameter::p_EXP_DESTROY_STATU = 4
    integer,parameter::p_MIS_DESTROY_STATU = 5
    integer,parameter::p_ABSORBED_STATU = 6
    integer,parameter::p_ANNIHILATE_STATU = 7
    integer,parameter::p_Empty = 0
    character*20,parameter::p_CStatu(p_NUMBER_OF_STATU) = (/"ACTIVEFREE","ACTIVEINGB","OUT_DESTROY","EXP_DESTRO","MIS_DESTROY","ABSORBED","ANNIHILATE"/)


    real(kind=KINDDF),parameter::p_GAMMA = 4.D0                          ! the parameter for cluster diffusion

    !--- numbers
    real(kind=KINDDF), parameter::ZERO=0
    real(kind=KINDDF), parameter::ONE=1
    real(kind=KINDDF), parameter::TWO=2
    real(kind=KINDDF), parameter::THREE=3
    real(kind=KINDDF), parameter::TEN=10
    real(kind=KINDDF), parameter::HUNDRED=100
    integer(kind=KINDINT), parameter::IHUNDRED=100
    integer(kind=KMCLINT),parameter::TENPOWTHREE = 1*10**3
    integer(kind=KMCLINT),parameter::TENPOWFOUR = 1*10**4
    integer(kind=KMCLINT),parameter::TENPOWFIVE = 1*10**5
    integer(kind=KMCLINT),parameter::TENPOWSIX = 1*10**6
    integer(kind=KMCLINT),parameter::TENPOWSEVEN = 1*10**7
    integer(kind=KMCLINT),parameter::TENPOWEIGHT = 1*10**8


    !*** Math. and  Phys. constants used
    real(kind=KINDDF), parameter::A0B=5.29177249D-9, &      !BOHR RADIU
                              AVOG=6.0221367D23            !Avigado constants

    real(kind=KINDDF), parameter::C_FOURBYTHREE = 4.D0/3.D0

    real(kind=KINDDF), parameter::C_UM2CM = 1.D-4
    real(kind=KINDDF), parameter::C_CM2UM = 1.D4
    real(kind=KINDDF), parameter::C_NM2CM = 1.D-7
    real(kind=KINDDF), parameter::C_CM2NM = 1.D7
    real(kind=KINDDF), parameter::C_AM2CM = 1.D-8
    real(kind=KINDDF), parameter::C_CM2AM = 1.D8
    real(kind=KINDDF), parameter::C_JPERM2_TO_ERGPERCM2 = 1.D3
    real(kind=KINDDF), parameter::C_KB      =  1.38054D-16              !Boltzmann constant, in ERG/K
    real(kind=KINDDF), parameter::C_EV2ERG   = 1.60219D-12

    !*** Memory management *****************
    integer, parameter::C_BYTE = 8    ! (8 bits)
    integer, parameter::C_KBYTES = 1024*C_BYTE
    integer, parameter::C_MBYTES = 1024*C_KBYTES
    integer, parameter::C_GBYTES = 1024*C_MBYTES

    !***The directory associated parameters*************
    ! "/"  In fact, the "/" and "\" is same in windows and in cygwin bash or command windows, but in linux, should be "/",
    ! and in cygwin environment, if we call the system(mkdir ) command in fortran program, we cannot use the "/", it should be "\"
    ! so, in fortran program, it is necessary to use "/" in linux and use "\" in windows(cygwin)
    #ifdef CYGWIN
    character(len=1), parameter::FolderSpe = achar(92)   ! "\"
    #else
    character(len=1), parameter::FolderSpe = achar(47)   ! "/"
    #endif

    character(len=1), parameter::RelativeHead = achar(46) ! "."


end module MCLIB_CONSTANTS
