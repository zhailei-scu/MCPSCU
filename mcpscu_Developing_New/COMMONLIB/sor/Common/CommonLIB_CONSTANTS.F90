module COMMONLIB_CONSTANTS
    use MSM_CONSTANTS
    implicit none

    integer,parameter::KINDDF_SIZE_PGI = 8             ! 8 bit for a KINDDF kind
    integer,parameter::DEFAULT_SIZE_PGI = 4

    !--- numbers
    integer(kind=KINDINT8),parameter::TENPOWTHREE = 1*10**3
    integer(kind=KINDINT8),parameter::TENPOWFOUR = 1*10**4
    integer(kind=KINDINT8),parameter::TENPOWFIVE = 1*10**5
    integer(kind=KINDINT8),parameter::TENPOWSIX = 1*10**6
    integer(kind=KINDINT8),parameter::TENPOWSEVEN = 1*10**7
    integer(kind=KINDINT8),parameter::TENPOWEIGHT = 1*10**8


    !*** Math. and  Phys. constants used
    real(kind=KINDDF), parameter::C_FOURBYTHREE = 4.D0/3.D0

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


end module COMMONLIB_CONSTANTS
