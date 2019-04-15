module MCLIB_UTILITIES_FORMER

  USE MCLIB_CONSTANTS
  #ifdef MC_PROFILING
  USE MCLIB_TimeProfile
  #endif

  implicit none

  contains

  !**** Random number generators
  !     The fisrt one is RAND()
  real(kind=KMCDF) function RAND()
    !
    !     THIS IS AN ADAPTED VERSION OF SUBROUTINE RANECU WRITTEN BY
    !  F. JAMES (COMPUT. PHYS. COMMUN. 60 (1990) 329-344), WHICH HAS
    !  BEEN MODIFIED TO GIVE A SINGLE RANDOM NUMBER AT EACH CALL.
    !     THE 'SEEDS' ISEED1 AND ISEED2 MUST BE INITIALIZED IN THE
    !  MAIN PROGRAM AND TRANSFERRED THROUGH THE NAMED COMMON BLOCK
    !  /RSEED/.

    implicit none
    !IMPLICIT DOUBLE PRECISION (A-H,O-Z), INTEGER*4 (I)
    !---Local Vars---
    real(kind=KMCDF),parameter::USCALE=1.0D0/2.0D0**31
    integer::ISEED1,ISEED2,I1,I2,IZ
    common/RSEED/ISEED1,ISEED2

    #ifdef MC_PROFILING
    call Time_Start(T_RAND_Start)
    #endif    
    !---Body---
    I1=ISEED1/53668
    ISEED1=40014*(ISEED1-I1*53668)-I1*12211
    if(ISEED1.LT.0) ISEED1=ISEED1+2147483563

    I2=ISEED2/52774
    ISEED2=40692*(ISEED2-I2*52774)-I2*3791
    if(ISEED2.LT.0) ISEED2=ISEED2+2147483399

    IZ=ISEED1-ISEED2
    if(IZ.LT.1) IZ=IZ+2147483562
    RAND=IZ*USCALE
    

    #ifdef MC_PROFILING
    call Time_Accumulate(T_RAND_Start,T_RAND)
    #endif
    return
  end function RAND

  !**************************************
  subroutine RAND_PUTSEED(SEED1, SEED2)
    implicit none
    !---Dummy Vars---
    integer(kind=8), intent(in), optional::SEED1, SEED2
    !---Local Vars---
    integer::ISEED1=329445983,ISEED2=437984
    common/RSEED/ISEED1,ISEED2
    integer DATE_TIME(8)

    #ifdef MC_PROFILING
    call Time_Start(T_RAND_PUTSEED_Start)
    #endif
    !---Body---
    if(present(SEED1)) then
        ISEED1 = SEED1
    else
        CALL DATE_AND_TIME (values = DATE_TIME )
        ISEED1 = DATE_TIME(8)+1000*( DATE_TIME(7)+100*DATE_TIME(6))
    end if

    if(present(SEED2)) then
        ISEED2 = SEED2
    else
        call DATE_AND_TIME (values = DATE_TIME )
        ISEED2 = DATE_TIME(8)+1000*( DATE_TIME(7)+100*DATE_TIME(5))
    end if
   #ifdef MC_PROFILING
   call Time_Accumulate(T_RAND_PUTSEED_Start,T_RAND_PUTSEED)
   #endif
    return
  end subroutine RAND_PUTSEED

  !**************************************
  subroutine RAND_GETSEED(SEED1, SEED2)
    implicit none
    !---Dummy Vars---
    integer,intent(out)::SEED1, SEED2
    !---Local Vars---
    integer::ISEED1,ISEED2
    common/RSEED/ISEED1,ISEED2

    #ifdef MC_PROFILING
    call Time_Start(T_RAND_GETSEED_Start)
    #endif
    !---Body---
    SEED1 = ISEED1
    SEED2 = ISEED2

    #ifdef MC_PROFILING
    call Time_Accumulate(T_RAND_GETSEED_Start,T_RAND_GETSEED)
    #endif
    return
  end subroutine RAND_GETSEED

  !**************************************************************
  real(kind=KMCDF) function RGAUSS0(XMEAN,SD)
    implicit none
    !---dummy vars---
    real(kind=KMCDF)::XMEAN,SD
    !---Local vars---
    integer::I

    #ifdef MC_PROFILING
    call Time_Start(T_RGAUSS0_Start)
    #endif
    !---Body---
    RGAUSS0=-6.0
    DO I=1,12
      RGAUSS0=RGAUSS0+RAND()
    END DO

    RGAUSS0=XMEAN+SD*RGAUSS0

    #ifdef MC_PROFILING
    call Time_Accumulate(T_RGAUSS0_Start,T_RGAUSS0)
    #endif
    return
  end  function RGAUSS0

  !**************************************************************
  real(kind=KMCDF) function RGAUSS(XMEAN,SD, reset)
    implicit none
    !---Dummy Vars---
    real(kind=KMCDF)::XMEAN,SD
    integer, optional::reset
    !---Local Vars---
    integer::I, idum=0

    #ifdef MC_PROFILING
    call Time_Start(T_RGAUSS_Start)
    #endif
    !---Body---
    !RGAUSS=-6.0
    !DO I=1,12
    !  RGAUSS=RGAUSS+RAND()
    !END DO

    if(present(reset)) then
       if(reset .le. 0) then
         idum = reset
         return
       end if
    end if

    !RGAUSS=-6.0
    !DO I=1,12
    !  RGAUSS=RGAUSS+RAN1(idum)
    !END DO
    !RGAUSS=XMEAN+SD*RGAUSS
    !return
    RGAUSS = XMEAN + SD*gasdev(idum)

    #ifdef MC_PROFILING
    call Time_Accumulate(T_RGAUSS_Start,T_RGAUSS)
    #endif
    return
  end  function RGAUSS

  !**************************************************************
  !**** Random number generators
  !     RAN0(), from numerical receipt
  function ran0(idum)
    implicit none
    !---Dummy Vars---
    integer idum
    !---Local Vars---
    integer IA,IM,IQ,IR,MASK
    real(kind=KMCDF) ran0,AM
    parameter (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,MASK=123459876)
    integer k

    #ifdef MC_PROFILING
    call Time_Start(T_ran0_Start)
    #endif
    !---Body---
    idum=ieor(idum,MASK)
    k=idum/IQ
    idum=IA*(idum-k*IQ)-IR*k
    if (idum.lt.0) idum=idum+IM
    ran0=AM*idum
    idum=ieor(idum,MASK)

    #ifdef MC_PROFILING
    call Time_Accumulate(T_ran0_Start,T_ran0)
    #endif
    return
  end function ran0

  !**** Random number generators
  !     RAN1(), from numerical receipt
  function ran1(idum)
    implicit none
    !---Dummy Vars---
    integer idum
    !---Local Vars---
    integer IA,IM,IQ,IR,NTAB,NDIV
    real(kind=KMCDF)  ran1,AM,EPS,RNMX
    parameter (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836, NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
    integer j,k,iv(NTAB),iy
    save iv,iy
    DATA iv /NTAB*0/, iy /0/

    #ifdef MC_PROFILING
    call Time_Start(T_ran1_Start)
    #endif
    !---Body---
    if(idum.le.0.or.iy.eq.0) then
       idum=max(-idum,1)
       do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
    11      continue
        iy=iv(1)
    endif
    k=idum/IQ
    idum=IA*(idum-k*IQ)-IR*k
    if (idum.lt.0) idum=idum+IM
    j=1+iy/NDIV
    iy=iv(j)
    iv(j)=idum
    ran1=min(AM*iy,RNMX)

    #ifdef MC_PROFILING
    call Time_Accumulate(T_ran1_Start,T_ran1)
    #endif
    return
  end function ran1

  !**** Random number generators
  !     RAN2(), from numerical receipt
  function ran2(idum)
    implicit none
    !---Dummy Vars---
    integer idum
    !---Local Vars---
    integer IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
    real(kind=KMCDF)  ran2,AM,EPS,RNMX
    parameter (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1, &
               IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791, &
               NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
    integer idum2,j,k,iv(NTAB),iy
    save iv,iy,idum2
    DATA idum2/123456789/, iv/NTAB*0/, iy/0/

    #ifdef MC_PROFILING
    call Time_Start(T_ran2_Start)
    #endif
    !---Body---
    if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
    11      continue
        iy=iv(1)
    endif
    k=idum/IQ1
    idum=IA1*(idum-k*IQ1)-k*IR1
    if (idum.lt.0) idum=idum+IM1
    k=idum2/IQ2
    idum2=IA2*(idum2-k*IQ2)-k*IR2
    if (idum2.lt.0) idum2=idum2+IM2
    j=1+iy/NDIV
    iy=iv(j)-idum2
    iv(j)=idum
    if(iy.lt.1)iy=iy+IMM1
    ran2=min(AM*iy,RNMX)

    #ifdef MC_PROFILING
    call Time_Accumulate(T_Ran2_Start,T_Ran2)
    #endif
    return
  end function Ran2

  !**** Random number generators
  !     RAN3(), from numerical receipt
  function ran3(idum)
    implicit none
    !---Dummy Vars---
    integer idum
    !---Local Vars---
    integer MBIG,MSEED,MZ
    real(kind=KMCDF) ran3,FAC
    parameter (MBIG=1000000000,MSEED=161803398,MZ=0,FAC=1./MBIG)
    integer i,iff,ii,inext,inextp,k
    integer mj,mk,ma(55)
    save iff,inext,inextp,ma
    DATA iff /0/

    #ifdef MC_PROFILING
    call Time_Start(T_ran3_Start)
    #endif
    !---Body---
    if(idum.lt.0.or.iff.eq.0)then
      iff=1
      mj=MSEED-iabs(idum)
      mj=mod(mj,MBIG)
      ma(55)=mj
      mk=1
      do 11 i=1,54
          ii=mod(21*i,55)
          ma(ii)=mk
          mk=mj-mk
          if(mk.lt.MZ)mk=mk+MBIG
          mj=ma(ii)
    11      continue
        do 13 k=1,4
          do 12 i=1,55
            ma(i)=ma(i)-ma(1+mod(i+30,55))
            if(ma(i).lt.MZ)ma(i)=ma(i)+MBIG
    12        continue
    13      continue
        inext=0
        inextp=31
        idum=1
    endif
    inext=inext+1
    if(inext.eq.56)inext=1
    inextp=inextp+1
    if(inextp.eq.56)inextp=1
    mj=ma(inext)-ma(inextp)
    if(mj.lt.MZ)mj=mj+MBIG
    ma(inext)=mj
    ran3=mj*FAC

    #ifdef MC_PROFILING
    call Time_Accumulate(T_Ran3_Start,T_Ran3)
    #endif

    return
  end function Ran3

  !**** Random number generators
  !     RAN4(), from numerical receipt
  function ran4(idum)
    integer idum
    !---Dummy Vars---
    real(kind=KMCDF) ran4
    integer idums
    !---Local Vars---
    integer irword,itemp,jflmsk,jflone,lword
    real ftemp
    EQUIVALENCE (itemp,ftemp)
    SAVE idums,jflone,jflmsk
    DATA idums /0/, jflone /16#3F800000/, jflmsk /16#007FFFFF/

    #ifdef MC_PROFILING
    call Time_Start(T_ran4_Start)
    #endif
    !---Body---
    if(idum.lt.0)then
        idums=-idum
        idum=1
    endif
    irword=idum
    lword=idums
    call psdes(lword,irword)
    itemp=ior(jflone,iand(jflmsk,irword))
    ran4=ftemp-1.0
    idum=idum+1

    #ifdef MC_PROFILING
    call Time_Accumulate(T_ran4_Start,T_ran4)
    #endif
    return
  end function ran4

  ! **** Random number generators
  subroutine psdes(lword,irword)
    implicit none
    !---Dummy Vars---
    integer irword,lword
    !---Local Vars---
    integer NITER
    parameter (NITER=4)
    integer i,ia,ib,iswap,itmph,itmpl,c1(4),c2(4)
    save c1,c2
    DATA c1 /16#BAA96887,16#1E17D32C,16#03BCDC3C,16#0F33D1B2/, c2  &
            /16#4B0F3B58,16#E874F0C3,16#6955C5A6, 16#55A7CA46/

    #ifdef MC_PROFILING
    call Time_Start(T_psdes_Start)
    #endif
    !---Body---
    do 11 i=1,NITER
        iswap=irword
        ia=ieor(irword,c1(i))
        itmpl=iand(ia,65535)
        itmph=iand(ishft(ia,-16),65535)
        ib=itmpl**2+not(itmph**2)
        ia=ior(ishft(ib,16),iand(ishft(ib,-16),65535))
        irword=ieor(lword,ieor(c2(i),ia)+itmpl*itmph)
        lword=iswap
    11    continue

    #ifdef MC_PROFILING
    call Time_Accumulate(T_psdes_Start,T_psdes)
    #endif

    return
  end subroutine psdes

  !**** Random number generators for normal distribution
  !     gasdev(), from numerical receipt

  function gasdev(idum)
    implicit none
    !---Dummy Vars---
    integer idum
    real(kind=KMCDF) gasdev
    !---Local Vars---
    integer iset
    real(kind=KMCDF) fac,gset,rsq,v1,v2
    save   iset,gset
    DATA iset/0/

    #ifdef MC_PROFILING
    call Time_Start(T_gasdev_Start)
    #endif
    !---Body---
    if (iset.eq.0) then
    1   v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.) goto 1
        fac=dsqrt(-2.d0*dlog(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
    else
        gasdev=gset
        iset=0
    endif

    #ifdef MC_PROFILING
    call Time_Accumulate(T_gasdev_Start,T_gasdev)
    #endif
    return
  end function gasdev

  !**************************************************************
  function SCALEDCFunction1(r, rav, m, sep, rp) result(DN)
    implicit none
    !---Dummy Vars---
    real(kind=KMCDF)::r, rav, m
    real(kind=KMCDF)::DN, sep
    real(kind=KMCDF), optional::rp
    !---Local Vars---
    real(kind=KMCDF)::PP
    real(kind=KMCDF), parameter::C_4PI_3 = (4.D0*PI)/3.D0

    #ifdef MC_PROFILING
    call Time_Start(T_SCALEDCFunction1_Start)
    #endif
    !---Body---
    if(present(rp)) then
       PP = 3.D0/rp
    else
       PP = 1.5D0 !default, rp =2
    end if
    !DN = ((r*r+m*rav*rav)**0.5D0+rav)**3.D0-(r+rav)**3.D0-m*(rav+rav)**3.D0
    !DN = ((r*r+m*rav*rav)**0.5D0+rav)**3.D0-(r+rav)**3.D0 !-m*(rav+rav)**3.D0
    DN = ((r*r+m*rav*rav)**0.5D0)**3.D0-r**3.D0 !-m*(rav+rav)**3.D0
    DN = DN/sep**3.D0
    DN = C_4PI_3*DN

    !DN = ((r*r+m*rav*rav)**0.5D0+rav)**3.D0-(r+rav)**3.D0-m*(rav+rav)**3.D0

    #ifdef MC_PROFILING
    call Time_Accumulate(T_SCALEDCFunction1_Start,T_SCALEDCFunction1)
    #endif
    return
  end function SCALEDCFunction1

  !*******************************************************************
  !                       NUMERICAL TOOLS.
  !******************************************************************
  !

  !  **************************************************************
  subroutine GABQ(FCT,XL,XU,SUM,TOL,IER)
    !
    !     THIS INTEGRATION SUBROUTINE APPLIES THE GAUSS METHOD WITH
    !  AN ADAPTIVE-BIPARTITION SCHEME.
    !     FCT IS THE (EXTERNAL) FUNCTION BEING INTEGRATED OVER THE
    !  INTERVAL (XL,XU). SUM IS THE RESULTANT VALUE OF THE INTEGRAL.
    !     TOL IS THE TOLERANCE, I.E. MAXIMUM RELATIVE ERROR REQUIRED
    !  ON THE COMPUTED VALUE (SUM). TOL SHOULD NOT EXCEED 1.0D-13.
    !     IER IS AN ERROR CONTROL PARAMETER; ITS OUTPUT VALUE IS
    !  IER=0 IF THE INTEGRATION ALGORITHM HAS BEEN ABLE TO GET THE
    !  REQUIRED ACCURACY AND IER=1 OTHERWISE.
    implicit none
    real(kind=KMCDF)::FCT
    real(kind=KMCDF)::XL,XU,SUM,TOL
    integer(kind=KMCINT)::IER
    !***  Working space
    real(kind=KMCDF)::S(128),SN(128), CTOL, PTOL, ERR, H, A, B, C, D, HO, ASUM, SI, XA, XB, XC, S1, S2, S12
    integer(kind=KMCINT)::L(128),LN(128), I1, I2, I3, ICALL, LH, LHN, I, K
    !****  PRINTED OUTPUT OF PARTIAL RESULTS: SET IWR=1.
    integer(kind=KMCINT), parameter::IWR= 0
    !****  COEFFICIENTS FOR GAUSS 20-POINT INTEGRATION.
    integer(kind=KMCINT), parameter:: NP=10,NP2=20,NP4=40
    !****  ABSCISSAS.
    real(kind=KMCDF), parameter::X(10)=(/7.6526521133497334D-02,2.2778585114164508D-01,   &
             3.7370608871541956D-01,5.1086700195082710D-01,   &
             6.3605368072651503D-01,7.4633190646015079D-01,   &
             8.3911697182221882D-01,9.1223442825132591D-01,   &
             9.6397192727791379D-01,9.9312859918509492D-01/)
    !****  WEIGHTS.
    real(kind=KMCDF), parameter::W(10)=(/1.5275338713072585D-01,1.4917298647260375D-01,   &
             1.4209610931838205D-01,1.3168863844917663D-01,   &
             1.1819453196151842D-01,1.0193011981724044D-01,   &
             8.3276741576704749D-02,6.2672048334109064D-02,   &
             4.0601429800386941D-02,1.7614007139152118D-02/)
    !****  CORRECTED TOLERANCE.

    #ifdef MC_PROFILING
    call Time_Start(T_GABQ_Start)
    #endif

    !---Body---
    CTOL=DMAX1(TOL,1.0D-13)
    PTOL=0.01D0*CTOL
    ERR=1.0D35
    H=XU-XL

    if(IWR.EQ.1) then
      write(6,10)
    10 format(///5X,'GAUSS ADAPTIVE-BIPARTITION QUADRATURE')
      write(6,11) XL,XU,TOL
    11 format(/5X,'XL = ',1P,E15.8,', XU = ',E15.8,', TOL = ', E8.1)
    end if
    IER=0
    !****GAUSS INTEGRATION FROM XL TO XU.
    A=0.5D0*(XU-XL)
    B=0.5D0*(XL+XU)
    C=A*X(1)
    D=W(1)*(FCT(B+C)+FCT(B-C))
    DO 1 I1=2,NP
       C=A*X(I1)
    1 D=D+W(I1)*(FCT(B+C)+FCT(B-C))

    SUM=D*A
    !****ADAPTIVE BIPARTITION SCHEME.
    ICALL=NP2
    LH=1
    S(1)=SUM
    L(1)=1

    2 HO=H

    H=0.5D0*H
    ASUM=SUM
    LHN=0

    DO 5 I=1,LH
      K=L(I)
      SI=S(I)
      XA=XL+(K-1)*HO
      XB=XA+H
      XC=XA+HO
      A=0.5D0*(XB-XA)
      B=0.5D0*(XB+XA)
      C=A*X(1)
      D=W(1)*(FCT(B+C)+FCT(B-C))

      DO 3 I2=2,NP
         C=A*X(I2)
      3 D=D+W(I2)*(FCT(B+C)+FCT(B-C))

      S1=D*A
      A=0.5D0*(XC-XB)
      B=0.5D0*(XC+XB)
      C=A*X(1)
      D=W(1)*(FCT(B+C)+FCT(B-C))
      DO 4 I3=2,NP
        C=A*X(I3)
      4 D=D+W(I3)*(FCT(B+C)+FCT(B-C))

      S2=D*A
      ICALL=ICALL+NP4
      S12=S1+S2
      SUM=SUM+S12-SI
      if(DABS(S12-SI).LT.DMAX1(PTOL*DABS(S12),1.0D-35)) GO TO 5

      LHN=LHN+2
      if(LHN.GT.128.OR.ICALL.GT.9999) GO TO 8

      SN(LHN)=S2
      LN(LHN)=K+K
      SN(LHN-1)=S1
      LN(LHN-1)=LN(LHN)-1
    5 continue

    ERR=DABS(SUM-ASUM)/DMAX1(DABS(SUM),1.0D-35)
    if(IWR.EQ.1) write(6,12) ICALL,SUM,ERR,LHN
    12 format(5X,'N = ',I5,', SUM = ',1P,E19.12,', ERR = ',E8.1, ', LH = ',I3)

    if(ERR.GT.CTOL.AND.LHN.GT.0) GO TO 6

    if(IWR.EQ.1) write(6,13)
    13 format(5X,'END OF GAUSS-BIPARTITION PROCEDURE'///)
    #ifdef MC_PROFILING
    call Time_Accumulate(T_GABQ_Start,T_GABQ)
    #endif
    return

    6 LH=LHN

    DO 7 I=1,LH
       S(I)=SN(I)
    7 L(I)=LN(I)
    GO TO 2
    !  ****  WARNING (LOW ACCURACY) MESSAGE.
    8 write(6,14)
    14 format(/5X,'*** LOW ACCURACY IN SUBROUTINE GABQ.')
      write(6,11) XL,XU,TOL
      write(6,15) SUM,ERR
    15 format(5X,'SUM = ',1P,E19.12,', ERR = ',E8.1//)

    IER=1

    #ifdef MC_PROFILING
    call Time_Accumulate(T_GABQ_Start,T_GABQ)
    #endif
    return
  end subroutine GABQ

  !**************************************************************
  subroutine SPLINE(X,Y,A,B,C,D,S1,SN,N)
    !     CUBIC SPLINE INTERPOLATION BETWEEN TABULATED DATA.
    !
    !  INPUT:
    !     X(I) (I=1, ...,N) ........ GRID POINTS.
    !                     (THE X VALUES MUST BE IN INCREASING ORDER).
    !     Y(I) (I=1, ...,N) ........ CORRESPONDING FUNCTION VALUES.
    !     S1,SN ..... SECOND DERIVATIVES AT X(1) AND X(N).
    !             (THE NATURAL SPLINE CORRESPONDS TO TAKING S1=SN=0).
    !     N ........................ NUMBER OF GRID POINTS.
    !
    !     THE INTERPOLATING POLYNOMIAL IN THE I-TH INTERVAL, FROM
    !  X(I) TO X(I+1), IS PI(X)=A(I)+X*(B(I)+X*(C(I)+X*D(I))).
    !
    !  OUTPUT:
    !     A(I),B(I),C(I),D(I) ...... SPLINE COEFFICIENTS.
    !
    !     REF.: M.J. MARON, 'NUMERICAL ANALYSIS: A PRACTICAL
    !           APPROACH', MACMILLAN PUBL. CO., NEW YORK 1982.
    !
    implicit none
    !---Dummy Vars---
    real(kind=KMCDF), intent(in)::X(*),Y(*), S1, SN
    real(kind=KMCDF)::A(*),B(*),D(*), C(*)
    integer(kind=KMCINT), intent(in)::N
    !---Local Vars---
    integer(kind=KMCINT)::N1, N2, I, K
    real(kind=KMCDF)::R, SI, SI1, H, HI


    #ifdef MC_PROFILING
    call Time_Start(T_SPLINE_Start)
    #endif
    !---Body---

    if(N.LT.4) then
      write(6,10) N
    10 format(5X,'SPLINE INTERPOLATION CANNOT BE PERFORMED WITH',I4,' POINTS. STOP.')
      stop
    end if
    N1=N-1
    N2=N-2
    !****AUXILIARY ARRAYS H(=A) AND DELTA(=D).
    DO 1 I=1,N1
      if(X(I+1)-X(I).LT.1.0D-10) then
        write(6,11)
        11 format(5X,'SPLINE X VALUES NOT IN INCREASING ORDER. STOP.')
        stop
      end if
      A(I)=X(I+1)-X(I)
    1 D(I)=(Y(I+1)-Y(I))/A(I)
    !****SYMMETRIC COEFFICIENT MATRIX (AUGMENTED).
    DO 2 I=1,N2
      B(I)=2.0D0*(A(I)+A(I+1))
      K=N1-I+1
    2 D(K)=6.0D0*(D(K)-D(K-1))

    D(2)=D(2)-A(1)*S1
    D(N1)=D(N1)-A(N1)*SN
    !****GAUSS SOLUTION OF THE TRIDIAGONAL SYSTEM.
    DO 3 I=2,N2
       R=A(I)/B(I-1)
       B(I)=B(I)-R*A(I)
    3 D(I+1)=D(I+1)-R*D(I)
    !****THE SIGMA COEFFICIENTS ARE STORED IN ARRAY D.
    D(N1)=D(N1)/B(N2)
    DO 4 I=2,N2
      K=N1-I+1
    4 D(K)=(D(K)-A(K)*D(K+1))/B(K-1)

    D(N)=SN
    !****SPLINE COEFFICIENTS.
    SI1=S1
    DO 5 I=1,N1
      SI=SI1
      SI1=D(I+1)
      H=A(I)
      HI=1.0D0/H
      A(I)=(HI/6.0D0)*(SI*X(I+1)**3-SI1*X(I)**3)   &
           +HI*(Y(I)*X(I+1)-Y(I+1)*X(I))           &
           +(H/6.0D0)*(SI1*X(I)-SI*X(I+1))
      B(I)=(HI/2.0D0)*(SI1*X(I)**2-SI*X(I+1)**2)   &
           +HI*(Y(I+1)-Y(I))+(H/6.0D0)*(SI-SI1)
      C(I)=(HI/2.0D0)*(SI*X(I+1)-SI1*X(I))
    5 D(I)=(HI/6.0D0)*(SI1-SI)


    #ifdef MC_PROFILING
    call Time_Accumulate(T_SPLINE_Start,T_SPLINE)
    #endif
    return
  end subroutine SPLINE
  !**************************************************************
  subroutine INTEG(X,A,B,C,D,XL,XU,SUM,N)
    !     INTEGRAL OF A CUBIC SPLINE FUNCTION.
    !  INPUT:
    !     X(I) (I=1, ...,N) ........ GRID POINTS.
    !                     (THE X VALUES MUST BE IN INCREASING ORDER).
    !     A(I),B(I),C(I),D(I) ...... SPLINE COEFFICIENTS.
    !     N ........................ NUMBER OF GRID POINTS.
    !     XL ....................... LOWER LIMIT IN THE INTEGRAL.
    !     XU ....................... UPPER LIMIT IN THE INTEGRAL.
    !
    !  OUTPUT:
    !     SUM ...................... VALUE OF THE INTEGRAL.
    !
    implicit none
    !---Dummy Vars---
    integer(kind=KMCINT),intent(in)::N
    real(kind=KMCDF),intent(in)::X(N),A(N),B(N),C(N),D(N)
    real(kind=KMCDF)::XL, XU, SUM

    !---Local Vars---
    real(kind=KMCDF)::SIGN, X1, X2, SUMP
    integer(kind=KMCINT)::IWR, IL, IU, I

    #ifdef MC_PROFILING
    call Time_Start(T_INTEG_Start)
    #endif
    !---Body---
    !****SET INTEGRATION LIMITS IN INCREASING ORDER.
    SIGN=1.0D0
    if(XU.LT.XL) then
      SUM=XL
      XL=XU
      XU=SUM
      SIGN=-1.0D0
    end if
    !****CHECK INTEGRAL LIMITS.
    IWR=0
    if(XL.LT.X(1).OR.XU.GT.X(N)) IWR=1
    !****FIND INVOLVED INTERVALS.
    SUM=0.0D0
    call FINDI(X,XL,N,IL)
    call FINDI(X,XU,N,IU)
    !****ONLY A SINGLE INTERVAL INVOLVED.
    if(IL.EQ.IU) then
      X1=XL
      X2=XU
      SUM=X2*(A(IL)+X2*((B(IL)/2)+X2*((C(IL)/3)+X2*D(IL)/4)))  &
         -X1*(A(IL)+X1*((B(IL)/2)+X1*((C(IL)/3)+X1*D(IL)/4)))
      GO TO 2
    end if
    !****CONTRIBUTIONS FROM DIFFERENT INTERVALS.
    X1=XL
    X2=X(IL+1)
    SUM=X2*(A(IL)+X2*((B(IL)/2)+X2*((C(IL)/3)+X2*D(IL)/4)))  &
         -X1*(A(IL)+X1*((B(IL)/2)+X1*((C(IL)/3)+X1*D(IL)/4)))
    IL=IL+1
    DO 1 I=IL,IU
      X1=X(I)
      X2=X(I+1)
      if(I.EQ.IU) X2=XU
      SUMP=X2*(A(I)+X2*((B(I)/2)+X2*((C(I)/3)+X2*D(I)/4)))    &
          -X1*(A(I)+X1*((B(I)/2)+X1*((C(I)/3)+X1*D(I)/4)))
    1 SUM=SUM+SUMP
    2 SUM=SIGN*SUM
    !****INTEGRAL LIMITS OUT OF RANGE.
    if(IWR.EQ.1) write(6,10)
    10 format(/'*** WARNING: INTEGRAL LIMITS OUT OF RANGE. ***')

    #ifdef MC_PROFILING
    call Time_Accumulate(T_INTEG_Start,T_INTEG)
    #endif
    return
  end subroutine INTEG

  !**************************************************************
  subroutine FINDI(X,XC,N,I)
    !
    !     FINDS THE INTERVAL (X(I),X(I+1)) CONTAINING THE VALUE XC.
    !  INPUT:
    !     X(I) (I=1, ...,N) ........ GRID POINTS.
    !                     (THE X VALUES MUST BE IN INCREASING ORDER).
    !     XC ....................... POINT TO BE LOCATED.
    !     N ........................ NUMBER OF GRID POINTS.
    !
    !  OUTPUT:
    !     I ........................ INTERVAL INDEX.
    !
    implicit none
    !---Dummy Vars---
    integer(KIND=KMCINT), intent(in)::N
    integer(KIND=KMCINT),intent(out)::I
    real(KIND=KMCDF), intent(in)::X(*), XC
    !---Local Vars---
    integer(KIND=KMCINT)::I1, IT

    #ifdef MC_PROFILING
    call Time_Start(T_FINDI_Start)
    #endif
    !---Body---
    if(XC.GT.X(N)) then
      I=N-1
      #ifdef MC_PROFILING
      call Time_Accumulate(T_FINDI_Start,T_FINDI)
      #endif
      return
    end if

    if(XC.LT.X(1)) then
      I=1
      #ifdef MC_PROFILING
      call Time_Accumulate(T_FINDI_Start,T_FINDI)
      #endif
      return
    end if

    I=1
    I1=N
    1 IT=(I+I1)/2

    if(XC.GT.X(IT)) then
        I=IT
    else
        I1=IT
    end if

    if(I1-I.GT.1) GO TO 1

    #ifdef MC_PROFILING
    call Time_Accumulate(T_FINDI_Start,T_FINDI)
    #endif
    return
   end subroutine FINDI

   !**************************************************************
   subroutine RQSORT(n,a,p)
   !======================================================================
   !     Return integer array p which indexes array a in increasing order.
   !     Array a is not disturbed.  The Quicksort algorithm is used.
   !
   !     B. G. Knapp, 86/12/23
   !
   !     Reference: N. Wirth, Algorithms and Data Structures,
   !     Prentice-Hall, 1986
   !======================================================================
     implicit none
     !---Dummy Vars---
     integer(KIND=KMCINT),intent(in)::n
     real(KIND=KMCDF), intent(in)::a(n)
     integer(KIND=KMCINT)::p(n)
     !---Local Vars---
     integer::LGN, Q
     parameter (LGN=32, Q=11)
     !(LGN = log base 2 of maximum n;
     !Q = smallest subfile to use quicksort on)
     real x
     integer   stackl(LGN),stackr(LGN),s,t,l,m,r,i,j

     #ifdef MC_PROFILING
     call Time_Start(T_RQSORT_Start)
     #endif
     !---Body---
     !Initialize the stack
     stackl(1)=1
     stackr(1)=n
     s=1

     !Initialize the pointer array
     do i=1,n
         p(i)=i
     end do

     2 if (s.gt.0) then
         l=stackl(s)
         r=stackr(s)
         s=s-1

     3    if ((r-l).lt.Q) then

      !Use straight insertion
            do i=l+1,r
               t = p(i)
               x = a(t)
               do j=i-1,l,-1
                  if (a(p(j)).le.x) goto 5
                  p(j+1) = p(j)
               end do
               j=l-1
    5          p(j+1) = t
           end do
         else

       !Use quicksort, with pivot as median of a(l), a(m), a(r)
            m=(l+r)/2
            t=p(m)
            if (a(t).lt.a(p(l))) then
               p(m)=p(l)
               p(l)=t
               t=p(m)
            endif
            if (a(t).gt.a(p(r))) then
               p(m)=p(r)
               p(r)=t
               t=p(m)
               if (a(t).lt.a(p(l))) then
                  p(m)=p(l)
                  p(l)=t
                  t=p(m)
               endif
            endif

       !Partition
            x=a(t)
            i=l+1
            j=r-1
    7       if (i.le.j) then
    8          if (a(p(i)).lt.x) then
                  i=i+1
                  goto 8
               endif
    9          if (x.lt.a(p(j))) then
                  j=j-1
                  goto 9
               endif
               if (i.le.j) then
                  t=p(i)
                  p(i)=p(j)
                  p(j)=t
                  i=i+1
                  j=j-1
               endif
               goto 7
            endif

          !Stack the larger subfile
            s=s+1
            if ((j-l).gt.(r-i)) then
               stackl(s)=l
               stackr(s)=j
               l=i
            else
               stackl(s)=i
               stackr(s)=r
               r=j
            endif
            goto 3
         endif
         goto 2
      endif

      #ifdef MC_PROFILING
      call Time_Accumulate(T_RQSORT_Start,T_RQSORT)
      #endif
      return
  end subroutine RQSORT

  !**************************************************************
  !                       SUBROUTINE GRAPH
  !**************************************************************
  subroutine DRAFT_GRAPH(X,Y,DY,NF,NL,IW)
    !
    !     THIS SUBROUTINE PLOTS THE FUNCTION Y OF THE VARIABLE X AS
    !  A MATRIX OF ASCII CHARACTERS. THE VALUES X SHOULD BE EVENLY
    !  SPACED AND IN INCREASING ORDER. ERROR BARS OF LENGTH DY ARE
    !  ALSO PLOTTED.
    !
    !  ARGUMENTS:
    !     X .............. ABSCISSAS,
    !     Y .............. CORRESPONDING ORDINATES,
    !     DY ............. ERROR BAR LENGTHS,
    !     NF, NL ......... FIRST AND LAST PLOTTED POINTS,
    !     IW ............. OUTPUT UNIT.
    !
    IMPLICIT DOUBLE PRECISION (A-H,O-Z),CHARACTER(L)
    integer(KIND=KMCINT), intent(in)::NF, NL, IW
    real(KIND=KMCDF), intent(in)::X(*),Y(*),DY(*)
    !Working space
    real(KIND=KMCDF)::S(9)
    CHARACTER*1::L(91)
    CHARACTER*1, parameter::L1='+',L2=' ',L3='I',L4='*',L5='-',L6='(',L7=')'
    integer(KIND=KMCINT)::I, J, IZERO, K, K1, K2

    #ifdef MC_PROFILING
    call Time_Start(T_DRAFT_GRAPH_Start)
    #endif
    !----------Body-------------
    YMAX=0.0D0
    YMIN=0.0D0
    DO 1 I=NF,NL
       E=DABS(DY(I))
       YMAX=DMAX1(Y(I)+E,YMAX)
    1 YMIN=DMIN1(Y(I)-E,YMIN)
      F=YMAX-YMIN
      IF(F.LE.0.0D0) then
        #ifdef MC_PROFILING
        call Time_Accumulate(T_DRAFT_GRAPH_Start,T_DRAFT_GRAPH)
        #endif
        RETURN
      END IF
      IF(YMIN.LT.0.0D0) GO TO 2
      IZERO=1
      D=YMAX/1.8D1
      GO TO 5
    2 CONTINUE
      IF(YMAX.GT.0.0D0) GO TO 3
      IZERO=19
      D=-YMIN/1.8D1
      GO TO 5
    3 K=19
    4 K=K-1
      D=F/DFLOAT(K)
      IZERO=IDINT(-YMIN/D)+1
      I=IDINT(YMAX/D)+1
      IF(IZERO+I.GT.18) GO TO 4
      IZERO=IZERO+1
    5 F=1.0D1
      IF(D.GT.1.0D1) F=0.1D0
      D=D/F
      DO 6 I=1,90
      D=D*F
      IF(D.LT.1.0D1.OR.D.GE.1.0D2) GO TO 6
      D=DFLOAT(IDINT(D)+1)/F**(I-1)
      GO TO 7
    6 CONTINUE
    7 DO 8 I=1,9
    8 S(I)=(I+I-IZERO)*D
      D=0.2D0*D
      IZERO=5*(IZERO-1)+1
      YMIN=(1-IZERO-0.5D0)*D
      WRITE(IW,101) (S(I),I=1,9)
      WRITE(IW,102)
      DO 12 I=NF,NL
      L(1)=L1
      L(91)=L1
      DO 9 J=2,90
    9 L(J)=L2
      L(IZERO)=L3
      E=DABS(DY(I))
      K=IDINT((Y(I)-YMIN)/D)+1
      K1=IDINT((Y(I)-E-YMIN)/D)+1
      K2=IDINT((Y(I)+E-YMIN)/D)+1
      IF(K1.GE.K2) GO TO 11
      DO 10 J=K1,K2
   10 L(J)=L5
      L(K1)=L6
      L(K2)=L7
   11 L(K)=L4
      WRITE(IW,103) X(I),(L(K),K=1,91),Y(I),E
   12 CONTINUE
      WRITE(IW,104)
      WRITE(IW,105) (S(I),I=1,9)
      #ifdef MC_PROFILING
      call Time_Accumulate(T_DRAFT_GRAPH_Start,T_DRAFT_GRAPH)
      #endif
      RETURN
  101 FORMAT(' ',7X,'Y :',1X,1P,E9.2,1X,8(E9.2,1X))
  102 FORMAT(' ',' X :',7X,9('+----I----'),'+',2X,'Y +- DY :')
  103 FORMAT(' ',1P,E10.3,1X,91A1,1X,E10.3,' +-',E8.1)
  104 FORMAT(' ',11X,9('+----I----'),'+')
  105 FORMAT(' ',11X,1P,E9.2,1X,8(E9.2,1X))

      #ifdef MC_PROFILING
      call Time_Accumulate(T_DRAFT_GRAPH_Start,T_DRAFT_GRAPH)
      #endif
      RETURN
  end subroutine DRAFT_GRAPH

end module MCLIB_UTILITIES_FORMER
