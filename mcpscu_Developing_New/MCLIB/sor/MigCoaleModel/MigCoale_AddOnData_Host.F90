module MIGCOALE_ADDONDATA_HOST
    use MSM_TYPEDEF_InputPaser
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    implicit none


    real(kind=KINDDF)::m_FREEDIFCOES(3) = 0.D0                       ! the surface diffusion coeffciency
    real(kind=KINDDF)::m_FREEDIFCOESPRE(3) = 0.D0                    ! the prefactor for the surface diffusion coeffciency
    real(kind=KINDDF)::m_FREEDIFCOESES(3) = 0.D0                     ! the surface ative energy for surface diffusion coeffciency

    real(kind=KINDDF)::m_FREESURDIFPRE = 0.D0

    real(kind=KINDDF)::m_GBDIFCOES(3) = 0.D0                     ! the surface diffusion coeffciency in GB
    real(kind=KINDDF)::m_GBDIFCOESPRE(3) = 0.D0                  ! the prefactor for the surface diffusion coeffciency in GB
    real(kind=KINDDF)::m_GBDIFCOESES(3) = 0.D0                   ! the surface ative energy for surface diffusion coeffciency in GB

    real(kind=KINDDF)::m_GBSURDIFPRE = 0.D0
                                                             ! 8*PI*SURFE/(3*KB*TEMP)
    logical::m_DumplicateBox = .true.

    contains

    subroutine resolveAddOnData(Host_Boxes,Host_SimuCtrlParam)
        !---Dummy Vars---
        type(SimulationBoxes)::Host_Boxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        !---Local Vars---
        integer::LINE
        character*1000::STR
        character*32::KEYWORD
        character*20::STRTEMP(10)
        integer::N
        integer::I
        integer::hFile
        !---Body---
        hFile = 6

        KEYWORD = "&DUMPLICATEBOX"
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), Host_SimuCtrlParam%AddOnData, STR, LINE)
        call RemoveComments(STR,"!")
        call EXTRACT_NUMB(STR,1,N,STRTEMP)
        if(N .LT. 1) then
            write(*,*) "MCPSCUERROR: Too few parameters for dumplicate box strategy at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &DUMPLICATEBOX  If use the dumplicate box strategy = "
            pause
            stop
        end if
        if(ISTR(STRTEMP(1)) .eq. 0) then
            m_DumplicateBox = .false.
        else
            m_DumplicateBox = .true.
        end if

        KEYWORD = "&SURDIF"
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), Host_SimuCtrlParam%AddOnData, STR, LINE)
        call EXTRACT_NUMB(STR,6,N,STRTEMP)
        if(N .LT. 6) then
            write(*,*) "MCPSCUERROR: Too few parameters for surface diffusion oarameters at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &SURDIF  THE Surface Diffusion coefficiens, prefactor (cm^2/s) and ES(ev): 0.0012, 1.0, 0.0012, 1.0, 0.0012, 1.0"
            pause
            stop
        end if
        DO I=1,3
            m_FREEDIFCOESPRE(I) = DRSTR(STRTEMP(2*I-1))
            m_FREEDIFCOESES(I) =  DRSTR(STRTEMP(2*I))
            m_FREEDIFCOES(I) = m_FREEDIFCOESPRE(I)*DEXP(-m_FREEDIFCOESES(I)*C_EV2ERG/Host_SimuCtrlParam%TKB)
        END DO
        m_FREESURDIFPRE = (3.D0/(2.D0*CP_PI))*(Host_Boxes%MatrixAtom%m_Volum**C_FOURBYTHREE)*m_FREEDIFCOES(1)

        KEYWORD = "&GBSURDIF"
        call Get_StatementList(KEYWORD(1:LENTRIM(KEYWORD)), Host_SimuCtrlParam%AddOnData, STR, LINE)
        call EXTRACT_NUMB(STR,6,N,STRTEMP)
        if(N .LT. 6) then
            write(*,*) "MCPSCUERROR: Too few parameters for surface diffusion parameters in GB at line: ",LINE
            write(*,*) STR
            write(*,*) "You should special: &GBSURDIF THE Surface Diffusion coefficiens in GB, prefactor (cm^2/s) and ES(ev): 0.0012, 1.0, 0.0012, 1.0, 0.0012, 1.0"
            pause
            stop
        end if
        DO I=1,3
            m_GBDIFCOESPRE(I) = DRSTR(STRTEMP(2*I-1))
            m_GBDIFCOESES(I) =  DRSTR(STRTEMP(2*I))
            m_GBDIFCOES(I) = m_GBDIFCOESPRE(I)*DEXP(-m_GBDIFCOESES(I)*C_EV2ERG/Host_SimuCtrlParam%TKB)
        END DO

        m_GBSURDIFPRE = (3.D0/(2.D0*CP_PI))*(Host_Boxes%MatrixAtom%m_Volum**C_FOURBYTHREE)*m_GBDIFCOES(1)

        call PrintResolveAddOnData(hFile)

        return
    end subroutine resolveAddOnData

    subroutine PrintResolveAddOnData(hFile)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::hFile
        !---Body---
        write(*,*) "***************The Add On Data****************"
        if(m_DumplicateBox .eq. .true.) then
            write(hFile,fmt="('!',A70,'!',2x,A10)") "Whether rescale box = ","true"
        else
            write(hFile,fmt="('!',A70,'!',2x,A10)") "Whether rescale box = ","false"
        end if
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "Prefactor for bigger cluster model  in free maxtrix = ",m_FREESURDIFPRE
        write(hFile,fmt="('!',A70,'!',2x,1ES10.4)") "Prefactor for bigger cluster model  in GB = ",m_GBSURDIFPRE
        return
    end subroutine PrintResolveAddOnData

end module MIGCOALE_ADDONDATA_HOST
