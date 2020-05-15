module INLET_BATCHIMPLANTATION_GPU
    use INLET_TYPEDEF_IMPLANTSECTION
    implicit none

    character(len=14),private,parameter::OKMC_CASCADE_FORMAT18 = "&CASCADEOKMC18"
    character(len=12),private,parameter::SRIM_CASCADE = "&CASCADESRIM"
    character(len=10),private,parameter::MD_CASCADE = "&CASCADEMD"
    character(len=16),private,parameter::MARLOWER_CASCADE = "&CASCADEMARLOWER"


    TYPE,public,extends(ImplantSection)::BatchImplantSection
        integer::InletClustersNumOneCase = 0

        contains

        procedure,non_overridable,public,pass::LoadOne_ImplantSection

    END TYPE

    TYPE,public::BatchImplantList
        type(BatchImplantSection)::TheBatchImplantSection

        type(BatchImplantList),pointer::next=>null()

        integer::ListCount=0

    END TYPE

    contains

    !*****The declare for abstract method***************
    !*****Note: this is not same with the "Fortran 95/2003 For Scientists and Engineers, Third Edition (chinese version)(P668)"
    !*****Because the abstract useage way in this book is not depended on PGFORTRAN compiler. The  following is based on our test
    !*****and verified to suit for pgfortran.
    subroutine LoadOne_ImplantSection(this,hFile,SimBoxes,Host_SimuCtrlParam,LINE)
        implicit none
        !---Dummy Vars---
        CLASS(BatchImplantSection)::this
        integer, intent(in)::hFile
        type(SimulationBoxes)::SimBoxes
        type(SimulationCtrlParam)::Host_SimuCtrlParam
        integer::LINE
        !---Local Vars---
        character*256::STR
        character*32::KEYWORD
        character*20::STRTMP(10)
        integer::N
        !---Body---
        Do While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit
                case("&TYPE")
                    call EXTRACT_NUMB(STR,1,N,STRTMP)
                    if(N .LT. 1) then
                        write(*,*) "MCPSCUERROR: Too few parameters for implantation distribution type."
                        write(*,*) "At Line :", LINE
                        write(*,*) "You should special by the way : &TYPE The implantation cluster distribution type =  "
                        pause
                        stop
                    end if
                    this%ImplantConfigType = ISTR(STRTMP(1))
                    exit
                case default
                    write(*,*) "MCPSCUERROR: You must special the implantation distribution type first!"
                    write(*,*) "By the way: &TYPE The implantation cluster distribution type = "
                    write(*,*) "However, the words you input is: ",STR
                    pause
                    stop
            end select
        End Do

        Do While(.true.)
            call GETINPUTSTRLINE(hFile,STR,LINE,"!",*100)
            call RemoveComments(STR,"!")
            STR = adjustl(STR)
            call GETKEYWORD("&",STR,KEYWORD)
            call UPCASE(KEYWORD)

            select case(KEYWORD(1:LENTRIM(KEYWORD)))
                case("&ENDSUBCTL")
                    exit

                case("&SIZESUBCTL","&DEPTHSUBCTL","&EXTFSUBCTL")
                    call this%ReadImplantSection(hFile,KEYWORD,SimBoxes,Host_SimuCtrlParam,LINE)

                case default
                    write(*,*) "MCPSCUERROR: Unknown Flag: ",KEYWORD
                    write(*,*) "At LINE: ",LINE
                    pause
                    stop
            end select
        End Do

        return

        100 write(*,*) "MCPSCUERROR : Load implantation configuration file failed !"
            write(*,*) "At line :",LINE
            write(*,*) "The program would stop."
            pause
            stop
    end subroutine


end module INLET_BATCHIMPLANTATION_GPU
