!*********************************************************************************!
!--- Description:
!--- Author : Lei Zhai, Insti. of Nucle. Sci. and Tech., Sichuan University
!--- Email : zhaileiytp@163.com
!--- data: From 2017-09 to 2021-12
!--- License: MIT License (There are no limitation for anyone to use/modify/sale of this code, be happy to use this code)
!--- Please ref "Lei Zhai, Chaoqiong Ma, Jiechao Cui and Qing Hou, GPU-based acceleration of Monte Carlo simulations for migration-coalescence evolution of gas bubbles in materials
!---                        Modelling Simul. Mater. Sci. Eng. 2019. 27 055008,https://iopscience.iop.org/article/10.1088/1361-651X/ab1d14
!*********************************************************************************!
module MC_MethodClass_Factory_GPU
    use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
    use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
    implicit none

    abstract interface
        subroutine For_One_Test(Host_SimBoxes,Host_SimuCtrlParamList,Dev_Boxes,JobIndex)
            use MCLIB_TYPEDEF_SIMULATIONBOXARRAY
            use MCLIB_TYPEDEF_SIMULATIONCTRLPARAM
            use MCLIB_TYPEDEF_SIMULATIONBOXARRAY_GPU
            use MCLIB_TYPEDEF_BASICRECORD
            implicit none
            type(SimulationBoxes)::Host_SimBoxes
            type(SimulationCtrlParamList),target::Host_SimuCtrlParamList
            type(SimulationBoxes_GPU)::Dev_Boxes
            integer,intent(in)::JobIndex
        end subroutine
    end interface


    type,public::MCMethodClassGPU
        character(len=20)::name
        type(SimulationBoxes),pointer::pSimulationBoxes=>null()
        type(SimulationCtrlParamList),pointer::pSimulationCtrlParamList=>null()
        procedure(For_One_Test),pointer,nopass::ForOneTest=>null()

        contains

        procedure,public,pass,non_overridable::Register_Method_Class
        procedure,public,pass,non_overridable::Clean_MethodClass
        Final::CleanMethodClass
    end type

    private::Register_Method_Class
    private::Clean_MethodClass
    private::CleanMethodClass

    contains
    !*********************************************
    subroutine Register_Method_Class(this,className,SimBoxes,SimCtrlParamsList)
        use MC_Method_MIGCOALE_CLUSTER_GPU, only:For_One_Test_MIGCOALE_CLUSTER_GPU => For_One_Test
        implicit none
        !---Dummy Vars---
        CLASS(MCMethodClassGPU)::this
        character*(*)::className
        type(SimulationBoxes),target::SimBoxes
        type(SimulationCtrlParamList),target::SimCtrlParamsList
        !---Local Vars---
        !---Body---
        this%pSimulationBoxes=>SimBoxes
        this%pSimulationCtrlParamList=>SimCtrlParamsList

        select case(className(1:LENTRIM(className)))
            case("MIGCOALE_CLUSTER_GPU")
                this%name = "MIGCOALE_CLUSTER_GPU"
                this%ForOneTest=>For_One_Test_MIGCOALE_CLUSTER_GPU
            case default
                write(*,*) "MCPSCUERROR: The unknown method name: ",className
                pause
                stop
        end select

        return
    end subroutine Register_Method_Class

    !**************************************************
    subroutine Clean_MethodClass(this)
        implicit none
        !---Dummy Vars---
        class(MCMethodClassGPU)::this
        !---Body---
        Nullify(this%ForOneTest)
        Nullify(this%pSimulationBoxes)
        Nullify(this%pSimulationCtrlParamList)
        this%ForOneTest=>null()
        this%pSimulationBoxes=>null()
        this%pSimulationCtrlParamList=>null()
        this%name = ""

        return
    end subroutine

    !**************************************************
    subroutine CleanMethodClass(this)
        implicit none
        !---Dummy Vars---
        type(MCMethodClassGPU)::this
        !---Body---
        call this%Clean_MethodClass()
        return
    end subroutine


end module MC_MethodClass_Factory_GPU
