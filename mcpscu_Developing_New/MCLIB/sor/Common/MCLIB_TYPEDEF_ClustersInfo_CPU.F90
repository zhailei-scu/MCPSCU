!*********************************************************************************!
!--- Description:
!--- Author : Lei Zhai, Insti. of Nucle. Sci. and Tech., Sichuan University
!--- Email : zhaileiytp@163.com
!--- data: From 2017-09 to 2021-12
!--- License: MIT License (There are no limitation for anyone to use/modify/sale of this code, be happy to use this code)
!--- Please ref "Lei Zhai, Chaoqiong Ma, Jiechao Cui and Qing Hou, GPU-based acceleration of Monte Carlo simulations for migration-coalescence evolution of gas bubbles in materials
!---                        Modelling Simul. Mater. Sci. Eng. 2019. 27 055008,https://iopscience.iop.org/article/10.1088/1361-651X/ab1d14
!*********************************************************************************!
module MCLIB_TYPEDEF_ClustersInfo_CPU

    USE MCLIB_CONSTANTS
    USE MCLIB_TYPEDEF_ACLUSTER
    USE MCLIB_TYPEDEF_NEIGHBOR_LIST

    implicit none

    type,public::ClustersInfo_CPU
        !---Cluster In Host
        type(Acluster), dimension(:), allocatable::m_Clusters
        !---NeighborList-Info in Host---
        type(NEIGHBOR_LIST)::m_list
        !---Active Index---
        integer, dimension(:),allocatable::m_ActiveIndex

        contains

        procedure,NON_OVERRIDABLE,pass,public::AllocateClustersInfo_CPU=>Allocate_ClustersInfo_CPU
        procedure,NON_OVERRIDABLE,pass,public::GetClustersInfo_ArraySize=>Get_ClustersInfo_ArraySize
        procedure,NON_OVERRIDABLE,pass,public::DumplicateClustersInfo_CPU=>Dumplicate_ClustersInfo_CPU
        procedure,NON_OVERRIDABLE,pass,private::Copy_ClustersInfo_CPU
        procedure,NON_OVERRIDABLE,nopass,public::GetMemoryConsuming_OneClusterInfo_CPU=>Get_MemoryConsuming_OneClusterInfo_CPU
        procedure,NON_OVERRIDABLE,pass,public::Clean=>Clean_ClustersInfo_CPU
        !---overload some operator---
        GENERIC::ASSIGNMENT(=)=>Copy_ClustersInfo_CPU

        !---DeConstructor function---
        FINAL::CleanClustersInfo_CPU

    end type ClustersInfo_CPU

    private::Allocate_ClustersInfo_CPU
    private::Get_ClustersInfo_ArraySize
    private::Dumplicate_ClustersInfo_CPU
    private::Copy_ClustersInfo_CPU
    private::Get_MemoryConsuming_OneClusterInfo_CPU
    private::Clean_ClustersInfo_CPU
    private::CleanClustersInfo_CPU

    contains

    !******************************************************************
    integer function Get_MemoryConsuming_OneClusterInfo_CPU(NeighborhoodsNum)
        implicit none
        !---Dummy Vars---
        integer,intent(in)::NeighborhoodsNum
        !---Local Vars---
        type(ClustersInfo_CPU)::oneClusterInfo_CPU
        !---Body---
        call oneClusterInfo_CPU%AllocateClustersInfo_CPU(1,NeighborhoodsNum)

        Get_MemoryConsuming_OneClusterInfo_CPU = sizeof(oneClusterInfo_CPU)

        call oneClusterInfo_CPU%Clean()

        return
    end function Get_MemoryConsuming_OneClusterInfo_CPU

    !******************************************************************
    subroutine Allocate_ClustersInfo_CPU(this,AllocSize,NeighborhoodsNum)
        implicit none
        !------Dummy Vars-------
        CLASS(ClustersInfo_CPU)::this
        integer,intent(in)::AllocSize
        integer,intent(in)::NeighborhoodsNum
        !------Local Vars------
        integer::istat
        !---------Body---------
        !---Cluster In host
        if(AllocSize .GT. 0) then

            call AllocateArray_Host(this%m_Clusters,AllocSize,"m_Clusters")

            !---Active Index---
            call AllocateArray_Host(this%m_ActiveIndex,AllocSize,"m_ActiveIndex")

            !---NeighborList-Info in host
            call this%m_list%AllocateNEIGHBOR_LIST(AllocSize,NeighborhoodsNum)

        end if
        return
    end subroutine Allocate_ClustersInfo_CPU

    !******************************************************************
    subroutine Clean_ClustersInfo_CPU(this)
        implicit none
        !------Dummy Vars-------
        CLASS(ClustersInfo_CPU)::this
        !---------Body---------
        !---Cluster In host
        call DeAllocateArray_Host(this%m_Clusters,"m_Clusters")

        !---NeighborList-Info in Device
        call this%m_list%Release()

        !---Active Index---
        call DeAllocateArray_Host(this%m_ActiveIndex,"m_ActiveIndex")

        return
    end subroutine Clean_ClustersInfo_CPU

    !******************************************************************
    subroutine CleanClustersInfo_CPU(this)
        implicit none
        !------Dummy Vars-------
        type(ClustersInfo_CPU)::this
        !---------Body---------
        call this%Clean()

        return
    end subroutine CleanClustersInfo_CPU

    !*****************************************************************
    subroutine Get_ClustersInfo_ArraySize(this,ClustersNum,NeigborNum)
        implicit none
        !---Dummy Vars---
        CLASS(ClustersInfo_CPU)::this
        integer,intent(out)::ClustersNum
        integer,intent(out)::NeigborNum
        !---Local Vars---
        integer::ArraySize
        integer::NeigborNum_NeighborList
        !---Body---
        ClustersNum = size(this%m_Clusters)

        call this%m_list%GetNeighborListSize(ArraySize,NeigborNum_NeighborList)

        if(ClustersNum .ne. ArraySize) then
            write(*,*) "MCPSCUERROR: The clusters number for clusters array is not same with Neighbor-List."
            pause
            stop
        end if

        if(ClustersNum .ne. size(this%m_ActiveIndex)) then
            write(*,*) "MCPSCUERROR: The clusters number for clusters array is not same with ActiveIndex."
            pause
            stop
        end if

        NeigborNum = NeigborNum_NeighborList
        return
    end subroutine Get_ClustersInfo_ArraySize

    !****************************************************************
    subroutine Dumplicate_ClustersInfo_CPU(this,DumplicateNum)
        implicit none
        !---Dummy Vars---
        CLASS(ClustersInfo_CPU)::this
        integer,intent(in)::DumplicateNum

        !---Body-----------

        !*********For Clusters Array****************
        call DumplicateClustersArray_OneDim(this%m_Clusters,DumplicateNum)

        !*********For Neighbor-List****************
        call this%m_list%DumplicateNeighborList(DumplicateNum)

        !********For ActiveIndex****************
        call DumplicateArrayi_OneDim(this%m_ActiveIndex,DumplicateNum)

        return
    end subroutine Dumplicate_ClustersInfo_CPU

    !*****************************************************************
    subroutine Copy_ClustersInfo_CPU(Dist_Info,Source_Info)
        implicit none
        !---Dummy Vars---
        CLASS(ClustersInfo_CPU),intent(out)::Dist_Info
        CLASS(ClustersInfo_CPU),intent(in)::Source_Info
        !---Local Vars---
        integer::SourceClustersNum
        integer::sourceNeighborNum
        !---Body----
        call Dist_Info%Clean()

        call Get_ClustersInfo_ArraySize(Source_Info,SourceClustersNum,sourceNeighborNum)

        if(SourceClustersNum .GT. 0) then
            Dist_Info%m_Clusters = reshape(SOURCE=[Source_Info%m_Clusters],SHAPE=[SourceClustersNum])

            !****Note: the Assignment(=) had been overloaded
            Dist_Info%m_list = Source_Info%m_list

            Dist_Info%m_ActiveIndex = reshape(SOURCE=[Source_Info%m_ActiveIndex],SHAPE=[SourceClustersNum])
        end if

        return
    end subroutine Copy_ClustersInfo_CPU

end module MCLIB_TYPEDEF_ClustersInfo_CPU
