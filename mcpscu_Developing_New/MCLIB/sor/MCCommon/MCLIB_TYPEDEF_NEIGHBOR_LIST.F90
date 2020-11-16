module MCLIB_TYPEDEF_NEIGHBOR_LIST

  #ifdef MC_PROFILING
  USE MCLIB_TimeProfile
  #endif
  USE MCLIB_UTILITIES
  implicit none

  !*** The neighbor list
  TYPE,PUBLIC::NEIGHBOR_LIST
      integer,public,dimension(:,:),allocatable::m_INDI                ! the index of neighbores
      integer,public,dimension(:),allocatable::m_KVOIS                  ! the number of neighbores at current time step

      integer,private::NLUpdateCount_Host = 0

      contains
      procedure,non_overridable,public,pass::AllocateNEIGHBOR_LIST=>Allocate_NEIGHBOR_LIST
      procedure,non_overridable,public,pass::GetSTATUS_NEIGHBOR_LIST_Allocated=>STATUS_NEIGHBOR_LIST_Allocated
      procedure,non_overridable,public,pass::GetNeighborListSize=>Get_NeighborListSize
      procedure,non_overridable,public,pass::ResizeNeighborList=>Resize_NeighborList
      procedure,non_overridable,public,pass::DumplicateNeighborList=>Dumplicate_NeighborList
      procedure,non_overridable,private,pass::CopyNeighborList=>Copy_NeighborList
      procedure,non_overridable,public,pass::GetNLUpdateCount_Host=>Get_NLUpdateCount_Host
      procedure,non_overridable,public,pass::SetNLUpdateCount_Host=>Set_NLUpdateCount_Host
      procedure,non_overridable,public,pass::IncreaseOneNLUpdateCount_Host=>IncreaseOne_NLUpdateCount_Host
      procedure,non_overridable,public,pass::Release=>Release_NEIGHBOR_LIST
      !---Overload the operators
      GENERIC::ASSIGNMENT(=)=>CopyNeighborList

      !---Deconstructor---
      Final::CLEAR_NEIGHBOR_LIST

  END TYPE

  private::Allocate_NEIGHBOR_LIST
  private::STATUS_NEIGHBOR_LIST_Allocated
  private::Get_NeighborListSize
  private::Resize_NeighborList
  private::Dumplicate_NeighborList
  private::Copy_NeighborList
  private::Get_NLUpdateCount_Host
  private::Set_NLUpdateCount_Host
  private::IncreaseOne_NLUpdateCount_Host
  private::Release_NEIGHBOR_LIST
  private::CLEAR_NEIGHBOR_LIST

  contains
  !************************************************************************************************
  subroutine Allocate_NEIGHBOR_LIST(this,NC,NeigborNum)
      !***  PURPOSE:  to allocate the memory allocated in List
      !     Note:value of mxKVOIS in optional
      !
      !     INPUT:     this,        the neighbore list that need to be allocated
      !                NC,          the number of particles
      !                NeigborNum,  user defined maxmum permitted number of neighbores

      implicit none
      !---Dummy Vars---
      CLASS(NEIGHBOR_LIST)::this
      INTEGER::NC
      INTEGER::NeigborNum
      !---Local Vars---
      INTEGER::istat

      #ifdef MC_PROFILING
      call Time_Start(T_Allocate_NEIGHBOR_LIST_Start)
      #endif
      !---Body---
      ! ALLOCATE
      call AllocateArray_Host(this%m_INDI,NC,NeigborNum,"m_INDI")

      call AllocateArray_Host(this%m_KVOIS,NC,"m_KVOIS")

      this%m_INDI = 0
      this%m_KVOIS = 0

      this%NLUpdateCount_Host = 0

      #ifdef MC_PROFILING
      call Time_Accumulate(T_Allocate_NEIGHBOR_LIST_Start,T_Allocate_NEIGHBOR_LIST)
      #endif
      return
  end subroutine Allocate_NEIGHBOR_LIST

  !************************************************************************************************
  logical function STATUS_NEIGHBOR_LIST_Allocated(this)
      !***  PURPOSE:  to check whether the members of List had been allocated
      !
      !     INPUT:     this, the neighbore list that need to be check
      implicit none
      !---Dummy Vars---
      CLASS(NEIGHBOR_LIST)::this

      #ifdef MC_PROFILING
      call Time_Start(T_STATUS_NEIGHBOR_LIST_Allocated_Start)
      #endif
      !---Body---
      STATUS_NEIGHBOR_LIST_Allocated = ( allocated(this%m_INDI) .AND. allocated(this%m_KVOIS) )

      #ifdef MC_PROFILING
      call Time_Accumulate(T_STATUS_NEIGHBOR_LIST_Allocated_Start,T_STATUS_NEIGHBOR_LIST_Allocated)
      #endif
      return
  end function STATUS_NEIGHBOR_LIST_Allocated

  !*********************************************************************************************
  subroutine Get_NeighborListSize(this,ClustersNum,NeighborNum)
      implicit none
      !---Dummy Vars---
      Class(NEIGHBOR_LIST)::this
      integer::ClustersNum
      integer::NeighborNum
      !---Local Vars---
      integer::TheShape(2)
      !---Body---
      ClustersNum = 0
      NeighborNum = 0

      if(allocated(this%m_INDI)) then
        TheShape = shape(this%m_INDI)
        ClustersNum = TheShape(1)
        NeighborNum = TheShape(2)
      end if

      if(ClustersNum .ne. size(this%m_KVOIS)) then
        write(*,*) "MCPSCUERROR: The clusters number for m_INDI and m_KVOIS are not same."
        pause
      end if

      return
  end subroutine Get_NeighborListSize

  !**********************************************************************************************
  subroutine Resize_NeighborList(this,newClustersNum,newNeighborNum)
      implicit none
      !---Dummy Vars---
      class(NEIGHBOR_LIST)::this
      integer::newClustersNum
      integer::newNeighborNum
      !---Body---

      call ResizeArrayi_TwoDim(this%m_INDI,newClustersNum,newNeighborNum)

      call ResizeArrayi_OneDim(this%m_KVOIS,newClustersNum)

      return
  end subroutine Resize_NeighborList

  !************************************************************************************************
  subroutine Dumplicate_NeighborList(this,DumplicateNum)
      implicit none
      !---Dummy Vars---
      class(NEIGHBOR_LIST)::this
      integer,intent(in)::DumplicateNum
      !------Body------
      call DumplicateArrayi_TwoDim(this%m_INDI,DumplicateNum)

      call DumplicateArrayi_OneDim(this%m_KVOIS,DumplicateNum)

      return
  end subroutine Dumplicate_NeighborList

  !************************************************************************************************
  subroutine Release_NEIGHBOR_LIST(this)
      implicit none
      !---Dummy Vars---
      CLASS(NEIGHBOR_LIST)::this
      !---Body---

      call DeAllocateArray_Host(this%m_INDI,"m_INDI")

      call DeAllocateArray_Host(this%m_KVOIS,"m_KVOIS")

      this%NLUpdateCount_Host = 0

      return
  end subroutine



  !************************************************************************************************
  subroutine CLEAR_NEIGHBOR_LIST(this)
      !***  PURPOSE:  to deallocate the memory allocated in List
      !
      !     INPUT:    this, the neighbore list that need to be deallocated
      implicit none
      !---Dummy Vars---
      type(NEIGHBOR_LIST)::this
      !---Local Vars---
      integer::istat
      !---Body---
      #ifdef MC_PROFILING
      call Time_Start(T_CLEAR_NEIGHBOR_LIST_Start)
      #endif
      !---Body---

      call this%Release()

      this%NLUpdateCount_Host = 0

      #ifdef MC_PROFILING
      call Time_Accumulate(T_CLEAR_NEIGHBOR_LIST_Start,T_CLEAR_NEIGHBOR_LIST)
      #endif
      return
  end subroutine CLEAR_NEIGHBOR_LIST

  !**********************************************************************************************
  subroutine Copy_NeighborList(Dist_List,Source_List)
      implicit none
      !---Dummy Vars---
      CLASS(NEIGHBOR_LIST),intent(out)::Dist_List
      type(NEIGHBOR_LIST),intent(in)::Source_List
      !---Local Vars---
      integer::Sor_Size
      integer::Source_NeighborNum

      call Source_List%GetNeighborListSize(Sor_Size,Source_NeighborNum)

      Dist_List%m_INDI = reshape(SOURCE=[Source_List%m_INDI],SHAPE=[Sor_Size,Source_NeighborNum])

      Dist_List%m_KVOIS = reshape(SOURCE=[Source_List%m_KVOIS],SHAPE=[Sor_Size])

      Dist_List%NLUpdateCount_Host = Source_List%NLUpdateCount_Host

      return
  end subroutine Copy_NeighborList

  !**************************************************************************************
  function Get_NLUpdateCount_Host(this) result(TheCount)
    implicit none
    !---Dummy Vars---
    CLASS(NEIGHBOR_LIST)::this
    integer,intent(out)::TheCount
    !---Body---

    TheCount = this%NLUpdateCount_Host
    return
  end function Get_NLUpdateCount_Host

  !**************************************************************************************
  subroutine Set_NLUpdateCount_Host(this,TheCount)
    implicit none
    !---Dummy Vars---
    CLASS(NEIGHBOR_LIST)::this
    integer,intent(in)::TheCount
    !---Body---

    this%NLUpdateCount_Host = TheCount
    return
  end subroutine Set_NLUpdateCount_Host

  !**************************************************************************************
  subroutine IncreaseOne_NLUpdateCount_Host(this)
    implicit none
    !---Dummy Vars---
    CLASS(NEIGHBOR_LIST)::this
    !---Body---
    this%NLUpdateCount_Host = this%NLUpdateCount_Host + 1
    return
  end subroutine IncreaseOne_NLUpdateCount_Host

end module MCLIB_TYPEDEF_NEIGHBOR_LIST
