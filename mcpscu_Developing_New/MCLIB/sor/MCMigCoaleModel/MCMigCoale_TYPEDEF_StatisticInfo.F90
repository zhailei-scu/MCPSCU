module MCMIGCOALE_TYPEDEF_STATISTICINFO

    use MCLIB_CONSTANTS
    use MCLIB_UTILITIES

    implicit none

    type,public::MCMigCoaleStatisticOneBox

        integer::ICMAX(p_NUMBER_OF_STATU) = 0                               ! the ID of the largest cluster
        real(kind=KINDDF)::RMAX(p_NUMBER_OF_STATU) = 0.D0                    ! the radius of the largest cluster at current time intervale
        real(kind=KINDDF)::RMIN(p_NUMBER_OF_STATU) = 0.D0                    ! the radius of the smallest cluster at current time intervale, to be used to determine the time step
        real(kind=KINDDF)::RAVA(p_NUMBER_OF_STATU) = 0.D0                    ! the average radius of the clusters at current time intervale

        real(kind=KINDDF)::DiffusorValueMax(p_NUMBER_OF_STATU) = 0.D0

        contains
        procedure,non_overridable,public,pass::InitStatisticInfo
        procedure,non_overridable,public,pass::Clean_StatisticInfo
        procedure,non_overridable,private,pass::CopyMCMigCoaleStatisticOneBoxFromOther
        Generic::Assignment(=)=>CopyMCMigCoaleStatisticOneBoxFromOther
        Final::CleanStatisticInfo

    end type MCMigCoaleStatisticOneBox

    type,public::MCMigCoaleStatisticInfo
        type(MCMigCoaleStatisticOneBox),dimension(:),allocatable::statistic_SingleBoxes
        type(MCMigCoaleStatisticOneBox)::statistic_IntegralBox

        contains
        procedure,non_overridable,public,pass::Init=>InitMCMigCoaleStatisticInfo
        procedure,non_overridable,public,pass::Clean=>Clean_MCMigCoaleStatisticInfo
        procedure,non_overridable,private,pass::CopyMCMigCoaleStatisticInfoFromOther
        Generic::Assignment(=)=>CopyMCMigCoaleStatisticInfoFromOther
        Final::CleanMCMigCoaleStatisticInfo
    end type MCMigCoaleStatisticInfo

    type,public,extends(MCMigCoaleStatisticInfo)::MCMigCoaleStatisticInfo_Used

    end type

    type,public,extends(MCMigCoaleStatisticInfo)::MCMigCoaleStatisticInfo_Expd
        contains
        procedure,non_overridable,public,pass::ConverFromUsed=>ConvertUsedToExpd
    end type

    type,public,extends(MCMigCoaleStatisticInfo)::MCMigCoaleStatisticInfo_Virtual
        contains
        procedure,non_overridable,public,pass::ConverFromUsed=>ConvertUsedToVirtual
    end type


    type,public::MCMigCoaleStatInfoWrap
        type(MCMigCoaleStatisticInfo_Used)::m_MCMigCoaleStatisticInfo_Used
        type(MCMigCoaleStatisticInfo_Expd)::m_MCMigCoaleStatisticInfo_Expd
        type(MCMigCoaleStatisticInfo_Virtual)::m_MCMigCoaleStatisticInfo_Virtual

        contains
        procedure,non_overridable,public,pass::Init=>InitMCMigCoaleStatInfoWrap
        procedure,non_overridable,public,pass::Clean=>Clean_MCMigCoaleStatInfoWrap
        Final::CleanMCMigCoaleStatInfoWrap
    end type


    private::InitStatisticInfo
    private::CopyMCMigCoaleStatisticOneBoxFromOther
    private::Clean_StatisticInfo
    private::CleanStatisticInfo
    private::InitMCMigCoaleStatisticInfo
    private::CopyMCMigCoaleStatisticInfoFromOther
    private::Clean_MCMigCoaleStatisticInfo
    private::CleanMCMigCoaleStatisticInfo
    private::ConvertUsedToExpd
    private::ConvertUsedToVirtual
    private::InitMCMigCoaleStatInfoWrap
    private::Clean_MCMigCoaleStatInfoWrap
    private::CleanMCMigCoaleStatInfoWrap

    contains

    !****************Type MCMigCoaleStatisticOneBox*****************************
    subroutine InitStatisticInfo(this)
        implicit none
        !---Dummy Vars---
        Class(MCMigCoaleStatisticOneBox)::this
        !---Body---

        this%ICMAX = 0
        this%RMAX = 0.D0
        this%RMIN = 0.D0
        this%RAVA = 0.D0

        this%DiffusorValueMax = 0.D0

        return
    end subroutine InitStatisticInfo

    !***********************************************
    subroutine CopyMCMigCoaleStatisticOneBoxFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoaleStatisticOneBox),intent(out)::this
        TYPE(MCMigCoaleStatisticOneBox),intent(in)::other
        !---Body---
        this%RMAX = other%RMAX
        this%RMIN = other%RMIN
        this%RAVA = other%RAVA

        this%ICMAX = other%ICMAX

        this%DiffusorValueMax = other%DiffusorValueMax

        return
    end subroutine

    !**********************************************
    subroutine Clean_StatisticInfo(this)
        implicit none
        !---Dummy Vars---
        Class(MCMigCoaleStatisticOneBox)::this
        !---Body---

        this%ICMAX = 0
        this%RMAX = 0.D0
        this%RMIN = 0.D0
        this%RAVA = 0.D0

        this%DiffusorValueMax = 0.D0

        return
    end subroutine Clean_StatisticInfo

    subroutine CleanStatisticInfo(this)
        implicit none
        !---Dummy Vars---
        type(MCMigCoaleStatisticOneBox)::this
        !---Body---

        call this%Clean_StatisticInfo()

        return
    end subroutine CleanStatisticInfo

    !****************Type MCMigCoaleStatisticOneBox*****************************
    subroutine InitMCMigCoaleStatisticInfo(this,MultiBox)
        implicit none
        !---Dummy Vars---
        Class(MCMigCoaleStatisticInfo)::this
        integer,intent(in)::MultiBox
        !---Local Vars---
        integer::IBox
        !---Body---
        if(allocated(this%statistic_SingleBoxes)) then
            deallocate(this%statistic_SingleBoxes)
        end if

        allocate(this%statistic_SingleBoxes(MultiBox))

        DO IBox = 1,MultiBox
            call this%statistic_SingleBoxes(IBox)%InitStatisticInfo()
        END DO

        call this%statistic_IntegralBox%InitStatisticInfo()

        return
    end subroutine

    !***********************************************
    subroutine CopyMCMigCoaleStatisticInfoFromOther(this,other)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoaleStatisticInfo),intent(out)::this
        TYPE(MCMigCoaleStatisticInfo),intent(in)::other
        !---Local Vars---
        integer::I
        !---Body---
        this%statistic_IntegralBox = other%statistic_IntegralBox

        if(allocated(this%statistic_SingleBoxes)) then
            deallocate(this%statistic_SingleBoxes)
        end if

        if(size(other%statistic_SingleBoxes) .GT. 0) then
             allocate(this%statistic_SingleBoxes(size(other%statistic_SingleBoxes)))

             DO I = 1,size(other%statistic_SingleBoxes)
                this%statistic_SingleBoxes(I) = other%statistic_SingleBoxes(I)
             END DO
        end if

        return
    end subroutine

    subroutine Clean_MCMigCoaleStatisticInfo(this)
        implicit none
        !---Dummy Vars---
        Class(MCMigCoaleStatisticInfo)::this
        !---Body---
        if(allocated(this%statistic_SingleBoxes)) then
            deallocate(this%statistic_SingleBoxes)
        end if

        call this%statistic_IntegralBox%Clean_StatisticInfo()

        return
    end subroutine Clean_MCMigCoaleStatisticInfo

    subroutine CleanMCMigCoaleStatisticInfo(this)
        implicit none
        !---Dummy Vars---
        type(MCMigCoaleStatisticInfo)::this
        !---Body---
        call this%Clean()

        return
    end subroutine CleanMCMigCoaleStatisticInfo

    !****************For type MCMigCoaleStatisticInfo_Expd*************************
    subroutine ConvertUsedToExpd(this,Used)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoaleStatisticInfo_Expd)::this
        TYPE(MCMigCoaleStatisticInfo_Used)::Used
        !---Local Vars---
        integer::I
        !---Body---
        this%statistic_IntegralBox = Used%statistic_IntegralBox

        if(allocated(this%statistic_SingleBoxes)) then
            deallocate(this%statistic_SingleBoxes)
        end if

        if(size(Used%statistic_SingleBoxes) .GT. 0) then
             allocate(this%statistic_SingleBoxes(size(Used%statistic_SingleBoxes)))

             DO I = 1,size(Used%statistic_SingleBoxes)
                this%statistic_SingleBoxes(I) = Used%statistic_SingleBoxes(I)
             END DO
        end if

        return
    end subroutine

    !****************For type MCMigCoaleStatisticInfo_Virtual*************************
    subroutine ConvertUsedToVirtual(this,Used)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoaleStatisticInfo_Virtual)::this
        TYPE(MCMigCoaleStatisticInfo_Used)::Used
        !---Local Vars---
        integer::I
        !---Body---
        this%statistic_IntegralBox = Used%statistic_IntegralBox

        if(allocated(this%statistic_SingleBoxes)) then
            deallocate(this%statistic_SingleBoxes)
        end if

        if(size(Used%statistic_SingleBoxes) .GT. 0) then
             allocate(this%statistic_SingleBoxes(size(Used%statistic_SingleBoxes)))

             DO I = 1,size(Used%statistic_SingleBoxes)
                this%statistic_SingleBoxes(I) = Used%statistic_SingleBoxes(I)
             END DO
        end if

        return
    end subroutine

    !****************For type MCMigCoaleStatInfoWrap************************
    subroutine InitMCMigCoaleStatInfoWrap(this,MultiBox)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoaleStatInfoWrap)::this
        integer,intent(in)::MultiBox
        !---Body---
        call this%m_MCMigCoaleStatisticInfo_Expd%Init(MultiBox)

        call this%m_MCMigCoaleStatisticInfo_Used%Init(MultiBox)

        call this%m_MCMigCoaleStatisticInfo_Virtual%Init(MultiBox)

        return
    end subroutine

    subroutine Clean_MCMigCoaleStatInfoWrap(this)
        implicit none
        !---Dummy Vars---
        CLASS(MCMigCoaleStatInfoWrap)::this
        !---Body---
        call this%m_MCMigCoaleStatisticInfo_Expd%Clean()

        call this%m_MCMigCoaleStatisticInfo_Used%Clean()

        call this%m_MCMigCoaleStatisticInfo_Virtual%Clean()

        return
    end subroutine

    subroutine CleanMCMigCoaleStatInfoWrap(this)
        implicit none
        !---Dummy Vars---
        type(MCMigCoaleStatInfoWrap)::this
        !---Body---
        call this%Clean()

        return
    end subroutine

end module MCMIGCOALE_TYPEDEF_STATISTICINFO