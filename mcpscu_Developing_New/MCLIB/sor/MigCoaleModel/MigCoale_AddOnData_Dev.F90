module MIGCOALE_ADDONDATA_DEV
    use MIGCOALE_ADDONDATA_HOST
    implicit none

    real(kind=KINDDF),constant::dm_FREESURDIFPRE
    real(kind=KINDDF),constant::dm_GBSURDIFPRE
    real(kind=KINDDF),constant::dm_OutRadius
    real(kind=KINDDF),constant::dm_CascadeCenter(3)

    contains

    subroutine CopyAddOnDataToDev()
        implicit none

        dm_FREESURDIFPRE = m_FREESURDIFPRE
        dm_GBSURDIFPRE = m_GBSURDIFPRE

        dm_OutRadius = m_OutRadius
        dm_CascadeCenter = m_CascadeCenter
        return
    end subroutine CopyAddOnDataToDev



end module MIGCOALE_ADDONDATA_DEV
