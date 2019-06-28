module MIGCOALE_ADDONDATA_DEV
    use MIGCOALE_ADDONDATA_HOST
    implicit none

    real(kind=KINDDF),constant::dm_FREESURDIFPRE
    real(kind=KINDDF),constant::dm_GBSURDIFPRE

    contains

    subroutine CopyAddOnDataToDev()
        implicit none

        dm_FREESURDIFPRE = m_FREESURDIFPRE
        dm_GBSURDIFPRE = m_GBSURDIFPRE

        return
    end subroutine CopyAddOnDataToDev



end module MIGCOALE_ADDONDATA_DEV
