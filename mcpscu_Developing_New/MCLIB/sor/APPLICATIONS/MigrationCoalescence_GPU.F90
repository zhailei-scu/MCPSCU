program MigrationCoalescence_GPU_Main
    use MC_SimBoxArray_AppShell_GPU
    implicit none
    !---Local Vars---
    integer::nmpi
    integer::procid
    !---Body----
    nmpi = 1
    procid = 1

    !---Exclute the main process---
    call AppShell_Main_GPU(nmpi,procid)

    write(*,*) "---End MigrationCoalescence_GPU---"

end program MigrationCoalescence_GPU_Main
