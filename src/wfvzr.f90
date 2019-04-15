function wfvzr (t)
    implicit none

    real wfvzr, t

    save

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real b /0.08459/, c /-1.34/, cz /33897./, bz /8490./

    wfvzr=0.

    if (t .lt. 0.) then
        call terror (t, 'wfvzr ')
    else if (t .gt. 0. .and. t .lt. 0.263) then
        wfvzr=2.5e4*t**0.8
    else
        wfvzr=(1.-b*t**c)*(cz*t+bz)
    endif

    return
end
