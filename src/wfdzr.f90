function wfdzr (t)
    implicit none
    real wfdzr, t
    save

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real b /0.03499/, c /-1.068/, cz /33897./, bz /8490./

    wfdzr=0.

    if (t .lt. 0.) then
        call terror (t, 'wfdzr ')
    else if (t .gt. 0. .and. t .lt. 0.265) then
        wfdzr=2.568e4*t**0.395
    else
        wfdzr=(1.-b*t**c)*(cz*t+bz)+500.
    endif

    return
end
