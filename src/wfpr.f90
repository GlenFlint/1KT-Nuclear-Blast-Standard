function wfpr(t)

    implicit none

    real, intent(in) :: t
    real :: wfpr

    save

    ! This function also calculates the shock front radius at the specified time.
    !
    ! See section 4.6.1.1
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real r_early, r_late

    r_early=24210.*t**0.371*(1.+(1.23*t+0.123)*(1.-exp(-26.25*t**0.79)))
    r_late=(1.-0.03291*t**(-1.086))*(33897.*t+8490.)+8.36e3+2.5e3*alog(t)+800.*t**(-0.21)

    if (t .lt. 0.21) then
        wfpr=r_early
    else if (t .gt. 0.28) then
        wfpr=r_late
    else
        wfpr=(r_late*(t-0.21)+r_early*(0.28-t))/0.07
    endif

    return
end
