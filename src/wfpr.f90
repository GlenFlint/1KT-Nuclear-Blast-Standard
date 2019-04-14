function wfpr(t)

    implicit none

    real t, wfpr

    save

    ! This function also calculates the shock front radius at the specified time.

    real r1, r2

    r1=24210.*t**0.371*(1.+(1.23*t+0.123)*(1.-exp(-26.25*t**0.79)))
    r2=(1.-0.03291*t**(-1.086))*(33897.*t+8490.)+8.36e3+2.5e3*alog(t)+800.*t**(-0.21)

    if (t .lt. 0.21) then
        wfpr=r1
    else if (t .gt. 0.28) then
        wfpr=r2
    else
        wfpr=(r2*(t-0.21)+r1*(0.28-t))/0.07
    endif

    return
end
