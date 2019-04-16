function speed (t)

    implicit none

    real speed, t

    real r1, r2, v1, v2, v3

    save
    !
    ! This routine is the time derivative of wfpr2. It returns the
    ! shock front speed as a function of time.
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    !
    r1=24210.*t**0.371*(1.+(1.23*t+0.123)*(1.-exp(-26.25*t**.79)))
    r2=(1.-0.03291*t**(-1.086))*(33897.*t+8490.)+8.36e3+2.5e3*alog(t)+800.*t**(-0.21)
    v1=10086.685*t**(-0.629)+40826.049*t**0.371+exp(-26.25*t**0.79)*   &
        (617527.5*t**1.161-40826.049*t**0.371-1104.7749*t**(-0.629)+61752.75*t**0.161)
    v2=33897.+95.937326*t**(-1.086)+303.43481*t**(-2.086)+2.5e3/t-168.*t**(-1.21)
    v3=(v2*(t-0.21)+v1*(0.28-t))/0.07

    if (t .lt. 0.21) then
        speed=v1
    else if (t .gt. 0.28) then
        speed=v2
    else
        speed=v3
    endif

    return
end
