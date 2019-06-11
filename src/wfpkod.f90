function wfpkod (dummy)
    implicit none

    real wfpkod, dummy

    save

    ! This routine computes the peak overdensity given the peak
    ! overpressure using Rankine-Hugoniot relations.
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    include "wfrt.inc"

    real, parameter :: AIR_PRESSURE = 1.01325e6 ! Ambient pressure (dyne/cm**2) at sea level (page 65)
    
    real, parameter :: GAMMA = 1.404574 ! Effective gamma (page 83)
    
    real op, ratio, p, gmone, gamra, rho1, ee, gamrao
    real dum1, dum2

    integer n

    op    = oppk				! Overpressure
    ratio = op / AIR_PRESSURE
    p     = op + AIR_PRESSURE	! Pressure at shock front (Equation 3.1)
    
    gmone = GAMMA - 1
    gamra = GAMMA

    do 2 n=1,20
        rho1=AIR_DENSITY*((2.*gamra+(gamra+1.)*ratio)/(2.*gamra+(gamra-1.)*ratio))  ! Equation 3.2
        ee=p/(gmone*rho1)						! Figure 3.1 - Equation of State
        call air (ee,rho1,gmone,dum1,dum2)
        gamrao=gamra
        gamra=2.*gmone/(2.5*gmone+1.)+1.
        if (abs(gamra-gamrao) .lt. 1.e-5) go to 3
2   continue

3   wfpkod=rho1-AIR_DENSITY

    return
end
