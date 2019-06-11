function wfpkop (r)

    implicit none

    real wfpkop
    
    real, intent(in) :: r   ! Radius in cm

    save
    !
    ! Calculates the waveform peak overpressure (wfpkop) at the specified
    ! radius (r), i.e., the overpressure at the shock front having the
    ! specified radius (r).
    !
    ! r - Radius(cm)
    ! wfpkop - Peak overpressure (dynes/cm**2)
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    !
    ! See page 33
    !
    include "wfrt.inc"

    real, parameter :: A = 3.04e18
    real, parameter :: B = 1.13e14
    real, parameter :: C = 7.9e9
	real, parameter :: R0 = 4.454e4

    real ratio, cfactor

    ratio   = r/R0
    cfactor = sqrt(alog(ratio+3.*exp(-(sqrt(ratio)/3.))))
    wfpkop  = ((A/r+B)/r+C/cfactor)/r

    return
end
