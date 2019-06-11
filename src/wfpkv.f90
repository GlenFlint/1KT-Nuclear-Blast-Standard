function wfpkv (dummy)

    implicit none

    real wfpkv, dummy

    save

    ! This routine computes the particle velocity at peak overpressure
    ! given the overdensity and overpressure using Rankine-Hugoniot
    ! relations.
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    include "wfrt.inc"

    wfpkv=sqrt(oppk*odpk/(AIR_DENSITY*(AIR_DENSITY+odpk)))

    return
end
