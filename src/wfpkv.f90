function wfpkv (dummy)

    implicit none

    real wfpkv, dummy

    save

    ! This routine computes the particle velocity at peak overpressure
    ! given the overdensity and overpressure using Rankine-Hugoniot
    ! relations.
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real rhoz /1.225e-3/

    wfpkv=sqrt(oppk*odpk/(rhoz*(rhoz+odpk)))

    return
end
