function wfpkop (r)

    implicit none

    real wfpkop, r
    real
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
    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real ac /3.04e18/, aq /1.13e14/, astar /7.9e9/, rstar /4.454e4/

    real rr, rtio, cf

    rr=1./r
    rtio=2.24517e-5*r
    cf=sqrt(alog(rtio+3.*exp(-(sqrt(rtio)/3.))))
    wfpkop=((ac*rr+aq)*rr+astar/cf)*rr

    return
end
