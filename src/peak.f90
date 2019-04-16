subroutine peak(t,ra,prado,oppko,odpko,opro,odro,vpko,vro)

    implicit none

    save

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real t,ra,prado,oppko,odpko,opro,odro,vpko,vro

    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real told /0./
    real, parameter :: psca = 0.1, vsca = 0.01, rhosca = 1000.

    real r, ow, rzdm, rzvm, odw

    real wfvzr, wfpr, wfzr, wfdzr, wfpkop, wfpkod, wfpkv

    ! Convert distance to cm
    r=ra*100.

    ! Calculate waveform radius at peak overpressure.
    if (t .ne. told) then

        rzp=wfzr(t)
        rzd=wfdzr(t)
        rzv=wfvzr(t)
        prad=wfpr(t)

        prado=prad*vsca

        ! Calculate waveform peak overpressure.
        oppk=wfpkop(prad)
        oppko=oppk*psca

        !Calculate waveform peak overdensity.
        odpk=wfpkod(prad)
        odpko=odpk*rhosca

        ! Calculate waveform peak velocity.
        vpk=wfpkv(prad)
        vpko=vpk*vsca
        told=t
    endif

    opr=0.
    odr=0.
    vr=0.
    opro=0.
    odro=0.
    vro=0.

    if (r .le. prad) then

        ! Calculate overpressure at r.
        call wfprmt (t,r)

        opro=opr*psca

        ! Calculate overdensity at r.
        call wfdrmt (t,r)
        call well (t,r,odw)
        odro=odw*rhosca

        ! Calculate velocity at r.
        call wfvrmt (t,r)
        vro=vr*vsca
    endif

    return
end
