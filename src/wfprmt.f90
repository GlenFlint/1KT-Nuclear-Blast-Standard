subroutine wfprmt (t,r)
    implicit none

    real t, r

    save
    ! Calculate the overpressure at r.
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    !
    include "wfrt.inc"

    real ttold /0./

    real a, alpha, aln, b, beta, bcrmn, bgz, bgzl, bln, c, cgz, cgzl, crmnlb
    real dnom, fngz, fmlt, gr, hr, rbr, rmn, rneg, rpls, rnp, rpk, rz, wflt
    real opmnln, opmhy

    rpk=prad

    if (t .ge. 0.1) then

        rpk=rpk*1.e-5
        r=r*1.e-5

        if (t .ne. ttold) then
            rz=rzp
            rmn=rz-9.7163e3*t**0.12115
            rz=rz*1.e-5
            rmn=rmn*1.e-5
            opmn=2.2e4/(t*sqrt(t))-0.5*oppk
            rneg=rz-rmn
            rpls=rpk-rz
            rnp=rpk-rmn
            aln=oppk/rpls
            bln=oppk-aln*rpk
            opmnln=aln*rmn+bln
            fmlt=abs(opmn/oppk)
            opmhy=opmn+fmlt*(opmnln-opmn)
            alpha=(rnp/(opmhy-oppk)+rpls/oppk)/rneg
            beta=-rpls*(alpha+1./oppk)
            fngz=rnp/rpls
            dnom=alpha*rnp+beta
            bcrmn=1.-opmn/(rnp/dnom+oppk)
            crmnlb=alog(bcrmn)
            cgzl=(beta*(1./bcrmn-1.)/dnom)/((fngz*crmnlb*rnp**(fngz-1.))*(rnp+oppk*dnom))
            cgz=exp(cgzl)
            bgzl=crmnlb/cgz**(rnp**fngz)
            bgz=exp(bgzl)
        endif

        rbr=rpk-r
        gr=1.-bgz**(cgz**(rbr**fngz))
        if (r .gt. rz) gr=(rpk-r)/rpls*gr+(r-rz)/rpls
        hr=rbr/(alpha*rbr+beta)+oppk
        opr=gr*hr
        rpk=rpk*1.e5
        r=r*1.e5
    endif

    if (t .lt. 0.95) then
        if (t .ne. ttold) then
            a=5.446e4*t**(-1.22)-7.135e5*(1.-t**2)
            if (t .lt. 0.2) c=7.41e-5*t**(-0.885)
            if (t .ge. 0.2) c=alog(max(1.e-20,-a/(oppk-a)))/(rzp-rpk)
            b=(oppk-a)*exp(-c*rpk)
        endif

        wflt=a+b*exp(c*r)

        if (t .lt. 0.1) then
            opr=wflt
        else
            opr=(wflt*(0.95-t)+opr*(t-0.1))/.85
        endif
    endif

    ttold=t

    return
end
