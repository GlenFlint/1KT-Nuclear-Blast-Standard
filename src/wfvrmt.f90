subroutine wfvrmt (t,r)
    implicit none

    real t, r

    save

	! Calculate the velocity at r.
	!
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    include "wfrt.inc"

    real ttold /0./

    real aln, alpha, bcrmn, beta, bln, bgz, bgzl, cgz, cgzl, crnmlb, dnom
    real fmlt, fngz, hr, ovmhy, ovmnln, rbr, rpk, rmn, rz, wflt
    real rneg, rpls, rnp, crmnlb, gr, rtio

    rpk=prad

    if (t .ge. 0.15) then

        rpk=rpk*1.e-5
        r=r*1.e-5

        if (t .ne. ttold) then
            rz=rzv
            rmn=rz-9.31e3*t**0.12115
            rz=rz*1.e-5
            rmn=rmn*1.e-5
            vmn=650./(t*sqrt(t))-0.5*vpk
            rneg=rz-rmn
            rpls=rpk-rz
            rnp=rpk-rmn
            aln=vpk/rpls
            bln=vpk-aln*rpk
            ovmnln=aln*rmn+bln
            fmlt=abs(vmn/vpk)
            ovmhy=vmn+fmlt*(ovmnln-vmn)
            alpha=(rnp/(ovmhy-vpk)+rpls/vpk)/rneg
            beta=-rpls*(alpha+1./vpk)
            fngz=rnp/rpls
            dnom=alpha*rnp+beta
            bcrmn=1.-vmn/(rnp/dnom+vpk)
            crmnlb=alog(bcrmn)
            cgzl=(beta*(1./bcrmn-1.)/dnom)/((fngz*crmnlb*rnp**(fngz-1.))*(rnp+vpk*dnom))
            cgz=exp(cgzl)
            bgzl=crmnlb/cgz**(rnp**fngz)
            bgz=exp(bgzl)
        endif

        rbr=rpk-r
        gr=1.-bgz**(cgz**(rbr**fngz))
        if (r .gt. rz) gr=(rpk-r)/rpls*gr+(r-rz)/rpls
        hr=rbr/(alpha*rbr+beta)+vpk
        vr=gr*hr
        rpk=rpk*1.e5
        r=r*1.e5
    endif

    if (t .le. 0.2) then

        rtio=r/prad
        wflt=vpk*rtio*sqrt(rtio)

        if (t .lt. 0.15) then
            vr=wflt
        else
            vr=(wflt*(0.20-t)+vr*(t-0.15))*20.
        endif
    endif

    ttold=t

    return
end
