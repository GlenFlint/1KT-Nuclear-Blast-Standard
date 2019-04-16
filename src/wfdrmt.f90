subroutine wfdrmt (t,r)
    implicit none
    real t, r

    save

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real ttold /0./

    real rpk, rmn, rz, rnp, rneg, rbr, odmnln, alpha, beta, crmnlb,  &
         a, b, c, fngz, aln, bln, bgz, bgzl, cgz, cgzl, rpls, fmlt,  &
         dnom, bcrmn, gr, hr, odmhy, wflt


    rpk=prad

    if (t .ge. 0.2) then

        rpk=rpk*1.e-5
        r=r*1.e-5

        if (t .ne. ttold) then
            rz=rzd
            rmn=rz-9.7163e3*t**0.12115
            rz=rz*1.e-5
            rmn=rmn*1.e-5
            odmn=-0.5*odpk+2.2e-5*t**(-1.6026)
            rneg=rz-rmn
            rpls=rpk-rz
            rnp=rpk-rmn
            aln=odpk/rpls
            bln=odpk-aln*rpk
            odmnln=aln*rmn+bln
            fmlt=abs(odmn/odpk)
            odmhy=odmn+fmlt*(odmnln-odmn)
            alpha=(rnp/(odmhy-odpk)+rpls/odpk)/rneg
            beta=-rpls*(alpha+1./odpk)
            fngz=rnp/rpls
            dnom=alpha*rnp+beta
            bcrmn=1.-odmn/(rnp/dnom+odpk)
            crmnlb=alog(bcrmn)

            cgzl=(beta*(1./bcrmn-1.)/dnom)/  &
                 ((fngz*crmnlb*rnp**(fngz-1.))*(rnp+odpk*dnom))

            cgz=exp(cgzl)
            bgzl=crmnlb/cgz**(rnp**fngz)
            bgz=exp(bgzl)
        endif

        rbr=rpk-r
        gr=1.-bgz**(cgz**(rbr**fngz))
        if (r .gt. rz) gr=(rpk-r)/rpls*gr+(r-rz)/rpls
        hr=rbr/(alpha*rbr+beta)+odpk
        odr=gr*hr
        rpk=rpk*1.e5
        r=r*1.e5
    endif

    if (t .le. 1.) then
        if (t .ne. ttold) then
            a=-1.2e-3
            c=alog(max(1.e-20,-a/(odpk-a)))/(rzd-rpk)
            b=(odpk-a)*exp(-c*rpk)
        endif

        wflt=a+b*exp(c*r)

        if (t .le. 0.2) then
            odr=wflt
        else
            odr=(wflt*(1.-t)+odr*(t-0.2))*1.25
        endif
    endif

    ttold=t

    return
end
