function wfpkod (dummy)
    implicit none

    real wfpkod, dummy

    save

    ! This routine computes the peak overdensity given the peak
    !overpressure using Rankine-Hugoniot relations.
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real rhoz /1.225e-3/, opz /1.01325e6/, gamm1 /0.404574/
    real op, rtio, p, gmone, gamma, gamra, rho1, ee, gamrao
    real dum1, dum2

    integer n

    op=oppk
    rtio=op/opz
    p=op+opz
    gmone=gamm1
    gamma=gamm1+1.
    gamra=gamma

    do 2 n=1,20
        rho1=rhoz*((2.*gamra+(gamra+1.)*rtio)/(2.*gamra+(gamra-1.)*rtio))
        ee=p/(gmone*rho1)
        call air (ee,rho1,gmone,dum1,dum2)
        gamrao=gamra
        gamra=2.*gmone/(2.5*gmone+1.)+1.
        if (abs(gamra-gamrao) .lt. 1.e-5) go to 3
2   continue

3   wfpkod=rho1-rhoz

    return
end
