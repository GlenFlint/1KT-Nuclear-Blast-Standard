subroutine scalkt (hfpt,wb,vscale,dscale,tscale,cscale,pscale)

    implicit none

    real hfpt,wb,vscale,dscale,tscale,cscale,pscale

    save
    ! This routine sets up the scale factors for each burst so the 1 KT
    ! sea level data may be used
    !
    ! Inputs
    !
    ! hfpt - Height of field point above sea level in meters
    ! wb   - Yield of the burst in KT
    !
    ! Output
    !
    ! tscale - scales the actual time to the 1 KT sea level time
    ! vscale - scales the 1 KT sea level velocities to actual
    ! pscale - scales the 1 KT sea level pressures to actual
    ! cscale - scales the 1 KT sea level dimensions to actual
    ! dscale - scales the 1 KT sea level densities to actual
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    !

    real wsw, c3, r3, p3, t3, rp3, x, cubrt
    real hn(4)
    real, parameter :: p1 = 1.01325e5, c1 = 3.4029399e2, r1 = 1.225, t1 = 288.15

    cubrt(x)=sign(exp(alog(abs(x))/3. ),x)

    call matm62 (hfpt,p3,c3,r3,t3)

    rp3=cubrt(p1/p3)
    wsw=cubrt(wb)
    tscale=c3/(wsw*rp3*c1)
    vscale=c3/c1
    pscale=p3/p1
    dscale=r3/r1
    cscale=rp3*wsw

    return
end
