subroutine well (t,r,depth)
    implicit none

    real t,r,depth

    save

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn

    real rmsw

    depth=odr
    rmsw=amin1(0.7*rzd,1.55e4)

    if (t .le. 1.2 .and. r .le. rmsw) then
        depth=amax1(-1.21e-3,-1.5e-3*exp(-8.e-13*r**3))
        if (t .gt. 1.) depth=(1.2-t)*depth*5.
        if (r .ge. 0.9*rmsw) depth=10.*(depth*(rmsw-r)+odr*(r-0.9*rmsw))/rmsw
    endif

    return
end
