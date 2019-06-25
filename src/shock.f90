subroutine shock (yield,height,tim,ra,opr,odr,vr)

    implicit none

    real, intent(in)  :: yield,height,tim,ra
    real, intent(out) :: opr,odr,vr

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real alt, vScale, dScale, tScale, cScale, pScale, t, r
    real prado, oppko, odpko, opro, odro, vpko, vro

    alt=height

    call scalkt (alt,yield,vScale,dScale,tScale,cScale,pScale)

    ! Scale time and range to the 1KT blast

    t=tim*tScale
    r=ra/cScale

    ! Given time and range for 1KT blast
    ! Find:
    ! Peak over pressure
    ! Peak over density
    ! Over pressure at range
    ! Over density at range
    ! Peak velocity at range
    ! velocity at range

    call peak (t,r,prado,oppko,odpko,opro,odro,vpko,vro)

    ! Now scale back to the yield of interest

    opr=opro*pScale
    odr=odro*dScale
    vr=vro*vScale

    return
end

