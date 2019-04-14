subroutine shock (yield,height,tim,ra,opr,odr,vr)

    implicit none

    real yield,height,tim,ra,opr,odr,vr

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real alt, vsl, dsl, tsl, csl, psl, t, r

    alt=height

    call scalkt (alt,yield,vsl,dsl,tsl,csl,psl)

    t=tim*tsl
    r=ra/csl

    call peak (t,r,prado,oppko,odpko,opro,odro,vpko,vro)

    opr=opro*psl
    odr=odro*dsl
    vr=vro*vsl

    return
end

