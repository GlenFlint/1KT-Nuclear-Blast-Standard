! ============================================================================
! Name        : HelloWorld.f90
! Author      : Glen Flint
! Version     :
! Copyright   : Your copyright notice
! Description : Hello World in Fortran
! ============================================================================

Program MAIN
    implicit none

    real yield, height, tim, ra, opr, odr, vr

    yield=1.
    height=100.
    tim=.25

    do 1 I=1,250
        ra=float(I)
        call shock(yield,height,tim,ra,opr,odr,vr)
        Print 950, tim,ra,opr,odr,vr
  1  continue
 950 format(1P8e12.4)
end
