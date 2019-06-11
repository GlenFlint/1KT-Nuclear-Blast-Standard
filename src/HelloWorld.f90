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

    integer i

    yield=180.
    height=900.0
    tim=15.0

    Print 940

    do 1 I=1,250
        ra=float(I)*25.0
        call shock(yield,height,tim,ra,opr,odr,vr)
        Print 950, tim,ra,opr,odr,vr
  1  continue

!            123456789012123456789012123456789012123456789012123456789012123456789012
 940 format("Time      Radius (m)    Pressure    Density     Velocity")
 950 format(1P8e12.4)
end
