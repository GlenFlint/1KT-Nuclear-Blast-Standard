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
    real vScale, dScale, tScale, cScale, pScale

    integer i
    logical skipping, allZeroes
    real, parameter :: DELTA_RA = 25.0

    yield=1000.
    height=0.0
    tim=15.0

    skipping = .FALSE.
    allZeroes = .FALSE.

    call scalkt (height,yield,vScale,dScale,tScale,cScale,pScale)

    Print 930
    Print 950, tScale,vScale,pScale,dScale,cScale

    Print 920

    Print 940

    do 1 I=1,250
        ra=float(I)*DELTA_RA
        call shock(yield,height,tim,ra,opr,odr,vr)

        allZeroes = opr .EQ. 0.0 .and. odr .EQ. 0.0 .and. vr .EQ. 0.0

        if (.not. allZeroes) then
           if (skipping) then
              Print 950, tim, ra-DELTA_RA, 0.0, 0.0, 0.0
              skipping = .FALSE.
           end if
           Print 950, tim,ra,opr,odr,vr
        else if (.not. skipping) then
           Print 950, tim,ra,opr,odr,vr  !  Print all zeroes
           Print 960
           skipping = .TRUE.
        end if
  1  continue

     if (skipping) then
        Print 950, tim,ra,opr,odr,vr  !  Print all zeroes
     end if

!            12345678901234567890123456789012345678901234567890123456789012
 920 format(/)
 930 format("  TScale      VScale      PScale      DScale      CScale")
 940 format("  Time        Radius (m)  Pressure    Density     Velocity")
 950 format(1P8e12.4)
 960 format("  ...")
end
