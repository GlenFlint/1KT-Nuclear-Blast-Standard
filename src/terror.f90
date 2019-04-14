subroutine terror (t, name)
    implicit none

    ! This routine is part of the AFWL 1KT Standard by Needham, et al.

    real t
    character*6 name

    write (6,2) t, name
    stop

2   format (1x/' Time ERROR.'/' t = ',1pe13.5,' seconds in routine ',a6/)

end
