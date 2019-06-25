function wfzr (t)
    implicit none

    real, intent(in) :: t

    save

    ! Calculates the waveform radius at zero overpressure, (wfzr).
    ! (i.e., the radius separating the negative and positive phase
    ! portions of the waveform, at the specified time (t)).
    !
    ! wfzr is not defined for times less than approximately 0.1 sec
    ! because for these times the waveform does not exhibit a
    ! negative phase.
    !
    !   t    - Time (sec)
    !   wfzr - Radius (cm)
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    
    ! See section 4.6.1.1
    !
    real, parameter :: B  = 0.03291
    real, parameter :: C  = -1.086
    real, parameter :: CZ = 33897.0
    real, parameter :: BZ = 8490.
    real wfzr

    wfzr=0.

    if (t .lt. 0.) then
        call terror (t, 'wfzr ')
    else
        wfzr=(1.-B*t**C)*(CZ*t+BZ)
    endif

    return
end
