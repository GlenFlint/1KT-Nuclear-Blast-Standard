function wfvzr (t)
    implicit none

    real wfvzr, t

    save
    
    ! Calculates the waveform velocity at zero overpressure, (wfvzr)??  (Not sure about this.)
    ! (i.e., the radius separating the negative and positive phase
    ! portions of the waveform, at the specified time (t)).
    !
    ! wfvzr is not defined for times less than approximately 0.1 sec
    ! because for these times the waveform does not exhibit a
    ! negative phase.
    !
    !   t     - Time (sec)
    !   wfvzr - Radius (cm)
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    
    ! See section 4.6.1.1
    !
    real, parameter :: B  = 0.08459
    real, parameter :: C  = -1.34
    real, parameter :: CZ = 33897.0
    real, parameter :: BZ = 8490.

    wfvzr=0.

    if (t .lt. 0.) then
        call terror (t, 'wfvzr ')
    else if (t .gt. 0. .and. t .lt. 0.263) then
        wfvzr=2.5e4*t**0.8
    else
        wfvzr=(1.-B*t**C)*(CZ*t+BZ)   ! Equation 4.3???
    endif

    return
end
