subroutine scalkt (hfpt,wb,vScale,dScale,tScale,cScale,pScale)

    implicit none

    real hfpt,wb,vScale,dScale,tScale,cScale,pScale

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
    ! tScale - scales the actual time to the 1 KT sea level time
    ! vScale - scales the 1 KT sea level velocities to actual
    ! pScale - scales the 1 KT sea level pressures to actual
    ! cScale - scales the 1 KT sea level dimensions to actual
    ! dScale - scales the 1 KT sea level densities to actual
    !
    ! This routine is part of the AFWL 1KT Standard by Needham, et al.
    !

    real wsw, c3, r3, p3, t3, rp3, x, cubrt
    real hn(4)
    real, parameter :: AIR_PRESSURE_PASCALS = 1.01325e5     ! Ambient pressure (pascals) at sea level (page 65)
    !
    ! https://catsr.vse.gmu.edu/SYST460/LectureNotes_StandardAtmosphere_1.pdf
    ! http://www.sengpielaudio.com/calculator-speedsound.htm
    !
    real, parameter :: SPEED_OF_SOUND         = 3.4029399e2 ! Speed of sound (m/sec) at sea level
    real, parameter :: AIR_DENSITY_MG_PER_CM3 = 1.225       ! Ambient density (mg/cm3) at sea level (page 10)

    cubrt(x)=sign(exp(alog(abs(x))/3. ),x)

    call matm62 (hfpt,p3,c3,r3,t3)

    rp3=cubrt(AIR_PRESSURE_PASCALS/p3)
    wsw=cubrt(wb)
    tScale=c3/(wsw*rp3*SPEED_OF_SOUND)
    vScale=c3/SPEED_OF_SOUND
    pScale=p3/AIR_PRESSURE_PASCALS
    dScale=r3/AIR_DENSITY_MG_PER_CM3
    cScale=rp3*wsw

    return
end
