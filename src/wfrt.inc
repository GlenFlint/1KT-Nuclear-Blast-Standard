    ! wfrt - wave front common block

	! prad - peak radius
	! oppk - peak overpressure
	! odpk - peak overdensity
	! vpk  - peak velocity
	! opr  - overpressure at radius?
	! odr  - overdensity  at radius?
	! vr   - velocity at radius?
	! rzp  - pressure?
	! rzd  - density?
	! rzv  - velocity?
	! opmn - minimum overpressure?
	! odmn - minimum overdensity?
	! vmn  - minimum velocity? 
	!
    common /wfrt/ prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    real          prad,oppk,odpk,vpk,opr,odr,vr,rzp,rzd,rzv,opmn,odmn,vmn
    
    real, parameter :: AIR_DENSITY  = 1.225e-3  ! Ambient air density (g/cm**3) at sea level (page 10)