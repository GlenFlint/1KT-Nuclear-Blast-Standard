subroutine matm62 (ty,wsp,cs,wsr,wst)

    implicit none

    real ty, wsp, cs, wsr, wst

    save

    ! This routine supplies internal specific energy and density.
    !
    ! This routine is part of the DNA 1-Kt Standard by Needham, et al.
    !
    ! calculate atmosphere
    !
    ! tabat(1)=r, the gas constant in ergs/mole/degree
    ! tabat(2)=radius of the earth in cm.
    ! tabat(3)=acceleration due to gravity at sea level in cm/sec**2
    ! tabat(4)=molecular weight of air at sea level
    !
    ! tabz is altitude in cm.
    ! tabt is molecular scale temperature in degrees kelvin.
    ! tabl is molecular scale temperature gradient in deg./cm.
    ! tabp is pressure in dynes/cm**2.
    !
    ! nz is the number of altitudes. tabz, tabt, and tabp are
    ! dimensioned nz. tabl is dimensioned nz-1.
    !

    logical init

    data init/.false./

    ! Data for temperate atmosphere

    integer, parameter :: nz = 21
    real,    parameter :: rhoz = 1.22500000e-03

    real, dimension(4), parameter :: tabat = (/       &
     8.31440000e+07,6.35670000e+08,9.80665000e+02,2.89644000e+01/)

    real, dimension(22), parameter :: tabz = (/       &
     0.            , 1.10190000e+06, 2.00630000e+06, 3.21620000e+06,  &
     4.73500000e+06, 5.24290000e+06, 6.15910000e+06, 7.99940000e+06,  &
     9.00000000e+06, 1.00000000e+07, 1.10000000e+07, 1.20000000e+07,  &
     1.50000000e+07, 1.60000000e+07, 1.70000000e+07, 1.90000000e+07,  &
     2.20000000e+07, 3.00000000e+07, 4.00000000e+07, 5.00000000e+07,  &
     6.00000000e+07, 7.00000000e+07/)

    real, dimension(21), parameter :: tabl = (/    &
     -6.49291769e-05, 9.28018576e-08, 9.86255062e-06, 2.77080373e-05,  &
     -1.72248474e-07,-1.96000240e-05,-3.91696897e-05, 1.60821507e-07,  &
      2.98166740e-05, 5.02020080e-05, 9.97762300e-05, 2.00108809e-04,  &
      1.49589024e-04, 1.00407490e-04, 6.97598500e-05, 6.68801467e-05,  &
      3.49035000e-05, 3.31099360e-05, 2.58868500e-05, 1.71252960e-05,  &
      1.09162420e-05/)

    real, dimension(22), parameter :: tabt = (/                        &
      2.88150000e+02, 2.16604540e+02, 2.16688470e+02, 2.28621170e+02,  &
      2.70704137e+02, 2.70616652e+02, 2.52659110e+02, 1.80575130e+02,  &
      1.80736048e+02, 2.10552722e+02, 2.60754730e+02, 3.60530960e+02,  &
      9.60857386e+02, 1.11044641e+03, 1.21085390e+03, 1.35037360e+03,  &
      1.55101404e+03, 1.83024204e+03, 2.16134140e+03, 2.42020990e+03,  &
      2.59146286e+03, 2.70062528e+03/)

    real, dimension(22), parameter ::  tabp = (/                       &
      1.01325000e+06, 2.26320000e+05, 5.47486994e+04, 8.68013979e+03,  &
      1.10904998e+03, 5.90004987e+02, 1.82098959e+02, 1.03769924e+01,  &
      1.64379881e+00, 3.00749781e-01, 7.35439451e-02, 2.52169805e-02,  &
      5.06169601e-03, 3.69429709e-03, 2.79259780e-03, 1.68519867e-03,  &
      8.67381898e-04, 1.94317430e-04, 4.15743157e-05, 1.13023464e-05,  &
      3.55894454e-06, 1.22936355e-06/)

      real cons, dum1, dum2, dum3, fs, rvar1, re, trho, tty, var1, var2
      integer j

    ! Check if initialized.

1   if (init) go to 2

    ! Initialize.

    re=tabat(2)
    cons=re**2*tabat(3)*tabat(4)/tabat(1)
    trho=tabt(1)*rhoz*1.e3
    init=.true.

2   tty=ty*100.
    j=1
    if (tty .le. 0.) go to 5

    ! Find the altitude and index of point of interest.

    do 3 j=2,nz
        if (tty-tabz(j)) 4,5,3
3   continue

    j=nz+1

4   j=j-1

5   dum2=(tabz(j)-tty)/((re+tty)*(re+tabz(j)))
    dum3=(re+tabz(j))/(re+tty)
    var1=(re+tabz(j))*tabl(j)-tabt(j)
    rvar1=1./var1
    var2=((tty-tabz(j))*tabl(j)+tabt(j))/tabt(j)
    fs=-cons*(((tabl(j)*alog(dum3*var2))*rvar1+dum2)*rvar1)
    wsp=tabp(j)*exp(fs)
    wst=tabt(j)+tabl(j)*(tty-tabz(j))
    wsr=trho*wsp/(wst*tabp(1))
    wsp=0.1*wsp
    cs=sqrt(1.4*wsp/wsr)

    return
end
