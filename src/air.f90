subroutine air (eee,rrr,gmone,p,temp)

    implicit none

    real eee, rrr, gmone, p, temp

    real alr, beta, c1, c2, con, con1, con2, e, e1, e2,  &
         f, fe, fn, fo, fon,                             &
         g, gm, h, rholn, rhols, swit, ws

    e=eee*1.0e-10
    rholn=alog(773.39520495*rrr)
    e1=(8.5-e)*1.0256410256
    if (abs(e1) .lt. 5.) go to 20
    if (e1 .le. 0.) go to 10
    fo=exp(-0.22421524664*e)
    fon=0.
    ws=1.
    go to 30

10  fo=0.
    fon=exp(-0.15082956259*e)
    ws=0.
    go to 30

20  ws=(8.5+0.15504314*rholn-e)*exp(-0.05*rholn+0.02531780798)
    ws=1./(exp(-ws)+1.)
    fo=exp(-0.22421524664*e)*ws
    fon=exp(-0.15082956259*e)*(1.-ws)

30  beta=0.
    if (e .gt. 1.) beta=(6.9487e-3*ws+1.38974e-2)*alog(e)
    e2=(e-40.)*0.33333333333333
    if (abs(e2) .lt. 5.) go to 50
    if (e2 .gt. 0.) go to 40
    fn=0.
    ws=0.
    go to 60

40  fn=exp(-0.039215686275*e)
    ws=1.
    go to 60

50  ws=(e-exp(0.0157*rholn+3.806662489))*exp(-0.085*rholn-1.38629436)
    ws=1.0/(exp(-ws)+1.0)
    fn=exp(-0.039215686275*e)*ws

60  beta=beta+ws*(0.045-beta)
    ws=(e-160.)/amax1(30.+3.474356*rholn,6.)
    fe=0.
    if (ws .gt. -5.) fe=1./(exp(-ws)+1.)
    gm=(0.161+0.255*fo+0.28*fon+0.137*fn+0.05*fe)*exp(beta*rholn)
    gmone=gm
    p=gmone*eee*rrr
    rhols=rholn*rholn
    if (e .gt. 120.) go to 111
    f=9.72e-1-2.71434e-3*rholn+6.582549e-5*rhols
    g=2.645e-2-6.418873e-4*rholn-2.338785e-5*rhols
    h=-9.21e-5+5.971549e-6*rholn+3.923123e-7*rhols
    con1=3480.*gm*e
    con2=f+g*e+h*e*e
    temp=con1/con2
    go to 222

111 alr=alog10(rrr)
    c1=1.69081e-5+2.99265e-7*alr
    c2=7.69813e-1+3.8618e-3*alr
    temp=c1*eee**c2+102.6275735

222 continue
    beta=(e-3.)*1.5151515151
    if (beta .gt. 10.) return
    swit=1./(exp(beta)+1.)
    temp=con1/(con2*(1.-swit)+swit)
    if (temp .gt. 0.) return
    alr=alog10(rrr)
    c1=1.69081e-5+2.99265e-7*alr
    c2=7.69813e-1+3.8618e-3*alr
    temp=c1*eee**c2+102.6275735

    return
end
