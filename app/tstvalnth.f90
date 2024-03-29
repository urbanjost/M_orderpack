program chrono
use M_orderpack__valnth
use M_orderpack__mrgrnk
      Integer, Parameter :: nbcl = 10000
      Integer, Parameter :: kdp = selected_real_kind(15)
      Real(kind=kdp), Dimension (5001) :: dvalt
      Real, Dimension (5001) :: xvalt
      Integer, Dimension (5001) :: jvalt
      Integer, Dimension (5001) :: jrnkt
      Integer, Dimension (:), Allocatable :: jseet
      Integer :: nsee, ibcl, lrnk, jres
      Real :: tdep1, tdep2, tfin1, tfin2
      Real :: xres
      Real(kind=kdp) :: dres
!
      Call random_seed (size=nsee)
      Allocate (jseet(1:nsee))
!
      Call random_seed (get=jseet)
!      write (unit=*, fmt=*) jseet
!
      Call cpu_time (tdep1)
      Do ibcl = 1, nbcl
        Call random_number (xvalt(:))
        jvalt = Nint(1000.0*xvalt)
     End Do
     Call cpu_time (tfin1)
     Call random_seed (put=jseet)
     Call cpu_time (tdep2)
      Do ibcl = 1, nbcl
        Call random_number (xvalt(:))
         jvalt = Nint(1000.0*xvalt)
         lrnk = 10 + modulo (ibcl, 10)
         jres = valnth (jvalt, lrnk)
      End Do
      Call cpu_time (tfin2)
      write (unit=*, fmt=*) "Integer: ",((tfin2-tdep2)-(tfin1-tdep1))*1000.0/real(nbcl)," ms"
      Call random_seed (put=jseet)
      Do ibcl = 1, nbcl
        Call random_number (xvalt(:))
        jvalt = Nint(1000.0*xvalt)
        lrnk = 10 + modulo (ibcl, 10)
        jres = valnth (jvalt, lrnk)
        Call mrgrnk (jvalt, jrnkt)
         If (jvalt(jrnkt(lrnk)) /= jres) then
           write (unit=*, fmt=*) "*** Check Failed"
           write (unit=*, fmt=*) jvalt(jrnkt(lrnk))
           write (unit=*, fmt=*) jres
           write (unit=*, fmt=*) ibcl, "seed ", jseet
           stop
         End If
      End Do
!
      Call random_seed (put=jseet)
      Call cpu_time (tdep1)
      Do ibcl = 1, nbcl
         Call random_number (xvalt(:))
      End Do
     Call cpu_time (tfin1)
     Call random_seed (put=jseet)
     Call cpu_time (tdep2)
      Do ibcl = 1, nbcl
        Call random_number (xvalt(:))
         lrnk = 10 + modulo (ibcl, 10)
         xres = valnth (xvalt, lrnk)
      End Do
      Call cpu_time (tfin2)
      write (unit=*, fmt=*) "Real:    ",((tfin2-tdep2)-(tfin1-tdep1))*1000.0/real(nbcl)," ms"
      Call random_seed (put=jseet)
      Do ibcl = 1, nbcl
        Call random_number (xvalt(:))
        lrnk = 10 + modulo (ibcl, 10)
        xres = valnth (xvalt, lrnk)
        Call mrgrnk (xvalt, jrnkt)
         If (xvalt(jrnkt(lrnk)) /= xres) then
           write (unit=*, fmt=*) "*** Check Failed"
           write (unit=*, fmt=*) xvalt(jrnkt(lrnk))
           write (unit=*, fmt=*) xres
           write (unit=*, fmt=*) ibcl, "seed ", jseet
           stop
         End If
      End Do
!
      Call random_seed (put=jseet)
      Call cpu_time (tdep1)
      Do ibcl = 1, nbcl
         Call random_number (xvalt(:))
         dvalt = xvalt
     End Do
     Call cpu_time (tfin1)
     Call random_seed (put=jseet)
     Call cpu_time (tdep2)
      Do ibcl = 1, nbcl
         Call random_number (xvalt(:))
         dvalt = xvalt
         lrnk = 10 + modulo (ibcl, 10)
         dres = valnth (dvalt, lrnk)
      End Do
      Call cpu_time (tfin2)
      write (unit=*, fmt=*) "Double:  ",((tfin2-tdep2)-(tfin1-tdep1))*1000.0/real(nbcl)," ms"
      Call random_seed (put=jseet)
      Do ibcl = 1, nbcl
         Call random_number (xvalt(:))
         dvalt = xvalt
         lrnk = 10 + modulo (ibcl, 10)
         dres = valnth (dvalt, lrnk)
         Call mrgrnk (dvalt, jrnkt)
         If (dvalt(jrnkt(lrnk)) /= dres) then
           write (unit=*, fmt=*) "*** Check Failed"
           write (unit=*, fmt=*) dvalt(jrnkt(lrnk))
           write (unit=*, fmt=*) dres
           write (unit=*, fmt=*) ibcl, "seed ", jseet
           stop
         End If
      End Do
!
      end program chrono
