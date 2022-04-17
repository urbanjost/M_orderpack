     program demo_mrgref
     use M_mrgref, only : mrgref
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,parameter             :: dp=kind(0.0d0)
     integer,parameter             :: isz=10000
     real(kind=dp)                 :: dd(isz)
     real(kind=dp)                 :: pp
     integer                       :: indx(isz)
     integer                       :: i,j,k
        ! make some random numbers
        call random_seed()
        call random_number(dd)
        dd=dd-0.50_dp
        k=int(log(huge(0.0_dp))/log(2.0_dp))-1
        do i=1,isz
           call random_number(pp)
           j=floor((k+1)*pp)
           dd(i)=dd(i)*(2.0_dp**j)
        enddo
        ! rank data
        call mrgref(dd,indx)
        ! check order
        do i=1,isz-1
           if(dd(indx(i)).gt.dd(indx(i+1)))then
              write(*,g)'ERROR: data not sorted i=',i,'index=',indx(i), &
              & 'values ',dd(indx(i)),dd(indx(i+1))
              stop 1
           endif
        enddo
        ! sort data using rank values
        dd=dd(indx)
        write(*,g)'sorted ',isz,'values'
        write(*,g)'from',dd(1),'to',dd(isz)
        write(*,*)minval(dd).eq.dd(1)
        write(*,*)maxval(dd).eq.dd(isz)
        write(*,*)minloc(dd).eq.1
        write(*,*)maxloc(dd).eq.isz
     end program demo_mrgref
