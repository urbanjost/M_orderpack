     program demo_valnth
     use M_valnth, only : valnth
     implicit none
     character(len=*),parameter :: list= '(*(g0:,", "))'
     character(len=*),parameter :: sp='(*(g0,1x))'
     real,parameter ::  xdont(*)=[1.1,20.20,3.3,10.10,5.5,4.4,2.2]
     integer :: i
     integer :: imiddle
        write(*,list) 'ORIGINAL:',xdont
        ! can return the same values as intrinsics minval() and maxval()
        print sp, 'minval',valnth(xdont,1),          minval(xdont)
        print sp, 'maxval',valnth(xdont,size(xdont)), maxval(xdont)
        ! but more generally it can return the Nth lowest value.
        print sp,'nord=',4, ' fractile=',valnth(xdont,4)
        ! so a value at the middle would be
        imiddle=(size(xdont)+1)/2
        print sp,'median=',valnth(xdont,imiddle)
        ! sorting the hard way
        do i=1,size(xdont)
           write(*,list)i,valnth(xdont,i)
        enddo
     end program demo_valnth
