     program demo_fndnth
     use M_fndnth, only : fndnth
     implicit none
     character(len=*),parameter :: sp='(*(g0,1x))'
     integer,allocatable :: iarr(:)
     integer :: imiddle
        iarr=[80,70,30,40,50,60,20,10]
        print sp, 'ORIGINAL:',iarr
        ! can return the same values as intrinsics minval() and maxval()
        print sp, 'minval',fndnth(iarr,1),          minval(iarr)
        print sp, 'maxval',fndnth(iarr,size(iarr)), maxval(iarr)
        ! but more generally it can return the Nth lowest value.
        print sp,'nord=',4, ' fractile=',fndnth(iarr,4)
        ! so a value at the middle would be
        imiddle=(size(iarr)+1)/2
        print sp,'median=',fndnth(iarr,imiddle)
     end program demo_fndnth
