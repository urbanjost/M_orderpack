     program demo_fndnth
     ! return Nth ordered value of an array
     use M_fndnth, only : fndnth
     use M_valmed, only : valmed
     implicit none
     character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
     integer,allocatable :: iarr(:)
     integer :: i
        iarr=[80,70,30,40,-50,60,20,10]
        print sp, 'ORIGINAL:',iarr
        ! can return the same values as intrinsics minval() and maxval()
        print sp, 'minval',fndnth(iarr,1),          minval(iarr)
        print sp, 'maxval',fndnth(iarr,size(iarr)), maxval(iarr)
        ! but more generally it can return the Nth lowest value.
        print sp, 'median',fndnth(iarr,(size(iarr+1))/2), valmed(iarr)
        ! so only Nth ordered value can be found
        print sp,'nord=',3, ' fractile=',fndnth(iarr,3)
        ! sorting the hard way
        print sp, 'ORIGINAL:',iarr
        do i=1,size(iarr)
           write(*,list)i,fndnth(iarr,i)
        enddo
        print *
     end program demo_fndnth
