     program demo_indnth
     ! find Nth lowest ordered value in an array without sorting entire array
     use M_indnth, only : indnth
     use M_indmed, only : indmed
     implicit none
     integer,allocatable :: iarr(:)
     character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
     integer :: i
     integer :: indx
        iarr=[80,70,30,40,50,60,20,10,0,-100]
        print list, 'ORIGINAL:',iarr
        ! like minloc() and maxloc()
        print sp,'minloc',indnth(iarr,1),                minloc(iarr)
        print sp,'maxloc',indnth(iarr,size(iarr)),       maxloc(iarr)
        ! can find median
        call indmed(iarr,indx)
        print sp,'median',indnth(iarr,(size(iarr)+1)/2), indx
        ! but more general so can find location of the Nth lowest value ...
        !
        ! sort the hard way, finding location of Nth value one at a time
        do i=1,size(iarr)
           write(*,sp,advance='no') iarr(indnth(iarr,i))
        enddo
        print *
     contains
     subroutine printme(n)
     integer,intent(in) :: n
     integer :: ii
        ii=indnth(iarr,n)
        print sp,'nord=',n,' index=',ii,' fractile=',iarr(ii)
     end subroutine printme
     end program demo_indnth
