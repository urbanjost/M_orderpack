     program demo_rapknr
     ! create index to lowest N values in input array in decreasing order
     use M_rapknr, only : rapknr
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: xdont(:)
     integer,allocatable :: irngt(:)
     integer :: nord
     xdont=[10,5,7,1,4,5,6,8,9,10,1]
     nord=5
     allocate(irngt(nord))
        write(*,g)'ORIGINAL:',xdont
        call rapknr(xdont,irngt,nord)
        write(*,g)'NUMBER OF INDICES TO RETURN:',nord
        write(*,g)'RETURNED INDICES:',irngt
        write(*,g)nord,'MAXIMUM VALUES:',xdont(irngt(:nord))
     end program demo_rapknr
