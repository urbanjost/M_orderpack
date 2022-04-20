     program demo_ctrper
     use M_ctrper, only : ctrper
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: list= '(*(g0:,", "))'
     integer,allocatable :: xdont(:)
     integer,allocatable :: xout(:,:)
     integer          :: isz, i, j
     isz=200
        if(allocated(xout))deallocate(xout)
        allocate(xout(3,isz))

        xdont=[(i,i=1,isz)]*10
        call ctrper(xdont,0.0)
        xout(1,:)=xdont

        xdont=[(i,i=1,isz)]*10
        call ctrper(xdont,0.1)
        xout(2,:)=xdont

        xdont=[(i,i=1,isz)]*10
        call ctrper(xdont,1.0)
        xout(3,:)=xdont

        write(*,'(a)')'count    unchanged  perturbed  random'
        do i=1,size(xdont)
           write(*,'(*(i8,1x))')i,xout(:,i)
        enddo

     char: block
      character(len=:),allocatable :: xdont(:)
      xdont=[character(len=20) :: 'a','be','car','dam','fan','gas','egg']
      isz=size(xdont)
      write(*,g)'Original.................:',(trim(xdont(i)),i=1,isz)
      call ctrper(xdont,1.0)
      write(*,g)'Perturbed ...............:',(trim(xdont(i)),i=1,isz)
      write(*,g)
     endblock char

     end program demo_ctrper
