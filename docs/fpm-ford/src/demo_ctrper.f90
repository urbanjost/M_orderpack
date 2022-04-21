     program demo_ctrper
     ! generate a random perturbation of an array
     use M_ctrper, only : ctrper
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: list= '(*(g0:,", "))'
     integer,allocatable :: xdont(:)
     integer,allocatable :: xout(:,:)
     integer          :: isz, i, j
     isz=200
        ! randomly pertube location of values
        !
        ! make an array with three identical rows
        if(allocated(xout))deallocate(xout)
        allocate(xout(3,isz))
        xdont=[(i,i=isz,1,-1)]*10
        xout(1,:)=xdont
        xout(2,:)=xdont
        xout(3,:)=xdont
        ! pertube each row a different amount
        call ctrper(xout(1,:),0.0)
        call ctrper(xout(2,:),0.1)
        call ctrper(xout(3,:),1.0)
        ! show values
        write(*,'(a)')'count    unchanged  perturbed  random'
        do i=1,size(xdont)
           write(*,'(*(i8,1x))')i,xout(:,i)
        enddo
     char: block
     character(len=:),allocatable :: cdont(:)
        cdont=[character(len=20) :: 'a', 'be', 'car', 'dam','fan','gas','egg']
        isz=size(cdont)
        write(*,g)'Original.................:',(trim(cdont(i)),i=1,isz)
        call ctrper(cdont,1.0)
        write(*,g)'Perturbed ...............:',(trim(cdont(i)),i=1,isz)
        write(*,g)
     endblock char

     end program demo_ctrper
