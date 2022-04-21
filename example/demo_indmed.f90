     program demo_indmed
     ! return index of median value
     use M_indmed, only : indmed
     implicit none
     real,allocatable :: xdont(:)
     character(len=:),allocatable :: cdont(:)
     integer :: ii
        xdont=[80.0,70.0,20.0,10.0,1000.0]
        call indmed(xdont,ii)
        write(*,*) ii,xdont(ii)
        !
        xdont=[11, 22, 33, 44, 55, 66, 77, 88]
        call indmed(xdont,ii)
        write(*,*) ii,xdont(ii)
        !
        xdont=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]
        call indmed(xdont,ii)
        write(*,*) ii,xdont(ii)
        !
        cdont=[character(len=20) :: 'apple','bee','cherry','duck',&
                'elephant','finger','goose','h','insect','j']
        call indmed(cdont,ii)
        write(*,*) ii,cdont(ii)
        !
     end program demo_indmed
