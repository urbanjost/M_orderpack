     program demo_uniinv
     ! rank input array ranking duplicates the same
     use M_uniinv, only : uniinv
     implicit none
     character(len=*),parameter :: fmt='(a,*(g3.3,1x))'
     integer,allocatable,dimension(:) :: xdont, igoest, distinct, count
     integer :: imx, i
        ! create an input array
        xdont=[11, 11, 22, 11, 33, 33, 22, 33, 33]
        ! make an index array of the same size
        if(allocated(igoest))deallocate(igoest)
        allocate(igoest(size(xdont)))
        print fmt, 'Original:                 ',xdont
        print fmt, 'Number of indices to sort:',size(xdont)
        ! rank input array ranking duplicates the same
        call uniinv(xdont,igoest)
        print fmt, 'Returned Indices:         ',igoest(:)
        !
        ! interrogate the results
        !
        imx=maxval(igoest)
        print fmt, 'Number of unique indices :',imx
        ! squeeze it down to just IMX unique values
        count=[(0,i=1,imx)] ! count how many times a value occurs
        distinct=count      ! array to set of unique values
        do i=1,size(xdont)
           distinct(igoest(i))=xdont(i)
           count(igoest(i))= count(igoest(i))+1
        enddo
        print fmt, 'Sorted unique values:     ',distinct
        print fmt, 'count of occurrences:     ',count
     end program demo_uniinv
