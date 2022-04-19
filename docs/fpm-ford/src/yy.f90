   program demo_mrgref
   use M_mrgref, only : mrgref
   character(len=:),allocatable :: array(:)
   integer,allocatable :: indx(:)
 
   array= [ character(len=20) ::                               &
   & 'red',    'green', 'blue', 'yellow', 'orange',   'black', &
   & 'white',  'brown', 'gray', 'cyan',   'magenta',           &
   & 'purple']
   if(allocated(indx))deallocate(indx);allocate(indx(size(array)))
 
   write(*,'(a,*(a:,","))')'BEFORE ',(trim(array(i)),i=1,size(array))

   call mrgref(array,indx)

   write(*,'(a,*(a:,","))')'A-Z    ',(trim(array(indx(i))),i=1,size(array))

   array=array(indx)

   do i=1,size(array)-1
      if(array(i).gt.array(i+1))then
         write(*,*)'Error in sorting strings a-z'
      endif
   enddo
 
   end program demo_mrgref
