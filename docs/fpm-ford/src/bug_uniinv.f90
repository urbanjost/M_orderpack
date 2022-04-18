   program demo_uniinv
   use M_uniinv, only : uniinv
   implicit none
   character(len=*),parameter :: g='(*(g0,1x))'
   integer,allocatable :: xdont(:)
      !
      write(*,g)'If enough values are unique, will return NORD indices'
      xdont=[10,5,7,1,4,5,6,8,9,10,1]
      call printme()
      ! 
      write(*,g)'If not enough values are unique '
      xdont=[-1,0,-1,0,-1,0,-1]
      call printme()
   contains
   subroutine printme()
   integer,allocatable :: igoest(:)
      write(*,g)'ORIGINAL:',xdont
      write(*,g)'NUMBER OF INDICES TO SORT:',size(xdont)
      if(allocated(igoest))deallocate(igoest)
      allocate(igoest(size(xdont)))
      call uniinv(xdont,igoest)
      write(*,g)'NUMBER OF INDICES RETURNED: ????'
      write(*,g)'RETURNED INDICES:',igoest(:)
      write(*,g)'SORTED VALUES:?????',xdont(igoest)
   end subroutine
   end program demo_uniinv
