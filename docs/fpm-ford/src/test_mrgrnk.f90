program test_mrgrnk
! depending on the compiler and options you might have to unlimit stacksize
! to avoid segmentation faults when using large arrays, or specify on 
! compiler to put arrays on heap.
!
!    ulimit -s unlimited # bash shell
use M_mrgrnk, only : mrgrnk
implicit none
integer,parameter            :: dp=kind(0.0d0)
integer,parameter            :: isz=10000000
real(kind=dp),allocatable    :: dd(:)
real(kind=dp)                :: pp
integer,allocatable          :: indx(:)
integer                      :: i,j,k,m
real(kind=dp) :: start, finish
   !
   ! set up storage
   !
   if(allocated(indx))deallocate(indx)
   allocate(indx(isz))
   if(allocated(dd))deallocate(dd)
   allocate(dd(isz))
   !
   ! make some random numbers
   !
   call random_seed()
   call random_number(dd)
   dd=dd-0.50_dp
   k=int(log(huge(0.0_dp))/log(2.0_dp))-1
   do i=1,isz
      call random_number(pp)
      j=floor((k+1)*pp)
      dd(i)=dd(i)*(2.0_dp**j)
   enddo
   !do i=1,isz
   !   write(*,*)i,indx(i),dd(indx(i))
   !enddo
   write(*,*)'for MRGRNK(3f):         '
   write(*,*)'number of values to sort',isz
   ! sort data
   call cpu_time(start)
   call mrgrnk(dd,indx)
   call cpu_time(finish)
   write(*,*)"Processor Time (random)=              ",finish-start," seconds."
   !
   ! do some checks
   !
   m=0
   do i=1,isz-1
      if(dd(indx(i)).gt.dd(indx(i+1)))then
         write(*,*)'ERROR: data not sorted i=',i,'indx=',indx(i), &
         & 'values ',dd(indx(i)),dd(indx(i+1))
         m=m+1
      endif
   enddo
   ! time if already sorted
   dd=dd(indx)
   call cpu_time(start)
   call mrgrnk(dd,indx)
   call cpu_time(finish)
   write(*,*)"Processor Time (already sorted)=      ",finish-start," seconds."
   ! time if initially reverse sorted
   dd=dd(isz:1:-1)
   call cpu_time(start)
   call mrgrnk(dd,indx)
   call cpu_time(finish)
   write(*,*)"Processor Time (input reverse sorted)=",finish-start," seconds."
   write(*,*)'lowest                  ',dd(indx(1)),minval(dd),&
                                      & dd(indx(1)).eq.minval(dd)
   write(*,*)'highest                 ',dd(indx(size(indx))),maxval(dd),&
                                      & dd(indx(size(indx))).eq.maxval(dd)
   write(*,*)'smallest absolute value ',minval(abs(dd))
   write(*,*)'for reference huge is   ',huge(0.0_dp)
   write(*,*)'              tiny is   ',tiny(0.0_dp)
   write(*,*)'              epsilon is',epsilon(0.0_dp)
   if(m.eq.0)then
      write(*,*)'sort passed'
   else
      write(*,*)'sort failed, bad=',m
   endif

end program test_mrgrnk
