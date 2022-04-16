program test_mrgref
use M_mrgref, only : mrgref
implicit none
integer,parameter            :: dp=kind(0.0)
integer,parameter             :: isz=1000000
real(kind=dp),allocatable     :: dd(:)
real(kind=dp)                 :: pp
integer,allocatable           :: indx(:)
integer                       :: i,j,k,m
   ! set up storage
   if(allocated(indx))deallocate(indx)
   allocate(indx(isz))
   if(allocated(dd))deallocate(dd)
   allocate(dd(isz))
   ! make some random numbers
   call random_seed()
   call random_number(dd)
   dd=dd-0.50_dp
   k=int(log(huge(0.0_dp))/log(2.0_dp))-1
   do i=1,isz
      call random_number(pp)
      j=floor((k+1)*pp)
      dd(i)=dd(i)*(2.0_dp**j)
   enddo
   ! sort data
   call mrgref(dd,indx)
   m=0
   do i=1,isz-1
      if(dd(indx(i)).gt.dd(indx(i+1)))then
         write(*,*)'ERROR: data not sorted i=',i,'index=',indx(i), &
         & 'values ',dd(indx(i)),dd(indx(i+1))
         m=m+1
      endif
   enddo
   do i=1,isz
      write(*,*)i,indx(i),dd(indx(i))
   enddo
   write(*,*)'lowest                  ',dd(indx(1)),minval(dd)
   write(*,*)'highest                 ',dd(indx(size(indx))),maxval(dd)
   write(*,*)'smallest absolute value ',minval(abs(dd))
   write(*,*)'huge                    ',huge(0.0_dp)
   write(*,*)'tiny                    ',tiny(0.0_dp)
   if(m.eq.0)then
      write(*,*)'sort passed'
   else
      write(*,*)'sort failed, bad=',m
   endif

end program test_mrgref
