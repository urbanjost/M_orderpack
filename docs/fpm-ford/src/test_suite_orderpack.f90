!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_msg
use M_msg, only : str
use M_verify
use M_verify, only : unit_check_level
use M_verify,  only : unit_check_start, unit_check, unit_check_done, unit_check_good, unit_check_bad, unit_check_msg
use M_verify, only : unit_check_stop
! full ranking
use m_mrgref, only : mrgref
use m_mrgrnk, only : mrgrnk
! full sorting
use m_inssor, only : inssor
use m_refsor, only : refsor

use m_ctrper
use m_fndnth
use m_indmed
use m_indnth
use m_inspar
use m_median
use m_mulcnt
use m_rapknr
use m_refpar
use m_rinpar
use m_rnkpar
use m_uniinv
use m_unipar
use m_unirnk
use m_unista
use m_valmed
use m_valnth
implicit none
integer,parameter            :: dp=kind(0.0d0)
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_mrgref()
   call test_mrgrnk()
   call test_inssor()
   call test_refsor()

   call unit_check_stop()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mrgrnk()
integer,parameter             :: isz=1000000
real,allocatable              :: rr(:)
real(kind=dp),allocatable     :: dd(:)
character(len=10),allocatable :: jj(:)
integer,allocatable           :: ii(:)
integer,allocatable           :: indx(:)
integer                       :: i
logical                       :: gb

   call unit_check_start('mrgrnk', '-library orderpack') ! start tests
   if(allocated(indx))deallocate(indx)
   allocate(indx(isz))

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(RR)
   rr = rr*huge(0.0)
   gb=.true.
   call mrgrnk(rr,indx)
   do i=1,isz-1
      if(rr(indx(i)).gt.rr(indx(i+1)))then
         gb=.false.
      endif
   enddo
   call unit_check('mrgrnk',gb,'real test',isz,'values')
   deallocate(rr)

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(rr)
   if(allocated(ii))deallocate(ii)
   allocate(ii(isz))
   ii = FLOOR((huge(0)-1)*rr)  ! probably not distributed well because of number of bits
   deallocate(rr)
   gb=.true.
   call mrgrnk(ii,indx)
   do i=1,isz-1
      if(ii(indx(i)).gt.ii(indx(i+1)))then
         gb=.false.
      endif
   enddo
   call unit_check('mrgrnk',gb,'integer test',isz,'values')
   deallocate(ii)

   if(allocated(dd))deallocate(dd)
   allocate(dd(isz))
   CALL RANDOM_NUMBER(DD)
   dd = dd*huge(0.0_dp)
   gb=.true.
   call mrgrnk(dd,indx)
   do i=1,isz-1
      if(dd(indx(i)).gt.dd(indx(i+1)))then
         gb=.false.
      endif
   enddo
   call unit_check('mrgrnk',gb,'double test',isz,'values')
   deallocate(dd)

   !if(allocated(jj))deallocate(jj)
   !allocate(jj(isz))
   !jj = [(random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',len(jj)),i=1,isz)]
   !gb=.true.
   !call mrgrnk(jj,indx)
   !do i=1,isz-1
   !   if(jj(indx(i)).gt.jj(indx(i+1)))then
   !      gb=.false.
   !   endif
   !enddo
   !call unit_check('mrgrnk',gb,'character test')
   !deallocate(jj)
   
   call unit_check_done('mrgrnk',msg='test of mrgrnk(3f) completed') 

end subroutine test_mrgrnk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mrgref()
integer,parameter             :: isz=1000000
real,allocatable              :: rr(:)
real(kind=dp),allocatable     :: dd(:)
character(len=10),allocatable :: jj(:)
integer,allocatable           :: ii(:)
integer,allocatable           :: indx(:)
integer                       :: i
logical                       :: gb

   call unit_check_start('mrgref', '-library orderpack') ! start tests
   if(allocated(indx))deallocate(indx)
   allocate(indx(isz))

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(RR)
   rr = rr*huge(0.0)
   gb=.true.
   call mrgref(rr,indx)
   do i=1,isz-1
      if(rr(indx(i)).gt.rr(indx(i+1)))then
         gb=.false.
      endif
   enddo
   call unit_check('mrgref',gb,'real test',isz,'values')
   if(allocated(rr))deallocate(rr)

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(rr)
   if(allocated(ii))deallocate(ii)
   allocate(ii(isz))
   ii = FLOOR((huge(0)-1)*rr)  ! probably not distributed well because of number of bits
   deallocate(rr)
   gb=.true.
   call mrgref(ii,indx)
   do i=1,isz-1
      if(ii(indx(i)).gt.ii(indx(i+1)))then
         gb=.false.
      endif
   enddo
   call unit_check('mrgref',gb,'integer test',isz,'values')
   deallocate(ii)

   if(allocated(dd))deallocate(dd)
   allocate(dd(isz))
   CALL RANDOM_NUMBER(DD)
   dd = dd*huge(0.0_dp)
   gb=.true.
   call mrgref(dd,indx)
   do i=1,isz-1
      if(dd(indx(i)).gt.dd(indx(i+1)))then
         gb=.false.
      endif
   enddo
   call unit_check('mrgref',gb,'double test',isz,'values')
   if(allocated(dd))deallocate(dd)

   !if(allocated(jj))deallocate(jj)
   !allocate(jj(isz))
   !jj = [(random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',len(jj)),i=1,isz)]
   !gb=.true.
   !call mrgref(jj,indx)
   !do i=1,isz-1
   !   if(jj(indx(i)).gt.jj(indx(i+1)))then
   !      gb=.false.
   !   endif
   !enddo
   !call unit_check('mrgref',gb,'character test')
   !deallocate(jj)
   
   call unit_check_done('mrgref',msg='test of mrgref(3f) completed') 

end subroutine test_mrgref
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_refsor()
integer,parameter             :: isz=1000000
real,allocatable              :: rr(:)
real(kind=dp),allocatable     :: dd(:)
character(len=10),allocatable :: jj(:)
integer,allocatable           :: ii(:)
integer                       :: i
logical                       :: gb

   call unit_check_start('refsor', '-library orderpack') ! start tests

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(RR)
   rr = rr*huge(0.0)
   gb=.true.
   call refsor(rr)
   do i=1,isz-1
      if(rr(i).gt.rr(i+1))then
         gb=.false.
      endif
   enddo
   call unit_check('refsor',gb,'real test',isz,'values')
   deallocate(rr)

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(rr)
   if(allocated(ii))deallocate(ii)
   allocate(ii(isz))
   ii = FLOOR((huge(0)-1)*rr)  ! probably not distributed well because of number of bits
   deallocate(rr)
   gb=.true.
   call refsor(ii)
   do i=1,isz-1
      if(ii(i).gt.ii(i+1))then
         gb=.false.
      endif
   enddo
   call unit_check('refsor',gb,'integer test',isz,'values')
   deallocate(ii)

   if(allocated(dd))deallocate(dd)
   allocate(dd(isz))
   CALL RANDOM_NUMBER(DD)
   dd = dd*huge(0.0_dp)
   gb=.true.
   call refsor(dd)
   do i=1,isz-1
      if(dd(i).gt.dd(i+1))then
         gb=.false.
      endif
   enddo
   call unit_check('refsor',gb,'double test',isz,'values')
   deallocate(dd)

   !if(allocated(jj))deallocate(jj)
   !allocate(jj(isz))
   !jj = [(random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',len(jj)),i=1,isz)]
   !gb=.true.
   !call refsor(jj)
   !do i=1,isz-1
   !   if(jj(i).gt.jj(i+1))then
   !      gb=.false.
   !   endif
   !enddo
   !call unit_check('refsor',gb,'character test')
   !deallocate(jj)
   
   call unit_check_done('refsor',msg='test of refsor(3f) completed') 

end subroutine test_refsor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inssor()
integer,parameter             :: isz=10000
real,allocatable              :: rr(:)
real(kind=dp),allocatable     :: dd(:)
character(len=10),allocatable :: jj(:)
integer,allocatable           :: ii(:)
integer                       :: i
logical                       :: gb
   call unit_check_start('inssor', '-library orderpack') ! start tests

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(RR)
   rr = rr*huge(0.0)
   CALL RANDOM_NUMBER(RR)
   gb=.true.
   call inssor(rr)
   do i=1,isz-1
      if(rr(i).gt.rr(i+1))then
         gb=.false.
      endif
   enddo
   call unit_check('inssor',gb,'real test',isz,'values')
   deallocate(rr)

   if(allocated(rr))deallocate(rr)
   allocate(rr(isz))
   CALL RANDOM_NUMBER(rr)
   if(allocated(ii))deallocate(ii)
   allocate(ii(isz))
   ii = FLOOR((huge(0)-1)*rr)  ! probably not distributed well because of number of bits
   deallocate(rr)
   gb=.true.
   call inssor(ii)
   do i=1,isz-1
      if(ii(i).gt.ii(i+1))then
         gb=.false.
      endif
   enddo
   call unit_check('inssor',gb,'integer test',isz,'values')
   deallocate(ii)

   if(allocated(dd))deallocate(dd)
   allocate(dd(isz))
   CALL RANDOM_NUMBER(DD)
   dd = dd*huge(0.0_dp)
   gb=.true.
   call inssor(dd)
   do i=1,isz-1
      if(dd(i).gt.dd(i+1))then
         gb=.false.
      endif
   enddo
   call unit_check('inssor',gb,'double test',isz,'values')
   deallocate(dd)

   !if(allocated(jj))deallocate(jj)
   !allocate(jj(isz))
   !jj = [(random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',len(jj)),i=1,isz)]
   !gb=.true.
   !call inssor(jj)
   !do i=1,isz-1
   !   if(jj(i).gt.jj(i+1))then
   !      gb=.false.
   !   endif
   !enddo
   !call unit_check('inssor',gb,'character test')
   !deallocate(jj)
   
   call unit_check_done('inssor',msg='test of inssor(3f) completed') 

end subroutine test_inssor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
function random_string(chars,length) result(out)

!$@(#) M_random::random_string(3f): create random string composed of provided characters of specified length

character(len=*),intent(in)  :: chars
integer,intent(in)           :: length
character(len=:),allocatable :: out
real                         :: x
integer                      :: ilen   ! length of list of characters
integer                      :: which
integer                      :: i
   ilen=len(chars)
   out=''
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
