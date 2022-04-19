!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_msg
use M_msg, only : str
use M_verify
use M_verify, only : unit_check_level
use M_verify, only : unit_check_start, unit_check, unit_check_done, unit_check_good, unit_check_bad, unit_check_msg
use M_verify, only : unit_check_stop
! full ranking
use M_mrgref, only : mrgref
use M_mrgrnk, only : mrgrnk
! full sorting
use M_inssor, only : inssor
use M_refsor, only : refsor

use M_ctrper
use M_fndnth
use M_indmed
use M_indnth
use M_inspar
use M_median
use M_mulcnt
use M_rapknr
use M_refpar
use M_rinpar
use M_rnkpar
use M_uniinv
use M_unipar
use M_unirnk
use M_unista
use M_valmed
use M_valnth
implicit none
integer,parameter            :: dp=kind(0.0d0)
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_gen('mrgref')
   call test_gen('mrgrnk')
   call test_gen('inssor')
   call test_gen('refsor')

   call unit_check_stop()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gen(name)
character(len=*),intent(in)   :: name
integer,parameter             :: isz=10000
real                          :: rr(isz)
real(kind=dp)                 :: dd(isz)
integer                       :: ii(isz)
character(len=10)             :: cc(isz)
integer                       :: indx(isz)
integer                       :: i

   call unit_check_start(name, '-library orderpack') ! start tests

   CALL RANDOM_NUMBER(RR)
   rr = rr*huge(0.0)
   select case(name)
   case('inssor');call inssor(rr)
   case('refsor');call refsor(rr)
   case('mrgrnk');call mrgrnk(rr,indx); rr=rr(indx)
   case('mrgref');call mrgref(rr,indx); rr=rr(indx)
   endselect
   call unit_check(name,all(rr(1:isz-1) .le. rr(2:isz)),'real test',isz,'values') 

   CALL RANDOM_NUMBER(RR)
   ii = rr*huge(0)
   select case(name)
   case('inssor');call inssor(ii)
   case('refsor');call refsor(ii)
   case('mrgrnk');call mrgrnk(ii,indx); ii=ii(indx)
   case('mrgref');call mrgref(ii,indx); ii=ii(indx)
   endselect
   call unit_check(name,all(ii(1:isz-1) .le. ii(2:isz)),'integer test',isz,'values') 

   CALL RANDOM_NUMBER(DD)
   dd = dd*huge(0.0_dp)
   select case(name)
   case('inssor');call inssor(dd)
   case('refsor');call refsor(dd)
   case('mrgrnk');call mrgrnk(dd,indx); dd=dd(indx)
   case('mrgref');call mrgref(dd,indx); dd=dd(indx)
   endselect
   call unit_check(name,all(dd(1:isz-1) .le. dd(2:isz)),'double test',isz,'values') 

   do i=1,isz
      cc(i) = random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',10)
   enddo
   select case(name)
   case('inssor');call inssor(cc)
   case('refsor');call refsor(cc)
   case('mrgrnk');call mrgrnk(cc,indx); cc=cc(indx)
   case('mrgref');call mrgref(cc,indx); cc=cc(indx)
   endselect
   call unit_check(name,all(cc(1:isz-1) .le. cc(2:isz)),'string test, random',isz,'values') 
   
   call unit_check_done(name,msg='test completed') 

end subroutine test_gen
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
