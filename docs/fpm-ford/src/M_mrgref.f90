Module M_mrgref
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
public :: mrgref
interface mrgref
  module procedure real64_mrgref, real32_mrgref, int32_mrgref
end interface mrgref
contains
!>
!!##NAME
!!    mrgref(3f) - [orderpack:RANK] Ranks array XVALT into index array IRNGT,
!!                 using basic merge-sort
!!
!!##SYNOPSIS
!!
!!     Subroutine ${KIND}_mrgref (XVALT, IRNGT)
!!
!!       ${TYPE} (kind=${KIND}), Dimension (:), Intent (In) :: XVALT
!!       Integer, Dimension (:), Intent (Out) :: IRNGT
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!
!!##DESCRIPTION
!!    Ranks array XVALT into index array IRNGT, using merge-sort
!!
!!    This version is not optimized for performance, and is thus
!!    not as difficult to read as some other ones.
!!
!!##OPTIONS
!!     XVALT      input array to rank
!!     IRNGT      returned rank array
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_mrgref
!!    use M_mrgref, only : mrgref
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer,parameter             :: dp=kind(0.0d0)
!!    integer,parameter             :: isz=10000
!!    real(kind=dp)                 :: dd(isz)
!!    real(kind=dp)                 :: pp
!!    integer                       :: indx(isz)
!!    integer                       :: i,j,k
!!       ! make some random numbers
!!       call random_seed()
!!       call random_number(dd)
!!       dd=dd-0.50_dp
!!       k=int(log(huge(0.0_dp))/log(2.0_dp))-1
!!       do i=1,isz
!!          call random_number(pp)
!!          j=floor((k+1)*pp)
!!          dd(i)=dd(i)*(2.0_dp**j)
!!       enddo
!!       ! rank data
!!       call mrgref(dd,indx)
!!       ! check order
!!       do i=1,isz-1
!!          if(dd(indx(i)).gt.dd(indx(i+1)))then
!!             write(*,g)'ERROR: data not sorted i=',i,'index=',indx(i), &
!!             & 'values ',dd(indx(i)),dd(indx(i+1))
!!             stop 1
!!          endif
!!       enddo
!!       ! sort data using rank values
!!       dd=dd(indx)
!!       write(*,g)'sorted ',isz,'values'
!!       write(*,g)'from',dd(1),'to',dd(isz)
!!       write(*,*)minval(dd).eq.dd(1)
!!       write(*,*)maxval(dd).eq.dd(isz)
!!       write(*,*)minloc(dd).eq.1
!!       write(*,*)maxloc(dd).eq.isz
!!    end program demo_mrgref
!!
!!   Results:
!!
!!##AUTHOR
!!     Michel Olagnon - April 2000
!!
!!     John Urban, 2022.04.16
!!         o added man-page and reduced to a template using the
!!           prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Subroutine real64_mrgref (XVALT, IRNGT)
!!__________________________________________________________
      Real (kind=real64), Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
!
      Integer, Dimension (:), Allocatable :: JWRKT
      Integer :: LMTNA, LMTNC
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      If (NVAL <= 0) Then
         Return
      End If
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo (NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      Allocate (JWRKT(1:NVAL))
      LMTNC = 2
      LMTNA = 2
!
!  Iteration. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
         IWRK = 0
!
!   Loop on merges of A and B into C
!
         Do
            IINDA = IWRKF
            IWRKD = IWRKF + 1
            IWRKF = IINDA + LMTNC
            JINDA = IINDA + LMTNA
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDB = JINDA
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B (no need to do anything)
!
            If (XVALT(IRNGT(JINDA)) <= XVALT(IRNGT(JINDA+1))) Then
               IWRK = IWRKF
               Cycle
            End If
!
!  One steps in the C subset, that we create in the final rank array
!
            Do
               If (IWRK >= IWRKF) Then
!
!  Make a copy of the rank array for next iteration
!
                  IRNGT (IWRKD:IWRKF) = JWRKT (IWRKD:IWRKF)
                  Exit
               End If
!
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (IINDA < JINDA) Then
                  If (IINDB < IWRKF) Then
                     If (XVALT(IRNGT(IINDA+1)) > XVALT(IRNGT(IINDB+1))) &
                    & Then
                        IINDB = IINDB + 1
                        JWRKT (IWRK) = IRNGT (IINDB)
                     Else
                        IINDA = IINDA + 1
                        JWRKT (IWRK) = IRNGT (IINDA)
                     End If
                  Else
!
!  Only A still with unprocessed values
!
                     IINDA = IINDA + 1
                     JWRKT (IWRK) = IRNGT (IINDA)
                  End If
               Else
!
!  Only B still with unprocessed values
!
                  IRNGT (IWRKD:IINDB) = JWRKT (IWRKD:IINDB)
                  IWRK = IWRKF
                  Exit
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!  Clean up
!
      Deallocate (JWRKT)
      Return
!
End Subroutine real64_mrgref
Subroutine real32_mrgref (XVALT, IRNGT)
!!__________________________________________________________
      Real (kind=real32), Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
!
      Integer, Dimension (:), Allocatable :: JWRKT
      Integer :: LMTNA, LMTNC
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      If (NVAL <= 0) Then
         Return
      End If
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo (NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      Allocate (JWRKT(1:NVAL))
      LMTNC = 2
      LMTNA = 2
!
!  Iteration. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
         IWRK = 0
!
!   Loop on merges of A and B into C
!
         Do
            IINDA = IWRKF
            IWRKD = IWRKF + 1
            IWRKF = IINDA + LMTNC
            JINDA = IINDA + LMTNA
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDB = JINDA
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B (no need to do anything)
!
            If (XVALT(IRNGT(JINDA)) <= XVALT(IRNGT(JINDA+1))) Then
               IWRK = IWRKF
               Cycle
            End If
!
!  One steps in the C subset, that we create in the final rank array
!
            Do
               If (IWRK >= IWRKF) Then
!
!  Make a copy of the rank array for next iteration
!
                  IRNGT (IWRKD:IWRKF) = JWRKT (IWRKD:IWRKF)
                  Exit
               End If
!
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (IINDA < JINDA) Then
                  If (IINDB < IWRKF) Then
                     If (XVALT(IRNGT(IINDA+1)) > XVALT(IRNGT(IINDB+1))) &
                    & Then
                        IINDB = IINDB + 1
                        JWRKT (IWRK) = IRNGT (IINDB)
                     Else
                        IINDA = IINDA + 1
                        JWRKT (IWRK) = IRNGT (IINDA)
                     End If
                  Else
!
!  Only A still with unprocessed values
!
                     IINDA = IINDA + 1
                     JWRKT (IWRK) = IRNGT (IINDA)
                  End If
               Else
!
!  Only B still with unprocessed values
!
                  IRNGT (IWRKD:IINDB) = JWRKT (IWRKD:IINDB)
                  IWRK = IWRKF
                  Exit
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!  Clean up
!
      Deallocate (JWRKT)
      Return
!
End Subroutine real32_mrgref
Subroutine int32_mrgref (XVALT, IRNGT)
!!__________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
!
      Integer, Dimension (:), Allocatable :: JWRKT
      Integer :: LMTNA, LMTNC
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      If (NVAL <= 0) Then
         Return
      End If
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo (NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      Allocate (JWRKT(1:NVAL))
      LMTNC = 2
      LMTNA = 2
!
!  Iteration. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
         IWRK = 0
!
!   Loop on merges of A and B into C
!
         Do
            IINDA = IWRKF
            IWRKD = IWRKF + 1
            IWRKF = IINDA + LMTNC
            JINDA = IINDA + LMTNA
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDB = JINDA
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B (no need to do anything)
!
            If (XVALT(IRNGT(JINDA)) <= XVALT(IRNGT(JINDA+1))) Then
               IWRK = IWRKF
               Cycle
            End If
!
!  One steps in the C subset, that we create in the final rank array
!
            Do
               If (IWRK >= IWRKF) Then
!
!  Make a copy of the rank array for next iteration
!
                  IRNGT (IWRKD:IWRKF) = JWRKT (IWRKD:IWRKF)
                  Exit
               End If
!
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (IINDA < JINDA) Then
                  If (IINDB < IWRKF) Then
                     If (XVALT(IRNGT(IINDA+1)) > XVALT(IRNGT(IINDB+1))) &
                    & Then
                        IINDB = IINDB + 1
                        JWRKT (IWRK) = IRNGT (IINDB)
                     Else
                        IINDA = IINDA + 1
                        JWRKT (IWRK) = IRNGT (IINDA)
                     End If
                  Else
!
!  Only A still with unprocessed values
!
                     IINDA = IINDA + 1
                     JWRKT (IWRK) = IRNGT (IINDA)
                  End If
               Else
!
!  Only B still with unprocessed values
!
                  IRNGT (IWRKD:IINDB) = JWRKT (IWRKD:IINDB)
                  IWRK = IWRKF
                  Exit
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!  Clean up
!
      Deallocate (JWRKT)
      Return
!
End Subroutine int32_mrgref
end module M_mrgref
