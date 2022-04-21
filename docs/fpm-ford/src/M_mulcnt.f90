Module M_mulcnt
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: mulcnt
interface mulcnt
  module procedure real64_mulcnt, real32_mulcnt, int32_mulcnt, f_char_mulcnt
end interface mulcnt
contains
!>
!!##NAME
!!    mulcnt(3f) - [orderpack:MULTIPLICITY] Give the multiplicity for each
!!                 array value (number of times that it appears in the array)
!!
!!##SYNOPSIS
!!
!!     Subroutine mulcnt (XDONT, IMULT)
!!
!!       ${TYPE} (kind=${KIND}), Intent (In) :: XDONT(:)
!!       Integer, Intent (Out)               :: IMULT(:)
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!     MULCNT = Give for each array value its multiplicity
!!              (number of times that it appears in the array)
!!
!!##OPTIONS
!!     XDONT      input array
!!     IMULT      array containing how often the value in XDONT
!!                appears in XDONT
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!      program demo_mulcnt
!!      use M_mulcnt, only : mulcnt
!!      ! determine how many times each value appears in an input array
!!      implicit none
!!      character(len=*),parameter    :: g='(*(g0,1x))'
!!      character(len=20),allocatable :: strings(:)
!!      integer,allocatable           :: cindx(:)
!!      integer                       :: csz
!!      integer                       :: i
!!         ! each name appears the number of times its name represents
!!         strings= [ character(len=20) ::                           &
!!         & 'two  ',  'four ', 'three', 'five',   'five',           &
!!         & 'two  ',  'four ', 'three', 'five',   'five',           &
!!         & 'four ',  'four ', 'three', 'one  ',  'five']
!!         csz=size(strings)
!!         if(allocated(cindx))deallocate(cindx)
!!         allocate(cindx(csz))
!!         call mulcnt(strings,cindx)
!!         write(*,g)(trim(strings(i)),i=1,csz)
!!         write(*,g)cindx
!!      end program demo_mulcnt
!!
!! Results:
!!
!!  two four three five five two four three five five four four three one five
!!  2   4    3     5    5    2   4    3     5    5    4    4    3     1   5
!!
!!##AUTHOR
!!     Michel Olagnon, Mar 2000
!!
!!     John Urban, 2022.04.16
!!     o added man-page and reduced to a template using the
!!       prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Subroutine real64_mulcnt (XDONT, IMULT)
! __________________________________________________________
      Real (kind=real64), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Integer, Dimension (Size(XDONT)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (XDONT, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(XDONT)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(XDONT)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine real64_mulcnt
Subroutine real32_mulcnt (XDONT, IMULT)
! __________________________________________________________
      Real (kind=real32), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Integer, Dimension (Size(XDONT)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (XDONT, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(XDONT)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(XDONT)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine real32_mulcnt
Subroutine int32_mulcnt (XDONT, IMULT)
! __________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Integer, Dimension (Size(XDONT)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (XDONT, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(XDONT)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(XDONT)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine int32_mulcnt
Subroutine f_char_mulcnt (XDONT, IMULT)
! __________________________________________________________
      character (kind=f_char,len=*), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Integer, Dimension (Size(XDONT)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (XDONT, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(XDONT)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(XDONT)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine f_char_mulcnt
end module M_mulcnt
