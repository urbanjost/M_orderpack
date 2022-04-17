Module M_mulcnt
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
Private
public :: mulcnt
private :: real64_mulcnt, real32_mulcnt, int32_mulcnt
interface mulcnt
  module procedure real64_mulcnt, real32_mulcnt, int32_mulcnt
end interface mulcnt
contains
!>
!!##NAME
!!    mulcnt(3f) - [orderpack:MULTIPLICITY] Give the multiplicity for each array value
!!                 (number of times that it appears in the array)
!!                 (LICENSE:CC0-1.0)
!!
!!##SYNOPSIS
!!
!!     Subroutine mulcnt (XDONT, IMULT)
!!
!!       ${TYPE} (kind=${KIND}), Intent (In) :: XDONT(:)
!!       Integer, Dimension (:), Intent (Out) :: IMULT
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
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
!!   Sample program:
!!
!!    program demo_mulcnt
!!    use M_mulcnt, only : mulcnt
!!    implicit none
!!    real,parameter :: xdont(*)=[1,2,3,4,5,6,7,4,5,6,6,2]
!!    integer, dimension(size(xdont)) :: imult
!!       call mulcnt(xdont,imult)
!!       write(*,*)xdont
!!       write(*,*)imult
!!    end program demo_mulcnt
!!
!!   Results:
!!
!!     1.00 2.00 3.00 4.00 5.00 6.00 7.00 4.00 5.00 6.00 6.00 2.00
!!     1 2 1 2 2 3 1 2 2 3 3 2
!!
!!##AUTHOR
!!     Michel Olagnon, Mar 2000
!!
!!     John Urban, 2022.04.16
!!         o added man-page and reduced to a template using the
!!           prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Subroutine real64_mulcnt (XDONT, IMULT)
!j__________________________________________________________
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
!j__________________________________________________________
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
!j__________________________________________________________
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
end module M_mulcnt
