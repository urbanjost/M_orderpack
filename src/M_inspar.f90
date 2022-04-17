Module M_inspar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
public :: inspar
private :: real64_inspar, real32_inspar, int32_inspar
interface inspar
  module procedure real64_inspar, real32_inspar, int32_inspar
end interface inspar
contains
!>
!!##NAME
!!    inspar(3f) - [orderpack] partially sorts an array, bringing th N
!!                 lowest values to the beginning of the array
!!                 (LICENSE:CC0-1.0)
!!
!!##SYNOPSIS
!!
!!     Subroutine inspar (yyyyyy)
!!
!!      ${TYPE} (kind=${KIND}), Intent (InOut) :: yyyyyy(:)
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!
!!##DESCRIPTION
!!    INSPAR partially sorts XDONT, bringing the NORD lowest values to the
!!    beginning of the array.
!!
!!    This subroutine uses insertion sort, limiting insertion to the first
!!    NORD values. It does not use any work array and is faster when NORD is
!!    very small (2-5), but worst case behavior can happen fairly probably
!!    (initially inverse sorted) In many cases, the refined quicksort method
!!    is faster.
!!
!!##OPTIONS
!!     XXXXX      description
!!     YYYYY      description
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_inspar
!!    use M_inspar, only : inspar
!!    implicit none
!!       !x!call inspar(yyyyyy)
!!    end program demo_inspar
!!
!!   Results:
!!
!!##AUTHOR
!!     Michel Olagnon - Feb. 2000
!!
!!     John Urban, 2022.04.16
!!     o added man-page and reduced to a template using the
!!       prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Subroutine real64_inspar (XDONT, NORD)
Real (kind=real64), Dimension (:), Intent (InOut) :: XDONT
Integer, Intent (In) :: NORD
! __________________________________________________________
Real (kind=real64) :: XWRK, XWRK1
Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = XDONT (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= XDONT(IDCR)) Exit
         XDONT (IDCR+1) = XDONT (IDCR)
      End Do
      XDONT (IDCR+1) = XWRK
   End Do
!
   XWRK1 = XDONT (NORD)
   Do ICRS = NORD + 1, SIZE (XDONT)
      If (XDONT(ICRS) < XWRK1) Then
         XWRK = XDONT (ICRS)
         XDONT (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
         XWRK1 = XDONT (NORD)
      End If
   End Do
!
End Subroutine real64_inspar
Subroutine real32_inspar (XDONT, NORD)
Real (kind=real32), Dimension (:), Intent (InOut) :: XDONT
Integer, Intent (In) :: NORD
! __________________________________________________________
Real (kind=real32) :: XWRK, XWRK1
Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = XDONT (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= XDONT(IDCR)) Exit
         XDONT (IDCR+1) = XDONT (IDCR)
      End Do
      XDONT (IDCR+1) = XWRK
   End Do
!
   XWRK1 = XDONT (NORD)
   Do ICRS = NORD + 1, SIZE (XDONT)
      If (XDONT(ICRS) < XWRK1) Then
         XWRK = XDONT (ICRS)
         XDONT (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
         XWRK1 = XDONT (NORD)
      End If
   End Do
!
End Subroutine real32_inspar
Subroutine int32_inspar (XDONT, NORD)
Integer (kind=int32), Dimension (:), Intent (InOut) :: XDONT
Integer, Intent (In) :: NORD
! __________________________________________________________
Integer (kind=int32) :: XWRK, XWRK1
Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = XDONT (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= XDONT(IDCR)) Exit
         XDONT (IDCR+1) = XDONT (IDCR)
      End Do
      XDONT (IDCR+1) = XWRK
   End Do
!
   XWRK1 = XDONT (NORD)
   Do ICRS = NORD + 1, SIZE (XDONT)
      If (XDONT(ICRS) < XWRK1) Then
         XWRK = XDONT (ICRS)
         XDONT (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
         XWRK1 = XDONT (NORD)
      End If
   End Do
!
End Subroutine int32_inspar
end module M_inspar
