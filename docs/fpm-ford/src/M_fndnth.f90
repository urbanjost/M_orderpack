Module M_fndnth
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
public :: fndnth
private :: real64_fndnth, real32_fndnth, int32_fndnth
interface fndnth
  module procedure real64_fndnth, real32_fndnth, int32_fndnth
end interface fndnth
contains
!>
!!##NAME
!!    fndnth(3f) - [orderpack:FRACTILE] Return Nth lowest value of an array, i.e. return fractile
!!                 of order NORD/SIZE(XDONT).
!!                 (LICENSE:CC0-1.0)
!!
!!##SYNOPSIS
!!
!!     Function fndnth (XDONT, NORD) Result (FNDNTH)
!!
!!      ${TYPE} (Kind=${KIND}), Dimension (:), Intent (In) :: XDONT
!!      Integer, Intent (In) :: NORD
!!      ${TYPE} (Kind=${KIND}) :: FNDNTH
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!    o Real(kind=real32)
!!    o Real(kind=real64)
!!    o Integer(kind=int32)
!!
!!##DESCRIPTION
!!    Return NORDth value of XDONT, i.e. fractile of order NORD/SIZE(XDONT).
!!
!!    This subroutine uses an insertion sort, limiting insertion to the
!!    first NORD values. It is faster when NORD is very small (2-5), and
!!    it requires only a work array of size NORD and type of XDONT, but
!!    worst case behavior can happen fairly probably (initially inverse
!!    sorted). In many cases, the refined quicksort method is faster.
!!
!!##OPTIONS
!!     XDONT     input array of values
!!     NORD      specify Nth value of sorted XDONT array to return, from
!!               1 to size(XDONT).
!!##RETURNS
!!     FNDNTH    returned value0
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_fndnth
!!    use M_fndnth, only : fndnth
!!    implicit none
!!    integer,allocatable :: iarr(:)
!!       iarr=[80,70,30,40,50,60,20,10]
!!       write(*,*)fndnth(iarr,3)
!!       write(*,*)fndnth(iarr,1)
!!       write(*,*)fndnth(iarr,7)
!!    end program demo_fndnth
!!
!!   Results:
!!
!!           30
!!           10
!!           70
!!
!!##AUTHOR
!!    Michel Olagnon - Aug. 2000
!!
!!     John Urban, 2022.04.16
!!         o added man-page and reduced to a template using the
!!           prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Function real64_fndnth (XDONT, NORD) Result (FNDNTH)
!!__________________________________________________________
      Real (Kind=real64), Dimension (:), Intent (In) :: XDONT
      Real (Kind=real64) :: FNDNTH
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Real (Kind=real64), Dimension (NORD) :: XWRKT
      Real (Kind=real64) :: XWRK, XWRK1
!
      Integer :: ICRS, IDCR, ILOW, NDON
!
      XWRKT (1) = XDONT (1)
      Do ICRS = 2, NORD
         XWRK = XDONT (ICRS)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK >= XWRKT(IDCR)) Exit
            XWRKT (IDCR+1) = XWRKT (IDCR)
         End Do
         XWRKT (IDCR+1) = XWRK
      End Do
!
      NDON = SIZE (XDONT)
      XWRK1 = XWRKT (NORD)
      ILOW = 2*NORD - NDON
      Do ICRS = NORD + 1, NDON
         If (XDONT(ICRS) < XWRK1) Then
            XWRK = XDONT (ICRS)
            Do IDCR = NORD - 1, MAX (1, ILOW) , - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
            XWRK1 = XWRKT(NORD)
         End If
         ILOW = ILOW + 1
      End Do
      FNDNTH = XWRK1
!
End Function real64_fndnth
Function real32_fndnth (XDONT, NORD) Result (FNDNTH)
!!__________________________________________________________
      Real (Kind=real32), Dimension (:), Intent (In) :: XDONT
      Real (Kind=real32) :: FNDNTH
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Real (Kind=real32), Dimension (NORD) :: XWRKT
      Real (Kind=real32) :: XWRK, XWRK1
!
      Integer :: ICRS, IDCR, ILOW, NDON
!
      XWRKT (1) = XDONT (1)
      Do ICRS = 2, NORD
         XWRK = XDONT (ICRS)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK >= XWRKT(IDCR)) Exit
            XWRKT (IDCR+1) = XWRKT (IDCR)
         End Do
         XWRKT (IDCR+1) = XWRK
      End Do
!
      NDON = SIZE (XDONT)
      XWRK1 = XWRKT (NORD)
      ILOW = 2*NORD - NDON
      Do ICRS = NORD + 1, NDON
         If (XDONT(ICRS) < XWRK1) Then
            XWRK = XDONT (ICRS)
            Do IDCR = NORD - 1, MAX (1, ILOW) , - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
            XWRK1 = XWRKT(NORD)
         End If
         ILOW = ILOW + 1
      End Do
      FNDNTH = XWRK1
!
End Function real32_fndnth
Function int32_fndnth (XDONT, NORD) Result (FNDNTH)
!!__________________________________________________________
      Integer (Kind=int32), Dimension (:), Intent (In) :: XDONT
      Integer (Kind=int32) :: FNDNTH
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Integer (Kind=int32), Dimension (NORD) :: XWRKT
      Integer (Kind=int32) :: XWRK, XWRK1
!
      Integer :: ICRS, IDCR, ILOW, NDON
!
      XWRKT (1) = XDONT (1)
      Do ICRS = 2, NORD
         XWRK = XDONT (ICRS)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK >= XWRKT(IDCR)) Exit
            XWRKT (IDCR+1) = XWRKT (IDCR)
         End Do
         XWRKT (IDCR+1) = XWRK
      End Do
!
      NDON = SIZE (XDONT)
      XWRK1 = XWRKT (NORD)
      ILOW = 2*NORD - NDON
      Do ICRS = NORD + 1, NDON
         If (XDONT(ICRS) < XWRK1) Then
            XWRK = XDONT (ICRS)
            Do IDCR = NORD - 1, MAX (1, ILOW) , - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
            XWRK1 = XWRKT(NORD)
         End If
         ILOW = ILOW + 1
      End Do
      FNDNTH = XWRK1
!
End Function int32_fndnth

end module M_fndnth