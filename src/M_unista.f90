Module M_unista
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
Private
public :: unista
private :: real64_unista, real32_unista, int32_unista
interface unista
  module procedure real64_unista, real32_unista, int32_unista
end interface unista
contains
!>
!!##NAME
!!    unista(3f) - [orderpack] (Stable unique) Removes duplicates from an
!!                 array in original order
!!                 (LICENSE:CC0-1.0)
!!
!!##SYNOPSIS
!!
!!     Subroutine unista (XDONT, NUNI)
!!
!!      ${TYPE} (kind=${KIND}), Dimension (:), Intent (InOut) :: XDONT
!!      Integer, Intent (Out) :: NUNI
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!
!!##DESCRIPTION
!!    UNISTA ("Stable Unique") removes duplicates from an array, leaving
!!    unique entries in the order of their first appearance in the initial
!!    set.
!!
!!##OPTIONS
!!     XDONT   input array to reduce to unique values
!!     NUNI    number of values comprising the returned set of unique
!!             values
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unista
!!    use M_unista, only : unista
!!    implicit none
!!    character(len=*),parameter :: list= '(*(g0:,","))'
!!    real,allocatable :: xdont(:)
!!    integer :: nuni
!!       xdont=[4.4,3.3,3.3,3.3,2.2,1.1,3.3,4.4,5.5,3.3]
!!       print list,'ORIGINAL:',xdont
!!       print
!!       call unista(xdont,nuni)
!!       xdont=xdont(:nuni)
!!       print list,'UNIQUE:',xdont
!!    end program demo_unista
!!
!!   Results:
!!     ORIGINAL:,4.400000,3.300000,3.300000,3.300000,2.200000,
!!     1.100000,3.300000,4.400000,5.500000,3.300000
!!
!!     UNIQUE:,4.400000,3.300000,2.200000,1.100000,5.500000
!!
!!##AUTHOR
!!   Michel Olagnon - Feb. 2000
!!
!!   John Urban, 2022.04.16
!!   o added man-page and reduced to a template using the
!!     prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Subroutine real64_unista (XDONT, NUNI)

Real (kind=real64), Dimension (:), Intent (InOut) :: XDONT
Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Logical, Dimension (Size(XDONT)) :: IFMPTYT
      Integer :: ICRS
! __________________________________________________________
   Call UNIINV (XDONT, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(XDONT)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         XDONT (NUNI) = XDONT (ICRS)
      End If
   End Do
!
End Subroutine real64_unista
Subroutine real32_unista (XDONT, NUNI)

Real (kind=real32), Dimension (:), Intent (InOut) :: XDONT
Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Logical, Dimension (Size(XDONT)) :: IFMPTYT
      Integer :: ICRS
! __________________________________________________________
   Call UNIINV (XDONT, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(XDONT)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         XDONT (NUNI) = XDONT (ICRS)
      End If
   End Do
!
End Subroutine real32_unista
Subroutine int32_unista (XDONT, NUNI)

Integer (kind=int32), Dimension (:), Intent (InOut) :: XDONT
Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (Size(XDONT)) :: IWRKT
      Logical, Dimension (Size(XDONT)) :: IFMPTYT
      Integer :: ICRS
! __________________________________________________________
   Call UNIINV (XDONT, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(XDONT)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         XDONT (NUNI) = XDONT (ICRS)
      End If
   End Do
!
End Subroutine int32_unista
end module M_unista
