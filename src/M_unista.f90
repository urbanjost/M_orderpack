Module M_unista
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: unista
interface unista
  module procedure real64_unista, real32_unista, int32_unista, f_char_unista
end interface unista
contains
!>
!!##NAME
!!    unista(3f) - [orderpack:UNIQUE] (Stable unique) Removes duplicates from an
!!                 array otherwise retaining original order
!!
!!##SYNOPSIS
!!
!!     Subroutine unista (XDONT, NUNI)
!!
!!      ${TYPE} (kind=${KIND}), Intent (InOut) :: XDONT(:)
!!      Integer, Intent (Out)                  :: NUNI
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
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
!!    ! remove duplicates with remaining elements remaining in initial order
!!    use M_unista, only : unista
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
!!    integer :: nuni
!!
!!    int : block
!!    integer,allocatable :: xdont(:)
!!     xdont=[44,33,33,33,22,11,33,44,55,33]
!!     print list,'ORIGINAL:',xdont
!!     call unista(xdont,nuni)
!!     xdont=xdont(:nuni)
!!     print list,'UNIQUE:',xdont
!!    endblock int
!!
!!    float : block
!!    real,allocatable :: xdont(:)
!!     xdont=[4.4,3.3,3.3,3.3,2.2,1.1,3.3,4.4,5.5,3.3]
!!     print list,'ORIGINAL:',xdont
!!     call unista(xdont,nuni)
!!     xdont=xdont(:nuni)
!!     print list,'UNIQUE:',xdont
!!    endblock float
!!
!!    char: block
!!     character(len=:),allocatable :: ar(:)
!!     integer :: i
!!     integer :: isz
!!     ! make an array of strings with lots of duplicates
!!     ar=[character(len=20) :: 'fan','a','car','be','egg','dam','gas']
!!     isz=size(ar)
!!     ar=[ar(5),ar(isz:1:-2),ar(isz-1:2:-2),ar,ar(1:2),ar(1)]
!!     isz=size(ar)
!!     write(*,g)'Original.................:',(trim(ar(i)),i=1,isz)
!!     call unista(ar,nuni)
!!     write(*,g)nuni,'Unique values..........:',(trim(ar(i)),i=1,nuni)
!!     write(*,g)'Entire array.............:',(trim(ar(i)),i=1,isz)
!!     write(*,g)
!!    endblock char
!!    end program demo_unista
!!
!!   Results:
!!
!!    ORIGINAL:, 44, 33, 33, 33, 22, 11, 33, 44, 55, 33
!!    UNIQUE:, 44, 33, 22, 11, 55
!!    ORIGINAL:, 4.400000, 3.300000, 3.300000, 3.300000, 2.200000,
!!    1.100000, 3.300000, 4.400000, 5.500000, 3.300000
!!    UNIQUE:, 4.400000, 3.300000, 2.200000, 1.100000, 5.500000
!!    Original.................: egg gas egg car fan dam be a fan
!!    a car be egg dam gas fan a fan
!!    7 Unique values..........: egg gas car fan dam be a
!!    Entire array.............: egg gas car fan dam be a
!!    a fan a car be egg dam gas fan a fan
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
! __________________________________________________________
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
! __________________________________________________________
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
! __________________________________________________________
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
Subroutine f_char_unista (XDONT, NUNI)
! __________________________________________________________
   character (kind=f_char,len=*), Dimension (:), Intent (InOut) :: XDONT
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
End Subroutine f_char_unista
end module M_unista
