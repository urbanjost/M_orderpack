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
!!      ${TYPE} (kind=${KIND}), Dimension (:), Intent (InOut) :: XDONT
!!      Integer, Intent (Out) :: NUNI
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
!!     character(len=:),allocatable :: xdont(:)
!!     integer :: i
!!     integer :: isz
!!     ! make an array of strings with lots of duplicates
!!     xdont=[character(len=20) :: 'fan','a','car','be','egg','dam','gas']
!!     isz=size(xdont)
!!     xdont=[xdont(5),xdont(isz:1:-2),xdont(isz-1:2:-2),xdont,xdont(1:2),xdont(1)]
!!     isz=size(xdont)
!!     write(*,g)'Original.................:',(trim(xdont(i)),i=1,isz)
!!     call unista(xdont,nuni)
!!     write(*,g)nuni,'Unique values..........:',(trim(xdont(i)),i=1,nuni)
!!     write(*,g)'Entire array.............:',(trim(xdont(i)),i=1,isz)
!!     write(*,g)
!!    endblock char
!!    end program demo_unista
!!
!!   Results:
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
