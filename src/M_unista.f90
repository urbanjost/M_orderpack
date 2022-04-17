Module M_unista
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
Private
Integer, Parameter :: kdp = selected_real_kind(15)
public :: unista
private :: kdp
private :: R_unista, I_unista, D_unista
interface unista
  module procedure d_unista, r_unista, i_unista
end interface unista
contains
!>
!!##NAME
!!    unista(3f) - [orderpack] do stuff
!!                 (LICENSE:CC0-1.0)
!!
!!##SYNOPSIS
!!
!!     Subroutine unista (yyyyyy)
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
!!
!!##OPTIONS
!!     XXXXX      description
!!     YYYYY      description
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unista
!!    use M_unista, only : unista
!!    implicit none
!!       call unista(yyyyyy)
!!    end program demo_unista
!!
!!   Results:
!!
!!##AUTHOR
!!     Michel Olagnon, 2000-2012
!!
!!     John Urban, 2022.04.16
!!         o added man-page and reduced to a template using the
!!           prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0

Subroutine D_unista (XDONT, NUNI)
!!  UNISTA = (Stable unique) Removes duplicates from an array,
!!           leaving unique entries in the order of their first
!!           appearance in the initial set.
!! Michel Olagnon - Feb. 2000
!!__________________________________________________________
!!__________________________________________________________
      Real (kind=kdp), Dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
!
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
      Return
!
End Subroutine D_unista

Subroutine R_unista (XDONT, NUNI)
!!  UNISTA = (Stable unique) Removes duplicates from an array,
!!           leaving unique entries in the order of their first
!!           appearance in the initial set.
!! Michel Olagnon - Feb. 2000
!!__________________________________________________________
!!_________________________________________________________
      Real, Dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
!
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
      Return
!
End Subroutine R_unista
Subroutine I_unista (XDONT, NUNI)
!!  UNISTA = (Stable unique) Removes duplicates from an array,
!!           leaving unique entries in the order of their first
!!           appearance in the initial set.
!! Michel Olagnon - Feb. 2000
!!__________________________________________________________
!!__________________________________________________________
      Integer, Dimension (:), Intent (InOut)  :: XDONT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
!
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
      Return
!
End Subroutine I_unista
end module M_unista
