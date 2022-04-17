Module M_ctrper
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_mrgrnk
implicit none
Private
public :: ctrper
private :: real64_ctrper, real32_ctrper, int32_ctrper
interface ctrper
  module procedure real64_ctrper, real32_ctrper, int32_ctrper
end interface ctrper
contains
!>
!!##NAME
!!    ctrper(3f) - [orderpack] generate a random permutation of an array
!!                 leaving elements close to initial locations
!!                 (LICENSE:CC0-1.0)
!!
!!##SYNOPSIS
!!
!!     Subroutine ctrper (XDONT, PCLS)
!!
!!      ${TYPE} (kind=${KIND}), Intent (InOut) :: XDONT(:)
!!      Real, Intent (In) :: PCLS
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!
!!##DESCRIPTION
!!   Permute array XDONT randomly, but leaving elements close to their
!!   initial locations (nearbyness is controlled by PCLS).
!!
!!   The routine creates a real array of the indices of XDONT() perturbed
!!   by random values that are based on the size of PCLS. The new array is
!!   then ranked and the resulting index is used to permute the input array.
!!
!!   The relative proportion of initial order and random order is defined
!!   as 1-PCLS / PCLS, thus when PCLS = 0, there is no change in the order
!!   whereas the new order is fully random when PCLS = 1.
!!
!!   Note this differs from adding random noise to the values. The original
!!   values remain unchanged, their order is just perturbed.
!!
!!##OPTIONS
!!     XDONT      Array of values to perturb.
!!     PCLS       Proportion of closeness, constrained to the range 0.0(no
!!                change) to 1.0(fully random).
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_ctrper
!!    use M_ctrper, only : ctrper
!!    implicit none
!!    integer,allocatable :: xdont(:)
!!    integer,allocatable :: xout(:,:)
!!    real             :: pcls
!!    integer          :: isz, i, j
!!    isz=200
!!       if(allocated(xout))deallocate(xout)
!!       allocate(xout(3,isz))
!!
!!       xdont=[(i,i=1,isz)]*10
!!       call ctrper(xdont,0.0)
!!       xout(1,:)=xdont
!!
!!       xdont=[(i,i=1,isz)]*10
!!       call ctrper(xdont,0.1)
!!       xout(2,:)=xdont
!!
!!       xdont=[(i,i=1,isz)]*10
!!       call ctrper(xdont,1.0)
!!       xout(3,:)=xdont
!!
!!       write(*,'(a)')'count    unchanged  perturbed  random'
!!       do i=1,size(xdont)
!!          write(*,'(*(i8,1x))')i,xout(:,i)
!!       enddo
!!
!!    end program demo_ctrper
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
Subroutine real64_ctrper (XDONT, PCLS)
! _________________________________________________________
      Real (kind=real64), Dimension (:), Intent (InOut) :: XDONT
      Real, Intent (In) :: PCLS
! __________________________________________________________
!
      Real, Dimension (Size(XDONT)) :: XINDT
      Integer, Dimension (Size(XDONT)) :: JWRKT
      Real :: PWRK
      Integer :: I
!
      Call Random_Number (XINDT(:))
      PWRK = Min (Max (0.0, PCLS), 1.0)
      XINDT = Real(Size(XDONT)) * XINDT
      XINDT = PWRK*XINDT + (1.0-PWRK)*[ (Real(I), I=1,size(XDONT)) ]
      Call MRGRNK (XINDT, JWRKT)
      XDONT = XDONT (JWRKT)
!
End Subroutine real64_ctrper
Subroutine real32_ctrper (XDONT, PCLS)
! _________________________________________________________
      Real (kind=real32), Dimension (:), Intent (InOut) :: XDONT
      Real, Intent (In) :: PCLS
! __________________________________________________________
!
      Real, Dimension (Size(XDONT)) :: XINDT
      Integer, Dimension (Size(XDONT)) :: JWRKT
      Real :: PWRK
      Integer :: I
!
      Call Random_Number (XINDT(:))
      PWRK = Min (Max (0.0, PCLS), 1.0)
      XINDT = Real(Size(XDONT)) * XINDT
      XINDT = PWRK*XINDT + (1.0-PWRK)*[ (Real(I), I=1,size(XDONT)) ]
      Call MRGRNK (XINDT, JWRKT)
      XDONT = XDONT (JWRKT)
!
End Subroutine real32_ctrper
Subroutine int32_ctrper (XDONT, PCLS)
! _________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (InOut) :: XDONT
      Real, Intent (In) :: PCLS
! __________________________________________________________
!
      Real, Dimension (Size(XDONT)) :: XINDT
      Integer, Dimension (Size(XDONT)) :: JWRKT
      Real :: PWRK
      Integer :: I
!
      Call Random_Number (XINDT(:))
      PWRK = Min (Max (0.0, PCLS), 1.0)
      XINDT = Real(Size(XDONT)) * XINDT
      XINDT = PWRK*XINDT + (1.0-PWRK)*[ (Real(I), I=1,size(XDONT)) ]
      Call MRGRNK (XINDT, JWRKT)
      XDONT = XDONT (JWRKT)
!
End Subroutine int32_ctrper
end module M_ctrper
