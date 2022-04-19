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
!!    ctrper(3f) - [orderpack:PERMUTATION] generate a random permutation
!!                 of an array leaving elements close to initial locations
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
!!    > count unchanged  perturbed  random
!!    >     1       10       30     1930
!!    >     2       20       20     1430
!!    >     3       30      120      320
!!    >     4       40      110      670
!!    >     5       50       90      750
!!    >     6       60       60      790
!!    >     7       70      130     1850
!!    >     8       80       50      390
!!    >     9       90      150     1470
!!    >    10      100       80      710
!!    >    11      110       70      820
!!    >    12      120       10      680
!!    >    .       .         .       .
!!    >    .       .         .       .
!!    >    .       .         .       .
!!    >    .       .         .       .
!!    >   190     1900     1860     1210
!!    >   191     1910     1890      580
!!    >   192     1920     1840      600
!!    >   193     1930     1870     1640
!!    >   194     1940     1920      290
!!    >   195     1950     2000     1180
!!    >   196     1960     1910      340
!!    >   197     1970     1990     1540
!!    >   198     1980     1940      990
!!    >   199     1990     1950      470
!!    >   200     2000     1980      200
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
