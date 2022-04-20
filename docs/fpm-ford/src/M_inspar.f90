Module M_inspar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: inspar
interface inspar
  module procedure real64_inspar, real32_inspar, int32_inspar, f_char_inspar
end interface inspar
contains
!>
!!##NAME
!!    inspar(3f) - [orderpack:SORT:PARTIAL] partially sorts an array,
!!                 bringing the N lowest values to the beginning of the array
!!
!!##SYNOPSIS
!!
!!
!!     Subroutine inspar (XDONT, NORD)
!!
!!      ${TYPE} (kind=${KIND}), Dimension (:), Intent (InOut) :: XDONT
!!      Integer, Intent (In) :: NORD
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    INSPAR partially sorts XDONT, bringing the NORD lowest values to the
!!    beginning of the array.
!!
!!    This subroutine uses an insertion sort, limiting insertion to the
!!    first NORD values. It does not use any work array and is faster when
!!    NORD is very small (2-5), but worst case behavior can happen fairly
!!    probably (initially inverse sorted). Therefore, in many cases, the
!!    refined quicksort method is faster.
!!
!!##OPTIONS
!!     XDONT      The array to partially sort
!!     NORD       number of sorted values to return.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_inspar
!!    use M_inspar, only : inspar
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer :: nord
!!
!!    int: block
!!       integer,allocatable :: ia(:)
!!       ia=[10,5,7,1,4,5,6,8,9,10,1]
!!       nord=5
!!       write(*,g)'Original.................:',ia
!!       call inspar(ia,nord)
!!       write(*,g)'Number of indices to sort:',nord
!!       write(*,g)nord,'Lowest values..........:',ia(:nord)
!!       write(*,g)'Entire array.............:',ia
!!       write(*,g)
!!    endblock int
!!    char: block
!!       character(len=:),allocatable :: ca(:)
!!       integer :: i
!!       ca=[character(len=20) :: 'fan','a','car','be','egg','dam','gas']
!!       nord=3
!!       write(*,g)'Original.................:',(trim(ca(i)),i=1,size(ca))
!!       call inspar(ca,nord)
!!       write(*,g)'Number of indices to sort:',nord
!!       write(*,g)nord,'Lowest values..........:',(trim(ca(i)),i=1,nord)
!!       write(*,g)'Entire array.............:',(trim(ca(i)),i=1,size(ca))
!!       write(*,g)
!!    endblock char
!!
!!    end program demo_inspar
!!
!!   Results:
!!
!!    Original.................: 10 5 7 1 4 5 6 8 9 10 1
!!    Number of indices to sort: 5
!!    5 Lowest values..........: 1 1 4 5 5
!!    Entire array.............: 1 1 4 5 5 10 7 8 9 10 6
!!
!!    Original.................: fan a car be egg dam gas
!!    Number of indices to sort: 3
!!    3 Lowest values..........: a be car
!!    Entire array.............: a be car fan egg dam gas
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
Subroutine f_char_inspar (XDONT, NORD)
   character (kind=f_char,len=*), Dimension (:), Intent (InOut) :: XDONT
   Integer, Intent (In) :: NORD
! __________________________________________________________
   character (kind=f_char,len=len(XDONT)) :: XWRK, XWRK1
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
End Subroutine f_char_inspar
end module M_inspar
