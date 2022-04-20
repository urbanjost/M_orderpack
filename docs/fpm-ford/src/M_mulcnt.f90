Module M_mulcnt
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
integer,parameter :: f_char=selected_char_kind("DEFAULT")
Private
public :: mulcnt
private :: real64_mulcnt, real32_mulcnt, int32_mulcnt
interface mulcnt
  module procedure real64_mulcnt, real32_mulcnt, int32_mulcnt, f_char_mulcnt
end interface mulcnt
contains
!>
!!##NAME
!!    mulcnt(3f) - [orderpack:MULTIPLICITY] Give the multiplicity for each
!!                 array value (number of times that it appears in the array)
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
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
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
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    real,parameter :: xdont(*)=[1,2,3,4,5,6,7,4,5,6,6,2]
!!    integer, dimension(size(xdont)) :: imult
!!    character(len=20),allocatable :: strings(:)
!!       !
!!       call mulcnt(xdont,imult)
!!       write(*,*)xdont
!!       write(*,g)imult
!!       !
!!       strings= [ character(len=20) ::                           &
!!       & 'red',    'green', 'blue', 'yellow', 'orange',   'black']
!!       call printme()
!!       !
!!       strings= [ character(len=20) ::                           &
!!       & 'two  ',  'four ', 'three', 'five',   'five',           &
!!       & 'two  ',  'four ', 'three', 'five',   'five',           &
!!       & 'four ',  'four ', 'three', 'one  ',  'five']
!!       call printme()
!!       !
!!       strings=['purple', 'purple', 'purple', 'purple']
!!       call printme()
!!       contains
!!       subroutine printme()
!!       integer,allocatable :: cindx(:)
!!       integer :: csz
!!       integer :: i
!!          csz=size(strings)
!!          if(allocated(cindx))deallocate(cindx)
!!          allocate(cindx(csz))
!!          call mulcnt(strings,cindx)
!!          write(*,g)(trim(strings(i)),i=1,csz)
!!          write(*,g)cindx
!!       end subroutine printme
!!    end program demo_mulcnt
!!
!!   Results:
!!
!!       1.000000   2.000000   3.000000   4.000000   5.000000
!!       6.000000   7.000000   4.000000   5.000000   6.000000
!!       6.000000   2.000000
!!    1 2 1 2 2 3 1 2 2 3 3 2
!!    red green blue yellow orange black
!!    1 1 1 1 1 1
!!    two four three five five two four three five five four four three one five
!!    2 4 3 5 5 2 4 3 5 5 4 4 3 1 5
!!    purple purple purple purple
!!    4 4 4 4
!!
!!
!!##AUTHOR
!!     Michel Olagnon, Mar 2000
!!
!!     John Urban, 2022.04.16
!!     o added man-page and reduced to a template using the
!!       prep(1) preprocessor.
!!
!!##LICENSE
!!    CC0-1.0
Subroutine real64_mulcnt (XDONT, IMULT)
! __________________________________________________________
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
! __________________________________________________________
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
! __________________________________________________________
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
Subroutine f_char_mulcnt (XDONT, IMULT)
! __________________________________________________________
      character (kind=f_char,len=*), Dimension (:), Intent (In) :: XDONT
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
End Subroutine f_char_mulcnt
end module M_mulcnt
