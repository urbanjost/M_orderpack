     program demo_mulcnt
     use M_mulcnt, only : mulcnt
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     real,parameter :: xdont(*)=[1,2,3,4,5,6,7,4,5,6,6,2]
     integer, dimension(size(xdont)) :: imult
     character(len=20),allocatable :: strings(:)
        !
        call mulcnt(xdont,imult)
        write(*,*)xdont
        write(*,g)imult
        !
        strings= [ character(len=20) ::                           &
        & 'red',    'green', 'blue', 'yellow', 'orange',   'black']
        call printme()
        !
        strings= [ character(len=20) ::                           &
        & 'two  ',  'four ', 'three', 'five',   'five',           &
        & 'two  ',  'four ', 'three', 'five',   'five',           &
        & 'four ',  'four ', 'three', 'one  ',  'five']
        call printme()
        !
        strings=['purple', 'purple', 'purple', 'purple']
        call printme()
        contains
        subroutine printme()
        integer,allocatable :: cindx(:)
        integer :: csz
        integer :: i
           csz=size(strings)
           if(allocated(cindx))deallocate(cindx)
           allocate(cindx(csz))
           call mulcnt(strings,cindx)
           write(*,g)(trim(strings(i)),i=1,csz)
           write(*,g)cindx
        end subroutine printme
     end program demo_mulcnt
