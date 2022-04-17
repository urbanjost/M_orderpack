     program demo_mulcnt
     use M_mulcnt, only : mulcnt
     implicit none
     real,parameter :: xdont(*)=[1,2,3,4,5,6,7,4,5,6,6,2]
     integer, dimension(size(xdont)) :: imult
        call mulcnt(xdont,imult)
        write(*,*)xdont
        write(*,*)imult
     end program demo_mulcnt
