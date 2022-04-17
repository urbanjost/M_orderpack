     program demo_fndnth
     use M_fndnth, only : fndnth
     implicit none
     integer,allocatable :: iarr(:)
        iarr=[80,70,30,40,50,60,20,10]
        write(*,*)fndnth(iarr,3)
        write(*,*)fndnth(iarr,1)
        write(*,*)fndnth(iarr,7)
     end program demo_fndnth
