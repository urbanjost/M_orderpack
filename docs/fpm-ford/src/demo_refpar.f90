     program demo_refpar
     use,intrinsic :: iso_fortran_env, only : int32, real32, real64
     use M_refpar, only : refpar
     implicit none
     real(kind=real32) :: valsr(2000)
     integer           :: indx(2000)
     integer           :: i
        call random_seed()
        call random_number(valsr)
        valsr=valsr*1000000.0-500000.0
        call refpar(valsr,indx,300)
        valsr(:300)=valsr(indx(:300))
        do i=1,300-1
           if (valsr(i+1).lt.valsr(i))then
              write(*,*)'not sorted'
              stop 1
           endif
        enddo
        write(*,*)'random array now sorted'
     end program demo_refpar
