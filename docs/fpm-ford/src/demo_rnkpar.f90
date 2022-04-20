     program demo_rnkpar
     use,intrinsic :: iso_fortran_env, only : int32, real32, real64
     use M_rnkpar, only : rnkpar
     implicit none
     integer,parameter :: ivals=300
     real(kind=real32) :: valsr(2000)
     real(kind=real32) :: out(ivals)
     integer           :: indx(2000)
     integer           :: i
        call random_seed()
        call random_number(valsr)
        valsr=valsr*1000000.0-500000.0
        call rnkpar(valsr,indx,ivals)
        out=valsr(indx(:ivals))
        do i=1,ivals-1
           if (out(i+1).lt.out(i))then
              write(*,*)'not sorted'
              stop 1
           endif
        enddo
        write(*,*)'random array now sorted'
     end program demo_rnkpar