     program demo_inssor
     use,intrinsic :: iso_fortran_env, only : int32, real32, real64
     use M_inssor, only : inssor
     implicit none
     ! an insertion sort is very efficient for very small arrays
     ! but generally slower than methods like quicksort and mergesort.
     real(kind=real32) :: valsr(2000)
     real(kind=real64) :: valsd(2000)
     integer           :: valsi(2000)
     integer           :: i
        call random_seed()
        call random_number(valsr)
        call random_number(valsd)
        valsi=int(valsr*1000000.0)
        valsr=valsr*1000000.0-500000.0
        valsd=valsd*1000000.0-500000.0
        call inssor(valsi)
        do i=1,size(valsi)-1
           if (valsi(i+1).lt.valsi(i))then
              write(*,*)'not sorted'
              stop 1
           endif
        enddo
        call inssor(valsr)
        do i=1,size(valsr)-1
           if (valsr(i+1).lt.valsr(i))then
              write(*,*)'not sorted'
              stop 2
           endif
        enddo
        call inssor(valsd)
        do i=1,size(valsd)-1
           if (valsd(i+1).lt.valsd(i))then
              write(*,*)'not sorted'
              stop 3
           endif
        enddo
        write(*,*)'random arrays are now sorted'
     end program demo_inssor
