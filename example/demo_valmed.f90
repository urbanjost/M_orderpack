     program demo_valmed
     ! return median value
     use M_valmed, only : valmed
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
        write(*,g)'real   ',&
        valmed( [80.0,70.0,20.0,10.0,1000.0] )
        write(*,g)'integer',&
        valmed( [11, 22, 33, 44, 55, 66, 77, 88] )
        write(*,g)'double ',&
        valmed( [11.0d0, 22.0d0, 33.0d0, 66.0d0, 77.0d0, 88.0d0] )
     end program demo_valmed
