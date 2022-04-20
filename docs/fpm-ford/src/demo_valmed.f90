     program demo_valmed
     use M_valmed, only : valmed
     implicit none
     real,parameter :: xdont(*)=[80.0,70.0,20.0,10.0,1000.0]
     real res_med
        write(*,*)valmed(xdont)
        write(*, *)valmed([11, 22, 33, 44, 55, 66, 77, 88])
        write(*, *)valmed([11.0d0, 22.0d0, 33.0d0, 66.0d0, 77.0d0, 88.0d0])
     end program demo_valmed
