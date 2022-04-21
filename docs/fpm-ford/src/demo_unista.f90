     program demo_unista
     ! remove duplicates with remaining elements remaining in initial order
     use M_unista, only : unista
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
     integer :: nuni

     int : block
     integer,allocatable :: xdont(:)
      xdont=[44,33,33,33,22,11,33,44,55,33]
      print list,'ORIGINAL:',xdont
      call unista(xdont,nuni)
      xdont=xdont(:nuni)
      print list,'UNIQUE:',xdont
     endblock int

     float : block
     real,allocatable :: xdont(:)
      xdont=[4.4,3.3,3.3,3.3,2.2,1.1,3.3,4.4,5.5,3.3]
      print list,'ORIGINAL:',xdont
      call unista(xdont,nuni)
      xdont=xdont(:nuni)
      print list,'UNIQUE:',xdont
     endblock float

     char: block
      character(len=:),allocatable :: ar(:)
      integer :: i
      integer :: isz
      ! make an array of strings with lots of duplicates
      ar=[character(len=20) :: 'fan','a','car','be','egg','dam','gas']
      isz=size(ar)
      ar=[ar(5),ar(isz:1:-2),ar(isz-1:2:-2),ar,ar(1:2),ar(1)]
      isz=size(ar)
      write(*,g)'Original.................:',(trim(ar(i)),i=1,isz)
      call unista(ar,nuni)
      write(*,g)nuni,'Unique values..........:',(trim(ar(i)),i=1,nuni)
      write(*,g)'Entire array.............:',(trim(ar(i)),i=1,isz)
      write(*,g)
     endblock char
     end program demo_unista
