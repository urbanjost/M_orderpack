!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_orderpack_docs
implicit none
private
public help_intrinsics
!interface help_intrinsics
!   module procedure help_intrinsics_all
!   module procedure help_intrinsics_one
!end interface help_intrinsics
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)                       :: name
logical,intent(in),optional                       :: prefix
logical,intent(in),optional                       :: topic
logical,intent(in),optional                       :: m_help
character(len=256),allocatable                    :: textblock(:)
character(len=:),allocatable                      :: a, b, c
integer                                           :: i, p, pg
   select case(name)
   case('','manual','orderpack')
      textblock=help_intrinsics_all(prefix,topic,m_help)
   case('fortran','toc')
      textblock=help_intrinsics_section()
      do i=1,size(textblock)
         p = index(textblock(i), '[')
         pg = index(textblock(i), ']')
         if(p.gt.0.and.pg.gt.p)then
          a=textblock(i)(:p-1)
          b=textblock(i)(p:pg)
          c=textblock(i)(pg+1:)
          textblock(i)=b//' '//a//c
         endif
      enddo
      call sort_name(textblock)
   case default
      textblock=help_intrinsics_one(name,prefix,topic,m_help)
   end select
end function help_intrinsics
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_section() result (textblock)

!@(#) grab lines in NAME section and append them to generate an index of manpages

character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: add(:)
character(len=256),allocatable  :: label
character(len=10)               :: cnum
integer                         :: i
integer                         :: icount
logical                         :: is_label
logical                         :: grab
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum)
      if( size(add) .eq. 0 ) exit
      label=''
      grab=.false.
      is_label=.false.
      do i=1,size(add)
         if(add(i).ne.'')then
            is_label=verify(add(i)(1:1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0 &
            .and. verify(trim(add(i)),'ABCDEFGHIJKLMNOPQRSTUVWXYZ ') == 0
         endif
         if(add(i).eq.'')then
            ! skip
         elseif(is_label.and.add(i).eq.'NAME')then
            grab=.true.
         elseif(is_label)then
            exit
         elseif(grab)then
            label=adjustl(trim(label))//' '//adjustl(compact(trim(add(i))))
         endif
      enddo
      textblock=[character(len=256) :: textblock,compact(label)]
      icount=icount + 1
   enddo
end function help_intrinsics_section
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_all(prefix,topic,m_help) result (textblock)
logical,intent(in),optional     :: prefix
logical,intent(in),optional     :: topic
logical,intent(in),optional     :: m_help
character(len=256),allocatable  :: textblock(:)
character(len=256),allocatable  :: header(:)
character(len=256),allocatable  :: add(:)
character(len=10)               :: cnum
integer                         :: icount
   allocate(textblock(0))
   icount=1
   do
      write(cnum,'(i0)') icount
      add=help_intrinsics_one(cnum,prefix,topic,m_help)
      if( size(add) .eq. 0 ) exit
      textblock=[character(len=256) :: textblock,add]
      icount=icount + 1
   enddo
   if(present(m_help))then
      if(m_help)then
         header=[ character(len=256) :: &
         '================================================================================',    &
         'SUMMARY',    &
         ' The primary Fortran topics are',    &
         ' tan                   tanh                      this_image',    &
         '']
         textblock=[header,textblock]
      endif
   endif
end function help_intrinsics_all
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function help_intrinsics_one(name,prefix,topic,m_help) result (textblock)
character(len=*),intent(in)      :: name
logical,intent(in),optional      :: prefix
logical,intent(in),optional      :: m_help
logical,intent(in),optional      :: topic
character(len=256),allocatable   :: textblock(:)
character(len=:),allocatable     :: shortname
integer                          :: i
select case(name)

case('1','median')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   median(3f) - [orderpack:MEDIAN] Calculates median VALUE. If', &
'                number of elements is even, returns average of the two', &
'                "medians".', &
'', &
'SYNOPSIS', &
'    Function Median (INVALS)', &
'', &
'     ${TYPE} (Kind=${KIND}), Intent (In) :: INVALS(:)', &
'     ${TYPE} (Kind=${KIND})              :: MEDIAN', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'   MEDIAN(3f) calculates the median value of the array INVALS(). It is', &
'   a modified version of MEDIANVAL(3f) that provides the average between', &
'   the two middle values in the case size(INVALS) is even.', &
'', &
'   This routine uses a pivoting strategy similar to the method of finding', &
'   the median based on the Quick-sort algorithm, but we skew the pivot', &
'   choice to try to bring it to NORD as fast as possible. It uses two', &
'   temporary arrays, where it stores the indices of the values smaller', &
'   than the pivot (ILOWT), and the indices of values larger than the', &
'   pivot that we might still need later on (IHIGT). It iterates until', &
'   it can bring the number of values in ILOWT to exactly NORD, and then', &
'   finds the maximum of this set.', &
'', &
'OPTIONS', &
'    INVALS      array to determine the median value of.', &
'', &
'RETURNS', &
'    MEDIAN     median value. If INVALS contains an even number', &
'               of elements the value is the average of the', &
'               two "medians".', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_median', &
'   ! calculate median value', &
'   use M_orderpack, only : median', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'', &
'      write(*,g) ''real   '',median(&', &
'      [80.0,70.0,20.0,10.0,1000.0] )', &
'', &
'      write(*,g) ''integer'',median(&', &
'      [11, 22, 33, 44, 55, 66, 77, 88] )', &
'', &
'      write(*,g) ''double '',median(&', &
'      [11.0d0,22.0d0,33.0d0,66.0d0,77.0d0,88.0d0])', &
'', &
'   end program demo_median', &
'', &
'  Results:', &
'', &
'   real    70.00000', &
'   integer 49', &
'   double  49.50000000000000', &
'', &
'AUTHOR', &
'   Michel Olagnon - Aug. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="median"

call select()


case('2','medianloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   medianloc(3f) - [orderpack:MEDIAN] Returns median value''s INDEX.', &
'', &
'SYNOPSIS', &
'    Subroutine MedianLoc (INVALS, OUTORD)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: OUTORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   MEDIANLOC(3f) Returns the index of the median (((Size(INVALS)+1))/2^th value).', &
'', &
'', &
'   Internally, MEDIANLOC(3f) Finds the index of the median of the array', &
'   INVALS() using the recursive procedure described in Knuth, The Art of', &
'   Computer Programming, vol. 3, 5.3.3.', &
'', &
'   This procedure is linear in time, and does not require to be able', &
'   to interpolate in the set as the one used in ORDERLOC(3f), which can', &
'   also be used to calculate a median. It also has better worst-case', &
'   behavior than ORDERLOC(3f), but is about 10% slower on average for', &
'   random uniformly distributed values.', &
'', &
'OPTIONS', &
'    INVALS     array to find the median value of.', &
'    OUTORD     index of the median value.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_medianloc', &
'   ! return index of median value', &
'   use M_orderpack, only : medianloc', &
'   implicit none', &
'   real,allocatable :: INVALS(:)', &
'   character(len=:),allocatable :: cdont(:)', &
'   character(len=*),parameter :: fmt=''(i5,t11,g0)''', &
'   integer :: ii', &
'      write(*,*) ''location  median''', &
'', &
'      INVALS=[80.0,70.0,20.0,10.0,1000.0]', &
'      call medianloc(INVALS,ii)', &
'      write(*,fmt) ii,INVALS(ii)', &
'      !', &
'      INVALS=[11, 22, 33, 44, 55, 66, 77, 88]', &
'      call medianloc(INVALS,ii)', &
'      write(*,fmt) ii,INVALS(ii)', &
'      !', &
'      INVALS=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]', &
'      call medianloc(INVALS,ii)', &
'      write(*,fmt) ii,INVALS(ii)', &
'      !', &
'      cdont=[character(len=20) :: ''apple'',''bee'',''cherry'',''duck'',&', &
'              ''elephant'',''finger'',''goose'',''h'',''insect'',''j'']', &
'      call medianloc(cdont,ii)', &
'      write(*,fmt) ii,cdont(ii)', &
'      !', &
'   end program demo_medianloc', &
'', &
'  Results:', &
'', &
'    location  median', &
'       2     70.00000', &
'       4     44.00000', &
'       5     33.00000', &
'       5     elephant', &
'', &
'AUTHOR', &
'   Michel Olagnon, 2000-2012', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="medianloc"

call select()


case('3','medianval')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   medianval(3f) - [orderpack:MEDIAN] Returns median VALUE.', &
'', &
'SYNOPSIS', &
'    Recursive Function MedianVal (INVALS) Result (RES_MED)', &
'', &
'      ${TYPE} (kind=${KIND}),  Intent (In) :: INVALS(:)', &
'      ${TYPE} (kind=${KIND})               :: RES_MED', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'', &
'   Finds out and returns the median (((Size(INVALS)+1))/2^th value)', &
'   of INVALS.', &
'', &
'   Internally, it uses the recursive procedure described in Knuth,', &
'   The Art of Computer Programming, vol. 3, 5.3.3 .', &
'', &
'   The procedure is linear in time, and does not require to be able to', &
'   interpolate in the set as the one used in ORDERVAL(3f)/ORDERLOC(3f). It', &
'   also has better worst case behavior than ORDERVAL(3f)/ORDERLOC(3f), and', &
'   is about 20% faster in average for random uniformly distributed values.', &
'', &
'OPTIONS', &
'    INVALS      input array', &
'', &
'RETURNS', &
'    RES_MED    the median value of the array INVALS', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_medianval', &
'   ! return median value', &
'   use M_orderpack, only : medianval', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'      write(*,g)''real   '',&', &
'      medianval( [80.0,70.0,20.0,10.0,1000.0] )', &
'      write(*,g)''integer'',&', &
'      medianval( [11, 22, 33, 44, 55, 66, 77, 88] )', &
'      write(*,g)''double '',&', &
'      medianval( [11.0d0, 22.0d0, 33.0d0, 66.0d0, 77.0d0, 88.0d0] )', &
'   end program demo_medianval', &
'', &
'  Results:', &
'', &
'   real    70.00000', &
'   integer 44', &
'   double  33.00000000000000', &
'', &
'AUTHOR', &
'   Michel Olagnon, 2000-2012', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="medianval"

call select()


case('4','M_orderpack')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   M_orderpack(3f) - [orderpack::INTRO]General and Specialized Ranking and Sorting Routines', &
'SYNOPSIS', &
'   Procedure names and syntax:', &
'', &
'    use M_orderpack, only : &', &
'     Sort,             & ! Subroutine Sort             (INOUTVALS)', &
'     Sort_Special,     & ! Subroutine Sort_Special     (INOUTVALS)', &
'     Psort,            & ! Subroutine Psort            (INOUTVALS, nord)', &
'', &
'     Rank,             & ! Subroutine Rank             (INVALS, imult)', &
'     Rank_Basic,       & ! Subroutine Rank_Basic       (INVALS, irngt)', &
'     Rank_Decreasing,  & ! Subroutine Rank_Decreasing  (INVALS, igoest)', &
'     Rank_Unique,      & ! Subroutine Rank_Unique      (INVALS, irngt, nuni)', &
'', &
'     Prank,            & ! Subroutine Prank            (INVALS, irngt, nord)', &
'     Prank_Basic,      & ! Subroutine Prank_Basic      (INVALS, irngt, nord)', &
'     Prank_Decreasing, & ! Subroutine Prank_Decreasing (INVALS, irngt, nord)', &
'     Prank_Special,    & ! Subroutine Prank_Special,   (INVALS, irngt, nord)', &
'     Prank_Unique,     & ! Subroutine Prank_Unique     (INVALS, irngt, nord)', &
'', &
'     Median,           & ! Function Median             (INVALS)', &
'     MedianVal,        & ! Function MedianVal          (INVALS)', &
'     MedianLoc,        & ! Subroutine MedianLoc        (INVALS, indm)', &
'', &
'     Orderval,         & ! Function OrderVal           (INVALS, nord)', &
'     OrderLoc,         & ! Integer Function OrderLoc   (INVALS, nord)', &
'     Orderval_Special, & ! Function OrderVal_Special   (INVALS, nord)', &
'', &
'     Occurrences,      & ! Subroutine Occurrences      (INVALS, imult)', &
'     Unique,           & ! Subroutine Unique           (INOUTVALS, nuni)', &
'     Perturb             ! Subroutine Perturb          (INOUTVALS, CLOSENESS)', &
'', &
'   The procedures may be accessed via their original names in ORDERPACK2.0', &
'   as well, one per module:', &
'', &
'    ! previous ORDERPACK2.0 name ! ORDERPACK 2.1 name', &
'    use M_refsor, only : refsor  ! Sort', &
'    use M_inssor, only : inssor  ! Sort_special', &
'    use M_inspar, only : inspar  ! psort', &
'    use M_mrgrnk, only : mrgrnk  ! rank', &
'    use M_mrgref, only : mrgref  ! rank_basic', &
'    use M_uniinv, only : uniinv  ! rank_decreasing', &
'    use M_unirnk, only : unirnk  ! rank_unique', &
'    use M_rnkpar, only : rnkpar  ! prank', &
'    use M_refpar, only : refpar  ! prank_basic', &
'    use M_rapknr, only : rapknr  ! prank_decreasing', &
'    use M_rinpar, only : rinpar  ! prank_special', &
'    use M_unipar, only : unipar  ! prank_unique', &
'    use M_median, only : median  ! median', &
'    use M_valmed, only : valmed  ! medianval', &
'    use M_indmed, only : indmed  ! medianloc', &
'    use M_valnth, only : valnth  ! orderval', &
'    use M_indnth, only : indnth  ! orderloc', &
'    use M_fndnth, only : fndnth  ! orderval_special', &
'    use M_mulcnt, only : mulcnt  ! occurrences', &
'    use M_unista, only : unista  ! unique', &
'    use M_ctrper, only : ctrper  ! perturb', &
'', &
'DESCRIPTION', &
'   ORDERPACK 2.1 - Unconditional, Unique and Partial Ranking, Sorting,', &
'                   and Permutation', &
'', &
'   ORDERPACK 2.1 performs both conventional sorting and ranking as well as', &
'   the rarer specialized ordering tasks such as partial sorting, partial', &
'   ranking, unique sorting, unique ranking, inverse unique ranking, and', &
'   more. These partial sort and ranking routines can greatly accelerate', &
'   many computations when users need only the M largest or smallest', &
'   elements out of a N-element vector.', &
'', &
'   All the specialized procedures have a range over which they far', &
'   outperform a basic sort, and most have a range where they dramatically', &
'   underperform. If you are not limited by memory requirements or have no', &
'   issues with runtimes the simplest solution may be just to use SORT(3f)', &
'   and RANK(3f).', &
'', &
'   Otherwise, your solution method may very well depend on the size of', &
'   the input arrays, whether the data is already close to the required', &
'   order, or how costly it is to create work arrays or an index array.', &
'', &
'   So, if you want the smallest value in an array call the intrinsic', &
'   MINVAL(3f), not ORDERVAL(3f).', &
'', &
'SORTING', &
'    FULL SORTING', &
'       Sort          Sorts array into ascending order (Quicksort)', &
'       Sort_Special  Sorts array into ascending order (Insertion sort,', &
'                     generally for small or nearly sorted arrays)', &
'    PARTIAL SORTING', &
'       Psort             partially sorts an array', &
'       Orderval          Return VALUE of Nth lowest value of array', &
'                         (QuickSort)', &
'       Orderval_Special  Return Nth lowest value of an array', &
'                         (Insert-sort, generally for small or nearly', &
'                         sorted arrays))', &
'       MedianVal         finds the median of an array', &
'       Median            Return median value of array. If number of elements', &
'                         is even, return average of the two "medians"', &
'RANKING', &
'    UNCONDITIONAL RANKING', &
'       Rank        ranks array (optimized merge-sort)', &
'       Rank_Basic  ranks array (basic merge-sort)', &
'    PARTIAL RANKING', &
'       Prank             partially ranks array (Optimized QuickSort)', &
'       Prank_Basic       partially ranks array', &
'       Prank_Decreasing  partially ranks array in DECREASING order', &
'       Prank_Special     partially ranks array (Basic Insert-Sort)', &
'       Orderloc          Return INDEX of Nth value of array (QuickSort-like)', &
'       MedianLoc         Returns INDEX of median value of an array.', &
'    UNIQUE RANKING', &
'       Rank_Unique       performs a MergeSort ranking of an array,', &
'                         with removal of duplicate entries.', &
'       Rank_Decreasing   an inverse ranking of an array,', &
'                         with duplicate entries assigned the same rank.', &
'       Prank_Unique      partially rank an array removing duplicates', &
'UNIQUE', &
'       Unique        Removes duplicates from an array', &
'                     otherwise retaining original order', &
'MULTIPLICITY', &
'       Occurrences   Give the multiplicity for each array value', &
'PERMUTATION', &
'       Perturb  a random permutation of an array, optionally leaving', &
'                elements close to initial locations', &
'', &
'RATIONALE', &
'', &
'   While Fortran 90 and later variants have made life much easier for', &
'   scientific programmers than Fortran 77, the language still lacks', &
'   depth in public domain utilities. The following package, ORDERPACK', &
'   2.1, provides important but uncommon routines needed to complete the', &
'   Fortran programming environment.', &
'', &
'INTRODUCTION', &
'', &
'   The existing fortran code base provides many conventional ranking', &
'   or sorting routines, but very few specialized ranking or sorting', &
'   routines. Specifically, we know of no other Fortran code which sorts', &
'   or ranks only a small proportion of an array (partial ordering). Such', &
'   partial ranking routines have applications in statistics for rapidly', &
'   computing extreme order statistics, finding nearest neighbors, and', &
'   other clustering operations. In addition, many applications need to', &
'   work with only the unique values in an array (unique ordering). Such', &
'   unique ranking routines allow users to isolate individual cases out', &
'   of a mass of discrete data. Many times the frequency of the unique', &
'   values proves interesting (e.g., empirical distributions).', &
'', &
'   ORDERPACK handles all of these ordering needs.', &
'', &
'   Also, ORDERPACK contains a partial unique ranking routine. Such a', &
'   routine would prove useful in finding a limited number of unique', &
'   values in an array.', &
'', &
'   Inversion of orderings becomes difficult when duplicates exist (not', &
'   a one-to-one relation). The ORDERPACK inverse ranking routine handles', &
'   this difficult case.', &
'', &
'   As an added bonus ORDERPACK provides an unusual routine which allows', &
'   user controllable partial random permutation of arrays.', &
'', &
'   ORDERPACK of course contains conventional or unconditional sorting', &
'   routines as well.', &
'', &
'   Finally, many Fortran sorting or ranking routines do not take advantage', &
'   of available memory and cache to maximize performance. The routines', &
'   in ORDERPACK have been designed to take advantage of modern machines.', &
'', &
'RANKING VERSUS SORTING', &
'', &
'   Ranking consists in finding, for each element of a set, its order', &
'   (rank) in the sorted set, without effectively changing the initial', &
'   order (or disorder! ) of the set. In many instances, it better suits', &
'   the actual need of the user than sorting, as the ranks can then be', &
'   used to order other related sets or components of a user type.', &
'', &
'   Ranking is especially needed when the sizes of the elements are large,', &
'   and therefore moving them around is resource-consuming.', &
'', &
'RANKING', &
'', &
'   In some instances, one is not actually interested in modifying the', &
'   order of the elements in a set, but only in knowing how to access them', &
'   in increasing -- or decreasing -- order. Ranking, as it is called,', &
'   provides the index array I(:) such as the set S(I(:)) is ordered. One', &
'   of the advantages of carrying out ranking rather than sorting is that', &
'   the index array can be computed without the performance penalty of', &
'   moving the elements around when they are of large sizes. A similar', &
'   point is that the index array can be used to index other data.', &
'', &
'OPTIMIZATION CHOICES', &
'', &
'   We tried to take into account the recent trends in computing to make', &
'   our compromise choices. Of course, no two problems are the same, and', &
'   for some of them the following decisions may happen to be wrong. We', &
'   just hope that for most cases, they will be right.', &
'', &
'     * Make extensive use of work arrays: Memory can be extended,', &
'       time cannot.', &
'     * Try to reduce the number of operations in the inner loops, even', &
'       if it increases code size.', &
'     * Assume that cache size is relatively small, and try to maximize', &
'       cache hits.', &
'', &
'INTERFACE', &
'', &
'   Robust routines make their interface known to the calling', &
'   program. There are three main ways to implement this in Fortran:', &
'', &
'     * Explicit interfaces, either included in the body of the calling', &
'       routine, or gathered in an ''interface module''. An example of', &
'       including an interface block in the calling program can be found', &
'       in the sample program sort7.f90.', &
'     * Embedding the routine of interest as a "contained routine" into', &
'       the calling procedure. An example of such way can be found in', &
'       the follow.f90 program, that rebuilds a curve from a set of X,', &
'       Y coordinates.', &
'     * Embedding the routine of interest into a MODULE, and USEing that', &
'       module in the procedure that calls the routine. This creates', &
'       order dependencies when compiling code, generally resulting in', &
'       requiring such tools as Makefiles but has many other benefits,', &
'       such as most easily allowing for generic versions of the routines,', &
'       This is the way we used here. An example of use is provided as', &
'       the test program tstvalnth.f90.', &
'', &
'A WORD OF APOLOGY', &
'', &
'   When one looks at the description of a sorting algorithm, the', &
'   process seems pretty simple, and can usually be held in 10 to 20', &
'   lines of pseudo-code. But if one wants an optimized program, one', &
'   takes this simple implementation, and looks for redundant operations,', &
'   investigates runs with sample data sets with a profiling tool, and', &
'   is led to duplicate code with slight modifications rather than use', &
'   tests in inner loops, to process differently the first and the last', &
'   iterations, or to take into account some special cases that are only', &
'   special in that they can be done faster.', &
'', &
'   In the end, the number of lines of source code may be', &
'   multiplied tenfold, and the readability decreased in a similar', &
'   proportion. Unfortunately, this is the price to pay for speed of', &
'   execution. It was that way when I started programming more than 20', &
'   years ago, and I have forsaken any hope that it might become otherwise', &
'   before I return to dust. So please accept my apologies that this code', &
'   is often complex and difficult to read.', &
'', &
'AUTHORS', &
'   Michel Olagnon IFREMER Brest / Michel.Olagnon@ifremer.fr', &
'', &
'   2000- 2013/11/06', &
'MAINTAINERS', &
'   John S. Urban, 2022-04-16', &
'', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="M_orderpack"

call select()


case('5','occurrences')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   occurrences(3f) - [orderpack:MULTIPLICITY] Give the multiplicity for each', &
'                array value (number of times that it appears in the array)', &
'', &
'SYNOPSIS', &
'    Subroutine Occurrences (INVALS, IMULT)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IMULT(:)', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'    OCCURRENCES(3f) Gives, for each array element, its multiplicity', &
'    (number of times that it appears in the array).', &
'', &
'    Internally, the number of times that a value appears in the array is', &
'    computed by using inverse ranking, counting for each rank the number', &
'    of values that "collide" to this rank, and returning this sum to', &
'    the locations in the original set. It uses subroutine RANK_ORDERS(3f).', &
'', &
'OPTIONS', &
'    INVALS      input array', &
'    IMULT      array containing how often the value in INVALS', &
'               appears in INVALS', &
'', &
'EXAMPLES', &
'Sample program:', &
'', &
'     program demo_occurrences', &
'     use M_orderpack, only : occurrences', &
'     ! determine how many times each value appears in an input array', &
'     implicit none', &
'     character(len=*),parameter    :: g=''(*(g0,1x))''', &
'     character(len=20),allocatable :: strings(:)', &
'     integer,allocatable           :: cindx(:)', &
'     integer                       :: csz', &
'     integer                       :: i', &
'        ! each name appears the number of times its name represents', &
'        strings= [ character(len=20) ::                           &', &
'        & ''two  '',  ''four '', ''three'', ''five'',   ''five'',           &', &
'        & ''two  '',  ''four '', ''three'', ''five'',   ''five'',           &', &
'        & ''four '',  ''four '', ''three'', ''one  '',  ''five'']', &
'        csz=size(strings)', &
'        if(allocated(cindx))deallocate(cindx)', &
'        allocate(cindx(csz))', &
'        call occurrences(strings,cindx)', &
'        write(*,g)(trim(strings(i)),i=1,csz)', &
'        write(*,g)cindx', &
'     end program demo_occurrences', &
'', &
'Results:', &
'', &
' two four three five five two four three five five four four three one five', &
' 2   4    3     5    5    2   4    3     5    5    4    4    3     1   5', &
'', &
'AUTHOR', &
'   Michel Olagnon, Mar 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="occurrences"

call select()


case('6','orderloc')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   orderloc(3f) - [orderpack:FRACTILE] Return INDEX of Nth ordered value of', &
'                array, or "fractile of order N/SIZE(array)" (QuickSort-like)', &
'', &
'SYNOPSIS', &
'    Function OrderLoc (INVALS, NORD)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer                             :: orderloc', &
'      Integer, Intent (In)                :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'   orderloc(3f) returns the index of NORDth value of INVALS, i.e. the', &
'   fractile of order NORD/SIZE(INVALS).', &
'', &
'   That is, the result is the same as sorting the array first and then', &
'   returning the value INVALS(NORD).', &
'', &
'   Internally orderloc(3f) uses a pivoting strategy such as the one', &
'   of finding the median based on the quicksort algorithm, but we skew', &
'   the pivot choice to try to bring it to NORD as fast as possible. It', &
'   uses two temporary arrays, where it stores the indices of the values', &
'   smaller than the pivot (ILOWT), and the indices of values larger than', &
'   the pivot that we might still need later on (IHIGT). It iterates', &
'   until it can bring the number of values in ILOWT to exactly NORD,', &
'   and then finds the maximum of this set.', &
'', &
'OPTIONS', &
'    INVALS      array to search', &
'    NORD       indicates the Nth ordered value to search for', &
'', &
'RETURNS', &
'    orderloc     the index of INVALS() that contains the requested value', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_orderloc', &
'   ! find Nth lowest ordered value in an array without sorting entire array', &
'   use M_orderpack, only : orderloc', &
'   use M_orderpack, only : medianloc', &
'   implicit none', &
'   integer,allocatable :: iarr(:)', &
'   character(len=*),parameter :: list= ''(*(g0:,", "))'',sp=''(*(g0,1x))''', &
'   integer :: i', &
'   integer :: indx', &
'      iarr=[80,70,30,40,50,60,20,10,0,-100]', &
'      print list, ''ORIGINAL:'',iarr', &
'      ! like minloc(3f) and maxloc(3f)', &
'      print sp,''minloc'',orderloc(iarr,1),                minloc(iarr)', &
'      print sp,''maxloc'',orderloc(iarr,size(iarr)),       maxloc(iarr)', &
'      ! can find median', &
'      call medianloc(iarr,indx)', &
'      print sp,''median'',orderloc(iarr,(size(iarr)+1)/2), indx', &
'      ! but more general so can find location of the Nth lowest value ...', &
'      !', &
'      ! sort the hard way, finding location of Nth value one at a time', &
'      do i=1,size(iarr)', &
'         write(*,sp,advance=''no'') iarr(orderloc(iarr,i))', &
'      enddo', &
'      print *', &
'   contains', &
'   subroutine printme(n)', &
'   integer,intent(in) :: n', &
'   integer :: ii', &
'      ii=orderloc(iarr,n)', &
'      print sp,''nord='',n,'' index='',ii,'' fractile='',iarr(ii)', &
'   end subroutine printme', &
'   end program demo_orderloc', &
'', &
'  Results:', &
'', &
'   ORIGINAL:, 80, 70, 30, 40, 50, 60, 20, 10, 0, -100', &
'   minloc 10 10', &
'   maxloc 1 1', &
'   median 3 3', &
'   -100 0 10 20 30 40 50 60 70 80', &
'', &
'AUTHOR', &
'   Michel Olagnon - Aug. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="orderloc"

call select()


case('7','orderval')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   orderval(3f) - [orderpack:FRACTILE] Return VALUE of Nth ordered element of', &
'                array, or "fractile of order N/SIZE(array)". (QuickSort-like)', &
'', &
'SYNOPSIS', &
'    Function OrderVal (INVALS, NORD)', &
'', &
'     ${TYPE} (Kind=${KIND}), Intent (In) :: INVALS(:)', &
'     ${TYPE} (Kind=${KIND})              :: orderval', &
'     Integer, Intent (In)                :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'  ORDERVAL(3f) returns the NORDth (ascending order) value of INVALS,', &
'  i.e. the fractile of order NORD/SIZE(INVALS).', &
'', &
'  Internally, this subroutine simply calls ORDERLOC(3f).', &
'', &
'  This routine uses a pivoting strategy such as the one of finding the', &
'  median based on the QuickSort algorithm, but we skew the pivot choice', &
'  to try to bring it to NORD as fast as possible. It uses two temporary', &
'  arrays, where it stores the indices of the values smaller than the', &
'  pivot (ILOWT), and the indices of values larger than the pivot that we', &
'  might still need later on (IHIGT). It iterates until it can bring the', &
'  number of values in ILOWT to exactly NORD, and then finds the maximum', &
'  of this set.', &
'', &
'OPTIONS', &
'    INVALS    array to search', &
'    NORD     Nth lowest value to find', &
'RETURNS', &
'    orderval   Nth lowest value', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_orderval', &
'   !  Return value of Nth lowest value of array', &
'   use M_orderpack, only : orderval', &
'   implicit none', &
'   character(len=*),parameter :: list= ''(*(g0:,", "))''', &
'   character(len=*),parameter :: sp=''(*(g0,1x))''', &
'   real,parameter ::  INVALS(*)=[1.1,20.20,3.3,10.10,5.5,4.4,2.2]', &
'   integer :: i', &
'   integer :: imiddle', &
'      write(*,list) ''ORIGINAL:'',INVALS', &
'      ! can return the same values as intrinsics minval(3f) and maxval(3f)', &
'      print sp, ''minval'',orderval(INVALS,1),          minval(INVALS)', &
'      print sp, ''maxval'',orderval(INVALS,size(INVALS)), maxval(INVALS)', &
'      ! but more generally it can return the Nth lowest value.', &
'      print sp,''nord='',4, '' fractile='',orderval(INVALS,4)', &
'      ! so a value at the middle would be', &
'      imiddle=(size(INVALS)+1)/2', &
'      print sp,''median'',orderval(INVALS,imiddle)', &
'      ! sorting the hard way', &
'      do i=1,size(INVALS)', &
'         write(*,list)i,orderval(INVALS,i)', &
'      enddo', &
'   end program demo_orderval', &
'', &
'  Results:', &
'', &
'   ORIGINAL:, 1.1000, 20.200, 3.300, 10.100, 5.500, 4.400, 2.200', &
'   minval 1.100 1.100', &
'   maxval 20.200 20.200', &
'   nord= 4  fractile= 4.400', &
'   median 4.400', &
'   1, 1.100', &
'   2, 2.200', &
'   3, 3.300', &
'   4, 4.400', &
'   5, 5.500', &
'   6, 10.100', &
'   7, 20.200', &
'', &
'AUTHOR', &
'   Michel Olagnon - Aug. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="orderval"

call select()


case('8','orderval_special')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   orderval_special(3f) - [orderpack:FRACTILE] Return VALUE of Nth', &
'                          ordered element of array, or "fractile of', &
'                          order N/SIZE(array)" (InsertSort-like)', &
'', &
'SYNOPSIS', &
'    Function Orderval_Special (INVALS, INORD)', &
'', &
'     ${TYPE} (Kind=${KIND}), Intent (In) :: INVALS(:)', &
'     Integer, Intent (In)                :: INORD', &
'     ${TYPE} (Kind=${KIND})              :: orderval_special', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'   o Real(kind=real32)', &
'   o Real(kind=real64)', &
'   o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'   ORDERVAL_SPECIAL(3f) returns the INORDth lowest value of INVALS(),', &
'   i.e. the fractile of order INORD/SIZE(INVALS).', &
'', &
'   Internally, This subroutine uses an insertion sort, limiting insertion', &
'   to the first INORD values and even less when one can know that the', &
'   value that is considered will not be the INORDth.', &
'', &
'   An insertion sort is very fast when INORD is very small', &
'   (2-5). Additionally, internally it requires only a work array of size', &
'   INORD (and type of INVALS),', &
'', &
'   But worst case behavior can happen fairly probably (e.g., initially', &
'   inverse sorted). Therefore, in many cases, the refined QuickSort', &
'   method is faster.', &
'', &
'   so ORDERVAL_SPECIAL(3f) should be used when INORD is small and INVALS', &
'   is likely to be a random array, otherwise consider using ORDERLOC(3f)', &
'   or ORDERVAL(3f).', &
'', &
'OPTIONS', &
'    INVALS              input array of values', &
'    INORD                specify Nth value of sorted INVALS array to', &
'                        return, from 1 to size(INVALS).', &
'RETURNS', &
'    ORDERVAL_SPECIAL    returned value', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_orderval_special', &
'   ! return Nth ordered value of an array', &
'   use M_orderpack, only : orderval_special, medianval', &
'   implicit none', &
'   character(len=*),parameter :: list= ''(*(g0:,", "))'',sp=''(*(g0,1x))''', &
'   integer,allocatable :: iarr(:)', &
'   integer :: i', &
'      iarr=[80,70,30,40,-50,60,20,10]', &
'      print sp, ''ORIGINAL:'',iarr', &
'      ! can return the same values as intrinsics minval(3f) and maxval(3f)', &
'      print sp, ''minval'',orderval_special(iarr,1),          minval(iarr)', &
'      print sp, ''maxval'',orderval_special(iarr,size(iarr)), maxval(iarr)', &
'      ! but more generally it can return the Nth lowest value.', &
'      print sp, ''median'',orderval_special(iarr,(size(iarr+1))/2), &', &
'      & medianval(iarr)', &
'      ! so only Nth ordered value can be found', &
'      print sp,''inord='',3, '' fractile='',orderval_special(iarr,3)', &
'      ! sorting the hard way', &
'      print sp, ''ORIGINAL:'',iarr', &
'      do i=1,size(iarr)', &
'         write(*,list)i,orderval_special(iarr,i)', &
'      enddo', &
'      print *', &
'   end program demo_orderval_special', &
'', &
'  Results:', &
'', &
'   ORIGINAL: 80 70 30 40 -50 60 20 10', &
'   minval -50 -50', &
'   maxval 80 80', &
'   median 30 30', &
'   inord= 3  fractile= 20', &
'   ORIGINAL: 80 70 30 40 -50 60 20 10', &
'   1, -50', &
'   2, 10', &
'   3, 20', &
'   4, 30', &
'   5, 40', &
'   6, 60', &
'   7, 70', &
'   8, 80', &
'', &
'SEE ALSO', &
'', &
'   ORDERLOC(3f), ORDERVAL(3f)', &
'', &
'AUTHOR', &
'   Michel Olagnon - Aug. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="orderval_special"

call select()


case('9','perturb')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   perturb(3f) - [orderpack:PERMUTATION] generate a random permutation', &
'                 of an array leaving elements close to initial locations', &
'', &
'SYNOPSIS', &
'    Subroutine Perturb (INOUTVALS, CLOSENESS)', &
'', &
'     ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)', &
'     Real, Intent (In)                      :: CLOSENESS', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'  Shuffle the array INOUTVALS randomly, leaving elements close to their', &
'  initial locations.', &
'', &
'  Nearbyness is controlled by CLOSENESS. The relative proportion of', &
'  initial order and random order is defined as 1-CLOSENESS / CLOSENESS,', &
'  thus when CLOSENESS = 0, there is no change in the order whereas the', &
'  new order is fully random when CLOSENESS = 1.', &
'', &
'  Note this differs from adding random noise to the values. The original', &
'  values remain unchanged, their order is just perturbed.', &
'', &
'  Internally, the routine creates a real array of the indices of', &
'  INOUTVALS() perturbed by random values that are based on the size', &
'  of CLOSENESS. The new array is then ranked using RANK(3f) and the', &
'  resulting index is used to permute the input array.', &
'', &
'OPTIONS', &
'    INOUTVALS   Array of values to perturb.', &
'    CLOSENESS   Proportion of closeness, constrained to the range 0.0(no', &
'                change) to 1.0(fully random).', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_perturb', &
'   ! generate a random perturbation of an array', &
'   use M_orderpack, only : perturb', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   character(len=*),parameter :: list= ''(*(g0:,", "))''', &
'   integer,allocatable :: xout(:,:)', &
'   integer          :: isz, i, j', &
'   isz=200', &
'      ! randomly perturb location of values', &
'      !', &
'      ! make an array with three initially identical rows of', &
'      ! values perturbed by different amounts', &
'      if(allocated(xout))deallocate(xout)', &
'      allocate(xout(3,isz))', &
'      xout(1,:)=[(i,i=isz,1,-1)]*10', &
'      xout(2,:)=xout(1,:)', &
'      xout(3,:)=xout(1,:)', &
'      ! perturb each row a different amount', &
'      call perturb(xout(1,:),0.0)', &
'      call perturb(xout(2,:),0.1)', &
'      call perturb(xout(3,:),1.0)', &
'      ! show values', &
'      write(*,''(a)'')''count    unchanged  perturbed  random''', &
'      do i=1,size(xout,dim=2)', &
'         write(*,''(*(i8,1x))'')i,xout(:,i)', &
'      enddo', &
'   char: block', &
'   character(len=:),allocatable :: cdont(:)', &
'      cdont=[character(len=20) :: ''a'', ''be'', ''car'', ''dam'',''fan'',''gas'',''egg'']', &
'      isz=size(cdont)', &
'      write(*,g)''Original.................:'',(trim(cdont(i)),i=1,isz)', &
'      call perturb(cdont,1.0)', &
'      write(*,g)''Perturbed ...............:'',(trim(cdont(i)),i=1,isz)', &
'      write(*,g)', &
'   endblock char', &
'', &
'   end program demo_perturb', &
'', &
'  Results:', &
'', &
'   count    unchanged  perturbed  random', &
'          1     2000     1980       80', &
'          2     1990     1990      580', &
'          3     1980     1890     1690', &
'          4     1970     1900     1340', &
'          5     1960     1920     1260', &
'          6     1950     1950     1220', &
'          7     1940     1880      160', &
'          8     1930     1960     1620', &
'          9     1920     1860      540', &
'         10     1910     1930     1300', &
'         11     1900     1940     1190', &
'          .        .        .        .', &
'          .        .        .        .', &
'          .        .        .        .', &
'          .        .        .        .', &
'        189      120       80     1200', &
'        190      110      150      800', &
'        191      100      120     1430', &
'        192       90      170     1410', &
'        193       80      140      370', &
'        194       70       90     1720', &
'        195       60       10      830', &
'        196       50      100     1670', &
'        197       40       20      470', &
'        198       30       70     1020', &
'        199       20       60     1540', &
'        200       10       30     1810', &
'   Original.................: a be car dam fan gas egg', &
'   Perturbed ...............: a be gas dam fan car egg', &
'', &
'AUTHOR', &
'   Michel Olagnon, 2000-2012', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="perturb"

call select()


case('10','prank')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   prank(3f) - [orderpack:RANK:PARTIAL] partially ranks an array', &
'               (QuickSort-like)', &
'', &
'SYNOPSIS', &
'    Subroutine Prank (INVALS, IRNGT, NORD)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IRNGT(:)', &
'      Integer, Intent (In)                :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'   Partially ranks array INVALS(), returning indices of the requested', &
'   number of elements (NORD) in index array IRNGT(), where NORD is the', &
'   order ( aka. the number of sorted elements requested).', &
'', &
'   This routine is refined for speed, and uses a pivoting strategy such', &
'   as the one of finding the median based on the quicksort algorithm,', &
'   but we skew the pivot choice to try to bring it to NORD as fast as', &
'   possible. It uses two temporary arrays, where it stores the indices of', &
'   the values smaller than the pivot (ILOWT), and the indices of values', &
'   larger than the pivot that we might still need later on (IHIGT). It', &
'   iterates until it can bring the number of values in ILOWT to exactly', &
'   NORD, and then uses an insertion sort to rank this set, since it is', &
'   supposedly small.', &
'', &
'OPTIONS', &
'    INVALS      array to rank the elements of', &
'    IRNGT      returned ranks', &
'    NORD       number of rank values to return', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_prank', &
'   ! partially rank array', &
'   use,intrinsic :: iso_fortran_env, only : int32, real32, real64', &
'   use M_orderpack, only : prank', &
'   implicit none', &
'   integer,parameter :: ivals=300', &
'   real(kind=real32) :: valsr(2000)', &
'   real(kind=real32) :: out(ivals)', &
'   integer           :: indx(2000)', &
'   integer           :: i', &
'      call random_seed()', &
'      call random_number(valsr)', &
'      valsr=valsr*1000000.0-500000.0', &
'      call prank(valsr,indx,ivals)', &
'      out=valsr(indx(:ivals))', &
'      do i=1,ivals-1', &
'         if (out(i+1).lt.out(i))then', &
'            write(*,*)''not sorted''', &
'            stop 1', &
'         endif', &
'      enddo', &
'      write(*,*)''random array now sorted''', &
'   end program demo_prank', &
'', &
'  Results:', &
'', &
'    random array now sorted', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="prank"

call select()


case('11','prank_basic')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   prank_basic(3f) - [orderpack:RANK:PARTIAL] partially ranks an array', &
'                (QuickSort)', &
'', &
'SYNOPSIS', &
'    Subroutine Prank_Basic (INVALS, IRNGT, NORD)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IRNGT(:)', &
'      Integer, Intent (In)                :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   creates index IRNGT() which partially ranks input array INVALS(),', &
'   up to order NORD.', &
'', &
'   This version is not optimized for performance, and is thus not as', &
'   difficult to read as some other ones.', &
'', &
'   Internally this routine uses a pivoting strategy such as the one used', &
'   in finding the median based on the QuickSort algorithm. It uses a', &
'   temporary array, where it stores the partially ranked indices of the', &
'   values. It iterates until it can bring the number of values lower', &
'   than the pivot to exactly NORD, and then uses an Insertion-Sort to', &
'   rank this set, since it is supposedly small.', &
'', &
'OPTIONS', &
'    INVALS      array to partially rank', &
'    IRNGT      array to hold indices of ranked elements', &
'    NORD       number of elements to rank', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_prank', &
'   ! create index to lowest N values in input array in ascending order', &
'   use,intrinsic :: iso_fortran_env, only : int32, real32, real64', &
'   use M_orderpack, only : prank_basic', &
'   implicit none', &
'   real(kind=real32) :: valsr(2000)', &
'   integer           :: indx(2000)', &
'   integer           :: i', &
'   real,allocatable  :: results(:)', &
'      ! create some random data', &
'      call random_seed()', &
'      call random_number(valsr)', &
'      valsr=valsr*1000000.0-500000.0', &
'      ! get 300 lowest values sorted', &
'      call prank_basic(valsr,indx,300)', &
'      !', &
'      results=valsr(indx(:300))', &
'      ! check if sorted', &
'      do i=1,300-1', &
'         if (results(i+1).lt.results(i))then', &
'            write(*,*)''ERROR: not sorted''', &
'            stop 1', &
'         endif', &
'      enddo', &
'      write(*,*)''random array now sorted''', &
'   end program demo_prank', &
'', &
'  Results:', &
'', &
'    random array now sorted', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="prank_basic"

call select()


case('12','prank_decreasing')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   prank_decreasing(3f) - [orderpack:RANK:PARTIAL] partially ranks an', &
'                         array in DECREASING order.', &
'', &
'SYNOPSIS', &
'    Subroutine Prank_Decreasing (INVALS, IRNGT, NORD)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IRNGT(:)', &
'      Integer, Intent (In)                :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'   Same as PRANK(3f), but in decreasing order.', &
'', &
'   PRANK_DECREASING(3f) partially ranks input array INVALS() in decreasing', &
'   order up to order NORD, placing the indices pointing to the selected', &
'   values into IRNGT().', &
'', &
'   Internally this routine uses a pivoting strategy such as the one of', &
'   finding the median based on the quicksort algorithm, but we skew the', &
'   pivot choice to try to bring it to NORD as fast as possible. It uses', &
'   two temporary arrays, where it stores the indices of the values larger', &
'   than the pivot (IHIGT), and the indices of values smaller than the', &
'   pivot that we might still need later on (ILOWT). It iterates until', &
'   it can bring the number of values in IHIGT to exactly NORD, and then', &
'   uses an insertion sort to rank this set, since it is supposedly small.', &
'', &
'OPTIONS', &
'    INVALS      Array to rank', &
'    IRNGT      returned rank array, indicating order of values in', &
'               INVALS from largest to smallest', &
'    NORD       number of values to return in IRNGT', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_prank_decreasing', &
'   ! create index to lowest N values in input array in decreasing order', &
'   use M_orderpack, only : prank_decreasing', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer,allocatable :: INVALS(:)', &
'   integer,allocatable :: irngt(:)', &
'   integer :: nord', &
'   INVALS=[10,5,7,1,4,5,6,8,9,10,1]', &
'   nord=5', &
'   allocate(irngt(nord))', &
'      write(*,g)''ORIGINAL:'',INVALS', &
'      call prank_decreasing(INVALS,irngt,nord)', &
'      write(*,g)''NUMBER OF INDICES TO RETURN:'',nord', &
'      write(*,g)''RETURNED INDICES:'',irngt', &
'      write(*,g)nord,''MAXIMUM VALUES:'',INVALS(irngt(:nord))', &
'   end program demo_prank_decreasing', &
'', &
'  Results:', &
'', &
'   ORIGINAL: 10 5 7 1 4 5 6 8 9 10 1', &
'   NUMBER OF INDICES TO RETURN: 5', &
'   RETURNED INDICES: 1 10 9 8 7', &
'   5 MAXIMUM VALUES: 10 10 9 8 6', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2011', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="prank_decreasing"

call select()


case('13','prank_special')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   prank_special(3f) - [orderpack:RANK:PARTIAL] partially ranks an array in', &
'                ASCENDING order (Insertion Sort)', &
'', &
'SYNOPSIS', &
'    Subroutine Prank_Special (INVALS, IRNGT, NORD)', &
'', &
'     ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'     Integer, Intent (Out)               :: IRNGT(:)', &
'     Integer, Intent (In)                :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'   o Real(kind=real32)', &
'   o Real(kind=real64)', &
'   o Integer(kind=int32)', &
'   o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   Partially ranks INVALS(). Returns IRNGT(1:NORD) filled with the', &
'   indices of the lowest values in the array INVALS. More technically,', &
'   it does a partial ranking of the array INVALS of order NORD.', &
'', &
'   NORD is restricted to the range 1 to size(IRNGT).', &
'', &
'   Internally, this version is not optimized for performance, and is', &
'   thus not as difficult to read as some other ones.', &
'', &
'   The subroutine uses an insertion sort, limiting insertion to the', &
'   first NORD values. It does not use any work array and is fastest when', &
'   NORD is very small (2-5). but worst case behavior can easily happen', &
'   (ie. if INVALS initially is inverse sorted). Therefore, In many cases,', &
'   the refined Quicksort method is faster.', &
'', &
'OPTIONS', &
'    INVALS   array to partially sort', &
'    NORD    number of indices to return, restricted to 1 to size(IRNGT)', &
'RETURNS', &
'    IRNGT   indices of requested number (NORD) of lowest values in INVALS', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_prank_special', &
'   ! partially rank N lowest values in an array', &
'   use M_orderpack, only : prank_special', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer,allocatable :: INVALS(:)', &
'   integer,allocatable :: irngt(:)', &
'   integer :: nord', &
'   INVALS=[10,5,7,1,4,5,6,8,9,10,1]', &
'   nord=5', &
'   allocate(irngt(nord))', &
'      write(*,g)''ORIGINAL:'',INVALS', &
'      call prank_special(INVALS,irngt,nord)', &
'      write(*,g)''NUMBER OF INDICES TO RETURN:'',nord', &
'      write(*,g)''RETURNED INDICES:'',irngt', &
'      write(*,g)nord,''SMALLEST VALUES:'',INVALS(irngt(:nord))', &
'   end program demo_prank_special', &
'', &
'  Results:', &
'', &
'   ORIGINAL: 10 5 7 1 4 5 6 8 9 10 1', &
'   NUMBER OF INDICES TO RETURN: 5', &
'   RETURNED INDICES: 4 11 5 2 6', &
'   5 SMALLEST VALUES: 1 1 4 5 5', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="prank_special"

call select()


case('14','prank_unique')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   prank_unique(3f) - [orderpack:RANK:PARTIAL:UNIQUE] partially ranks', &
'                      an array removing duplicates', &
'', &
'SYNOPSIS', &
'    Subroutine Prank_Unique (INVALS, IRNGT, NORD)', &
'', &
'     ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'     Integer, Intent (Out)               :: IRNGT(:)', &
'     Integer, Intent (InOut)             :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'   Partially rank INVALS() up to order NORD at most, removing duplicate', &
'   entries.', &
'', &
'   Internally, this routine uses a pivoting strategy such as the one of', &
'   finding the median based on the quicksort algorithm, but we skew the', &
'   pivot choice to try to bring it to NORD as quickly as possible. It', &
'   uses two temporary arrays, where it stores the indices of the values', &
'   smaller than the pivot (ILOWT), and the indices of values larger', &
'   than the pivot that we might still need later on (IHIGT). It iterates', &
'   until it can bring the number of values in ILOWT to exactly NORD, and', &
'   then uses an insertion sort to rank this set, since it is supposedly', &
'   small. At all times, the NORD first values in ILOWT correspond to', &
'   distinct values of the input array.', &
'', &
'', &
'OPTIONS', &
'    INVALS      array to partially sort', &
'    IRNGT      indices returned that point to lowest values', &
'    NORD       number of sorted values to determine before', &
'               eliminating duplicates', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_prank_unique', &
'   ! ranks array, removing duplicates', &
'   use M_orderpack, only : prank_unique', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer,allocatable :: INVALS(:)', &
'   integer,allocatable :: irngt(:)', &
'   integer :: nord', &
'   !', &
'   write(*,g)''If enough values are unique, will return NORD indices''', &
'   if(allocated(irngt))deallocate(irngt)', &
'   INVALS=[10,5,7,1,4,5,6,8,9,10,1]', &
'   nord=5', &
'   allocate(irngt(nord))', &
'   call printme()', &
'   !', &
'   !BUG!write(*,g)''If not enough values are unique, will change NORD''', &
'   !BUG!INVALS=[-1,0,-1,0,-1,0,-1]', &
'   !BUG!nord=5', &
'   !BUG!if(allocated(irngt))deallocate(irngt)', &
'   !BUG!allocate(irngt(nord))', &
'   !BUG!call printme()', &
'   contains', &
'   subroutine printme()', &
'      write(*,g)''ORIGINAL:'',INVALS', &
'      write(*,g)''NUMBER OF INDICES TO SORT:'',nord', &
'      call prank_unique(INVALS,irngt,nord)', &
'      write(*,g)''NUMBER OF INDICES RETURNED:'',nord', &
'      write(*,g)''RETURNED INDICES:'',irngt(:nord)', &
'      write(*,g)nord,''SMALLEST UNIQUE VALUES:'',INVALS(irngt(:nord))', &
'   end subroutine', &
'   end program demo_prank_unique', &
'', &
'  Results:', &
'', &
'   If enough values are unique, will return NORD indices', &
'   ORIGINAL: 10 5 7 1 4 5 6 8 9 10 1', &
'   NUMBER OF INDICES TO SORT: 5', &
'   NUMBER OF INDICES RETURNED: 5', &
'   RETURNED INDICES: 11 5 2 7 3', &
'   5 SMALLEST UNIQUE VALUES: 1 4 5 6 7', &
'   If not enough values are unique, will change NORD', &
'   ORIGINAL: -1 0 -1 0 -1 0 -1', &
'   NUMBER OF INDICES TO SORT: 5', &
'   NUMBER OF INDICES RETURNED: 2', &
'   RETURNED INDICES: 1 2', &
'   2 SMALLEST UNIQUE VALUES: -1 0', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="prank_unique"

call select()


case('15','psort')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   psort(3f) - [orderpack:SORT:PARTIAL] partially sorts an array', &
'               (Insertion Sort, generally for small or nearly sorted', &
'               arrays)', &
'', &
'SYNOPSIS', &
'    Subroutine Psort (INOUTVALS, NORD)', &
'', &
'     ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)', &
'     Integer, Intent (In)                   :: NORD', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   PSORT(3f) partially sorts INOUTVALS, bringing the NORD lowest values', &
'   to the beginning of the array.', &
'', &
'   Internally, this subroutine uses an insertion sort, limiting insertion', &
'   to the first NORD values. It does not use any work array and is faster', &
'   when NORD is very small (2-5), but worst case behavior can happen', &
'   fairly probably (initially inverse sorted). Therefore, in many cases,', &
'   the refined quicksort method is faster.', &
'', &
'OPTIONS', &
'    INOUTVALS      The array to partially sort', &
'    NORD       number of sorted values to return.', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_psort', &
'   ! partially sort an array', &
'   use M_orderpack, only : psort', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer :: nord', &
'', &
'   int: block', &
'      integer,allocatable :: ia(:)', &
'      ia=[10,5,7,1,4,5,6,8,9,10,1]', &
'      nord=5', &
'      write(*,g)''Original.................:'',ia', &
'      call psort(ia,nord)', &
'      write(*,g)''Number of indices to sort:'',nord', &
'      write(*,g)nord,''Lowest values..........:'',ia(:nord)', &
'      write(*,g)''Entire array.............:'',ia', &
'      write(*,g)', &
'   endblock int', &
'   char: block', &
'      character(len=:),allocatable :: ca(:)', &
'      integer :: i', &
'      ca=[character(len=20) :: ''fan'',''a'',''car'',''be'',''egg'',''dam'',''gas'']', &
'      nord=3', &
'      write(*,g)''Original.................:'',(trim(ca(i)),i=1,size(ca))', &
'      call psort(ca,nord)', &
'      write(*,g)''Number of indices to sort:'',nord', &
'      write(*,g)nord,''Lowest values..........:'',(trim(ca(i)),i=1,nord)', &
'      write(*,g)''Entire array.............:'',(trim(ca(i)),i=1,size(ca))', &
'      write(*,g)', &
'   endblock char', &
'', &
'   end program demo_psort', &
'', &
'  Results:', &
'', &
'   Original.................: 10 5 7 1 4 5 6 8 9 10 1', &
'   Number of indices to sort: 5', &
'   5 Lowest values..........: 1 1 4 5 5', &
'   Entire array.............: 1 1 4 5 5 10 7 8 9 10 6', &
'', &
'   Original.................: fan a car be egg dam gas', &
'   Number of indices to sort: 3', &
'   3 Lowest values..........: a be car', &
'   Entire array.............: a be car fan egg dam gas', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="psort"

call select()


case('16','rank')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   rank(3f) - [orderpack:RANK] produces an INDEX that sorts an input', &
'              array (optimized merge-sort)', &
'', &
'SYNOPSIS', &
'    Subroutine Rank (INVALS, IRNGT)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IRNGT(:)', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'    RANK(3f) ranks an input array; i.e. it produces an index of the input', &
'    array elements that can order the input array in ascending order.', &
'', &
'    The ranks can be used to sort the input array, or other associated arrays', &
'    or components of user types.', &
'', &
'    Internally, it uses an optimized and modified version of merge-sort.', &
'    For performance reasons, the first two passes are taken out of the', &
'    standard loop, and use dedicated coding.', &
'', &
'OPTIONS', &
'    INVALS      The array to sort', &
'    IRNGT      The rank index returned', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_rank', &
'   ! create an index that can order an array in ascending order', &
'   use M_orderpack, only : rank', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer,parameter             :: dp=kind(0.0d0)', &
'   integer,parameter             :: isz=10000', &
'   real(kind=dp)                 :: dd(isz)', &
'   real(kind=dp)                 :: pp', &
'   integer                       :: indx(isz)', &
'   integer                       :: i,j,k', &
'   character(len=:),allocatable  :: strings(:)', &
'   integer,allocatable           :: cindx(:)', &
'      ! make some random numbers', &
'      call random_seed()', &
'      call random_number(dd)', &
'      dd=dd-0.50_dp', &
'      k=int(log(huge(0.0_dp))/log(2.0_dp))-1', &
'      do i=1,isz', &
'         call random_number(pp)', &
'         j=floor((k+1)*pp)', &
'         dd(i)=dd(i)*(2.0_dp**j)', &
'      enddo', &
'      ! rank the numeric data', &
'      call rank(dd,indx)', &
'      ! check order', &
'      do i=1,isz-1', &
'         if(dd(indx(i)).gt.dd(indx(i+1)))then', &
'            write(*,g)''ERROR: data not sorted i='',i,''index='',indx(i), &', &
'            & ''values '',dd(indx(i)),dd(indx(i+1))', &
'            stop 1', &
'         endif', &
'      enddo', &
'      ! sort data using rank values', &
'      dd=dd(indx)', &
'      write(*,g)''sorted '',isz,''values''', &
'      write(*,g)''from'',dd(1),''to'',dd(isz)', &
'      write(*,*)minval(dd).eq.dd(1)', &
'      write(*,*)maxval(dd).eq.dd(isz)', &
'      write(*,*)minloc(dd).eq.1', &
'      write(*,*)maxloc(dd).eq.isz', &
'      ! do a character sort', &
'      strings= [ character(len=20) ::                               &', &
'      & ''red'',    ''green'', ''blue'', ''yellow'', ''orange'',   ''black'', &', &
'      & ''white'',  ''brown'', ''gray'', ''cyan'',   ''magenta'',           &', &
'      & ''purple'']', &
'      if(allocated(cindx))deallocate(cindx);allocate(cindx(size(strings)))', &
'', &
'      write(*,''(a,8(a:,","))'')''BEFORE '',&', &
'              & (trim(strings(i)),i=1,size(strings))', &
'', &
'      call rank(strings,cindx)', &
'', &
'      write(*,''(a,8(a:,","))'')''SORTED '',&', &
'              & (trim(strings(cindx(i))),i=1,size(strings))', &
'', &
'      strings=strings(cindx) ! sort the array using the rank index', &
'', &
'      do i=1,size(strings)-1', &
'         if(strings(i).gt.strings(i+1))then', &
'            write(*,*)''Error in sorting strings a-z''', &
'         endif', &
'      enddo', &
'   end program demo_rank', &
'', &
'  Results:', &
'', &
'   sorted  10000 values', &
'   from -.4206770472235745E+308 to .3500810518521505E+308', &
'    T', &
'    T', &
'    T', &
'    T', &
'   BEFORE red,green,blue,yellow,orange,black,white,brown,', &
'   gray,cyan,magenta,purple', &
'   SORTED black,blue,brown,cyan,gray,green,magenta,orange,', &
'   purple,red,white,yellow', &
'', &
'AUTHOR', &
'   Michel Olagnon, 2000-2012', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="rank"

call select()


case('17','rank_basic')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   rank_basic(3f) - [orderpack:RANK] create an INDEX that defines the', &
'                    order of array sorted in ascending order (basic', &
'                    merge-sort)', &
'', &
'SYNOPSIS', &
'    Subroutine Rank_Basic (INVALS, IRNGT)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IRNGT(:)', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   Ranks array INVALS, filling array IRNGT with sorted indices.', &
'', &
'   It uses a basic merge-sort.', &
'', &
'   This version is not optimized for performance, and is thus', &
'   not as difficult to read as some other ones.', &
'', &
'   It uses Merge-sort.', &
'', &
'OPTIONS', &
'    INVALS      input array to rank', &
'    IRNGT      returned rank array', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_rank_basic', &
'   ! create an index that can order an array in ascending order', &
'   use M_orderpack, only : rank_basic', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer,parameter             :: dp=kind(0.0d0)', &
'   integer,parameter             :: isz=10000', &
'   real(kind=dp)                 :: dd(isz)', &
'   real(kind=dp)                 :: pp', &
'   integer                       :: indx(isz)', &
'   integer                       :: i,j,k', &
'   character(len=:),allocatable  :: strings(:)', &
'   integer,allocatable           :: cindx(:)', &
'      ! make some random numbers', &
'      call random_seed()', &
'      call random_number(dd)', &
'      dd=dd-0.50_dp', &
'      k=int(log(huge(0.0_dp))/log(2.0_dp))-1', &
'      do i=1,isz', &
'         call random_number(pp)', &
'         j=floor((k+1)*pp)', &
'         dd(i)=dd(i)*(2.0_dp**j)', &
'      enddo', &
'      ! rank the numeric data', &
'      call rank_basic(dd,indx)', &
'      ! check order', &
'      do i=1,isz-1', &
'         if(dd(indx(i)).gt.dd(indx(i+1)))then', &
'            write(*,g)''ERROR: data not sorted i='',i,''index='',indx(i), &', &
'            & ''values '',dd(indx(i)),dd(indx(i+1))', &
'            stop 1', &
'         endif', &
'      enddo', &
'      ! sort data using rank values', &
'      dd=dd(indx)', &
'      write(*,g)''sorted '',isz,''values''', &
'      write(*,g)''from'',dd(1),''to'',dd(isz)', &
'      write(*,*)minval(dd).eq.dd(1)', &
'      write(*,*)maxval(dd).eq.dd(isz)', &
'      write(*,*)minloc(dd).eq.1', &
'      write(*,*)maxloc(dd).eq.isz', &
'      ! do a character sort', &
'      strings= [ character(len=20) ::                               &', &
'      & ''red'',    ''green'', ''blue'', ''yellow'', ''orange'',   ''black'', &', &
'      & ''white'',  ''brown'', ''gray'', ''cyan'',   ''magenta'',           &', &
'      & ''purple'']', &
'      if(allocated(cindx))deallocate(cindx)', &
'      allocate(cindx(size(strings)))', &
'', &
'      write(*,''(a,8(a:,","))'')''BEFORE '',&', &
'              & (trim(strings(i)),i=1,size(strings))', &
'', &
'      call rank_basic(strings,cindx)', &
'', &
'      write(*,''(a,8(a:,","))'')''SORTED '',&', &
'              & (trim(strings(cindx(i))),i=1,size(strings))', &
'', &
'      strings=strings(cindx) ! sort the array using the rank index', &
'', &
'      do i=1,size(strings)-1', &
'         if(strings(i).gt.strings(i+1))then', &
'            write(*,*)''Error in sorting strings a-z''', &
'         endif', &
'      enddo', &
'   end program demo_rank_basic', &
'', &
'  Results:', &
'', &
'   sorted  10000 values', &
'   from -.3393216923767161E+308 to .4341912370205701E+308', &
'    T', &
'    T', &
'    T', &
'    T', &
'   BEFORE red,green,blue,yellow,orange,black,white,brown,', &
'   gray,cyan,magenta,purple', &
'   SORTED black,blue,brown,cyan,gray,green,magenta,orange,', &
'   purple,red,white,yellow', &
'', &
'AUTHOR', &
'   Michel Olagnon - April 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="rank_basic"

call select()


case('18','rank_decreasing')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   Rank_Decreasing(3f) - [orderpack:RANK:UNIQUE] ranks an array', &
'   in decreasing order, with duplicate entries assigned the same', &
'   rank(MergeSort)', &
'', &
'SYNOPSIS', &
'    Subroutine Rank_Decreasing (INVALS, IGOEST)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IGOEST(:)', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'', &
'   RANK_DECREASING(3f) generates an inverse ranking of an array, but', &
'   with duplicate entries assigned the same rank.', &
'', &
'   Internally, the routine is similar to pure merge-sort ranking, but on', &
'   the last pass, it sets indices in IGOEST to the rank of the original', &
'   value in an ordered set with duplicates removed. For performance', &
'   reasons, the first two passes are taken out of the standard loop,', &
'   and use dedicated coding.', &
'', &
'OPTIONS', &
'    INVALS     array to rank', &
'    IGOEST     returned rank array', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_rank_decreasing', &
'   ! rank input array ranking duplicates the same', &
'   use M_orderpack, only : rank_decreasing', &
'   implicit none', &
'   character(len=*),parameter :: fmt=''(a,*(g3.3,1x))''', &
'   integer,allocatable,dimension(:) :: INVALS, igoest, distinct, count', &
'   integer :: imx, i', &
'      ! create an input array', &
'      INVALS=[11, 11, 22, 11, 33, 33, 22, 33, 33]', &
'      ! make an index array of the same size', &
'      if(allocated(igoest))deallocate(igoest)', &
'      allocate(igoest(size(INVALS)))', &
'      print fmt, ''Original:                 '',INVALS', &
'      print fmt, ''Number of indices to sort:'',size(INVALS)', &
'      ! rank input array ranking duplicates the same', &
'      call rank_decreasing(INVALS,igoest)', &
'      print fmt, ''Returned Indices:         '',igoest(:)', &
'      !', &
'      ! interrogate the results', &
'      !', &
'      imx=maxval(igoest)', &
'      print fmt, ''Number of unique indices :'',imx', &
'      ! squeeze it down to just IMX unique values', &
'      count=[(0,i=1,imx)] ! count how many times a value occurs', &
'      distinct=count      ! array to set of unique values', &
'      do i=1,size(INVALS)', &
'         distinct(igoest(i))=INVALS(i)', &
'         count(igoest(i))= count(igoest(i))+1', &
'      enddo', &
'      print fmt, ''Sorted unique values:     '',distinct', &
'      print fmt, ''count of occurrences:     '',count', &
'   end program demo_rank_decreasing', &
'', &
'  Results:', &
'', &
'   Original:                  11  11  22  11  33  33  22  33  33', &
'   Number of indices to sort:  9', &
'   Returned Indices:           1   1   2   1   3   3   2   3   3', &
'   Number of unique indices :  3', &
'   Sorted unique values:      11  22  33', &
'   count of occurrences:       3   2   4', &
'', &
'AUTHOR', &
'   Michel Olagnon, 2000-2012', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="rank_decreasing"

call select()


case('19','rank_unique')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   Rank_Unique(3f) - [orderpack:RANK:UNIQUE] ranks an array, with removal', &
'                     of duplicate entries (MergeSort)', &
'', &
'SYNOPSIS', &
'    Subroutine rank_unique (INVALS, IRNGT, NUNI)', &
'', &
'      ${TYPE} (Kind=${KIND}), Intent (In) :: INVALS(:)', &
'      Integer, Intent (Out)               :: IRNGT(:)', &
'      Integer, Intent (Out)               :: NUNI', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'', &
'DESCRIPTION', &
'', &
'   Ranks an array, removing duplicate entries.', &
'', &
'   Internally, RANK_UNIQUE(3f) performs a Merge-sort ranking of an array,', &
'   with removal of duplicate entries.', &
'', &
'   The routine is similar to pure merge-sort ranking, but on the last', &
'   pass, it discards indices that correspond to duplicate entries.', &
'', &
'   For performance reasons, the first two passes are taken out of the', &
'   standard loop, and use dedicated coding.', &
'', &
'OPTIONS', &
'    INVALS      array to index', &
'    IRNGT      rank index returned pointing to unique values', &
'    NUNI       the number of unique values found', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_rank_unique', &
'   ! rank an array, with removal of duplicate entries.', &
'   use M_orderpack, only : rank_unique', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   integer,allocatable :: INVALS(:)', &
'   !', &
'   INVALS=[10,5,7,1,4,5,6,8,9,10,1]', &
'   call printme()', &
'   INVALS=[-1,0,-2,0,-3,0,-4]', &
'   call printme()', &
'   contains', &
'   subroutine printme()', &
'   integer,allocatable :: irngt(:)', &
'   integer :: nuni', &
'      if(allocated(irngt))deallocate(irngt)', &
'      allocate(irngt(size(INVALS)))', &
'      write(*,g)''ORIGINAL:'',INVALS', &
'      call rank_unique(INVALS,irngt,nuni)', &
'      write(*,g)''NUMBER OF UNIQUE INDICES:'',nuni', &
'      write(*,g)''RETURNED INDICES:'',irngt(:nuni)', &
'      write(*,g)''SORTED DATA:'',INVALS(irngt(:nuni))', &
'   end subroutine', &
'   end program demo_rank_unique', &
'', &
'  Results:', &
'', &
'   ORIGINAL: 10 5 7 1 4 5 6 8 9 10 1', &
'   NUMBER OF UNIQUE INDICES: 8', &
'   RETURNED INDICES: 4 5 2 7 3 8 9 1', &
'   SORTED DATA: 1 4 5 6 7 8 9 10', &
'   ORIGINAL: -1 0 -2 0 -3 0 -4', &
'   NUMBER OF UNIQUE INDICES: 5', &
'   RETURNED INDICES: 7 5 3 1 2', &
'   SORTED DATA: -4 -3 -2 -1 0', &
'', &
'AUTHOR', &
'   Michel Olagnon, 2000-2012', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="rank_unique"

call select()


case('20','sort')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sort(3f) - [orderpack:SORT] Sorts array into ascending order', &
'                (Quicksort)', &
'', &
'SYNOPSIS', &
'    Subroutine Sort (INOUTVALS)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   Sorts INOUTVALS into ascending order (Quicksort)', &
'', &
'   This version is not optimized for performance, and is thus not as', &
'   difficult to read as some other ones.', &
'', &
'   Internally, This subroutine uses Quick-sort in a recursive', &
'   implementation, and insertion sort for the last steps with small', &
'   subsets. It does not use any work array.', &
'', &
'   The Quick-sort', &
'   chooses a "pivot" in the set, and explores the array from', &
'   both ends, looking for a value > pivot with the increasing index,', &
'   for a value <= pivot with the decreasing index, and swapping them', &
'   when it has found one of each. The array is then subdivided in', &
'   two subsets:', &
'', &
'       { values <= pivot} {pivot} {values > pivot}', &
'', &
'   It then recursively the procedure to sort each subset. When the', &
'   size of the subarray is small enough, it switches to an insertion', &
'   sort that is faster for very small sets.', &
'', &
'OPTIONS', &
'    INOUTVALS      array to sort', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_sort', &
'   ! sort array in ascending order', &
'   use,intrinsic :: iso_fortran_env, only : int32, real32, real64', &
'   use M_orderpack, only : sort', &
'   implicit none', &
'   ! an insertion sort is very efficient for very small arrays', &
'   ! but generally slower than methods like quicksort and mergesort.', &
'   real(kind=real64) :: valsd(2000)', &
'   integer           :: i', &
'      call random_seed()', &
'      call random_number(valsd)', &
'      valsd=valsd*1000000.0-500000.0', &
'      call sort(valsd)', &
'      do i=1,size(valsd)-1', &
'         if (valsd(i+1).lt.valsd(i))then', &
'            write(*,*)''not sorted''', &
'            stop 3', &
'         endif', &
'      enddo', &
'      write(*,*)''random arrays are now sorted''', &
'   end program demo_sort', &
'', &
'  Results:', &
'', &
'    random arrays are now sorted', &
'', &
'AUTHOR', &
'   Michel Olagnon - Apr. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="sort"

call select()


case('21','sort_special')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   sort_special(3f) - [orderpack:SORT] Sorts array into ascending order', &
'                (Insertion sort, generally for small or nearly sorted', &
'                arrays)', &
'SYNOPSIS', &
'     Subroutine Sort_Special (INOUTVALS)', &
'', &
'      ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   Sorts INOUTVALS() into ascending order (Insertion sort).', &
'', &
'   If certain requirements are met and performance is important this', &
'   procedure can be far faster, but SORT(3f) and ranking routines', &
'   RANK(3f) and RANK_BASIC(3f) are recommended for the general case.', &
'', &
'   This subroutine uses an Insertion sort. It does not use any work array', &
'   and is faster when INOUTVALS() is of very small size (< 20), or already', &
'   almost sorted; but worst case behavior can be triggered by commonly', &
'   encountered data order (e.g. initially inverse sorted). Therefore,', &
'   in many cases the Quicksort or Mergesort method is faster.', &
'', &
'OPTIONS', &
'    INOUTVALS      array to sort', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_sort_special', &
'   ! sort an array using insertion sort', &
'   use,intrinsic :: iso_fortran_env, only : int32, real32, real64', &
'   use M_orderpack, only : sort_special', &
'   implicit none', &
'   ! an insertion sort is very efficient for very small arrays', &
'   ! but generally slower than methods like quicksort and mergesort.', &
'   integer,parameter :: isz=2000', &
'   real(kind=real64) :: dd(isz), hi, low', &
'   integer           :: i', &
'      ! make an array of random values', &
'      call random_seed()', &
'      call random_number(dd)', &
'      dd=dd*1000000.0-500000.0', &
'      low= minval(dd)', &
'      hi = maxval(dd)', &
'      ! sort the data', &
'      call sort_special(dd)', &
'      ! cursory checks', &
'      if(any(dd(1:isz-1) .gt. dd(2:isz)))stop ''ERROR: array not sorted''', &
'      write(*,*)''check min:'',dd(1).eq.low', &
'      write(*,*)''check max:'',dd(isz).eq.hi', &
'      write(*,*)''PASSED: random array is now sorted''', &
'   end program demo_sort_special', &
'', &
'  Results:', &
'', &
'    check min: T', &
'    check max: T', &
'    PASSED: random array is now sorted', &
'', &
'AUTHOR', &
'   Michel Olagnon - Apr. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="sort_special"

call select()


case('22','unique')

textblock=[character(len=256) :: &
'', &
'NAME', &
'   unique(3f) - [orderpack:UNIQUE] removes duplicates from an array', &
'                otherwise retaining original order (i.e. it is "stable")', &
'', &
'SYNOPSIS', &
'    Subroutine Unique (INOUTVALS, NUNI)', &
'', &
'     ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)', &
'     Integer, Intent (Out)                  :: NUNI', &
'', &
'   Where ${TYPE}(kind=${KIND}) may be', &
'', &
'      o Real(kind=real32)', &
'      o Real(kind=real64)', &
'      o Integer(kind=int32)', &
'      o Character(kind=selected_char_kind("DEFAULT"),len=*)', &
'', &
'DESCRIPTION', &
'   UNIQUE(3f) does a stable removal of duplicates from an array.', &
'', &
'   It leaves in the initial set only those entries that are unique,', &
'   packing the array, and leaving the order of the retained values', &
'   unchanged.', &
'', &
'   Internally this subroutine uses Merge-sort unique inverse ranking.', &
'', &
'OPTIONS', &
'    INOUTVALS   input array to reduce to unique values', &
'    NUNI    number of values comprising the returned set of unique', &
'            values', &
'', &
'EXAMPLES', &
'  Sample program:', &
'', &
'   program demo_unique', &
'   ! remove duplicates with remaining elements remaining in initial order', &
'   use M_orderpack, only : unique', &
'   implicit none', &
'   character(len=*),parameter :: g=''(*(g0,1x))''', &
'   character(len=*),parameter :: list= ''(*(g0:,", "))'',sp=''(*(g0,1x))''', &
'   integer :: nuni', &
'', &
'   int : block', &
'   integer,allocatable :: INOUTVALS(:)', &
'    INOUTVALS=[44,33,33,33,22,11,33,44,55,33]', &
'    print list,''ORIGINAL:'',INOUTVALS', &
'    call unique(INOUTVALS,nuni)', &
'    INOUTVALS=INOUTVALS(:nuni)', &
'    print list,''UNIQUE:'',INOUTVALS', &
'   endblock int', &
'', &
'   end program demo_unique', &
'', &
'  Results:', &
'', &
'   ORIGINAL:, 44, 33, 33, 33, 22, 11, 33, 44, 55, 33', &
'   UNIQUE:, 44, 33, 22, 11, 55', &
'', &
'AUTHOR', &
'   Michel Olagnon - Feb. 2000', &
'MAINTAINER', &
'   John Urban, 2022.04.16', &
'LICENSE', &
'   CC0-1.0', &
'']

shortname="unique"

call select()

case default
   allocate (character(len=256) :: textblock(0))
end select
contains
subroutine select()
if(present(topic))then
   if(topic)then
      textblock=[shortname]
   endif
endif

if(present(prefix))then
   if(prefix)then
      do i=1,size(textblock)
         textblock(i)= shortname//':'//trim(textblock(i))
      enddo
   endif
endif

if(present(m_help))then
   if(m_help)then
      textblock=[character(len=len(textblock)+1) :: ' ',textblock] ! add blank line to put shortname into
      textblock=' '//textblock                                     ! shift to right by one character
      textblock(1)=shortname
   endif
endif
end subroutine select
end function help_intrinsics_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sort_name(lines)
!@(#) sort_name(3fp):sort strings(a-z) over specified field using shell sort starting with [ character
character(len = *)                :: lines(:)
   character(len = :),allocatable :: ihold
   integer                        :: n, igap, i, j, k, jg
   n = size(lines)
   if(n.gt.0)then
      allocate(character(len = len(lines(1))) :: ihold)
   else
      ihold = ''
   endif
   igap = n
   INFINITE: do
      igap = igap/2
      if(igap.eq.0) exit INFINITE
      k = n-igap
      i = 1
      INNER: do
         j = i
         INSIDE: do
            jg = j+igap
            if( lle( lower(lines(j)), lower(lines(jg)) ) )exit INSIDE
            ihold = lines(j)
            lines(j) = lines(jg)
            lines(jg) = ihold
            j = j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i = i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental pure function lower(str) result (string)
!@(#) M_strings::lower(3f): Changes a string to lowercase over specified range
character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)     ! step thru each letter in the string
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32) ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!elemental pure function compact(str,char) result (outstr)
function compact(str,char) result (outstr)

!$@(#) M_strings::compact(3f): Converts white-space to single spaces; removes leading spaces

character(len=*),intent(in)          :: str
character(len=*),intent(in),optional :: char
character(len=len(str))              :: outstr
character(len=1)                     :: ch
integer                              :: i
integer                              :: position_in_output
logical                              :: last_was_space
character(len=1)                     :: char_p
logical                              :: nospace
if(present(char))then
   char_p=char
   if(len(char).eq.0)then
      nospace=.true.
   else
      nospace=.false.
   endif
else
   char_p=' '
   nospace=.false.
endif
   outstr=' '
   last_was_space=.false.
   position_in_output=0

   IFSPACE: do i=1,len_trim(str)
     ch=str(i:i)
     select case(iachar(ch))
       case(0:32,127)                                         ! space or tab character or control character
         if(position_in_output.eq.0)then                      ! still at beginning so ignore leading whitespace
            cycle IFSPACE
         elseif(.not.last_was_space) then                     ! if have not already put out a space output one
           if(.not.nospace)then
              position_in_output=position_in_output+1
              outstr(position_in_output:position_in_output)=char_p
           endif
         endif
         last_was_space=.true.
       case(:-1,33:126,128:)                                  ! not a space, quote, or control character so copy it
         position_in_output=position_in_output+1
         outstr(position_in_output:position_in_output)=ch
         last_was_space=.false.
     end select
   enddo IFSPACE

end function compact
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_orderpack_docs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
