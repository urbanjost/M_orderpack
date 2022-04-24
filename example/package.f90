program demo_package
use M_mrgref,    only : mrgref
use M_orderpack, only : rank_basic

use M_mrgrnk,    only : mrgrnk
use M_orderpack, only : rank

use M_inssor,    only : inssor
use M_orderpack, only : Sort_special

use M_refsor,    only : refsor
use M_orderpack, only : Sort

use M_rinpar,    only : rinpar
use M_orderpack, only : prank_special

use M_refpar,    only : refpar
use M_orderpack, only : prank_basic

use M_rapknr,    only : rapknr
use M_orderpack, only : prank_decreasing

use M_rnkpar,    only : rnkpar
use M_orderpack, only : prank

use M_inspar,    only : inspar
use M_orderpack, only : psort

implicit none
character(len=*),parameter :: g='(a,*(i3,1x))'
integer,parameter             :: isz=30 ! number of random numbers
integer                       :: mx=13  ! maxval(abs(ii)), biggest magnitude of random values, < isz to likely get duplicates
integer,parameter             :: par=8  ! order of partial sort
integer                       :: ii(isz), jj(isz)
integer                       :: indx(isz)
real                          :: rr(isz)
integer                       :: i,j,k
!===================================================================================================================================
call random_seed()
call random_number(rr)
rr=rr-0.50
ii=floor(rr*mx)
write(*,g)'Original ',ii

write(*,g)'mrgref     - [RANK] produces a sorted ranking index array of input array (basic merge-sort)'
indx=-99
call mrgref(ii,indx)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx)
write(*,g)'rank_basic - [RANK] produces a sorted ranking index array of input array (basic merge-sort)'
indx=-99
call mrgref(ii,indx)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx)
!===================================================================================================================================
write(*,g)'mrgrnk - [RANK] produces a sorted ranking index array of input array (optimized merge-sort)'
indx=-99
call mrgrnk(ii,indx)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx)
write(*,g)'rank   - [RANK] produces a sorted ranking index array of input array (optimized merge-sort)'
indx=-99
call rank(ii,indx)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx)
!===================================================================================================================================

write(*,g)'inssor       - [SORT] Sorts array into ascending order (Insertion sort, generally for small or nearly sorted arrays)'
jj=ii
call inssor(jj)
write(*,g)'         ',jj
write(*,g)'Sort_special - [SORT] Sorts array into ascending order (Insertion sort, generally for small or nearly sorted arrays)'
jj=ii
call Sort_special(jj)
write(*,g)'         ',jj
!===================================================================================================================================

write(*,g)'refsor - [SORT] Sorts array into ascending order (Quicksort)'
jj=ii
call refsor(jj)
write(*,g)'         ',jj
write(*,g)'Sort   - [SORT] Sorts array into ascending order (Quicksort)'
jj=ii
call Sort(jj)
write(*,g)'         ',jj
!===================================================================================================================================

write(*,g)'inspar - [SORT:PARTIAL] partially sorts an array, bringing the N lowest values to the beginning of the array'
jj=ii
call inspar(jj,par)
! note the remainder of the data is perturbed
write(*,g)'         ',jj
write(*,g)'         ',jj(1:par)
write(*,g)'psort  - [SORT:PARTIAL] partially sorts an array, bringing the N lowest values to the beginning of the array'
jj=ii
call psort(jj,par)
! note the remainder of the data is perturbed
write(*,g)'         ',jj
write(*,g)'         ',jj(1:par)
!===================================================================================================================================

write(*,g)'rinpar        - [RANK:PARTIAL] creates partial rank index of N lowest values in an array'
indx=-99
call rinpar(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
write(*,g)'prank_special - [RANK:PARTIAL] creates partial rank index of N lowest values in an array'
indx=-99
call prank_special(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
!===================================================================================================================================

write(*,g)'refpar - [RANK:PARTIAL] partially rank array up to specified number of elements (QuickSort-like)'
indx=-99
call refpar(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
write(*,g)'prank_basic  - [RANK:PARTIAL] partially rank array up to specified number of elements (QuickSort-like)'
indx=-99
call prank_basic(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
!===================================================================================================================================

write(*,g)'rapknr           - [RANK:PARTIAL] partially ranks an array up to a specified number of values, in DECREASING order.'
indx=-99
call rapknr(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
write(*,g)'prank_decreasing - [RANK:PARTIAL] partially ranks an array up to a specified number of values, in DECREASING order.'
indx=-99
call prank_decreasing(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
!===================================================================================================================================

write(*,g)'rnkpar   - [RANK:PARTIAL] partially rank array, up to order N (N number of sorted elements to return) (QuickSort-like)'
indx=-99
call rnkpar(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
write(*,g)'prank    - [RANK:PARTIAL] partially rank array, up to order N (N number of sorted elements to return) (QuickSort-like)'
indx=-99
call prank(ii,indx,par)
write(*,g)'         ',indx
write(*,g)'         ',ii(indx(1:par))
!===================================================================================================================================
write(*,g)'unipar - [RANK:PARTIAL:UNIQUE] partially rank an array removing duplicates'
write(*,g)'prank_unique - [RANK:PARTIAL:UNIQUE] partially rank an array removing duplicates'
!===================================================================================================================================
write(*,g)'uniinv - [RANK:UNIQUE] a MergeSort inverse ranking of an array, with duplicate entries assigned the same rank.'
write(*,g)'rank_orders - [RANK:UNIQUE] a MergeSort inverse ranking of an array, with duplicate entries assigned the same rank.'
!===================================================================================================================================
write(*,g)'unirnk - [RANK:UNIQUE] performs a MergeSort ranking of an array, with removal of duplicate entries.'
write(*,g)'rank_unique - [RANK:UNIQUE] performs a MergeSort ranking of an array, with removal of duplicate entries.'
!===================================================================================================================================
write(*,g)'unista - [UNIQUE] (Stable unique) Removes duplicates from an array otherwise retaining original order'
write(*,g)'unique - [UNIQUE] (Stable unique) Removes duplicates from an array otherwise retaining original order'
!===================================================================================================================================
write(*,g)'fndnth           - [FRACTILE] Return Nth lowest value of an array  (InsertSort-like)'
write(*,g)'orderval_special - [FRACTILE] Return Nth lowest value of an array  (InsertSort-like)'
!===================================================================================================================================
write(*,g)'indnth   - [FRACTILE] Return INDEX of Nth value of array, i.e fractile of order N/SIZE(array) (QuickSort-like)'
write(*,g)'orderloc - [FRACTILE] Return INDEX of Nth value of array, i.e fractile of order N/SIZE(array) (QuickSort-like)'
!===================================================================================================================================
write(*,g)'valnth   - [FRACTILE] Return VALUE of Nth lowest value of array, i.e fractile of order N/SIZE(array) (QuickSort-like)'
write(*,g)'orderval - [FRACTILE] Return VALUE of Nth lowest value of array, i.e fractile of order N/SIZE(array) (QuickSort-like)'
!===================================================================================================================================
write(*,g)'valmed    - [MEDIAN] finds the median of an array'
write(*,g)'medianval - [MEDIAN] finds the median of an array'
!===================================================================================================================================
write(*,g)'median - [MEDIAN] Return median value of array. If number of data is even, return average of the two "medians".'
write(*,g)'median - [MEDIAN] Return median value of array. If number of data is even, return average of the two "medians".'
!===================================================================================================================================
write(*,g)'indmed    - [MEDIAN] Returns INDEX of median value of an array.'
write(*,g)'medianloc - [MEDIAN] Returns INDEX of median value of an array.'
!===================================================================================================================================
write(*,g)'ctrper  - [PERMUTATION] generate a random permutation of an array leaving elements close to initial locations'
write(*,g)'perturb - [PERMUTATION] generate a random permutation of an array leaving elements close to initial locations'
!===================================================================================================================================
write(*,g)'mulcnt      - [MULTIPLICITY] Give the multiplicity for each array value (number of times that it appears in the array)'
write(*,g)'occurrences - [MULTIPLICITY] Give the multiplicity for each array value (number of times that it appears in the array)'
!===================================================================================================================================
end program demo_package
