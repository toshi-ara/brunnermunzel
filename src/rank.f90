!
! rank:
! rank function in R
!
! (input)
!  n : length of data
!  r : length of data
! (output)
!  rk: rank of each data
!
subroutine rank(n, x, rk)
  implicit none
  integer,intent(in)::n
  real(8),intent(in)::x(n)
  real(8),intent(out)::rk(n)

  interface
     subroutine qsort4(v, indx, ii, jj)
       implicit none
       real(8),intent(in)::v(*)
       integer,intent(inout)::indx(*)
       integer,intent(in)::ii,jj
     end subroutine qsort4
  end interface

  integer i,j
  integer idx(n)

  do i = 1, n
     idx(i) = i
  enddo

  call qsort4(x, idx, 1, n)

  i = 1; j = 1
  do while (j < n)
     j = i
     do while (j < n)
        if (x(j) == x(j + 1)) then
           j = j + 1
        else
           exit
        endif
     enddo

     rk(idx(i:j)) = (i + j) * 0.5
     i = j + 1
  enddo

  return
end subroutine rank
