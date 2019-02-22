*
*     bm_permutation_stat:
*     calculation of statistics for Brunner-Munzel permutation test
*     Recursive version
*
*     (input)
*     n    : length of data
*     r    : length of provided data
*     n_nCr: choose(n, r) in R function
*     dat  : provided data (length is n)
*     (output)
*     statistics: statistics in all combinations (length is n_nCr)
*
      subroutine bm_permutation_stat(n, r, n_nCr, dat, statistics)
      implicit none
      integer,intent(in)::n,r,n_nCr
      double precision,intent(in)::dat(n)
      double precision,intent(out)::statistics(n_nCr)

!     variables for recursive subroutine
      integer idx(r)
      integer nest,column
      integer cnt

!     variables for calculation of statistics
      integer nx,ny
      double precision const(4)

      nx = r; ny = n - r

!     constant values (from nx and ny) to avoid multiple calculation
      const(1) = (nx + 1) * 0.5
      const(2) = (ny + 1) * 0.5
      const(3) = nx * 1.0 / (nx - 1)
      const(4) = ny * 1.0 / (ny - 1)

!     initial setting
      nest = 0; column = 1
      cnt = 1
      call nest0(nest, column, nx, ny, idx, nest0)
      return

      contains
      subroutine nest0(nest, column, n1, n2, idx, nest1)
      integer,intent(in)::nest,column,n1,n2
      integer,intent(out)::idx(n1)
      external nest1
      external calc_statistics

      integer i
      double precision stat

      do i = nest + 1, n2 + column
         idx(column) = i
         if (column.ne.n1) then
            call nest1(i, column + 1, n1, n2, idx, nest1)
         else
            call calc_statistics(nx, ny, dat, const, idx, stat)
            statistics(cnt) = stat
            cnt = cnt + 1
         end if
      end do

      return
      end subroutine nest0

      end
