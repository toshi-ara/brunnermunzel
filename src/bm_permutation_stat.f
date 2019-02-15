*
*     bm_permutation_stat:
*     calculation of statistics for Brunner-Munzel permutation test
*
*     (input)
*     n    : length of data (combination of x and y groups)
*     r    : length of provided data
*     n_nCr: choose(n, r) in R function
*     dat  : provided data (length is n)
*     (output)
*     statistics: statistics in all combinations (length is n_nCr)
*
      subroutine bm_permutation_stat(n, r, n_nCr, dat, statistics)
      implicit none
      integer,intent(in)::n, r, n_nCr
      double precision,intent(in)::dat(n)
      double precision,intent(out)::statistics(n_nCr)

      integer::nx,ny,i,j
      double precision stat
      integer ini(r), idx(r)

      external::nCr
      external::calc_statistics
      external::combination

      nx = r
      ny = n - r
      ini(1:r) = 0
      idx(1:r) = 0

      do j = 1,r
         ini(j) = j
      enddo
      idx(1:r) = ini(1:r)

      do i = 1, n_nCr
         call calc_statistics(nx, ny, dat, idx, stat)
         statistics(i) = stat
         call combination(n, r, ini, idx)
      enddo
      return
      end
