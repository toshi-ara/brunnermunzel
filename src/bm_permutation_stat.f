*
*     bm_permutation_stat:
*     calculation of statistics for permuted Brunner-Munzel test
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

      integer::nx,ny,i,j
      double precision stat
      integer ini(r),idx(r)
      double precision const(4)

      nx = r
      ny = n - r

!     constant values (from nx and ny) to avoid multiple calculation
      const(1) = (nx + 1) * 0.5
      const(2) = (ny + 1) * 0.5
      const(3) = nx * 1.0 / (nx - 1)
      const(4) = ny * 1.0 / (ny - 1)

!     variables to use 'combination' subroutine
      do j = 1,r
         ini(j) = j
      enddo
      idx(1:r) = ini(1:r)

! start analysis (get statistics in all combinations)
      do i = 1, n_nCr
         call calc_statistics(nx, ny, dat, const, idx, stat)
         statistics(i) = stat
         call combination(n, r, ini, idx)
      enddo
      return
      end
