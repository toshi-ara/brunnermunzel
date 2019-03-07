!
! bm_permutation_stat:
!  calculation of statistics for Brunner-Munzel permutation test
!
! (input)
!  n    : length of data
!  r    : length of provided data
!  n_nCr: choose(n, r) in R function
!  dat  : provided data (length is n)
! (output)
!  statistics: statistics in all combinations (length is n_nCr)
!
subroutine bm_permutation_stat(n, r, n_nCr, dat, statistics)
  implicit none
  integer,intent(in)::n,r,n_nCr
  real(8),intent(in)::dat(n)
  real(8),intent(out)::statistics(n_nCr)

  interface
     subroutine calc_statistics(nx, ny, dat, const, idx, stat)
       implicit none
       integer,intent(in)::nx,ny
       real(8),intent(in)::dat(nx+ny)
       real(8),intent(in)::const(6)
       integer,intent(in)::idx(nx)
       real(8),intent(out)::stat
     end subroutine calc_statistics
  end interface

  interface
     subroutine combination(n, r, ini, arr)
       implicit none
       integer,intent(in)::n,r,ini(1:r)
       integer,intent(inout)::arr(1:r)
     end subroutine combination
  end interface

  integer nx,ny,i,j
  real(8) stat
  integer ini(r),idx(r)
  real(8) const(6)

  nx = r
  ny = n - r

  ! constant values (from nx and ny) to avoid multiple calculation
  const(1) = (nx + 1) * 0.5
  const(2) = (ny + 1) * 0.5
  const(3) = nx * 1.0 / (nx - 1)
  const(4) = ny * 1.0 / (ny - 1)
  const(5) = 1.0 / nx
  const(6) = 1.0 / ny

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
end subroutine bm_permutation_stat
