!
!     calc_statistics:
!     calculation of statistics for permuted Brunner-Munzel test
!
!     (input)
!     nx, ny: length of x or y groups
!     dat   : provided data (length is n)
!     const : constant value (from nx and ny) to avoid multiple calculation
!         const(1) = (nx + 1) * 0.5
!         const(2) = (ny + 1) * 0.5
!         const(3) = nx * 1.0 / (nx - 1)
!         const(4) = ny * 1.0 / (ny - 1)
!         const(5) = 1.0 / nx
!         const(6) = 1.0 / ny
!     idx   : index for 'group x' (length is nx)
!     (output)
!     stat  : statistics
!
subroutine calc_statistics(nx, ny, dat, const, idx, stat)
  implicit none
  integer,intent(in)::nx,ny
  real(8),intent(in)::dat(nx+ny)
  real(8),intent(in)::const(6)
  integer,intent(in)::idx(nx)
  real(8),intent(out)::stat

  interface
     subroutine divide_groups(nx, ny, dat, idx, x, y, xy)
       implicit none
       integer,intent(in)::nx,ny
       real(8),intent(in)::dat(nx+ny)
       integer,intent(in)::idx(nx)
       real(8),intent(out)::x(nx),y(ny),xy(nx+ny)
     end subroutine divide_groups
  end interface

  interface
     subroutine rank(n, x, rk)
       implicit none
       integer,intent(in)::n
       real(8),intent(in)::x(n)
       real(8),intent(out)::rk(n)
     end subroutine rank
  end interface

  integer i
  real(8) x(nx),y(ny),xy(nx+ny)
  real(8) rkx(nx),rky(ny),rkxy(nx+ny)
  real(8) mx,my,vx,vy,v

  call divide_groups(nx, ny, dat, idx, x, y, xy)
  call rank(nx, x, rkx)
  call rank(ny, y, rky)
  call rank(nx+ny, xy, rkxy)

  mx = sum(rkxy(1:nx)) * const(5)          ! mean(rkxy(1:nx))
  my = sum((rkxy(nx+1:nx+ny))) * const(6)  ! mean(rkxy(nx+1:nx+ny))

  vx = sum((rkxy(1:nx) - rkx - mx + const(1))**2)
  vy = sum((rkxy(nx+1:nx+ny) - rky - my + const(2))**2)
  v = const(3) * vx + const(4) * vy

  ! to avoid division by zero
  if (v.lt.0.000001) then
     v = 0.00001
  endif

  stat = (my - mx) / sqrt(v)
  return
end subroutine calc_statistics

