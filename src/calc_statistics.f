*
*     calc_statistics:
*     calculation of statistics for permuted Brunner-Munzel test
*
*     (input)
*     nx, ny: length of x or y groups
*     dat   : provided data (length is n)
*     const : constant value (from nx and ny) to avoid multiple calculation
*         const(1) = (nx + 1) * 0.5
*         const(2) = (ny + 1) * 0.5
*         const(3) = nx * 1.0 / (nx - 1)
*         const(4) = ny * 1.0 / (ny - 1)
*     idx   : index for 'group x' (length is nx)
*     (output)
*     stat  : statistics
*
      subroutine calc_statistics(nx, ny, dat, const, idx, stat)
      implicit none
      integer,intent(in)::nx,ny
      double precision,intent(in)::dat(nx+ny)
      double precision,intent(in)::const(4)
      integer,intent(in)::idx(nx)
      double precision,intent(out)::stat

      integer i
      double precision x(nx),y(ny),xy(nx+ny)
      double precision rkx(nx),rky(ny),rkxy(nx+ny)
      double precision dx(nx),dy(ny)
      double precision mx,my,vx,vy,v

      double precision mean

      call divide_groups(nx, ny, dat, idx, x, y, xy)
      call rank(nx, x, rkx)
      call rank(ny, y, rky)
      call rank(nx+ny, xy, rkxy)

      mx = mean(nx, rkxy(1:nx))
      my = mean(ny, rkxy(nx+1:nx+ny))

      dx = (rkxy(1:nx) - rkx - mx + const(1))**2
      dy = (rkxy(nx+1:nx+ny) - rky - my + const(2))**2

      vx = 0; vy = 0
      do i = 1, nx
         vx = vx + dx(i)
      enddo

      do i = 1, ny
         vy = vy + dy(i)
      enddo

      v = const(3) * vx + const(4) * vy
      stat = (my - mx) / sqrt(v)
      return
      end


*
*     mean
*
*     (input)
*     n: length of data
*     x: data
*
      double precision function mean(n, x)
      implicit none
      integer,intent(in)::n
      double precision,intent(in)::x(n)

      integer i
      double precision s

      s = 0
      do i = 1, n
         s = s + x(i)
      enddo

      mean = s / n
      return
      end
