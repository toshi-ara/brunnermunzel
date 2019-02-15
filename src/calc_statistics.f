*
*     calc_statistics:
*     calculation of statistics for Brunner-Munzel permutation test
*
*     (input)
*     nx, ny: length of x or y groups
*     dat   : provided data (length is n)
*     idx   : index for 'group x' (length is nx)
*     (output)
*     stat  : statistics
*
      subroutine calc_statistics(nx, ny, dat, idx, stat)
      implicit none
      integer,intent(in)::nx, ny
      double precision,intent(in)::dat(nx+ny)
      integer,intent(in)::idx(nx)
      double precision,intent(out)::stat

      integer i
      double precision x(nx), y(ny), xy(nx+ny)
      double precision rkx(nx), rky(ny), rkxy(nx+ny)
      double precision dx(nx), dy(ny)
      double precision mx, my, vx, vy, v

      external mean
      double precision mean

      call divide_groups(nx, ny, dat, idx, x, y, xy)
      call rank(nx, x, rkx)
      call rank(ny, y, rky)
      call rank(nx+ny, xy, rkxy)

      mx = mean(nx, rkxy(1:nx))
      my = mean(ny, rkxy(nx+1:nx+ny))

      dx = (rkxy(1:nx) - rkx - mx + (nx + 1) / 2.0)**2
      dy = (rkxy(nx+1:nx+ny) - rky - my + (ny + 1) / 2.0)**2

      vx = 0; vy = 0
      do i = 1, nx
         vx = vx + dx(i)
      enddo
      vx = vx / (nx - 1)

      do i = 1, ny
         vy = vy + dy(i)
      enddo
      vy = vy / (ny - 1)

      v = nx * vx + ny * vy
      ! to avoid division by zero
      if (v.lt.0.000001) then
         v = 0.00001
      endif

      stat = (my - mx) / sqrt(v)
      return
      end


*
*     divide_groups:
*     divide data into two groups by idx
*
*     (input)
*     nx, ny: length of x or y groups
*     dat   : provided data (length is n)
*     idx   : index for 'group x' (length is nx)
*     (output)
*     x, y  : grouped data by idx
*     xy    : conbination of x and y
*
      subroutine divide_groups(nx, ny, dat, idx, x, y, xy)
      implicit none
      integer,intent(in)::nx, ny
      double precision,intent(in)::dat(nx + ny)
      integer,intent(in)::idx(nx)
      double precision,intent(out)::x(nx), y(ny), xy(nx + ny)
      integer i, j, ix, iy

      ix = 1; iy = 1
      do i = 1, nx + ny
         j = idx(ix)
         if (i.eq.j) then
            x(ix) = dat(i)
            ix = ix + 1
         else
            y(iy) = dat(i)
            iy = iy + 1
         end if
      enddo

      xy(1:nx) = x
      xy(nx+1:nx+ny) = y
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
