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
      integer,intent(in)::nx,ny
      double precision,intent(in)::dat(nx+ny)
      integer,intent(in)::idx(nx)
      double precision,intent(out)::x(nx),y(ny),xy(nx+ny)

      integer i,ix,iy
      integer tmp(nx+1)         ! to avoid overflow

      tmp(1:(nx+1)) = (/idx(1:nx), -1/)
      ix = 1; iy = 1
      do i = 1, nx + ny
         if (i.eq.tmp(ix)) then
            x(ix) = dat(i)
            ix = ix + 1
         else
            y(iy) = dat(i)
            iy = iy + 1
         end if
      enddo

      xy(1:nx) = x(1:nx)
      xy((nx+1):(nx+ny)) = y(1:ny)
      return
      end
