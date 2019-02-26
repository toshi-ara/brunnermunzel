*
*     rank:
*      rank function in R
*
*     (input)
*     n  : length of data
*     r  : length of data
*     (output)
*     rk : rank of each data
*
      subroutine rank(n, x, rk)
      implicit none
      integer,intent(in)::n
      double precision,intent(in)::x(n)
      double precision,intent(out)::rk(n)

      integer i,j
      integer idx(n)

      do i = 1, n
         idx(i) = i
      enddo

      call qsort4(x, idx, 1, n)

      i = 1
      do
         j = i
         do
            if (j.ge.n) then
               exit
            else if (x(j).eq.x(j + 1)) then
               j = j + 1
            else
               exit
            endif
         enddo

         rk(idx(i:j)) = (i + j) * 0.5

         if (j.eq.n) then
            exit
         endif
         i = j + 1
      enddo

      return
      end
