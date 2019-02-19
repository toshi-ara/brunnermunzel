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
      ! in
      integer n
      double precision x(n)
      ! out
      double precision rk(n)

      integer k,i,j
      integer idx(n)
      double precision averank

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

         averank = (i + j) * 0.5
         do k = i, j
            rk(idx(k)) = averank
         enddo

         i = j + 1
         if (i.gt.n) then
            exit
         endif
      enddo

      return
      end
