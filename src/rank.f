      subroutine rank(n, x, rk)
      implicit none
      integer,intent(in)::n
      double precision,intent(in)::x(n)
      double precision,intent(out)::rk(n)
      integer k,i,j
      integer idx(N)

      do i = 1, n
         idx(i) = i
      enddo

      call qsort4(x, idx, 1, n)

      i = 1
      do
         j = i
         do
            if ((j.lt.n).and.(x(j).eq.x(j + 1))) then
               j = j + 1
            else
               exit
            endif
         enddo

         do k = i, j
            rk(idx(k)) = (i + j) / 2.0
         enddo

         i = j + 1
         if (i.gt.n) then
            exit
         endif
      enddo

      return
      end
