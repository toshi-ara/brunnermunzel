*
*     combination
*     by sikino (http://slpr.sakura.ne.jp/qp/combination/) (CC-BY-4.0)
*      with slight modification
*
      subroutine combination(n, r, ini, arr)
      implicit none
      integer,intent(in)::n,r,ini(1:r)
      integer,intent(inout)::arr(1:r)
      integer::i,bef(1:r),numx,key(1:r)

!     Memory array to bef(1~r) before update in "(n-r+1)ary" picture.
      bef(1:r) = arr(1:r)-ini(1:r)
!     Initialize "arr"
      arr(1:r) = 0
      key(1:r) = 0

!     numx means moves up number (n-r).
      numx = n - r
      do i = 1,r
         if (bef(i).eq.numx) key(i) = 1
      enddo

      do i = 1,r - 1
         if (key(i + 1).eq.1) then
            if (key(i).eq.1) then
               if (i.ne.1) arr(i) = arr(i - 1)
            else
               arr(i) = bef(i) + 1
            endif
         else
            arr(i) = bef(i)
         endif
      enddo
      if (key(r).eq.1) then
         arr(r) = arr(r - 1)
      else
         arr(r) = bef(r) + 1
      endif

!     Restore from "(n-r+1)ary" picture to "10ary" picture
      arr(1:r) = arr(1:r) + ini(1:r)

      return
      end subroutine combination


*
*     the same function of choose(n,r) in R
*     by shikino (CC-BY-4.0)
*
      function nCr(n,r)
      implicit none
      integer,intent(in)::n,r
      integer::i,r0,nCr

      r0 = n - r
      if (r.le.r0) r0 = r

      nCr = 1
      do i = 1, r0
        nCr = nCr * (n - r0 + i)
        nCr = nCr / i
      enddo

      return
      end function nCr
