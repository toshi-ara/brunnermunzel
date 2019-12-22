*
*     bm_permutation_test:
*     perform permuted Brunner-Munzel test
*
*     (input)
*     n    : length of data
*     r    : length of provided data
*     n_nCr: choose(n, r) in R function
*     dat  : provided data (length is n)
*     alter : alterative (1: "two.sided", 2: "greater", 3: "less")
*     (output)
*     pval : P value
*     pst  : estimate of "P(X<Y)+.5*P(X=Y)"
*
      subroutine bm_permutation_test(n, r, n_nCr, dat, alter, pval, pst)
      implicit none
      integer,intent(in)::n,r,n_nCr,alter
      double precision,intent(in)::dat(n)
      double precision,intent(out)::pval
      double precision,intent(out)::pst

      integer::i,count
      double precision dat2(n),rkx(r),rky(n-r),rkxy(n),mx,my
      double precision statistics(n_nCr)
      double precision mean     ! external function

!     caluculate estimate
!     rkx, rky, rkxy: rank in x, y, xy
!     mx, my: mean rank of x and y group
      dat2(1:n) = dat(1:n)
      call rank(r, dat2(1:r), rkx)
      call rank(n-r, dat2((r+1):n), rky)
      call rank(n, dat2, rkxy)
      mx = mean(r, rkxy(1:r))
      my = mean(n-r, rkxy((r+1):n))
      pst = (my - (n - r + 1) * 0.5) / r

      call bm_permutation_stat(n, r, n_nCr, dat, statistics)

!     description by R
!     - z0: statistic by observed data, statistic(1)
!     - z1: statistic by permuted data, statistic(i)
!     - "two.sided": mean(abs(z1) >= abs(z0))
!     - "greater"  : mean(z1 <= z0)  i.e. mean(-z1 >= -z0)
!     - "less"     : mean(z1 >= z0)
      if (alter.eq.1) then      ! "two sided"
         statistics(1:n_nCr) = abs(statistics(1:n_nCr))
      else if (alter.eq.2) then ! "greater"
         statistics(1:n_nCr) = -statistics(1:n_nCr)
      endif

      count = 0
      do i = 1, n_nCr
         if (statistics(i).ge.statistics(1)) then
            count = count + 1
         endif
      enddo

      pval = dble(count) / n_nCr
      return
      end

