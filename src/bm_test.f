*
*     bm_test:
*     return the results of Brunner-Munzel test
*
*     (input)
*     nx, ny: length of data
*     x, y  : provided data
*     alter : alterative (1: "two.sided", 2: "greater", 3: "less")
*     alpha : level of significance
*     (output)
*     pst   : estimation of "P(X<Y)+.5*P(X=Y)"
*     ci    : confidence interval of estimation (length = 2)
*     stat  : statistics in Brunner-Munzel test
*     df    : degree of freedom
*     pval  : P value of Brunner-Munzel test
*
      subroutine bm_test(nx,ny,x,y,alpha,alter,pst,ci,stat,df,pval)
      implicit none
      integer,intent(in)::nx,ny
      double precision,intent(in)::x(nx),y(ny)
      double precision,intent(in)::alpha
      integer,intent(in)::alter
      double precision,intent(out)::pst,ci(2),stat,df,pval

!     variables for calculation of statistics
      double precision rkx(nx),rky(ny),xy(nx+ny),rkxy(nx+ny),mx,my
      double precision se

!     P value for non-overlapped data
      double precision,parameter::P1(3) = (/0.0, 1.0, 0.0/)
      double precision,parameter::P0(3) = (/0.0, 0.0, 1.0/)

      double precision ZERO     ! for Inf, NaN
      double precision mean     ! external function

      ZERO = 0.0
      xy = (/x, y/)

      call rank(nx, x, rkx)
      call rank(ny, y, rky)
      call rank(nx+ny, xy, rkxy)
      mx = mean(nx, rkxy(1:nx))
      my = mean(ny, rkxy(nx+1:nx+ny))

      pst = (my - (ny + 1) * 0.5) / nx

      if (pst.eq.1) then        ! non-overlapped data: X < Y
         ci(1:2) = pst          !   (/1.0, 1.0/)
         stat = 1.0 / ZERO      !   Inf
         df = 0.0 / ZERO        !   NaN
         pval = P1(alter)       !   P = 1 in "greater", P = 0 in others
      else if (pst.eq.0) then   ! non-overlapped data: X > Y
         ci(1:2) = pst          !   (/0.0, 0.0/)
         stat = -1.0 / ZERO     !   -Inf
         df = 0.0 / ZERO        !   NaN
         pval = P0(alter)       !   P = 1 in "less", P = 0 in others
      else                      ! overlapped data
         call calc_stat(nx, ny, rkx, rky, rkxy, mx, my, stat, df, se)
         call calc_pval(stat, df, alter, pval)
         call calc_confint(pst, df, se, alpha, ci)
      endif

      return
      end


*
*     calculation of parameters (stat, df, se)
*     (input)
*     nx, ny: length of data
*     rkx, rky, rkxy: rank in x, y, xy
*     mx, my: mean rank of x and y group
*     (output)
*     stat: statistics of Brunner-Munzel test
*     df  : degree of freedom
*     se  : standard error for confidence interval
*
      subroutine calc_stat(nx, ny, rkx, rky, rkxy, mx, my, stat, df, se)
      implicit none
      integer,intent(in)::nx,ny
      double precision,intent(in)::rkx(nx),rky(ny),rkxy(nx+ny)
      double precision,intent(in)::mx,my
      double precision,intent(out)::stat,df,se

      double precision n1,n2
      double precision nv,nvx,nvy
      double precision dx(nx),dy(ny),vx,vy
      integer i

      n1 = dble(nx); n2 = dble(ny)

      vx = 0; vy = 0
      dx = (rkxy(1:nx) - rkx - mx + (nx + 1) * 0.5)**2
      dy = (rkxy(nx+1:nx+ny) - rky - my + (ny + 1) * 0.5)**2
      do i = 1, nx
         vx = vx + dx(i)
      enddo
      do i = 1, ny
         vy = vy + dy(i)
      enddo
      vx = vx / (nx - 1); vy = vy / (ny - 1) ! variance of group x and y

      nvx = n1 * vx; nvy = n2 * vy
      nv = nvx + nvy

      stat = n1 * n2 / (nx + ny) * (my - mx) / sqrt(nv)
      df = nv * nv / (nvx * nvx / (nx - 1) + nvy * nvy / (ny - 1))
      se = sqrt(vx / (n1 * n2 * n2) + vy / (n1 * n1 * n2))
      return
      end subroutine calc_stat


*
*     calculation of P value
*     (input)
*     stat : statistics of Brunner-Munzel test
*     df   : degree of freedom
*     alter: alterative (1: "two.sided", 2: "greater", 3: "less")
*     (output)
*     pval : P value of Brunner-Munzel test
*
      subroutine calc_pval(stat, df, alter, pval)
      implicit none
      double precision,intent(in)::stat,df
      integer,intent(in)::alter
      double precision,intent(out)::pval

      double precision stat2
      integer lowertail(3)
      double precision multi(3)
      double precision Rf_pt

      lowertail = (/0, 1, 0/)
      multi = (/2.0, 1.0, 1.0/)

      stat2 = stat
      if (alter.eq.1) then
         stat2 = abs(stat)
      endif
      pval = Rf_pt(stat2, df, lowertail(alter)) * multi(alter)
      return
      end subroutine calc_pval


*
*     calculation of confidence interval
*     (input)
*     pst  : estimation of "P(X<Y)+.5*P(X=Y)"
*     df   : degree of freedom
*     se   : standard error for confidence interval
*     alpha: level of significance
*     (output)
*     ci   : confidence interval of estimation (length = 2)
*
      subroutine calc_confint(pst, df, se, alpha, ci)
      implicit none
      double precision,intent(in)::pst,df,se,alpha
      double precision,intent(out)::ci(2)

      double precision Rf_qt

      ci = pst + Rf_qt(alpha * 0.5, abs(df), 0) * se * (/-1.0, 1.0/)

!      ci(1) = max(0.0, ci(1))
!      ci(2) = min(1.0, ci(2))
      return
      end subroutine calc_confint

