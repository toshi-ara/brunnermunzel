*
*     bm_test:
*     return the results of Brunner-Munzel test
*
*     (input)
*     nx, ny: length of data
*     x, y  : provided data
*     alt   : alterative (1: "two.sided", 2: "greater", 3: "less")
*     alpha : level of significance
*     (output)
*     pst : estimation of "P(X<Y)+.5*P(X=Y)"
*     ci  : confidence interval of estimation
*     stat: statistics in Brunner-Munzel test
*     df  : degree of freedom
*     pval: P value of Brunner-Munzel test
*
      subroutine bm_test(nx,ny,x,y,alpha,alter,pst,ci,stat,df,pval)
      implicit none
      integer,intent(in)::nx,ny
      double precision,intent(in)::x(nx),y(ny)
      double precision,intent(in)::alpha
      integer,intent(in)::alter
      double precision,intent(out)::pst,ci(2),stat,df,pval

      double precision rkx(nx),rky(ny),xy(nx+ny),rkxy(nx+ny)
      double precision mx,my,se
      double precision P1(3), P0(3) ! P value for non-overlapped data
      double precision ZERO ! for Inf, NaN
      double precision mean ! external functions

      ZERO = 0.0
      P1 = (/0.0, 1.0, 0.0/)
      P0 = (/0.0, 0.0, 1.0/)
      xy = (/x, y/)

      call rank(nx, x, rkx)
      call rank(ny, y, rky)
      call rank(nx+ny, xy, rkxy)
      mx = mean(nx, rkxy(1:nx))
      my = mean(ny, rkxy(nx+1:nx+ny))

      pst = (my - (ny + 1) * 0.5) / nx

      if (pst.eq.1) then ! X < Y: non-overlapped data
         ci(1:2) = (/1.0, 1.0/)
         stat = 1.0 / ZERO ! Inf
         df = 0.0 / ZERO ! NaN
         pval = P1(alter)
      else if (pst.eq.0) then ! X > Y: non-overlapped data
         ci(1:2) = (/0.0, 0.0/)
         stat = -1.0 / ZERO ! -Inf
         df = 0.0 / ZERO ! NaN
         pval = P0(alter)
      else ! overlapped data
         call calc_stat(nx, ny, rkx, rky, rkxy, mx, my, stat, df, se)
         call calc_pval(stat, df, alter, pval)
         call calc_confint(pst, df, se, alpha, ci)
      endif
      return
      end


!     calculation of parameters (stat, df, se)
      subroutine calc_stat(nx, ny, rkx, rky, rkxy, mx, my, stat, df, se)
      implicit none
      integer,intent(in)::nx,ny
      double precision,intent(in)::rkx(nx),rky(ny),rkxy(nx+ny)
      double precision,intent(in)::mx,my
      double precision,intent(out)::stat,df,se

      double precision hm,nv,nvx,nvy
      double precision dx(nx),dy(ny),vx,vy
      integer i

      vx = 0; vy = 0
      dx = (rkxy(1:nx) - rkx - mx + (nx + 1) * 0.5)**2
      dy = (rkxy(nx+1:nx+ny) - rky - my + (ny + 1) * 0.5)**2
      do i = 1, nx
         vx = vx + dx(i)
      enddo
      do i = 1, ny
         vy = vy + dy(i)
      enddo
      vx = vx / (nx - 1); vy = vy / (ny - 1)

      hm = dble(nx) * dble(ny) / (nx + ny)
      nvx = nx * vx; nvy = ny * vy
      nv = nvx + nvy

      stat = hm * (my - mx) / sqrt(nv)
      df = nv**2 / (nvx**2 / (nx - 1) + nvy**2 / (ny - 1))
      se = sqrt(vx / (nx * ny * ny) + vy / (nx * nx * ny))
      return
      end subroutine calc_stat


!     calculation of P value
      subroutine calc_pval(stat, df, alter, pval)
      implicit none
      double precision,intent(in)::stat,df
      integer,intent(in)::alter
      double precision,intent(out)::pval

      double precision stat2
      integer ltail(3)
      double precision multi(3)
      double precision Rf_pt

      ltail = (/0, 1, 0/)
      multi = (/2.0, 1.0, 1.0/)

      stat2 = stat
      if (alter.eq.1) then
         stat2 = abs(stat)
      endif
      pval = Rf_pt(stat2, df, ltail(alter)) * multi(alter)
      return
      end subroutine calc_pval


!     calculation of confidence interval
      subroutine calc_confint(pst, df, se, alpha, ci)
      implicit none
      double precision,intent(in)::pst,df,se,alpha
      double precision,intent(out)::ci(2)

      double precision Rf_qt

      ci = pst + Rf_qt(alpha * 0.5, abs(df), 0) * se * (/-1.0, 1.0/)

      ci(1) = max(0.0, ci(1))
      ci(2) = min(1.0, ci(2))
      return
      end subroutine calc_confint

