!
! bm_test:
! return the results of Brunner-Munzel test
!
! (input)
! nx, ny: length of data
! x, y  : provided data
! alter : alterative (1: "two.sided", 2: "greater", 3: "less")
! alpha : level of significance
! (output)
! pst   : estimation of "P(X<Y)+.5*P(X=Y)"
! ci    : confidence interval of estimation (length = 2)
! stat  : statistics in Brunner-Munzel test
! df    : degree of freedom
! pval  : P value of Brunner-Munzel test
!
subroutine bm_test(nx,ny,x,y,alpha,alter,pst,ci,stat,df,pval)
  implicit none
  integer,intent(in)::nx,ny
  real(8),intent(in)::x(nx),y(ny)
  real(8),intent(in)::alpha
  integer,intent(in)::alter
  real(8),intent(out)::pst,ci(2),stat,df,pval

  interface
     subroutine calc_stat(nx, ny, rkx, rky, rkxy, mx, my, stat, df, se)
       implicit none
       integer,intent(in)::nx,ny
       real(8),intent(in)::rkx(nx),rky(ny),rkxy(nx+ny)
       real(8),intent(in)::mx,my
       real(8),intent(out)::stat,df,se
     end subroutine calc_stat
  end interface

  interface
     subroutine calc_pval(stat, df, alter, pval)
       implicit none
       real(8),intent(in)::stat,df
       integer,intent(in)::alter
       real(8),intent(out)::pval
     end subroutine calc_pval
  end interface

  interface
     subroutine calc_confint(pst, df, se, alpha, ci)
       implicit none
       real(8),intent(in)::pst,df,se,alpha
       real(8),intent(out)::ci(2)
     end subroutine calc_confint
  end interface

  ! variables for calculation of statistics
  real(8) rkx(nx),rky(ny),xy(nx+ny),rkxy(nx+ny),mx,my
  real(8) se

  ! P value for non-overlapped data
  real(8),parameter::P1(3) = (/0.0, 1.0, 0.0/)
  real(8),parameter::P0(3) = (/0.0, 0.0, 1.0/)

  real(8) ZERO     ! for Inf, NaN
  real(8) mean     ! external function

  ZERO = 0.0
  xy = (/x, y/)

  call rank(nx, x, rkx)
  call rank(ny, y, rky)
  call rank(nx+ny, xy, rkxy)
  mx = sum(rkxy(1:nx)) / nx
  my = sum(rkxy(nx+1:nx+ny)) / ny

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
end subroutine bm_test


!
! calculation of parameters (stat, df, se)
! (input)
! nx, ny: length of data
! rkx, rky, rkxy: rank in x, y, xy
! mx, my: mean rank of x and y group
! (output)
! stat: statistics of Brunner-Munzel test
! df  : degree of freedom
! se  : standard error for confidence interval
!
subroutine calc_stat(nx, ny, rkx, rky, rkxy, mx, my, stat, df, se)
  implicit none
  integer,intent(in)::nx,ny
  real(8),intent(in)::rkx(nx),rky(ny),rkxy(nx+ny)
  real(8),intent(in)::mx,my
  real(8),intent(out)::stat,df,se

  real(8) n1,n2
  real(8) nv,nvx,nvy
  real(8) vx,vy

  n1 = dble(nx); n2 = dble(ny)

  ! variance of group x and y
  vx = sum((rkxy(1:nx) - rkx(1:nx) - mx + (nx + 1) * 0.5)**2) / (nx - 1)
  vy = sum((rkxy(nx+1:nx+ny) - rky(1:ny) - my + (ny + 1) * 0.5)**2) / (ny - 1)

  nvx = n1 * vx; nvy = n1 * vy
  nv = nvx + nvy

  stat = n1 * n2 / (nx + ny) * (my - mx) / sqrt(nv)
  df = nv * nv / (nvx * nvx / (nx - 1) + nvy * nvy / (ny - 1))
  se = sqrt(vx / (n1 * n2 * n2) + vy / (n1 * n1 * n2))
  return
end subroutine calc_stat


!
! calculation of P value
! (input)
! stat : statistics of Brunner-Munzel test
! df   : degree of freedom
! alter: alterative (1: "two.sided", 2: "greater", 3: "less")
! (output)
! pval : P value of Brunner-Munzel test
!
subroutine calc_pval(stat, df, alter, pval)
  implicit none
  real(8),intent(in)::stat,df
  integer,intent(in)::alter
  real(8),intent(out)::pval

  real(8) stat2
  integer,parameter::lowertail(3) = (/0, 1, 0/)
  real(8),parameter::multi(3) = (/2.0, 1.0, 1.0/)

  interface
     real(8) function Rf_pt(q, df, lowertail)
       implicit none
       real(8),intent(in)::q,df
       integer,intent(in)::lowertail
     end function Rf_pt
  end interface

  select case(alter)
  case(1)
     stat2 = abs(stat)
  case default
     stat2 = stat
  end select

  pval = Rf_pt(stat2, df, lowertail(alter)) * multi(alter)
  return
end subroutine calc_pval


!
! calculation of confidence interval
! (input)
! pst  : estimation of "P(X<Y)+.5*P(X=Y)"
! df   : degree of freedom
! se   : standard error for confidence interval
! alpha: level of significance
! (output)
! ci   : confidence interval of estimation (length = 2)
!
subroutine calc_confint(pst, df, se, alpha, ci)
  implicit none
  real(8),intent(in)::pst,df,se,alpha
  real(8),intent(out)::ci(2)

  interface
     real(8) function Rf_qt(p, df, lowertail)
       implicit none
       real(8),intent(in)::p,df
       integer,intent(in)::lowertail
     end function Rf_qt
  end interface

  ci = pst + Rf_qt(alpha * 0.5, abs(df), 0) * se * (/-1.0, 1.0/)

  ! ci(1) = max(0.0, ci(1))
  ! ci(2) = min(1.0, ci(2))
  return
end subroutine calc_confint

