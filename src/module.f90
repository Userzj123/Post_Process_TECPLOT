!234567
module mparameter
implicit none
include 'PARAM.H'
end module mparameter
!===============================================
module msetup
use mparameter
implicit none
integer(int_p)::inssn,insen,insitv,iref,jref,kref
integer(int_p)::wgrid,iwrite3d,iwrite2d
integer(int_p)::nxbox ,nybox ,nzbox ,nx_xy,ny_xy,nx_xz,nz_xz
integer(int_p)::nxboxm,nyboxm,nzboxm,Jindexmax
integer(int_p)::nxboxp,nyboxp,nzboxp
real(real_p)::pi,tke_th,Re,ystep,ymin
character(100)::cfn
end module msetup
!===============================================
module mvariable
use mparameter
implicit none
real(real_p),allocatable,dimension(:,:,:)::    &
   u,v,w,p,lam
REAL(real_p), ALLOCATABLE, dimension(:, :, :, :):: theta
end module mvariable
!===============================================
module mgeomdata
use mparameter
implicit none
real(real_p):: &
  zpp(ngz-1),xpp(ngx-1,ngy-1),ypp(ngx-1,ngy-1),   &
  zpu(ngz-1),xpu(ngx  ,ngy-1),ypu(ngx  ,ngy-1),   &
  zpv(ngz-1),xpv(ngx-1,ngy  ),ypv(ngx-1,ngy  ),   &
  zpw(ngz  ),xpw(ngx-1,ngy-1),ypw(ngx-1,ngy-1),   &
  zpg(ngz  ),xpg(ngx  ,ngy  ),ypg(ngx  ,ngy  )
real(real_p)::dz,dzbox,lx,ly,lz,dx,dy
end module mgeomdata
!===============================================
module mindices
use mparameter
integer(int_p),dimension(:),allocatable::i_3d_full
integer(int_p),dimension(:),allocatable::j_3d_full
integer(int_p),dimension(:),allocatable::k_3d_full
end module mindices
!===============================================
module mreadavg
use mparameter
real(real_p),dimension(1:ngx,1:ngy):: Ux , Uy , Uz
real(real_p),dimension(1:ngx,1:ngy):: uu , vv , ww
real(real_p),dimension(1:ngx,1:ngy):: uv , vw , uw
real(real_p),dimension(1:ngx,1:ngy):: P  , VT , eps, PP
end module mreadavg
!===============================================
