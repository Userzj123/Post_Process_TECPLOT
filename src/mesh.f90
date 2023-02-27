!234567
 subroutine mesh
 use mgeomdata
 implicit none
 integer(int_p):: i,j,ierr,ilength

 write(*,*) 'Reading... grid.data'
 inquire(iolength=ilength) xpg
 open(19, & 
      file='grid.data', &
      form='unformatted',access='direct', &
      recl=ilength, status='old', iostat=ierr)
 if (ierr .ne. 0) stop 'grid.data file is required'
 read(19, rec=1) xpg
 read(19, rec=2) ypg
 close(19)

! write(*,*) xpg(:,1); pause
! write(*,*) ypg(1,:); pause

 dz    = Lz / real(ngz-1)

	!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	! calculate the location of u points in physical domain
	!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	do j=1,ngy-1
	do i=1,ngx 
		xpu(i,j)=0.5*(xpg(i,j)+xpg(i,j+1))
		ypu(i,j)=0.5*(ypg(i,j)+ypg(i,j+1))
	end do
	end do

	!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	! calculate the location of v points in physical domain
	!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	do j=1,ngy
	do i=1,ngx-1
		xpv(i,j)=0.5*(xpg(i,j)+xpg(i+1,j))
		ypv(i,j)=0.5*(ypg(i,j)+ypg(i+1,j))
	end do
	end do

	do j=1,ngy-1
	do i=1,ngx-1
		xpp(i,j)=0.5*(xpu(i,j)+xpu(i+1,j))
		ypp(i,j)=0.5*(ypv(i,j)+ypv(i,j+1))
	end do
	end do

	do j=1,ngy-1
	do i=1,ngx-1
		xpw(i,j)=xpp(i,j)
		ypw(i,j)=ypp(i,j)
	end do
	end do



 return
 end subroutine mesh
