!234567
subroutine setup
      use msetup
      use mvariable
      use mgeomdata, only: Lz, Lx, Ly
      implicit none
      integer(int_p)::i
      character(70)::dummy

!==== Read parameter file for file number ========
      open(2,file='parame.ter',status='old',action='read')
      do i = 1 , 4
       read (2,300) dummy
       write(*,300) dummy
      end do

      read (2,304) dummy, cfn
      write(*,304) dummy, cfn
      read (2,301) dummy, inssn
      write(*,301) dummy, inssn
      read (2,301) dummy, insen
      write(*,301) dummy, insen
      read (2,301) dummy, insitv
      write(*,301) dummy, insitv
      read (2,302) dummy, Lx
      write(*,302) dummy, Lx
      read (2,302) dummy, Ly
      write(*,302) dummy, Ly
      read (2,302) dummy, Lz
      write(*,302) dummy, Lz
      read (2,302) dummy, Re
      write(*,302) dummy, Re
      read (2,301) dummy, wgrid
      write(*,301) dummy, wgrid
      read (2,301) dummy, iwrite3d
      write(*,301) dummy, iwrite3d
      read (2,301) dummy, iwrite2d
      write(*,301) dummy, iwrite2d
      read (2,300) dummy
      write(*,300) dummy
      close(2)

      nxbox = ngx
      nybox = ngy
      nzbox = ngz
      print*,ngx, ngy, ngz

      nxboxm = nxbox - 1 ; nyboxm = nybox - 1 ; nzboxm = nzbox - 1
      nxboxp = nxbox + 1 ; nyboxp = nybox + 1 ; nzboxp = nzbox + 1

300   format(a70)
301   format(a50,i15)
302   format(a50,es15.8)
303   format(a50,i4,1x,i4,1x,i4)
304   format(a15,a50)

      pi = acos(-1.d0)
        Lx = Lx*pi
        Ly = Ly*pi
      !call mesh
      call indices

!========================================
      allocate( u(ngx,ngy,ngz) )
      allocate( v(ngx,ngy,ngz) )
      allocate( w(ngx,ngy,ngz) )
      allocate( theta(ngx,ngy,ngz) )

return
end subroutine setup
