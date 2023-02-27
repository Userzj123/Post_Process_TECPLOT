subroutine writeins3d(varname,imore)
 use mgeomdata
 use msetup
 use mvariable
 use mindices
 use mreadavg
 implicit none
 double precision, dimension(nzboxp,nxboxp,nyboxp)::dat
 integer::i,j,k,ii,jj,kk,imore
 real,parameter :: str_factor = 1.5
 real :: tmp
 character(70)::outfname
 !integer::TecIni111,TecDat,TecZne,TecEnd,TecNod,TecFil  ! MZW: Use TecIni
 integer::TecIni,TecDat,TecZne,TecEnd,TecNod,TecFil
 integer::O,OO,Debug,VIsDouble,FileType
 double precision, dimension(:,:,:),allocatable:: OutArray
 character(8)::cidx
 character(4)::varname

 FileType = 0    ! 0 = full, 1 = grid, 2 = solution
 Debug = 0       ! 0 = no debug & 1 = debug
 VIsDouble = 0   ! 0 = single & 1 = double precision

 dx = lx / real(nxbox, real_p)
 dy = ly / real(nybox, real_p)
 dz = lz / real(nzbox, real_p)

 write(cidx,fmt='(i8.8)') imore

 write(*,*) '    Writing the instantaneous flow field'
 !----------- TECPLOT FILE -----------
 outfname = './OUTPUT/ins_'//varname//'_'
 !O=TecIni111('InsField'//char(0), &        ! title
! MZW: Use TecIni
 O=TecIni('InsField'//char(0), &        ! title
          'x y z u v w theta'//char(0),       &        ! variables
          trim(outfname)//cidx//'.plt'//char(0), & ! Fname
          '.'//char(0), &                  ! ScratchDir
          FileType,Debug,VIsDouble);

 !Write the zone header information
 O=TecZne('SOULTION'//char(0), &
          nxbox,nybox,nzbox,'BLOCK'//char(0),char(0));
 OO=nxbox*nybox*nzbox

 allocate( OutArray(nxbox,nybox,nzbox) )

 !--- Output grid node locations -----
 do i = 1, nxbox
    OutArray(i, :, :) = dx * real(i - 1, real_p)
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 do j = 1, nybox
    OutArray(:, j, :) = dy * real(j - 1, real_p)
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 do k = 1, nzbox
    OutArray(:, :, k) = (tanh((dz * real(k - 1, real_p)-real(1, real_p)) * str_factor)/tanh(str_factor) + real(1,real_p))*2.0
 end do
 O=TecDat(OO,OutArray(1,1,1),1);


 !--- Output velocity data ----
 do k = 1, nzbox
 do j = 1, nybox
 do i = 1, nxbox
   OutArray(i,j,k) = u(i,j,k)
 end do
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 do k = 1, nzbox
 do j = 1, nybox
 do i = 1, nxbox
   OutArray(i,j,k) = v(i,j,k)
 end do
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 do k = 1, nzbox
 do j = 1, nybox
 do i = 1, nxbox
   OutArray(i,j,k) = w(i,j,k)
 end do
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 O=TecEnd();

 deallocate( OutArray )
 return
end subroutine writeins3d
