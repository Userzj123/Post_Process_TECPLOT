subroutine write_grid
 use mgeomdata
 use msetup
 use mindices
 implicit none
 real,parameter :: str_factor = 1.5
 integer(int_p)::i,j,k,ii,jj,kk,imore
 character(30)::outfname
 !integer::TecIni111,TecDat,TecZne,TecEnd,TecNod,TecFil
 integer::TecIni,TecDat,TecZne,TecEnd,TecNod,TecFil  ! MZW: Use TecIni
 integer::O,OO,Debug,VIsDouble,FileType
 double precision, dimension(:,:,:), allocatable:: OutArray

 write(*,*) 'Start writing grid...'
 nxbox = ngx
 nybox = ngy
 nzbox = ngz
 FileType = 1    ! 0 = full, 1 = grid, 2 = solution
 Debug = 0       ! 0 = no debug & 1 = debug
 VIsDouble = 0   ! 0 = single & 1 = double precision
 dx = lx / real(nxbox, real_p)
 dy = ly / real(nybox, real_p)
 dz = lz / real(nzbox, real_p)

 !----------- TECPLOT FILE -----------
 outfname = './OUTPUT/01_Grid3D.plt'
 !O=TecIni111('InsField'//char(0), & ! title
!MZW: Use TEcIni
 O=TecIni('InsField'//char(0), & ! title
          'x y z'//char(0), &         ! variables
          outfname(1:LEN_TRIM(outfname))//char(0), & ! Fname
          '.'//char(0), &           ! ScratchDir
          FileType,Debug,VIsDouble);

 !Write the zone header information
 O=TecZne('GRID'//char(0), &
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
    OutArray(:, :, k) = tanh((dz * real(k - 1, real_p)-real(1, real_p)) * str_factor)/tanh(str_factor) + real(1,real_p)
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 O=TecEnd();

 deallocate( OutArray )

 write(*,*) 'End writing grid...'

return
end subroutine write_grid
