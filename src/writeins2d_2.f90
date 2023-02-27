subroutine writeins2d_2(dat1,dat2,varname1,varname2,imore)
 use mgeomdata
 use msetup
 use mindices
 use mreadavg
 implicit none
 double precision, dimension(1,ngx,ngy)::dat1,dat2
 integer::i,j,k,ii,jj,kk,imore,j_blade
 character(70)::outfname
 !integer::TecIni111,TecDat,TecZne,TecEnd,TecNod,TecFil
 integer::TecIni,TecDat,TecZne,TecEnd,TecNod,TecFil ! MZW: Use TecIni
 integer::O,OO,Debug,VIsDouble,FileType
 double precision, dimension(:,:,:),allocatable:: OutArray
 character(7)::cidx
 character(4)::varname1,varname2

 FileType = 0    ! 0 = full, 1 = grid, 2 = solution
 Debug = 0       ! 0 = no debug & 1 = debug
 VIsDouble = 0   ! 0 = single & 1 = double precision

 write(cidx,fmt='(i7.7)') imore

 !----------- TECPLOT FILE -----------
 outfname = './OUTPUT/ins_'//varname1//'_'//varname2//'_XY_'
 !O=TecIni111('InsField'//char(0), & ! title
! MZW: Use TecIni
 O=TecIni('InsField'//char(0), & ! title
          'x y '//varname1//' '//varname2//char(0), &          ! variables
          trim(outfname)//cidx//'.plt'//char(0), & ! Fname
          '.'//char(0), &           ! ScratchDir
          FileType,Debug,VIsDouble);

 !Write the zone header information
 O=TecZne('SOULTION'//char(0), &
          ngx,ngy,1,'BLOCK'//char(0),char(0));
 OO=ngx * ngy

 allocate( OutArray(ngx,ngy,1) )
 !--- Output grid node locations -----
 do j = 1, ngy
 do i = 1, ngx  
   OutArray(i,j,1) = xpg(i,j)
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 do j = 1, ngy
 do i = 1, ngx  
   OutArray(i,j,1) = ypg(i,j)
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);


 !--- Output velocity data ----
 do j = 1, ngy
 do i = 1, ngx  
   OutArray(i,j,1) = dat1(1,i,j)
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 do j = 1, ngy
 do i = 1, ngx  
   OutArray(i,j,1) = dat2(1,i,j)
 end do
 end do
 O=TecDat(OO,OutArray(1,1,1),1);

 O=TecEnd();

 deallocate( OutArray )

 return
end subroutine writeins2d_2
