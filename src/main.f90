!234567
program main
      use msetup
      use mvariable
      use omp_lib 
      implicit none
      integer(int_p)::navg,imore,nthds,i,j

      call setup

!      nthds = omp_get_max_threads()
!      write(*,*) 'No. of threads = ',nthds
!      call openmpcut(nthds)

!      call readavg
      if( wgrid == 1 ) call write_grid

      navg = 0
!====================================================
      timestep : do imore = inssn, insen, insitv
      navg = navg + 1

      write(*,101)
      write(*,102) imore
!      write(*,103) navg
      write(*,101)

      call read_restart(imore)
!      if(iwrite2d==1) call writeins2d_2(u(1,1:ngx,1:ngy),v(1,1:ngx,1:ngy),'uraw','vraw',imore)
!      if(iwrite3d==1) call writeins3d(u,'uraw',imore)
!      if(iwrite3d==1) call writeins3d(v,'vraw',imore)
      if(iwrite3d==1) call writeins3d('velo',imore)

100   continue

      write(*,101)
      write(*,*) ' '

      end do timestep
!====================================================

101   format(30('*='))
102   format('     ##### Post process for the ',i6,' snapshot #####')
103   format('     #####              N step :',i6,'          #####')

      write(*,*) '  '
      write(*,*) 'END SIMULATION! GOOD LUCK!'
      stop
end program main
