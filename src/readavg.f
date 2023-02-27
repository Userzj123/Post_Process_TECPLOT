      subroutine readavg
      use mreadavg
      use mgeomdata
      use msetup   , only: nybox
      use mindices , only: i_3d_full,j_3d_full
      implicit none
      double precision, dimension(0:ngx,0:ngy)::Uxm,Uym,Uzm
      double precision, dimension(0:ngx,0:ngy)::uum,vvm,wwm
      double precision, dimension(0:ngx,0:ngy)::uvm,vwm,uwm
      double precision, dimension(0:ngx,0:ngy)::Pm ,VTm,epsm,PPm
      integer::length,i,j,j_max
      double precision::one_double,u_0p99
      character(50)::meanfname

        one_double = 0.0
        inquire(iolength=length) one_double
        length = length*(ngx+1)*(ngy+1)

        meanfname = '../../../RESEARCH/DATA/TBL_ICL_mean/dataD.mean_Rij'

        open(10,file=meanfname, status='unknown',form='unformatted'
     >      ,access='direct', recl=length)
                read(10,rec=1) Uxm
                read(10,rec=2) Uym
                read(10,rec=3) Uzm
                read(10,rec=4) Pm
                read(10,rec=5) VTm
                read(10,rec=6) uum
                read(10,rec=7) vvm
                read(10,rec=8) wwm
                read(10,rec=9) uvm
                read(10,rec=10) vwm
                read(10,rec=11) uwm
                read(10,rec=12) epsm
        close(10)

        !--- Compute stats at grid points ---
      Ux(1:ngx,1:ngy) = 0.50 * ( Uxm(1:ngx  , 0:ngy-1)
     >                         + Uxm(1:ngx  , 1:ngy  ) )
      Uy(1:ngx,1:ngy) = 0.50 * ( Uym(0:ngx-1, 1:ngy  )
     >                         + Uym(1:ngx  , 1:ngy  ) )
      Uz(1:ngx,1:ngy) = 0.25 * ( Uzm(0:ngx-1, 0:ngy-1)
     >                         + Uzm(1:ngx  , 0:ngy-1)
     >                         + Uzm(0:ngx-1, 1:ngy  )
     >                         + Uzm(1:ngx  , 1:ngy  ) )

      uu(1:ngx,1:ngy) = 0.50 * ( uum(1:ngx  , 0:ngy-1)
     >                         + uum(1:ngx  , 1:ngy  ) )
      vv(1:ngx,1:ngy) = 0.50 * ( vvm(0:ngx-1, 1:ngy  )
     >                         + vvm(1:ngx  , 1:ngy  ) )
      ww(1:ngx,1:ngy) = 0.25 * ( wwm(0:ngx-1, 0:ngy-1)
     >                         + wwm(1:ngx  , 0:ngy-1)
     >                         + wwm(0:ngx-1, 1:ngy  )
     >                         + wwm(1:ngx  , 1:ngy  ) )
      uv(1:ngx,1:ngy) = uvm(1:ngx,1:ngy)
      vw(1:ngx,1:ngy) = vwm(1:ngx,1:ngy)
      uw(1:ngx,1:ngy) = uwm(1:ngx,1:ngy)

      P(1:ngx,1:ngy) = Pm(1:ngx,1:ngy)

!      write(*,*) Uy(:,j_3d_full(nybox))

      return
      end subroutine readavg
