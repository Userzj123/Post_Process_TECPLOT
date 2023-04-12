!234567
  subroutine read_restart(imore)
  use mvariable
  use msetup
  use mgeomdata
  implicit none
  integer :: imore
  integer :: length
  integer :: k
  character(8)::cidx
  character(50)::cfnm
  character(150)::ffname1

  write(cidx,fmt='(i8.8)') 0!imore

  !---------------------------------------------------------
  !       Read in velocity field
  !---------------------------------------------------------
  inquire(iolength=length) u
  write(cidx,fmt='(i8.8)') imore
  open (18,file=trim(cfn)//'u_velocity.'//cidx,form="unformatted", &
           access="direct",action='read',status='old',recl=length)
        read (18,rec=1) u
  write(*,*) '   * Reading u-component... done'
  close(18)

  open (18,file=trim(cfn)//'v_velocity.'//cidx,form="unformatted", &
           access="direct",action='read',status='old',recl=length)
        read (18,rec=1) v
  write(*,*) '   * Reading v-component... done'
  close(18)

  open (18,file=trim(cfn)//'w_velocity.'//cidx,form="unformatted", &
           access="direct",action='read',status='old',recl=length)
        read (18,rec=1) w
  write(*,*) '   * Reading w-component... done'
  close(18)

  do k = 1, ntheta
  write(ffname1,'(a,i2.2,a)')'theta.', k, '.'
  open (18,file=trim(cfn)//trim(ffname1)//cidx,form="unformatted", &
           access="direct",action='read',status='old',recl=length)
        read (18,rec=1) theta(1:ngx, 1:ngy, 1:ngz, k)
  write(*,*) '   * Reading theta-component... done'
  close(18)
  end do
  return
  end subroutine read_restart
