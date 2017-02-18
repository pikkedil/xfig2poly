module mod_xfig
  implicit none
  type type_xfig_header
    character(len=25) :: orientation
    character(len=25) :: justification
    character(len=25) :: units
    character(len=25) :: papersize
    real              :: magnification
    character(len=25) :: multpage
    integer           :: color
    integer           :: resolution,coord_system
  end type

  ! Object types
  !   0. Color pseudo-object.
  !   1. Ellipse which is a generalization of circle.
  !   2. Polyline which includes polygon and box.
  !   3. Spline which includes closed/open approximated/interpolated/x-spline spline.
  !   4. Text.
  !   5. Arc.
  !   6. Compound object which is composed of one or more objects.
  !
  ! Polyline sub-types 
  !   1: polyline 
  !   2: box 
  !   3: polygon 
  !   4: arc-box 
  !   5: imported-picture bounding-box
  !
  ! Line styles
  !   1. -1 = Default
  !   2.  0 = Solid
  !   3.  1 = Dashed
  !   4.  2 = Dotted
  !   5.  3 = Dash-dotted
  !   6.  4 = Dash-double-dotted
  !   7.  5 = Dash-triple-dotted
  !
  type type_polyline
    integer :: object_code
    integer :: sub_type  
    integer :: line_style
    integer :: thickness
    integer :: pen_color
    integer :: fill_color
    integer :: depth
    integer :: pen_style
    integer :: area_fill
    real    :: style_val
    integer :: join_style
    integer :: cap_style
    integer :: radius
    integer :: forward_arrow
    integer :: backward_arrow
    integer :: npoints
  end type

  type type_text
    integer :: object_code
    integer :: sub_type    ! (0: Left justified  1: Center justified 2: Right justified)
    integer :: color
    integer :: depth
    integer :: pen_style
    integer :: font
    real    :: font_size
    real    :: angle
    integer :: font_flags
    real    :: height
    real    :: length
    integer :: x
    integer :: y                  
    character(len=25) :: txt 
  end type

  type type_xfig_data
    integer,dimension(2,5000) :: xy
    integer,dimension(5000) :: node_chain
    character(len=50) :: name
  end type  
  
  contains
  
  subroutine print_header(header)
    implicit none
    type(type_xfig_header) :: header
    
    write(*,'(1X,A,T30,A)')     'Orientation:',      header%orientation
    write(*,'(1X,A,T30,A)')     'Justification:',    header%justification
    write(*,'(1X,A,T30,A)')     'Units:',            header%units
    write(*,'(1X,A,T30,A)')     'Papersize:',        header%papersize
    write(*,'(1X,A,T30,F10.5)') 'Magnification:',    header%magnification
    write(*,'(1X,A,T30,A)')     'Multpage:',         header%multpage
    write(*,'(1X,A,T30,I4)')    'Color:',            header%color
    write(*,'(1X,A,T30,I4)')    'Resolution:',       header%resolution
    write(*,'(1X,A,T30,I4)')    'Coordinate system:',header%coord_system
    return
  end subroutine print_header
  
  subroutine xfig2poly(name)
    implicit none
    character(len=*) :: name
    character(len=20) :: sub_type
    character(len=20) :: txt
    integer :: n_polylines,n,n_edges,attribute
    
    real,parameter :: tol_r=5.0
    integer,parameter :: max_layer=10
    integer,parameter :: tol_d=3
    real :: tol_theta
    logical :: test,layer_test

    type(type_xfig_header) :: info
    type(type_polyline),dimension(1000) :: land
    type(type_xfig_data),dimension(1000) :: coord
    type(type_text),dimension(500) :: reg

    character(len=80) :: line,path,ofile
    integer :: void,i,j,object_code,k,nvt,ii,v1,v2,stat,ios,nreg,nvt_test
    integer :: n_layer,dx,dy,min_d,tmp
    integer,dimension(2,5000) :: xy
    integer,dimension(5000) :: att
    integer,dimension(max_layer) :: layer
    real :: r1,r2,theta1,theta2
    real :: fig2meter

    open(14,file=name)
    read(14,*)
    read(14,*) info
    call print_header(info)
    fig2meter = 0.24/10800.0
    write(*,*) fig2meter
    fig2meter = 1.0/info%resolution*2.54/100.0
    write(*,*) fig2meter,info%resolution    

    path = ''
    ofile = ''
    k = 0
    do i=len_trim(name),1,-1
      if (name(i:i) == '/') then
        k = i
        path = name(1:k)
        ofile = name(k+1:len_trim(name)-4)
        write(*,'(1X,A,1X,A)') trim(path),trim(ofile)
      endif
    enddo
   
    i = 1
    nreg = 0
    n_polylines = 0
    do 
      read(14,'(A80)',iostat=stat) line
      if (stat /= 0) exit
      read(line,'(I2)',iostat=ios) object_code      
      if (ios /= 0) cycle
      write(*,'(1X,A,I2)') 'Read object-code:',object_code
      !
      ! Internal read for the polyline. Polygons are converted to polylines.
      !      
      if (object_code == 2) then
        read(line,*) land(i)
        write(*,'(2X,A,2I5)') 'Read sub-type:',land(i)%sub_type,land(i)%npoints
        if (land(i)%sub_type == 1) then
          write(*,'(2X,A)') 'Read polyline...'
          read(14,*) (coord(i)%xy(:,j),j=1,land(i)%npoints)
          n_polylines = n_polylines+1
        elseif (land(i)%sub_type == 2) then
          write(*,'(2X,A)') 'box not supported'
          cycle
        elseif (land(i)%sub_type == 3) then
          write(*,'(2X,A)') 'Read polygon...'
          k = land(i)%npoints
          read(14,*) (coord(i)%xy(:,j),j=1,k)
          n_polylines = n_polylines+1
        elseif (land(i)%sub_type == 4) then
          write(*,'(2X,A)') 'arc-box not supported'
          cycle          
        elseif (land(i)%sub_type == 5) then
          write(*,'(2X,A)') 'imported-picture bounding-box not supported'
          cycle
        else
          write(*,*) 'type unknown'
          cycle
        endif
        i = i + 1
      endif
      !
      ! Get the attributes of the regions (type_text)
      !
      if (object_code == 4) then
        write(*,'(2X,A)') 'Read regional attribute...'
        nreg = nreg + 1
        read(line,*) reg(nreg)
      endif
    enddo
    close(14) 
    !
    ! Test for regions
    !
    if (nreg == 0) then
      write(*,'(1X,A)') 'No regions defined'
    endif
    !
    ! Count the number of layers used in the .fig file
    !
    n_layer = 1
    layer(1) = land(1)%depth
    do i=1,n_polylines
      layer_test = .false.
      do j=1,n_layer
        if (layer(j) == land(i)%depth) then
          layer_test = .true.
          exit
        endif
      enddo
      if (.NOT. layer_test) then
        n_layer = n_layer+1
        if (n_layer > max_layer) then
          write(*,'(1X,A)') 'Exceeds the maximum number of layers'
          stop
        else
          layer(n_layer) = land(i)%depth
        endif
      endif
    enddo
    call bubble_sort(layer(1:n_layer))
    write(*,'(1X,A,11I4)') 'No of layers:',n_layer,(layer(i),i=1,n_layer)
    !
    ! Count the number of vertices    
    !
    call r2pol(coord(1)%xy(1,1),coord(i)%xy(2,1),r2,theta2)
    call calc_toll_theta(r2,tol_r,tol_theta)
    tol_theta = tol_theta*1.05
    nvt = 1
    do i=1,n_polylines
      if (land(i)%depth /= 51) cycle 
      xy(:,nvt) = coord(i)%xy(:,1)
      write(*,'(1X,A,2I3)') 'First node: polyline / layer:',i,land(i)%depth
      att(nvt) = land(i)%depth
      exit
    enddo
    layer(n_layer+1) = layer(1)
    write(*,'(1X,A,10I3)') 'Sort layers:',(layer(i),i=2,n_layer+1)
    do k=2,n_layer+1
      do i=1,n_polylines
        if (land(i)%depth /= layer(k)) cycle
        next_vertex: do ii=1,land(i)%npoints
          call r2pol(coord(i)%xy(1,ii),coord(i)%xy(2,ii),r2,theta2)
          do j=1,nvt
            call r2pol(xy(1,j),xy(2,j),r1,theta1)
            if ((abs(r1-r2)<tol_r).AND.(abs(theta1-theta2)<tol_theta)) then
              cycle next_vertex
            endif
          enddo
          nvt = nvt+1
          xy(1,nvt) = coord(i)%xy(1,ii)
          xy(2,nvt) = coord(i)%xy(2,ii)
          att(nvt)  = land(i)%depth
        enddo next_vertex
      enddo
    enddo
    write(*,'(1X,A,I5)') 'Number of vertices:',nvt
    !
    ! Assign the node chains
    !
    do i=1,n_polylines
      nvt_test = 0
      do ii=1,land(i)%npoints
        call r2pol(coord(i)%xy(1,ii),coord(i)%xy(2,ii),r2,theta2)
        test = .false.
        do j=1,nvt
          call r2pol(xy(1,j),xy(2,j),r1,theta1)
          if ((abs(r1-r2)<tol_r).AND.(abs(theta1-theta2)<tol_theta)) then
            coord(i)%node_chain(ii) = j
            nvt_test = nvt_test + 1
            test = .true.
            exit
          endif
        enddo
        if (.NOT.test) then
          write(*,'(1X,3I8)') ii,coord(i)%xy(:,ii)
        endif
      enddo
      if (nvt_test /= ii-1) then
        write(*,'(1X,3I8)') land(i)%sub_type,land(i)%npoints,nvt_test
      endif
    enddo  

    xy(2,1:nvt) = -xy(2,1:nvt)
    !
    ! Write the .poly file for Triangle
    !
    open(16,file=trim(name(1:len_trim(name)-4))//'.poly')
    write(16,*) nvt,2,0,1
    write(16,'(I8,2F12.5,I5)') (i,xy(:,i)*fig2meter,att(i),i=1,nvt)
    n_edges = 0
    do i=1,n_polylines
      n_edges = n_edges+land(i)%npoints-1
    enddo
    write(*,'(1X,A,I5)') 'Number of edges:',n_edges
    write(16,'(2I8)') n_edges,1
    k = 1
    do i=1,n_polylines
      do j=1,land(i)%npoints-1
        v1 = coord(i)%node_chain(j)
        v2 = coord(i)%node_chain(j+1)
        if (v1 == v2) then
          write(*,'(1X,A,4I8)') 'Vertices are equal',i,j,coord(i)%xy(:,j)
        endif
        if (v1 == 0 .OR. v2 == 0) then
          write(*,'(1X,A,4I8)') 'Vertice equal zero',v1,v2,land(i)%sub_type,land(i)%npoints
          write(*,'(1X,12I8)') coord(i)%xy(:,1:12)
        endif
        write(16,'(4I8)') k,v1,v2,land(i)%depth
        k = k+1
      enddo
    enddo
    write(16,'(I8)') 0 
    write(16,'(I8)') nreg
    do i=1,nreg
      txt = reg(i)%txt(1:len_trim(reg(i)%txt)-4)
      read(txt,'(I3)',iostat=ios) attribute
      if (ios /= 0) then
        write(*,'(1X,A)') 'attribute must be an integer: '//trim(txt)
        cycle
      endif
      write(16,'(I8,2F12.5,I8)') i,reg(i)%x*fig2meter,-reg(i)%y*fig2meter,attribute
    enddo    
    close(16)   
  end subroutine xfig2poly 
  
end module mod_xfig

program convert
  use mod_xfig
  implicit none
  REAL(8)    :: A,B
  integer :: num_args, ix
  character(len=50), dimension(:), allocatable :: args

  num_args = command_argument_count()
  allocate(args(num_args))  
  if (num_args == 0) then
    write(*,*) 'Enter the .fig file to convert'
  else
    do ix = 1, num_args
      call get_command_argument(ix,args(ix))
         ! now parse the argument as you wish
      write(*,*) args(ix)
    end do
    call xfig2poly(args(1))
  endif
  
end

subroutine r2pol(ix,iy,r,theta)
  implicit none
  integer :: ix,iy
  real :: r,theta
  r = sqrt(real(ix)**2+real(iy)**2)
  theta = atan2(real(iy),real(ix))
  return
end

subroutine calc_toll_theta(r0,r,theta)
  !
  ! Use the general Pythagoras theorem to calculate the angle used for
  ! the tolerance when testing for the same vertice.
  !
  ! INPUT
  !   r0    : radius from (0,0) to vertice
  !   r     : radius of cicle around reference point
  ! OUTPUT
  !   theta : angle for test
  !
  implicit none
  real :: a,b,c,theta,r,r0,pi

  pi = 4.0*atan(1.0)
  
  a = r0
  b = r0
  c = r
  write(*,'(1X,A,3F10.2)') 'Pythagoras a,b,c:',a,b,c
  theta = acos((c*c-a*a-b*b)/(-2.0*a*b))
  write(*,'(1X,A,2F10.4)') 'Angle tolerance in rad./deg.:',theta,theta*180.0/pi
  return
end

SUBROUTINE Bubble_Sort(a)
  IMPLICIT NONE
  INTEGER, INTENT(in out), DIMENSION(:) :: a
  INTEGER :: temp
  INTEGER :: i, j
  LOGICAL :: swapped

  DO j = SIZE(a)-1, 1, -1
    swapped = .FALSE.
    DO i = 1, j
      IF (a(i) > a(i+1)) THEN
        temp = a(i)
        a(i) = a(i+1)
        a(i+1) = temp
        swapped = .TRUE.
      END IF
    END DO
    IF (.NOT. swapped) EXIT
  END DO
END SUBROUTINE Bubble_Sort
  
  