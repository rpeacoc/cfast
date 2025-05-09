    module smokeview_routines

    use precision_parameters

    use exit_routines, only: cfastexit
    use fire_routines, only: get_gas_temp_and_velocity
    use spreadsheet_header_routines, only: ssheaders_smv
    use utility_routines, only: tointstring, get_filenumber

    use cfast_types, only: detector_type, iso_type, room_type, slice_type, target_type, vent_type
    use setup_data, only: smvcsv, sscompartment, ssdevice, sswall, ssmasses, ssvent, ssdiag, sscalculation

    use cparams, only: smoked, face_front, face_left, face_back, face_right

    use devc_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use room_data, only: n_rooms, roominfo
    use setup_data, only: smvcsv, iofilsmv, iofilsmvplt
    use smkview_data, only: n_iso, isoinfo, n_slice, sliceinfo
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo

    implicit none

    private

    public output_smokeview, output_smokeview_header, output_smokeview_plot_data, output_slicedata

    contains

    ! --------------------------- output_smokeview -------------------------------------------

    subroutine output_smokeview (pabs_ref, pamb, tamb, nrm, nfires, froom_number, fx0, fy0, fz0, stime, nscount)
    !
    ! creates the .smv file used by smokeview to determine size and location of rooms, vents, fires etc
    !
    ! this routine is called only once
    !
    !  smvfile -    name of .smv file
    !  plotfile -   name of file containing zone fire data
    !  pabs_ref -   reference absolute pressure
    !  pamb -       ambient pressure
    !  tamb -       ambient temperature
    !  nrm -     number of rooms
    !  x0,y0,z0 -   room origin
    !  vfrom -      from room number
    !  vto =        to room number
    !  vface -      face number
    !  vwidth -     vent width
    !  vrelbot -    bottom elevation w.r.t. floor of from room
    !  vreltop -    top elevation w.r.t. floor of from room
    !  nfires -     number of fires
    !  froom_number - room containing fire
    !  fx0,fy0,fz0 - location of fire base

    real(eb), intent(in) :: pabs_ref, pamb, tamb, stime
    integer, intent(in) :: nrm, nscount, nfires
    integer, intent(in), dimension(nfires) :: froom_number
    real(eb), intent(in), dimension(nfires) :: fx0, fy0, fz0

    real(eb) :: vred, vgreen, vblue
    real(eb) :: targetvector(6)
    real(eb) :: xyz(6)
    integer ::i, iroom1, iroom2
    character(len=128) :: dir
    character(len=64) :: smokeviewplotfilename, drive, ext, name ! the extension is .plt
    character(len=35) :: cTarg
    integer(4) :: length, splitpathqq
    integer :: vtype
    integer :: csvf_output

    external splitpathqq

    integer ibar, jbar, kbar
    integer :: j
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr
    type(slice_type), pointer :: sf
    type(iso_type), pointer :: isoptr

    ! this code is to trim the file name to the name itself along with the extension
    ! for compatibility with version 4 and later of smokeview
    length = splitpathqq(smvcsv, drive, dir, name, ext)
    smokeviewplotfilename = trim(name) // trim(ext)

    rewind (iofilsmv)
    write (iofilsmv,"(a)") "ZONE"
    write (iofilsmv,"(1x,a)") trim(smokeviewplotfilename)
    write (iofilsmv,"(1x,a)") "PRESSURE"
    write (iofilsmv,"(1x,a)") "P"
    write (iofilsmv,"(1x,a)") "Pa"
    write (iofilsmv,"(1x,a)") "Layer Height"
    write (iofilsmv,"(1x,a)") "zlay"
    write (iofilsmv,"(1x,a)") "m"
    write (iofilsmv,"(1x,a)") "TEMPERATURE"
    write (iofilsmv,"(1x,a)") "TEMP"
    write (iofilsmv,"(1x,a)") "C"
    write (iofilsmv,"(1x,a)") "TEMPERATURE"
    write (iofilsmv,"(1x,a)") "TEMP"
    write (iofilsmv,"(1x,a)") "C"
    write (iofilsmv,"(a)") "AMBIENT"
    write (iofilsmv,"(1x,e13.6,1x,e13.6,1x,e13.6)") pabs_ref,pamb,tamb

    ! change to csvf_output = 1 to test 2d plots in smokeview
    csvf_output = 0
    if (csvf_output == 1) then
       write (iofilsmv,"(a)") "CSVF"
       write (iofilsmv,"( a)") "zone"
       write (iofilsmv,"( a)") trim(smvcsv)
       write (iofilsmv,"(a)") "CSVF"
       write (iofilsmv,"( a)") "compartments"
       write (iofilsmv,"( a)") trim(sscompartment)
       write (iofilsmv,"(a)") "CSVF"
       write (iofilsmv,"( a)") "walls"
       write (iofilsmv,"( a)") trim(sswall)
       write (iofilsmv,"(a)") "CSVF"
       write (iofilsmv,"( a)") "vents"
       write (iofilsmv,"( a)") trim(ssvent)
    endif

    ! Compartment geometry
    do i = 1, nrm
        roomptr=>roominfo(i)

        write (iofilsmv,"(a,1x)")"ROOM"
        write (iofilsmv,"(1x,e11.4,1x,e11.4,1x,e11.4)") roomptr%cwidth, roomptr%cdepth, roomptr%cheight
        write (iofilsmv,"(1x,e11.4,1x,e11.4,1x,e11.4)") roomptr%x0, roomptr%y0, roomptr%z0

        if (n_slice.gt.0) then
            ibar = roomptr%ibar
            jbar = roomptr%jbar
            kbar = roomptr%kbar

            write (iofilsmv,"(a,1x)")"GRID"
            write (iofilsmv,"(1x,i5,1x,i5,1x,i5,1x,i5)")ibar,jbar,kbar,0

            write (iofilsmv,"(a,1x)")"PDIM"
            write (iofilsmv,"(9(f14.5,1x))")roomptr%x0,roomptr%x1,roomptr%y0,roomptr%y1,roomptr%z0,roomptr%z1,0.0_eb,0.0_eb,0.0_eb
            write (iofilsmv,"(a,1x)")"TRNX"
            write (iofilsmv,"(1x,i1)")0
            do j = 0, ibar
                write (iofilsmv,"(i5,1x,f14.5)")j,roomptr%xplt(j)
            end do

            write (iofilsmv,"(a,1x)")"TRNY"
            write (iofilsmv,"(1x,i1)")0
            do j = 0, jbar
                write (iofilsmv,"(i5,1x,f14.5)")j,roomptr%yplt(j)
            end do

            write (iofilsmv,"(a,1x)")"TRNZ"
            write (iofilsmv,"(1x,i1)")0
            do j = 0, kbar
                write (iofilsmv,"(i5,1x,f14.5)")j,roomptr%zplt(j)
            end do

            write (iofilsmv,"(a,1x)")"OBST"
            write (iofilsmv,"(1x,i1)")0
            write (iofilsmv,"(a,1x)")"VENT"
            write (iofilsmv,"(1x,i1,1x,i1)")0,0
        end if
    end do

    ! slice files
    do i = 1, n_slice
        sf=>sliceinfo(i)

        if (sf%skip.eq.1)cycle
        write (iofilsmv,"(a,1x,i3,' &',6(i4,1x))")"SLCF",sf%roomnum,sf%ijk(1),sf%ijk(2),sf%ijk(3),sf%ijk(4),sf%ijk(5),sf%ijk(6)
        write (iofilsmv,"(1x,a)")trim(sf%filename)
        write (iofilsmv,"(1x,a)")trim(sf%menu_label)
        write (iofilsmv,"(1x,a)")trim(sf%colorbar_label)
        write (iofilsmv,"(1x,a)")trim(sf%unit_label)
    end do

    do i = 1, n_iso
        isoptr=>isoinfo(i)

        write (iofilsmv,"(a,1x,i3,' &',6(i4,1x))")"ISOG",isoptr%roomnum
        write (iofilsmv,"(1x,a)")trim(isoptr%filename)
        write (iofilsmv,"(1x,a)")trim(isoptr%menu_label)
        write (iofilsmv,"(1x,a)")trim(isoptr%colorbar_label)
        write (iofilsmv,"(1x,a)")trim(isoptr%unit_label)
    end do

    ! fires
    do i = 1, nfires
        write (iofilsmv,"(a)")"FIRE"
        write (iofilsmv,"(1x,i3,1x,e11.4,1x,e11.4,1x,e11.4)") froom_number(i),fx0(i),fy0(i),fz0(i)
    end do

    ! horizontal vents
    if (n_hvents/=0) then
        do i = 1, n_hvents
            write (iofilsmv,"(a)") "HVENTPOS"
            call get_vent_info ("H", i, iroom1, iroom2, xyz, vred, vgreen, vblue, vtype)
            write (iofilsmv,"(2(1x,i3),1x,6(e11.4,1x))") iroom1, iroom2, xyz(1), xyz(2), xyz(3), xyz(4), xyz(5), xyz(6)
        end do
    end if

    ! vertical vents
    if (n_vvents/=0) then
        do i = 1, n_vvents
            write (iofilsmv,"(a)") "VVENTPOS"
            call get_vent_info ("V",i , iroom1, iroom2, xyz, vred, vgreen, vblue, vtype)
            write (iofilsmv,"(2(1x,i3),1x,6(e11.4,1x),1x,i3)") iroom1, iroom2, xyz(1), xyz(2), xyz(3), xyz(4), xyz(5), xyz(6), vtype
        end do
    end if

    ! mechanical vents
    if (n_mvents/=0) then
        do i = 1, n_mvents
            write (iofilsmv,'(a)') "MVENTPOS"
            call get_vent_info ("M", i, iroom1, iroom2, xyz, vred, vgreen, vblue, vtype)
            write (iofilsmv,"(1x,i3,1x,6(e11.4,1x))") iroom1, xyz(1), xyz(2), xyz(3), xyz(4), xyz(5), xyz(6)
        end do
    end if

    ! detection devices (smoke detectors, heat detectors, sprinklers).
    !these must be first since activation assumes device number is detector number
    if (n_detectors>0) then
        do i = 1, n_detectors
            dtectptr => detectorinfo(i)
            write (iofilsmv,"(a)") "DEVICE"
            if (dtectptr%dtype==smoked) then
                write (iofilsmv,"(a)") "SMOKE_DETECTOR"
            else if (dtectptr%quench) then
                write (iofilsmv,"(a)") "SPRINKLER_PENDENT"
            else
                write (iofilsmv,"(a)") "HEAT_DETECTOR"
            end if
            call getabsdetector(i,targetvector)
            write (iofilsmv,"(1x,6f10.2,2i6)") targetvector,0,0
        end do
    end if

    ! target devices
    do i = 1, n_targets
        call toIntString(i,cTarg)
        write (iofilsmv,"(a)") "DEVICE"
        write (iofilsmv,"(a)") "TARGET % % % TARGET_"//trim(cTarg)
        call getabstarget(i,targetvector)
        write (iofilsmv,"(1x,6f10.2,2i6)") targetvector,0,0
    end do

    write (iofilsmv,"(a)") "TIME"
    write (iofilsmv,"(1x,i6,1x,f11.0)") nscount, stime

    ! zone model devices
    call ssheaders_smv(.false.)

    return
    end subroutine output_smokeview

    ! ---------------------------------- get_vent_info -------------------------------------------------

    subroutine get_vent_info(venttype, ivent, iroom1, iroom2, xyz, vred, vgreen, vblue, vtype)

    ! get the shape data for mechanical flow vent external connections

    character(len=1), intent(in) :: venttype
    integer, intent(in) :: ivent
    integer, intent(out) :: iroom1, iroom2
    real(eb), intent(out) :: xyz(6),vred,vgreen,vblue
    integer, intent(out) :: vtype

    real(eb) :: vheight, varea, voffset
    type(room_type), pointer :: roomptr
    type(vent_type), pointer :: ventptr

    vtype = 2
    if (venttype=='H') then
        ventptr=>hventinfo(ivent)
        if (ventptr%room1<=n_rooms) then
            iroom1 =ventptr%room1
            iroom2 = ventptr%room2
            voffset = ventptr%offset(1)
        else
            iroom1 =ventptr%room2
            iroom2 = ventptr%room1
            voffset = ventptr%offset(2)
        end if
        roomptr => roominfo(iroom1)
        if (ventptr%face==face_front) then
            xyz(1) = voffset
            xyz(2) = voffset + ventptr%width
            xyz(3) = 0.0_eb
            xyz(4) = 0.0_eb
        else if (ventptr%face==face_left) then
            xyz(1) = 0.0_eb
            xyz(2) = 0.0_eb
            xyz(3) = voffset
            xyz(4) = voffset + ventptr%width
        else if (ventptr%face==face_back) then
            xyz(1) = voffset
            xyz(2) = voffset + ventptr%width
            xyz(3) = roomptr%cdepth
            xyz(4) = roomptr%cdepth
        else if (ventptr%face==face_right) then
            xyz(1) = roomptr%cwidth
            xyz(2) = roomptr%cwidth
            xyz(3) = voffset
            xyz(4) = voffset + ventptr%width
        end if
        xyz(5) = ventptr%sill
        xyz(6) = ventptr%soffit
    else if (venttype=='V') then
        ventptr => vventinfo(ivent)
        if (ventptr%room1<=n_rooms) then
            iroom1 = ventptr%room1
            iroom2 = ventptr%room2
            roomptr => roominfo(iroom1)
            xyz(5) = 0.0_eb
            xyz(6) = 0.0_eb
        else
            iroom1 = ventptr%room2
            iroom2 = ventptr%room1
            roomptr => roominfo(iroom1)
            xyz(5) = roomptr%cheight
            xyz(6) = roomptr%cheight
        end if
        varea = ventptr%area
        xyz(1) = ventptr%xoffset - sqrt(varea)/2
        xyz(2) = ventptr%xoffset + sqrt(varea)/2
        xyz(3) = ventptr%yoffset - sqrt(varea)/2
        xyz(4) = ventptr%yoffset + sqrt(varea)/2
        vtype = ventptr%shape
    else if (venttype=='M') then
        ventptr => mventinfo(ivent)
        if (ventptr%room1<=n_rooms) then
            iroom1 =ventptr%room1
            iroom2 = ventptr%room2
            roomptr => roominfo(iroom1)
        else
            iroom1 = ventptr%room2
            iroom2 = ventptr%room1
            roomptr => roominfo(iroom1)
        end if

        vheight = ventptr%height(1)
        varea = ventptr%diffuser_area(1)
        if (ventptr%orientation(1)==1) then
            if (ventptr%xoffset==0.0_eb.or.ventptr%xoffset==roomptr%cwidth) then
                xyz(1) = ventptr%xoffset
                xyz(2) = ventptr%xoffset
                xyz(3) = ventptr%yoffset - sqrt(varea)/2
                xyz(4) = ventptr%yoffset + sqrt(varea)/2
            else if (ventptr%yoffset==0.0_eb.or.ventptr%yoffset==roomptr%cdepth) then
                xyz(1) = ventptr%xoffset - sqrt(varea)/2
                xyz(2) = ventptr%xoffset + sqrt(varea)/2
                xyz(3) = ventptr%yoffset
                xyz(4) = ventptr%yoffset
            else
                xyz(1) = ventptr%xoffset
                xyz(2) = ventptr%xoffset
                xyz(3) = ventptr%yoffset - sqrt(varea)/2
                xyz(4) = ventptr%yoffset + sqrt(varea)/2
            end if
            xyz(5) = vheight - sqrt(varea)/2
            xyz(6) = vheight + sqrt(varea)/2
        else
            xyz(1) = ventptr%xoffset - sqrt(varea)/2
            xyz(2) = ventptr%xoffset + sqrt(varea)/2
            xyz(3) = ventptr%yoffset - sqrt(varea)/2
            xyz(4) = ventptr%yoffset + sqrt(varea)/2
            xyz(5) = vheight
            xyz(6) = vheight
        end if
    end if

    vred = 1.0_eb
    vgreen = 1.0_eb
    vblue = 1.0_eb

    return

    end subroutine get_vent_info

    ! --------------------------- output_smokeview_plot_data -------------------------------------------

    subroutine  output_smokeview_plot_data(time,nrm,pr,zlay,tl,tu,nfires,qdot,height)

    !
    ! this routine records data for the current time step into the smokeview zone fire data file
    !
    ! arguments: time current time
    !            nrm - number of rooms
    !            pr - real array of size nrm of room pressures
    !            zlay - real array of size nrm of layer interface heights
    !            tl - real array of size nrm of lower layer temperatures
    !            tu - real array of size nrm of upper layer temperatures
    !            nfires - number of fires
    !            qdot - real array of size nfires of fire heat release rates
    !            height - real array of size nfires of fire heights

    real(eb), intent(in) :: time
    integer, intent(in) :: nrm
    real(eb), intent(in), dimension(nrm) :: pr, zlay, tl, tu
    integer, intent(in) :: nfires
    real(eb), intent(in), dimension(nfires) :: qdot, height
    real xxtime, xxpr, xxylay, xxtl, xxtu, xxheight, xxqdot

    integer :: i

    xxtime = time
    write (iofilsmvplt) xxtime

    do i = 1, nrm
        xxpr = pr(i)
        xxylay = zlay(i)
        xxtl = tl(i)
        xxtu = tu(i)
        write (iofilsmvplt) xxpr, xxylay, xxtl, xxtu
    end do

    do i = 1, nfires
        xxheight = height(i)
        xxqdot = qdot(i)
        write (iofilsmvplt) xxheight, xxqdot
    end do

    end subroutine output_smokeview_plot_data

    ! --------------------------- output_slicedata -------------------------------------------

    subroutine output_slicedata(time,first_time)

    real(eb), intent(in) :: time
    integer, intent(in) :: first_time
    real(fb), allocatable, dimension(:,:,:) :: tslicedata, uslicedata, vslicedata, wslicedata, sslicedata
    integer :: nx, ny, nz
    type(slice_type), pointer :: sf
    type(room_type), pointer :: roomptr
    integer :: i, ii, jj, kk, roomnum
    real(eb) :: xx, yy, zz, tgas, vgas(4)
    integer :: unit

    do i = 1, n_slice, 5
        sf => sliceinfo(i)
        roomptr => roominfo(sf%roomnum)

        if (sf%skip.eq.1)cycle
        nx = sf%ijk(2) + 1 - sf%ijk(1)
        ny = sf%ijk(4) + 1 - sf%ijk(3)
        nz = sf%ijk(6) + 1 - sf%ijk(5)
        roomnum = sf%roomnum
        if (nx.le.0.or.ny.le.0.or.nz.le.0)cycle
        allocate(tslicedata(0:nx-1,0:ny-1,0:nz-1))
        allocate(uslicedata(0:nx-1,0:ny-1,0:nz-1))
        allocate(vslicedata(0:nx-1,0:ny-1,0:nz-1))
        allocate(wslicedata(0:nx-1,0:ny-1,0:nz-1))
        allocate(sslicedata(0:nx-1,0:ny-1,0:nz-1))
        do ii = 0, nx-1
            xx = roomptr%xplt(sf%ijk(1)+ii) - roomptr%x0
            do jj = 0, ny-1
                yy = roomptr%yplt(sf%ijk(3)+jj) - roomptr%y0
                do kk = 0, nz-1
                    zz = roomptr%zplt(sf%ijk(5)+kk) - roomptr%z0
                    call get_gas_temp_and_velocity(roomnum,xx,yy,zz,tgas, vgas)
                    tslicedata(ii,jj,kk) = real(tgas-273.15_eb,fb)
                    uslicedata(ii,jj,kk) = real(vgas(1),fb)
                    vslicedata(ii,jj,kk) = real(vgas(2),fb)
                    wslicedata(ii,jj,kk) = real(vgas(3),fb)
                    sslicedata(ii,jj,kk) = real(vgas(4),fb)
                end do
            end do
        end do

        if (first_time.eq.1) then
            unit = get_filenumber()
            open (unit,file=sf%filename,form='unformatted',status='replace')
            write (unit) sf%menu_label(1:30)
            write (unit) sf%colorbar_label(1:30)
            write (unit) sf%unit_label(1:30)
            write (unit) (sf%ijk(ii),ii=1,6)
        else
            unit = get_filenumber()
            open (unit,FILE=sf%filename,form='unformatted',status='old',position='append')
        end if
        write (unit) real(time,fb)
        write (unit) (((tslicedata(ii,jj,kk),ii=0,nx-1),jj=0,ny-1),kk=0,nz-1)
        deallocate(tslicedata)
        close(unit)

        sf => sliceinfo(i+1)
        if (first_time.eq.1) then
            unit = get_filenumber()
            open (unit,file=sf%filename,form='unformatted',status='replace')
            write (unit) sf%menu_label(1:30)
            write (unit) sf%colorbar_label(1:30)
            write (unit) sf%unit_label(1:30)
            write (unit) (sf%ijk(ii),ii=1,6)
        else
            unit = get_filenumber()
            open (unit,file=sf%filename,form='unformatted',status='old',position='append')
        end if
        write (unit) real(time,fb)
        write (unit) (((uslicedata(ii,jj,kk),ii=0,nx-1),jj=0,ny-1),kk=0,nz-1)
        deallocate(uslicedata)
        close(unit)

        sf => sliceinfo(i+2)
        if (first_time.eq.1) then
            unit = get_filenumber()
            open (unit,file=sf%filename,form='unformatted',status='replace')
            write (unit) sf%menu_label(1:30)
            write (unit) sf%colorbar_label(1:30)
            write (unit) sf%unit_label(1:30)
            write (unit) (sf%ijk(ii),ii=1,6)
        else
            unit = get_filenumber()
            open (unit,FILE=sf%filename,form='unformatted',status='old',position='append')
        end if
        write (unit) real(time,fb)
        write (unit) (((vslicedata(ii,jj,kk),ii=0,nx-1),jj=0,ny-1),kk=0,nz-1)
        deallocate(vslicedata)
        close(unit)

        sf => sliceinfo(i+3)
        if (first_time.eq.1) then
            unit = get_filenumber()
            open (unit,file=sf%filename,form='unformatted',status='replace')
            write (unit) sf%menu_label(1:30)
            write (unit) sf%colorbar_label(1:30)
            write (unit) sf%unit_label(1:30)
            write (unit) (sf%ijk(ii),ii=1,6)
        else
            unit = get_filenumber()
            open (unit,FILE=sf%filename,form='unformatted',status='old',position='append')
        end if
        write (unit) real(time,fb)
        write (unit) (((wslicedata(ii,jj,kk),ii=0,nx-1),jj=0,ny-1),kk=0,nz-1)
        deallocate(wslicedata)
        close(unit)

        sf => sliceinfo(i+4)
        if (first_time.eq.1) then
            unit = get_filenumber()
            open (unit,file=sf%filename,form='unformatted',status='replace')
            write (unit) sf%menu_label(1:30)
            write (unit) sf%colorbar_label(1:30)
            write (unit) sf%unit_label(1:30)
            write (unit) (sf%ijk(ii),ii=1,6)
        else
            unit = get_filenumber()
            open (unit,FILE=sf%filename,form='unformatted',status='old',position='append')
        end if
        write (unit) real(time,fb)
        write (unit) (((sslicedata(ii,jj,kk),ii=0,nx-1),jj=0,ny-1),kk=0,nz-1)
        deallocate(sslicedata)
        close(unit)
    end do


    end subroutine output_slicedata

    ! --------------------------- output_smokeview_header -------------------------------------------

    subroutine output_smokeview_header (version, nrm, nfires)

    !
    ! This routine prints out a header for the smokeview zone fire data file
    !
    ! This routine is called once
    !
    !  version  - Presently smokeview only supports version=1 .  In the future
    !            if the file format changes then change version to allow
    !            smokeview to determine how the data file is organized
    !  nrm  - number of rooms in simulation
    !  nfires  - number of fires in simulation
    !

    integer, intent(in) :: version, nrm, nfires

    write (iofilsmvplt) version
    write (iofilsmvplt) nrm
    write (iofilsmvplt) nfires
    return
    end subroutine output_smokeview_header

    ! --------------------------- getabsdetector -------------------------------------------

    subroutine getabsdetector(detectornumber, positionvector)

    ! get the absolute position of a target in the computational space

    !	This is the protocol between cfast and smokeview

    integer, intent(in) :: detectornumber
    real(eb), intent(out) :: positionvector(*)
    type(room_type), pointer :: roomptr
    type(detector_type), pointer :: dtectptr

    dtectptr => detectorinfo(detectornumber)
    roomptr => roominfo(dtectptr%room)
    positionvector(1) = dtectptr%center(1) + roomptr%x0
    positionvector(2) = dtectptr%center(2) + roomptr%y0
    positionvector(3) = dtectptr%center(3) + roomptr%z0
    positionvector(4) = 0.0_eb
    positionvector(5) = 0.0_eb
    positionvector(6) = -1.0_eb

    return

    end subroutine getabsdetector

! --------------------------- getabstarget -------------------------------------------

    subroutine getabstarget(itarg, positionvector)

    ! get the absolute position of a target in the computational space

    integer, intent(in) :: itarg
    real(eb), intent(out) :: positionvector(*)

    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr

    targptr => targetinfo(itarg)
    roomptr => roominfo(targptr%room)

    positionvector(1:3) = targptr%center(1:3)
    positionvector(4:6) = targptr%normal(1:3)

    positionvector(1) = positionvector(1) + roomptr%x0
    positionvector(2) = positionvector(2) + roomptr%y0
    positionvector(3) = positionvector(3) + roomptr%z0

    return

    end subroutine getabstarget

end module smokeview_routines

module isosurface

    use precision_parameters

    use exit_routines, only: cfastexit
    use fire_routines, only: get_gas_temp_and_velocity
    use utility_routines, only: get_filenumber
    
    
    use cfast_types, only: room_type

    use cenviro
    use room_data, only: roominfo
    use setup_data
    use smkview_data

    implicit none

    private

    public iso_to_file, output_isodata

    contains

    ! --------------------------- output_isodata -------------------------------------------

    subroutine output_isodata(time,first_time)

    real(eb), intent(in) :: time
    integer, intent(in) :: first_time
    real(fb), allocatable, dimension(:,:,:) :: isodataf
    integer :: ibar, jbar, kbar
    type(iso_type), pointer :: isoptr
    type(room_type), pointer :: roomptr
    integer :: i, ii, jj, kk, roomnum
    real(eb) :: xx, yy, zz, tgas, vgas(4)
    integer :: unit
    real(fb) :: levelsf(1)
    integer :: nlevels
    real(fb) :: timef
    
    save unit

    nlevels = 1
    timef = real(time,fb)

    do i = 1, n_iso
        isoptr => isoinfo(i)

        levelsf(1) = isoptr%value-273.15_eb
        roomnum = isoptr%roomnum
        roomptr => roominfo(roomnum)
        ibar = roomptr%ibar
        jbar = roomptr%jbar
        kbar = roomptr%kbar
        allocate(isodataf(0:ibar+1,0:jbar+1,0:kbar+1))
        do ii = 0, ibar
            xx = roomptr%xplt(ii) - roomptr%x0
            do jj = 0, jbar
                yy = roomptr%yplt(jj) - roomptr%y0
                do kk = 0, kbar
                    zz = roomptr%zplt(kk) - roomptr%z0
                    call get_gas_temp_and_velocity(roomnum,xx,yy,zz,tgas,vgas)
                    isodataf(ii+1,jj+1,kk+1) = real(tgas-273.15_eb,fb)
                end do
            end do
        end do

        if (first_time.eq.1) then
            unit = get_filenumber()
            open (unit,file=isoptr%filename,form='unformatted',status='replace')
        else
            unit = get_filenumber()
            open (unit,file=isoptr%filename,form='unformatted',status='old',position='append')
        end if
        call iso_to_file(unit,first_time, timef,isodataf,levelsf, nlevels, roomptr%xpltf, ibar+1, roomptr%ypltf, jbar+1, &
            roomptr%zpltf, kbar+1)

        deallocate(isodataf)
        close(unit)
    end do

    end subroutine output_isodata

    ! ------------------ ISO_TO_FILE ------------------------

    SUBROUTINE ISO_TO_FILE(LU_ISO,FIRST_TIME,T,VDATA,&
        LEVELS, NLEVELS, XPLT, NX, YPLT, NY, ZPLT, NZ)

    INTEGER, INTENT(IN) :: FIRST_TIME
    INTEGER, INTENT(IN) :: NX, NY, NZ
    INTEGER, INTENT(INOUT) :: LU_ISO
    REAL(FB), INTENT(IN) :: T
    REAL(FB), INTENT(IN), DIMENSION(0:NX,0:NY,0:NZ) :: VDATA
    INTEGER, INTENT(IN) :: NLEVELS
    REAL(FB), INTENT(IN), DIMENSION(NLEVELS) :: LEVELS
    REAL(FB), INTENT(IN), DIMENSION(NX) :: XPLT
    REAL(FB), INTENT(IN), DIMENSION(NY) :: YPLT
    REAL(FB), INTENT(IN), DIMENSION(NZ) :: ZPLT

    INTEGER :: I
    INTEGER :: NXYZVERTS, NTRIANGLES, NXYZVERTS_ALL, NTRIANGLES_ALL
    REAL(FB), DIMENSION(:), POINTER :: XYZVERTS
    INTEGER, DIMENSION(:), POINTER :: TRIANGLES, LEVEL_INDICES
    REAL(FB), DIMENSION(:), POINTER :: XYZVERTS_ALL
    INTEGER, DIMENSION(:), POINTER :: TRIANGLES_ALL, LEVEL_INDICES_ALL
    INTEGER :: MEMERR

    NXYZVERTS_ALL=0
    NTRIANGLES_ALL=0

    NULLIFY(XYZVERTS)
    NULLIFY(TRIANGLES)
    NULLIFY(LEVEL_INDICES)

    NULLIFY(XYZVERTS_ALL)
    NULLIFY(TRIANGLES_ALL)
    NULLIFY(LEVEL_INDICES_ALL)

    DO I =1, NLEVELS
        CALL ISO_TO_GEOM(VDATA, LEVELS(I), &
            XPLT, NX, YPLT, NY, ZPLT, NZ,XYZVERTS, NXYZVERTS, TRIANGLES, NTRIANGLES)
        IF (NTRIANGLES>0.AND.NXYZVERTS>0) THEN
            ALLOCATE(LEVEL_INDICES(NTRIANGLES),STAT=MEMERR)
            CALL ChkMemErr('ISO_TO_FILE','LEVEL_INDICES',MEMERR)
            LEVEL_INDICES=I
            CALL MERGE_GEOM(TRIANGLES_ALL,LEVEL_INDICES_ALL,NTRIANGLES_ALL,XYZVERTS_ALL,NXYZVERTS_ALL,&
                TRIANGLES,LEVEL_INDICES,NTRIANGLES,XYZVERTS,NXYZVERTS)
            DEALLOCATE(LEVEL_INDICES)
            DEALLOCATE(XYZVERTS) ! these variables were allocated in ISO_TO_GEOM
            DEALLOCATE(TRIANGLES)
        end if
    END DO
    IF (NXYZVERTS_ALL>0.AND.NTRIANGLES_ALL>0) THEN
        CALL REMOVE_DUPLICATE_ISO_VERTS(XYZVERTS_ALL,NXYZVERTS_ALL,TRIANGLES_ALL,NTRIANGLES_ALL)
    end if
    IF (FIRST_TIME==1) THEN
        CALL ISO_HEADER_OUT(LU_ISO,LEVELS,NLEVELS)
    end if
    CALL ISO_OUT(LU_ISO,T,XYZVERTS_ALL,NXYZVERTS_ALL,TRIANGLES_ALL,LEVEL_INDICES_ALL,NTRIANGLES_ALL)
    IF (NXYZVERTS_ALL>0.AND.NTRIANGLES>0) THEN
        DEALLOCATE(XYZVERTS_ALL)
        DEALLOCATE(LEVEL_INDICES_ALL)
        DEALLOCATE(TRIANGLES_ALL)
    end if

    RETURN
    END SUBROUTINE ISO_TO_FILE

    ! ------------------ COMPARE_VEC3 ------------------------

    INTEGER FUNCTION COMPARE_VEC3(XI,XJ)
    REAL(FB), INTENT(IN), DIMENSION(3) :: XI, XJ

    REAL(FB) :: DELTA=0.01_FB

    IF (XI(1)<XJ(1)-DELTA) THEN
        COMPARE_VEC3 = -1
        RETURN
    end if
    IF (XI(1)>XJ(1)+DELTA) THEN
        COMPARE_VEC3 = 1
        RETURN
    end if
    IF (XI(2)<XJ(2)-DELTA) THEN
        COMPARE_VEC3 = -1
        RETURN
    end if
    IF (XI(2)>XJ(2)+DELTA) THEN
        COMPARE_VEC3 = 1
        RETURN
    end if
    IF (XI(3)<XJ(3)-DELTA) THEN
        COMPARE_VEC3 = -1
        RETURN
    end if
    IF (XI(3)>XJ(3)+DELTA) THEN
        COMPARE_VEC3 = 1
        RETURN
    end if
    COMPARE_VEC3 = 0
    RETURN
    END FUNCTION COMPARE_VEC3

    ! ------------------ GET_MATCH ------------------------

    INTEGER FUNCTION GET_MATCH(XYZ,VERTS,NVERTS)

    INTEGER, INTENT(IN) :: NVERTS
    REAL(FB), INTENT(IN), DIMENSION(3) :: XYZ
    REAL(FB), INTENT(IN), DIMENSION(3*NVERTS) :: VERTS

    REAL(FB), DIMENSION(3) :: XYZI
    INTEGER :: I

    DO I = 1, NVERTS
        XYZI = VERTS(3*I-2:3*I)
        IF (COMPARE_VEC3(XYZ,XYZI)==0) THEN
            GET_MATCH=I
            RETURN
        end if
    END DO
    GET_MATCH=0
    END FUNCTION GET_MATCH

    ! ------------------ REMOVE_DUPLICATE_ISO_VERTS ------------------------

    SUBROUTINE REMOVE_DUPLICATE_ISO_VERTS(VERTS,NVERTS,TRIANGLES,NTRIANGLES)

    REAL(FB), INTENT(INOUT), POINTER, DIMENSION(:)  :: VERTS
    INTEGER, INTENT(INOUT), POINTER, DIMENSION(:) :: TRIANGLES
    INTEGER, INTENT(IN) :: NTRIANGLES
    INTEGER, INTENT(INOUT) :: NVERTS

    INTEGER, ALLOCATABLE, DIMENSION(:) :: MAPVERTS
    INTEGER :: I,NVERTS_OLD,IFROM,ITO
    INTEGER :: MEMERR
    REAL(FB), DIMENSION(3) :: XYZFROM
    INTEGER :: IMATCH

    NVERTS_OLD = NVERTS

    IF (NVERTS==0.OR.NTRIANGLES==0)RETURN
    ALLOCATE(MAPVERTS(NVERTS),STAT=MEMERR)
    CALL ChkMemErr('REMOVE_DUPLICATE_VERTS','MAPVERTS',MEMERR)

    MAPVERTS(1)=1
    ITO=2
    DO IFROM=2, NVERTS
        XYZFROM(1:3) = VERTS(3*IFROM-2:3*IFROM)
        IMATCH = GET_MATCH(XYZFROM,VERTS,ITO-1)
        if (IMATCH/=0) then
            MAPVERTS(IFROM)=IMATCH
            CYCLE
        end if
        MAPVERTS(IFROM)=ITO
        VERTS(3*ITO-2:3*ITO)=VERTS(3*IFROM-2:3*IFROM)
        ITO = ITO + 1
    END DO
    NVERTS=ITO-1

    ! MAP TRIANGLE NODES TO NEW NODES

    DO I=1,3*NTRIANGLES
        TRIANGLES(I) = MAPVERTS(TRIANGLES(I) + 1) - 1
    END DO

    DEALLOCATE(MAPVERTS)

    END SUBROUTINE REMOVE_DUPLICATE_ISO_VERTS

    ! ------------------ ISO_TO_GEOM ------------------------

    SUBROUTINE ISO_TO_GEOM(VDATA, LEVEL, &
        XPLT, NX, YPLT, NY, ZPLT, NZ,&
        XYZVERTS, NXYZVERTS, TRIANGLES, NTRIANGLES)

    INTEGER, INTENT(IN) :: NX, NY, NZ
    REAL(FB), DIMENSION(0:NX,0:NY,0:NZ), INTENT(IN) :: VDATA
    REAL(FB), INTENT(IN) :: LEVEL
    REAL(FB), INTENT(IN), DIMENSION(NX) :: XPLT
    REAL(FB), INTENT(IN), DIMENSION(NY) :: YPLT
    REAL(FB), INTENT(IN), DIMENSION(NZ) :: ZPLT

    REAL(FB), INTENT(OUT), DIMENSION(:), POINTER :: XYZVERTS
    INTEGER, INTENT(OUT), DIMENSION(:), POINTER :: TRIANGLES
    INTEGER, INTENT(OUT) :: NTRIANGLES, NXYZVERTS

    REAL(FB), DIMENSION(0:1) :: XX, YY, ZZ
    REAL(FB), DIMENSION(0:7) :: VALS
    REAL(FB), DIMENSION(0:35) :: XYZVERTS_LOCAL
    INTEGER :: NXYZVERTS_LOCAL
    INTEGER, DIMENSION(0:14) :: TRIS_LOCAL
    INTEGER :: NTRIS_LOCAL
    INTEGER :: NXYZVERTS_MAX, NTRIANGLES_MAX
    REAL(FB) :: VMIN, VMAX

    INTEGER :: I, J, K

    INTEGER :: MEMERR

    NULLIFY(XYZVERTS)
    NULLIFY(TRIANGLES)
    NTRIANGLES=0
    NXYZVERTS=0
    NXYZVERTS_MAX=1000
    NTRIANGLES_MAX=1000
    ALLOCATE(XYZVERTS(3*NXYZVERTS_MAX),STAT=MEMERR)
    CALL ChkMemErr('ISO_TO_GEOM','XYZVERTS',MEMERR)
    ALLOCATE(TRIANGLES(3*NTRIANGLES_MAX),STAT=MEMERR)
    CALL ChkMemErr('ISO_TO_GEOM','TRIANGLES',MEMERR)

    DO I=1, NX-1
        XX(0) = XPLT(I)
        XX(1) = XPLT(I+1)
        DO J=1,NY-1
            YY(0) = YPLT(J);
            YY(1) = YPLT(J+1);
            DO K=1,NZ-1
                VALS(0) = VDATA(  I,  J,  K)
                VALS(1) = VDATA(  I,J+1,  K)
                VALS(2) = VDATA(I+1,J+1,  K)
                VALS(3) = VDATA(I+1,  J,  K)
                VALS(4) = VDATA(  I,  J,K+1)
                VALS(5) = VDATA(  I,J+1,K+1)
                VALS(6) = VDATA(I+1,J+1,K+1)
                VALS(7) = VDATA(I+1,  J,K+1)

                VMIN = MIN(VALS(0),VALS(1),VALS(2),VALS(3),VALS(4),VALS(5),VALS(6),VALS(7))
                VMAX = MAX(VALS(0),VALS(1),VALS(2),VALS(3),VALS(4),VALS(5),VALS(6),VALS(7))
                IF (VMIN > LEVEL.OR.VMAX < LEVEL) CYCLE

                ZZ(0) = ZPLT(K);
                ZZ(1) = ZPLT(K+1);

                CALL GETISOBOX(XX,YY,ZZ,VALS,LEVEL,&
                    XYZVERTS_LOCAL,NXYZVERTS_LOCAL,TRIS_LOCAL,NTRIS_LOCAL)

                IF (NXYZVERTS_LOCAL > 0.OR.NTRIS_LOCAL > 0) THEN
                    CALL UPDATEISOSURFACE(XYZVERTS_LOCAL, NXYZVERTS_LOCAL, TRIS_LOCAL, NTRIS_LOCAL, &
                        XYZVERTS, NXYZVERTS, NXYZVERTS_MAX, TRIANGLES, NTRIANGLES, NTRIANGLES_MAX)
                end if
            END DO
        END DO
    END DO
    RETURN
    END SUBROUTINE ISO_TO_GEOM

    ! ------------------ ISO_HEADER_OUT ------------------------

    SUBROUTINE ISO_HEADER_OUT(LU_ISO,ISO_LEVELS,NISO_LEVELS)

    INTEGER, INTENT(IN) :: NISO_LEVELS
    INTEGER, INTENT(IN) :: LU_ISO
    REAL(FB), INTENT(IN), DIMENSION(NISO_LEVELS) :: ISO_LEVELS

    INTEGER :: VERSION=1
    INTEGER :: I
    INTEGER :: ONE=1,ZERO=0

    write (LU_ISO) ONE
    write (LU_ISO) VERSION
    write (LU_ISO) NISO_LEVELS
    IF (NISO_LEVELS>0) write (LU_ISO) (ISO_LEVELS(I),I=1,NISO_LEVELS)
    write (LU_ISO) ZERO  ! no integer header
    write (LU_ISO) ZERO, ZERO  ! no static nodes or triangles
    RETURN
    END SUBROUTINE ISO_HEADER_OUT

    ! ------------------ ISO_OUT ------------------------

    SUBROUTINE ISO_OUT(LU_ISO,STIME,VERTS,NVERTS,TRIANGLES,SURFACES,NTRIANGLES)
    INTEGER, INTENT(IN) :: LU_ISO
    REAL(FB), INTENT(IN) :: STIME
    INTEGER, INTENT(IN) :: NVERTS,  NTRIANGLES
    REAL(FB), INTENT(IN), DIMENSION(:), POINTER :: VERTS
    INTEGER, INTENT(IN), DIMENSION(:), POINTER :: TRIANGLES
    INTEGER, INTENT(IN), DIMENSION(:), POINTER :: SURFACES

    INTEGER :: GEOM_TYPE=0

    INTEGER :: I

    write (LU_ISO) STIME, GEOM_TYPE ! dynamic geometry (displayed only at time STIME)
    write (LU_ISO) NVERTS,NTRIANGLES
    IF (NVERTS>0) THEN
        write (LU_ISO) (VERTS(I),I=1,3*NVERTS)
    end if
    IF (NTRIANGLES>0) THEN
        write (LU_ISO) (1+TRIANGLES(I),I=1,3*NTRIANGLES)
        write (LU_ISO) (SURFACES(I),I=1,NTRIANGLES)
    end if

    RETURN
    END SUBROUTINE ISO_OUT

    ! ------------------ MERGE_GEOM ------------------------

    SUBROUTINE MERGE_GEOM(  TRIS_TO,  SURFACES_TO,  NTRIS_TO,  NODES_TO,  NNODES_TO,&
        TRIS_FROM,SURFACES_FROM,NTRIS_FROM,NODES_FROM,NNODES_FROM)

    INTEGER, INTENT(INOUT), DIMENSION(:), POINTER :: TRIS_TO, SURFACES_TO
    REAL(FB), INTENT(INOUT), DIMENSION(:), POINTER :: NODES_TO
    INTEGER, INTENT(INOUT) :: NTRIS_TO,NNODES_TO

    INTEGER, DIMENSION(:), POINTER :: TRIS_FROM, SURFACES_FROM
    REAL(FB), DIMENSION(:), POINTER :: NODES_FROM
    INTEGER, INTENT(IN) :: NTRIS_FROM,NNODES_FROM

    INTEGER :: NNODES_NEW, NTRIS_NEW, n

    NNODES_NEW = NNODES_TO + NNODES_FROM
    NTRIS_NEW = NTRIS_TO + NTRIS_FROM

    CALL REALLOCATE_F('MERGE_GEOM','NODES_TO',NODES_TO,3*NNODES_TO,3*NNODES_NEW)
    CALL REALLOCATE_I('MERGE_GEOM','TRIS_TO',TRIS_TO,3*NTRIS_TO,3*NTRIS_NEW)
    CALL REALLOCATE_I('MERGE_GEOM','SURFACES_TO',SURFACES_TO,NTRIS_TO,NTRIS_NEW)

    NODES_TO(1+3*NNODES_TO:3*NNODES_NEW) = NODES_FROM(1:3*NNODES_FROM)
    TRIS_TO(1+3*NTRIS_TO:3*NTRIS_NEW) = TRIS_FROM(1:3*NTRIS_FROM)
    SURFACES_TO(1+NTRIS_TO:NTRIS_NEW) = SURFACES_FROM(1:NTRIS_FROM)

    DO n=1,3*NTRIS_FROM
        TRIS_TO(3*NTRIS_TO+n) = TRIS_TO(3*NTRIS_TO+n) + NNODES_TO
    END DO
    NNODES_TO = NNODES_NEW
    NTRIS_TO = NTRIS_NEW
    END SUBROUTINE MERGE_GEOM

    ! ------------------ GETISOBOX ------------------------

    SUBROUTINE GETISOBOX(X,Y,Z,VALS,LEVEL,XYZV_LOCAL,NXYZV,TRIS,NTRIS)
    USE utility_routines, only: fmix

    IMPLICIT NONE
    REAL(FB), DIMENSION(0:1), INTENT(IN) :: X, Y, Z
    REAL(FB), DIMENSION(0:7), INTENT(IN) :: VALS
    REAL(FB), INTENT(OUT), DIMENSION(0:35) :: XYZV_LOCAL
    INTEGER, INTENT(OUT), DIMENSION(0:14) :: TRIS
    REAL(FB), INTENT(IN) :: LEVEL
    INTEGER, INTENT(OUT) :: NXYZV
    INTEGER, INTENT(OUT) :: NTRIS

    INTEGER :: I, J

    INTEGER, DIMENSION(0:14) :: COMPCASE=(/0,0,0,-1,0,0,-1,-1,0,0,0,0,-1,-1,0/)

    INTEGER, DIMENSION(0:11,0:1) :: EDGE2VERTEX
    INTEGER, DIMENSION(0:1,0:11) :: EDGE2VERTEXTT
    DATA ((EDGE2VERTEXTT(I,J),I=0,1),J=0,11) /0,1,1,2,2,3,0,3,&
        0,4,1,5,2,6,3,7,&
        4,5,5,6,6,7,4,7/

    INTEGER, POINTER, DIMENSION(:) :: CASE2
    INTEGER, TARGET,DIMENSION(0:255,0:9) :: CASES
    INTEGER, DIMENSION(0:9,0:255) :: CASEST
    DATA ((CASEST(I,J),I=0,9),J=0,255) /&
        0,0,0,0,0,0,0,0, 0,  0,0,1,2,3,4,5,6,7, 1,  1,1,2,3,0,5,6,7,4, 1,  2,&
        1,2,3,0,5,6,7,4, 2,  3,2,3,0,1,6,7,4,5, 1,  4,0,4,5,1,3,7,6,2, 3,  5,&
        2,3,0,1,6,7,4,5, 2,  6,3,0,1,2,7,4,5,6, 5,  7,3,0,1,2,7,4,5,6, 1,  8,&
        0,1,2,3,4,5,6,7, 2,  9,3,7,4,0,2,6,5,1, 3, 10,2,3,0,1,6,7,4,5, 5, 11,&
        3,0,1,2,7,4,5,6, 2, 12,1,2,3,0,5,6,7,4, 5, 13,0,1,2,3,4,5,6,7, 5, 14,&
        0,1,2,3,4,5,6,7, 8, 15,4,0,3,7,5,1,2,6, 1, 16,4,5,1,0,7,6,2,3, 2, 17,&
        1,2,3,0,5,6,7,4, 3, 18,5,1,0,4,6,2,3,7, 5, 19,2,3,0,1,6,7,4,5, 4, 20,&
        4,5,1,0,7,6,2,3, 6, 21,2,3,0,1,6,7,4,5, 6, 22,3,0,1,2,7,4,5,6,14, 23,&
        4,5,1,0,7,6,2,3, 3, 24,7,4,0,3,6,5,1,2, 5, 25,2,6,7,3,1,5,4,0, 7, 26,&
        3,0,1,2,7,4,5,6, 9, 27,2,6,7,3,1,5,4,0, 6, 28,4,0,3,7,5,1,2,6,11, 29,&
        0,1,2,3,4,5,6,7,12, 30,0,0,0,0,0,0,0,0, 0,  0,5,4,7,6,1,0,3,2, 1, 32,&
        0,3,7,4,1,2,6,5, 3, 33,1,0,4,5,2,3,7,6, 2, 34,4,5,1,0,7,6,2,3, 5, 35,&
        2,3,0,1,6,7,4,5, 3, 36,3,7,4,0,2,6,5,1, 7, 37,6,2,1,5,7,3,0,4, 5, 38,&
        0,1,2,3,4,5,6,7, 9, 39,3,0,1,2,7,4,5,6, 4, 40,3,7,4,0,2,6,5,1, 6, 41,&
        5,6,2,1,4,7,3,0, 6, 42,3,0,1,2,7,4,5,6,11, 43,3,0,1,2,7,4,5,6, 6, 44,&
        1,2,3,0,5,6,7,4,12, 45,0,1,2,3,4,5,6,7,14, 46,0,0,0,0,0,0,0,0, 0,  0,&
        5,1,0,4,6,2,3,7, 2, 48,1,0,4,5,2,3,7,6, 5, 49,0,4,5,1,3,7,6,2, 5, 50,&
        4,5,1,0,7,6,2,3, 8, 51,4,7,6,5,0,3,2,1, 6, 52,1,0,4,5,2,3,7,6,12, 53,&
        4,5,1,0,7,6,2,3,11, 54,0,0,0,0,0,0,0,0, 0,  0,5,1,0,4,6,2,3,7, 6, 56,&
        1,0,4,5,2,3,7,6,14, 57,0,4,5,1,3,7,6,2,12, 58,0,0,0,0,0,0,0,0, 0,  0,&
        4,0,3,7,5,1,2,6,10, 60,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,6,7,3,2,5,4,0,1, 1, 64,0,1,2,3,4,5,6,7, 4, 65,&
        1,0,4,5,2,3,7,6, 3, 66,0,4,5,1,3,7,6,2, 6, 67,2,1,5,6,3,0,4,7, 2, 68,&
        6,7,3,2,5,4,0,1, 6, 69,5,6,2,1,4,7,3,0, 5, 70,0,1,2,3,4,5,6,7,11, 71,&
        3,0,1,2,7,4,5,6, 3, 72,0,1,2,3,4,5,6,7, 6, 73,7,4,0,3,6,5,1,2, 7, 74,&
        2,3,0,1,6,7,4,5,12, 75,7,3,2,6,4,0,1,5, 5, 76,1,2,3,0,5,6,7,4,14, 77,&
        1,2,3,0,5,6,7,4, 9, 78,0,0,0,0,0,0,0,0, 0,  0,4,0,3,7,5,1,2,6, 3, 80,&
        0,3,7,4,1,2,6,5, 6, 81,2,3,0,1,6,7,4,5, 7, 82,5,1,0,4,6,2,3,7,12, 83,&
        2,1,5,6,3,0,4,7, 6, 84,0,1,2,3,4,5,6,7,10, 85,5,6,2,1,4,7,3,0,12, 86,&
        0,0,0,0,0,0,0,0, 0,  0,0,1,2,3,4,5,6,7, 7, 88,7,4,0,3,6,5,1,2,12, 89,&
        3,0,1,2,7,4,5,6,13, 90,0,0,0,0,0,0,0,0, 0,  0,7,3,2,6,4,0,1,5,12, 92,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        5,4,7,6,1,0,3,2, 2, 96,6,2,1,5,7,3,0,4, 6, 97,2,1,5,6,3,0,4,7, 5, 98,&
        2,1,5,6,3,0,4,7,14, 99,1,5,6,2,0,4,7,3, 5,100,1,5,6,2,0,4,7,3,12,101,&
        1,5,6,2,0,4,7,3, 8,102,0,0,0,0,0,0,0,0, 0,  0,5,4,7,6,1,0,3,2, 6,104,&
        0,4,5,1,3,7,6,2,10,105,2,1,5,6,3,0,4,7,12,106,0,0,0,0,0,0,0,0, 0,  0,&
        5,6,2,1,4,7,3,0,11,108,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,7,6,5,4,3,2,1,0, 5,112,0,4,5,1,3,7,6,2,11,113,&
        6,5,4,7,2,1,0,3, 9,114,0,0,0,0,0,0,0,0, 0,  0,1,5,6,2,0,4,7,3,14,116,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        7,6,5,4,3,2,1,0,12,120,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,7,6,5,4,3,2,1,0, 1,128,&
        0,1,2,3,4,5,6,7, 3,129,1,2,3,0,5,6,7,4, 4,130,1,2,3,0,5,6,7,4, 6,131,&
        7,4,0,3,6,5,1,2, 3,132,1,5,6,2,0,4,7,3, 7,133,1,5,6,2,0,4,7,3, 6,134,&
        3,0,1,2,7,4,5,6,12,135,3,2,6,7,0,1,5,4, 2,136,4,0,3,7,5,1,2,6, 5,137,&
        7,4,0,3,6,5,1,2, 6,138,2,3,0,1,6,7,4,5,14,139,6,7,3,2,5,4,0,1, 5,140,&
        2,3,0,1,6,7,4,5, 9,141,1,2,3,0,5,6,7,4,11,142,0,0,0,0,0,0,0,0, 0,  0,&
        4,0,3,7,5,1,2,6, 2,144,3,7,4,0,2,6,5,1, 5,145,7,6,5,4,3,2,1,0, 6,146,&
        1,0,4,5,2,3,7,6,11,147,4,0,3,7,5,1,2,6, 6,148,3,7,4,0,2,6,5,1,12,149,&
        1,0,4,5,2,3,7,6,10,150,0,0,0,0,0,0,0,0, 0,  0,0,3,7,4,1,2,6,5, 5,152,&
        4,0,3,7,5,1,2,6, 8,153,0,3,7,4,1,2,6,5,12,154,0,0,0,0,0,0,0,0, 0,  0,&
        0,3,7,4,1,2,6,5,14,156,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,5,1,0,4,6,2,3,7, 3,160,1,2,3,0,5,6,7,4, 7,161,&
        1,0,4,5,2,3,7,6, 6,162,4,5,1,0,7,6,2,3,12,163,3,0,1,2,7,4,5,6, 7,164,&
        0,1,2,3,4,5,6,7,13,165,6,2,1,5,7,3,0,4,12,166,0,0,0,0,0,0,0,0, 0,  0,&
        3,2,6,7,0,1,5,4, 6,168,4,0,3,7,5,1,2,6,12,169,1,2,3,0,5,6,7,4,10,170,&
        0,0,0,0,0,0,0,0, 0,  0,6,7,3,2,5,4,0,1,12,172,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,6,5,4,7,2,1,0,3, 5,176,&
        0,4,5,1,3,7,6,2, 9,177,0,4,5,1,3,7,6,2,14,178,0,0,0,0,0,0,0,0, 0,  0,&
        6,5,4,7,2,1,0,3,12,180,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,5,4,7,6,1,0,3,2,11,184,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        7,3,2,6,4,0,1,5, 2,192,6,5,4,7,2,1,0,3, 6,193,7,3,2,6,4,0,1,5, 6,194,&
        0,3,7,4,1,2,6,5,10,195,3,2,6,7,0,1,5,4, 5,196,3,2,6,7,0,1,5,4,12,197,&
        3,2,6,7,0,1,5,4,14,198,0,0,0,0,0,0,0,0, 0,  0,2,6,7,3,1,5,4,0, 5,200,&
        0,3,7,4,1,2,6,5,11,201,2,6,7,3,1,5,4,0,12,202,0,0,0,0,0,0,0,0, 0,  0,&
        3,2,6,7,0,1,5,4, 8,204,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,5,4,7,6,1,0,3,2, 5,208,3,7,4,0,2,6,5,1,14,209,&
        5,4,7,6,1,0,3,2,12,210,0,0,0,0,0,0,0,0, 0,  0,4,7,6,5,0,3,2,1,11,212,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        6,7,3,2,5,4,0,1, 9,216,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,4,7,6,5,0,3,2,1, 5,224,&
        4,7,6,5,0,3,2,1,12,225,1,5,6,2,0,4,7,3,11,226,0,0,0,0,0,0,0,0, 0,  0,&
        7,6,5,4,3,2,1,0, 9,228,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,2,6,7,3,1,5,4,0,14,232,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        5,4,7,6,1,0,3,2, 8,240,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,0,0,0,0,0,0,0,0, 0,  0,&
        0,0,0,0,0,0,0,0, 0,  0&
        /

    INTEGER, TARGET,DIMENSION(0:14,0:12) :: PATHCCLIST
    INTEGER, DIMENSION(0:12,0:14) :: PATHCCLISTT
    DATA ((PATHCCLISTT(I,J),I=0,12),J=0,14) /&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        3, 0, 1, 2,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        6, 0, 1, 2, 2, 3, 0,-1,-1,-1,-1,-1,-1,&
        6, 0, 1, 2, 3, 4, 5,-1,-1,-1,-1,-1,-1,&
        6, 0, 1, 2, 3, 4, 5,-1,-1,-1,-1,-1,-1,&
        9, 0, 1, 2, 2, 3, 4, 0, 2, 4,-1,-1,-1,&
        9, 0, 1, 2, 2, 3, 0, 4, 5, 6,-1,-1,-1,&
        9, 0, 1, 2, 3, 4, 5, 6, 7, 8,-1,-1,-1,&
        6, 0, 1, 2, 2, 3, 0,-1,-1,-1,-1,-1,-1,&
        12, 0, 1, 5, 1, 4, 5, 1, 2, 4, 2, 3, 4,&
        12, 0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7,&
        12, 0, 1, 5, 1, 4, 5, 1, 2, 4, 2, 3, 4,&
        12, 0, 1, 2, 3, 4, 5, 3, 5, 6, 3, 6, 7,&
        12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,&
        12, 0, 1, 5, 1, 4, 5, 1, 2, 4, 2, 3, 4&
        /

    INTEGER, TARGET,DIMENSION(0:14,0:15) :: PATHCCLIST2
    INTEGER, DIMENSION(0:15,0:14) :: PATHCCLIST2T
    DATA ((PATHCCLIST2T(I,J),I=0,15),J=0,14) /&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        12, 0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        15, 0, 1, 2, 0, 2, 3, 4, 5, 6, 7, 8, 9, 7, 9,10,&
        15, 0, 1, 2, 3, 4, 5, 3, 5, 7, 3, 7, 8, 5, 6, 7,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        12, 0, 1, 2, 0, 2, 3, 4, 5, 6, 4, 6, 7,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        12, 0, 1, 2, 3, 4, 6, 3, 6, 7, 4, 5, 6,-1,-1,-1,&
        12, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1&
        /

    INTEGER, POINTER,DIMENSION(:) :: PATH
    INTEGER, TARGET,DIMENSION(0:14,0:12) :: PATHCCWLIST
    INTEGER, DIMENSION(0:12,0:14) :: PATHCCWLISTT
    DATA ((PATHCCWLISTT(I,J),I=0,12),J=0,14) /&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        3, 0, 2, 1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        6, 0, 2, 1, 0, 3, 2,-1,-1,-1,-1,-1,-1,&
        6, 0, 2, 1, 3, 5, 4,-1,-1,-1,-1,-1,-1,&
        6, 0, 2, 1, 3, 5, 4,-1,-1,-1,-1,-1,-1,&
        9, 0, 2, 1, 2, 4, 3, 0, 4, 2,-1,-1,-1,&
        9, 0, 2, 1, 0, 3, 2, 4, 6, 5,-1,-1,-1,&
        9, 0, 2, 1, 3, 5, 4, 6, 8, 7,-1,-1,-1,&
        6, 0, 2, 1, 0, 3, 2,-1,-1,-1,-1,-1,-1,&
        12, 0, 5, 1, 1, 5, 4, 1, 4, 2, 2, 4, 3,&
        12, 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6,&
        12, 0, 5, 1, 1, 5, 4, 1, 4, 2, 2, 4, 3,&
        12, 0, 2, 1, 3, 5, 4, 3, 6, 5, 3, 7, 6,&
        12, 0, 2, 1, 3, 5, 4, 6, 8, 7, 9,11,10,&
        12, 0, 5, 1, 1, 5, 4, 1, 4, 2, 2, 4, 3&
        /

    INTEGER, TARGET,DIMENSION(0:14,0:15) :: PATHCCWLIST2
    INTEGER, DIMENSION(0:15,0:14) :: PATHCCWLIST2T
    DATA ((PATHCCWLIST2T(I,J),I=0,15),J=0,14) /&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        12, 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        15, 0, 2, 1, 0, 3, 2, 4, 6, 5, 7, 9, 8, 7,10, 9,&
        15, 0, 2, 1, 3, 5, 4, 3, 7, 5, 3, 8, 7, 5, 7, 6,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        12, 0, 2, 1, 0, 3, 2, 4, 6, 5, 4, 7, 6,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        12, 0, 2, 1, 3, 6, 4, 3, 7, 6, 4, 6, 5,-1,-1,-1,&
        12, 0, 2, 1, 3, 5, 4, 6, 8, 7, 9,11,10,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1&
        /


    INTEGER, POINTER,DIMENSION(:) :: EDGES
    INTEGER, TARGET,DIMENSION(0:14,0:12) :: EDGELIST
    INTEGER, DIMENSION(0:12,0:14) :: EDGELISTT
    DATA ((EDGELISTT(I,J),I=0,12),J=0,14) /&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        3, 0, 4, 3,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        4, 0, 4, 7, 2,-1,-1,-1,-1,-1,-1,-1,-1,&
        6, 0, 4, 3, 7,11,10,-1,-1,-1,-1,-1,-1,&
        6, 0, 4, 3, 6,10, 9,-1,-1,-1,-1,-1,-1,&
        5, 0, 3, 7, 6, 5,-1,-1,-1,-1,-1,-1,-1,&
        7, 0, 4, 7, 2, 6,10, 9,-1,-1,-1,-1,-1,&
        9, 4, 8,11, 2, 3, 7, 6,10, 9,-1,-1,-1,&
        4, 4, 7, 6, 5,-1,-1,-1,-1,-1,-1,-1,-1,&
        6, 2, 6, 9, 8, 4, 3,-1,-1,-1,-1,-1,-1,&
        8, 0, 8,11, 3,10, 9, 1, 2,-1,-1,-1,-1,&
        6, 4, 3, 2,10, 9, 5,-1,-1,-1,-1,-1,-1,&
        8, 4, 8,11, 0, 3, 7, 6, 5,-1,-1,-1,-1,&
        12, 0, 4, 3, 7,11,10, 2, 6, 1, 8, 5, 9,&
        6, 3, 7, 6, 9, 8, 0,-1,-1,-1,-1,-1,-1&
        /

    INTEGER, TARGET,DIMENSION(0:14,0:15) :: EDGELIST2
    INTEGER, DIMENSION(0:15,0:14) :: EDGELIST2T
    DATA ((EDGELIST2T(I,J),I=0,15),J=0,14) /&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        8, 3, 0,10, 7, 0, 4,11,10,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        11, 7,10, 9, 4, 0, 4, 9, 0, 9, 6, 2,-1,-1,-1,-1,&
        9, 7,10,11, 3, 4, 8, 9, 6, 2,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        8, 0, 8, 9, 1, 3, 2,10,11,-1,-1,-1,-1,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,&
        8, 0, 3, 4, 8,11, 7, 6, 5,-1,-1,-1,-1,-1,-1,-1,&
        12, 4,11, 8, 0, 5, 1, 7, 3, 2, 9,10, 6,-1,-1,-1,&
        0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1&
        /

    REAL(FB) :: VMIN, VMAX
    INTEGER :: CASENUM, BIGGER, SIGN, n
    INTEGER, DIMENSION(0:7) :: PRODS=(/1,2,4,8,16,32,64,128/);
    REAL(FB), DIMENSION(0:7) :: XXVAL,YYVAL,ZZVAL
    INTEGER, DIMENSION(0:3) :: IXMIN=(/0,1,4,5/), IXMAX=(/2,3,6,7/)
    INTEGER, DIMENSION(0:3) :: IYMIN=(/0,3,4,7/), IYMAX=(/1,2,5,6/)
    INTEGER, DIMENSION(0:3) :: IZMIN=(/0,1,2,3/), IZMAX=(/4,5,6,7/)
    INTEGER :: TYPE2,THISTYPE2
    INTEGER :: NEDGES,NPATH
    INTEGER :: OUTOFBOUNDS, EDGE, V1, V2
    REAL(FB) :: VAL1, VAL2, DENOM, FACTOR
    REAL(FB) :: XX, YY, ZZ

    EDGE2VERTEX=TRANSPOSE(EDGE2VERTEXTT)
    CASES=TRANSPOSE(CASEST)
    PATHCCLIST=TRANSPOSE(PATHCCLISTT)
    PATHCCLIST2=TRANSPOSE(PATHCCLIST2T)
    PATHCCWLIST=TRANSPOSE(PATHCCWLISTT)
    PATHCCWLIST2=TRANSPOSE(PATHCCWLIST2T)
    EDGELIST=TRANSPOSE(EDGELISTT)
    EDGELIST2=TRANSPOSE(EDGELIST2T)

    VMIN=MIN(VALS(0),VALS(1),VALS(2),VALS(3),VALS(4),VALS(5),VALS(6),VALS(7))
    VMAX=MAX(VALS(0),VALS(1),VALS(2),VALS(3),VALS(4),VALS(5),VALS(6),VALS(7))

    NXYZV=0
    NTRIS=0

    IF (VMIN>LEVEL.OR.VMAX<LEVEL) RETURN

    CASENUM=0
    BIGGER=0
    SIGN=1

    DO n = 0, 7
        IF (VALS(n)>LEVEL) THEN
            BIGGER=BIGGER+1
            CASENUM = CASENUM + PRODS(n);
        end if
    END DO

    ! THERE ARE MORE NODES GREATER THAN THE ISO-SURFACE LEVEL THAN BELOW, SO
    !   SOLVE THE COMPLEMENTARY PROBLEM

    IF (BIGGER > 4) THEN
        SIGN=-1
        CASENUM=0
        DO n=0, 7
            IF (VALS(n)<LEVEL) THEN
                CASENUM = CASENUM + PRODS(n)
            end if
        END DO
    end if

    ! STUFF MIN AND MAX GRID DATA INTO A MORE CONVENIENT FORM
    !  ASSUMING THE FOLLOWING GRID NUMBERING SCHEME

    !       5-------6
    !     / |      /|
    !   /   |     / |
    !  4 -------7   |
    !  |    |   |   |
    !  Z    1---|---2
    !  |  Y     |  /
    !  |/       |/
    !  0--X-----3


    DO n=0, 3
        XXVAL(IXMIN(n)) = X(0);
        XXVAL(IXMAX(n)) = X(1);
        YYVAL(IYMIN(n)) = Y(0);
        YYVAL(IYMAX(n)) = Y(1);
        ZZVAL(IZMIN(n)) = Z(0);
        ZZVAL(IZMAX(n)) = Z(1);
    END DO

    IF (CASENUM<=0.OR.CASENUM>=255) THEN ! NO ISO-SURFACE
        NTRIS=0
        RETURN
    end if

    CASE2(0:9) => CASES(CASENUM,0:9)
    TYPE2 = CASE2(8);
    IF (TYPE2==0) THEN
        NTRIS=0
        RETURN
    end if

    IF (COMPCASE(TYPE2) == -1) THEN
        THISTYPE2=SIGN
    ELSE
        THISTYPE2=1
    end if

    IF (THISTYPE2 /= -1) THEN
        !EDGES = &(EDGELIST[TYPE][1]);
        EDGES(-1:11) => EDGELIST(TYPE2,0:12)
        IF (SIGN >=0) THEN
            ! PATH = &(PATHCCLIST[TYPE][1])   !  CONSTRUCT TRIANGLES CLOCK WISE
            PATH(-1:11) => PATHCCLIST(TYPE2,0:12)
        ELSE
            ! PATH = &(PATHCCWLIST[TYPE][1])  !  CONSTRUCT TRIANGLES COUNTER CLOCKWISE
            PATH(-1:11) => PATHCCWLIST(TYPE2,0:12)
        end if
    ELSE
        !EDGES = &(EDGELIST2[TYPE][1]);
        EDGES(-1:11) => EDGELIST2(TYPE2,0:12)
        IF (SIGN > 0) THEN
            ! PATH = &(PATHCCLIST2[TYPE][1])  !  CONSTRUCT TRIANGLES CLOCK WISE
            PATH(-1:14) => PATHCCLIST2(TYPE2,0:15)
        ELSE
            ! PATH = &(PATHCCWLIST2[TYPE][1]) !  CONSTRUCT TRIANGLES COUNTER CLOCKWISE
            PATH(-1:14) => PATHCCWLIST2(TYPE2,0:15)
        end if
    end if
    NPATH = PATH(-1);
    NEDGES = EDGES(-1);

    OUTOFBOUNDS=0
    DO n=0,NEDGES-1
        EDGE = EDGES(n)
        V1 = CASE2(EDGE2VERTEX(EDGE,0));
        V2 = CASE2(EDGE2VERTEX(EDGE,1));
        VAL1 = VALS(V1)-LEVEL
        VAL2 = VALS(V2)-LEVEL
        DENOM = VAL2 - VAL1
        FACTOR = 0.5_FB
        IF (DENOM /= 0.0_FB)FACTOR = -VAL1/DENOM
        XX = FMIX(FACTOR,XXVAL(V1),XXVAL(V2));
        YY = FMIX(FACTOR,YYVAL(V1),YYVAL(V2));
        ZZ = FMIX(FACTOR,ZZVAL(V1),ZZVAL(V2));
        XYZV_LOCAL(3*n) = XX;
        XYZV_LOCAL(3*n+1) = YY;
        XYZV_LOCAL(3*n+2) = ZZ;

    END DO

    ! COPY COORDINATES TO OUTPUT ARRAY

    NXYZV = NEDGES;
    NTRIS = NPATH/3;
    IF (NPATH > 0) THEN
        TRIS(0:NPATH-1) = PATH(0:NPATH-1)
    end if
    RETURN
    END SUBROUTINE GETISOBOX

    ! ------------------ UPDATEISOSURFACE ------------------------

    SUBROUTINE UPDATEISOSURFACE(XYZVERTS_BOX, NXYZVERTS_BOX, TRIS_BOX, NTRIS_BOX,  &
        XYZVERTS, NXYZVERTS, NXYZVERTS_MAX, TRIANGLES, NTRIANGLES, NTRIANGLES_MAX)
    REAL(FB), INTENT(IN), DIMENSION(0:35) :: XYZVERTS_BOX
    INTEGER, INTENT(IN) :: NXYZVERTS_BOX, NTRIS_BOX
    INTEGER, INTENT(IN), DIMENSION(0:14) :: TRIS_BOX
    REAL(FB), POINTER, DIMENSION(:) :: XYZVERTS
    INTEGER, INTENT(INOUT) :: NXYZVERTS, NXYZVERTS_MAX, NTRIANGLES, NTRIANGLES_MAX
    INTEGER, POINTER, DIMENSION(:) :: TRIANGLES

    INTEGER :: NXYZVERTS_NEW, NTRIANGLES_NEW

    NXYZVERTS_NEW = NXYZVERTS + NXYZVERTS_BOX
    NTRIANGLES_NEW = NTRIANGLES + NTRIS_BOX
    IF (1+NXYZVERTS_NEW > NXYZVERTS_MAX) THEN
        NXYZVERTS_MAX = 1+NXYZVERTS_NEW+1000
        CALL REALLOCATE_F('UPDATEISOSURFACES','XYZVERTS',XYZVERTS,3*NXYZVERTS,3*NXYZVERTS_MAX)
    end if
    IF (1+NTRIANGLES_NEW > NTRIANGLES_MAX) THEN
        NTRIANGLES_MAX = 1+NTRIANGLES_NEW+1000
        CALL REALLOCATE_I('UPDATEISOSURFACES','TRIANGLES',TRIANGLES,3*NTRIANGLES,3*NTRIANGLES_MAX)
    end if
    XYZVERTS(1+3*NXYZVERTS:3*NXYZVERTS_NEW) = XYZVERTS_BOX(0:3*NXYZVERTS_BOX-1)
    TRIANGLES(1+3*NTRIANGLES:3*NTRIANGLES_NEW) = NXYZVERTS+TRIS_BOX(0:3*NTRIS_BOX-1)
    NXYZVERTS = NXYZVERTS_NEW
    NTRIANGLES = NTRIANGLES_NEW
    RETURN
    END SUBROUTINE UPDATEISOSURFACE

    ! ------------------ REALLOCATE_I ------------------------

    SUBROUTINE REALLOCATE_I(ROUTINE,VAR,VALS,OLDSIZE,NEWSIZE)
    CHARACTER(LEN=*), INTENT(IN) :: ROUTINE, VAR
    INTEGER, DIMENSION(:), POINTER :: VALS
    INTEGER, INTENT(IN) :: OLDSIZE, NEWSIZE
    INTEGER, DIMENSION(:), ALLOCATABLE :: VALS_TEMP
    INTEGER :: MEMERR

    IF (OLDSIZE > 0) THEN
        ALLOCATE(VALS_TEMP(OLDSIZE),STAT=MEMERR)
        CALL ChkMemErr(ROUTINE,VAR//'_TEMP',MEMERR)
        VALS_TEMP(1:OLDSIZE) = VALS(1:OLDSIZE)
        DEALLOCATE(VALS)
    end if
    ALLOCATE(VALS(NEWSIZE),STAT=MEMERR)
    CALL ChkMemErr(ROUTINE,VAR,MEMERR)
    IF (OLDSIZE > 0) THEN
        VALS(1:OLDSIZE)=VALS_TEMP(1:OLDSIZE)
        DEALLOCATE(VALS_TEMP)
    end if
    RETURN
    END SUBROUTINE REALLOCATE_I

    ! ------------------ REALLOCATE_F ------------------------

    SUBROUTINE REALLOCATE_F(ROUTINE,VAR,VALS,OLDSIZE,NEWSIZE)
    CHARACTER(LEN=*), INTENT(IN) :: ROUTINE,VAR
    REAL(FB), INTENT(INOUT), DIMENSION(:), POINTER :: VALS
    INTEGER, INTENT(IN) :: OLDSIZE, NEWSIZE
    REAL(FB), DIMENSION(:), ALLOCATABLE :: VALS_TEMP
    INTEGER :: MEMERR

    IF (OLDSIZE > 0) THEN
        ALLOCATE(VALS_TEMP(OLDSIZE),STAT=MEMERR)
        CALL ChkMemErr(ROUTINE,VAR//'_TEMP',MEMERR)
        VALS_TEMP(1:OLDSIZE) = VALS(1:OLDSIZE)
        DEALLOCATE(VALS)
    end if
    ALLOCATE(VALS(NEWSIZE),STAT=MEMERR)
    CALL ChkMemErr(ROUTINE,VAR,MEMERR)
    IF (OLDSIZE > 0) THEN
        VALS(1:OLDSIZE)=VALS_TEMP(1:OLDSIZE)
        DEALLOCATE(VALS_TEMP)
    end if
    RETURN
    END SUBROUTINE REALLOCATE_F

    ! --------------------------- ChkMemErr -------------------------------------------

    subroutine chkmemerr(codesect,varname,izero)

    ! memory checking routine

    character(len=*), intent(in) :: codesect, varname
    integer izero

    if (izero==0) return

    write (errormessage,'(4a)') 'Error, Memory allocation failed for ', trim(varname),' in the routine ',trim(codesect)
    call cfastexit('chkmemerr',1)

    end subroutine chkmemerr

    END MODULE ISOSURFACE
