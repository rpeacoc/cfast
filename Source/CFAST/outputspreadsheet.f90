module spreadsheet_routines

    use precision_parameters

    use fire_routines, only : flame_height
    use target_routines, only: get_target_temperatures
    use opening_fractions, only : get_vent_opening
    use spreadsheet_header_routines
    use utility_routines, only: ssaddtolist, readcsvformat, tointstring
    
    use cfast_types, only: fire_type, ramp_type, room_type, detector_type, target_type, vent_type, dump_type, ssout_type, vent_type

    use cparams, only: u, l, in, out, mxrooms, mxfires, mxdtect, mxtarg, mxhvents, mxfslab, mxvvents, mxmvents, mxleaks, &
        ns, soot, soot_flaming, soot_smolder, smoked, mx_dumps, mxss, cjetvelocitymin, &
        n2, o2, co2, co, hcn, hcl, fuel, h2o, soot, soot_flaming, soot_smolder, ct, ts, &
        fuel_moles, fuel_Q, fuel_n2, fuel_o2, fuel_co2, fuel_co, fuel_hcn, fuel_hcl, fuel_h2o, fuel_soot
    use diag_data, only: radi_verification_flag
    use fire_data, only: n_fires, fireinfo
    use ramp_data, only: n_ramps, rampinfo
    use room_data, only: n_rooms, roominfo
    use setup_data, only: validation_flag, iofilsmvzone, iofilssc, iofilssd, iofilssw, iofilssm, iofilssv, &
        iofilssdiag, iofilcalc, iofill, ss_out_interval, project, extension
    use spreadsheet_output_data, only: n_sscomp, sscompinfo, n_ssdevice, ssdeviceinfo, n_sswall, sswallinfo, n_ssmass, &
        ssmassinfo, n_ssvent, ssventinfo, outarray
    use target_data, only: n_detectors, detectorinfo, n_targets, targetinfo
    use vent_data, only: n_hvents, hventinfo, n_vvents, vventinfo, n_mvents, mventinfo, n_leaks, leakinfo
    use dump_data, only: n_dumps, dumpinfo, csvnames, num_csvfiles, iocsv

    implicit none
    external cfastexit
    
    integer, dimension(4), parameter :: iwptr = (/1, 3, 4, 2/)

    private

    public output_spreadsheet, output_spreadsheet_smokeview, output_spreadsheet_dump

    contains
    

! --------------------------- output_spreadsheet -------------------------------------------

    subroutine output_spreadsheet(time)

    real(eb), intent(in) :: time

    call output_spreadsheet_compartments (time)
    call output_spreadsheet_devices (time)
    call output_spreadsheet_masses (time)
    call output_spreadsheet_vents (time)
    call output_spreadsheet_walls (time)
    
    if (radi_verification_flag) call output_spreadsheet_diag(time)

    return

    end subroutine output_spreadsheet

! --------------------------- output_spreadsheet_compartments ------------------------------------

    subroutine output_spreadsheet_compartments (time)
    
    ! writes compartment-related results to the {project}_compartments.csv file
    
    real(eb), intent(in) :: time
    
    logical :: firstc = .true.
    integer :: position, i
    character(len=35) :: cRoom, cFire, species_units, smoke_units
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr
    type(ssout_type), pointer :: ssptr
    
    save firstc
    
    !initialize header data for spreadsheet
    if (firstc) then
        n_sscomp = 0
        call ssaddtoheader (sscompinfo, n_sscomp, 'Time', 'Simulation Time', 'Time', 's')
            
        ! Compartment results
        do i = 1, n_rooms
            roomptr => roominfo(i)
            call toIntString(i,cRoom)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULT_'//trim(cRoom), 'Upper Layer Temperature', roomptr%id, 'C')
            if (.not. roomptr%shaft) then
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLT_'//trim(cRoom), 'Lower Layer Temperature', roomptr%id, 'C')
                call ssaddtoheader (sscompinfo, n_sscomp, 'HGT_'//trim(cRoom), 'Layer Height', roomptr%id, 'm')
            end if
            call ssaddtoheader (sscompinfo, n_sscomp, 'VOL_'//trim(cRoom), 'Upper Layer Volume', roomptr%id, 'm^3')
            call ssaddtoheader (sscompinfo, n_sscomp, 'PRS_'//trim(cRoom), 'Pressure', roomptr%id, 'Pa')
            if (validation_flag) then
                species_units = 'mol_frac'
                smoke_units = 'mg/m^3'
            else
                species_units = 'mol %'
                smoke_units = '1/m'
            end if 
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULN2_'//trim(cRoom), 'N2 Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULO2_'//trim(cRoom), 'O2 Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULCO2_'//trim(cRoom), 'CO2 Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULCO_'//trim(cRoom), 'CO Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULHCN_'//trim(cRoom), 'HCN Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULHCL_'//trim(cRoom), 'HCl Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULTUHC_'//trim(cRoom), 'Unburned Fuel Upper Layer', roomptr%id, &
                species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULH2O_'//trim(cRoom), 'H2O Upper Layer', roomptr%id, species_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULOD_'//trim(cRoom), 'Optical Density Upper Layer', roomptr%id, &
                smoke_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULODF_'//trim(cRoom), 'OD from Flaming Upper Layer', &
                roomptr%id, smoke_units)
            call ssaddtoheader (sscompinfo, n_sscomp, 'ULODS_'//trim(cRoom), 'OD from Smoldering Upper Layer', &
                roomptr%id,smoke_units)
            if (.not. roomptr%shaft) then
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLN2_'//trim(cRoom), 'N2 Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLO2_'//trim(cRoom), 'O2 Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLCO2_'//trim(cRoom), 'CO2 Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLCO_'//trim(cRoom), 'CO Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLHCN_'//trim(cRoom), 'HCN Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLHCL_'//trim(cRoom), 'HCl Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLTUHC_'//trim(cRoom), 'Unburned Fuel Lower Layer', roomptr%id, &
                    species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLH2O_'//trim(cRoom), 'H2O Lower Layer', roomptr%id, species_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLOD_'//trim(cRoom), 'Optical Density Lower Layer', roomptr%id, &
                    smoke_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLODF_'//trim(cRoom), 'OD from Flaming Lower Layer', roomptr%id, &
                    smoke_units)
                call ssaddtoheader (sscompinfo, n_sscomp, 'LLODS_'//trim(cRoom), 'OD from Smoldering Lower Layer', roomptr%id, &
                    smoke_units)
            end if
        end do
        
        ! Fire results
        do i = 1, n_fires
            fireptr => fireinfo(i)
            call toIntString(i,cFire)
            call ssaddtoheader (sscompinfo, n_sscomp, 'IGN_'//trim(cFire), 'Ignition', fireptr%id, ' ')
            call ssaddtoheader (sscompinfo, n_sscomp, 'PLUM_'//trim(cFire), 'Plume Entrainment Rate', fireptr%id, 'kg/s')
            call ssaddtoheader (sscompinfo, n_sscomp, 'PYROL_'//trim(cFire), 'Pyrolysis Rate', fireptr%id, 'kg/s')
            call ssaddtoheader (sscompinfo, n_sscomp, 'HRR_E'//trim(cFire), 'HRR Expected', fireptr%id, 'W')
            call ssaddtoheader (sscompinfo, n_sscomp, 'HRR_'//trim(cFire), 'HRR Actual', fireptr%id, 'W')
            call ssaddtoheader (sscompinfo, n_sscomp, 'HRRC_'//trim(cFire), 'HRR Convective Actual', fireptr%id, 'W')
            roomptr => roominfo(fireptr%room)
            if (.not. roomptr%shaft) then
                call ssaddtoheader (sscompinfo, n_sscomp, 'HRRL_'//trim(cFire), 'HRR Lower Actual', fireptr%id, 'W')
                call ssaddtoheader (sscompinfo, n_sscomp, 'HRRU_'//trim(cFire), 'HRR Upper Actual', fireptr%id, 'W')
            end if
            call ssaddtoheader (sscompinfo, n_sscomp, 'FLHGT_'//trim(cFire), 'Flame Height', fireptr%id, 'm')
        end do
        
        ! Door jet fire results
        do i = 1, n_rooms+1
            if (i==n_rooms+1) then
                call ssaddtoheader (sscompinfo, n_sscomp, 'DJET_'//'Outside', 'HRR Door Jet Fires', 'Outside', 'W')
            else
                call toIntString(i,cRoom)
                roomptr => roominfo(i)
                call ssaddtoheader (sscompinfo, n_sscomp, 'DJET_'//trim(cRoom), 'HRR Door Jet Fires', roomptr%id, 'W')
            end if
        end do
        
        ! write out header
        write (iofilssc,"(32767a)") (trim(sscompinfo(i)%short) // ',',i=1,n_sscomp-1),trim(sscompinfo(n_sscomp)%short)
        write (iofilssc,"(32767a)") (trim(sscompinfo(i)%measurement) // ',',i=1,n_sscomp-1),trim(sscompinfo(n_sscomp)%measurement)
        write (iofilssc,"(32767a)") (trim(sscompinfo(i)%device) // ',',i=1,n_sscomp-1),trim(sscompinfo(n_sscomp)%device)
        write (iofilssc,"(32767a)") (trim(sscompinfo(i)%units) // ',',i=1,n_sscomp-1),trim(sscompinfo(n_sscomp)%units)
        firstc = .false.
    end if

    !write out spreadsheet values for the current time step
    position = 0
    outarray = 0._eb
    do i = 1, n_sscomp
        ssptr => sscompinfo(i)
        call ssaddvaluetooutput (ssptr, time, position, outarray)
    end do
    
    call ssprintresults (iofilssc, position, outarray)
    return
    
    end subroutine output_spreadsheet_compartments

! --------------------------- output_spreadsheet_devices ------------------------------------

    subroutine output_spreadsheet_devices (time)
    
    ! writes device-related results to the {project}_devices.csv file

    real(eb), intent(in) :: time

    logical :: firstc = .true.
    integer :: position, i
    character(len=35) :: cDet
    type(ssout_type), pointer :: ssptr
    type(target_type), pointer :: targptr
    type(detector_type), pointer :: dtectptr

    save firstc

    ! initialize header data for spreadsheet
    if (firstc) then
        n_ssdevice = 0
        call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'Time', 'Simulation Time', 'Time', 's')

        ! targets
        do i = 1, n_targets
            call toIntString(i,cDet)
            targptr => targetinfo(i)
            ! front surface
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGGAST_'//trim(cDet), 'Target Surrounding Gas Temperature', &
                targptr%id, 'C')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGSURT_'//trim(cDet), 'Target Surface Temperature', &
                targptr%id, 'C')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGINT_'//trim(cDet), 'Target Internal Temperature', &
                targptr%id, 'C')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXI_'//trim(cDet), 'Target Incident Flux', &
                targptr%id, 'kW/m^2')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXT_'//trim(cDet), 'Target Net Flux', &
                targptr%id, 'kW/m^2')
            if (validation_flag) then
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXR_'//trim(cDet), 'Target Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXC_'//trim(cDet), 'Target Convective Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXF_'//trim(cDet), 'Target Fire Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXS_'//trim(cDet), 'Target Surface Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXG_'//trim(cDet), 'Target Gas Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXRE_'//trim(cDet), 'Target Radiative Loss Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXTG_'//trim(cDet), 'Target Total Gauge Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXRG_'//trim(cDet), 'Target Radiative Gauge Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXCG_'//trim(cDet), 'Target Convective Gauge Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFLXREG_'//trim(cDet), 'Target Radiative Loss Gauge Flux', &
                    targptr%id, 'kW/m^2')
            end if
            ! back surface
            if (validation_flag) then
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXI_'//trim(cDet), 'Back Target Incident Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXT_'//trim(cDet), 'Back Target Net Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXR_'//trim(cDet), 'Back Target Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXC_'//trim(cDet), 'Back Target Convective Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXF_'//trim(cDet), 'Back Target Fire Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXS_'//trim(cDet), 'Back Target Surface Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXG_'//trim(cDet), 'Back Target Gas Radiative Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXRE_'//trim(cDet), 'Back Target Radiative Loss Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXTG_'//trim(cDet), 'Back Target Total Gauge Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXRG_'//trim(cDet), 'Back Target Radiative Gauge Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXCG_'//trim(cDet), 'Back Target Convective Gauge Flux', &
                    targptr%id, 'kW/m^2')
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'B_TRGFLXREG_'//trim(cDet), 'Back Target Radiative Loss Gauge Flux', &
                    targptr%id, 'kW/m^2')
            end if
            ! tenability
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFEDG_'//trim(cDet), 'Target Gas FED', targptr%id, '')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGDFEDG_'//trim(cDet), 'Target Gas FED Increment', targptr%id, '')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGFEDH_'//trim(cDet), 'Target Heat FED', targptr%id, '')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGDFEDH_'//trim(cDet), 'Target Heat FED Increment', targptr%id, '')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'TRGOBS_'//trim(cDet), 'Target Obscuration', targptr%id, '1/m')
        end do
        
        
        ! detectors
        do i = 1, n_detectors
            call toIntString(i,cDet)
            dtectptr => detectorinfo(i)
            if (dtectptr%dtype==smoked) then
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'SENSOD_'//trim(cDet), 'Sensor Obscuration', dtectptr%id, '1/m')
            else
                call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'SENST_'//trim(cDet), 'Sensor Temperature', dtectptr%id, 'C')
            end if
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'SENSACT_'//trim(cDet), 'Sensor Activation', dtectptr%id, '1=yes')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'SENSGAST_'//trim(cDet), 'Sensor Surrounding Gas Temperature', &
                dtectptr%id, 'C')
            call ssaddtoheader (ssdeviceinfo, n_ssdevice, 'SENSGASV_'//trim(cDet), 'Sensor Surrounding Gas Velocity', &
                dtectptr%id, 'm/s')
        end do

        ! write out header
        write (iofilssd,"(32767a)") (trim(ssdeviceinfo(i)%short) // ',',i=1,n_ssdevice-1),trim(ssdeviceinfo(n_ssdevice)%short)
        write (iofilssd,"(32767a)") (trim(ssdeviceinfo(i)%measurement) // ',',i=1,n_ssdevice-1),&
            trim(ssdeviceinfo(n_ssdevice)%measurement)
        write (iofilssd,"(32767a)") (trim(ssdeviceinfo(i)%device) // ',',i=1,n_ssdevice-1),trim(ssdeviceinfo(n_ssdevice)%device)
        write (iofilssd,"(32767a)") (trim(ssdeviceinfo(i)%units) // ',',i=1,n_ssdevice-1),trim(ssdeviceinfo(n_ssdevice)%units)

        firstc = .false.
    end if

    ! write out spreadsheet values for the current time step
    position = 0
    outarray = 0._eb
    call get_target_temperatures
    do i = 1, n_ssdevice
        ssptr => ssdeviceinfo(i)
        call ssaddvaluetooutput (ssptr, time, position, outarray)
    end do

    call ssprintresults (iofilssd, position, outarray)
    return

    end subroutine output_spreadsheet_devices

! --------------------------- output_spreadsheet_masses ------------------------------------

    subroutine output_spreadsheet_masses (time)
    
    ! writes layer mass-related results to the {project}_masses.csv file
    
    real(eb), intent(in) :: time
    
    logical :: firstc = .true.
    integer :: position, i
    character(len=35) :: cRoom, cFire
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr
    type(ssout_type), pointer :: ssptr
    
    save firstc
    
    !initialize header data for spreadsheet
    if (firstc) then
        n_ssmass = 0
        call ssaddtoheader (ssmassinfo, n_ssmass, 'Time', 'Simulation Time', 'Time', 's')
            
        ! layer mass results
        do i = 1, n_rooms
            roomptr => roominfo(i)
            call toIntString(i,cRoom)
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMN2_'//trim(cRoom), 'N2 Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMO2_'//trim(cRoom), 'O2 Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMCO2_'//trim(cRoom), 'CO2 Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMCO_'//trim(cRoom), 'CO Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMHCN_'//trim(cRoom), 'HCN Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMHCL_'//trim(cRoom), 'HCl Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMTUHC_'//trim(cRoom), 'Unburned Fuel Upper Layer Mass', &
                roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMH2O_'//trim(cRoom), 'H2O Upper Layer Mass', roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMOD_'//trim(cRoom), 'Soot Upper Layer Mass', &
                roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMODF_'//trim(cRoom), 'Soot from Flaming Upper Layer Mass', &
                roomptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMODS_'//trim(cRoom), 'Soot from Smoldering Upper Layer Mass', &
                roomptr%id,'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMTS_'//trim(cRoom), 'Trace Species Upper Layer Mass', &
                roomptr%id,'kg')
            if (validation_flag) then
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMF_'//trim(cRoom), 'Fuel Upper Layer Mass', roomptr%id, 'mole')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPQ_'//trim(cRoom), 'Potential Total Heat Upper Layer', &
                    roomptr%id, 'J')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPMN2_'//trim(cRoom), 'Potential N2 Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPMO2_'//trim(cRoom), 'Potential O2 Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPMCO2_'//trim(cRoom), 'Potential CO2 Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPMCO_'//trim(cRoom), 'Potential CO Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPMHCN_'//trim(cRoom), 'Potential HCN Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULPMHCL_'//trim(cRoom), 'Potential HCl Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMPH2O_'//trim(cRoom), 'Potential H2O Upper Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'ULMSoot_'//trim(cRoom), 'Potential Soot Upper Layer Mass', &
                    roomptr%id, 'kg')
            end if
            if (.not. roomptr%shaft) then
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMN2_'//trim(cRoom), 'N2 Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMO2_'//trim(cRoom), 'O2 Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMCO2_'//trim(cRoom), 'CO2 Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMCO_'//trim(cRoom), 'CO Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMHCN_'//trim(cRoom), 'HCN Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMHCL_'//trim(cRoom), 'HCl Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMTUHC_'//trim(cRoom), 'Unburned Fuel Lower Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMH2O_'//trim(cRoom), 'H2O Lower Layer Mass', roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMOD_'//trim(cRoom), 'Soot Lower Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMODF_'//trim(cRoom), 'Soot from Flaming Lower Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMODS_'//trim(cRoom), 'Soot from Smoldering Lower Layer Mass', &
                    roomptr%id, 'kg')
                call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMTS_'//trim(cRoom), 'Trace Species Lower Layer Mass', &
                    roomptr%id,'kg')
                if (validation_flag) then
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMF_'//trim(cRoom), 'Fuel Lower Layer Mass', roomptr%id, 'mole')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPQ_'//trim(cRoom), 'Potential Total Heat Lower Layer', &
                        roomptr%id, 'J')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPMN2_'//trim(cRoom), 'Potential N2 Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPMO2_'//trim(cRoom), 'Potential O2 Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPMCO2_'//trim(cRoom), 'Potential CO2 Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPMCO_'//trim(cRoom), 'Potential CO Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPMHCN_'//trim(cRoom), 'Potential HCN Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLPMHCL_'//trim(cRoom), 'Potential HCl Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMPH2O_'//trim(cRoom), 'Potential H2O Lower Layer Mass', &
                        roomptr%id, 'kg')
                    call ssaddtoheader (ssmassinfo, n_ssmass, 'LLMSoot_'//trim(cRoom), 'Potential Soot Lower Layer Mass', &
                        roomptr%id, 'kg')
                end if
            end if
        end do
        
        ! total pyrolysate and trace species results
        do i = 1, n_fires
            fireptr => fireinfo(i)
            call toIntString(i,cFire)
            call ssaddtoheader (ssmassinfo, n_ssmass, 'PYTOL_T_'//trim(cFire), 'Total Pyrolysate Released', fireptr%id, 'kg')
            call ssaddtoheader (ssmassinfo, n_ssmass, 'TRACE_T_'//trim(cFire), 'Total Trace Species Released', fireptr%id, 'kg')
        end do
            
        
        ! write out header
        write (iofilssm,"(32767a)") (trim(ssmassinfo(i)%short) // ',',i=1,n_ssmass-1),trim(ssmassinfo(n_ssmass)%short)
        write (iofilssm,"(32767a)") (trim(ssmassinfo(i)%measurement) // ',',i=1,n_ssmass-1),trim(ssmassinfo(n_ssmass)%measurement)
        write (iofilssm,"(32767a)") (trim(ssmassinfo(i)%device) // ',',i=1,n_ssmass-1),trim(ssmassinfo(n_ssmass)%device)
        write (iofilssm,"(32767a)") (trim(ssmassinfo(i)%units) // ',',i=1,n_ssmass-1),trim(ssmassinfo(n_ssmass)%units)
        
        firstc = .false.
    end if

    !write out spreadsheet values for the current time step
    position = 0
    outarray = 0._eb
    do i = 1, n_ssmass
        ssptr => ssmassinfo(i)
        call ssaddvaluetooutput (ssptr, time, position, outarray)
    end do
    
    call ssprintresults (iofilssm, position, outarray)
    return
    
    end subroutine output_spreadsheet_masses

! --------------------------- output_spreadsheet_vents ------------------------------------

    subroutine output_spreadsheet_vents (time)
    
    ! writes layer vent-related results to the {project}_vents.csv file
    
    real(eb), intent(in) :: time
    
    logical :: firstc = .true.
    integer :: position, i, ifrom, ito, counter
    character(len=35) :: cifrom, cito, clfrom, clto, cvent
    type(vent_type), pointer :: ventptr
    type(ssout_type), pointer :: ssptr
    
    save firstc

    !initialize header data for spreadsheet
    if (firstc) then
        n_ssvent = 0
        call ssaddtoheader (ssventinfo, n_ssvent, 'Time', 'Simulation Time', 'Time', 's')

        ! wall vent results
        do i = 1, n_hvents
            ventptr => hventinfo(i)
            ifrom = ventptr%room1
            call tointstring(ifrom,cifrom)
            if (ifrom==n_rooms+1) cifrom = 'Outside'
            clfrom = 'Room_' //cifrom
            if (ifrom==n_rooms+1) clfrom = 'Outside'
            
            ito = ventptr%room2
            call tointstring(ito,cito)
            if (ito==n_rooms+1) cito = 'Outside'
            clto = 'Room_' //cito
            if (ito==n_rooms+1) clto = 'Outside'
            
            counter = ventptr%counter
            call tointstring(counter,cvent)

            call ssaddtoheader (ssventinfo, n_ssvent,'W_'//trim(cifrom)//'_'//trim(cito)//'_'//trim(cvent), &
                'Net Inflow',ventptr%id,'kg/s')
            call ssaddtoheader (ssventinfo, n_ssvent,'WF_'//trim(cito)//'_'//trim(cifrom)//'_'//trim(cvent), &
                'Opening Fraction',ventptr%id,'')
            if (validation_flag) then
                call ssaddtoheader (ssventinfo, n_ssvent,'WT_'//trim(cifrom)//'_u_inflow'//'_'//trim(cvent), &
                    'Total Inflow Upper',ventptr%id,'kg/s')  
                call ssaddtoheader (ssventinfo, n_ssvent,'WT_'//trim(cifrom)//'_u_outflow'//'_'//trim(cvent), &
                    'Total Outflow Upper',ventptr%id,'kg/s') 
                call ssaddtoheader (ssventinfo, n_ssvent,'WT_'//trim(cifrom)//'_l_inflow'//'_'//trim(cvent), &
                    'Total Inflow Lower',ventptr%id,'kg/s')  
                call ssaddtoheader (ssventinfo, n_ssvent,'WT_'//trim(cifrom)//'_l_outflow'//'_'//trim(cvent), &
                    'Total Outflow Lower',ventptr%id,'kg/s')
            end if
        end do 
        
        ! ceiling/floor vent results. note it's with respect to the bottom room, room 2
        do i = 1, n_vvents
            ventptr => vventinfo(i)
            ifrom = ventptr%room2
            call tointstring(ifrom,cifrom)
            if (ifrom==n_rooms+1) cifrom = 'Outside'
            clfrom = 'Room_' //cifrom
            if (ifrom==n_rooms+1) clfrom = 'Outside'
            
            ito = ventptr%room1
            call tointstring(ito,cito)
            if (ito==n_rooms+1) cito = 'Outside'
            clto = 'Room_' //cito
            if (ito==n_rooms+1) clto = 'Outside'
            
            counter = ventptr%counter
            call tointstring(counter,cvent)

            call ssaddtoheader (ssventinfo, n_ssvent,'CF_'//trim(cifrom)//'_'//trim(cito)//'_'//trim(cvent), &
                'Net Inflow',ventptr%id,'kg/s')
            call ssaddtoheader (ssventinfo, n_ssvent,'CFF_'//trim(cito)//'_'//trim(cifrom)//'_'//trim(cvent), &
                'Opening Fraction',ventptr%id,'')
            if (validation_flag) then
                call ssaddtoheader (ssventinfo, n_ssvent,'CFT_'//trim(cifrom)//'_u_inflow'//'_'//trim(cvent), &
                    'Total Inflow Upper',ventptr%id,'kg/s')  
                call ssaddtoheader (ssventinfo, n_ssvent,'CFT_'//trim(cifrom)//'_u_outflow'//'_'//trim(cvent), &
                    'Total Outflow Upper',ventptr%id,'kg/s')  
                call ssaddtoheader (ssventinfo, n_ssvent,'CFT_'//trim(cifrom)//'_l_inflow'//'_'//trim(cvent), &
                    'Total Inflow Lower',ventptr%id,'kg/s')   
                call ssaddtoheader (ssventinfo, n_ssvent,'CFT_'//trim(cifrom)//'_l_outflow'//'_'//trim(cvent), &
                    'Total Outflow Lower',ventptr%id,'kg/s') 
            end if
        end do  
        
        ! mechanical vent results
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            ifrom = ventptr%room1
            call tointstring(ifrom,cifrom)
            if (ifrom==n_rooms+1) cifrom = 'Outside'
            clfrom = 'Room_' //cifrom
            if (ifrom==n_rooms+1) clfrom = 'Outside'
            
            ito = ventptr%room2
            call tointstring(ito,cito)
            if (ito==n_rooms+1) cito = 'Outside'
            clto = 'Room_' //cito
            if (ito==n_rooms+1) clto = 'Outside'
            
            counter = ventptr%counter
            call tointstring(counter,cvent)

            call ssaddtoheader (ssventinfo, n_ssvent,'M_'//trim(cifrom)//'_'//trim(cito)//'_'//trim(cvent), &
                'Net Inflow',ventptr%id,'kg/s')
            call ssaddtoheader (ssventinfo, n_ssvent,'M_TRACE__'//trim(cifrom)//'_'//trim(cito)//'_'//trim(cvent), &
                'Trace Species Flow',ventptr%id,'kg')
            call ssaddtoheader (ssventinfo, n_ssvent,'M_FILTERED_'//trim(cifrom)//'_'//trim(cito)//'_'//trim(cvent), &
                'Trace Species Filtered',ventptr%id,'kg')
            call ssaddtoheader (ssventinfo, n_ssvent,'MF_'//trim(cito)//'_'//trim(cifrom)//'_'//trim(cvent), &
                'Opening Fraction',ventptr%id,'')
            if (validation_flag) then
                call ssaddtoheader (ssventinfo, n_ssvent,'MT_'//trim(cifrom)//'_u_inflow'//'_'//trim(cvent), &
                    'Total Inflow Upper',ventptr%id,'kg/s') 
                call ssaddtoheader (ssventinfo, n_ssvent,'MT_'//trim(cifrom)//'_u_outflow'//'_'//trim(cvent), &
                    'Total Outflow Upper',ventptr%id,'kg/s') 
                call ssaddtoheader (ssventinfo, n_ssvent,'MT_'//trim(cifrom)//'_l_inflow'//'_'//trim(cvent), &
                    'Total Inflow Lower',ventptr%id,'kg/s')   
                call ssaddtoheader (ssventinfo, n_ssvent,'MT_'//trim(cifrom)//'_l_outflow'//'_'//trim(cvent), &
                    'Total Outflow Lower',ventptr%id,'kg/s')
            end if
        end do  
        
        ! leakage results
        do i = 1, n_leaks
            ventptr => leakinfo(i)
            ifrom = ventptr%room1
            call tointstring(ifrom,cifrom)
            if (ifrom==n_rooms+1) cifrom = 'Outside'
            cifrom = 'Room_' //cifrom
            if (ifrom==n_rooms+1) cifrom = 'Outside'
            
            ito = ventptr%room2
            call tointstring(ito,cito)
            if (ito==n_rooms+1) cito = 'Outside'
            clto = 'Room_' //cito
            if (ito==n_rooms+1) clto = 'Outside'
            
            counter = ventptr%counter
            call tointstring(counter,cvent)

            call ssaddtoheader (ssventinfo, n_ssvent,'L_'//trim(cifrom)//'_'//trim(cito)//'_'//trim(cvent),'Net Inflow', &
                ventptr%id,'kg/s')
        end do       

        ! write out header
        write (iofilssv,"(32767a)") (trim(ssventinfo(i)%short) // ',',i=1,n_ssvent-1),trim(ssventinfo(n_ssvent)%short)
        write (iofilssv,"(32767a)") (trim(ssventinfo(i)%measurement) // ',',i=1,n_ssvent-1),trim(ssventinfo(n_ssvent)%measurement)
        write (iofilssv,"(32767a)") (trim(ssventinfo(i)%device) // ',',i=1,n_ssvent-1),trim(ssventinfo(n_ssvent)%device)
        write (iofilssv,"(32767a)") (trim(ssventinfo(i)%units) // ',',i=1,n_ssvent-1),trim(ssventinfo(n_ssvent)%units)
        
        firstc = .false.
    end if

    !write out spreadsheet values for the current time step
    position = 0
    outarray = 0._eb
    do i = 1, n_ssvent
        ssptr => ssventinfo(i)
        call ssaddvaluetooutput (ssptr, time, position, outarray)
    end do

    call ssprintresults (iofilssv, position, outarray)
    
    return
    
    end subroutine output_spreadsheet_vents

! --------------------------- output_spreadsheet_walls ------------------------------------

    subroutine output_spreadsheet_walls (time)
    
    ! writes wall surface-related results to the {project}_walls.csv file

    real(eb), intent(in) :: time

    logical :: firstc = .true.
    integer :: position, i
    character(len=35) :: cRoom
    type(room_type), pointer :: roomptr
    type(ssout_type), pointer :: ssptr

    save firstc

    ! initialize header data for spreadsheet
    if (firstc) then
        n_sswall = 0
        call ssaddtoheader (sswallinfo, n_sswall, 'Time', 'Simulation Time', 'Time', 's')

        ! compartment surface temperatures
        do i = 1, n_rooms
            call toIntString(i,cRoom)
            roomptr => roominfo(i)
            call ssaddtoheader (sswallinfo, n_sswall, 'CEILT_'//trim(cRoom), 'Ceiling Temperature', roomptr%id, 'C')
            call ssaddtoheader (sswallinfo, n_sswall, 'UWALLT_'//trim(cRoom), 'Upper Wall Temperature', roomptr%id, 'C')
            call ssaddtoheader (sswallinfo, n_sswall, 'LWALLT_'//trim(cRoom), 'Lower Wall Temperature', roomptr%id, 'C')
            call ssaddtoheader (sswallinfo, n_sswall, 'FLOORT_'//trim(cRoom), 'Floor Temperature', roomptr%id, 'C')
        end do

        ! write out header
        write (iofilssw,"(32767a)") (trim(sswallinfo(i)%short) // ',',i=1,n_sswall-1),trim(sswallinfo(n_sswall)%short)
        write (iofilssw,"(32767a)") (trim(sswallinfo(i)%measurement) // ',',i=1,n_sswall-1),trim(sswallinfo(n_sswall)%measurement)
        write (iofilssw,"(32767a)") (trim(sswallinfo(i)%device) // ',',i=1,n_sswall-1),trim(sswallinfo(n_sswall)%device)
        write (iofilssw,"(32767a)") (trim(sswallinfo(i)%units) // ',',i=1,n_sswall-1),trim(sswallinfo(n_sswall)%units)

        firstc = .false.
    end if

    ! write out spreadsheet values for the current time step
    position = 0
    outarray = 0._eb
    do i = 1, n_sswall
        ssptr => sswallinfo(i)
        call ssaddvaluetooutput (ssptr, time, position, outarray)
    end do

    call ssprintresults (iofilssw, position, outarray)
    return

    end subroutine output_spreadsheet_walls

! --------------------------- ssaddtoheader ------------------------------------

    subroutine ssaddtoheader (ssheaderinfo, i, short, long, location, units)
    
    ! adds header lines to spreadsheet output data structures
    
    integer, intent(inout) :: i
    type(ssout_type), intent(inout), target :: ssheaderinfo(*)
    character(len=*), intent(in) :: short, long, location, units
    
    type(ssout_type), pointer :: ssptr
    
    if (i<mxss) then
        i = i + 1
        ssptr => ssheaderinfo(i)
        ssptr%short = short
        ssptr%measurement = long
        ssptr%device = location
        ssptr%units = units
    end if
    
    return
    
    end subroutine ssaddtoheader

! --------------------------- ssaddvaluetooutput ------------------------------------

    subroutine ssaddvaluetooutput (ssptr, time, position, outarray)
    
    ! finds values for output in spreadsheet. Used by all main spreadsheet output routines
    
    type(ssout_type), intent(in) :: ssptr
    integer, intent(inout) :: position
    real(eb), intent(in) :: time
    real(eb), intent(out) :: outarray(*)
    
    integer i, idir, layer
    real(eb) :: fire_ignition, f_height, ssvalue, tjet, flow(4)
    
    character(len=50) :: measurement, device
    
    type(detector_type), pointer :: dtectptr
    type(fire_type), pointer :: fireptr
    type(room_type), pointer :: roomptr
    type(target_type), pointer :: targptr
    type(vent_type), pointer :: ventptr

    measurement = ssptr%measurement
    device = ssptr%device
    
    select case (measurement)
    case ('Simulation Time')
        call ssaddtolist (position, time, outarray)

        ! compartment related outputs
    case ('Upper Layer Temperature', 'Lower Layer Temperature')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%temp(layer)-kelvin_c_offset, outarray)
            end if
        end do
    case ('Layer Height')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%depth(l), outarray)
            end if
        end do
    case ('Upper Layer Volume', 'Lower Layer Volume')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%volume(layer), outarray)
            end if
        end do
    case ('Pressure')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position,roomptr%relp - roomptr%interior_relp_initial ,outarray)
            end if
        end do
        
        ! compartment surfaces
    case ('Ceiling Temperature')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%t_surfaces(1,iwptr(1))-kelvin_c_offset, outarray)
            end if
        end do
    case ('Upper Wall Temperature')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%t_surfaces(1,iwptr(2))-kelvin_c_offset, outarray)
            end if
        end do
    case ('Lower Wall Temperature')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%t_surfaces(1,iwptr(3))-kelvin_c_offset, outarray)
            end if
        end do
    case ('Floor Temperature')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                call ssaddtolist (position, roomptr%t_surfaces(1,iwptr(4))-kelvin_c_offset, outarray)
            end if
        end do
        
        ! species
    case ('N2 Upper Layer', 'N2 Upper Layer Mass', 'N2 Lower Layer', 'N2 Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,n2)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,n2)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('O2 Upper Layer', 'O2 Upper Layer Mass', 'O2 Lower Layer', 'O2 Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,o2)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,o2)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('CO2 Upper Layer', 'CO2 Upper Layer Mass', 'CO2 Lower Layer', 'CO2 Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,co2)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,co2)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('CO Upper Layer', 'CO Upper Layer Mass', 'CO Lower Layer', 'CO Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,co)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,co)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('HCN Upper Layer', 'HCN Upper Layer Mass', 'HCN Lower Layer', 'HCN Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,hcn)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,hcn)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('HCl Upper Layer', 'HCl Upper Layer Mass', 'HCl Lower Layer', 'HCl Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,hcl)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,hcl)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Unburned Fuel Upper Layer', 'Unburned Fuel Upper Layer Mass', 'Unburned Fuel Lower Layer', &
          'Unburned Fuel Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,fuel)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,fuel)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('H2O Upper Layer', 'H2O Upper Layer Mass', 'H2O Lower Layer', 'H2O Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,h2o)
                    if (validation_flag) ssvalue = ssvalue*0.01_eb ! converts molar % to  molar fraction
                else
                    ssvalue = roomptr%species_mass(layer,h2o)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Optical Density Upper Layer', 'Soot Upper Layer Mass', 'Optical Density Lower Layer', &
          'Soot Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,soot)
                    if (validation_flag) ssvalue = ssvalue*264.6903_eb ! converts converts od to mg/m^3
                else
                    ssvalue = roomptr%species_mass(layer,soot)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('OD from Flaming Upper Layer', 'Soot from Flaming Upper Layer Mass', 'OD from Flaming Lower Layer', &
          'Soot from Flaming Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,soot_flaming)
                    if (validation_flag) ssvalue = ssvalue*264.6903_eb ! converts converts od to mg/m^3
                else
                    ssvalue = roomptr%species_mass(layer,soot_flaming)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('OD from Smoldering Upper Layer', 'Soot from Smoldering Upper Layer Mass', 'OD from Smoldering Lower Layer', &
          'Soot from Smoldering Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                if (index(measurement,'Mass')==0) then
                    ssvalue = roomptr%species_output(layer,soot_smolder)
                    if (validation_flag) ssvalue = ssvalue*264.6903_eb ! converts converts od to mg/m^3
                else
                    ssvalue = roomptr%species_mass(layer,soot_smolder)
                end if
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Trace Species Upper Layer Mass', 'Trace Species Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_mass(layer,ts)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
        
        ! unburned fuel-related outputs
    case ('Fuel Upper Layer Mass', 'Fuel Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_moles)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential Total Heat Upper Layer', 'Potential Total Heat Lower Layer')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_Q)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential N2 Upper Layer Mass', 'Potential N2 Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_n2)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential O2 Upper Layer Mass', 'Potential O2 Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_o2)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential CO2 Upper Layer Mass', 'Potential CO2 Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_co2)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential CO Upper Layer Mass', 'Potential CO Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_co)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential HCN Upper Layer Mass', 'Potential HCN Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_hcn)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential HCl Upper Layer Mass', 'Potential HCl Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_hcl)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential H2O Upper Layer Mass', 'Potential H2O Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_h2o)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do
    case ('Potential Soot Upper Layer Mass', 'Potential Soot Lower Layer Mass')
        do i = 1, n_rooms+1
            layer = u
            if (index(measurement,'Upper')==0) layer = l
            roomptr => roominfo(i)
            if (roomptr%id==device) then
                ssvalue = roomptr%species_output(layer,fuel_soot)
                call ssaddtolist (position,ssvalue ,outarray)
            end if
        end do

        ! vent-related outputs
    case ('Net Inflow')
        ! wall vents
        do i = 1, n_hvents
            ventptr => hventinfo(i)
            if (ventptr%id==device) then
                ssvalue = ventptr%h_mflow(2,1,1) - ventptr%h_mflow(2,1,2) + ventptr%h_mflow(2,2,1) - ventptr%h_mflow(2,2,2)
                call ssaddtolist (position, ssvalue ,outarray)
            end if
        end do
        
        ! ceiling/floor vents
        do i = 1, n_vvents
            ventptr => vventinfo(i)
            if (ventptr%id==device) then
                flow = 0.0_eb
                if (ventptr%mflow(2,u)>=0.0_eb) flow(1) = ventptr%mflow(2,u)
                if (ventptr%mflow(2,u)<0.0_eb) flow(2) = -ventptr%mflow(2,u)
                if (ventptr%mflow(2,l)>=0.0_eb) flow(3) = ventptr%mflow(2,l)
                if (ventptr%mflow(2,l)<0.0_eb) flow(4) = -ventptr%mflow(2,l)
                ssvalue = flow(1) + flow(3) - flow(2) - flow(4)
                call ssaddtolist (position, ssvalue, outarray)
            end if
        end do

        ! mechanical vents
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            if (ventptr%id==device) then
                flow = 0.0_eb
                if (ventptr%mflow(2,u)>=0.0_eb) flow(1) = ventptr%mflow(2,u)
                if (ventptr%mflow(2,u)<0.0_eb) flow(2) = -ventptr%mflow(2,u)
                if (ventptr%mflow(2,l)>=0.0_eb) flow(3) = ventptr%mflow(2,l)
                if (ventptr%mflow(2,l)<0.0_eb) flow(4) = -ventptr%mflow(2,l)
                ssvalue = flow(1) + flow(3) - flow(2) - flow(4)
                call ssaddtolist (position, ssvalue, outarray)
            end if
        end do

        !leaks
        do i = 1, n_leaks
            ventptr => leakinfo(i)
            if (ventptr%id==device) then
                ssvalue = ventptr%h_mflow(2,1,1) - ventptr%h_mflow(2,1,2) + ventptr%h_mflow(2,2,1) - ventptr%h_mflow(2,2,2)
                call ssaddtolist (position, ssvalue ,outarray)
            end if
        end do
    case ('Total Inflow Upper', 'Total Inflow Lower','Total Outflow Upper', 'Total Outflow Lower')
        layer = u
        if (index(measurement,'Upper')==0) layer = l
        idir = in
        if (index(measurement,'Inflow')==0) idir = out
        ! wall vents
        do i = 1, n_hvents
            ventptr => hventinfo(i)
            ! note that flows are relative to the first compartment
            if (ventptr%id==device) then
                call ssaddtolist (position, ventptr%h_mflow(1,layer,idir), outarray)
            end if
        end do
        ! ceiling / floor vents
        do i = 1, n_vvents
            ventptr => vventinfo(i)
            ssvalue = 0.0_eb
            ! note that flows are relative to the bottom compartment
            if (ventptr%id==device) then
                if (idir==in) then
                    ssvalue = max(0._eb,ventptr%mflow(1,layer))
                else
                    ssvalue = max(0._eb,-ventptr%mflow(1,layer))
                end if
                call ssaddtolist (position, ssvalue, outarray)
            end if
        end do
        ! mechanical vents
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            ! note that flows are relative to the from compartment
            if (ventptr%id==device) then
                if (idir==in) then
                    ssvalue = max(0._eb,ventptr%mflow(1,layer))
                else
                    ssvalue = max(0._eb,-ventptr%mflow(1,layer))
                end if
                call ssaddtolist (position, ssvalue, outarray)
            end if
        end do
    case ('Opening Fraction')
        ! wall vents
        do i = 1, n_hvents
            ventptr => hventinfo(i)
            if (ventptr%id==device) then
                call ssaddtolist (position, ventptr%opening_fraction, outarray)
            end if
        end do
        ! ceiling / floor vents
        do i = 1, n_vvents
            ventptr => vventinfo(i)
            if (ventptr%id==device) then
                call ssaddtolist (position, ventptr%opening_fraction, outarray)
            end if
        end do
        ! mechanical vents
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            if (ventptr%id==device) then
                call ssaddtolist (position, ventptr%opening_fraction, outarray)
            end if
        end do
    case ('Trace Species Flow', 'Trace Species Filtered')
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            if (ventptr%id==device) then
                if (measurement=='Trace Species Flow') then
                    ssvalue =abs(ventptr%total_trace_flow(u))+abs(ventptr%total_trace_flow(l))
                else
                    ssvalue =abs(ventptr%total_trace_filtered(u))+abs(ventptr%total_trace_filtered(l))
                end if
                call ssaddtolist (position, ssvalue, outarray)
            end if
        end do
        
        ! fire-related outputs
    case ('Ignition')
        do i = 1, n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                if (fireptr%ignited) then
                    fire_ignition = 1
                else
                    fire_ignition = 0
                end if
                call ssaddtolist (position,fire_ignition,outarray)
            end if
        end do
    case ('Plume Entrainment Rate')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%mdot_entrained,outarray)
            end if
        end do
    case ('Pyrolysis Rate')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%mdot_pyrolysis,outarray)
            end if
        end do
    case ('HRR Expected')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%qdot_theoretical,outarray)
            end if
        end do
    case ('HRR Actual')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%qdot_actual,outarray)
            end if
        end do
    case ('HRR Convective Actual')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%qdot_convective,outarray)
            end if
        end do
    case ('HRR Upper Actual')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%qdot_layers(u),outarray)
            end if
        end do
    case ('HRR Lower Actual')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%qdot_layers(l),outarray)
            end if
        end do
    case ('Flame Height')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                f_height = flame_height (fireptr%qdot_actual,fireptr%firearea)
                call ssaddtolist (position,fireptr%qdot_layers(l),outarray)
            end if
        end do
    case ('HRR Door Jet Fires')
        do i = 1, n_rooms+1
            roomptr => roominfo(i)
            if (i<n_rooms+1.and.roomptr%id==device) then
                call ssaddtolist (position,roomptr%qdot_doorjet,outarray)
            else if (i==n_rooms+1.and.device=='Outside') then
                call ssaddtolist (position,roomptr%qdot_doorjet,outarray)
            end if
        end do
    case ('Total Pyrolysate Released')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%total_pyrolysate,outarray)
            end if
        end do
    case ('Total Trace Species Released')
        do i = 1,n_fires
            fireptr => fireinfo(i)
            if (fireptr%id==device) then
                call ssaddtolist (position,fireptr%total_trace,outarray)
            end if
        end do
    
        ! device-related outputs
    case ('Target Surrounding Gas Temperature')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%tgas-kelvin_c_offset,outarray)
            end if
        end do
    case ('Target Surface Temperature')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%tfront-kelvin_c_offset,outarray)
            end if
        end do
    case ('Target Internal Temperature')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%tinternal-kelvin_c_offset,outarray)
            end if
        end do
    case ('Target Incident Flux', 'Back Target Incident Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_incident_front/1000._eb,outarray)
                else
                    call ssaddtolist (position, targptr%flux_incident_back/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Net Flux','Back Target Net Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_net(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_net(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Gas FED')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%fed_gas,outarray)
            end if
        end do
    case ('Target Gas FED Increment')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%dfed_gas,outarray)
            end if
        end do
    case ('Target Heat FED')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%fed_heat,outarray)
            end if
        end do
    case ('Target Heat FED Increment')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%dfed_heat,outarray)
            end if
        end do
    case ('Target Obscuration')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                call ssaddtolist (position,targptr%fed_obs,outarray)
            end if
        end do
    case ('Target Radiative Flux','Back Target Radiative Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_radiation(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_radiation(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Convective Flux','Back Target Convective Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_convection(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_convection(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Fire Radiative Flux','Back Target Fire Radiative Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_fire(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_fire(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Surface Radiative Flux','Back Target Surface Radiative Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_surface(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_surface(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Gas Radiative Flux','Back Target Gas Radiative Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_gas(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_gas(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Radiative Loss Flux','Back Target Radiative Loss Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_target(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_target(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Total Gauge Flux','Back Target Total Gauge Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_net_gauge(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_net_gauge(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Radiative Gauge Flux','Back Target Radiative Gauge Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_radiation_gauge(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_radiation_gauge(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Convective Gauge Flux','Back Target Convective Gauge Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_convection_gauge(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_convection_gauge(2)/1000._eb,outarray)
                end if
            end if
        end do
    case ('Target Radiative Loss Gauge Flux','Back Target Radiative Loss Gauge Flux')
        do i = 1, n_targets
            targptr => targetinfo(i)
            if (targptr%id==device) then
                if (index(measurement,'Back')==0) then
                    call ssaddtolist (position,targptr%flux_target_gauge(1)/1000._eb,outarray)
                else
                    call ssaddtolist (position,targptr%flux_target_gauge(2)/1000._eb,outarray)
                end if
            end if
        end do
        
        ! detectors
    case ('Sensor Surrounding Gas Temperature')
        do i = 1, n_detectors
            dtectptr =>detectorinfo(i)
            if (dtectptr%id==device) then
                roomptr => roominfo(dtectptr%room)
                if (dtectptr%center(3)>roomptr%depth(l)) then
                    tjet = max(dtectptr%temp_gas,roomptr%temp(u))
                else
                    tjet = max(dtectptr%temp_gas,roomptr%temp(l))
                end if
                call ssaddtolist (position, tjet-kelvin_c_offset,outarray)
            end if
        end do
    case ('Sensor Surrounding Gas Velocity')
        do i = 1, n_detectors
            dtectptr =>detectorinfo(i)
            if (dtectptr%id==device) then
                call ssaddtolist (position,max(dtectptr%velocity,cjetvelocitymin),outarray)
            end if
        end do
    case ('Sensor Obscuration')
        do i = 1, n_detectors
            dtectptr =>detectorinfo(i)
            if (dtectptr%id==device) then
                call ssaddtolist (position, dtectptr%value,outarray)
            end if
        end do
    case ('Sensor Temperature')
        do i = 1, n_detectors
            dtectptr =>detectorinfo(i)
            if (dtectptr%id==device) then
                call ssaddtolist (position, dtectptr%value-kelvin_c_offset,outarray)
            end if
        end do
    case ('Sensor Activation')
        do i = 1, n_detectors
            dtectptr =>detectorinfo(i)
            if (dtectptr%id==device) then
                if (dtectptr%activated) then
                    call ssaddtolist (position, 1._eb,outarray)
                else
                    call ssaddtolist (position, 0._eb,outarray)
                end if
            end if
        end do
        
        ! output not found. this is an internal error and cause for alarm
    case default
        write(*, '(2a)') '***Error in spreadsheet output: Output measurement not found, ' , trim(measurement)
        write(iofill, '(2a)') '***Error in spreadsheet output: Output measurement not found, ' , trim(measurement)
        call cfastexit('outputspreadsheet',1)
        stop
    end select
    
    return

    end subroutine ssaddvaluetooutput

    subroutine ssprintresults (iounit, ic, array)
    
    ! prints a row of spreadsheet output

    real(eb), intent(in) :: array(*)
    integer, intent(in) :: iounit, ic

    integer i
    character(len=35), dimension(16384) :: out
    
    if (ic>0) then
        out = ' '
        do i = 1, ic
            if (validation_flag) then
                write (out(i),"(e19.12)" ) array(i)
            else
                write (out(i),"(e13.6)" ) array(i)
            end if
        end do
        write (iounit,"(16384a)") (trim(out(i)) // ',',i=1,ic-1),out(ic)
    end if
    
    return

    end subroutine ssprintresults

! --------------------------- output_spreadsheet_smokeview -------------------------------------------

    subroutine output_spreadsheet_smokeview (time)

    ! writes to the {project}_zone.csv file, the smokeview information

    real(eb), intent(in) :: time

    real(eb) :: f_height, avent, slabs, vflow
    logical :: firstc
    integer :: position
    integer :: i, j


    type(vent_type), pointer :: ventptr
    type(room_type), pointer :: roomptr
    type(fire_type), pointer :: fireptr
    type(target_type), pointer :: targptr

    data firstc/.true./
    save firstc

    ! Headers
    if (firstc) then
        call ssheaders_smv(.true.)
        firstc = .false.
    end if

    position = 0
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, n_rooms
        roomptr => roominfo(i)
        call ssaddtolist(position,roomptr%temp(u)-kelvin_c_offset,outarray)
        if (.not.roomptr%shaft) then
            call ssaddtolist(position,roomptr%temp(l)-kelvin_c_offset,outarray)
            call ssaddtolist(position,roomptr%depth(l),outarray)
        end if
        call ssaddtolist(position,roomptr%relp,outarray)
        call ssaddtolist(position,roomptr%rho(u),outarray)
        if (.not.roomptr%shaft) call ssaddtolist(position,roomptr%rho(l),outarray)
        call ssaddtolist(position,roomptr%species_output(u,soot),outarray)
        if (.not.roomptr%shaft) call ssaddtolist(position,roomptr%species_output(l,soot),outarray)
        do j = 1, 4
            call ssaddtolist(position,roomptr%t_surfaces(1,iwptr(j))-kelvin_c_offset,outarray)
        end do
    end do

    ! fires
    if (n_fires/=0) then
        do i = 1, n_fires
            fireptr => fireinfo(i)
            f_height = flame_height (fireptr%qdot_actual,fireptr%firearea)
            call ssaddtolist (position,fireptr%qdot_actual/1000.,outarray)
            call ssaddtolist (position,f_height,outarray)
            call ssaddtolist (position,fireptr%z_position+fireptr%z_offset,outarray)
            call ssaddtolist (position,fireptr%firearea,outarray)
        end do
    end if

    ! horizontal vents
    do i = 1, n_hvents
        ventptr=>hventinfo(i)
        avent = ventptr%current_area
        call ssaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call ssaddtolist (position,slabs,outarray)
        do j = 1, mxfslab
            call ssaddtolist(position,ventptr%temp_slab(j),outarray)
            call ssaddtolist(position,ventptr%flow_slab(j),outarray)
            call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
            call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
        end do
    end do

    ! vertical vents
    do i = 1, n_vvents
        ventptr => vventinfo(i)
        avent = ventptr%current_area
        call ssaddtolist (position,avent,outarray)
        ! flow slabs for the vent
        slabs = ventptr%n_slabs
        call ssaddtolist (position,slabs,outarray)
        do j = 2, 1, -1
            vflow = ventptr%flow_slab(j)
            if (ventptr%room1<=n_rooms.and.j==1) vflow = -vflow
            call ssaddtolist(position,ventptr%temp_slab(j),outarray)
            call ssaddtolist(position,vflow,outarray)
            call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
            call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
        end do
    end do

    ! mechanical vents (note sign of flow is different here to make it relative to compartment instead of hvac system
    if (n_mvents/=0) then
        do i = 1, n_mvents
            ventptr => mventinfo(i)
            avent = ventptr%current_area
            call ssaddtolist (position,avent,outarray)
            ! flow slabs for the vent
            slabs = ventptr%n_slabs
            call ssaddtolist (position,slabs,outarray)
            do j = 1, 2
                call ssaddtolist(position,ventptr%temp_slab(j),outarray)
                if (ventptr%room1<=n_rooms) then
                call ssaddtolist(position,-ventptr%flow_slab(j),outarray)
                else
                call ssaddtolist(position,ventptr%flow_slab(j),outarray)
                end if
                call ssaddtolist(position,ventptr%ybot_slab(j),outarray)
                call ssaddtolist(position,ventptr%ytop_slab(j),outarray)
            end do
        end do
    end if
    
    ! target temperature
    if (n_targets/=0) then
        call get_target_temperatures
        do i = 1, n_targets
            targptr => targetinfo(i)
            call ssaddtolist(position,targptr%tinternal-kelvin_c_offset,outarray)
        end do
    end if
    call ssprintresults (iofilsmvzone, position, outarray)

    return
    end subroutine output_spreadsheet_smokeview
    
    ! --------------------------- output_spreadsheet_diag -------------------------------------------

    subroutine output_spreadsheet_diag (time)

    ! writes to the {project}_diagnostics.csv file, the diagnostic parameters

    real(eb), intent(in) :: time

    logical :: firstc = .true.
    integer :: position, i, j
    type(room_type), pointer :: roomptr

    save firstc

    ! headers
    if (firstc) then
        call ssheaders_diagnosis
        firstc = .false.
    end if

    position = 0
    outarray = 0._eb
    call ssaddtolist (position,time,outarray)

    ! compartment information
    do i = 1, n_rooms
        roomptr => roominfo(i)
        do j = 1, 10
            call ssaddtolist (position,roomptr%chi(j),outarray)
        end do
    end do

    call ssprintresults (iofilssdiag, position, outarray)
    
    return
    end subroutine output_spreadsheet_diag
    
    !--------------------------output_spreadsheet_dump-----------------------------------------------------------
    
    subroutine output_spreadsheet_dump 
    
    integer, parameter :: nr = 2, nc = mx_dumps+1
    real(eb) :: dumparray(nr, nc)
    character(len=128) :: dumpcarray(nr, nc)
    integer :: i, icount, mxcol
    
    dumparray(1, 1:nc) = 0.0
    dumparray(2, 2:nc) = -1001
    dumpcarray(1:nr, 2:nc) = 'NO VALUE ASSIGNED'
    dumparray(2,1) = 0.0
    dumpcarray(1,1) = 'File Name'
    dumpcarray(2,1) = trim(project) // trim(extension)
    mxcol = 0 
    if (ss_out_interval>0 .and. n_dumps > 0) then 
        icount = n_dumps
        do i = 1, num_csvfiles
            if (icount>0) then
                call do_csvfile(nr, nc, dumparray, dumpcarray, i, icount, mxcol)
            else 
                exit
            end if
        end do
        call writecsvformat(iofilcalc, dumparray, dumpcarray, nr, nc, 1, 2, mxcol)
    end if      
    
    return
    end subroutine output_spreadsheet_dump
    
    !--------------------do_csvfile---------------------------------------
    
    subroutine do_csvfile(nr, nc, dumparray, dumpcarray, idx, icount, mxcol)
    
    integer, intent(in) :: nr, nc, idx
    integer, intent(inout) :: mxcol, icount
    real(eb), intent(inout) :: dumparray(nr, nc)
    character(len=*), intent(inout) :: dumpcarray(nr, nc)
    
    integer :: i
    type(dump_type), pointer :: dumpptr
    logical :: first, lend
    
    integer, parameter :: numr = 3, numc = 32000
    real(eb) :: lastval(2, mx_dumps), lasttime(mx_dumps), x(numr, numc)
    character(len=128) :: header(numr, numc), c(numr, numc)
    
    integer :: relcol, mxhr, mxhc, ic, cols(mx_dumps), icol, num_entries
    integer :: primecol(mx_dumps), seccol(2, mx_dumps), mxr, mxc
    real(eb) :: dummy(2, mx_dumps)
    
    lastval = 0.0_eb
    lasttime = 0.0_eb
    x = 0.0_eb
    header = ' '
    c = ' '
    cols = 0
    primecol = 0
    seccol = 0
    dummy = 0.0_eb
    
    first = .true.
    num_entries = 0
    icol = 0
    do i = 1, n_dumps
        if (icount>0)  then
            dumpptr => dumpinfo(i)
            dumpptr%found = .false.
            if (dumpptr%file_type==csvnames(idx)) then
                num_entries = num_entries + 1
                relcol = dumpptr%relative_column + 1
                icount = icount - 1
                icol = icol + 1
                cols(icol) = i
                mxcol = max(mxcol, relcol)
                dumpcarray(1,relcol) = dumpptr%id
                if (first) then
                    rewind(iocsv(idx))
                    call readcsvformat(iocsv(idx), x, header, numr, numc, 2, 3, mxhr, mxhc, lend, iofill)
                    first = .false. 
                    if (lend) then 
                        return
                    end if
                end if 
                call fnd_col(ic, header, numr, numc, mxhr, mxhc, dumpptr%first_device, dumpptr%first_measurement)
                primecol(cols(icol)) = ic
                if (ic>0) then
                    dumpptr%found = .true.
                end if
                if ((dumpptr%type(1:8) == 'TRIGGER_' .or. &
                        dumpptr%type(1:9) == 'INTEGRATE').and.dumpptr%found) then 
                    call fnd_col(ic, header, numr, numc, mxhr, mxhc, dumpptr%second_device, &
                                    dumpptr%second_measurement)
                    seccol(1,cols(icol)) = ic
                    if (ic<1) then
                        dumpptr%found = .false.
                    end if
                else if (dumpptr%type(1:15) == 'CHECK_TOTAL_HRR'.and.dumpptr%found) then
                    call fnd_col(ic, header, numr, numc, mxhr, mxhc, dumpptr%second_device, &
                                    dumpptr%second_measurement)
                    seccol(1,cols(icol)) = ic
                    if (ic<1) then
                        dumpptr%found = .false.
                    end if
                    call fnd_col(ic, header, numr, numc, mxhr, mxhc, dumpptr%second_device, &
                                    'HRR Expected')
                    seccol(2,cols(icol)) = ic
                    if (ic<1) then
                        dumpptr%found = .false.
                    end if
                end if
            end if
        end if
    end do
    
    call readcsvformat(iocsv(idx), x, c, numr, numc, 2, 2, mxr, mxc, lend, iofill) 
    if (.not.lend) then
        do i = 1, icol
            dumpptr => dumpinfo(cols(i))
            if (dumpptr%found) then
                relcol = dumpptr%relative_column + 1
                if (dumpptr%type(1:1) == 'M') then
                    dumparray(2,relcol) = x(1, primecol(cols(i)))
                else if (dumpptr%type(1:8) == 'TRIGGER_') then
                    dumparray(2,relcol) = -1
                else if (dumpptr%type(1:9) == 'INTEGRATE') then
                    dumparray(2,relcol) = -1
                    lasttime(i) = x(1, primecol(cols(i)))
                    lastval(1,i) = x(1, seccol(1,cols(i)))
                else if (dumpptr%type(1:15) == 'CHECK_TOTAL_HRR') then
                    dumparray(2,relcol) = -1
                    dummy(1:2,i) = 0
                    lasttime(i) = x(1, primecol(cols(i)))
                    lastval(1:2,i) = x(1, seccol(1:2,cols(i)))
                else
                    dumparray(2,relcol) = -1001
                end if
            end if 
        end do 
    else
        return
    end if
    
    do while (.not.lend)
        call readcsvformat(iocsv(idx), x, c, numr, numc, 1, 1, mxr, mxc, lend, iofill)
        if (.not.lend) then
            do i = 1, icol
                dumpptr => dumpinfo(cols(i))
                if (dumpptr%found) then
                    relcol = dumpptr%relative_column + 1
                    if (dumpptr%type(1:3) == 'MAX') then
                        dumparray(2,relcol) = max(dumparray(2,relcol),x(1, primecol(cols(i))))
                    else if (dumpptr%type(1:3) == 'MIN') then
                        dumparray(2,relcol) = min(dumparray(2,relcol),x(1, primecol(cols(i))))
                    else if (dumpptr%type(1:15) == 'TRIGGER_GREATER') then
                        if (x(1, seccol(1,cols(i)))>=dumpptr%criteria.and.dumparray(2,relcol)== -1) then
                            dumparray(2,relcol) = x(1, primecol(cols(i)))
                        end if
                    else if (dumpptr%type(1:14) == 'TRIGGER_LESSER') then
                        if (x(1, seccol(1,i))<=dumpptr%criteria.and.dumparray(2,relcol)== -1) then
                            dumparray(2,relcol) = x(1, primecol(cols(i)))
                        end if
                    else if (dumpptr%type(1:9) == 'INTEGRATE') then
                        dumparray(2,relcol) = dumparray(2,relcol) + &
                            (x(1, seccol(1,cols(i)))+lastval(1,i))/2*(x(1, primecol(cols(i)))-lasttime(i))
                        lasttime(i) = x(1, primecol(cols(i)))
                        lastval(1,i) = x(1, seccol(1,cols(i)))
                    else if (dumpptr%type(1:15) == 'CHECK_TOTAL_HRR') then
                        dummy(1:2,i) = dummy(1:2,i) + &
                            (x(1, seccol(1:2,cols(i)))+lastval(1:2,i))/2*(x(1, primecol(cols(i)))-lasttime(i))
                        if (dummy(2,i)>0) then
                            dumparray(2,relcol) = dummy(1,i)/dummy(2,i)*100.0
                        end if 
                        lasttime(i) = x(1, primecol(cols(i)))
                        lastval(1:2,i) = x(1, seccol(1:2,cols(i)))
                    else
                        dumparray(2,relcol) = -1001
                    end if
                end if
            end do
        end if
    end do 
    
    return
    end subroutine do_csvfile
        
    !-----------------------------fnd_col(ic, c, nr, nc, mxr, mxc, instrument, measurement)-----------------------------------
    
    subroutine fnd_col(ic, c, nr, nc, mxr, mxc, instrument, measurement)

    integer, intent(out) :: ic
    integer, intent(in) :: nr, nc, mxr, mxc
    character(len=*), intent(in) :: c(nr, nc), instrument, measurement
    
    ! note that we read in the headers ignoring the first row (short names) so the row below are one less that the actual row
    integer, parameter :: instrumentRow = 2, measurementRow = 1, timeColumn = 1
    integer :: i
    
    ic = -1
    if (trim(instrument)=='Time') then
        ic = timeColumn
        return
    end if 
    
    if (mxr < 2) then
        write(*,*)'Error: need at least two rows to use fnd_col mxr = ',mxr
        write(iofill,*)'Error: need at least two rows to use fnd_col mxr = ',mxr
        call cfastexit('spreadsheet_routines: fnd_col',1)
    end if
    do i = 1, mxc
        if (trim(instrument) == trim(c(instrumentRow,i))) then
            if (trim(measurement) == trim(c(measurementRow,i))) then
                ic = i
                return
            end if
        end if
    end do
    
    return
    
    end subroutine fnd_col
    
    
    ! --------------------------- writecsvformat -------------------------------------------

    subroutine writecsvformat (iunit, x, c, nr, nc, nstart, mxr, mxc)

    !     routine: writecsvformat
    !     purpose:writess a comma-delimited file as generated by Micorsoft Excel, assuming that all
    !              the data is in the form of real numbers
    !     arguments: iunit  = logical unit, already open to .csv file
    !                x      = array of dimension (numr,numc) for values in spreadsheet
    !                c      = character array of same dimenaion as x for character values in spreadsheet
    !                nr     = # of rows of arrays x and c
    !                nc     = # of columns of arrays x and c
    !                nstart = starting row of spreadsheet to read
    !                mxr    = actual number of rows read
    !                mxc    = actual number of columns read
    
    integer, intent(in) :: iunit, nr, nc, nstart, mxr, mxc

    real(eb), intent(in) :: x(nr,nc)
    character(len=*), intent(inout) :: c(nr,nc)

    character(len=204800) :: buf
    integer :: i, j, ic, ie
    
    do i = nstart, mxr
        buf = '                    '
        ic = 1
        do j = 1, mxc
            if (x(i,j) /= 0.0) then
                write(c(i,j),'(e16.9)') x(i,j)
            end if
            ie = ic + len_trim(c(i,j))
            buf(ic:ie) = trim(c(i,j))
            ic = ie+1
            buf(ic:ic) = ','
            ic = ic+1
        end do
        write(iunit,'(A)') buf(1:ic)
    end do
    
    return
    end subroutine writecsvformat

end module spreadsheet_routines
