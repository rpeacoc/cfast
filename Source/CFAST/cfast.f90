
    !> \brief   main program for cfast

    program cfast

    !     Permission is hereby granted, free of charge, to any person
    !     obtaining a copy of this software and associated documentation
    !     files (the "Software"), to deal in the Software without
    !     restriction, including without limitation the rights to use,
    !     copy, modify, merge, publish, distribute, sublicense, and/or sell
    !     copies of the Software, and to permit persons to whom the
    !     Software is furnished to do so, subject to the following
    !     conditions:

    !     The above copyright notice and this permission notice shall be
    !     included in all copies or substantial portions of the Software.

    !     The software is provided "as is", without warranty of any kind,
    !     express or implied, including but not limited to the warranties
    !     of merchantability, fitness for a particular purpose and
    !     noninfringement. In no event shall the authors or copyright
    !     holders be liable for any claim, damages or other liability,
    !     whether in an action of contract, tort or otherwise, arising
    !     from, out of or in connection with the software or the use or
    !     other dealings in the software.

    use precision_parameters
    
    use initialization_routines, only : initialize_memory, initialize_fire_objects, initialize_species, initialize_walls
    use input_routines, only : open_files, read_input_file
    use output_routines, only: output_version, output_initial_conditions, delete_output_files, closeoutputfiles
    use solve_routines, only : solve_simulation
    use spreadsheet_routines, only : output_spreadsheet_dump
    use utility_routines, only : cptime, read_command_options
    use radiation_routines, only : radiation

    use dump_data, only: n_dumps
    use option_data, only: total_steps
    use setup_data, only: cfast_version, stime, iofill, i_time_step, time_end, deltat, i_time_end, validation_flag, &
        ss_out_interval

    implicit none
    external cfastexit

    real(eb) :: xdelt, tstop, tbeg, tend 

    cfast_version = 7501        ! Current CFAST version number

    if (command_argument_count().eq.0) then
        call output_version(0)
        call cfastexit('CFAST',0)
    end if

    ! initialize the basic memory configuration

    stime = 0.0_eb
    call initialize_memory
    call read_command_options
    call open_files

    call output_version (iofill)

    call read_input_file

    call initialize_species

    i_time_step = 1
    xdelt = time_end/deltat
    i_time_end = xdelt + 1
    tstop = i_time_end - 1

    call initialize_walls (tstop)

    call output_initial_conditions

    call cptime(tbeg)
    call solve_simulation (tstop)
    call cptime(tend)

    if (.not.validation_flag) write (*,5000) tend - tbeg
    if (.not.validation_flag) write (*,5010) total_steps
    write (iofill,5000) tend - tbeg
    write (iofill,5010) total_steps


    ! create the spreadsheet file of calculation results if necessary
    if (n_dumps/=0.and.ss_out_interval/=0.) then
        call output_spreadsheet_dump
    end if

    call cfastexit ('CFAST', 0)

5000 format ('Total execution time = ',1pg10.3,' seconds')
5010 format ('Total time steps = ',i10)

    end program cfast
    
! --------------------------- cfastexit -------------------------------------------

!> \brief   called when CFAST exits, printing an error code if necessary
!> \param   name        routine name calling for exit
!> \param   errorcode   numeric code indicating which call to cfastexit in routine

    subroutine cfastexit (name, errorcode)

    use output_routines, only: closeoutputfiles, delete_output_files
    use setup_data, only: validation_flag, iofill, iofilkernel, stopfile
    
    character(len=*), intent(in) :: name
    integer, intent(in) :: errorcode


    if (errorcode/=0) then
        if (trim(name)=='solve_simulation' .and. errorcode==5) then
            ! validation flag test for the maximum iteration exit is because of CFASTBot's testing to make
            !   sure that CFAST can initialize and run a few steps of all the cases in debug mode but doesn't run
            !   to completion. DO NOT CHANGE WITHOUT CHANGING CFASTBOT.
            if (.not.validation_flag) write (*, '(''Maximum iteration exit from CFAST'')')
            if (iofill/=0) write (iofill, '(''Maximum iteration exit from CFAST'')')
        else
            write (*,'(''***Error exit from CFAST, error '',i0,'' from routine '',a)') errorcode, trim(name)
            if (iofill/=0) write (iofill,'(''***Error exit from CFAST, error '',i0,'' from routine '',a)') errorcode, trim(name)
        end if
    else
        if (.not.validation_flag) write (*, '(''Normal exit from CFAST'')')
        if (iofill/=0) write (iofill, '(''Normal exit from CFAST'')')
    end if
    
    call closeoutputfiles
    close (unit=iofilkernel, status='delete')
    call delete_output_files (stopfile)

    stop

    end subroutine cfastexit
    

