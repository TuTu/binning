!this program is to bin the input data and output a file for 
!drawing histogram
PROGRAM binning
  IMPLICIT NONE
  INTEGER, PARAMETER :: REQUIRED_NUM_PAR = 4
  INTEGER :: num_input_files
  INTEGER, DIMENSION(:), ALLOCATABLE :: input_fileid
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: input_filename
  CHARACTER(LEN=128) :: command, arg, usage
  INTEGER :: num_arg, stat, i, j, num_bins, bin_index, bin_overflow_count, num_data
  REAL(KIND=8) :: datum, datum_max, datum_min, bin_max, bin_min, bin_width, swap
  INTEGER, DIMENSION(:), ALLOCATABLE :: bin
  CHARACTER(LEN=128) :: output_filename = "bin_results.txt"
  INTEGER, PARAMETER :: output_fileid = 10
  LOGICAL :: is_given_n, is_given_f, is_given_r, is_normalize
  
  !check if arguments are given
  is_given_n = .FALSE.
  is_given_f = .FALSE.
  is_given_r = .FALSE.
  is_normalize = .FALSE.

  call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=command)
  usage = "Usage: " // TRIM(ADJUSTL(command)) // " -n <number of bins> &
       &-f <input file1> [<input file2> ...] [-o <output file>] &
       &[-r <bin range min> <bin range max>] [--normalize]"
  
  !-- read parameters from command line arguments --!
  num_arg = COMMAND_ARGUMENT_COUNT()
  if (num_arg < REQUIRED_NUM_PAR) then
     write(*,*) "Error: number of argumetns is insufficient."
     write(*,*) usage
     call EXIT(1)
  end if

  i = 1
  do while (i <= num_arg)
     call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg)
     select case (arg)
     case ('-n')
        i = i + 1
        call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
        if (stat /= 0) then
           write(*,*) "Error: unable to read the value of argument -n <number of bins>."
           write(*,*) usage
           call EXIT(1)
        end if
        read(arg, *, IOSTAT=stat) num_bins
        if (stat /= 0) then
           write(*,*) "Error: unable to parse the value of argument -n <number of bins>."
           write(*,*) "       an integer is needed!"
           write(*,*) usage
           call EXIT(1)
        end if
        is_given_n = .TRUE.
     case ('-r')
        do j = 1, 2
           i = i + 1
           call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=arg, STATUS=stat)
           if (stat /= 0) then
              write(*,*) "Error: unable to read the value of argument -r &
                   &<bin range min> <bin range max>."
              write(*,*) usage
              call EXIT(1)
           end if
           if (j == 1) then
              read(arg, *, IOSTAT=stat) bin_min
           else
              read(arg, *, IOSTAT=stat) bin_max
           end if
           if (stat /= 0) then
              write(*,*) "Error: unable to parse the value of argument -r &
                   &<bin range min> <bin range max>."
              write(*,*) "       two integers are needed!"
              write(*,*) usage
              call EXIT(1)
           end if
        end do
        !swap bin_min and bin_max if they are given in reverse order
        if (bin_min > bin_max) then
           swap = bin_min
           bin_min = bin_max
           bin_max = swap
        end if
        is_given_r = .TRUE.
     case ('-f')
        !count number of input files
        num_input_files = 0
        j = i
        do while (.TRUE.)
           j = j + 1
           if (j > num_arg) then
              EXIT
           end if
           call GET_COMMAND_ARGUMENT(NUMBER=j, VALUE=arg, STATUS=stat)
           if (stat /= 0) then
              write(*,*) "Error: unable to read the value of argument -f &
                   &<input file1> [<input file2> ...]"
              write(*,*) usage
              call EXIT(1)
           else if (arg(1:1) == '-') then !end of input file arguments
              EXIT
           end if
           num_input_files = num_input_files + 1
        end do

        if (num_input_files == 0) then
           write(*,*) "Error: at least one input file must be provided!"
           write(*,*) usage
           call EXIT(1)
        end if

        ALLOCATE(input_fileid(num_input_files))
        ALLOCATE(input_filename(num_input_files))

        do j = 1, num_input_files
           i = i + 1
           call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=input_filename(j), STATUS=stat)
           if (stat /= 0) then
              write(*,*) "Error: unable to read the value of argument -f &
                   &<input file1> [<input file2> ...]"
              write(*,*) usage
              call EXIT(1)
           end if
           is_given_f = .TRUE.
        end do
     case ('-o')
        i = i + 1
        call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=output_filename, STATUS=stat)
        if (stat /= 0) then
           write(*,*) "Error: unable to read the value of argument -o <output file>."
           write(*,*) usage
           call EXIT(1)
        end if
     case ('--normalize')
        is_normalize = .TRUE.
     case default
        write(*,*) "Undefined argument: ", arg
        write(*,*) usage
        call EXIT(1)
     end select
     i = i + 1
  end do
  !-- End of reading parameters from command line arguments --!
  
  if ((.NOT. is_given_n) .OR. (.NOT. is_given_f)) then
     write(*,*) "Error: at least arguments -n and -f are needed!"
     write(*,*) usage
  end if
  
  !open every input file
  do i = 1, num_input_files
     input_fileid(i) = 10 + i
     open(UNIT=input_fileid(i), FILE=input_filename(i), IOSTAT=stat, &
          &STATUS="OLD", ACTION="READ")
     if (stat /=0) then
        write(*,*) "Error: unable to open file: ", TRIM(ADJUSTL(input_filename(i)))
        call EXIT(1)
     end if
     write(*,*) "input file ",i ,":", TRIM(ADJUSTL(input_filename(i)))
  end do
  
  ALLOCATE(bin(num_bins))
  
  call get_max_min_num()
  write(*,*) "number of input files:", num_input_files  
  write(*,*) "number of data:", num_data
  write(*,*) "number of bins:", num_bins  
  write(*,*)
  write(*,*) "min datum:", datum_min  
  write(*,*) "max datum:", datum_max
  write(*,*)
  
  !no bin min max range provided, use data min max values
  if (.NOT. is_given_r) then
     bin_min = datum_min
     bin_max = datum_max
     write(*,*) "No <bin min> and <bin max> provided."
     write(*,*) "Using min datum and max datum as <bin min> and <bin max>."
  else !user has assigned bin range
     write(*,*) "bin mininum:", bin_min
     write(*,*) "bin maximum:", bin_max
  end if

  bin_width = (bin_max - bin_min) / num_bins
  if (bin_width == 0) then
     write(*,*) "Error: bin width equals zero!"
     write(*,*) "       either bin min max or datum min max given are the same."
     call EXIT(1)
  end if
  
  !start to read and bin the data
  !binning method is
  ! 1st   2nd   3rd       Nth
  !@---@ O---@ O---@ ... O---@
  bin = 0
  bin_overflow_count = 0
  do i = 1, num_input_files
     do while (.TRUE.)
        read(input_fileid(i), *, IOSTAT=stat) datum
        if (stat > 0) then
           write(*,*) "Error occurred while reading data from file: "&
                &//TRIM(ADJUSTL(input_filename(i)))
           call EXIT(1)
        else if (stat < 0) then !meet EOF
           EXIT
        end if
        if (datum < bin_min .OR. datum > bin_max) then
           bin_overflow_count = bin_overflow_count + 1
        else
           bin_index = CEILING((datum - bin_min) / bin_width)
           if (bin_index == 0) then !bin_min belongs to the 1st bin
              bin_index = 1
           end if
           bin(bin_index) = bin(bin_index) + 1
        end if
     end do
  end do
  
  if (bin_overflow_count > 0) then
     write(*,*)
     write(*,*) "Warning: there are ", bin_overflow_count, " out of ",num_data, &
          &" data locating outside the bin range."
  end if

  !output results for drawing histogram
  open(UNIT=output_fileid, FILE=output_filename, IOSTAT=stat)
  if (stat /= 0) then
     write(*,*) "Error: unable to create output file: ", TRIM(ADJUSTL(output_filename))
     write(*,*) "IOSTAT=", stat
     call EXIT(1)
  end if
  !the middle value of each bin is bin_min + bin_width*(i-0.5)
  if (is_normalize) then
     do i = 1, num_bins
        write(output_fileid, *) bin_min + bin_width*(i-0.5), REAL(bin(i))/num_data
     end do
     write(*,*) "binning results are normalized with total number of data."
  else
     do i = 1, num_bins
        write(output_fileid, *) bin_min + bin_width*(i-0.5), bin(i)
     end do
  end if
CONTAINS
  !go through each file to find the data min and max, and number of data
  SUBROUTINE get_max_min_num()
    IMPLICIT NONE
    
    read(input_fileid(1), *, IOSTAT=stat) datum_max
    if (stat /= 0) then
       write(*,*) "Error occurred while reading data from file: "//TRIM(ADJUSTL(input_filename(i)))
       call EXIT(1)
    end if
    datum_min = datum_max

    num_data = 1
    do i = 1, num_input_files
       do while (.TRUE.)
          read(input_fileid(i), *, IOSTAT=stat) datum
          if (stat > 0) then
             write(*,*) "Error occurred while reading data from file: "&
                  &//TRIM(ADJUSTL(input_filename(i)))
             call EXIT(1)
          else if (stat < 0) then !meet EOF
             REWIND(input_fileid(i))
             EXIT
          end if
          num_data = num_data + 1
          if (datum > datum_max) then
             datum_max = datum
          else if (datum < datum_min) then
             datum_min = datum
          end if
       end do
    end do
  END SUBROUTINE get_max_min_num
END PROGRAM binning
