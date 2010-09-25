!this program is to bin the input data and output a file for 
!drawing histogram
PROGRAM binning
  IMPLICIT NONE
  INTEGER, PARAMETER :: REQUIRED_NUM_PAR = 3
  INTEGER :: num_input_files
  INTEGER, DIMENSION(:), ALLOCATABLE :: input_fileid
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: input_filename
  CHARACTER(LEN=128) :: command, arg, usage
  INTEGER :: num_arg, stat, i, num_bins, bin_index, bin_overflow_count
  REAL(KIND=8) :: datum, datum_max, datum_min, bin_max, bin_min, bin_width
  INTEGER, DIMENSION(:), ALLOCATABLE :: bin
  CHARACTER(LEN=*), PARAMETER :: output_filename = "bin_results.txt"
  INTEGER, PARAMETER :: output_fileid = 10

  call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=command)
  usage = "Usage: " // TRIM(ADJUSTL(command)) // " <number of input files>&                   
          & <file1> <file2> ... <number of bins> [<bin min> <bin max>]"
  
  !-- read parameters from command line arguments --!
  num_arg = COMMAND_ARGUMENT_COUNT()
  if (num_arg < REQUIRED_NUM_PAR) then
     write(*,*) usage
     call EXIT(1)
  end if
  
  call GET_COMMAND_ARGUMENT(NUMBER=1, VALUE=arg)
  read(arg, *, IOSTAT=stat) num_input_files
  if (stat /= 0) then
     write(*,*) usage
     call EXIT(1)
  end if

  if (num_arg - 2 /= num_input_files .AND. num_arg - 4 /= num_input_files) then
     write(*,*) "Error: number of arguments is not correct."
     write(*,*) "       e.g. number of input files and actual files provided are not the same."
     write(*,*) usage
     call EXIT(1)
  end if
  write(*,*) "numbmer of input files:", num_input_files
  
  ALLOCATE(input_fileid(num_input_files))
  ALLOCATE(input_filename(num_input_files))
  
  !open every input file
  do i = 1, num_input_files
     input_fileid(i) = 10 + i
     call GET_COMMAND_ARGUMENT(NUMBER=i+1, VALUE=input_filename(i), STATUS=stat)
     if (stat /= 0) then
        write(*,*) "Error: number of input files is not correct!"
        write(*,*) usage
        call EXIT(1)
     end if
     open(UNIT=input_fileid(i), FILE=input_filename(i), IOSTAT=stat, &
          &STATUS="OLD", ACTION="READ")
     if (stat /=0) then
        write(*,*) "Error: unable to open file: ", TRIM(ADJUSTL(input_filename(i)))
        call EXIT(1)
     end if
     write(*,*) "input file ",i ,":", TRIM(ADJUSTL(input_filename(i)))
  end do

  call GET_COMMAND_ARGUMENT(NUMBER=num_input_files+2, VALUE=arg)
  read(arg, *, IOSTAT=stat) num_bins
  if (stat /= 0) then
     write(*,*) "Error: please provide the number of bins!"
     write(*,*) usage
     call EXIT(1)
  end if
  write(*,*) "number of bins:", num_bins
  ALLOCATE(bin(num_bins))

  call get_max_min()
  write(*,*)
  write(*,*) "min datum:", datum_min  
  write(*,*) "max datum:", datum_max
  write(*,*)
  
  !no data min max provided, use automatically found values
  if (num_arg - 2 == num_input_files) then
     bin_min = datum_min
     bin_max = datum_max
     write(*,*) "No <bin min> and <bin max> provided."
     write(*,*) "Using Min datum and Max datum as <bin min> and <bin max>."
  else !bin_min and bin_max assigned manually by user
     call GET_COMMAND_ARGUMENT(NUMBER=num_input_files+3, VALUE=arg)
     read(arg, *, IOSTAT=stat) bin_min
     if (stat /= 0) then
        write(*,*) "Error: error occurred while reading <bin min>!"
        write(*,*) usage
        call EXIT(1)
     end if
     call GET_COMMAND_ARGUMENT(NUMBER=num_input_files+4, VALUE=arg)
     read(arg, *, IOSTAT=stat) bin_max
     if (stat /= 0) then
        write(*,*) "Error: error occurred while reading <bin max>!"
        write(*,*) usage
        call EXIT(1)
     end if
     write(*,*) "bin mininum:", bin_min
     write(*,*) "bin maximum:", bin_max
  end if

  bin_width = (bin_max - bin_min) / num_bins
  
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

  !output results for drawing histogram
  open(UNIT=output_fileid, FILE=output_filename, IOSTAT=stat)
  if (stat /= 0) then
     write(*,*) "Error: unable to create output file: ", TRIM(ADJUSTL(output_filename))
     call EXIT(1)
  end if
  !the middle value of each bin is bin_min + bin_width*(i-0.5)
  do i = 1, num_bins
     write(output_fileid, *) bin_min + bin_width*(i-0.5), bin(i)
  end do
  
CONTAINS
  !go through each file to find the boundary values of data
  SUBROUTINE get_max_min()
    IMPLICIT NONE
    
    read(input_fileid(1), *, IOSTAT=stat) datum_max
    if (stat /= 0) then
       write(*,*) "Error occurred while reading data from file: "//TRIM(ADJUSTL(input_filename(i)))
       call EXIT(1)
    end if
    datum_min = datum_max
    
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
          if (datum > datum_max) then
             datum_max = datum
          else if (datum < datum_min) then
             datum_min = datum
          end if
       end do
    end do
  END SUBROUTINE get_max_min
  
END PROGRAM binning
