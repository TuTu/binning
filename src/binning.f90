!this program is to bin the input data and output a file for 
!drawing histogram
PROGRAM binning
  IMPLICIT NONE
  INTEGER, PARAMETER :: REQUIRED_NUM_PAR = 2
  INTEGER :: num_input_files
  INTEGER, DIMENSION(:), ALLOCATABLE :: input_fileid
  CHARACTER(LEN=128), DIMENSION(:), ALLOCATABLE :: input_filename
  CHARACTER(LEN=128) :: command, arg, usage
  INTEGER :: n, stat, i

  call GET_COMMAND_ARGUMENT(NUMBER=0, VALUE=command)
  usage = "Usage: " // TRIM(ADJUSTL(command)) // " <number of input files>&                   
          & <file1> <file2> ..."
  
  !-- read parameters from command line arguments --!
  n = COMMAND_ARGUMENT_COUNT()
  if (n < REQUIRED_NUM_PAR) then
     write(*,*) usage
     call EXIT(1)
  end if
  
  call GET_COMMAND_ARGUMENT(NUMBER=1, VALUE=arg)
  read(arg, *, IOSTAT=stat) num_input_files
  if (stat /= 0) then
     write(*,*) usage
     call EXIT(1)
  end if
  write(*,*) "num_input_files=", num_input_files
  
  do i = 1, num_input_files
     input_fileid(i) = 10 + i
     call GET_COMMAND_ARGUMENT(NUMBER=i, VALUE=input_filename(i), STATUS=stat)
     if (stat /= 0) then
        write(*,*) "Error: number of input files is not correct!"
        write(*,*) usage
        call EXIT(1)
     end if
     open(UNIT=input_fileid(i), FILE=input_filename(i), IOSTAT=stat, &
          &STATUS="OLD", ACTION="READ")
     if (stat /=0) then
        write(*,*) "Error: unable to open file: ", input_filename(i)
        call EXIT(1)
     end if
     write(*,*) "input file #",i ,":", input_filename(i)
     
  end do
END PROGRAM binning
