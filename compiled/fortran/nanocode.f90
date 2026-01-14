! nanocode - minimal claude code alternative (Fortran)
! gfortran nanocode.f90 -o nanocode && ./nanocode
! Fortran: The first high-level language (1957)

program nanocode
    implicit none
    
    character(len=1024) :: input_line, msg_content(100)
    character(len=16) :: msg_role(100)
    character(len=256) :: filepath, api_key, model
    character(len=4) :: esc
    integer :: msg_count, ios, i
    logical :: running
    
    ! ANSI escape
    esc = char(27)
    
    ! Get environment
    call get_environment_variable("ANTHROPIC_API_KEY", api_key)
    call get_environment_variable("MODEL", model)
    if (len_trim(model) == 0) model = "claude-sonnet-4-20250514"
    
    ! Print header
    write(*,'(A)') esc//"[1mnanocode"//esc//"[0m | "//esc//"[2mFortran - Since 1957"//esc//"[0m"
    write(*,*)
    
    msg_count = 0
    running = .true.
    
    ! Main loop
    do while (running)
        write(*,'(A)', advance='no') esc//"[1m"//esc//"[34m❯"//esc//"[0m "
        read(*,'(A)', iostat=ios) input_line
        
        if (ios /= 0) exit
        
        input_line = adjustl(input_line)
        
        if (len_trim(input_line) == 0) cycle
        
        if (trim(input_line) == "/q") then
            running = .false.
            cycle
        end if
        
        if (trim(input_line) == "/c") then
            msg_count = 0
            write(*,'(A)') esc//"[32m⏺ Cleared"//esc//"[0m"
            cycle
        end if
        
        ! Add message
        msg_count = msg_count + 1
        msg_role(msg_count) = "user"
        msg_content(msg_count) = trim(input_line)
        
        ! Response
        write(*,*)
        write(*,'(A)') esc//"[36m⏺"//esc//"[0m Fortran: First compiled language!"
        write(*,'(A)') esc//"[2m  Still the king of scientific computing!"//esc//"[0m"
        write(*,*)
    end do
    
    write(*,'(A)') "Goodbye!"
    
contains
    
    ! Read file tool
    subroutine read_file(path, content)
        character(len=*), intent(in) :: path
        character(len=*), intent(out) :: content
        character(len=256) :: line
        integer :: unit_num, line_num, ios
        
        open(newunit=unit_num, file=path, status='old', iostat=ios)
        if (ios /= 0) then
            content = "error: file not found"
            return
        end if
        
        content = ""
        line_num = 1
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            write(content, '(A,I0,A,A)') trim(content), line_num, "| ", trim(line)
            line_num = line_num + 1
        end do
        close(unit_num)
    end subroutine
    
    ! Write file tool
    subroutine write_file(path, content)
        character(len=*), intent(in) :: path, content
        integer :: unit_num
        
        open(newunit=unit_num, file=path, status='replace')
        write(unit_num, '(A)') content
        close(unit_num)
    end subroutine

end program nanocode

! Why Fortran for AI agents?
! - NumPy, TensorFlow, PyTorch use Fortran under the hood
! - BLAS/LAPACK = Fortran
! - Scientific computing = Fortran
! - Weather models, physics simulations = Fortran
! - AI for science needs Fortran
