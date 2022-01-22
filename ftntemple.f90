module m_ftntemple
        implicit none

        integer, parameter :: type_max_length = 256 
        integer, parameter :: line_max_length = 256 
        integer, parameter :: max_types = 10000
        integer, parameter :: max_template_lines = 100000
        integer, parameter :: notfound = line_max_length + 1

        character(len=*), parameter :: validchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

        character, parameter :: substitute = achar(26)
        character, parameter :: recordsep  = achar(30)

        ! Mutable module vars for storing the list of types from the type file. 
        integer :: ntypes = 0
        character(len=type_max_length) :: actual_types(max_types) 
        character(len=type_max_length) :: debracketed_types(max_types) 

        ! Mutable module vars for storing information about the template the parser is currently parsing. 
        integer :: n_template_lines = 0
        character(len=:), allocatable  :: typesymbol
        character(len=line_max_length) :: template_lines(max_template_lines)

contains
        subroutine remove_brackets(instr, outstr)
                ! Convert parentheses to underscores
                ! output str must have at least the length of the input str
                character(len=*), intent(in) :: instr
                character(len=*), intent(out) :: outstr

                integer :: i 

                outstr = trim(instr)
                do i = 1,len(outstr)
                        if(outstr(i:i) == '(' .or. outstr(i:i) == ')') then
                                outstr(i:i) = '_'
                        end if
                end do
        end subroutine

        integer function ci_index(string, substring)
                ! Case insensitive subset of the functionality provided by the index intrinsic
                character(len=*), intent(in)  :: string, substring
                character(len=len(string))    :: tmp_str
                character(len=len(substring)) :: tmp_sub
                tmp_str = to_lower(string)
                tmp_sub = to_lower(substring)
                ci_index = index(tmp_str, tmp_sub)
        contains
                pure character function to_lower_c(c)
                        character, intent(in) :: c
                        character(len=*), parameter :: up = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                        character(len=*), parameter :: lw = 'abcdefghijklmnopqrstuvwxyz'
                        integer :: i
                        to_lower_c = c
                        i = index(up,c)
                        if (i /= 0) to_lower_c = lw(i:i)
                end function
                pure function to_lower(str)
                        character(len=*), intent(in) :: str 
                        character(len=len(str)) :: to_lower
                        integer :: i
                        do i = 1, len(str)
                                to_lower(i:i) = to_lower_c(str(i:i))
                        end do
                end function
        end function

        integer function find_start_of_next_word(string, word_idx) result(next_word_idx)
                character(len=*), intent(in) :: string
                integer, intent(in) :: word_idx
                integer :: i, j
                i = verify(string(word_idx:len(string)), validchars)
                if (i == 0) then
                        next_word_idx = 0
                        return
                end if
                i = i + word_idx - 1
                j = verify(string(i:len(string)), ' ') 
                next_word_idx = j + i - 1
        end function

        integer function find_end_of_word(string, word_idx) result(end_idx)
                character(len=*), intent(in) :: string
                integer, intent(in) :: word_idx
                integer :: i, j
                i = verify(string(word_idx:len(string)), validchars)
                if (i == 0) then
                        end_idx = len(string) 
                        return
                end if
                end_idx = i + word_idx - 2
        end function

        subroutine die(line_no, message)
                character(len=*), intent(in) :: message
                integer, intent(in) :: line_no

                character(len=1000) :: fullmsg
                write(fullmsg, "(A,I0,A)") "Line ", line_no, ": " // trim(message)
                error stop fullmsg
        end subroutine

        subroutine write_template_instance(i, out_unit)
                integer, intent(in) :: out_unit, i
                integer :: l, j, k
                character(len=:), allocatable :: tmp

                do l = 1, n_template_lines
                        tmp = trim(template_lines(l))
                        inner_loop: do 
                                j = index(tmp, recordsep)
                                if (j /= 0) tmp = tmp(1:j-1) // "_" // trim(debracketed_types(i)) // tmp(j+1:)

                                k = index(tmp, substitute)
                                if (k /= 0) tmp = tmp(1:k-1) // trim(actual_types(i)) // tmp(k+1:)

                                if(k == 0 .and. j == 0) exit inner_loop
                        end do inner_loop
                        write(out_unit, "(A)") tmp
                end do
        end subroutine

        subroutine find_instance_of_typesymbol(line, instance_start, instance_end)
                character(len=*), intent(in) :: line
                integer, intent(out) :: instance_start, instance_end
                character(len=:), allocatable :: tmp

                integer i, j, k

                instance_start = 0
                instance_end = 0

                i = ci_index(line, 'type')
                if (i == 0) return
                j = index(line, '!')
                if (j /= 0 .and. j < i) return

                j = find_start_of_next_word(line, i) 
                if(line(j:j) == '(') then
                        k = index(line(j:), ')') + j - 1
                        if (k == 0) return
                        if (k-1 <= j) return
                        tmp = line(j+1:k-1)
                        if (trim(adjustl(tmp)) == typesymbol) then
                                instance_start = i
                                instance_end = k
                        end if
                end if
        end subroutine


        subroutine process_single_input_line(line, out_unit, line_no)
                character(len=*), intent(in) :: line
                integer, intent(in) :: out_unit, line_no

                integer i, j, k, i3(3)
                character(len=:), allocatable :: tmpstr

                integer, parameter :: NOT_IN_TEMPLATE = 0
                integer, parameter :: IN_TEMPLATE_PROCNAME_UNKNOWN = 1
                integer, parameter :: IN_TEMPLATE_PROCNAME_KNOWN = 2
                integer, save :: state = NOT_IN_TEMPLATE
               
                if (state == NOT_IN_TEMPLATE) then 
                        ! We're not currently in a template block. 
                        ! The only thing we need to look for is the start of a template block. 

                        i = ci_index(line, '!$template')
                        if (i /= 0 .and. i == index(line, '!')) then
                                ! We found a temlate directive on this line, extract the type symbol
                                tmpstr = line(i:len(line))
                                i = index(tmpstr, '(')
                                if (i == 0) then
                                        call die(line_no, "Invalid template encountered (missing type symbol).")
                                end if 

                                tmpstr = tmpstr(i+1:len(tmpstr))
                                i = index(tmpstr, ')')
                                if (i == 0) then
                                        call die(line_no, "Invalid template encountered (unterminated type symbol).")
                                end if

                                typesymbol = tmpstr(1:i-1)
                                if(len(typesymbol) < 1) then
                                        call die(line_no, "Invalid template encountered (empty type symbol).")
                                end if

                                if (0 /= verify(typesymbol, validchars)) then
                                        call die(line_no, "Invalid template type symbol (contains disallowed characters).")
                                end if 

                                state = IN_TEMPLATE_PROCNAME_UNKNOWN
                        else
                                ! No template directive on this line, so just echo it. 
                                write(out_unit, "(A)") line
                        end if
                elseif (state == IN_TEMPLATE_PROCNAME_UNKNOWN) then
                        ! We are inside a template block
                        ! - We need to be on the lookout for a function, procedure, or subroutine keyword
                        ! - An end template directive here would be unexpected

                        ! first, look for an end template directive, and abort if we find one
                        i = ci_index(line, '!$end template')
                        if (i /= 0 .and. i == index(line, '!')) then
                                call die(line_no, "Unexpected end template directive (no procedure name found in template).")
                        end if 

                        ! then, search for "procedure", "subroutine" or "function"
                        i3 = [ci_index(line, 'procedure'), ci_index(line, 'subroutine'), ci_index(line, 'function')]
                        i3 = merge(i3, [notfound,notfound,notfound], i3 > 0)
                        i = minval(i3)
                        ! Search for comment marker, to make sure any matches for those keywords aren't commented out
                        k = index(line, '!')
                        if (k == 0) k = notfound

                        if (k > i .and. i < notfound) then ! if we found one of those keywords and it's not after a '!'
                                ! found a function, procedure, or subroutine keyword
                                ! record line, insert appropriate marker

                                n_template_lines = n_template_lines + 1
                                if (n_template_lines > max_template_lines) then
                                        call die(line_no, "Limit on number of template lines exceeded.")
                                end if
        
                                ! need to find the procedure name  
                                j = find_start_of_next_word(line, i)
                                k = find_end_of_word(line, j)
                                ! procedure name is line(j:k)
                                ! insert marker character at end of procedure name
                                tmpstr = line(1:k) // recordsep // line(k+1:)

                                ! record the line
                                template_lines(n_template_lines) = tmpstr
                                state = IN_TEMPLATE_PROCNAME_KNOWN
                        else
                                ! normal line
                                n_template_lines = n_template_lines + 1
                                if (n_template_lines > max_template_lines) then
                                        call die(line_no, "Limit on number of template lines exceeded.")
                                end if
                                tmpstr = line
                                call find_instance_of_typesymbol(line, j,k)
                                if (j /= 0) then
                                        tmpstr = line(:j-1) // substitute // line(k+1:)
                                end if
                                template_lines(n_template_lines) = tmpstr

                        end if
                elseif (state == IN_TEMPLATE_PROCNAME_KNOWN) then
                        ! We are inside a template block
                        ! - Need to be on the lookout for the end of the template

                        i = ci_index(line, '!$end template')
                        if (i /= 0 .and. i == index(line, '!')) then
                                ! found an 'end template' directive

                                ! echo out the template
                                do j = 1, ntypes
                                        call write_template_instance(j, out_unit)
                                end do

                                ! reset state
                                state = NOT_IN_TEMPLATE
                                n_template_lines = 0
                        else
                                ! normal line
                                n_template_lines = n_template_lines + 1
                                if (n_template_lines > max_template_lines) then
                                        call die(line_no, "Limit on number of template lines exceeded.")
                                end if
                                tmpstr = line
                                call find_instance_of_typesymbol(line, j,k)
                                if (j /= 0) then
                                        tmpstr = line(:j-1) // substitute // line(k+1:)
                                end if
                                template_lines(n_template_lines) = tmpstr

                        end if

                else 
                        error stop "Bug! Program in invalid state."
                end if
        end subroutine
        
        logical function process(typefile_path, inputfile_path, outunit, errmsg)
                character(len=*), intent(in) :: typefile_path, inputfile_path
                integer, intent(in) :: outunit
                character(len=*), intent(out) :: errmsg

                logical :: typefile_exists, inputfile_exists

                process = .true.
                errmsg = ""
                inquire(file=typefile_path, exist=typefile_exists)
                inquire(file=inputfile_path, exist=inputfile_exists)
                if(.not. typefile_exists) then
                        errmsg = trim(errmsg) // " Type file " // typefile_path // " not found. " 
                        process = .false.
                end if
                if(.not. inputfile_exists) then
                        errmsg = trim(errmsg) // " Input file " // inputfile_path // " not found. " 
                        process = .false.
                end if

                if(.not. process) then
                        return
                end if

                parse_type_list: block 
                        integer :: tun, ios, i
                        character(len=line_max_length) :: line
                        open(newunit=tun, file=typefile_path, iostat=ios, status='old', action='read', form='formatted')
                        if (ios /= 0) then
                                errmsg = "Failed to open typefile " // typefile_path 
                                process = .false.
                                return
                        end if

                        do 
                                read(tun,'(A)', iostat=ios) line
                                if (ios < 0) exit
                                if (trim(line) /= '') then
                                        ntypes = ntypes + 1
                                        if(ntypes > max_types) error stop "Limit on number of types exceeded."

                                        actual_types(ntypes) = trim(line)
                                        call remove_brackets(actual_types(ntypes), debracketed_types(ntypes))
                                end if
                        end do
                        close(tun)
               end block parse_type_list

               hunt_for_template_blocks: block
                        integer :: iun, ios, i
                        character(len=line_max_length) :: line
                        open(newunit=iun, file=inputfile_path, iostat=ios, status='old', action='read', form='formatted')
                        if (ios /= 0) then
                                errmsg = "Failed to open input file " // inputfile_path 
                                process = .false.
                                return
                        end if

                        i = 0
                        do  
                                i = i + 1
                                read(iun,'(A)', iostat=ios) line
                                if (ios < 0) exit

                                call process_single_input_line(trim(line),outunit,i)
                        end do
                        close(iun)
               end block hunt_for_template_blocks
        end function
end module

program ftntemple
        use m_ftntemple, only: process
        use iso_fortran_env, only: output_unit, error_unit
        implicit none

        character(len=4096) :: typefile_path, inputfile_path, tmp
        integer :: nargs
        logical :: ok

        nargs = command_argument_count()

        if (nargs .eq. 1) then
                typefile_path = 'Typefile'
                call get_command_argument(1, inputfile_path)
        elseif (nargs .eq. 2) then
                call get_command_argument(1, typefile_path)
                call get_command_argument(2, inputfile_path)
        else
                call get_command_argument(0, tmp)
                print *, "Usage: ", trim(tmp), " [typefile] inputfile"
                stop
        end if

        ok = process(trim(typefile_path), trim(inputfile_path), output_unit, tmp)
        if (.not. ok) then
                write(error_unit, '(A)') trim(tmp)
                stop
        end if
end program
