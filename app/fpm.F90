#undef unix 
#undef linux
#define FPM_BOOTSTRAP
!>>>>> ././src/fpm_backend_console.f90
!># Build Backend Console
!> This module provides a lightweight implementation for printing to the console
!> and updating previously-printed console lines. It used by `[[fpm_backend_output]]`
!> for pretty-printing build status and progress.
!>
!> @note The implementation for updating previous lines relies on no other output
!> going to `stdout`/`stderr` except through the `console_t` object provided.
!>
!> @note All write statements to `stdout` are enclosed within OpenMP `critical` regions
!>
module fpm_backend_console
use iso_fortran_env, only: stdout=>output_unit
implicit none

private
public :: console_t
public :: LINE_RESET
public :: COLOR_RED, COLOR_GREEN, COLOR_YELLOW, COLOR_RESET

character(len=*), parameter :: ESC = char(27)
!> Escape code for erasing current line
character(len=*), parameter :: LINE_RESET = ESC//"[2K"//ESC//"[1G"
!> Escape code for moving up one line
character(len=*), parameter :: LINE_UP = ESC//"[1A"
!> Escape code for moving down one line
character(len=*), parameter :: LINE_DOWN = ESC//"[1B"
!> Escape code for red foreground color
character(len=*), parameter :: COLOR_RED = ESC//"[31m"
!> Escape code for green foreground color
character(len=*), parameter :: COLOR_GREEN = ESC//"[32m"
!> Escape code for yellow foreground color
character(len=*), parameter :: COLOR_YELLOW = ESC//"[93m"
!> Escape code to reset foreground color
character(len=*), parameter :: COLOR_RESET = ESC//"[0m"

!> Console object
type console_t
    !> Number of lines printed
    integer :: n_line = 1

contains
    !> Write a single line to the console
    procedure :: write_line => console_write_line
    !> Update a previously-written console line
    procedure :: update_line => console_update_line
end type console_t

contains

!> Write a single line to the standard output
subroutine console_write_line(console,str,line,advance)
    !> Console object
    class(console_t), intent(inout) :: console
    !> String to write
    character(*), intent(in) :: str
    !> Integer needed to later update console line
    integer, intent(out), optional :: line
    !> Advancing output (print newline?)
    logical, intent(in), optional :: advance

    character(3) :: adv

    adv = "yes"
    if (present(advance)) then
        if (.not.advance) then
            adv = "no"
        end if
    end if

    !$omp critical

    if (present(line)) then
        line = console%n_line
    end if
    
    write(stdout,'(A)',advance=trim(adv)) LINE_RESET//str

    if (adv=="yes") then
        console%n_line = console%n_line + 1
    end if

    !$omp end critical

end subroutine console_write_line

!> Overwrite a previously-written line in standard output
subroutine console_update_line(console,line_no,str)
    !> Console object
    class(console_t), intent(in) :: console
    !> Integer output from `[[console_write_line]]`
    integer, intent(in) :: line_no
    !> New string to overwrite line
    character(*), intent(in) :: str

    integer :: n

    !$omp critical

    n = console%n_line - line_no

    ! Step back to line
    write(stdout,'(A)',advance="no") repeat(LINE_UP,n)//LINE_RESET

    write(stdout,'(A)',advance="no") str

    ! Step forward to end
    write(stdout,'(A)',advance="no") repeat(LINE_DOWN,n)//LINE_RESET

    !$omp end critical

end subroutine console_update_line

end module fpm_backend_console 
 
!>>>>> ././src/fpm_strings.f90
!> This module defines general procedures for **string operations** for both CHARACTER and
!! TYPE(STRING_T) variables
!
!>## general routines for performing __string operations__
!!
!!### Types
!! - **TYPE(STRING_T)** define a type to contain strings of variable length
!!### Type Conversions
!! - [[F_STRING]]  return Fortran **CHARACTER** variable when given a C-like array of
!!                 single characters terminated with a C_NULL_CHAR **CHARACTER**
!! - [[STR]]  Converts **INTEGER** or** LOGICAL** to **CHARACTER** string
!!### Case
!! - [[LOWER]]  Changes a string to lowercase over optional specified column range
!!### Parsing and joining
!! - [[SPLIT]]  parse string on delimiter characters and store tokens into an allocatable array
!! - [[STRING_CAT]]  Concatenate an array of **type(string_t)** into a single **CHARACTER** variable
!! - [[JOIN]]  append an array of **CHARACTER** variables into a single **CHARACTER** variable
!!### Testing
!! - [[STR_ENDS_WITH]]  test if a **CHARACTER** string or array ends with a specified suffix
!! - [[STRING_ARRAY_CONTAINS]]  Check if array of **TYPE(STRING_T)** matches a particular **CHARACTER** string
!! - **OPERATOR(.IN.)**  Check if array of **TYPE(STRING_T)** matches a particular **CHARACTER** string
!! - [[GLOB]]  function compares text strings, one of which can have wildcards ('*' or '?').
!! - [[IS_FORTRAN_NAME]]  determine whether a string is an acceptable Fortran entity name
!! - [[TO_FORTRAN_NAME]]  replace allowed special but unusuable characters in names with underscore
!!### Whitespace
!! - [[NOTABS]]  Expand tab characters assuming a tab space every eight characters
!! - [[LEN_TRIM]]  Determine total trimmed length of **STRING_T** array
!!### Miscellaneous
!! - [[FNV_1A]]  Hash a **CHARACTER(*)** string of default kind or a **TYPE(STRING_T)** array
!! - [[REPLACE]]  Returns string with characters in charset replaced with target_char.
!! - [[RESIZE]]  increase the size of a **TYPE(STRING_T)** array by N elements
!!
module fpm_strings
use iso_fortran_env, only: int64
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer, c_size_t
implicit none

private
public :: f_string, lower, split, str_ends_with, string_t, str_begins_with_str
public :: to_fortran_name, is_fortran_name
public :: string_array_contains, string_cat, len_trim, operator(.in.), fnv_1a
public :: replace, resize, str, join, glob
public :: notabs

type string_t
    character(len=:), allocatable :: s
end type

interface len_trim
    module procedure :: string_len_trim
end interface len_trim

interface resize
  module procedure :: resize_string
end interface

interface operator(.in.)
    module procedure string_array_contains
end interface

interface fnv_1a
    procedure :: fnv_1a_char
    procedure :: fnv_1a_string_t
end interface fnv_1a

interface str_ends_with
    procedure :: str_ends_with_str
    procedure :: str_ends_with_any
end interface str_ends_with

interface str
    module procedure str_int, str_int64, str_logical
end interface

interface string_t
    module procedure new_string_t
end interface string_t

interface f_string
    module procedure f_string, f_string_cptr, f_string_cptr_n
end interface f_string

contains

!> test if a CHARACTER string ends with a specified suffix
pure logical function str_ends_with_str(s, e) result(r)
    character(*), intent(in) :: s, e
    integer :: n1, n2
    n1 = len(s)-len(e)+1
    n2 = len(s)
    if (n1 < 1) then
        r = .false.
    else
        r = (s(n1:n2) == e)
    end if
end function str_ends_with_str

!> test if a CHARACTER string ends with any of an array of suffixs
pure logical function str_ends_with_any(s, e) result(r)
    character(*), intent(in) :: s
    character(*), intent(in) :: e(:)

    integer :: i

    r = .true.
    do i=1,size(e)

        if (str_ends_with(s,trim(e(i)))) return

    end do
    r = .false.

end function str_ends_with_any

!> test if a CHARACTER string begins with a specified prefix
pure logical function str_begins_with_str(s, e) result(r)
    character(*), intent(in) :: s, e
    integer :: n1, n2
    n1 = 1
    n2 = 1 + len(e)-1
    if (n2 > len(s)) then
        r = .false.
    else
        r = (s(n1:n2) == e)
    end if
end function str_begins_with_str

!> return Fortran character variable when given a C-like array of
!! single characters terminated with a C_NULL_CHAR character
function f_string(c_string)
    use iso_c_binding
    character(len=1), intent(in) :: c_string(:)
    character(:), allocatable :: f_string

    integer :: i, n

    i = 0
    do while(c_string(i+1) /= C_NULL_CHAR)
      i = i + 1
    end do
    n = i

    allocate(character(n) :: f_string)
    do i=1,n
      f_string(i:i) = c_string(i)
    end do

end function f_string


!> return Fortran character variable when given a null-terminated c_ptr
function f_string_cptr(cptr) result(s)
    type(c_ptr), intent(in), value :: cptr
    character(len=:,kind=c_char), allocatable :: s

    interface
        function c_strlen(s) result(r) bind(c, name="strlen")
            import c_size_t, c_ptr
            type(c_ptr), intent(in), value :: s
            integer(kind=c_size_t) :: r
        end function
    end interface

    s = f_string_cptr_n(cptr, c_strlen(cptr))
end function

!> return Fortran character variable when given a null-terminated c_ptr and its length
function f_string_cptr_n(cptr, n) result(s)
    type(c_ptr), intent(in), value :: cptr
    integer(kind=c_size_t), intent(in) :: n
    character(len=n,kind=c_char) :: s
    character(len=n,kind=c_char), pointer :: sptr

    call c_f_pointer(cptr, sptr)
    s = sptr
end function

!> Hash a character(*) string of default kind
pure function fnv_1a_char(input, seed) result(hash)
    character(*), intent(in) :: input
    integer(int64), intent(in), optional :: seed
    integer(int64) :: hash

    integer :: i
    integer(int64), parameter :: FNV_OFFSET_32 = 2166136261_int64
    integer(int64), parameter :: FNV_PRIME_32 = 16777619_int64

    if (present(seed)) then
        hash = seed
    else
        hash = FNV_OFFSET_32
    end if

    do i=1,len(input)
        hash = ieor(hash,iachar(input(i:i),int64)) * FNV_PRIME_32
    end do

end function fnv_1a_char


!> Hash a string_t array of default kind
pure function fnv_1a_string_t(input, seed) result(hash)
    type(string_t), intent(in) :: input(:)
    integer(int64), intent(in), optional :: seed
    integer(int64) :: hash

    integer :: i

    hash = fnv_1a(input(1)%s,seed)

    do i=2,size(input)
        hash = fnv_1a(input(i)%s,hash)
    end do

end function fnv_1a_string_t


 !>Author: John S. Urban
 !!License: Public Domain
 !! Changes a string to lowercase over optional specified column range
elemental pure function lower(str,begin,end) result (string)

    character(*), intent(In)     :: str
    character(len(str))          :: string
    integer,intent(in),optional  :: begin, end
    integer                      :: i
    integer                      :: ibegin, iend
    string = str

    ibegin = 1
    if (present(begin))then
        ibegin = max(ibegin,begin)
    endif

    iend = len_trim(str)
    if (present(end))then
        iend= min(iend,end)
    endif

    do i = ibegin, iend                               ! step thru each letter in the string in specified range
        select case (str(i:i))
        case ('A':'Z')
            string(i:i) = char(iachar(str(i:i))+32)     ! change letter to miniscule
        case default
        end select
    end do

end function lower

!> Helper function to generate a new string_t instance
!>  (Required due to the allocatable component)
function new_string_t(s) result(string)
    character(*), intent(in) :: s
    type(string_t) :: string

    string%s = s

end function new_string_t

!> Check if array of TYPE(STRING_T) matches a particular CHARACTER string
!!
logical function string_array_contains(search_string,array)
    character(*), intent(in) :: search_string
    type(string_t), intent(in) :: array(:)

    integer :: i

    string_array_contains = any([(array(i)%s==search_string, &
                                   i=1,size(array))])

end function string_array_contains

!> Concatenate an array of type(string_t) into
!>  a single CHARACTER variable
function string_cat(strings,delim) result(cat)
    type(string_t), intent(in) :: strings(:)
    character(*), intent(in), optional :: delim
    character(:), allocatable :: cat

    integer :: i
    character(:), allocatable :: delim_str

    if (size(strings) < 1) then
        cat = ''
        return
    end if

    if (present(delim)) then
        delim_str = delim
    else
        delim_str = ''
    end if

    cat = strings(1)%s
    do i=2,size(strings)

        cat = cat//delim_str//strings(i)%s

    end do

end function string_cat

!> Determine total trimmed length of `string_t` array
pure function string_len_trim(strings) result(n)
    type(string_t), intent(in) :: strings(:)
    integer :: i, n

    n = 0
    do i=1,size(strings)
        n = n + len_trim(strings(i)%s)
    end do

end function string_len_trim

!>Author: John S. Urban
!!License: Public Domain
!! parse string on delimiter characters and store tokens into an allocatable array
subroutine split(input_line,array,delimiters,order,nulls)
    !! given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
    !!
    !! * by default adjacent delimiters in the input string do not create an empty string in the output array
    !! * no quoting of delimiters is supported
    character(len=*),intent(in)              :: input_line  !! input string to tokenize
    character(len=*),optional,intent(in)     :: delimiters  !! list of delimiter characters
    character(len=*),optional,intent(in)     :: order       !! order of output array sequential|[reverse|right]
    character(len=*),optional,intent(in)     :: nulls       !! return strings composed of delimiters or not ignore|return|ignoreend
    character(len=:),allocatable,intent(out) :: array(:)    !! output array of tokens

    integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
    integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
    integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
    character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
    character(len=:),allocatable  :: ordr                   ! string containing order keyword
    character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
    integer                       :: ii,iiii                ! loop parameters used to control print order
    integer                       :: icount                 ! number of tokens found
    integer                       :: ilen                   ! length of input string with trailing spaces trimmed
    integer                       :: i10,i20,i30            ! loop counters
    integer                       :: icol                   ! pointer into input string as it is being parsed
    integer                       :: idlim                  ! number of delimiter characters
    integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
    integer                       :: inotnull               ! count strings not composed of delimiters
    integer                       :: ireturn                ! number of tokens returned
    integer                       :: imax                   ! length of longest token

    ! decide on value for optional DELIMITERS parameter
    if (present(delimiters)) then                                     ! optional delimiter list was present
        if(delimiters/='')then                                       ! if DELIMITERS was specified and not null use it
            dlim=delimiters
        else                                                           ! DELIMITERS was specified on call as empty string
            dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0) ! use default delimiter when not specified
        endif
    else                                                              ! no delimiter value was specified
        dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)    ! use default delimiter when not specified
    endif
    idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string

    if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
    if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter

    n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
    allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
    allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
    ibegin(:)=1
    iterm(:)=1

    ilen=len(input_line)                                           ! ILEN is the column position of the last non-blank character
    icount=0                                                       ! how many tokens found
    inotnull=0                                                     ! how many tokens found not composed of delimiters
    imax=0                                                         ! length of longest token found

    select case (ilen)

    case (0)                                                      ! command was totally blank

    case default                                                   ! there is at least one non-delimiter in INPUT_LINE if get here
        icol=1                                                      ! initialize pointer into input line
        INFINITE: do i30=1,ilen,1                                   ! store into each array element
            ibegin(i30)=icol                                         ! assume start new token on the character
            if(index(dlim(1:idlim),input_line(icol:icol))==0)then  ! if current character is not a delimiter
            iterm(i30)=ilen                                       ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
                ifound=index(input_line(ibegin(i30):ilen),dlim(i10:i10))
                IF(ifound>0)then
                    iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
                endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
            else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
            endif
            imax=max(imax,iterm(i30)-ibegin(i30)+1)
            icount=i30                                               ! increment count of number of tokens found
            if(icol>ilen)then                                     ! no text left
            exit INFINITE
            endif
        enddo INFINITE

    end select

    select case (trim(adjustl(nlls)))
    case ('ignore','','ignoreend')
        ireturn=inotnull
    case default
        ireturn=icount
    end select
    allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
    !allocate(array(ireturn))                                       ! allocate the array to turn

    select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
    case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
    case default             ; ii=1       ; iiii=1                 ! first to last
    end select

    do i20=1,icount                                                ! fill the array with the tokens that were found
        if(iterm(i20)<ibegin(i20))then
            select case (trim(adjustl(nlls)))
            case ('ignore','','ignoreend')
            case default
            array(ii)=' '
            ii=ii+iiii
            end select
        else
            array(ii)=input_line(ibegin(i20):iterm(i20))
            ii=ii+iiii
        endif
    enddo
end subroutine split

!> Returns string with characters in charset replaced with target_char.
pure function replace(string, charset, target_char) result(res)
    character(*), intent(in) :: string
    character, intent(in) :: charset(:), target_char
    character(len(string)) :: res
    integer :: n
    res = string
    do n = 1, len(string)
        if (any(string(n:n) == charset)) then
            res(n:n) = target_char
        end if
    end do
end function replace

!> increase the size of a TYPE(STRING_T) array by N elements
subroutine resize_string(list, n)
  !> Instance of the array to be resized
  type(string_t), allocatable, intent(inout) :: list(:)
  !> Dimension of the final array size
  integer, intent(in), optional :: n

  type(string_t), allocatable :: tmp(:)
  integer :: this_size, new_size, i
  integer, parameter :: initial_size = 16

  if (allocated(list)) then
    this_size = size(list, 1)
    call move_alloc(list, tmp)
  else
    this_size = initial_size
  end if

  if (present(n)) then
    new_size = n
  else
    new_size = this_size + this_size/2 + 1
  end if

  allocate(list(new_size))

  if (allocated(tmp)) then
    this_size = min(size(tmp, 1), size(list, 1))
    do i = 1, this_size
      call move_alloc(tmp(i)%s, list(i)%s)
    end do
    deallocate(tmp)
  end if

end subroutine resize_string

!>AUTHOR: John S. Urban
!!LICENSE: Public Domain
!>
!!##NAME
!!    join(3f) - [M_strings:EDITING] append CHARACTER variable array into
!!    a single CHARACTER variable with specified separator
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure function join(str,sep,trm,left,right,start,end) result (string)
!!
!!     character(len=*),intent(in)          :: str(:)
!!     character(len=*),intent(in),optional :: sep
!!     logical,intent(in),optional          :: trm
!!     character(len=*),intent(in),optional :: right
!!     character(len=*),intent(in),optional :: left
!!     character(len=*),intent(in),optional :: start
!!     character(len=*),intent(in),optional :: end
!!     character(len=:),allocatable         :: string
!!
!!##DESCRIPTION
!!   JOIN(3f) appends the elements of a CHARACTER array into a single
!!   CHARACTER variable, with elements 1 to N joined from left to right.
!!   By default each element is trimmed of trailing spaces and the
!!   default separator is a null string.
!!
!!##OPTIONS
!!      STR(:)  array of CHARACTER variables to be joined
!!      SEP     separator string to place between each variable. defaults
!!              to a null string.
!!      LEFT    string to place at left of each element
!!      RIGHT   string to place at right of each element
!!      START   prefix string
!!      END     suffix string
!!      TRM     option to trim each element of STR of trailing
!!              spaces. Defaults to .TRUE.
!!
!!##RESULT
!!      STRING  CHARACTER variable composed of all of the elements of STR()
!!              appended together with the optional separator SEP placed
!!              between the elements.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_join
!!   use M_strings, only: join
!!   implicit none
!!   character(len=:),allocatable  :: s(:)
!!   character(len=:),allocatable  :: out
!!   integer                       :: i
!!     s=[character(len=10) :: 'United',' we',' stand,', &
!!     & ' divided',' we fall.']
!!     out=join(s)
!!     write(*,'(a)') out
!!     write(*,'(a)') join(s,trm=.false.)
!!     write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
!!     write(*,'(a)') join(s,sep='<>')
!!     write(*,'(a)') join(s,sep=';',left='[',right=']')
!!     write(*,'(a)') join(s,left='[',right=']')
!!     write(*,'(a)') join(s,left='>>')
!!   end program demo_join
!!
!!  Expected output:
!!
!!   United we stand, divided we fall.
!!   United     we        stand,    divided   we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United    | we       | stand,   | divided  | we fall.
!!   United<> we<> stand,<> divided<> we fall.
!!   [United];[ we];[ stand,];[ divided];[ we fall.]
!!   [United][ we][ stand,][ divided][ we fall.]
!!   >>United>> we>> stand,>> divided>> we fall.
pure function join(str,sep,trm,left,right,start,end) result (string)

! @(#)M_strings::join(3f): merge string array into a single CHARACTER value adding specified separators, caps, prefix and suffix

character(len=*),intent(in)          :: str(:)
character(len=*),intent(in),optional :: sep, right, left, start, end
logical,intent(in),optional          :: trm
character(len=:),allocatable         :: sep_local, left_local, right_local
character(len=:),allocatable         :: string
logical                              :: trm_local
integer                              :: i
   if(present(sep))then   ; sep_local=sep     ; else ; sep_local=''     ; endif
   if(present(trm))then   ; trm_local=trm     ; else ; trm_local=.true. ; endif
   if(present(left))then  ; left_local=left   ; else ; left_local=''    ; endif
   if(present(right))then ; right_local=right ; else ; right_local=''   ; endif
   string=''
   if(size(str)==0)then
      string=string//left_local//right_local
   else
      do i = 1,size(str)-1
         if(trm_local)then
            string=string//left_local//trim(str(i))//right_local//sep_local
         else
            string=string//left_local//str(i)//right_local//sep_local
         endif
      enddo
      if(trm_local)then
         string=string//left_local//trim(str(i))//right_local
      else
         string=string//left_local//str(i)//right_local
      endif
   endif
   if(present(start))string=start//string
   if(present(end))string=string//end
end function join

!>##AUTHOR John S. Urban
!!##LICENSE Public Domain
!!## NAME
!!    glob(3f) - [fpm_strings:COMPARE] compare given string for match to
!!    pattern which may contain wildcard characters
!!    (LICENSE:PD)
!!
!!## SYNOPSIS
!!
!!    logical function glob(string, pattern )
!!
!!     character(len=*),intent(in) :: string
!!     character(len=*),intent(in) :: pattern
!!
!!## DESCRIPTION
!!   glob(3f) compares given STRING for match to PATTERN which may
!!   contain wildcard characters.
!!
!!   In this version to get a match the entire string must be described
!!   by PATTERN. Trailing whitespace is significant, so trim the input
!!   string to have trailing whitespace ignored.
!!
!!## OPTIONS
!!    string   the input string to test to see if it contains the pattern.
!!    pattern  the following simple globbing options are available
!!
!!             o "?" matching any one character
!!             o "*" matching zero or more characters.
!!               Do NOT use adjacent asterisks.
!!             o Both strings may have trailing spaces which
!!               are ignored.
!!             o There is no escape character, so matching strings with
!!               literal question mark and asterisk is problematic.
!!
!!## EXAMPLES
!!
!!   Example program
!!
!!    program demo_glob
!!    implicit none
!!    ! This main() routine passes a bunch of test strings
!!    ! into the above code.  In performance comparison mode,
!!    ! it does that over and over. Otherwise, it does it just
!!    ! once. Either way, it outputs a passed/failed result.
!!    !
!!    integer :: nReps
!!    logical :: allpassed
!!    integer :: i
!!     allpassed = .true.
!!
!!     nReps = 10000
!!     ! Can choose as many repetitions as you're expecting
!!     ! in the real world.
!!     nReps = 1
!!
!!     do i=1,nReps
!!      ! Cases with repeating character sequences.
!!      allpassed=allpassed .and. test("a*abab", "a*b", .true.)
!!      !!cycle
!!      allpassed=allpassed .and. test("ab", "*?", .true.)
!!      allpassed=allpassed .and. test("abc", "*?", .true.)
!!      allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
!!      allpassed=allpassed .and. test("bLah", "bLaH", .false.)
!!      allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("mississipissippi", "*issip*ss*", .true.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
!!      allpassed=allpassed .and. &
!!       & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
!!      allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("ababac", "*abac*", .true.)
!!      allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12b12", "a12b", .false.)
!!      allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)
!!
!!      ! Additional cases where the '*' char appears in the tame string.
!!      allpassed=allpassed .and. test("*", "*", .true.)
!!      allpassed=allpassed .and. test("a*r", "a*", .true.)
!!      allpassed=allpassed .and. test("a*ar", "a*aar", .false.)
!!
!!      ! More double wildcard scenarios.
!!      allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
!!      allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
!!      allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
!!      allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
!!      allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
!!      allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
!!      allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
!!      allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
!!      allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
!!      allpassed=allpassed .and. test("oWn", "*oWn*", .true.)
!!
!!      ! Completely tame (no wildcards) cases.
!!      allpassed=allpassed .and. test("bLah", "bLah", .true.)
!!
!!      ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
!!      allpassed=allpassed .and. test("a", "*?", .true.)
!!
!!      ! More mixed wildcard tests including coverage for false positives.
!!      allpassed=allpassed .and. test("a", "??", .false.)
!!      allpassed=allpassed .and. test("ab", "?*?", .true.)
!!      allpassed=allpassed .and. test("ab", "*?*?*", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*?", .true.)
!!      allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
!!      allpassed=allpassed .and. test("abcd", "?b*??", .true.)
!!      allpassed=allpassed .and. test("abcd", "?a*??", .false.)
!!      allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
!!      allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
!!      allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)
!!
!!      ! Single-character-match cases.
!!      allpassed=allpassed .and. test("bLah", "bL?h", .true.)
!!      allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
!!      allpassed=allpassed .and. test("bLah", "bLa?", .true.)
!!      allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
!!      allpassed=allpassed .and. test("bLaH", "?LaH", .true.)
!!
!!      ! Many-wildcard scenarios.
!!      allpassed=allpassed .and. test(&
!!      &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
!!      &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
!!      &"a*a*a*a*a*a*aa*aaa*a*a*b",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacac&
!!      &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacaca&
!!      &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abababababababababababababababababababaacacacacacacacad&
!!      &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
!!      &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
!!      allpassed=allpassed .and. &
!!      test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
!!      &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
!!      allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa",&
!!      &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
!!      &*abc*abc*abc*",&
!!      &.false.)
!!      allpassed=allpassed .and. test(&
!!      &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
!!      &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd",&
!!      &"abc*abc*abc*abc*abc", .false.)
!!      allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd&
!!      &*abc*abcd*abc*abc*abcd", &
!!      &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
!!      &.true.)
!!      allpassed=allpassed .and. test("abc",&
!!      &"********a********b********c********", .true.)
!!      allpassed=allpassed .and.&
!!      &test("********a********b********c********", "abc", .false.)
!!      allpassed=allpassed .and. &
!!      &test("abc", "********a********b********b********", .false.)
!!      allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)
!!
!!      ! A case-insensitive algorithm test.
!!      ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
!!     enddo
!!
!!     if (allpassed)then
!!        write(*,'(a)')"Passed",nReps
!!     else
!!        write(*,'(a)')"Failed"
!!     endif
!!    contains
!!    ! This is a test program for wildcard matching routines.
!!    ! It can be used either to test a single routine for correctness,
!!    ! or to compare the timings of two (or more) different wildcard
!!    ! matching routines.
!!    !
!!    function test(tame, wild, bExpectedResult) result(bpassed)
!!    use fpm_strings, only : glob
!!       character(len=*) :: tame
!!       character(len=*) :: wild
!!       logical          :: bExpectedResult
!!       logical          :: bResult
!!       logical          :: bPassed
!!       bResult = .true.    ! We'll do "&=" cumulative checking.
!!       bPassed = .false.   ! Assume the worst.
!!       write(*,*)repeat('=',79)
!!       bResult = glob(tame, wild) ! Call a wildcard matching routine.
!!
!!       ! To assist correctness checking, output the two strings in any
!!       ! failing scenarios.
!!       if (bExpectedResult .eqv. bResult) then
!!          bPassed = .true.
!!          if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
!!       else
!!          if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
!!       endif
!!
!!    end function test
!!    end program demo_glob
!!
!!   Expected output
!!
!!
!!## REFERENCE
!!   The article "Matching Wildcards: An Empirical Way to Tame an Algorithm"
!!   in Dr Dobb's Journal, By Kirk J. Krauss, October 07, 2014
!!
function glob(tame,wild)

! @(#)fpm_strings::glob(3f): function compares text strings, one of which can have wildcards ('*' or '?').

logical                    :: glob       !! result of test
character(len=*)           :: tame       !! A string without wildcards to compare to the globbing expression
character(len=*)           :: wild       !! A (potentially) corresponding string with wildcards
character(len=len(tame)+1) :: tametext
character(len=len(wild)+1) :: wildtext
character(len=1),parameter :: NULL=char(0)
integer                    :: wlen
integer                    :: ti, wi
integer                    :: i
character(len=:),allocatable :: tbookmark, wbookmark
! These two values are set when we observe a wildcard character. They
! represent the locations, in the two strings, from which we start once we've observed it.
   tametext=tame//NULL
   wildtext=wild//NULL
   tbookmark = NULL
   wbookmark = NULL
   wlen=len(wild)
   wi=1
   ti=1
   do                                            ! Walk the text strings one character at a time.
      if(wildtext(wi:wi) == '*')then             ! How do you match a unique text string?
         do i=wi,wlen                            ! Easy: unique up on it!
            if(wildtext(wi:wi)=='*')then
               wi=wi+1
            else
               exit
            endif
         enddo
         if(wildtext(wi:wi)==NULL) then        ! "x" matches "*"
            glob=.true.
            return
         endif
         if(wildtext(wi:wi) /= '?') then
            ! Fast-forward to next possible match.
            do while (tametext(ti:ti) /= wildtext(wi:wi))
               ti=ti+1
               if (tametext(ti:ti)==NULL)then
                  glob=.false.
                  return                         ! "x" doesn't match "*y*"
               endif
            enddo
         endif
         wbookmark = wildtext(wi:)
         tbookmark = tametext(ti:)
      elseif(tametext(ti:ti) /= wildtext(wi:wi) .and. wildtext(wi:wi) /= '?') then
         ! Got a non-match. If we've set our bookmarks, back up to one or both of them and retry.
         if(wbookmark/=NULL) then
            if(wildtext(wi:)/= wbookmark) then
               wildtext = wbookmark;
               wlen=len_trim(wbookmark)
               wi=1
               ! Don't go this far back again.
               if (tametext(ti:ti) /= wildtext(wi:wi)) then
                  tbookmark=tbookmark(2:)
                  tametext = tbookmark
                  ti=1
                  cycle                          ! "xy" matches "*y"
               else
                  wi=wi+1
               endif
            endif
            if (tametext(ti:ti)/=NULL) then
               ti=ti+1
               cycle                             ! "mississippi" matches "*sip*"
            endif
         endif
         glob=.false.
         return                                  ! "xy" doesn't match "x"
      endif
      ti=ti+1
      wi=wi+1
      if (tametext(ti:ti)==NULL) then          ! How do you match a tame text string?
         if(wildtext(wi:wi)/=NULL)then
            do while (wildtext(wi:wi) == '*')    ! The tame way: unique up on it!
               wi=wi+1                           ! "x" matches "x*"
               if(wildtext(wi:wi)==NULL)exit
            enddo
         endif
         if (wildtext(wi:wi)==NULL)then
            glob=.true.
            return                               ! "x" matches "x"
         endif
         glob=.false.
         return                                  ! "x" doesn't match "xy"
      endif
   enddo
end function glob

!> Returns the length of the string representation of 'i'
pure integer function str_int_len(i) result(sz)
integer, intent(in) :: i
integer, parameter :: MAX_STR = 100
character(MAX_STR) :: s
! If 's' is too short (MAX_STR too small), Fortran will abort with:
! "Fortran runtime error: End of record"
write(s, '(i0)') i
sz = len_trim(s)
end function

!> Converts integer "i" to string
pure function str_int(i) result(s)
integer, intent(in) :: i
character(len=str_int_len(i)) :: s
write(s, '(i0)') i
end function

!> Returns the length of the string representation of 'i'
pure integer function str_int64_len(i) result(sz)
integer(int64), intent(in) :: i
integer, parameter :: MAX_STR = 100
character(MAX_STR) :: s
! If 's' is too short (MAX_STR too small), Fortran will abort with:
! "Fortran runtime error: End of record"
write(s, '(i0)') i
sz = len_trim(s)
end function

!> Converts integer "i" to string
pure function str_int64(i) result(s)
integer(int64), intent(in) :: i
character(len=str_int64_len(i)) :: s
write(s, '(i0)') i
end function

!> Returns the length of the string representation of 'l'
pure integer function str_logical_len(l) result(sz)
logical, intent(in) :: l
if (l) then
    sz = 6
else
    sz = 7
end if
end function

!> Converts logical "l" to string
pure function str_logical(l) result(s)
logical, intent(in) :: l
character(len=str_logical_len(l)) :: s
if (l) then
    s = ".true."
else
    s = ".false."
end if
end function

!> Returns string with special characters replaced with an underscore.
!! For now, only a hyphen is treated as a special character, but this can be
!! expanded to other characters if needed.
pure function to_fortran_name(string) result(res)
    character(*), intent(in) :: string
    character(len(string)) :: res
    character, parameter :: SPECIAL_CHARACTERS(*) = ['-']
    res = replace(string, SPECIAL_CHARACTERS, '_')
end function to_fortran_name

function is_fortran_name(line) result (lout)
! determine if a string is a valid Fortran name ignoring trailing spaces
! (but not leading spaces)
    character(len=*),parameter   :: int='0123456789'
    character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
    character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=*),parameter   :: allowed=upper//lower//int//'_'
    character(len=*),intent(in)  :: line
    character(len=:),allocatable :: name
    logical                      :: lout
        name=trim(line)
        if(len(name)/=0)then
            lout = .true.                                  &
             & .and. verify(name(1:1), lower//upper) == 0  &
             & .and. verify(name,allowed) == 0             &
             & .and. len(name) <= 63
        else
            lout = .false.
        endif
    end function is_fortran_name
!>
!!### NAME
!!   notabs(3f) - [fpm_strings:NONALPHA] expand tab characters
!!   (LICENSE:PD)
!!
!!### SYNOPSIS
!!
!!    subroutine notabs(INSTR,OUTSTR,ILEN)
!!
!!     character(len=*),intent=(in)  :: INSTR
!!     character(len=*),intent=(out) :: OUTSTR
!!     integer,intent=(out)          :: ILEN
!!
!!### DESCRIPTION
!!   NOTABS() converts tabs in INSTR to spaces in OUTSTR while maintaining
!!   columns. It assumes a tab is set every 8 characters. Trailing spaces
!!   are removed.
!!
!!   In addition, trailing carriage returns and line feeds are removed
!!   (they are usually a problem created by going to and from MSWindows).
!!
!!   What are some reasons for removing tab characters from an input line?
!!   Some Fortran compilers have problems with tabs, as tabs are not
!!   part of the Fortran character set. Some editors and printers will
!!   have problems with tabs. It is often useful to expand tabs in input
!!   files to simplify further processing such as tokenizing an input line.
!!
!!### OPTIONS
!!     instr     Input line to remove tabs from
!!
!!### RESULTS
!!     outstr    Output string with tabs expanded. Assumed to be of sufficient
!!               length
!!     ilen      Significant length of returned string
!!
!!### EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_notabs
!!
!!    !  test filter to remove tabs and trailing white space from input
!!    !  on files up to 1024 characters wide
!!    use fpm_strings, only : notabs
!!    character(len=1024) :: in,out
!!    integer             :: ios,iout
!!       do
!!          read(*,'(A)',iostat=ios)in
!!          if(ios /= 0) exit
!!          call notabs(in,out,iout)
!!          write(*,'(a)')out(:iout)
!!       enddo
!!    end program demo_notabs
!!
!!### SEE ALSO
!!   GNU/Unix commands expand(1) and unexpand(1)
!!
!!### AUTHOR
!!   John S. Urban
!!
!!### LICENSE
!!   Public Domain
elemental impure subroutine notabs(instr,outstr,ilen)

! ident_31="@(#)fpm_strings::notabs(3f): convert tabs to spaces while maintaining columns, remove CRLF chars"

character(len=*),intent(in)   :: instr        ! input line to scan for tab characters
character(len=*),intent(out)  :: outstr       ! tab-expanded version of INSTR produced
integer,intent(out)           :: ilen         ! column position of last character put into output string
                                              ! that is, ILEN holds the position of the last non-blank character in OUTSTR

integer,parameter             :: tabsize=8    ! assume a tab stop is set every 8th column
integer                       :: ipos         ! position in OUTSTR to put next character of INSTR
integer                       :: lenin        ! length of input string trimmed of trailing spaces
integer                       :: lenout       ! number of characters output string can hold
integer                       :: istep        ! counter that advances thru input string INSTR one character at a time
character(len=1)              :: c            ! character in input line being processed
integer                       :: iade         ! ADE (ASCII Decimal Equivalent) of character being tested

   ipos=1                                     ! where to put next character in output string OUTSTR
   lenin=len_trim(instr( 1:len(instr) ))      ! length of INSTR trimmed of trailing spaces
   lenout=len(outstr)                         ! number of characters output string OUTSTR can hold
   outstr=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters

      SCAN_LINE: do istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ichar(c)                        ! get ADE of the character
         EXPAND_TABS : select case (iade)     ! take different actions depending on which character was found
         case(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (mod(ipos-1,tabsize)))
         case(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         case default                         ! c is anything else other than a tab,newline,or return  insert it in output string
            if(ipos > lenout)then
               write(stderr,*)"*notabs* output string overflow"
               exit
            else
               outstr(ipos:ipos)=c
               ipos=ipos+1
            endif
         end select EXPAND_TABS
      enddo SCAN_LINE

      ipos=min(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=len_trim(outstr(:ipos))            ! trim trailing spaces

end subroutine notabs

end module fpm_strings
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/constants.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module tomlf_constants
   use, intrinsic :: iso_fortran_env, only : output_unit
   implicit none
   private

   !> Single precision real numbers
   integer, public, parameter :: tf_sp = selected_real_kind(6)

   !> Double precision real numbers
   integer, public, parameter :: tf_dp = selected_real_kind(15)

   !> Char length for integers
   integer, public, parameter :: tf_i1 = selected_int_kind(2)

   !> Short length for integers
   integer, public, parameter :: tf_i2 = selected_int_kind(4)

   !> Length of default integers
   integer, public, parameter :: tf_i4 = selected_int_kind(9)

   !> Long length for integers
   integer, public, parameter :: tf_i8 = selected_int_kind(18)


   !> Default character kind
   integer, public, parameter :: tfc = selected_char_kind('DEFAULT')

   !> Default float precision, IEEE 754 binary64 values expected
   integer, public, parameter :: tfr = tf_dp

   !> Default integer precision, 64 bit (signed long) range expected
   integer, public, parameter :: tfi = tf_i8

   !> Default output channel
   integer, public, parameter :: tfout = output_unit


   !> Possible escape characters in TOML
   type :: enum_escape

      !> Backslash is used to escape other characters
      character(kind=tfc, len=1) :: backslash = tfc_'\'

      !> Double quotes signal strings with escape characters enabled
      character(kind=tfc, len=1) :: dquote = tfc_'"'

      !> Single quotes signal strings without escape characters enabled
      character(kind=tfc, len=1) :: squote = tfc_''''

      !> Newline character
      character(kind=tfc, len=1) :: newline = achar(10, kind=tfc)

      !> Formfeed character is allowed in strings
      character(kind=tfc, len=1) :: formfeed = achar(12, kind=tfc)

      !> Carriage return is allowed as part of the newline and in strings
      character(kind=tfc, len=1) :: carriage_return = achar(13, kind=tfc)

      !> Backspace is allowed in strings
      character(kind=tfc, len=1) :: bspace = achar(8, kind=tfc)

      !> Tabulators are allowed as whitespace and in strings
      character(kind=tfc, len=1) :: tabulator = achar(9, kind=tfc)

   end type enum_escape

   !> Actual enumerator with TOML escape characters
   type(enum_escape), public, parameter :: toml_escape = enum_escape()


   !> Possible kinds of TOML values in key-value pairs
   type :: enum_type

      !> Invalid type
      integer :: invalid = 100

      !> String type
      integer :: string = 101

      !> Boolean type
      integer :: boolean = 102

      !> Integer type
      integer :: int = 103

      !> Float type
      integer :: float = 104

      !> Datetime type
      integer :: datetime = 105

   end type enum_type

   !> Actual enumerator with TOML value types
   type(enum_type), public, parameter :: toml_type = enum_type()


   !> Single quotes denote literal strings
   character(kind=tfc, len=*), public, parameter :: TOML_SQUOTE = "'"
   !> Double quotes denote strings (with escape character possible)
   character(kind=tfc, len=*), public, parameter :: TOML_DQUOTE = '"'
   character(kind=tfc, len=*), public, parameter :: TOML_NEWLINE = new_line('a') ! \n
   character(kind=tfc, len=*), public, parameter :: TOML_TABULATOR = achar(9) ! \t
   character(kind=tfc, len=*), public, parameter :: TOML_FORMFEED = achar(12) ! \f
   character(kind=tfc, len=*), public, parameter :: TOML_CARRIAGE_RETURN = achar(13) ! \r
   character(kind=tfc, len=*), public, parameter :: TOML_BACKSPACE = achar(8) ! \b
   character(kind=tfc, len=*), public, parameter :: TOML_LOWERCASE = &
      & 'abcdefghijklmnopqrstuvwxyz'
   character(kind=tfc, len=*), public, parameter :: TOML_UPPERCASE = &
      & 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   character(kind=tfc, len=*), public, parameter :: TOML_LETTERS = &
      & TOML_LOWERCASE//TOML_UPPERCASE
   !> Whitespace in TOML are blanks and tabs.
   character(kind=tfc, len=*), public, parameter :: TOML_WHITESPACE = &
      & ' '//toml_escape%tabulator
   character(kind=tfc, len=*), public, parameter :: TOML_DIGITS = '0123456789'
   character(kind=tfc, len=*), public, parameter :: TOML_BINDIGITS = &
      & '01'
   character(kind=tfc, len=*), public, parameter :: TOML_OCTDIGITS = &
      & '01234567'
   character(kind=tfc, len=*), public, parameter :: TOML_HEXDIGITS = &
      & '0123456789ABCDEFabcdef'
   character(kind=tfc, len=*), public, parameter :: TOML_TIMESTAMP = &
      & TOML_DIGITS//'.:+-T Zz'
   !> Allowed characters in TOML bare keys.
   character(kind=tfc, len=*), public, parameter :: TOML_BAREKEY = &
      & TOML_LETTERS//TOML_DIGITS//'_-'
   character(kind=tfc, len=*), public, parameter :: TOML_LITERALS = &
      & TOML_LETTERS//TOML_DIGITS//'_-+.'

end module tomlf_constants
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/version.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Version information on TOML-Fortran
module tomlf_version
   implicit none
   private

   public :: get_tomlf_version
   public :: tomlf_version_string, tomlf_version_compact


   !> String representation of the TOML-Fortran version
   character(len=*), parameter :: tomlf_version_string = "0.2.4"

   !> Major version number of the above TOML-Fortran version
   integer, parameter :: tomlf_major = 0

   !> Minor version number of the above TOML-Fortran version
   integer, parameter :: tomlf_minor = 2

   !> Patch version number of the above TOML-Fortran version
   integer, parameter :: tomlf_patch = 4

   !> Compact numeric representation of the TOML-Fortran version
   integer, parameter :: tomlf_version_compact = &
      & tomlf_major*10000 + tomlf_minor*100 + tomlf_patch


contains


!> Getter function to retrieve TOML-Fortran version
subroutine get_tomlf_version(major, minor, patch, string)

   !> Major version number of the TOML-Fortran version
   integer, intent(out), optional :: major

   !> Minor version number of the TOML-Fortran version
   integer, intent(out), optional :: minor

   !> Patch version number of the TOML-Fortran version
   integer, intent(out), optional :: patch

   !> String representation of the TOML-Fortran version
   character(len=:), allocatable, intent(out), optional :: string

   if (present(major)) then
      major = tomlf_major
   end if
   if (present(minor)) then
      minor = tomlf_minor
   end if
   if (present(patch)) then
      patch = tomlf_patch
   end if
   if (present(string)) then
      string = tomlf_version_string
   end if

end subroutine get_tomlf_version


end module tomlf_version
 
 
!>>>>> build/dependencies/M_CLI2/src/M_CLI2.f90
!VERSION 1.0 20200115
!VERSION 2.0 20200802
!VERSION 3.0 20201021  LONG:SHORT syntax
!VERSION 3.1 20201115  LONG:SHORT:: syntax
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     M_CLI2(3fm) - [ARGUMENTS::M_CLI2] - command line argument parsing
!!     using a prototype command
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Available procedures and variables:
!!
!!      use M_CLI2, only : set_args, get_args, unnamed, remaining, args
!!      use M_CLI2, only : get_args_fixed_length, get_args_fixed_size
!!      use M_CLI2, only : specified
!!      ! convenience functions
!!      use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!      use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!
!!##DESCRIPTION
!!    Allow for command line parsing much like standard Unix command line
!!    parsing using a simple prototype.
!!
!!    Typically one call to SET_ARGS(3f) is made to define the command
!!    arguments, set default values and parse the command line. Then a
!!    call is made to GET_ARGS(3f) for each command keyword to obtain the
!!    argument values.
!!
!!    The documentation for SET_ARGS(3f) and GET_ARGS(3f) provides further
!!    details.
!!
!!##EXAMPLE
!!
!! Sample program using type conversion routines
!!
!!     program demo_M_CLI2
!!     use M_CLI2,  only : set_args, get_args
!!     use M_CLI2,  only : filenames=>unnamed
!!     use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     integer,parameter            :: dp=kind(0.0d0)
!!     !
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real(kind=dp),allocatable    :: point(:)
!!     logical                      :: l, lbig
!!     logical,allocatable          :: logicals(:)
!!     character(len=:),allocatable :: title    ! VARIABLE LENGTH
!!     character(len=40)            :: label    ! FIXED LENGTH
!!     real                         :: p(3)     ! FIXED SIZE
!!     logical                      :: logi(3)  ! FIXED SIZE
!!     !
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o set a value for all keywords.
!!     !   o double-quote strings
!!     !   o set all logical values to F or T.
!!     !   o value delimiter is comma, colon, or space
!!     call set_args('                         &
!!             & -x 1 -y 2 -z 3                &
!!             & -p -1 -2 -3                   &
!!             & --point 11.11, 22.22, 33.33e0 &
!!             & --title "my title" -l F -L F  &
!!             & --logicals  F F F F F         &
!!             & -logi F T F                   &
!!             ! note space between quotes is required
!!             & --label " " &
!!             & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     call get_args('x',x)         ! SCALARS
!!     call get_args('y',y)
!!     call get_args('z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     call get_args('title',title) ! ALLOCATABLE STRING
!!     call get_args('point',point) ! ALLOCATABLE ARRAYS
!!     call get_args('logicals',logicals)
!!     !
!!     ! for NON-ALLOCATABLE VARIABLES
!!
!!     ! for non-allocatable string
!!     call get_args_fixed_length('label',label)
!!
!!     ! for non-allocatable arrays
!!     call get_args_fixed_size('p',p)
!!     call get_args_fixed_size('logi',logi)
!!     !
!!     ! USE VALUES
!!     write(*,*)'x=',x, 'y=',y, 'z=',z, x+y+z
!!     write(*,*)'p=',p
!!     write(*,*)'point=',point
!!     write(*,*)'title=',title
!!     write(*,*)'label=',label
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     write(*,*)'logicals=',logicals
!!     write(*,*)'logi=',logi
!!     !
!!     ! unnamed strings
!!     !
!!     if(size(filenames).gt.0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     !
!!     end program demo_M_CLI2
!!
!!##AUTHOR
!!     John S. Urban, 2019
!!##LICENSE
!!     Public Domain
!===================================================================================================================================
module MM_CLI2
!use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
!use, intrinsic :: iso_fortran_env, only : stdin=>INPUT_UNIT
use, intrinsic :: iso_fortran_env, only : warn=>OUTPUT_UNIT ! ERROR_UNIT
!use M_strings,                     only : upper, lower, quote, replace_str=>replace, unquote, split, string_to_value, atleast
!use M_list,                        only : insert, locate, remove, replace
!use M_args,                        only : longest_command_argument
!use M_journal,                     only : journal
implicit none
integer,parameter,private :: dp=kind(0.0d0)
integer,parameter,private :: sp=kind(0.0)
private
!logical,save :: debug_m_cli2=.true.
logical,public,save :: debug_m_cli2=.false.
!===================================================================================================================================
character(len=*),parameter          :: gen='(*(g0))'
character(len=:),allocatable,public :: unnamed(:)
character(len=:),allocatable,public :: args(:)
character(len=:),allocatable,public :: remaining
public                              :: set_args
public                              :: get_subcommand
public                              :: get_args
public                              :: get_args_fixed_size
public                              :: get_args_fixed_length
public                              :: specified
public                              :: print_dictionary

public                              :: dget, iget, lget, rget, sget, cget
public                              :: dgets, igets, lgets, rgets, sgets, cgets
public                              :: CLI_RESPONSE_FILE

private :: check_commandline
private :: wipe_dictionary
private :: prototype_to_dictionary
private :: update
private :: prototype_and_cmd_args_to_nlist
private :: get

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
   logical                  :: mandatory
end type option
!===================================================================================================================================
character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: shorts(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save          :: counts(:)
logical,allocatable,save          :: present_in(:)
logical,allocatable,save          :: mandatory(:)

logical,save                      :: G_keyword_single_letter=.true.
character(len=:),allocatable,save :: G_passed_in
logical,save                      :: G_remaining_on, G_remaining_option_allowed
character(len=:),allocatable,save :: G_remaining
character(len=:),allocatable,save :: G_subcommand              ! possible candidate for a subcommand
character(len=:),allocatable,save :: G_STOP_MESSAGE
integer,save                      :: G_STOP
logical,save                      :: G_STOPON
logical,save                      :: G_STRICT                  ! strict short and long rules or allow -longname and --shortname
!----------------------------------------------
! try out response files
logical,save                      :: CLI_RESPONSE_FILE=.false. ! allow @name abbreviations
logical,save                      :: G_APPEND                  ! whether to append or replace when duplicate keywords found
logical,save                      :: G_OPTIONS_ONLY            ! process response file only looking for options for get_subcommand()
logical,save                      :: G_RESPONSE                ! allow @name abbreviations
character(len=:),allocatable,save :: G_RESPONSE_IGNORED
!----------------------------------------------
!===================================================================================================================================
! return allocatable arrays
interface  get_args;  module  procedure  get_anyarray_d;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_i;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_r;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_x;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_c;  end interface  ! any size array and any length
interface  get_args;  module  procedure  get_anyarray_l;  end interface  ! any size array

! return scalars
interface  get_args;  module  procedure  get_scalar_d;               end interface
interface  get_args;  module  procedure  get_scalar_i;               end interface
interface  get_args;  module  procedure  get_scalar_real;            end interface
interface  get_args;  module  procedure  get_scalar_complex;         end interface
interface  get_args;  module  procedure  get_scalar_logical;         end interface
interface  get_args;  module  procedure  get_scalar_anylength_c;     end interface  ! any length
! multiple scalars
interface  get_args;  module  procedure  many_args;               end  interface
!==================================================================================================================================
! return non-allocatable arrays
! said in conflict with get_args_*. Using class to get around that.
! that did not work either. Adding size parameter as optional parameter works; but using a different name
interface  get_args_fixed_size;  module procedure get_fixedarray_class;            end interface ! any length, fixed size array
!interface   get_args;           module procedure get_fixedarray_d;                end interface
!interface   get_args;           module procedure get_fixedarray_i;                end interface
!interface   get_args;           module procedure get_fixedarray_r;                end interface
!interface   get_args;           module procedure get_fixedarray_l;                end interface
!interface   get_args;           module procedure get_fixedarray_fixed_length_c;   end interface

interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_a_array; end interface  ! fixed length any size array
interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_scalar_c;  end interface       ! fixed length
!===================================================================================================================================
!intrinsic findloc
!===================================================================================================================================

! ident_1="@(#)M_CLI2::str(3f): {msg_scalar,msg_one}"

private str
interface str
   module procedure msg_scalar, msg_one
end interface str
!===================================================================================================================================

private locate        ! [M_CLI2] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
private insert        ! [M_CLI2] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_i
   private insert_l
private replace       ! [M_CLI2] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_i
   private replace_l
private remove        ! [M_CLI2] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_i
   private remove_l

! Generic subroutine inserts element into allocatable array at specified position
interface  locate;   module procedure locate_c                            ; end interface
interface  insert;   module procedure insert_c,      insert_i,  insert_l  ; end interface
interface  replace;  module procedure replace_c,     replace_i, replace_l ; end interface
interface  remove;   module procedure remove_c,      remove_i,  remove_l  ; end interface
!-----------------------------------------------------------------------------------------------------------------------------------
! convenience functions
interface cgets;module procedure cgs, cg;end interface
interface dgets;module procedure dgs, dg;end interface
interface igets;module procedure igs, ig;end interface
interface lgets;module procedure lgs, lg;end interface
interface rgets;module procedure rgs, rg;end interface
interface sgets;module procedure sgs, sg;end interface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     check_commandline(3f) - [ARGUMENTS:M_CLI2]check command and process
!!     pre-defined options
!!
!!##SYNOPSIS
!!
!!      subroutine check_commandline(help_text,version_text,ierr,errmsg)
!!
!!       character(len=:),allocatable,intent(in),optional :: help_text(:)
!!       character(len=:),allocatable,intent(in),optional :: version_text(:)
!!
!!##DESCRIPTION
!!     Checks the commandline  and processes the implicit --help, --version,
!!     --verbose, and --usage parameters.
!!
!!     If the optional text values are supplied they will be displayed by
!!     --help and --version command-line options, respectively.
!!
!!##OPTIONS
!!
!!     HELP_TEXT     if present, will be displayed if program is called with
!!                   --help switch, and then the program will terminate. If
!!                   not supplied, the command line initialized string will be
!!                   shown when --help is used on the commandline.
!!
!!     VERSION_TEXT  if present, will be displayed if program is called with
!!                   --version switch, and then the program will terminate.
!!
!!        If the first four characters of each line are "@(#)" this prefix
!!        will not be displayed and the last non-blank letter will be
!!        removed from each line. This if for support of the SCCS what(1)
!!        command. If you do not have the what(1) command on GNU/Linux and
!!        Unix platforms you can probably see how it can be used to place
!!        metadata in a binary by entering:
!!
!!         strings demo_commandline|grep '@(#)'|tr '>' '\n'|sed -e 's/  */ /g'
!!
!!##EXAMPLE
!!
!!
!! Typical usage:
!!
!!      program check_commandline
!!      use M_CLI2,  only : unnamed, set_args, get_args
!!      implicit none
!!      integer                      :: i
!!      character(len=:),allocatable :: version_text(:), help_text(:)
!!      real               :: x, y, z
!!      character(len=*),parameter :: cmd='-x 1 -y 2 -z 3'
!!         version_text=[character(len=80) :: "version 1.0","author: me"]
!!         help_text=[character(len=80) :: "wish I put instructions","here","I suppose?"]
!!         call set_args(cmd,help_text,version_text)
!!         call get_args('x',x,'y',y,'z',z)
!!         ! All done cracking the command line. Use the values in your program.
!!         write (*,*)x,y,z
!!         ! the optional unnamed values on the command line are
!!         ! accumulated in the character array "UNNAMED"
!!         if(size(unnamed).gt.0)then
!!            write (*,'(a)')'files:'
!!            write (*,'(i6.6,3a)') (i,'[',unnamed(i),']',i=1,size(unnamed))
!!         endif
!!      end program check_commandline
!===================================================================================================================================
subroutine check_commandline(help_text,version_text)
character(len=:),allocatable,intent(in),optional :: help_text(:)
character(len=:),allocatable,intent(in),optional :: version_text(:)
character(len=:),allocatable                     :: line
integer                                          :: i
integer                                          :: istart
integer                                          :: iback
   if(get('usage').eq.'T')then
      call print_dictionary('USAGE:')
      !x!call default_help()
      call mystop(32)
      return
   endif
   if(present(help_text))then
      if(get('help').eq.'T')then
         do i=1,size(help_text)
            call journal('sc',help_text(i))
         enddo
         call mystop(1,'displayed help text')
         return
      endif
   elseif(get('help').eq.'T')then
      call default_help()
      call mystop(2,'displayed default help text')
      return
   endif
   if(present(version_text))then
      if(get('version').eq.'T')then
         istart=1
         iback=0
         if(size(version_text).gt.0)then
            if(index(version_text(1),'@'//'(#)').eq.1)then ! allow for what(1) syntax
               istart=5
               iback=1
            endif
         endif
         if(debug_m_cli2)write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:ALLOCATED',allocated(version_text)
         if(allocated(version_text).and.debug_m_cli2)then
            write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:LEN',len(version_text)
            write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:SIZE',size(version_text)
            write(*,gen)'<DEBUG>CHECK_COMMANDLINE:VERSION_TEXT:LEN',version_text
         endif
         do i=1,size(version_text)
            !xINTEL BUG*!call journal('sc',version_text(i)(istart:len_trim(version_text(i))-iback))
            line=version_text(i)(istart:len_trim(version_text(i))-iback)
            call journal('sc',line)
         enddo
         call mystop(3,'displayed version text')
         return
      endif
   elseif(get('version').eq.'T')then
      call journal('sc','*check_commandline* no version text')
      call mystop(4,'displayed default version text')
      return
   endif
contains
subroutine default_help()
character(len=:),allocatable :: cmd_name
integer :: ilength
   call get_command_argument(number=0,length=ilength)
   if(allocated(cmd_name))deallocate(cmd_name)
   allocate(character(len=ilength) :: cmd_name)
   call get_command_argument(number=0,value=cmd_name)
   G_passed_in=G_passed_in//repeat(' ',len(G_passed_in))
   call substitute(G_passed_in,' --',NEW_LINE('A')//' --')
   call journal('sc',cmd_name,G_passed_in) ! no help text, echo command and default options
   deallocate(cmd_name)
end subroutine default_help
end subroutine check_commandline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     set_args(3f) - [ARGUMENTS:M_CLI2] command line argument parsing
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_args(definition,help_text,version_text,ierr,errmsg)
!!
!!      character(len=*),intent(in),optional              :: definition
!!      character(len=:),intent(in),allocatable,optional  :: help_text
!!      character(len=:),intent(in),allocatable,optional  :: version_text
!!      integer,intent(out),optional                      :: ierr
!!      character(len=:),intent(out),allocatable,optional :: errmsg
!!##DESCRIPTION
!!
!!     SET_ARGS(3f) requires a unix-like command prototype for defining
!!     arguments and default command-line options. Argument values are then
!!     read using GET_ARGS(3f).
!!
!!     The --help and --version options require the optional
!!     help_text and version_text values to be provided.
!!
!!##OPTIONS
!!
!!      DESCRIPTION   composed of all command arguments concatenated
!!                    into a Unix-like command prototype string. For
!!                    example:
!!
!!                      call set_args('-L F -ints 10,20,30 -title "my title" -R 10.3')
!!
!!                    DESCRIPTION is pre-defined to act as if started with
!!                    the reserved options '--verbose F --usage F --help
!!                    F --version F'. The --usage option is processed when
!!                    the set_args(3f) routine is called. The same is true
!!                    for --help and --version if the optional help_text
!!                    and version_text options are provided.
!!
!!                    see "DEFINING THE PROTOTYPE" in the next section for
!!                    further details.
!!
!!      HELP_TEXT     if present, will be displayed if program is called with
!!                    --help switch, and then the program will terminate. If
!!                    not supplied, the command line initialization string
!!                    will be shown when --help is used on the commandline.
!!
!!      VERSION_TEXT  if present, will be displayed if program is called with
!!                    --version switch, and then the program will terminate.
!!      IERR          if present a non-zero option is returned when an
!!                    error occurs instead of program execution being
!!                    terminated
!!      ERRMSG        a description of the error if ierr is present
!!
!!##DEFINING THE PROTOTYPE
!!         o all keywords on the prototype MUST get a value.
!!
!!         o logicals MUST be set to F or T.
!!
!!         o strings MUST be delimited with double-quotes and
!!           must be at least one space. Internal double-quotes
!!           are represented with two double-quotes.
!!
!!         o numeric keywords are not allowed; but this allows
!!           negative numbers to be used as values.
!!
!!         o lists of values should be comma-delimited unless a
!!           user-specified delimiter is used. The prototype
!!           must use the same array delimiters as the call to
!!           the family of get_args*(3f) called.
!!
!!         o long names (--keyword) should be all lowercase
!!
!!         o The simplest way to have short names is to suffix the long
!!           name with :LETTER If this syntax is used then logical shorts
!!           may be combined on the command line and -- and - prefixes are
!!           strictly enforced.
!!
!!           mapping of short names to long names not using the
!!           --LONGNAME:SHORTNAME syntax is demonstrated in the manpage
!!           for SPECIFIED(3f).
!!
!!         o A very special behavior occurs if the keyword name ends in ::.
!!           The next parameter is taken as a value even if it starts with -.
!!           This is not generally recommended but is noted here for
!!           completeness.
!!
!!         o to define a zero-length allocatable array make the
!!           value a delimiter (usually a comma).
!!
!!         o all unused values go into the character array UNNAMED
!!
!!         o If the prototype ends with "--" a special mode is turned
!!           on where anything after "--" on input goes into the variable
!!           REMAINING and the array ARGS instead of becoming elements in
!!           the UNNAMED array. This is not needed for normal processing.
!!
!!##USAGE
!!      When invoking the program line note that (subject to change) the
!!      following variations from other common command-line parsers:
!!
!!         o Long names should be all lowercase and always more than one
!!           character.
!!
!!         o values for duplicate keywords are appended together with a space
!!           separator when a command line is executed.
!!
!!         o numeric keywords are not allowed; but this allows
!!           negative numbers to be used as values.
!!
!!         o Although not generally recommended you can equivalence
!!           keywords (usually for multi-lingual support). Be aware that
!!           specifying both names of an equivalenced keyword on a command
!!           line will have undefined results (currently, their ASCII
!!           alphabetical order will define what the Fortran variable
!!           values become).
!!
!!           The second of the names should only be called with a
!!           GET_ARGS*(3f) routine if the SPECIFIED(3f) function is .TRUE.
!!           for that name.
!!
!!           Note that allocatable arrays cannot be EQUIVALENCEd in Fortran.
!!
!!         o short keywords cannot be combined unless they were defined
!!           using the --LONGNAME:SHORTNAME syntax. Even then -a -b -c
!!           is required not -abc unless all the keywords are logicals
!!           (Boolean keys).
!!
!!         o shuffling is not supported. Values should follow their
!!           keywords.
!!
!!         o if a parameter value of just "-" is supplied it is
!!           converted to the string "stdin".
!!
!!         o values not matching a keyword go into the character
!!           array "UNUSED".
!!
!!         o if the keyword "--" is encountered the rest of the
!!           command arguments go into the character array "UNUSED".
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_set_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args, unnamed
!!     use M_CLI2,  only : get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real                         :: p(3)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!     integer,allocatable          :: ints(:)
!!     !
!!     !  DEFINE COMMAND (TO SET INITIAL VALUES AND ALLOWED KEYWORDS)
!!     !  AND READ COMMAND LINE
!!     call set_args(' &
!!        ! reals
!!        & -x 1 -y 2.3 -z 3.4e2 &
!!        ! integer array
!!        & -p -1,-2,-3 &
!!        ! always double-quote strings
!!        & --title "my title" &
!!        ! set all logical values to F or T.
!!        & -l F -L F &
!!        ! set allocatable size to zero if you like by using a delimiter
!!        & -ints , &
!!        ! string should be a single character at a minimum
!!        & --label " " &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     !     SCALARS
!!     call get_args('x',x)
!!     call get_args('y',y)
!!     call get_args('z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     call get_args('ints',ints)      ! ALLOCATABLE ARRAY
!!     call get_args('title',title)    ! ALLOCATABLE STRING
!!     call get_args_fixed_size('p',p) ! NON-ALLOCATABLE ARRAY
!!     ! USE VALUES
!!     write(*,*)'x=',x
!!     write(*,*)'y=',y
!!     write(*,*)'z=',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'ints=',ints
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     ! UNNAMED VALUES
!!     if(size(filenames).gt.0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_set_args
!!
!!##RESPONSE FILES
!!
!!  If you have no interest in using external files as abbreviations
!!  you can ignore this section. Otherwise, before calling set_args(3f)
!!  add:
!!
!!     use M_CLI2, only : CLI_response_file
!!     CLI_response_file=.true.
!!
!!  M_CLI2 Response files are small files containing CLI (Command Line
!!  Interface) arguments that end with ".rsp" that can be used when command
!!  lines are so long that they would exceed line length limits or so complex
!!  that it is useful to have a platform-independent method of creating
!!  an abbreviation.
!!
!!  Shell aliases and scripts are often used for similar purposes (and
!!  allow for much more complex conditional execution, of course), but
!!  they generally cannot be used to overcome line length limits and are
!!  typically platform-specific.
!!
!!  Examples of commands that support similar response files are the Clang
!!  and Intel compilers, although there is no standard format for the files.
!!
!!  They are read if you add options of the syntax "@NAME" as the FIRST
!!  parameters on your program command line calls. They are not recursive --
!!  that is, an option in a response file cannot be given the value "@NAME2"
!!  to call another response file.
!!
!!  Note that more than one response name may appear on a command line.
!!
!!  They are case-sensitive names.
!!
!!   LOCATING RESPONSE FILES
!!
!!  A search for the response file always starts with the current directory.
!!  The search then proceeds to look in any additional directories specified
!!  with the colon-delimited environment variable CLI_RESPONSE_PATH.
!!
!!  The first resource file found that results in lines being processed
!!  will be used and processing stops after that first match is found. If
!!  no match is found an error occurs and the program is stopped.
!!
!!   RESPONSE FILE SECTIONS
!!
!!  A simple response file just has options for calling the program in it
!!  prefixed with the word "options".
!!  But they can also contain section headers to denote selections that are
!!  only executed when a specific OS is being used, print messages, and
!!  execute system commands.
!!
!!   SEARCHING FOR OSTYPE IN REGULAR FILES
!!
!!  So assuming the name @NAME was specified on the command line a file
!!  named NAME.rsp will be searched for in all the search directories
!!  and then in that file a string that starts with the string @OSTYPE
!!  (if the environment variables $OS and $OSTYPE are not blank. $OSTYPE
!!  takes precedence over $OS).
!!
!!   SEARCHING FOR UNLABELED DIRECTIVES IN REGULAR FILES
!!
!!  Then, the same files will be searched for lines above any line starting
!!  with "@". That is, if there is no special section for the current OS
!!  it just looks at the top of the file for unlabeled options.
!!
!!   SEARCHING FOR OSTYPE AND NAME IN THE COMPOUND FILE
!!
!!  In addition or instead of files with the same name as the @NAME option
!!  on the command line, you can have one file named after the executable
!!  name that contains multiple abbreviation names.
!!
!!  So if your program executable is named EXEC you create a single file
!!  called EXEC.rsp and can append all the simple files described above
!!  separating them with lines of the form @OSTYPE@NAME or just @NAME.
!!
!!  So if no specific file for the abbreviation is found a file called
!!  "EXEC.rsp" is searched for where "EXEC" is the name of the executable.
!!  This file is always a "compound" response file that uses the following format:
!!
!!  Any compound EXEC.rsp file found in the current or searched directories
!!  will be searched for the string @OSTYPE@NAME first.
!!
!!  Then if nothing is found, the less specific line @NAME is searched for.
!!
!!   THE SEARCH IS OVER
!!
!!  Sounds complicated but actually works quite intuitively. Make a file in
!!  the current directory and put options in it and it will be used. If that
!!  file ends up needing different cases for different platforms add a line
!!  like "@Linux" to the file and some more lines and that will only be
!!  executed if the environment variable OSTYPE or OS is "Linux". If no match
!!  is found for named sections the lines at the top before any "@" lines
!!  will be used as a default if no match is found.
!!
!!  If you end up using a lot of files like this you can combine them all
!!  together and put them into a file called "program_name".rsp and just
!!  put lines like @NAME or @OSTYPE@NAME at that top of each selection.
!!
!!  Now, back to the details on just what you can put in the files.
!!
!!##SPECIFICATION FOR RESPONSE FILES
!!
!!   SIMPLE RESPONSE FILES
!!
!!  The first word of a line is special and has the following meanings:
!!
!!    options|-  Command options following the rules of the SET_ARGS(3f)
!!               prototype. So
!!                o It is preferred to specify a value for all options.
!!                o double-quote strings.
!!                o give a blank string value as " ".
!!                o use F|T for lists of logicals,
!!                o lists of numbers should be comma-delimited.
!!    comment|#  Line is a comment line
!!    system|!   System command.
!!               System commands are executed as a simple call to
!!               system (so a cd(1) or setting a shell variable
!!               would not effect subsequent lines, for example)
!!    print|>    Message to screen
!!    stop       display message and stop program.
!!
!!  So if a program that does nothing but echos its parameters
!!
!!    program testit
!!    use M_CLI2, only : set_args, rget, sget, lget
!!    use M_CLI2, only : CLI_response_file
!!    implicit none
!!       real :: x,y                           ; namelist/args/ x,y
!!       character(len=:),allocatable :: title ; namelist/args/ title
!!       logical :: big                        ; namelist/args/ big
!!       CLI_response_file=.true.
!!       call set_args('-x 10.0 -y 20.0 --title "my title" --big F')
!!       x=rget('x')
!!       y=rget('y')
!!       title=sget('title')
!!       big=lget('big')
!!       write(*,nml=args)
!!    end program testit
!!
!!  And a file in the current directory called "a.rsp" contains
!!
!!     # defaults for project A
!!     options -x 1000 -y 9999
!!     options --title " "
!!     options --big T
!!
!!  The program could be called with
!!
!!     $myprog     # normal call
!!      X=10.0 Y=20.0 TITLE="my title"
!!
!!     $myprog @a  # change defaults as specified in "a.rsp"
!!     X=1000.0 Y=9999.0 TITLE=" "
!!
!!     # change defaults but use any option as normal to override defaults
!!     $myprog @a -y 1234
!!      X=1000.0 Y=1234.0 TITLE=" "
!!
!!   COMPOUND RESPONSE FILES
!!
!!  A compound response file has the same basename as the executable with a
!!  ".rsp" suffix added. So if your program is named "myprg" the filename
!!  must be "myprg.rsp".
!!
!!    Note that here `basename` means the last leaf  of the
!!    name of the program as returned by the Fortran intrinsic
!!    GET_COMMAND_ARGUMENT(0,...) trimmed of anything after a period ("."),
!!    so it is a good idea not to use hidden files.
!!
!!  Unlike simple response files compound response files can contain multiple
!!  setting names.
!!
!!  Specifically in a compound file
!!  if the environment variable $OSTYPE (first) or $OS is set the first search
!!  will be for a line of the form (no leading spaces should be used):
!!
!!    @OSTYPE@alias_name
!!
!!  If no match or if the environment variables $OSTYPE and $OS were not
!!  set or a match is not found then a line of the form
!!
!!    @alias_name
!!
!!  is searched for in simple or compound files. If found subsequent lines
!!  will be ignored that start with "@" until a line not starting with
!!  "@" is encountered.  Lines will then be processed until another line
!!  starting with "@" is found or end-of-file is encountered.
!!
!!   COMPOUND RESPONSE FILE EXAMPLE
!!  An example compound file
!!
!!    #################
!!    @if
!!    > RUNNING TESTS USING RELEASE VERSION AND ifort
!!    options test --release --compiler ifort
!!    #################
!!    @gf
!!    > RUNNING TESTS USING RELEASE VERSION AND gfortran
!!    options test --release --compiler gfortran
!!    #################
!!    @nv
!!    > RUNNING TESTS USING RELEASE VERSION AND nvfortran
!!    options test --release --compiler nvfortran
!!    #################
!!    @nag
!!    > RUNNING TESTS USING RELEASE VERSION AND nagfor
!!    options test --release --compiler nagfor
!!    #
!!    #################
!!    # OS-specific example:
!!    @Linux@install
!!    #
!!    # install executables in directory (assuming install(1) exists)
!!    #
!!    system mkdir -p ~/.local/bin
!!    options run --release T --compiler gfortran --runner "install -vbp -m 0711 -t ~/.local/bin"
!!    @install
!!    STOP INSTALL NOT SUPPORTED ON THIS PLATFORM OR $OSTYPE NOT SET
!!    #
!!    #################
!!    @fpm@testall
!!    #
!!    !fpm test --compiler nvfortran
!!    !fpm test --compiler ifort
!!    !fpm test --compiler gfortran
!!    !fpm test --compiler nagfor
!!    STOP tests complete. Any additional parameters were ignored
!!    #################
!!
!!  Would be used like
!!
!!    fpm @install
!!    fpm @nag --
!!    fpm @testall
!!
!!   NOTES
!!
!!    The intel Fortran compiler now calls the response files "indirect
!!    files" and does not add the implied suffix ".rsp" to the files
!!    anymore. It also allows the @NAME syntax anywhere on the command
!!    line, not just at the beginning. --  20201212
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_args(prototype,help_text,version_text,string,ierr,errmsg)

! ident_2="@(#)M_CLI2::set_args(3f): parse prototype string"

character(len=*),intent(in)                       :: prototype
character(len=:),intent(in),allocatable,optional  :: help_text(:)
character(len=:),intent(in),allocatable,optional  :: version_text(:)
character(len=*),intent(in),optional              :: string
integer,intent(out),optional                      :: ierr
character(len=:),intent(out),allocatable,optional :: errmsg
character(len=:),allocatable                      :: hold               ! stores command line argument
integer                                           :: ibig
   G_response=CLI_RESPONSE_FILE
   G_options_only=.false.
   G_append=.true.
   G_passed_in=''
   G_STOP=0
   G_STOP_MESSAGE=''
   if(present(ierr))then
      G_STOPON=.false.
   else
      G_STOPON=.true.
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   if(allocated(unnamed)) deallocate(unnamed)
   allocate(character(len=ibig) :: unnamed(0))
   if(allocated(args)) deallocate(args)
   allocate(character(len=ibig) :: args(0))

   call wipe_dictionary()
   hold='--version F --usage F --help F --version F '//adjustl(prototype)
   call prototype_and_cmd_args_to_nlist(hold,string)
   if(allocated(G_RESPONSE_IGNORED))then
      if(debug_m_cli2)write(*,gen)'<DEBUG>SET_ARGS:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
      if(size(unnamed).ne.0)write(*,*)'LOGIC ERROR'
      call split(G_RESPONSE_IGNORED,unnamed)
   endif

   if(.not.allocated(unnamed))then
       allocate(character(len=0) :: unnamed(0))
   endif
   if(.not.allocated(args))then
       allocate(character(len=0) :: args(0))
   endif
   call check_commandline(help_text,version_text) ! process --help, --version, --usage
   if(present(ierr))then
      ierr=G_STOP
   endif
   if(present(errmsg))then
      errmsg=G_STOP_MESSAGE
   endif
end subroutine set_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_subcommand(3f) - [ARGUMENTS:M_CLI2] special-case routine for
!!    handling subcommands on a command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function get_subcommand()
!!
!!     character(len=:),allocatable :: get_subcommand
!!
!!##DESCRIPTION
!!    In the special case when creating a program with subcommands it
!!    is assumed the first word on the command line is the subcommand. A
!!    routine is required to handle response file processing, therefore
!!    this routine (optionally processing response files) returns that
!!    first word as the subcommand name.
!!
!!    It should not be used by programs not building a more elaborate
!!    command with subcommands.
!!
!!##RETURNS
!!    NAME   name of subcommand
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_get_subcommand
!!    !x! SUBCOMMANDS
!!    !x! For a command with subcommands like git(1)
!!    !x! you can make separate namelists for each subcommand.
!!    !x! You can call this program which has two subcommands (run, test),
!!    !x! like this:
!!    !x!    demo_get_subcommand --help
!!    !x!    demo_get_subcommand run -x -y -z -title -l -L
!!    !x!    demo_get_subcommand test -title -l -L -testname
!!    !x!    demo_get_subcommand run --help
!!       implicit none
!!    !x! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES
!!       real               :: x=-999.0,y=-999.0,z=-999.0
!!       character(len=80)  :: title="not set"
!!       logical            :: l=.false.
!!       logical            :: l_=.false.
!!       character(len=80)  :: testname="not set"
!!       character(len=20)  :: name
!!       call parse(name) !x! DEFINE AND PARSE COMMAND LINE
!!       !x! ALL DONE CRACKING THE COMMAND LINE.
!!       !x! USE THE VALUES IN YOUR PROGRAM.
!!       write(*,*)'command was ',name
!!       write(*,*)'x,y,z .... ',x,y,z
!!       write(*,*)'title .... ',title
!!       write(*,*)'l,l_ ..... ',l,l_
!!       write(*,*)'testname . ',testname
!!    contains
!!    subroutine parse(name)
!!    !x! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY
!!    use M_CLI2, only : set_args, get_args, get_args_fixed_length
!!    use M_CLI2, only : get_subcommand
!!    use M_CLI2, only : CLI_RESPONSE_FILE
!!    character(len=*)              :: name    ! the subcommand name
!!    character(len=:),allocatable  :: help_text(:), version_text(:)
!!       CLI_RESPONSE_FILE=.true.
!!    ! define version text
!!       version_text=[character(len=80) :: &
!!          '@(#)PROGRAM:     demo_get_subcommand            >', &
!!          '@(#)DESCRIPTION: My demo program  >', &
!!          '@(#)VERSION:     1.0 20200715     >', &
!!          '@(#)AUTHOR:      me, myself, and I>', &
!!          '@(#)LICENSE:     Public Domain    >', &
!!          '' ]
!!        ! general help for "demo_get_subcommand --help"
!!        help_text=[character(len=80) :: &
!!         ' allowed subcommands are          ', &
!!         '   * run  -l -L -title -x -y -z   ', &
!!         '   * test -l -L -title            ', &
!!         '' ]
!!       ! find the subcommand name by looking for first word on command
!!       ! not starting with dash
!!       name = get_subcommand()
!!       select case(name)
!!       case('run')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "run"        ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args( &
!!        & '-x 1 -y 2 -z 3 --title "my title" -l F -L F',&
!!        & help_text,version_text)
!!        call get_args('x',x)
!!        call get_args('y',y)
!!        call get_args('z',z)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!       case('test')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "test"       ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args(&
!!        & '--title "my title" -l F -L F --testname "Test"',&
!!        & help_text,version_text)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!        call get_args_fixed_length('testname',testname)
!!       case default
!!        ! process help and version
!!        call set_args(' ',help_text,version_text)
!!        write(*,'(*(a))')'unknown or missing subcommand [',trim(name),']'
!!        write(*,'(a)')[character(len=80) ::  &
!!        ' allowed subcommands are          ', &
!!        '   * run  -l -L -title -x -y -z   ', &
!!        '   * test -l -L -title            ', &
!!        '' ]
!!        stop
!!       end select
!!    end subroutine parse
!!    end program demo_get_subcommand
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
function get_subcommand() result(sub)

! ident_3="@(#)M_CLI2::get_subcommand(3f): parse prototype string to get subcommand, allowing for response files"

character(len=:),allocatable  :: sub
character(len=:),allocatable  :: cmdarg
character(len=:),allocatable  :: array(:)
character(len=:),allocatable  :: prototype
integer                       :: ilongest
integer                       :: i
integer                       :: j
   G_subcommand=''
   G_options_only=.true.

   if(.not.allocated(unnamed))then
      allocate(character(len=0) :: unnamed(0))
   endif

   ilongest=longest_command_argument()
   allocate(character(len=max(63,ilongest)):: cmdarg)
   cmdarg(:) = ''
   ! look for @NAME if CLI_RESPONSE_FILE=.TRUE. AND LOAD THEM
   do i = 1, command_argument_count()
      call get_command_argument(i, cmdarg)
      if(adjustl(cmdarg(1:1)) .eq. '@')then
         call get_prototype(cmdarg,prototype)
         call split(prototype,array)
         ! assume that if using subcommands first word not starting with dash is the subcommand
         do j=1,size(array)
            if(adjustl(array(j)(1:1)) .ne. '-')then
            G_subcommand=trim(array(j))
            sub=G_subcommand
            exit
         endif
         enddo
      endif
   enddo

   if(G_subcommand.ne.'')then
      sub=G_subcommand
   elseif(size(unnamed).ne.0)then
      sub=unnamed(1)
   else
      cmdarg(:) = ''
      do i = 1, command_argument_count()
         call get_command_argument(i, cmdarg)
         if(adjustl(cmdarg(1:1)) .ne. '-')then
            sub=trim(cmdarg)
           exit
        endif
      enddo
   endif
   G_options_only=.false.
end function get_subcommand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
subroutine set_usage(keyword,description,value)
character(len=*),intent(in) :: keyword
character(len=*),intent(in) :: description
character(len=*),intent(in) :: value
write(*,*)keyword
write(*,*)description
write(*,*)value
! store the descriptions in an array and then apply them when set_args(3f) is called.
! alternatively, could allow for a value as well in lieue of the prototype
end subroutine set_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      prototype_to_dictionary(3f) - [ARGUMENTS:M_CLI2] parse user command
!!      and store tokens into dictionary
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!     recursive subroutine prototype_to_dictionary(string)
!!
!!      character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!      given a string of form
!!
!!        -var value -var value
!!
!!      define dictionary of form
!!
!!        keyword(i), value(i)
!!
!!      o  string values
!!
!!          o must be delimited with double quotes.
!!          o adjacent double quotes put one double quote into value
!!          o must not be null. A blank is specified as " ", not "".
!!
!!      o  logical values
!!
!!          o logical values must have a value
!!
!!      o  leading and trailing blanks are removed from unquoted values
!!
!!
!!##OPTIONS
!!      STRING   string is character input string to define command
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!! sample program:
!!
!!     Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
recursive subroutine prototype_to_dictionary(string)
implicit none

! ident_4="@(#)M_CLI2::prototype_to_dictionary(3f): parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy   ! working copy of string
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
character(len=3)                  :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! character to left of CURRNT
character(len=1)                  :: forwrd  ! character to right of CURRNT
integer,dimension(2)              :: ipnt
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: itype
integer,parameter                 :: VAL=1, KEYW=2
integer                           :: ifwd
integer                           :: ibegin
integer                           :: iend
integer                           :: place

   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=adjustl(string)//'  '

   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in keyword
   ipnt(1)=1           ! pointer to position in value
   itype=VAL           ! itype=1 for value, itype=2 for variable

   delmt="off"
   prev=" "

   G_keyword_single_letter=.true.
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)      ! ensure not past end of string
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)

      if((currnt=="-" .and. prev==" " .and. delmt == "off" .and. index("0123456789.",forwrd) == 0).or.ipoint > islen)then
         ! beginning of a keyword
         if(forwrd.eq.'-')then                      ! change --var to -var so "long" syntax is supported
            !x!dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead (was changing it to _)
            G_keyword_single_letter=.false.         ! flag this is a long keyword
         else
            G_keyword_single_letter=.true.          ! flag this is a short (single letter) keyword
         endif
         if(ipnt(1)-1 >= 1)then                     ! position in value
            ibegin=1
            iend=len_trim(value(:ipnt(1)-1))
            TESTIT: do
               if(iend  ==  0)then                  ! len_trim returned 0, value is blank
                  iend=ibegin
                  exit TESTIT
               elseif(value(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit TESTIT
               endif
            enddo TESTIT
            if(keyword.ne.' ')then
               call update(keyword,value)            ! store name and its value
            elseif( G_remaining_option_allowed)then  ! meaning "--" has been encountered
               call update('_args_',trim(value))
            else
               !x!write(warn,'(*(g0))')'*prototype_to_dictionary* warning: ignoring string [',trim(value),'] for ',trim(keyword)
               G_RESPONSE_IGNORED=TRIM(VALUE)
               if(debug_m_cli2)write(*,gen)'<DEBUG>PROTOTYPE_TO_DICTIONARY:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
            endif
         else
            call locate_key(keyword,place)
            if(keyword.ne.' '.and.place.lt.0)then
               call update(keyword,'F')           ! store name and null value (first pass)
            elseif(keyword.ne.' ')then
               call update(keyword,' ')           ! store name and null value (second pass)
            elseif(.not.G_keyword_single_letter.and.ipoint-2.eq.islen) then ! -- at end of line
               G_remaining_option_allowed=.true.  ! meaning for "--" is that everything on commandline goes into G_remaining
            endif
         endif
         itype=KEYW                            ! change to expecting a keyword
         value=""                              ! clear value for this variable
         keyword=""                            ! clear variable name
         ipnt(1)=1                             ! restart variable value
         ipnt(2)=1                             ! restart variable name

      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  KEYW)then
            ! switch from building a keyword string to building a value string
            itype=VAL
            ! beginning of a delimited value
         elseif(currnt  ==  """".and.itype  ==  VAL)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype.eq.VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
               delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
               delmt="off"
            else
               delmt="on"
            endif
            if(prev /= """")then  ! leave quotes where found them
               if(itype.eq.VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current keyword or value
            if(itype.eq.VAL)then
               value=value//currnt
            else
               keyword=keyword//currnt
            endif
            ipnt(itype)=ipnt(itype)+1
         endif

      endif

      prev=currnt
      if(ipoint <= islen)then
         cycle
      else
         exit
      endif
   enddo

end subroutine prototype_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    specified(3f) - [ARGUMENTS:M_CLI2] return true if keyword was present
!!    on command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure function specified(name)
!!
!!     character(len=*),intent(in) :: name
!!     logical :: specified
!!
!!##DESCRIPTION
!!
!!    specified(3f) returns .true. if the specified keyword was present on
!!    the command line.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to query the presence of
!!
!!##RETURNS
!!    SPECIFIED  returns .TRUE. if specified NAME was present on the command
!!               line when the program was invoked.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!    program demo_specified
!!    use M_CLI2,  only : set_args, get_args, specified
!!    implicit none
!!    ! DEFINE ARGS
!!    integer                 :: flag
!!    integer,allocatable     :: ints(:)
!!    real,allocatable        :: twonames(:)
!!
!!    ! IT IS A BAD IDEA TO NOT HAVE THE SAME DEFAULT VALUE FOR ALIASED
!!    ! NAMES BUT CURRENTLY YOU STILL SPECIFY THEM
!!       call set_args(' -flag 1 -f 1 -ints 1,2,3 -i 1,2,3 -twonames 11.3 -T 11.3')
!!
!!    ! ASSIGN VALUES TO ELEMENTS CONDITIONALLY CALLING WITH SHORT NAME
!!       call get_args('flag',flag)
!!       if(specified('f'))call get_args('f',flag)
!!       call get_args('ints',ints)
!!       if(specified('i'))call get_args('i',ints)
!!       call get_args('twonames',twonames)
!!       if(specified('T'))call get_args('T',twonames)
!!
!!       ! IF YOU WANT TO KNOW IF GROUPS OF PARAMETERS WERE SPECIFIED USE
!!       ! ANY(3f) and ALL(3f)
!!       write(*,*)specified(['twonames','T       '])
!!       write(*,*)'ANY:',any(specified(['twonames','T       ']))
!!       write(*,*)'ALL:',all(specified(['twonames','T       ']))
!!
!!       ! FOR MUTUALLY EXCLUSIVE
!!       if (all(specified(['twonames','T       '])))then
!!           write(*,*)'You specified both names -T and -twonames'
!!       endif
!!
!!       ! FOR REQUIRED PARAMETER
!!       if (.not.any(specified(['twonames','T       '])))then
!!           write(*,*)'You must specify -T or -twonames'
!!       endif
!!
!!    ! USE VALUES
!!       write(*,*)'flag=',flag
!!       write(*,*)'ints=',ints
!!       write(*,*)'twonames=',twonames
!!    end program demo_specified
!!
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!===================================================================================================================================
elemental impure function specified(key)
character(len=*),intent(in) :: key
logical                     :: specified
integer                     :: place
   call locate_key(key,place)                   ! find where string is or should be
   if(place.lt.1)then
      specified=.false.
   else
      specified=present_in(place)
   endif
end function specified
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      update(3f) - [ARGUMENTS:M_CLI2] update internal dictionary given
!!      keyword and value
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!
!!     subroutine update(key,val)
!!
!!      character(len=*),intent(in)           :: key
!!      character(len=*),intent(in),optional  :: val
!!##DESCRIPTION
!!      Update internal dictionary in M_CLI2(3fm) module.
!!##OPTIONS
!!      key  name of keyword to add, replace, or delete from dictionary
!!      val  if present add or replace value associated with keyword. If not
!!           present remove keyword entry from dictionary.
!!
!!           If "present" is true, a value will be appended
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place, ii
integer                               :: iilen
character(len=:),allocatable          :: val_local
character(len=:),allocatable          :: short
character(len=:),allocatable          :: long
character(len=:),allocatable          :: long_short(:)
integer                               :: isize
logical                               :: set_mandatory
   set_mandatory=.false.
   call split(trim(key),long_short,':',nulls='return') ! split long:short keyname or long:short:: or long:: or short::
   ! check for :: on end
   isize=size(long_short)
   if(isize.gt.0)then                     ! very special-purpose syntax where if ends in :: next field is a value even
      if(long_short(isize).eq.'')then     ! if it starts with a dash, for --flags option on fpm(1).
         set_mandatory=.true.
         long_short=long_short(:isize-1)
      endif
   endif
   select case(size(long_short))
   case(0)
      long=''
      short=''
   case(1)
      long=trim(long_short(1))
      if(len_trim(long).eq.1)then
         !x!ii= findloc (shorts, long, dim=1) ! if parsing arguments on line and a short keyword look up long value
         ii=maxloc([0,merge(1, 0, shorts.eq.long)],dim=1)
         if(ii.gt.1)then
            long=keywords(ii-1)
         endif
         short=long
      else
         short=''
      endif
   case(2)
      G_STRICT=.true.  ! strict short and long rules so do not allow -longname and --shortname
      long=trim(long_short(1))
      short=trim(long_short(2))
   case default
      write(warn,*)'WARNING: incorrect syntax for key: ',trim(key)
      long=trim(long_short(1))
      short=trim(long_short(2))
   end select
   if(present(val))then
      val_local=val
      iilen=len_trim(val_local)
      call locate_key(long,place)                  ! find where string is or should be
      if(place.lt.1)then                                ! if string was not found insert it
         call insert(keywords,long,iabs(place))
         call insert(values,val_local,iabs(place))
         call insert(counts,iilen,iabs(place))
         call insert(shorts,short,iabs(place))
         call insert(present_in,.true.,iabs(place))
         call insert(mandatory,set_mandatory,iabs(place))
      else
         if(present_in(place))then                      ! if multiple keywords append values with space between them
            if(G_append)then
               if(values(place)(1:1).eq.'"')then
               ! UNDESIRABLE: will ignore previous blank entries
                  val_local='"'//trim(unquote(values(place)))//' '//trim(unquote(val_local))//'"'
               else
                  val_local=values(place)//' '//val_local
               endif
            endif
            iilen=len_trim(val_local)
         endif
         call replace(values,val_local,place)
         call replace(counts,iilen,place)
         call replace(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate_key(long,place)                       ! check name as long and short
      if(place.gt.0)then
         call remove(keywords,place)
         call remove(values,place)
         call remove(counts,place)
         call remove(shorts,place)
         call remove(present_in,place)
         call remove(mandatory,place)
      endif
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      wipe_dictionary(3fp) - [ARGUMENTS:M_CLI2] reset private M_CLI2(3fm) dictionary to empty
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!      subroutine wipe_dictionary()
!!##DESCRIPTION
!!      reset private M_CLI2(3fm) dictionary to empty
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_wipe_dictionary
!!      use M_CLI2, only : dictionary
!!         call wipe_dictionary()
!!      end program demo_wipe_dictionary
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
   if(allocated(shorts))deallocate(shorts)
   allocate(character(len=0) :: shorts(0))
   if(allocated(present_in))deallocate(present_in)
   allocate(present_in(0))
   if(allocated(mandatory))deallocate(mandatory)
   allocate(mandatory(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      get(3f) - [ARGUMENTS:M_CLI2] get dictionary value associated with key name in private M_CLI2(3fm) dictionary
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!      Get dictionary value associated with key name in private M_CLI2(3fm) dictionary.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate_key(key,place)
   if(place.lt.1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      prototype_and_cmd_args_to_nlist(3f) - [ARGUMENTS:M_CLI2] convert Unix-like command arguments to table
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!     subroutine prototype_and_cmd_args_to_nlist(prototype)
!!
!!      character(len=*)             :: prototype
!!##DESCRIPTION
!!    create dictionary with character keywords, values, and value lengths
!!    using the routines for maintaining a list from command line arguments.
!!##OPTIONS
!!      prototype
!!##EXAMPLE
!!
!!
!! Sample program
!!
!!      program demo_prototype_and_cmd_args_to_nlist
!!      use M_CLI2,  only : prototype_and_cmd_args_to_nlist, unnamed
!!      implicit none
!!      character(len=:),allocatable :: readme
!!      character(len=256)           :: message
!!      integer                      :: ios
!!      integer                      :: i
!!      doubleprecision              :: something
!!
!!      ! define arguments
!!      logical            :: l,h,v
!!      real               :: p(2)
!!      complex            :: c
!!      doubleprecision    :: x,y,z
!!
!!      ! uppercase keywords get an underscore to make it easier o remember
!!      logical            :: l_,h_,v_
!!      character(len=256) :: a_,b_                  ! character variables must be long enough to hold returned value
!!      integer            :: c_(3)
!!
!!         ! give command template with default values
!!         ! all values except logicals get a value.
!!         ! strings must be delimited with double quotes
!!         ! A string has to have at least one character as for -A
!!         ! lists of numbers should be comma-delimited. No spaces are allowed in lists of numbers
!!         call prototype_and_cmd_args_to_nlist('&
!!         & -l -v -h -LVH -x 0 -y 0.0 -z 0.0d0 -p 0,0 &
!!         & -A " " -B "Value B" -C 10,20,30 -c (-123,-456)',readme)
!!
!!         call get_args('x',x,'y',y,'z',z)
!!            something=sqrt(x**2+y**2+z**2)
!!            write (*,*)something,x,y,z
!!            if(size(unnamed).gt.0)then
!!               write (*,'(a)')'files:'
!!               write (*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!            endif
!!      end program demo_prototype_and_cmd_args_to_nlist
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine prototype_and_cmd_args_to_nlist(prototype,string)
implicit none

! ident_5="@(#)M_CLI2::prototype_and_cmd_args_to_nlist: create dictionary from prototype if not null and update from command line"

character(len=*),intent(in)           :: prototype
character(len=*),intent(in),optional  :: string
integer                               :: ibig
integer                               :: itrim
integer                               :: iused

   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:START'
   G_passed_in=prototype                            ! make global copy for printing
   G_STRICT=.false.  ! strict short and long rules or allow -longname and --shortname

   ibig=longest_command_argument()                  ! bug in gfortran. len=0 should be fine
   ibig=max(ibig,1)
   if(allocated(unnamed))deallocate(unnamed)
   allocate(character(len=ibig) :: unnamed(0))
   if(allocated(args))deallocate(args)
   allocate(character(len=ibig) :: args(0))

   G_remaining_option_allowed=.false.
   G_remaining_on=.false.
   G_remaining=''
   if(prototype.ne.'')then
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype

      ! if short keywords not used by user allow them for standard options

      call locate_key('h',iused)
      if(iused.le.0)then
         call update('help')
         call update('help:h','F')
      endif

      call locate_key('v',iused)
      if(iused.le.0)then
         call update('version')
         call update('version:v','F')
      endif

      call locate_key('V',iused)
      if(iused.le.0)then
         call update('verbose')
         call update('verbose:V','F')
      endif

      call locate_key('u',iused)
      if(iused.le.0)then
         call update('usage')
         call update('usage:u','F')
      endif

      present_in=.false.                            ! reset all values to false so everything gets written
   endif

   if(present(string))then                          ! instead of command line arguments use another prototype string
      if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL PROTOTYPE_TO_DICTIONARY:STRING=',STRING
      call prototype_to_dictionary(string)          ! build dictionary from prototype
   else
      if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL CMD_ARGS_TO_DICTIONARY:CHECK=',.true.
      call cmd_args_to_dictionary(check=.true.)
   endif

   if(len(G_remaining).gt.1)then                    ! if -- was in prototype then after -- on input return rest in this string
      itrim=len(G_remaining)
      if(G_remaining(itrim:itrim).eq.' ')then       ! was adding a space at end as building it, but do not want to remove blanks
         G_remaining=G_remaining(:itrim-1)
      endif
      remaining=G_remaining
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:NORMAL END'
end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_response(name)
character(len=*),intent(in) :: name
character(len=:),allocatable :: prototype
logical :: hold
   if(debug_m_cli2)write(*,gen)'<DEBUG>EXPAND_RESPONSE:START:NAME=',name
   call get_prototype(name,prototype)
   if(prototype.ne.'')then
      hold=G_append
      G_append=.false.
      if(debug_m_cli2)write(*,gen)'<DEBUG>EXPAND_RESPONSE:CALL PROTOTYPE_TO_DICTIONARY:PROTOTYPE=',prototype
      call prototype_to_dictionary(prototype)       ! build dictionary from prototype
      G_append=hold
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'
end subroutine expand_response
!===================================================================================================================================
subroutine get_prototype(name,prototype) ! process @name abbreviations
character(len=*),intent(in) :: name
character(len=:),allocatable,intent(out) :: prototype
character(len=:),allocatable             :: filename
character(len=:),allocatable             :: os
character(len=:),allocatable             :: plain_name
character(len=:),allocatable             :: search_for
integer                                  :: lun
integer                                  :: ios
integer                                  :: itrim
character(len=4096)                      :: line !x! assuming input never this long
character(len=256)                       :: message
character(len=:),allocatable             :: array(:) ! output array of tokens
integer                                  :: lines_processed
   lines_processed=0
   plain_name=name//'  '
   plain_name=trim(name(2:))
   os= '@' // get_env('OSTYPE',get_env('OS'))
   if(debug_m_cli2)write(*,gen)'<DEBUG>GET_PROTOTYPE:OS=',OS

   search_for=''
   ! look for NAME.rsp and see if there is an @OS  section in it and position to it and read
   if(os.ne.'@')then
      search_for=os
      call find_and_read_response_file(plain_name)
      if(lines_processed.ne.0)return
   endif

   ! look for NAME.rsp and see if there is anything before an OS-specific section
   search_for=''
   call find_and_read_response_file(plain_name)
   if(lines_processed.ne.0)return

   ! look for ARG0.rsp  with @OS@NAME  section in it and position to it
   if(os.ne.'@')then
      search_for=os//name
      call find_and_read_response_file(basename(get_name(),suffix=.true.))
      if(lines_processed.ne.0)return
   endif

   ! look for ARG0.rsp  with a section called @NAME in it and position to it
   search_for=name
   call find_and_read_response_file(basename(get_name(),suffix=.true.))
   if(lines_processed.ne.0)return

   write(*,gen)'<ERROR> response name ['//trim(name)//'] not found'
   stop 1
contains
!===================================================================================================================================
subroutine find_and_read_response_file(rname)
! seach for a simple file named the same as the @NAME field with one entry assumed in it
character(len=*),intent(in)  :: rname
character(len=:),allocatable :: paths(:)
character(len=:),allocatable :: testpath
character(len=256)           :: message
integer                      :: i
integer                      :: ios
   prototype=''
   ! look for NAME.rsp
   filename=rname//'.rsp'
   if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:FILENAME=',filename

   ! look for name.rsp in directories from environment variable assumed to be a colon-separated list of directories
   call split(get_env('CLI_RESPONSE_PATH'),paths)
   paths=[character(len=len(paths)) :: ' ',paths]
   if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:PATHS=',paths

   do i=1,size(paths)
      testpath=join_path(paths(i),filename)
      lun=fileopen(testpath,message)
      if(lun.ne.-1)then
         if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:SEARCH_FOR=',search_for
         if(search_for.ne.'') call position_response() ! set to end of file or where string was found
         call process_response()
         if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:LINES_PROCESSED=',LINES_PROCESSED
         close(unit=lun,iostat=ios)
         if(debug_m_cli2)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:CLOSE:LUN=',LUN,' IOSTAT=',IOS
         if(lines_processed.ne.0)exit
      endif
   enddo

end subroutine find_and_read_response_file
!===================================================================================================================================
subroutine position_response()
integer :: ios
   line=''
   INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         if(debug_m_cli2)write(*,gen)'<DEBUG>POSITION_RESPONSE:EOF'
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios.ne.0)then
         write(*,gen)'<ERROR>*position_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(line.eq.search_for)return
   enddo INFINITE
end subroutine position_response
!===================================================================================================================================
subroutine process_response()
   line=''
   lines_processed=0
      INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios.ne.0)then
         write(*,gen)'<ERROR>*process_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(index(line//' ','#').eq.1)cycle
      if(line.ne.'')then

         if(index(line,'@').eq.1.and.lines_processed.ne.0)exit INFINITE

         call split(line,array) ! get first word
         itrim=len_trim(array(1))+2
         line=line(itrim:)

         PROCESS: select case(lower(array(1)))
         case('comment','#','')
         case('system','!','$')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            call execute_command_line(line)
         case('options','option','-')
            lines_processed= lines_processed+1
            prototype=prototype//' '//trim(line)
         case('print','>','echo')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            write(*,'(a)')trim(line)
         case('stop')
            if(G_options_only)exit PROCESS
            write(*,'(a)')trim(line)
            stop
         case default
            if(array(1)(1:1).eq.'@')cycle INFINITE !skip adjacent @ lines from first
            lines_processed= lines_processed+1
            write(*,'(*(g0))')'unknown response keyword [',array(1),'] with options of [',trim(line),']'
         end select PROCESS

      endif
      enddo INFINITE
end subroutine process_response

end subroutine get_prototype
!===================================================================================================================================
function fileopen(filename,message) result(lun)
character(len=*),intent(in)              :: filename
character(len=*),intent(out),optional    :: message
integer                                  :: lun
integer                                  :: ios
character(len=256)                       :: message_local

   ios=0
   message_local=''
   open(file=filename,newunit=lun,&
    & form='formatted',access='sequential',action='read',&
    & position='rewind',status='old',iostat=ios,iomsg=message_local)

   if(ios.ne.0)then
      lun=-1
      if(present(message))then
         message=trim(message_local)
      else
         write(*,gen)trim(message_local)
      endif
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>FILEOPEN:FILENAME=',filename,' LUN=',lun,' IOS=',IOS,' MESSAGE=',trim(message_local)

end function fileopen
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   if(NAME.ne.'')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
          !x!print *, NAME, " is not defined in the environment. Strange..."
          VALUE=''
      case (2)
          !x!print *, "This processor doesn't support environment variables. Boooh!"
          VALUE=''
      case default
          ! make string to hold value of sufficient size
          if(allocated(value))deallocate(value)
          allocate(character(len=max(howbig,1)) :: VALUE)
          ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
          if(stat.ne.0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env
!===================================================================================================================================
function join_path(a1,a2,a3,a4,a5) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1.ne.'')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   path=adjustl(path//'  ')
   call substitute(path,filesep//filesep,'',start=2) ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function join_path
!===================================================================================================================================
function get_name() result(name)
! get the pathname of arg0
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=istat)
   if(istat.eq.0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=istat)
      if(istat.eq.0)then
         inquire(file=arg0,iostat=istat,name=long_name)
         name=trim(long_name)
      else
         name=arg0
      endif
   endif
end function get_name
!===================================================================================================================================
function basename(path,suffix) result (base)
    ! Extract filename from path with/without suffix
    !
character(*), intent(In) :: path
logical, intent(in), optional :: suffix
character(:), allocatable :: base

character(:), allocatable :: file_parts(:)
logical :: with_suffix

   if (.not.present(suffix)) then
      with_suffix = .true.
   else
      with_suffix = suffix
   endif

   if (with_suffix) then
      call split(path,file_parts,delimiters='\/')
      if(size(file_parts).gt.0)then
         base = trim(file_parts(size(file_parts)))
      else
         base = ''
      endif
   else
      call split(path,file_parts,delimiters='\/.')
      if(size(file_parts).ge.2)then
         base = trim(file_parts(size(file_parts)-1))
      else
         base = ''
      endif
   endif
end function basename
!===================================================================================================================================
function separator2() result(sep)
! use the pathname returned as arg0 to determine pathname separator
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
logical                      :: existing
character(len=1)             :: sep
character(len=4096)          :: name
character(len=:),allocatable :: fname
   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=istat)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=istat)
   ! check argument name
   if(index(arg0,'\').ne.0)then
      sep='\'
   elseif(index(arg0,'/').ne.0)then
      sep='/'
   else
      ! try name returned by INQUIRE(3f)
      existing=.false.
      name=' '
      inquire(file=arg0,iostat=istat,exist=existing,name=name)
      if(index(name,'\').ne.0)then
         sep='\'
      elseif(index(name,'/').ne.0)then
         sep='/'
      else
         ! well, try some common syntax and assume in current directory
         fname='.\'//arg0
         inquire(file=fname,iostat=istat,exist=existing)
         if(existing)then
            sep='/'
         else
            fname='./'//arg0
            inquire(file=fname,iostat=istat,exist=existing)
            if(existing)then
               sep='/'
            else
               !x!write(*,gen)'<WARNING>unknown system directory path separator'
               sep='/'
            endif
         endif
      endif
   endif
end function separator2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function separator() result(sep)
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!   First testing for the existence of "/.",  then if that fails a list
!!   of variable names assumed to contain directory paths {PATH|HOME} are
!!   examined first for a backslash, then a slash.  Assuming basically the
!!   choice is a ULS or MSWindows system, and users can do weird things like
!!   put a backslash in a ULS path and break it.
!!
!!   Therefore can be very system dependent. If the queries fail the
!!   default returned is "/".
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator

! use the pathname returned as arg0 to determine pathname separator
implicit none
integer                      :: ios
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!x!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !x!FORT BUG:if(sep_cache.ne.' ')then  ! use cached value.
    !x!FORT BUG:    sep=sep_cache
    !x!FORT BUG:    return
    !x!FORT BUG:endif
    if(isep.ne.-1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames to do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios.eq.0)then
        sep='/'
        exit FOUND
    endif
    ! check variables names common to many platforms that usually have a
    ! directory path in them although a ULS file can contain a backslash
    ! and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using
    ! POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME']
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),'\').ne.0)then
          sep='\'
          exit FOUND
       elseif(index(get_env(envnames(i)),'/').ne.0)then
          sep='/'
          exit FOUND
       endif
    enddo

    write(*,*)'<WARNING>unknown system directory path separator'
    sep='\'
    endblock FOUND
    !x!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary(check)
! convert command line arguments to dictionary entries
logical,intent(in),optional  :: check
logical                      :: check_local
!x!logical                      :: guess_if_value
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i, jj, kk
integer                      :: ilength, istatus, imax
character(len=1)             :: letter
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
logical                      :: next_mandatory
   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START'
   next_mandatory=.false.
   if(present(check))then
      check_local=check
   else
      check_local=.false.
   endif
   nomore=.false.
   pointer=0
   lastkeyword=' '
   G_keyword_single_letter=.true.
   i=1
   GET_ARGS: do while (get_next_argument()) ! insert and replace entries

      if( current_argument .eq. '-' .and. nomore .eqv. .true. )then   ! sort of
      elseif( current_argument .eq. '-')then                          ! sort of
         current_argument='"stdin"'
      endif
      if( current_argument .eq. '--' .and. nomore .eqv. .true. )then  ! -- was already encountered
      elseif( current_argument .eq. '--' )then                        ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         if(G_remaining_option_allowed)then
            G_remaining_on=.true.
         endif
         cycle
      endif

      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '

      !x!guess_if_value=maybe_value()

      if(.not.next_mandatory.and..not.nomore.and.current_argument_padded(1:2).eq.'--')then    ! beginning of long word
         G_keyword_single_letter=.false.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(3:),pointer)
         if(pointer.le.0.and.check_local)then
            call print_dictionary('UNKNOWN LONG KEYWORD: '//current_argument)
            call mystop(1)
            return
         endif
         lastkeyword=trim(current_argument_padded(3:))
         next_mandatory=mandatory(pointer)
      elseif(.not.next_mandatory &
      & .and..not.nomore &
      & .and.current_argument_padded(1:1).eq.'-' &
      & .and.index("0123456789.",dummy(2:2)).eq.0)then
      ! short word
         G_keyword_single_letter=.true.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(2:),pointer)
         if(pointer.le.0.and.check_local)then
            jj=len(current_argument)
            if(G_STRICT.and.jj.gt.2)then  ! in strict mode this might be multiple single-character values
              do kk=2,jj
                 letter=current_argument_padded(kk:kk)
                 call locate_key(letter,pointer)
                 if(pointer.gt.0)then
                    call update(keywords(pointer),'T')
                 else
                    call print_dictionary('UNKNOWN COMPOUND SHORT KEYWORD:'//letter//' in '//current_argument)
                    call mystop(2)
                    return
                 endif
                 current_argument='-'//current_argument_padded(jj:jj)
              enddo
            else
               call print_dictionary('UNKNOWN SHORT KEYWORD: '//current_argument)
               call mystop(2)
               return
            endif
         endif
         lastkeyword=trim(current_argument_padded(2:))
         next_mandatory=mandatory(pointer)
      elseif(pointer.eq.0)then                                       ! unnamed arguments
         if(G_remaining_on)then
            if(len(current_argument).lt.1)then
               G_remaining=G_remaining//'"" '
            elseif(current_argument(1:1).eq.'-')then
               !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
               G_remaining=G_remaining//'"'//current_argument//'" '
            else
               G_remaining=G_remaining//'"'//current_argument//'" '
            endif
            imax=max(len(args),len(current_argument))
            args=[character(len=imax) :: args,current_argument]
         else
            imax=max(len(unnamed),len(current_argument))
            if(index(current_argument//' ','@').eq.1.and.G_response)then
               if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:1:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
               call expand_response(current_argument)
            else
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
         endif
      else
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1).eq.'"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then  ! assume boolean parameter
            if(current_argument.ne.' ')then
               if(G_remaining_on)then
                  if(len(current_argument).lt.1)then
                        G_remaining=G_remaining//'"" '
                  elseif(current_argument(1:1).eq.'-')then
                       !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  else
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  endif
                  imax=max(len(args),len(current_argument))
                  args=[character(len=imax) :: args,current_argument]
               else
                  imax=max(len(unnamed),len(current_argument))
                  if(index(current_argument//' ','@').eq.1.and.G_response)then
               if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:2:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
                     call expand_response(current_argument)
                  else
                     unnamed=[character(len=imax) :: unnamed,current_argument]
                  endif
               endif
            endif
            current_argument='T'
         endif
         call update(keywords(pointer),current_argument)
         pointer=0
         lastkeyword=''
         next_mandatory=.false.
      endif
   enddo GET_ARGS
   if(lastkeyword.ne.'')then
      call ifnull()
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:NORMAL END'

contains

subroutine ifnull()
   oldvalue=get(lastkeyword)//' '
   if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then
      call update(lastkeyword,'T')
   elseif(oldvalue(1:1).eq.'"')then
      call update(lastkeyword,'" "')
   else
      call update(lastkeyword,' ')
   endif
end subroutine ifnull

function get_next_argument()
!
! get next argument from command line into allocated variable current_argument
!
logical,save :: hadequal=.false.
character(len=:),allocatable,save :: right_hand_side
logical :: get_next_argument
integer :: iright
integer :: iequal

   if(hadequal)then  ! use left-over value from previous -NAME=VALUE syntax
      current_argument=right_hand_side
      right_hand_side=''
      hadequal=.false.
      get_next_argument=.true.
      ilength=len(current_argument)
      return
   endif

   if(i>command_argument_count())then
      get_next_argument=.false.
      return
   else
      get_next_argument=.true.
   endif

   call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
   if(istatus /= 0) then                                                                          ! on error
      write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
         &'status=',istatus,&
         &'length=',ilength
      get_next_argument=.false.
   else
      ilength=max(ilength,1)
      if(allocated(current_argument))deallocate(current_argument)
      allocate(character(len=ilength) :: current_argument)
      call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
      if(istatus /= 0) then                                                                       ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength,&
            &'target length=',len(current_argument)
         get_next_argument=.false.
       endif

       ! if an argument keyword and an equal before a space split on equal and save right hand side for next call
       if(nomore)then
       elseif(len(current_argument).eq.0)then
       else
          iright=index(current_argument,' ')
          if(iright.eq.0)iright=len(current_argument)
          iequal=index(current_argument(:iright),'=')
          if(next_mandatory)then
          elseif(iequal.ne.0.and.current_argument(1:1).eq.'-')then
             if(iequal.ne.len(current_argument))then
                right_hand_side=current_argument(iequal+1:)
             else
                right_hand_side=''
             endif
             hadequal=.true.
             current_argument=current_argument(:iequal-1)
          endif
       endif
   endif
   i=i+1
end function get_next_argument

function maybe_value()
! if previous keyword value type is a string and it was
! given a null string because this value starts with a -
! try to see if this is a string value starting with a -
! to try to solve the vexing problem of values starting
! with a dash.
logical :: maybe_value
integer :: pointer
character(len=:),allocatable :: oldvalue

   oldvalue=get(lastkeyword)//' '
   if(current_argument_padded(1:1).ne.'-')then
      maybe_value=.true.
   elseif(oldvalue(1:1).ne.'"')then
      maybe_value=.false.
   elseif(index(current_argument,' ').ne.0)then
      maybe_value=.true.
   elseif(scan(current_argument,",:;!@#$%^&*+=()[]{}\|'""./><?").ne.0)then
      maybe_value=.true.
   else  ! the last value was a null string so see if this matches an allowed parameter
      pointer=0
      if(current_argument_padded(1:2).eq.'--')then
         call locate_key(current_argument_padded(3:),pointer)
      elseif(current_argument_padded(1:1).eq.'-')then
         call locate_key(current_argument_padded(2:),pointer)
      endif
      if(pointer.le.0)then
         maybe_value=.true.
      else                   ! matched an option name so LIKELY is not a value
         maybe_value=.false.
      endif
   endif
end function maybe_value

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     print_dictionary(3f) - [ARGUMENTS:M_CLI2] print internal dictionary created by calls to set_args(3f)
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!
!!     subroutine print_dictionary(header,stop)
!!
!!      character(len=*),intent(in),optional :: header
!!      logical,intent(in),optional          :: stop
!!##DESCRIPTION
!!    Print the internal dictionary created by calls to set_args(3f).
!!    This routine is intended to print the state of the argument list
!!    if an error occurs in using the set_args(3f) procedure.
!!##OPTIONS
!!     HEADER  label to print before printing the state of the command
!!             argument list.
!!     STOP    logical value that if true stops the program after displaying
!!             the dictionary.
!!##EXAMPLE
!!
!!
!!
!! Typical usage:
!!
!!       program demo_print_dictionary
!!       use M_CLI2,  only : set_args, get_args
!!       implicit none
!!       real :: x, y, z
!!          call set_args('-x 10 -y 20 -z 30')
!!          call get_args('x',x,'y',y,'z',z)
!!          ! all done cracking the command line; use the values in your program.
!!          write(*,*)x,y,z
!!       end program demo_print_dictionary
!!
!!      Sample output
!!
!!      Calling the sample program with an unknown parameter or the --usage
!!      switch produces the following:
!!
!!         $ ./demo_print_dictionary -A
!!         UNKNOWN SHORT KEYWORD: -A
!!         KEYWORD             PRESENT  VALUE
!!         z                   F        [3]
!!         y                   F        [2]
!!         x                   F        [1]
!!         help                F        [F]
!!         version             F        [F]
!!         usage               F        [F]
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine print_dictionary(header,stop)
character(len=*),intent(in),optional :: header
logical,intent(in),optional          :: stop
integer          :: i
   if(present(header))then
      if(header.ne.'')then
         write(warn,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords).gt.0)then
         write(warn,'(a,1x,a,1x,a,1x,a)')atleast('KEYWORD',max(len(keywords),8)),'SHORT','PRESENT','VALUE'
         write(warn,'(*(a,1x,a5,1x,l1,8x,"[",a,"]",/))') &
         & (atleast(keywords(i),max(len(keywords),8)),shorts(i),present_in(i),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(warn,'(a)')'UNNAMED'
         write(warn,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
   if(allocated(args))then
      if(size(args).gt.0)then
         write(warn,'(a)')'ARGS'
         write(warn,'(i6.6,3a)')(i,'[',args(i),']',i=1,size(args))
      endif
   endif
   if(G_remaining.ne.'')then
      write(warn,'(a)')'REMAINING'
      write(warn,'(a)')G_remaining
   endif
   if(present(stop))then
      if(stop) call mystop(5)
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
FUNCTION strtok(source_string,itoken,token_start,token_end,delimiters) result(strtok_status)
! JSU- 20151030

! ident_6="@(#)M_CLI2::strtok(3f): Tokenize a string"

character(len=*),intent(in)  :: source_string    ! Source string to tokenize.
character(len=*),intent(in)  :: delimiters       ! list of separator characters. May change between calls
integer,intent(inout)        :: itoken           ! token count since started
logical                      :: strtok_status    ! returned value
integer,intent(out)          :: token_start      ! beginning of token found if function result is .true.
integer,intent(inout)        :: token_end        ! end of token found if function result is .true.
integer                      :: isource_len
!----------------------------------------------------------------------------------------------------------------------------
!  calculate where token_start should start for this pass
   if(itoken.le.0)then                           ! this is assumed to be the first call
      token_start=1
   else                                          ! increment start to previous end + 1
      token_start=token_end+1
   endif
!----------------------------------------------------------------------------------------------------------------------------
   isource_len=len(source_string)                ! length of input string
!----------------------------------------------------------------------------------------------------------------------------
   if(token_start.gt.isource_len)then            ! user input error or at end of string
      token_end=isource_len                      ! assume end of token is end of string until proven otherwise so it is set
      strtok_status=.false.
      return
   endif
!----------------------------------------------------------------------------------------------------------------------------
   ! find beginning of token
   do while (token_start .le. isource_len)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_start:token_start)) .ne. 0) then
         token_start = token_start + 1
      else
         exit
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   token_end=token_start
   do while (token_end .le. isource_len-1)       ! step thru each character to find next delimiter, if any
      if(index(delimiters,source_string(token_end+1:token_end+1)) .ne. 0) then  ! found a delimiter in next character
         exit
      endif
      token_end = token_end + 1
   enddo
!----------------------------------------------------------------------------------------------------------------------------
   if (token_start .gt. isource_len) then        ! determine if finished
      strtok_status=.false.                      ! flag that input string has been completely processed
   else
      itoken=itoken+1                            ! increment count of tokens found
      strtok_status=.true.                       ! flag more tokens may remain
   endif
!----------------------------------------------------------------------------------------------------------------------------
end function strtok
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     get_args(3f) - [ARGUMENTS:M_CLI2] return keyword values when parsing command line arguments
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     use M_CLI2, only : get_args
!!     ! convenience functions
!!     use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!     use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!
!!     subroutine get_args(name,value,delimiters)
!!
!!      character(len=*),intent(in) :: name
!!
!!      character(len=:),allocatable :: value
!!      ! or
!!      character(len=:),allocatable :: value(:)
!!      ! or
!!      [real|doubleprecision|integer|logical|complex] :: value
!!      ! or
!!      [real|doubleprecision|integer|logical|complex],allocatable :: value(:)
!!
!!      character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    GET_ARGS(3f) returns the value of keywords after SET_ARGS(3f)
!!    has been called. For fixed-length CHARACTER variables
!!    see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
!!    GET_ARGS_FIXED_SIZE(3f).
!!
!!    As a convenience multiple pairs of keywords and variables may be
!!    specified if and only if all the values are scalars and the CHARACTER
!!    variables are fixed-length or pre-allocated.
!!
!!##OPTIONS
!!
!!     NAME        name of commandline argument to obtain the value of
!!     VALUE       variable to hold returned value. The kind of the value
!!                 is used to determine the type of returned value. May
!!                 be a scalar or allocatable array. If type is CHARACTER
!!                 the scalar must have an allocatable length.
!!     DELIMITERS  By default the delimiter for array values are comma,
!!                 colon, and whitespace. A string containing an alternate
!!                 list of delimiter characters may be supplied.
!!
!!##CONVENIENCE FUNCTIONS
!!
!!    There are convenience functions that are replacements for calls to
!!    get_args(3f) for each supported default intrinsic type
!!
!!      o scalars -- dget(3f), iget(3f), lget(3f), rget(3f), sget(3f),
!!                   cget(3f)
!!      o vectors -- dgets(3f), igets(3f), lgets(3f), rgets(3f),
!!                   sgets(3f), cgets(3f)
!!
!!    D is for DOUBLEPRECISION, I for INTEGER, L for LOGICAL, R for REAL,
!!    S for string (CHARACTER), and C for COMPLEX.
!!
!!    If the functions are called with no argument they will return the
!!    UNNAMED array converted to the specified type.
!!
!!##EXAMPLE
!!
!!
!! Sample program:
!!
!!     program demo_get_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     implicit none
!!     integer                      :: i
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real,allocatable             :: p(:)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o only quote strings and use double-quotes
!!     !   o set all logical values to F or T.
!!     call set_args(' &
!!        &-x 1 -y 2 -z 3 &
!!        &-p -1,-2,-3 &
!!        &--title "my title" &
!!        & -l F -L F  &
!!        & --label " " &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     ! SCALARS
!!     call get_args('x',x,'y',y,'z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     ! ALLOCATABLE STRING
!!     call get_args('title',title)
!!     ! NON-ALLOCATABLE ARRAYS
!!     call get_args('p',p)
!!     ! USE VALUES
!!     write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     if(size(filenames).gt.0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_get_args
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_length(3f) - [ARGUMENTS:M_CLI2] return keyword values for fixed-length string when parsing command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_length(name,value)
!!
!!     character(len=:),allocatable :: value
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    GET_ARGS_fixed_length(3f) returns the value of a string
!!    keyword when the string value is a fixed-length CHARACTER
!!    variable.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to obtain the value of
!!
!!    VALUE  variable to hold returned value.
!!           Must be a fixed-length CHARACTER variable.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_length
!!     use M_CLI2,  only : set_args, get_args_fixed_length
!!     implicit none
!!     ! DEFINE ARGS
!!     character(len=80)   :: title
!!     call set_args(' &
!!        & -title "my title" &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!        call get_args_fixed_length('title',title)
!!     ! USE VALUES
!!        write(*,*)'title=',title
!!     end program demo_get_args_fixed_length
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_size(3f) - [ARGUMENTS:M_CLI2] return keyword values for fixed-size array when parsing command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_size(name,value)
!!
!!     [real|doubleprecision|integer|logical|complex] :: value(NNN)
!!        or
!!     character(len=MMM) :: value(NNN)
!!
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    GET_ARGS_FIXED_SIZE(3f) returns the value of keywords for
!!    fixed-size arrays after SET_ARGS(3f) has been called.
!!    On input on the command line all values of the array must
!!    be specified.
!!
!!##OPTIONS
!!    NAME        name of commandline argument to obtain the value of
!!
!!    VALUE       variable to hold returned values. The kind of the value
!!                is used to determine the type of returned value. Must be
!!                a fixed-size array. If type is CHARACTER the length must
!!                also be fixed.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_size
!!     use M_CLI2,  only : set_args, get_args_fixed_size
!!     implicit none
!!     integer,parameter   :: dp=kind(0.0d0)
!!     ! DEFINE ARGS
!!     real                :: x(2)
!!     real(kind=dp)       :: y(2)
!!     integer             :: p(3)
!!     character(len=80)   :: title(1)
!!     logical             :: l(4), lbig(4)
!!     complex             :: cmp(2)
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o only quote strings
!!     !   o set all logical values to F or T.
!!     call set_args(' &
!!        & -x 10.0,20.0 &
!!        & -y 11.0,22.0 &
!!        & -p -1,-2,-3 &
!!        & -title "my title" &
!!        & -l F,T,F,T -L T,F,T,F  &
!!        & --cmp 111,222.0,333.0e0,4444 &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!        call get_args_fixed_size('x',x)
!!        call get_args_fixed_size('y',y)
!!        call get_args_fixed_size('p',p)
!!        call get_args_fixed_size('title',title)
!!        call get_args_fixed_size('l',l)
!!        call get_args_fixed_size('L',lbig)
!!        call get_args_fixed_size('cmp',cmp)
!!     ! USE VALUES
!!        write(*,*)'x=',x
!!        write(*,*)'p=',p
!!        write(*,*)'title=',title
!!        write(*,*)'l=',l
!!        write(*,*)'L=',lbig
!!        write(*,*)'cmp=',cmp
!!     end program demo_get_args_fixed_size
!!   Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine get_fixedarray_class(keyword,generic,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
class(*)                             :: generic(:)
character(len=*),intent(in),optional :: delimiters
   select type(generic)
    type is (character(len=*));  call get_fixedarray_fixed_length_c(keyword,generic,delimiters)
    type is (integer);           call get_fixedarray_i(keyword,generic,delimiters)
    type is (real);              call get_fixedarray_r(keyword,generic,delimiters)
    type is (complex);           call get_fixed_size_complex(keyword,generic,delimiters)
    type is (real(kind=dp));     call get_fixedarray_d(keyword,generic,delimiters)
    type is (logical);           call get_fixedarray_l(keyword,generic,delimiters)
    class default
      call mystop(-7,'*get_fixedarray_class* crud -- procedure does not know about this type')
   end select
end subroutine get_fixedarray_class
!===================================================================================================================================
! return allocatable arrays
!===================================================================================================================================
subroutine get_anyarray_l(keyword,larray,delimiters)

! ident_7="@(#)M_CLI2::get_anyarray_l(3f): given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
character(len=*),intent(in),optional   :: delimiters
character(len=:),allocatable :: carray(:)                  ! convert value to an array
character(len=:),allocatable :: val
integer                      :: i
integer                      :: place
integer                      :: iichar                     ! point to first character of word unless first character is "."
   call locate_key(keyword,place)                          ! find where string is or should be
   if(place.gt.0)then                                      ! if string was found
      val=values(place)(:counts(place))
      call split(adjustl(upper(val)),carray,delimiters=delimiters)  ! convert value to uppercase, trimmed; then parse into array
   else
      call journal('sc','*get_anyarray_l* unknown keyword '//keyword)
      call mystop(8 ,'*get_anyarray_l* unknown keyword '//keyword)
      if(allocated(larray))deallocate(larray)
      allocate(larray(0))
      return
   endif
   if(size(carray).gt.0)then                                  ! if not a null string
      if(allocated(larray))deallocate(larray)
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i)(1:1).eq.'.')then                        ! looking for fortran logical syntax .STRING.
            iichar=2
         else
            iichar=1
         endif
         select case(carray(i)(iichar:iichar))             ! check word to see if true or false
         case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
         case('F','N');     larray(i)=.false.              ! assume this is false or no
         case default
            call journal('sc',"*get_anyarray_l* bad logical expression for "//trim(keyword)//'='//carray(i))
         end select
      enddo
   else                                                       ! for a blank string return one T
      if(allocated(larray))deallocate(larray)
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.true.
   endif
end subroutine get_anyarray_l
!===================================================================================================================================
subroutine get_anyarray_d(keyword,darray,delimiters)

! ident_8="@(#)M_CLI2::get_anyarray_d(3f): given keyword fetch dble value array from Language Dictionary (0 on err)"

character(len=*),intent(in)           :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp),allocatable,intent(out) :: darray(:)    ! function type
character(len=*),intent(in),optional  :: delimiters

character(len=:),allocatable          :: carray(:)    ! convert value to an array using split(3f)
integer                               :: i
integer                               :: place
integer                               :: ierr
character(len=:),allocatable          :: val
!-----------------------------------------------------------------------------------------------------------------------------------
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place.gt.0)then                                 ! if string was found
      val=values(place)(:counts(place))
      val=replace_str(val,'(','')
      val=replace_str(val,')','')
      call split(val,carray,delimiters=delimiters)    ! find value associated with keyword and split it into an array
   else
      call journal('sc','*get_anyarray_d* unknown keyword '//keyword)
      call mystop(9 ,'*get_anyarray_d* unknown keyword '//keyword)
      if(allocated(darray))deallocate(darray)
      allocate(darray(0))
      return
   endif
   if(allocated(darray))deallocate(darray)
   allocate(darray(size(carray)))                     ! create the output array
   do i=1,size(carray)
      call a2d(carray(i), darray(i),ierr) ! convert the string to a numeric value
      if(ierr.ne.0)then
         call mystop(10 ,'*get_anyarray_d* unreadable value '//carray(i)//' for keyword '//keyword)
      endif
   enddo
end subroutine get_anyarray_d
!===================================================================================================================================
subroutine get_anyarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer,allocatable                  :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   iarray=nint(darray)
end subroutine get_anyarray_i
!===================================================================================================================================
subroutine get_anyarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real,allocatable                     :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   rarray=real(darray)
end subroutine get_anyarray_r
!===================================================================================================================================
subroutine get_anyarray_x(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex,allocatable                  :: xarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: half,sz,i
   call get_anyarray_d(keyword,darray,delimiters)
   sz=size(darray)
   half=sz/2
   if(sz.ne.half+half)then
      call journal('sc','*get_anyarray_x* uneven number of values defining complex value '//keyword)
      call mystop(11,'*get_anyarray_x* uneven number of values defining complex value '//keyword)
      if(allocated(xarray))deallocate(xarray)
      allocate(xarray(0))
   endif

   !x!================================================================================================
   !x!IFORT,GFORTRAN OK, NVIDIA RETURNS NULL ARRAY: xarray=cmplx(real(darray(1::2)),real(darray(2::2)))
   if(allocated(xarray))deallocate(xarray)
   allocate(xarray(half))
   do i=1,sz,2
      xarray((i+1)/2)=cmplx( darray(i),darray(i+1) )
   enddo
   !x!================================================================================================

end subroutine get_anyarray_x
!===================================================================================================================================
subroutine get_anyarray_c(keyword,strings,delimiters)

! ident_8="@(#)M_CLI2::get_anyarray_c(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=:),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
integer                              :: place
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings,delimiters=delimiters)   ! find value associated with keyword and split it into an array
   else
      call journal('sc','*get_anyarray_c* unknown keyword '//keyword)
      call mystop(12,'*get_anyarray_c* unknown keyword '//keyword)
      if(allocated(strings))deallocate(strings)
      allocate(character(len=0)::strings(0))
   endif
end subroutine get_anyarray_c
!===================================================================================================================================
!===================================================================================================================================
subroutine get_args_fixed_length_a_array(keyword,strings,delimiters)

! ident_9="@(#)M_CLI2::get_args_fixed_length_a_array(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=*),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: strings_a(:)
integer                              :: place
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings_a,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      if(len(strings_a).le.len(strings))then
         strings=strings_a
      else
         call journal('sc','*get_args_fixed_length_a_array* values too long. Longest is',len(strings_a),'allowed is',len(strings))
         write(*,'("strings=",3x,*(a,1x))')strings
         call journal('sc','*get_args_fixed_length_a_array* keyword='//keyword)
         call mystop(13,'*get_args_fixed_length_a_array* keyword='//keyword)
         strings=[character(len=len(strings)) ::]
      endif
   else
      call journal('sc','*get_args_fixed_length_a_array* unknown keyword '//keyword)
      call mystop(14,'*get_args_fixed_length_a_array* unknown keyword '//keyword)
      strings=[character(len=len(strings)) ::]
   endif
end subroutine get_args_fixed_length_a_array
!===================================================================================================================================
! return non-allocatable arrays
!===================================================================================================================================
subroutine get_fixedarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer                              :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(iarray,dim=1).eq.dsize)then
      iarray=nint(darray)
   else
      call journal('sc','*get_fixedarray_i* wrong number of values for keyword',keyword,'got',dsize,'expected',size(iarray))
      call print_dictionary('USAGE:')
      call mystop(33)
      iarray=0
   endif
end subroutine get_fixedarray_i
!===================================================================================================================================
subroutine get_fixedarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real                                 :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real,allocatable                     :: darray(:)    ! function type
integer                              :: dsize
   call get_anyarray_r(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(rarray,dim=1).eq.dsize)then
      rarray=darray
   else
      call journal('sc','*get_fixedarray_r* wrong number of values for keyword',keyword,'got',dsize,'expected',size(rarray))
      call print_dictionary('USAGE:')
      call mystop(33)
      rarray=0.0
   endif
end subroutine get_fixedarray_r
!===================================================================================================================================
subroutine get_fixed_size_complex(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex                              :: xarray(:)
character(len=*),intent(in),optional :: delimiters
complex,allocatable                  :: darray(:)    ! function type
integer                              :: half, sz
integer                              :: dsize
   call get_anyarray_x(keyword,darray,delimiters)
   dsize=size(darray)
   sz=dsize*2
   half=sz/2
   if(sz.ne.half+half)then
      call journal('sc','*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      call mystop(15,'*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      xarray=0
      return
   endif
   if(ubound(xarray,dim=1).eq.dsize)then
      xarray=darray
   else
      call journal('sc','*get_fixed_size_complex* wrong number of values for keyword',keyword,'got',dsize,'expected',size(xarray))
      call print_dictionary('USAGE:')
      call mystop(34)
      xarray=cmplx(0.0,0.0)
   endif
end subroutine get_fixed_size_complex
!===================================================================================================================================
subroutine get_fixedarray_d(keyword,darr,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                        :: darr(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(darr,dim=1).eq.dsize)then
      darr=darray
   else
      call journal('sc','*get_fixedarray_d* wrong number of values for keyword',keyword,'got',dsize,'expected',size(darr))
      call print_dictionary('USAGE:')
      call mystop(35)
      darr=0.0d0
   endif
end subroutine get_fixedarray_d
!===================================================================================================================================
subroutine get_fixedarray_l(keyword,larray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
logical                              :: larray(:)
character(len=*),intent(in),optional :: delimiters
logical,allocatable                  :: darray(:)    ! function type
integer                              :: dsize
   call get_anyarray_l(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(larray,dim=1).eq.dsize)then
      larray=darray
   else
      call journal('sc','*get_fixedarray_l* wrong number of values for keyword',keyword,'got',dsize,'expected',size(larray))
      call print_dictionary('USAGE:')
      call mystop(36)
      larray=.false.
   endif
end subroutine get_fixedarray_l
!===================================================================================================================================
subroutine get_fixedarray_fixed_length_c(keyword,strings,delimiters)

! ident_10="@(#)M_CLI2::get_fixedarray_fixed_length_c(3f): Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*)                     :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: str(:)
character(len=*),intent(in)          :: keyword   ! name to look up in dictionary
integer                              :: place
integer                              :: ssize
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                 ! find where string is or should be
   if(place > 0)then                              ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,str,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      ssize=size(str)
      if(ssize==size(strings))then
         strings(:ssize)=str
      else
         call journal('sc','*get_fixedarray_fixed_length_c* wrong number of values for keyword',&
            & keyword,'got',ssize,'expected ',size(strings)) !,ubound(strings,dim=1)
         call print_dictionary('USAGE:')
         call mystop(30,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
         strings=''
      endif
   else
      call journal('sc','*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      call mystop(16,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      strings=''
   endif
end subroutine get_fixedarray_fixed_length_c
!===================================================================================================================================
! return scalars
!===================================================================================================================================
subroutine get_scalar_d(keyword,d)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                 :: d
real(kind=dp),allocatable     :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray)
   if(size(darray).eq.1)then
      d=darray(1)
   else
      call journal('sc','*get_anyarray_d* incorrect number of values for keyword',keyword,'expected one found',size(darray))
      call print_dictionary('USAGE:')
      call mystop(31,'*get_anyarray_d* incorrect number of values for keyword'//keyword//'expected one')
   endif
end subroutine get_scalar_d
!===================================================================================================================================
subroutine get_scalar_real(keyword,r)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real,intent(out)              :: r
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   r=real(d)
end subroutine get_scalar_real
!===================================================================================================================================
subroutine get_scalar_i(keyword,i)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
integer,intent(out)           :: i
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   i=nint(d)
end subroutine get_scalar_i
!===================================================================================================================================
subroutine get_scalar_anylength_c(keyword,string)

! ident_11="@(#)M_CLI2::get_scalar_anylength_c(3f): Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=:),allocatable,intent(out)  :: string
integer                       :: place
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(17,'*get_anyarray_c* unknown keyword '//keyword)
      call journal('sc','*get_anyarray_c* unknown keyword '//keyword)
      string=''
   endif
end subroutine get_scalar_anylength_c
!===================================================================================================================================
elemental impure subroutine get_args_fixed_length_scalar_c(keyword,string)

! ident_12="@(#)M_CLI2::get_args_fixed_length_scalar_c(3f): Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=*),intent(out)  :: string
integer                       :: place
integer                       :: unlen
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return string
      string=unquote(values(place)(:counts(place)))
   else
      call mystop(18,'*get_args_fixed_length_scalar_c* unknown keyword '//keyword)
      string=''
   endif
   unlen=len_trim(unquote(values(place)(:counts(place))))
   if(unlen>len(string))then
      call journal('sc','*get_args_fixed_length_scalar_c* value too long for',keyword,'allowed is',len(string),&
      & 'input string [',values(place),'] is',unlen)
      call mystop(19,'*get_args_fixed_length_scalar_c* value too long')
      string=''
   endif
end subroutine get_args_fixed_length_scalar_c
!===================================================================================================================================
subroutine get_scalar_complex(keyword,x)
character(len=*),intent(in) :: keyword      ! keyword to retrieve value from dictionary
complex,intent(out)         :: x
real(kind=dp)               :: d(2)
   call get_fixedarray_d(keyword,d)
   if(size(d).eq.2)then
      x=cmplx(d(1),d(2),kind=sp)
   else
      call journal('sc','*get_scalar_complex* expected two values found',size(d))
      call mystop(20,'*get_scalar_complex* incorrect number of values for keyword '//keyword)
      x=cmplx(0.0,0.0)
   endif
end subroutine get_scalar_complex
!===================================================================================================================================
subroutine get_scalar_logical(keyword,l)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
logical                       :: l
logical,allocatable           :: larray(:)    ! function type
   call get_anyarray_l(keyword,larray)
   if(size(larray).eq.1)then
      l=larray(1)
   else
      call journal('sc','*get_anyarray_l* expected one value found',size(larray))
      call mystop(21,'*get_anyarray_l* incorrect number of values for keyword '//keyword)
      l=.false.
   endif
end subroutine get_scalar_logical
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! THE REMAINDER SHOULD BE ROUTINES EXTRACTED FROM OTHER MODULES TO MAKE THIS MODULE STANDALONE BY POPULAR REQUEST
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!use M_strings,                     only : UPPER, LOWER, QUOTE, REPLACE_STR=>REPLACE, UNQUOTE, SPLIT, STRING_TO_VALUE
!use M_list,                        only : insert, locate, remove, replace
!use M_journal,                     only : JOURNAL

!use M_args,                        only : LONGEST_COMMAND_ARGUMENT
! routines extracted from other modules
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_command_argument(3f) - [ARGUMENTS:M_args] length of longest argument on command line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function longest_command_argument() result(ilongest)
!!
!!     integer :: ilongest
!!
!!##DESCRIPTION
!!    length of longest argument on command line. Useful when allocating storage for holding arguments.
!!##RESULT
!!    longest_command_argument  length of longest command argument
!!##EXAMPLE
!!
!! Sample program
!!
!!      program demo_longest_command_argument
!!      use M_args, only : longest_command_argument
!!         write(*,*)'longest argument is ',longest_command_argument()
!!      end program demo_longest_command_argument
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
function longest_command_argument() result(ilongest)
integer :: i
integer :: ilength
integer :: istatus
integer :: ilongest
   ilength=0
   ilongest=0
   GET_LONGEST: do i=1,command_argument_count()                             ! loop throughout command line arguments to find longest
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining length for argument ',i
         exit GET_LONGEST
      elseif(ilength.gt.0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine journal(where, g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
implicit none

! ident_13="@(#)M_CLI2::journal(3f): writes a message to a string composed of any standard scalar types"

character(len=*),intent(in)   :: where
class(*),intent(in)           :: g0
class(*),intent(in),optional  :: g1, g2, g3, g4, g5, g6, g7, g8 ,g9
class(*),intent(in),optional  :: ga, gb, gc, gd, ge, gf, gg, gh ,gi, gj
character(len=*),intent(in),optional :: sep
if(debug_m_cli2)write(*,*)'<DEBUG>JOURNAL:',present(g1)
if(debug_m_cli2)write(*,*)'<DEBUG>JOURNAL:',present(g2)
if(debug_m_cli2)write(*,*)'<DEBUG>JOURNAL:',present(sep)
write(*,'(a)')str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep)
end subroutine journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    str(3f) - [M_CLI2] converts any standard scalar type to a string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function str(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!     character,len=(:),allocatable :: str
!!
!!##DESCRIPTION
!!    str(3f) builds a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!                Optionally, all the generic values can be
!!                single-dimensioned arrays. Currently, mixing scalar
!!                arguments and array arguments is not supported.
!!
!!    sep         separator to place between values. Defaults to a space.
!!##RETURNS
!!    str     description to print
!!##EXAMPLES
!!
!! Sample program:
!!
!!       program demo_msg
!!       use M_CLI2, only : str
!!       implicit none
!!       character(len=:),allocatable :: pr
!!       character(len=:),allocatable :: frmt
!!       integer                      :: biggest
!!
!!       pr=str('HUGE(3f) integers',huge(0),'and real',huge(0.0),'and double',huge(0.0d0))
!!       write(*,'(a)')pr
!!       pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!       write(*,'(a)')pr
!!       pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!       write(*,'(a)')pr
!!       pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!       write(*,'(a)')pr
!!
!!       ! create a format on the fly
!!       biggest=huge(0)
!!       frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep=' ')
!!       write(*,*)'format=',frmt
!!
!!       ! although it will often work, using str(3f) in an I/O statement is not recommended
!!       ! because if an error occurs str(3f) will try to write while part of an I/O statement
!!       ! which not all compilers can handle and is currently non-standard
!!       write(*,*)str('program will now stop')
!!
!!       end program demo_msg
!!
!!  Output
!!
!!     HUGE(3f) integers 2147483647 and real 3.40282347E+38 and double 1.7976931348623157E+308
!!     real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!     doubleprecision : 1.7976931348623157E+308 0.0000000000000000 12345.678900000001 2.2250738585072014E-308
!!     complex         : (3.40282347E+38,1.17549435E-38)
!!      format=(*(i9:,1x))
!!      program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function msg_scalar(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_14="@(#)M_CLI2::msg_scalar(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_scalar
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR'
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR'

   istart=1
   line=''
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR:CALL GENERIC:GENERIC0'
   if(present(generic0))call print_generic(generic0)
   if(debug_m_cli2)write(*,gen)'<DEBUG>:MSG_SCALAR:CALL GENERIC:GENERIC1'
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   msg_scalar=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:START'
   if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:LINE',trim(line)
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64))
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:REAL64'
         write(line(istart:),'(1pg0)') generic
      !x! DOES NOT WORK WITH NVFORTRAN: type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical)
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:REAL64'
         write(line(istart:),'(l1)') generic
      type is (character(len=*))
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:CHARACTER'
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:ISTART:',istart
         write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:START'
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function msg_one(generic0,generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9,sep)
implicit none

! ident_15="@(#)M_CLI2::msg_one(3fp): writes a message to a string composed of any standard one dimensional types"

class(*),intent(in)           :: generic0(:)
class(*),intent(in),optional  :: generic1(:), generic2(:), generic3(:), generic4(:), generic5(:)
class(*),intent(in),optional  :: generic6(:), generic7(:), generic8(:), generic9(:)
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
character(len=:), allocatable :: msg_one
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   line=' '
   call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   msg_one=trim(line)
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in),optional :: generic(:)
integer :: i
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int16));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int32));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (integer(kind=int64));    write(line(istart:),'("[",*(i0,1x))') generic
      type is (real(kind=real32));      write(line(istart:),'("[",*(1pg0,1x))') generic
      type is (real(kind=real64));      write(line(istart:),'("[",*(1pg0,1x))') generic
      !x! DOES NOT WORK WITH nvfortran: type is (real(kind=real128));     write(line(istart:),'("[",*(1pg0,1x))') generic
      !x! DOES NOT WORK WITH ifort:     type is (real(kind=real256));     write(error_unit,'(1pg0)',advance='no') generic
      type is (logical);                write(line(istart:),'("[",*(l1,1x))') generic
      type is (character(len=*))
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:CHARACTER'
         if(debug_m_cli2)write(*,gen)'<DEBUG>PRINT_GENERIC:ISTART:',istart
         write(line(istart:),'("[",:*("""",a,"""",1x))') (trim(generic(i)),i=1,size(generic))
      type is (complex);                write(line(istart:),'("[",*("(",1pg0,",",1pg0,")",1x))') generic
      class default
         call mystop(-22,'unknown type in *print_generic*')
   end select
   istart=len_trim(line)+increment+1
   line=trim(line)//"]"//sep_local
end subroutine print_generic
!===================================================================================================================================
end function msg_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function upper(str) result (string)

! ident_16="@(#)M_CLI2::upper(3f): Changes a string to uppercase"

character(*), intent(in)      :: str
character(:),allocatable      :: string
integer                       :: i
   string = str
   do i = 1, len_trim(str)
       select case (str(i:i))
       case ('a':'z')
          string(i:i) = char(iachar(str(i:i))-32)
       end select
   end do
end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lower(str) result (string)

! ident_17="@(#)M_CLI2::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(:),allocatable     :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2i(chars,valu,ierr)

! ident_18="@(#)M_CLI2::a2i(3fp): subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
   valu8=0.0d0
   call a2d(chars,valu8,ierr,onerr=0.0d0)
   if(valu8.le.huge(valu))then
      if(valu8.le.huge(valu))then
         valu=int(valu8)
      else
         call journal('sc','*a2i*','- value too large',valu8,'>',huge(valu))
         valu=huge(valu)
         ierr=-1
      endif
   endif
end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_19="@(#)M_CLI2::a2d(3fp): subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   call substitute(local_chars,',','')                          ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd.ne.0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         frmt='(Z'//i2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         frmt='(B'//i2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         frmt='(O'//i2s(len(local_chars))//')'
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr.ne.0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      if(local_chars.ne.'eod')then                           ! print warning message except for special value "eod"
         call journal('sc','*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg.ne.'')then
            call journal('sc','*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3f) - [M_CLI2:TOKENS] parse string into an array using specified delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!##DESCRIPTION
!!    SPLIT(3f) parses a string using specified delimiter characters and
!!    store tokens into an allocatable array
!!
!!##OPTIONS
!!
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!
!!    NULLS IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_split
!!     use M_CLI2, only: split
!!     character(len=*),parameter     :: &
!!     & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!     character(len=:),allocatable :: array(:) ! output array of tokens
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,'(80("="))')
!!        write(*,*)'typical call:'
!!        CALL split(line,array)
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'custom list of delimiters (colon and vertical line):'
!!        CALL split(line,array,delimiters=':|',order='sequential',nulls='ignore')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)&
!!      &'custom list of delimiters, reverse array order and count null fields:'
!!        CALL split(line,array,delimiters=':|',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,*)&
!!        &'default delimiters and reverse array order and return null fields:'
!!        CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!     end program demo_split
!!
!!   Output
!!
!!    > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    > ===========================================================================
!!    >  typical call:
!!    > 1 ==> aBcdef
!!    > 2 ==> ghijklmnop
!!    > 3 ==> qrstuvwxyz
!!    > 4 ==> 1:|:2
!!    > 5 ==> 333|333
!!    > 6 ==> a
!!    > 7 ==> B
!!    > 8 ==> cc
!!    >  SIZE:           8
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters (colon and vertical line):
!!    > 1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    > 2 ==> 2     333
!!    > 3 ==> 333 a B cc
!!    >  SIZE:           3
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters, reverse array order and return null fields:
!!    > 1 ==> 333 a B cc
!!    > 2 ==> 2     333
!!    > 3 ==>
!!    > 4 ==>
!!    > 5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    >  SIZE:           5
!!    > --------------------------------------------------------------------------
!!    >  INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    >  default delimiters and reverse array order and count null fields:
!!    > 1 ==>
!!    > 2 ==>
!!    > 3 ==>
!!    > 4 ==> cc
!!    > 5 ==> B
!!    > 6 ==> a
!!    > 7 ==> 333|333
!!    > 8 ==>
!!    > 9 ==>
!!    > 10 ==>
!!    > 11 ==>
!!    > 12 ==> 1:|:2
!!    > 13 ==>
!!    > 14 ==> qrstuvwxyz
!!    > 15 ==> ghijklmnop
!!    > 16 ==>
!!    > 17 ==>
!!    > 18 ==> aBcdef
!!    > 19 ==>
!!    > 20 ==>
!!    >  SIZE:          20
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine split(input_line,array,delimiters,order,nulls)
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_20="@(#)M_CLI2::split(3f): parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
intrinsic index, min, present, len
!-----------------------------------------------------------------------------------------------------------------------------------
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: iilen                  ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters.ne.'')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:' ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:'    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ???
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ???
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   iilen=len(input_line)                                          ! IILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
   if(iilen.gt.0)then                                             ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,iilen,1                                  ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(i30)=iilen                                      ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):iilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol.gt.iilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(nlls)))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   if(allocated(array))deallocate(array)
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                       ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (trim(adjustl(ordr)))                              ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20).lt.ibegin(i20))then
         select case (trim(adjustl(nlls)))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace_str(3f) - [M_CLI2:EDITING] function globally replaces one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function replace_str(targetline[,old,new|cmd],range,ierr) result (newline)
!!
!!     character(len=*)                       :: targetline
!!     character(len=*),intent(in),optional   :: old
!!     character(len=*),intent(in),optional   :: new
!!     character(len=*),intent(in),optional   :: cmd
!!     integer,intent(in),optional            :: range(2)
!!     integer,intent(out),optional           :: ierr
!!     logical,intent(in),optional            :: clip
!!     character(len=:),allocatable           :: newline
!!##DESCRIPTION
!!    Globally replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     cmd         alternate way to specify old and new string, in
!!                 the form c/old/new/; where "/" can be any character
!!                 not in "old" or "new"
!!     range       if present, only change range(1) to range(2) of occurrences of old string
!!     ierr        error code. iF ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!     clip        whether to return trailing spaces or not. Defaults to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!
!!##EXAMPLES
!!
!! Sample Program:
!!
!!       program demo_replace_str
!!       use M_CLI2, only : replace_str
!!       implicit none
!!       character(len=:),allocatable :: targetline
!!
!!       targetline='this is the input string'
!!
!!       call testit('th','TH','THis is THe input string')
!!
!!       ! a null old substring means "at beginning of line"
!!       call testit('','BEFORE:', 'BEFORE:THis is THe input string')
!!
!!       ! a null new string deletes occurrences of the old substring
!!       call testit('i','', 'BEFORE:THs s THe nput strng')
!!
!!       write(*,*)'Examples of the use of RANGE='
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A')
!!       write(*,*)'replace a with A ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A',range=[3,5])
!!       write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','',range=[3,5])
!!       write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa aa aa a a a aa aaaaaa','aa','CCCC',range=[3,5])
!!       write(*,*)'replace aa with CCCC instances 3 to 5 ['//targetline//']'
!!
!!       contains
!!       subroutine testit(old,new,expected)
!!       character(len=*),intent(in) :: old,new,expected
!!       write(*,*)repeat('=',79)
!!       write(*,*)':STARTED ['//targetline//']'
!!       write(*,*)':OLD['//old//']', ' NEW['//new//']'
!!       targetline=replace_str(targetline,old,new)
!!       write(*,*)':GOT     ['//targetline//']'
!!       write(*,*)':EXPECTED['//expected//']'
!!       write(*,*)':TEST    [',targetline.eq.expected,']'
!!       end subroutine testit
!!
!!       end program demo_replace_str
!!
!!   Expected output
!!
!!     ===============================================================================
!!     STARTED [this is the input string]
!!     OLD[th] NEW[TH]
!!     GOT     [THis is THe input string]
!!     EXPECTED[THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [THis is THe input string]
!!     OLD[] NEW[BEFORE:]
!!     GOT     [BEFORE:THis is THe input string]
!!     EXPECTED[BEFORE:THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [BEFORE:THis is THe input string]
!!     OLD[i] NEW[]
!!     GOT     [BEFORE:THs s THe nput strng]
!!     EXPECTED[BEFORE:THs s THe nput strng]
!!     TEST    [ T ]
!!     Examples of the use of RANGE=
!!     replace a with A [A b Ab bAAA AAAA]
!!     replace a with A instances 3 to 5 [a b ab bAAA aaaa]
!!     replace a with null instances 3 to 5 [a b ab b aaaa]
!!     replace aa with CCCC instances 3 to 5 [a b ab baaa aaCCCC CCCC CCCC a a a aa aaaaaa]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine crack_cmd(cmd,old,new,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)              :: cmd
character(len=:),allocatable,intent(out) :: old,new                ! scratch string buffers
integer                                  :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=1)                         :: delimiters
integer                                  :: itoken
integer,parameter                        :: id=2                   ! expected location of delimiter
logical                                  :: ifok
integer                                  :: lmax                   ! length of target string
integer                                  :: start_token,end_token
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   old=''
   new=''
   lmax=len_trim(cmd)                       ! significant length of change directive

   if(lmax.ge.4)then                      ! strtok ignores blank tokens so look for special case where first token is really null
      delimiters=cmd(id:id)               ! find delimiter in expected location
      itoken=0                            ! initialize strtok(3f) procedure

      if(strtok(cmd(id:),itoken,start_token,end_token,delimiters)) then        ! find OLD string
         old=cmd(start_token+id-1:end_token+id-1)
      else
         old=''
      endif

      if(cmd(id:id).eq.cmd(id+1:id+1))then
         new=old
         old=''
      else                                                                     ! normal case
         ifok=strtok(cmd(id:),itoken,start_token,end_token,delimiters)         ! find NEW string
         if(end_token .eq. (len(cmd)-id+1) )end_token=len_trim(cmd(id:))       ! if missing ending delimiter
         new=cmd(start_token+id-1:min(end_token+id-1,lmax))
      endif
   else                                                                        ! command was two or less characters
      ierr=-1
      call journal('sc','*crack_cmd* incorrect change directive -too short')
   endif

end subroutine crack_cmd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function replace_str(targetline,old,new,ierr,cmd,range) result (newline)

! ident_21="@(#)M_CLI2::replace_str(3f): Globally replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in),optional   :: old          ! old substring to replace
character(len=*),intent(in),optional   :: new          ! new substring
integer,intent(out),optional           :: ierr         ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
character(len=*),intent(in),optional   :: cmd          ! contains the instructions changing the string
integer,intent(in),optional            :: range(2)     ! start and end of which changes to make
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
character(len=:),allocatable  :: new_local, old_local
integer                       :: icount,ichange,ier2
integer                       :: original_input_length
integer                       :: len_old, len_new
integer                       :: ladd
integer                       :: left_margin, right_margin
integer                       :: ind
integer                       :: ic
integer                       :: iichar
integer                       :: range_local(2)
!-----------------------------------------------------------------------------------------------------------------------------------
!  get old_local and new_local from cmd or old and new
   if(present(cmd))then
      call crack_cmd(cmd,old_local,new_local,ier2)
      if(ier2.ne.0)then
         newline=targetline  ! if no changes are made return original string on error
         if(present(ierr))ierr=ier2
         return
      endif
   elseif(present(old).and.present(new))then
      old_local=old
      new_local=new
   else
      newline=targetline  ! if no changes are made return original string on error
      call journal('sc','*replace_str* must specify OLD and NEW or CMD')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   len_old=len(old_local)                              ! length of old substring to be replaced
   len_new=len(new_local)                              ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(range))then
      range_local=range
   else
      range_local=[1,original_input_length]
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(len_new.gt.0)then
         newline=new_local(:len_new)//targetline(left_margin:original_input_length)
      else
         newline=targetline(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iichar=left_margin                                  ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old_local(:len_old))+ic-1 ! try finding start of OLD in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.right_margin)then          ! did not find old string or found old string past edit window
         exit loop                                        ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:iichar-1)//targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(icount.ge.range_local(1).and.icount.le.range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new.ne.0)then                                          ! put in new string
            newline=newline(:iichar-1)//new_local(:len_new)
            iichar=iichar+len_new
         endif
      else
         if(len_old.ne.0)then                                          ! put in copy of old string
            newline=newline(:iichar-1)//old_local(:len_old)
            iichar=iichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline                               ! if no changes made output should be input
   case default
      if(ic.le.len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:iichar-1)//targetline(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     quote(3f) - [M_CLI2:QUOTES] add quotes to string as if written with list-directed input
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str         input string to add quotes to, using the rules of
!!                list-directed input (single quotes are replaced by two adjacent quotes)
!!    mode        alternate quoting methods are supported:
!!
!!                   DOUBLE   default. replace quote with double quotes
!!                   ESCAPE   replace quotes with backslash-quote instead of double quotes
!!
!!    clip        default is to trim leading and trailing spaces from the string. If CLIP
!!                is .FALSE. spaces are not trimmed
!!
!!##RESULT
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!##EXAMPLE
!!
!! Sample program:
!!
!!     program demo_quote
!!     use M_CLI2, only : quote
!!     implicit none
!!     character(len=:),allocatable :: str
!!     character(len=1024)          :: msg
!!     integer                      :: ios
!!     character(len=80)            :: inline
!!        do
!!           write(*,'(a)',advance='no')'Enter test string:'
!!           read(*,'(a)',iostat=ios,iomsg=msg)inline
!!           if(ios.ne.0)then
!!              write(*,*)trim(inline)
!!              exit
!!           endif
!!
!!           ! the original string
!!           write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!           ! the string processed by quote(3f)
!!           str=quote(inline)
!!           write(*,'(a)')'QUOTED     ['//str//']'
!!
!!           ! write the string list-directed to compare the results
!!           write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!           write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!        enddo
!!     end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
logical                              :: clip_local
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode
!-----------------------------------------------------------------------------------------------------------------------------------
   local_mode=merge_str(mode,'DOUBLE',present(mode))
   if(present(clip))then
      clip_local=clip
   else
      clip_local=.false.
   endif
   if(clip_local)then
      quoted_str=adjustl(str)
   else
      quoted_str=str
   endif
   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','\"'))//double_quote
   case default
      call journal('sc','*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     unquote(3f) - [M_CLI2:QUOTES] remove quotes from string as if read with list-directed input
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!   pure function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3f) does.
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!##RESULT
!!    unquoted_str  The output string, which is based on removing quotes from quoted_str.
!!##EXAMPLE
!!
!! Sample program:
!!
!!       program demo_unquote
!!       use M_CLI2, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc='\'
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3f)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios.ne.0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!       end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   if(allocated(unquoted_str))deallocate(unquoted_str)
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen.ge.1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1).eq.single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before.eq.iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current.eq.quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before.eq.quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before.ne.iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function i2s(ivalue,fmt) result(outstr)

! ident_22="@(#)M_CLI2::i2s(3fp): private function returns string given integer value"

integer,intent(in)           :: ivalue                         ! input value to convert to a string
character(len=*),intent(in),optional :: fmt
character(len=:),allocatable :: outstr                         ! output string to generate
character(len=80)            :: string
   if(present(fmt))then
      call value_to_string(ivalue,string,fmt=fmt)
   else
      call value_to_string(ivalue,string)
   endif
   outstr=trim(string)
end function i2s
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    merge_str(3f) - [M_CLI2:LENGTH] pads strings to same length and then calls MERGE(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function merge_str(str1,str2,expr) result(strout)
!!
!!     character(len=*),intent(in),optional :: str1
!!     character(len=*),intent(in),optional :: str2
!!     logical,intent(in)              :: expr
!!     character(len=:),allocatable    :: strout
!!##DESCRIPTION
!!    merge_str(3f) pads the shorter of str1 and str2 to the longest length
!!    of str1 and str2 and then calls MERGE(padded_str1,padded_str2,expr).
!!    It trims trailing spaces off the result and returns the trimmed
!!    string. This makes it easier to call MERGE(3f) with strings, as
!!    MERGE(3f) requires the strings to be the same length.
!!
!!    NOTE: STR1 and STR2 are always required even though declared optional.
!!          this is so the call "STR_MERGE(A,B,present(A))" is a valid call.
!!          The parameters STR1 and STR2 when they are optional parameters
!!          can be passed to a procedure if the options are optional on the
!!          called procedure.
!!
!!##OPTIONS
!!    STR1    string to return if the logical expression EXPR is true
!!    STR2    string to return if the logical expression EXPR is false
!!    EXPR    logical expression to evaluate to determine whether to return
!!            STR1 when true, and STR2 when false.
!!##RESULT
!!     MERGE_STR  a trimmed string is returned that is otherwise the value
!!                of STR1 or STR2, depending on the logical expression EXPR.
!!
!!##EXAMPLES
!!
!! Sample Program:
!!
!!     program demo_merge_str
!!     use M_CLI2, only : merge_str
!!     implicit none
!!     character(len=:), allocatable :: answer
!!        answer=merge_str('first string', 'second string is longer',10.eq.10)
!!        write(*,'("[",a,"]")') answer
!!        answer=merge_str('first string', 'second string is longer',10.ne.10)
!!        write(*,'("[",a,"]")') answer
!!     end program demo_merge_str
!!
!!   Expected output
!!
!!     [first string]
!!     [second string is longer]
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function merge_str(str1,str2,expr) result(strout)
! for some reason the MERGE(3f) intrinsic requires the strings it compares to be of equal length
! make an alias for MERGE(3f) that makes the lengths the same before doing the comparison by padding the shorter one with spaces

! ident_23="@(#)M_CLI2::merge_str(3f): pads first and second arguments to MERGE(3f) to same length"

character(len=*),intent(in),optional :: str1
character(len=*),intent(in),optional :: str2
character(len=:),allocatable         :: str1_local
character(len=:),allocatable         :: str2_local
logical,intent(in)                   :: expr
character(len=:),allocatable         :: strout
integer                              :: big
   if(present(str2))then
      str2_local=str2
   else
      str2_local=''
   endif
   if(present(str1))then
      str1_local=str1
   else
      str1_local=''
   endif
   big=max(len(str1_local),len(str2_local))
   ! note: perhaps it would be better to warn or fail if an optional value that is not present is returned, instead of returning ''
   strout=trim(merge(lenset(str1_local,big),lenset(str2_local,big),expr))
end function merge_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3f) - [M_CLI2:BASE] convert whole number string in base [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_decodebase
!!      use M_CLI2, only : codebase, decodebase
!!      implicit none
!!      integer           :: ba,bd
!!      character(len=40) :: x,y
!!      integer           :: r
!!
!!      print *,' BASE CONVERSION'
!!      write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!      write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!      INFINITE: do
!!         print *,''
!!         write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!         if(x.eq.'0') exit INFINITE
!!         if(decodebase(x,bd,r)) then
!!            if(codebase(r,ba,y)) then
!!              write(*,'("In base ",I2,": ",A20)')  ba, y
!!            else
!!              print *,'Error in coding number.'
!!            endif
!!         else
!!            print *,'Error in decoding number.'
!!         endif
!!      enddo INFINITE
!!
!!      end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)
implicit none

! ident_24="@(#)M_CLI2::decodebase(3f): convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(trim(adjustl(string)))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein.eq.0.and.ipound.gt.1)then                                  ! split string into two values
     call a2i(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local.ge.0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        if(ch.eq.'-'.and.k.eq.1)then
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lenset(3f) - [M_CLI2:LENGTH] return string trimmed or padded to specified length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lenset(str,length) result(strout)
!!
!!     character(len=*)                     :: str
!!     character(len=length)                :: strout
!!     integer,intent(in)                   :: length
!!##DESCRIPTION
!!    lenset(3f) truncates a string or pads it with spaces to the specified
!!    length.
!!##OPTIONS
!!    str     input string
!!    length  output string length
!!##RESULTS
!!    strout  output string
!!##EXAMPLE
!!
!! Sample Program:
!!
!!     program demo_lenset
!!      use M_CLI2, only : lenset
!!      implicit none
!!      character(len=10)            :: string='abcdefghij'
!!      character(len=:),allocatable :: answer
!!         answer=lenset(string,5)
!!         write(*,'("[",a,"]")') answer
!!         answer=lenset(string,20)
!!         write(*,'("[",a,"]")') answer
!!     end program demo_lenset
!!
!!    Expected output:
!!
!!     [abcde]
!!     [abcdefghij          ]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function lenset(line,length) result(strout)

! ident_25="@(#)M_CLI2::lenset(3f): return string trimmed or padded to specified length"

character(len=*),intent(in)  ::  line
integer,intent(in)           ::  length
character(len=length)        ::  strout
   strout=line
end function lenset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      value_to_string(3f) - [M_CLI2:NUMERIC] return numeric string from a numeric value
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine value_to_string(value,chars[,iilen,ierr,fmt,trimz])
!!
!!     character(len=*) :: chars  ! minimum of 23 characters required
!!     !--------
!!     ! VALUE may be any <em>one</em> of the following types:
!!     doubleprecision,intent(in)               :: value
!!     real,intent(in)                          :: value
!!     integer,intent(in)                       :: value
!!     logical,intent(in)                       :: value
!!     !--------
!!     character(len=*),intent(out)             :: chars
!!     integer,intent(out),optional             :: iilen
!!     integer,optional                         :: ierr
!!     character(len=*),intent(in),optional     :: fmt
!!     logical,intent(in)                       :: trimz
!!
!!##DESCRIPTION
!!    value_to_string(3f) returns a numeric representation of a numeric
!!    value in a string given a numeric value of type REAL, DOUBLEPRECISION,
!!    INTEGER or LOGICAL. It creates the string using internal writes. It
!!    then removes trailing zeros from non-zero values, and left-justifies
!!    the string.
!!
!!##OPTIONS
!!       VALUE   input value to be converted to a string
!!       FMT     You may specify a specific format that produces a string
!!               up to the length of CHARS; optional.
!!       TRIMZ   If a format is supplied the default is not to try to trim
!!               trailing zeros. Set TRIMZ to .true. to trim zeros from a
!!               string assumed to represent a simple numeric value.
!!
!!##RETURNS
!!       CHARS   returned string representing input value, must be at least
!!               23 characters long; or what is required by optional FMT if longer.
!!       IILEN   position of last non-blank character in returned string; optional.
!!       IERR    If not zero, error occurred; optional.
!!##EXAMPLE
!!
!! Sample program:
!!
!!      program demo_value_to_string
!!      use M_CLI2, only: value_to_string
!!      implicit none
!!      character(len=80) :: string
!!      integer           :: iilen
!!         call value_to_string(3.0/4.0,string,iilen)
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(3.0/4.0,string,iilen,fmt='')
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(3.0/4.0,string,iilen,fmt='("THE VALUE IS ",g0)')
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(1234,string,iilen)
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!         call value_to_string(1.0d0/3.0d0,string,iilen)
!!         write(*,*) 'The value is [',string(:iilen),']'
!!
!!      end program demo_value_to_string
!!
!!    Expected output
!!
!!     The value is [0.75]
!!     The value is [      0.7500000000]
!!     The value is [THE VALUE IS .750000000]
!!     The value is [1234]
!!     The value is [0.33333333333333331]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine value_to_string(gval,chars,length,err,fmt,trimz)

! ident_26="@(#)M_CLI2::value_to_string(3fp): subroutine returns a string from a value"

class(*),intent(in)                      :: gval
character(len=*),intent(out)             :: chars
integer,intent(out),optional             :: length
integer,optional                         :: err
integer                                  :: err_local
character(len=*),optional,intent(in)     :: fmt         ! format to write value with
logical,intent(in),optional              :: trimz
character(len=:),allocatable             :: fmt_local
character(len=1024)                      :: msg

!  Notice that the value GVAL can be any of several types ( INTEGER,REAL,DOUBLEPRECISION,LOGICAL)

   if (present(fmt)) then
      select type(gval)
      type is (integer)
         fmt_local='(i0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (real)
         fmt_local='(bz,g23.10e3)'
         fmt_local='(bz,g0.8)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         fmt_local='(bz,g0)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      type is (logical)
         fmt_local='(l1)'
         if(fmt.ne.'') fmt_local=fmt
         write(chars,fmt_local,iostat=err_local,iomsg=msg)gval
      class default
         call journal('sc','*value_to_string* UNKNOWN TYPE')
         chars=' '
      end select
      if(fmt.eq.'') then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   else                                                  ! no explicit format option present
      err_local=-1
      select type(gval)
      type is (integer)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (real)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (doubleprecision)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      type is (logical)
         write(chars,*,iostat=err_local,iomsg=msg)gval
      class default
         chars=''
      end select
      chars=adjustl(chars)
      if(index(chars,'.').ne.0) call trimzeros_(chars)
   endif
   if(present(trimz))then
      if(trimz)then
         chars=adjustl(chars)
         call trimzeros_(chars)
      endif
   endif

   if(present(length)) then
      length=len_trim(chars)
   endif

   if(present(err)) then
      err=err_local
   elseif(err_local.ne.0)then
      !-! cannot currently do I/O from a function being called from I/O
      !-!write(ERROR_UNIT,'(a)')'*value_to_string* WARNING:['//trim(msg)//']'
      chars=chars//' *value_to_string* WARNING:['//trim(msg)//']'
   endif

end subroutine value_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_CLI2:NUMERIC] Delete trailing zeros from numeric decimal string
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have trailing
!!          zeros removed
!!##EXAMPLES
!!
!! Sample program:
!!
!!       program demo_trimzeros_
!!       use M_CLI2, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          write(*,*)trimzeros_('123.450000000000')
!!          write(*,*)trimzeros_('12345')
!!          write(*,*)trimzeros_('12345.')
!!          write(*,*)trimzeros_('12345.00e3')
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_27="@(#)M_CLI2::trimzeros_(3fp): Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)             :: string
character(len=len(string)+2) :: str
character(len=len(string))   :: expo         ! the exponent string if present
integer                      :: ipos         ! where exponent letter appears if present
integer                      :: i, ii
   str=string                                ! working copy of string
   ipos=scan(str,'eEdD')                     ! find end of real number if string uses exponent notation
   if(ipos>0) then                           ! letter was found
      expo=str(ipos:)                        ! keep exponent string so it can be added back as a suffix
      str=str(1:ipos-1)                      ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if(index(str,'.').eq.0)then               ! if no decimal character in original string add one to end of string
      ii=len_trim(str)
      str(ii+1:ii+1)='.'                     ! add decimal to end of string
   endif
   do i=len_trim(str),1,-1                   ! scanning from end find a non-zero character
      select case(str(i:i))
      case('0')                              ! found a trailing zero so keep trimming
         cycle
      case('.')                              ! found a decimal character at end of remaining string
         if(i.le.1)then
            str='0'
         else
            str=str(1:i-1)
         endif
         exit
      case default
         str=str(1:i)                        ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if(ipos>0)then                            ! if originally had an exponent place it back on
      string=trim(str)//trim(expo)
   else
      string=str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    substitute(3f) - [M_CLI2:EDITING] subroutine globally substitutes one substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine substitute(targetline,old,new,ierr,start,end)
!!
!!     character(len=*)              :: targetline
!!     character(len=*),intent(in)   :: old
!!     character(len=*),intent(in)   :: new
!!     integer,intent(out),optional  :: ierr
!!     integer,intent(in),optional   :: start
!!     integer,intent(in),optional   :: end
!!##DESCRIPTION
!!    Globally substitute one substring for another in string.
!!
!!##OPTIONS
!!     TARGETLINE  input line to be changed. Must be long enough to
!!                 hold altered output.
!!     OLD         substring to find and replace
!!     NEW         replacement for OLD substring
!!     IERR        error code. If IER = -1 bad directive, >= 0 then
!!                 count of changes made.
!!     START       sets the left margin to be scanned for OLD in
!!                 TARGETLINE.
!!     END         sets the right margin to be scanned for OLD in
!!                 TARGETLINE.
!!
!!##EXAMPLES
!!
!! Sample Program:
!!
!!     program demo_substitute
!!     use M_CLI2, only : substitute
!!     implicit none
!!     ! must be long enough to hold changed line
!!     character(len=80) :: targetline
!!
!!     targetline='this is the input string'
!!     write(*,*)'ORIGINAL    : '//trim(targetline)
!!
!!     ! changes the input to 'THis is THe input string'
!!     call substitute(targetline,'th','TH')
!!     write(*,*)'th => TH    : '//trim(targetline)
!!
!!     ! a null old substring means "at beginning of line"
!!     ! changes the input to 'BEFORE:this is the input string'
!!     call substitute(targetline,'','BEFORE:')
!!     write(*,*)'"" => BEFORE: '//trim(targetline)
!!
!!     ! a null new string deletes occurrences of the old substring
!!     ! changes the input to 'ths s the nput strng'
!!     call substitute(targetline,'i','')
!!     write(*,*)'i => ""     : '//trim(targetline)
!!
!!     end program demo_substitute
!!
!!   Expected output
!!
!!     ORIGINAL    : this is the input string
!!     th => TH    : THis is THe input string
!!     "" => BEFORE: BEFORE:THis is THe input string
!!     i => ""     : BEFORE:THs s THe nput strng
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine substitute(targetline,old,new,ierr,start,end)

! ident_28="@(#)M_CLI2::substitute(3f): Globally substitute one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*)               :: targetline         ! input line to be changed
character(len=*),intent(in)    :: old                ! old substring to replace
character(len=*),intent(in)    :: new                ! new substring
integer,intent(out),optional   :: ierr               ! error code. if ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional    :: start              ! start sets the left margin
integer,intent(in),optional    :: end                ! end sets the right margin
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=len(targetline)) :: dum1               ! scratch string buffers
integer                        :: ml, mr, ier1
integer                        :: maxlengthout       ! MAXIMUM LENGTH ALLOWED FOR NEW STRING
integer                        :: original_input_length
integer                        :: len_old, len_new
integer                        :: ladd
integer                        :: ir
integer                        :: ind
integer                        :: il
integer                        :: id
integer                        :: ic
integer                        :: iichar
!-----------------------------------------------------------------------------------------------------------------------------------
   if (present(start)) then                            ! optional starting column
      ml=start
   else
      ml=1
   endif
   if (present(end)) then                              ! optional ending column
      mr=end
   else
      mr=len(targetline)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier1=0                                              ! initialize error flag/change count
   maxlengthout=len(targetline)                        ! max length of output string
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   dum1(:)=' '                                         ! initialize string to build output in
   id=mr-ml                                            ! check for window option !-! change to optional parameter(s)
!-----------------------------------------------------------------------------------------------------------------------------------
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   if(id.le.0)then                                     ! no window so change entire input string
      il=1                                             ! il is left margin of window to change
      ir=maxlengthout                                  ! ir is right margin of window to change
      dum1(:)=' '                                      ! begin with a blank line
   else                                                ! if window is set
      il=ml                                            ! use left margin
      ir=min0(mr,maxlengthout)                         ! use right margin or rightmost
      dum1=targetline(:il-1)                           ! begin with what's below margin
   endif                                               ! end of window settings
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old.eq.0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(iichar.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if (present(ierr))ierr=ier1
         return
      endif
      if(len_new.gt.0)then
         dum1(il:)=new(:len_new)//targetline(il:original_input_length)
      else
         dum1(il:)=targetline(il:original_input_length)
      endif
      targetline(1:maxlengthout)=dum1(:maxlengthout)
      ier1=1                                           ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ier1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iichar=il                                           ! place to put characters into output string
   ic=il                                               ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1    ! try to find start of old string in remaining part of input in change window
      if(ind.eq.ic-1.or.ind.gt.ir)then                 ! did not find old string or found old string past edit window
         exit loop                                     ! no more changes left to make
      endif
      ier1=ier1+1                                      ! found an old string to change, so increment count of changes
      if(ind.gt.ic)then                                ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         if(iichar-1+ladd.gt.maxlengthout)then
            ier1=-1
            exit loop
         endif
         dum1(iichar:)=targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(iichar-1+len_new.gt.maxlengthout)then
         ier1=-2
         exit loop
      endif
      if(len_new.ne.0)then
         dum1(iichar:)=new(:len_new)
         iichar=iichar+len_new
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ier1)
   case (:-1)
      call journal('sc','*substitute* new line will be too long')
   case (0)                                                ! there were no changes made to the window
   case default
      ladd=original_input_length-ic
      if(iichar+ladd.gt.maxlengthout)then
         call journal('sc','*substitute* new line will be too long')
         ier1=-1
         if(present(ierr))ierr=ier1
         return
      endif
      if(ic.lt.len(targetline))then
         dum1(iichar:)=targetline(ic:max(ic,original_input_length))
      endif
      targetline=dum1(:maxlengthout)
   end select
   if(present(ierr))ierr=ier1
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine substitute
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_CLI2] finds the index where a string is found or should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end.eq.0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus.eq.1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus.eq.end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_29="@(#)M_CLI2::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      elseif(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   elseif(error.ne.0)then
      write(warn,*)message//' VALUE=',trim(value)//' PLACE=',place
      call mystop(-24,'(*locate_c* '//message)
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove(3f) - [M_CLI2] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!! Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate, remove
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_30="@(#)M_CLI2::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove_c
subroutine remove_l(list,place)

! ident_31="@(#)M_CLI2::remove_l(3fp): remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_l
subroutine remove_i(list,place)

! ident_32="@(#)M_CLI2::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_CLI2] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_CLI2, only  : insert, locate, replace
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate_key('a',place)
!!     if(place.gt.0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate_key(key,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(keywords,key,abs(place))
!!        call insert(values,val,abs(place))
!!     else ! replace
!!        call replace(values,val,place)
!!     endif
!!
!!     end subroutine update
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_33="@(#)M_CLI2::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(warn,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
subroutine replace_l(list,value,place)

! ident_34="@(#)M_CLI2::replace_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_35="@(#)M_CLI2::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_CLI2] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate, insert
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_36="@(#)M_CLI2::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(warn,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
subroutine insert_l(list,value,place)

! ident_37="@(#)M_CLI2::insert_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_38="@(#)M_CLI2::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine many_args(n0,g0, n1,g1, n2,g2, n3,g3, n4,g4, n5,g5, n6,g6, n7,g7, n8,g8, n9,g9, &
                   & na,ga, nb,gb, nc,gc, nd,gd, ne,ge, nf,gf, ng,gg, nh,gh, ni,gi, nj,gj )
implicit none

! ident_39="@(#)M_CLI2::many_args(3fp): allow for multiple calls to get_args(3f)"

character(len=*),intent(in)          :: n0, n1
character(len=*),intent(in),optional ::         n2, n3, n4, n5, n6, n7, n8, n9, na, nb, nc, nd, ne, nf, ng, nh, ni, nj
class(*),intent(out)           :: g0, g1
class(*),intent(out),optional  ::         g2, g3, g4, g5, g6, g7, g8, g9
class(*),intent(out),optional  :: ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
   call get_generic(n0,g0)
   call get_generic(n1,g1)
   if( present(n2) .and. present(g2) )call get_generic(n2,g2)
   if( present(n3) .and. present(g3) )call get_generic(n3,g3)
   if( present(n4) .and. present(g4) )call get_generic(n4,g4)
   if( present(n5) .and. present(g5) )call get_generic(n5,g5)
   if( present(n6) .and. present(g6) )call get_generic(n6,g6)
   if( present(n7) .and. present(g7) )call get_generic(n7,g7)
   if( present(n8) .and. present(g8) )call get_generic(n8,g8)
   if( present(n9) .and. present(g9) )call get_generic(n9,g9)
   if( present(na) .and. present(ga) )call get_generic(na,ga)
   if( present(nb) .and. present(gb) )call get_generic(nb,gb)
   if( present(nc) .and. present(gc) )call get_generic(nc,gc)
   if( present(nd) .and. present(gd) )call get_generic(nd,gd)
   if( present(ne) .and. present(ge) )call get_generic(ne,ge)
   if( present(nf) .and. present(gf) )call get_generic(nf,gf)
   if( present(ng) .and. present(gg) )call get_generic(ng,gg)
   if( present(nh) .and. present(gh) )call get_generic(nh,gh)
   if( present(ni) .and. present(gi) )call get_generic(ni,gi)
   if( present(nj) .and. present(gj) )call get_generic(nj,gj)
contains
!===================================================================================================================================
function c(generic)
class(*),intent(in) :: generic
character(len=:),allocatable :: c
   select type(generic)
      type is (character(len=*)); c=trim(generic)
      class default
         c='unknown'
         stop 'get_many:: parameter name is not character'
   end select
end function c
!===================================================================================================================================
subroutine get_generic(name,generic)
use,intrinsic :: iso_fortran_env, only : real64
character(len=*),intent(in)  :: name
class(*),intent(out)         :: generic
   select type(generic)
      type is (integer);                        call get_args(name,generic)
      type is (real);                           call get_args(name,generic)
      type is (real(kind=real64));              call get_args(name,generic)
      type is (logical);                        call get_args(name,generic)
      !x!type is (character(len=:),allocatable ::);   call get_args(name,generic)
      type is (character(len=*));
      call get_args_fixed_length(name,generic)
      type is (complex);                        call get_args(name,generic)
      class default
         stop 'unknown type in *get_generic*'
   end select
end subroutine get_generic
!===================================================================================================================================
end subroutine many_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function iget(n); integer                      :: iget; character(len=*),intent(in) :: n; call get_args(n,iget); end function iget
function rget(n); real                         :: rget; character(len=*),intent(in) :: n; call get_args(n,rget); end function rget
function dget(n); real(kind=dp)                :: dget; character(len=*),intent(in) :: n; call get_args(n,dget); end function dget
function sget(n); character(len=:),allocatable :: sget; character(len=*),intent(in) :: n; call get_args(n,sget); end function sget
function cget(n); complex                      :: cget; character(len=*),intent(in) :: n; call get_args(n,cget); end function cget
function lget(n); logical                      :: lget; character(len=*),intent(in) :: n; call get_args(n,lget); end function lget

function igs(n); integer,allocatable          :: igs(:); character(len=*),intent(in) :: n; call get_args(n,igs); end function igs
function rgs(n); real,allocatable             :: rgs(:); character(len=*),intent(in) :: n; call get_args(n,rgs); end function rgs
function dgs(n); real(kind=dp),allocatable    :: dgs(:); character(len=*),intent(in) :: n; call get_args(n,dgs); end function dgs
function sgs(n); character(len=:),allocatable :: sgs(:); character(len=*),intent(in) :: n; call get_args(n,sgs); end function sgs
function cgs(n); complex,allocatable          :: cgs(:); character(len=*),intent(in) :: n; call get_args(n,cgs); end function cgs
function lgs(n); logical,allocatable          :: lgs(:); character(len=*),intent(in) :: n; call get_args(n,lgs); end function lgs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function ig()
integer,allocatable :: ig(:)
integer             :: i, ierr
   if(allocated(ig))deallocate(ig)
   allocate(ig(size(unnamed)))
   do i=1,size(ig)
      call a2i(unnamed(i),ig(i),ierr)
   enddo
end function ig
!===================================================================================================================================
function rg()
real,allocatable :: rg(:)
   rg=real(dg())
end function rg
!===================================================================================================================================
function dg()
real(kind=dp),allocatable :: dg(:)
integer                   :: i
integer                   :: ierr
   if(allocated(dg))deallocate(dg)
   allocate(dg(size(unnamed)))
   do i=1,size(dg)
      call a2d(unnamed(i),dg(i),ierr)
   enddo
end function dg
!===================================================================================================================================
function lg()
logical,allocatable   :: lg(:)
integer               :: i
integer               :: iichar
character,allocatable :: hold
   if(allocated(lg))deallocate(lg)
   allocate(lg(size(unnamed)))
   do i=1,size(lg)
      hold=trim(upper(adjustl(unnamed(i))))
      if(hold(1:1).eq.'.')then                 ! looking for fortran logical syntax .STRING.
         iichar=2
      else
         iichar=1
      endif
      select case(hold(iichar:iichar))         ! check word to see if true or false
      case('T','Y',' '); lg(i)=.true.          ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
      case('F','N');     lg(i)=.false.         ! assume this is false or no
      case default
         call journal('sc',"*lg* bad logical expression for element",i,'=',hold)
      end select
   enddo
end function lg
!===================================================================================================================================
function cg()
complex,allocatable :: cg(:)
integer             :: i, ierr
real(kind=dp)       :: rc, ic
   if(allocated(cg))deallocate(cg)
   allocate(cg(size(unnamed)))
   do i=1,size(cg),2
      call a2d(unnamed(i),rc,ierr)
      call a2d(unnamed(i+1),ic,ierr)
      cg(i)=cmplx(rc,ic,kind=sp)
   enddo
end function cg
!===================================================================================================================================
function sg()
character(len=:),allocatable :: sg(:)
   sg=unnamed
end function sg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mystop(sig,msg)
! negative signal means always stop program
! else do not stop and set G_STOP_MESSAGE if G_STOPON is false
! or
! print message and stop if G_STOPON is true
! the MSG is NOT for displaying except for internal errors when the program will be stopped.
! It is for returning a value when the stop is being ignored
!
integer,intent(in) :: sig
character(len=*),intent(in),optional :: msg
   !x!write(*,*)'MYSTOP:',sig,trim(msg)
   if(sig.lt.0)then
      if(present(msg))call journal('sc',msg)
      !x!stop abs(sig)
      stop 1
   elseif(G_STOPON)then
      stop
   else
      if(present(msg)) then
         G_STOP_MESSAGE=msg
      else
         G_STOP_MESSAGE=''
      endif
      G_STOP=sig
      !x!write(*,*)'G_STOP:',g_stop,trim(msg)
   endif
end subroutine mystop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_40="@(#)M_strings::atleast(3f): return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine locate_key(value,place)

! ident_41="@(#)M_CLI2::locate_key(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
integer                                 :: ii
   if(len_trim(value).eq.1)then
      !x!ii=findloc(shorts,value,dim=1)
      ii=maxloc([0,merge(1, 0, shorts.eq.value)],dim=1)
      if(ii.gt.1)then
         place=ii-1
      else
         call locate(keywords,value,place)
      endif
   else
      call locate(keywords,value,place)
   endif
end subroutine locate_key
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module MM_CLI2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! REVISION:  nvfortran does not support real128 from iso_fortran_env x86_64 GNU/Linux
!            nvfortran 20.7-0 LLVM 64-bit target on x86-64 Linux -tp nehalem
! < !NVFORTRAN-S-0000-Internal compiler error. size_of: attempt to get size of assumed size character       0  (M_CLI2.f90: 2012)
! < !  0 inform,   0 warnings,   1 severes, 0 fatal for get_anyarray_cc
! Changed
!       allocate(character(len=*)::strings(0))
! to
!       strings=[character(len=len(strings)) ::]
!===================================================================================================================================
 
 
!>>>>> ././src/fpm/error.f90
!> Implementation of basic error handling.
module fpm_error
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
    use fpm_strings, only : is_fortran_name, to_fortran_name
    implicit none
    private

    public :: error_t
    public :: fatal_error, syntax_error, file_not_found_error
    public :: file_parse_error
    public :: bad_name_error
    public :: fpm_stop


    !> Data type defining an error
    type :: error_t

        !> Error message
        character(len=:), allocatable :: message

    end type error_t

contains

    !> Generic fatal runtime error
    subroutine fatal_error(error, message)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Error message
        character(len=*), intent(in) :: message

        allocate(error)
        error%message = message

    end subroutine fatal_error

    subroutine syntax_error(error, message)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Error message
        character(len=*), intent(in) :: message

        allocate(error)
        error%message = message

    end subroutine syntax_error

    function bad_name_error(error, label,name)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Error message label to add to message
        character(len=*), intent(in) :: label

        !> name value to check
        character(len=*), intent(in) :: name

        logical :: bad_name_error

        if(.not.is_fortran_name(to_fortran_name(name)))then
           bad_name_error=.true.
           allocate(error)
           error%message = 'manifest file syntax error: '//label//' name must be composed only of &
           &alphanumerics, "-" and "_"  and start with a letter ::'//name
        else
          bad_name_error=.false.
        endif

    end function bad_name_error


    !> Error created when a file is missing or not found
    subroutine file_not_found_error(error, file_name)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Name of the missing file
        character(len=*), intent(in) :: file_name

        allocate(error)
        error%message = "'"//file_name//"' could not be found, check if the file exists"

    end subroutine file_not_found_error


    !> Error created when file parsing fails
    subroutine file_parse_error(error, file_name, message, line_num, &
                                 line_string, line_col)

        !> Instance of the error data
        type(error_t), allocatable, intent(out) :: error

        !> Name of file
        character(len=*), intent(in) :: file_name

        !> Parse error message
        character(len=*), intent(in) :: message

        !> Line number of parse error
        integer, intent(in), optional :: line_num

        !> Line context string
        character(len=*), intent(in), optional :: line_string

        !> Line context column
        integer, intent(in), optional :: line_col

        character(50) :: temp_string

        allocate(error)
        error%message = 'Parse error: '//message//new_line('a')

        error%message = error%message//file_name

        if (present(line_num)) then

            write(temp_string,'(I0)') line_num

            error%message = error%message//':'//trim(temp_string)

        end if

        if (present(line_col)) then

            if (line_col > 0) then

                write(temp_string,'(I0)') line_col
                error%message = error%message//':'//trim(temp_string)

            end if

        end if

        if (present(line_string)) then

            error%message = error%message//new_line('a')
            error%message = error%message//'   | '//line_string

            if (present(line_col)) then

                if (line_col > 0) then

                    error%message = error%message//new_line('a')
                    error%message = error%message//'   | '//repeat(' ',line_col-1)//'^'

                end if

            end if

        end if

    end subroutine file_parse_error

    subroutine fpm_stop(value,message)
    ! TODO: if verbose mode, call ERROR STOP instead of STOP
    ! TODO: if M_escape is used, add color
    ! to work with older compilers might need a case statement for values

        !> value to use on STOP
        integer, intent(in) :: value
        !> Error message
        character(len=*), intent(in) :: message
        if(message/='')then
           if(value>0)then
              write(stderr,'("<ERROR>",a)')trim(message)
           else
              write(stderr,'("<INFO> ",a)')trim(message)
           endif
        endif
        stop value
    end subroutine fpm_stop

end module fpm_error
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/datetime.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a TOML datetime value
module tomlf_datetime
   use tomlf_constants, only : tfc
   implicit none
   private

   public :: toml_datetime, toml_time, toml_date


   !> TOML time value (HH:MM:SS.sssssZ...)
   type :: toml_time
      integer :: hour = 0
      integer :: minute = 0
      integer :: second = 0
      integer, allocatable :: millisec
      character(len=:), allocatable :: zone
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => time_to_string
   end type

   interface toml_time
      module procedure :: new_toml_time
   end interface toml_time


   !> TOML date value (YYYY-MM-DD)
   type :: toml_date
      integer :: year = 0
      integer :: month = 0
      integer :: day = 0
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => date_to_string
   end type


   !> TOML datatime value type
   type :: toml_datetime
      type(toml_date), allocatable :: date
      type(toml_time), allocatable :: time
   contains
      generic :: assignment(=) => to_string
      procedure, pass(rhs) :: to_string => datetime_to_string
   end type


contains


subroutine date_to_string(lhs, rhs)
   character(kind=tfc, len=:), allocatable, intent(out) :: lhs
   class(toml_date), intent(in) :: rhs
   allocate(character(kind=tfc, len=10) :: lhs)
   write(lhs, '(i4.4,"-",i2.2,"-",i2.2)') &
      &  rhs%year, rhs%month, rhs%day
end subroutine date_to_string


subroutine time_to_string(lhs, rhs)
   character(kind=tfc, len=:), allocatable, intent(out) :: lhs
   class(toml_time), intent(in) :: rhs
   if (allocated(rhs%millisec)) then
      allocate(character(kind=tfc, len=12) :: lhs)
      write(lhs, '(i2.2,":",i2.2,":",i2.2,".",i3.3)') &
         &  rhs%hour, rhs%minute, rhs%second, rhs%millisec
   else
      allocate(character(kind=tfc, len=8) :: lhs)
      write(lhs, '(i2.2,":",i2.2,":",i2.2)') &
         &  rhs%hour, rhs%minute, rhs%second
   end if
   if (allocated(rhs%zone)) lhs = lhs // trim(rhs%zone)
end subroutine time_to_string


subroutine datetime_to_string(lhs, rhs)
   character(kind=tfc, len=:), allocatable, intent(out) :: lhs
   class(toml_datetime), intent(in) :: rhs
   character(kind=tfc, len=:), allocatable :: temporary
   if (allocated(rhs%date)) then
      call rhs%date%to_string(lhs)
      if (allocated(rhs%time)) then
         call rhs%time%to_string(temporary)
         lhs = lhs // tfc_'T' // temporary
      end if
   else
      if (allocated(rhs%time)) lhs = rhs%time
   end if
end subroutine datetime_to_string


!> Constructor for toml_time type, necessary due to PGI bug in NVHPC 20.7 and 20.9
elemental function new_toml_time(hour, minute, second, millisec, zone) &
      & result(self)
   integer, intent(in), optional :: hour
   integer, intent(in), optional :: minute
   integer, intent(in), optional :: second
   integer, intent(in), optional :: millisec
   character(len=*), intent(in), optional :: zone
   type(toml_time) :: self
   if (present(hour)) self%hour = hour
   if (present(minute)) self%minute = minute
   if (present(second)) self%second = second
   if (present(millisec)) self%millisec = millisec
   if (present(zone)) self%zone = zone
end function new_toml_time


end module tomlf_datetime
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/error.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Central registry for error codes
module tomlf_error
   use tomlf_constants, only : tfc, TOML_NEWLINE
   implicit none
   private

   public :: toml_stat, toml_error, toml_context
   public :: syntax_error, duplicate_key_error, io_error, vendor_error


   !> Possible TOML-Fortran error codes
   type :: enum_stat

      !> Successful run
      integer :: success = 0

      !> Internal error:
      !
      !  General undefined error state, usually caused by algorithmic errors.
      integer :: fatal = -1

      !> Duplicate key error:
      !
      !  Tried to push back an already present key on a TOML table or
      !  TOML document contains duplicate keys, already present in table.
      integer :: duplicate_key = 1

      !> Syntax error
      integer :: invalid_syntax = 2

      !> IO error
      integer :: io_failure = 3

   end type enum_stat

   !> Actual enumerator for return states
   type(enum_stat), parameter :: toml_stat = enum_stat()


   !> Context for error message (usually a line in a TOML document)
   type :: toml_context

      !> Current internal position
      integer :: pos = 0

      !> Current internal count
      integer :: num = 0

      !> Current internal location on the string buffer
      character(kind=tfc, len=:), pointer :: ptr => null()

   end type toml_context


   !> Error message produced by TOML-Fortran
   type :: toml_error

      !> Error code
      integer :: stat = toml_stat%fatal

      !> Payload of the error
      character(kind=tfc, len=:), allocatable :: message

   end type toml_error


contains


!> A syntactic error in a TOML document was found
subroutine syntax_error(error, context, message, stat)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Current context producing the error
   type(toml_context), intent(in), optional :: context

   !> A detailed message describing the error and (optionally) offering advice
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Overwrite of the error code
   integer, intent(in), optional :: stat

   allocate(error)

   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%invalid_syntax
   end if

   if (present(message)) then
      error%message = message
   else
      error%message = "Syntax error"
   end if

   if (present(context)) then
      call add_context(error%message, context)
   end if

end subroutine syntax_error


!> Key is present multiple times in a TOML document within the same table
subroutine duplicate_key_error(error, context, key, stat)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Current context producing the error
   type(toml_context), intent(in), optional :: context

   !> The offending duplicate key
   character(kind=tfc, len=*), intent(in), optional :: key

   !> Overwrite of the error code
   integer, intent(in), optional :: stat

   allocate(error)

   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%duplicate_key
   end if

   if (present(key)) then
      error%message = "Duplicate key ("//key//") found"
   else
      error%message = "Duplicate key found"
   end if

   if (present(context)) then
      call add_context(error%message, context)
   end if

end subroutine duplicate_key_error


!> IO runtime error
subroutine io_error(error, message)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> A detailed message describing the error and (optionally) offering advice
   character(kind=tfc, len=*), intent(in), optional :: message

   allocate(error)
   error%stat = toml_stat%io_failure

   if (present(message)) then
      error%message = message
   else
      error%message = "IO runtime error"
   end if

end subroutine io_error


!> A shortcoming in the implementation or an internal error occured, rather
!  than falling back to unpredictable and possibly harmful behaviour, we try
!  to offer an apology for this inconvenience
subroutine vendor_error(error, context, message, stat)

   !> Instance of the TOML error
   type(toml_error), allocatable, intent(out) :: error

   !> Current context producing the error
   type(toml_context), intent(in), optional :: context

   !> A detailed message describing the error and (optionally) offering advice
   character(kind=tfc, len=*), intent(in), optional :: message

   !> Overwrite of the error code
   integer, intent(in), optional :: stat

   allocate(error)

   if (present(stat)) then
      error%stat = stat
   else
      error%stat = toml_stat%fatal
   end if

   if (present(message)) then
      error%message = message
   else
      error%message = "Internal error"
   end if

   if (present(context)) then
      call add_context(error%message, context)
   end if

end subroutine vendor_error


!> Put an existing error message into a more useful context
subroutine add_context(message, context)

   !> A detailed message describing the error, requiring some more context
   character(len=:), allocatable, intent(inout) :: message

   !> Current context producing the error
   type(toml_context), intent(in) :: context

   character(len=20) :: num
   integer :: line_break

   if (context%num > 0) then
      write(num, '("line",1x,i0,":")') context%num
      message = num(1:len_trim(num)+1) // message
   end if

   if (associated(context%ptr)) then
      line_break = index(context%ptr, TOML_NEWLINE)-1
      if (line_break < 0) line_break = len(context%ptr)
      message = message // TOML_NEWLINE // &
         & '   | '// context%ptr(1:line_break) // TOML_NEWLINE // &
         & '   |'
      if (context%pos > 0 .and. context%pos <= line_break) then
         message = message // repeat('-', context%pos) // '^'
      end if
   end if

end subroutine add_context


end module tomlf_error
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/utils/verify.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Contains utilities to verify TOML raw values correspond to a certain datatype
module tomlf_utils_verify
   use tomlf_constants
   implicit none
   private

   public :: toml_raw_verify_string, toml_raw_verify_float, toml_raw_verify_bool
   public :: toml_raw_verify_integer, toml_raw_verify_timestamp
   public :: toml_raw_verify_date, toml_raw_verify_time


contains


!> Verify a raw value as TOML string
pure function toml_raw_verify_string(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   stat = raw(1:1) == TOML_SQUOTE .or. raw(1:1) == TOML_DQUOTE

end function toml_raw_verify_string


!> Verify a raw value as TOML float
pure function toml_raw_verify_float(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   logical :: plus_minus
   integer :: first
   integer :: dot_pos
   integer :: exp_pos
   first = 1
   stat = .false.
   if (raw == 'nan') then
      stat = .true.
      return
   end if
   ! allow leading + or -
   plus_minus = raw(1:1) == '+' .or. raw(1:1) == '-'
   if (plus_minus) first = first+1
   ! allow infinity and not-a-number
   if (raw(first:) == 'inf' .or. raw(first:) == 'nan') then
      stat = .true.
      return
   end if
   ! position of dot and exponent
   dot_pos = index(raw, '.')
   exp_pos = scan(raw, 'Ee')
   if (dot_pos == 0 .and. exp_pos == 0) return
   if (dot_pos > 0 .and. exp_pos > 0 .and. dot_pos > exp_pos) return
   ! check for leading or trailing underscores
   if (raw(first:first) == '_' .or. raw(len(raw):) == '_') return
   ! check for leading or trailing dots
   if (first == dot_pos .or. len(raw) == dot_pos) return
   if (dot_pos > 0) then
      if (raw(dot_pos+1:dot_pos+1) == '_' .or. raw(dot_pos-1:dot_pos-1) == '_') return
   end if
   ! zero must be followed by a dot or exponent
   if (raw(first:first) == '0' .and. len(raw(first:)) > 1) then
      if (first+1 /= dot_pos .and. first+1 /= exp_pos) return
   end if
   ! no double underscores
   if (index(raw, '__') > 0) return
   ! check for digits
   stat = verify(raw(first:), TOML_DIGITS//'._-+eE') == 0

end function toml_raw_verify_float


!> Verify a raw value as TOML integer
pure function toml_raw_verify_integer(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   logical :: plus_minus
   integer :: first, base

   first = 1
   base = 10
   stat = .false.
   ! allow leading + or -
   plus_minus = raw(1:1) == '+' .or. raw(1:1) == '-'
   if (plus_minus) first = first+1
   ! check for leading underscores
   if (raw(first:first) == '_') return
   ! no double underscores
   if (index(raw, '__') > 0) return
   ! 0 indicates other base systems
   if (raw(first:first) == '0' .and. len(raw) > first) then
      select case(raw(first+1:first+1))
      case('x'); base = 16
      case('o'); base = 8
      case('b'); base = 2
      case default; return ! disallow 0[0-9_]+
      end select
      first = first + 2
      ! check for leading underscores, again
      if (raw(first:first) == '_') return
   end if
   ! check for trailing underscores
   if (raw(len(raw):) == '_') return
   ! verify we only allowed digits
   select case(base)
   case default
      stat = verify(raw(first:), TOML_DIGITS//'_') == 0
   case(16)
      stat = verify(raw(first:), TOML_HEXDIGITS//'_') == 0
   case(8)
      stat = verify(raw(first:), TOML_OCTDIGITS//'_') == 0
   case(2)
      stat = verify(raw(first:), TOML_BINDIGITS//'_') == 0
   end select
end function


!> Verify a raw value as TOML bool
pure function toml_raw_verify_bool(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   stat = raw == 'true' .or. raw == 'false'

end function toml_raw_verify_bool


!> Verify a raw value as TOML datetime expression
pure function toml_raw_verify_timestamp(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   integer :: first

   first = 1
   stat = .false.
   if (toml_raw_verify_date(raw)) then
      if (len(raw) == 10) then
         stat = .true.
         return
      end if
      if (raw(11:11) /= ' ' .and. raw(11:11) /= 'T') return
      first = 12
   end if

   stat = toml_raw_verify_time(raw(first:))

end function toml_raw_verify_timestamp


!> Verify a raw value as TOML date expression (YYYY-MM-DD)
pure function toml_raw_verify_date(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   if (len(raw) >= 10) then
      stat = verify(raw(1:4), TOML_DIGITS) == 0 .and. raw(5:5) == '-' .and. &
         &   verify(raw(6:7), TOML_DIGITS) == 0 .and. raw(8:8) == '-' .and. &
         &   verify(raw(9:10), TOML_DIGITS) == 0
   else
      stat = .false.
   end if

end function toml_raw_verify_date


!> Verify a raw value as TOML time expression (HH:MM:SS)
pure function toml_raw_verify_time(raw) result(stat)

   !> Raw value to verify
   character(kind=tfc, len=*), intent(in) :: raw

   !> Status of the evaluation
   logical :: stat

   if (len(raw) >= 8) then
      stat = verify(raw(1:2), TOML_DIGITS) == 0 .and. raw(3:3) == ':' .and. &
         &   verify(raw(4:5), TOML_DIGITS) == 0 .and. raw(6:6) == ':' .and. &
         &   verify(raw(7:8), TOML_DIGITS) == 0
   else
      stat = .false.
   end if

end function toml_raw_verify_time


end module tomlf_utils_verify
 
 
!>>>>> ././src/fpm_environment.f90
!> This module contains procedures that interact with the programming environment.
!!
!! * [get_os_type] -- Determine the OS type
!! * [get_env] -- return the value of an environment variable
module fpm_environment
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                           & stdout=>output_unit, &
                                           & stderr=>error_unit
    use fpm_error, only : fpm_stop
    implicit none
    private
    public :: get_os_type
    public :: os_is_unix
    public :: get_env
    public :: get_command_arguments_quoted
    public :: separator

    integer, parameter, public :: OS_UNKNOWN = 0
    integer, parameter, public :: OS_LINUX   = 1
    integer, parameter, public :: OS_MACOS   = 2
    integer, parameter, public :: OS_WINDOWS = 3
    integer, parameter, public :: OS_CYGWIN  = 4
    integer, parameter, public :: OS_SOLARIS = 5
    integer, parameter, public :: OS_FREEBSD = 6
    integer, parameter, public :: OS_OPENBSD = 7
contains
    !> Determine the OS type
    integer function get_os_type() result(r)
        !!
        !! Returns one of OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN,
        !! OS_SOLARIS, OS_FREEBSD, OS_OPENBSD.
        !!
        !! At first, the environment variable `OS` is checked, which is usually
        !! found on Windows. Then, `OSTYPE` is read in and compared with common
        !! names. If this fails too, check the existence of files that can be
        !! found on specific system types only.
        !!
        !! Returns OS_UNKNOWN if the operating system cannot be determined.
        character(len=32) :: val
        integer           :: length, rc
        logical           :: file_exists
        logical, save     :: first_run = .true.
        integer, save     :: ret = OS_UNKNOWN
        !omp threadprivate(ret, first_run)

        if (.not. first_run) then
            r = ret
            return
        end if

        first_run = .false.
        r = OS_UNKNOWN

        ! Check environment variable `OS`.
        call get_environment_variable('OS', val, length, rc)

        if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
            r = OS_WINDOWS
            ret = r
            return
        end if

        ! Check environment variable `OSTYPE`.
        call get_environment_variable('OSTYPE', val, length, rc)

        if (rc == 0 .and. length > 0) then
            ! Linux
            if (index(val, 'linux') > 0) then
                r = OS_LINUX
                ret = r
                return
            end if

            ! macOS
            if (index(val, 'darwin') > 0) then
                r = OS_MACOS
                ret = r
                return
            end if

            ! Windows, MSYS, MinGW, Git Bash
            if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
                r = OS_WINDOWS
                ret = r
                return
            end if

            ! Cygwin
            if (index(val, 'cygwin') > 0) then
                r = OS_CYGWIN
                ret = r
                return
            end if

            ! Solaris, OpenIndiana, ...
            if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
                r = OS_SOLARIS
                ret = r
                return
            end if

            ! FreeBSD
            if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
                r = OS_FREEBSD
                ret = r
                return
            end if

            ! OpenBSD
            if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
                r = OS_OPENBSD
                ret = r
                return
            end if
        end if

        ! Linux
        inquire (file='/etc/os-release', exist=file_exists)

        if (file_exists) then
            r = OS_LINUX
            ret = r
            return
        end if

        ! macOS
        inquire (file='/usr/bin/sw_vers', exist=file_exists)

        if (file_exists) then
            r = OS_MACOS
            ret = r
            return
        end if

        ! FreeBSD
        inquire (file='/bin/freebsd-version', exist=file_exists)

        if (file_exists) then
            r = OS_FREEBSD
            ret = r
            return
        end if
    end function get_os_type

    !> Compare the output of [[get_os_type]] or the optional
    !! passed INTEGER value to the value for OS_WINDOWS
    !! and return .TRUE. if they match and .FALSE. otherwise
    logical function os_is_unix(os) result(unix)
        integer, intent(in), optional :: os
        integer :: build_os
        if (present(os)) then
            build_os = os
        else
            build_os = get_os_type()
        end if
        unix = build_os /= OS_WINDOWS
    end function os_is_unix

    !> get named environment variable value. It it is blank or
    !! not set return the optional default value
    function get_env(NAME,DEFAULT) result(VALUE)
    implicit none
    !> name of environment variable to get the value of
    character(len=*),intent(in)          :: NAME
    !> default value to return if the requested value is undefined or blank
    character(len=*),intent(in),optional :: DEFAULT
    !> the returned value
    character(len=:),allocatable         :: VALUE
    integer                              :: howbig
    integer                              :: stat
    integer                              :: length
        ! get length required to hold value
        length=0
        if(NAME/='')then
           call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
           select case (stat)
           case (1)
               !*!print *, NAME, " is not defined in the environment. Strange..."
               VALUE=''
           case (2)
               !*!print *, "This processor doesn't support environment variables. Boooh!"
               VALUE=''
           case default
               ! make string to hold value of sufficient size
               allocate(character(len=max(howbig,1)) :: VALUE)
               ! get value
               call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
               if(stat/=0)VALUE=''
           end select
        else
           VALUE=''
        endif
        if(VALUE==''.and.present(DEFAULT))VALUE=DEFAULT
     end function get_env

    function get_command_arguments_quoted() result(args)
    character(len=:),allocatable :: args
    character(len=:),allocatable :: arg
    character(len=1)             :: quote
    integer                      :: ilength, istatus, i
    ilength=0
    args=''
        quote=merge('"',"'",separator()=='\')
        do i=2,command_argument_count() ! look at all arguments after subcommand
            call get_command_argument(number=i,length=ilength,status=istatus)
            if(istatus /= 0) then
                write(stderr,'(*(g0,1x))')'<ERROR>*get_command_arguments_stack* error obtaining argument ',i
                exit
            else
                if(allocated(arg))deallocate(arg)
                allocate(character(len=ilength) :: arg)
                call get_command_argument(number=i,value=arg,length=ilength,status=istatus)
                if(istatus /= 0) then
                    write(stderr,'(*(g0,1x))')'<ERROR>*get_command_arguments_stack* error obtaining argument ',i
                    exit
                elseif(ilength>0)then
                    if(index(arg//' ','-')/=1)then
                        args=args//quote//arg//quote//' '
                    elseif(index(arg,' ')/=0)then
                        args=args//quote//arg//quote//' '
                    else
                        args=args//arg//' '
                    endif
                else
                    args=args//repeat(quote,2)//' '
                endif
             endif
         enddo
    end function get_command_arguments_quoted

function separator() result(sep)
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!    First using the name the program was invoked with, then the name
!!    returned by an INQUIRE(3f) of that name, then ".\NAME" and "./NAME"
!!    try to determine the separator character used to separate directory
!!    names from file basenames.
!!
!!    If a slash or backslash is not found in the name, the environment
!!    variable PATH is examined first for a backslash, then a slash.
!!
!!    Can be very system dependent. If the queries fail the default returned
!!    is "/".
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator

! use the pathname returned as arg0 to determine pathname separator
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
logical                      :: existing
character(len=1)             :: sep
!*ifort_bug*!character(len=1),save        :: sep_cache=' '
character(len=4096)          :: name
character(len=:),allocatable :: fname

   !*ifort_bug*!   if(sep_cache/=' ')then  ! use cached value. NOTE:  A parallel code might theoretically use multiple OS
   !*ifort_bug*!      sep=sep_cache
   !*ifort_bug*!      return
   !*ifort_bug*!   endif

   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=istat)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=istat)
   ! check argument name
   if(index(arg0,'\')/=0)then
      sep='\'
   elseif(index(arg0,'/')/=0)then
      sep='/'
   else
      ! try name returned by INQUIRE(3f)
      existing=.false.
      name=' '
      inquire(file=arg0,iostat=istat,exist=existing,name=name)
      if(index(name,'\')/=0)then
         sep='\'
      elseif(index(name,'/')/=0)then
         sep='/'
      else
         ! well, try some common syntax and assume in current directory
         fname='.\'//arg0
         inquire(file=fname,iostat=istat,exist=existing)
         if(existing)then
            sep='\'
         else
            fname='./'//arg0
            inquire(file=fname,iostat=istat,exist=existing)
            if(existing)then
               sep='/'
            else ! check environment variable PATH
               sep=merge('\','/',index(get_env('PATH'),'\')/=0)
               !*!write(*,*)'<WARNING>unknown system directory path separator'
            endif
         endif
      endif
   endif
   !*ifort_bug*!sep_cache=sep
end function separator
end module fpm_environment
 
 
!>>>>> ././src/fpm_os.F90
module fpm_os
    use, intrinsic :: iso_c_binding, only : c_char, c_int, c_null_char, c_ptr, c_associated
    use fpm_error, only : error_t, fatal_error
    implicit none
    private
    public :: change_directory, get_current_directory

#ifndef _WIN32
    character(len=*), parameter :: pwd_env = "PWD"
#else
    character(len=*), parameter :: pwd_env = "CD"
#endif

    interface
        function chdir(path) result(stat) &
#ifndef _WIN32
                bind(C, name="chdir")
#else
                bind(C, name="_chdir")
#endif
            import :: c_char, c_int
            character(kind=c_char, len=1), intent(in) :: path(*)
            integer(c_int) :: stat
        end function chdir

        function getcwd(buf, bufsize) result(path) &
#ifndef _WIN32
                bind(C, name="getcwd")
#else
                bind(C, name="_getcwd")
#endif
            import :: c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in) :: buf(*)
            integer(c_int), value, intent(in) :: bufsize
            type(c_ptr) :: path
        end function getcwd
    end interface

contains

    subroutine change_directory(path, error)
        character(len=*), intent(in) :: path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        integer :: stat

        allocate(cpath(len(path)+1))
        call f_c_character(path, cpath, len(path)+1)

        stat = chdir(cpath)

        if (stat /= 0) then
            call fatal_error(error, "Failed to change directory to '"//path//"'")
        end if
    end subroutine change_directory

    subroutine get_current_directory(path, error)
        character(len=:), allocatable, intent(out) :: path
        type(error_t), allocatable, intent(out) :: error

        character(kind=c_char, len=1), allocatable :: cpath(:)
        integer(c_int), parameter :: buffersize = 1000_c_int
        type(c_ptr) :: tmp

        allocate(cpath(buffersize))

        tmp = getcwd(cpath, buffersize)
        if (c_associated(tmp)) then
            call c_f_character(cpath, path)
        else
            call fatal_error(error, "Failed to retrieve current directory")
        end if

    end subroutine get_current_directory

    subroutine f_c_character(rhs, lhs, len)
        character(kind=c_char), intent(out) :: lhs(*)
        character(len=*), intent(in) :: rhs
        integer, intent(in) :: len
        integer :: length
        length = min(len-1, len_trim(rhs))

        lhs(1:length) = transfer(rhs(1:length), lhs(1:length))
        lhs(length+1:length+1) = c_null_char

    end subroutine f_c_character

    subroutine c_f_character(rhs, lhs)
        character(kind=c_char), intent(in) :: rhs(*)
        character(len=:), allocatable, intent(out) :: lhs

        integer :: ii

        do ii = 1, huge(ii) - 1
            if (rhs(ii) == c_null_char) then
                exit
            end if
        end do
        allocate(character(len=ii-1) :: lhs)
        lhs = transfer(rhs(1:ii-1), lhs)

    end subroutine c_f_character

end module fpm_os
 
 
!>>>>> ././src/fpm/versioning.f90
!> Implementation of versioning data for comparing packages
module fpm_versioning
    use fpm_error, only : error_t, syntax_error
    implicit none
    private

    public :: version_t, new_version, char


    type :: version_t
        private

        !> Version numbers found
        integer, allocatable :: num(:)

    contains

        generic :: operator(==) => equals
        procedure, private :: equals

        generic :: operator(/=) => not_equals
        procedure, private :: not_equals

        generic :: operator(>) => greater
        procedure, private :: greater

        generic :: operator(<) => less
        procedure, private :: less

        generic :: operator(>=) => greater_equals
        procedure, private :: greater_equals

        generic :: operator(<=) => less_equals
        procedure, private :: less_equals

        !> Compare a version against a version constraint (x.x.0 <= v < x.x.HUGE)
        generic :: operator(.match.) => match
        procedure, private :: match

        !> Create a printable string from a version data type
        procedure :: to_string

    end type version_t


    !> Arbitrary internal limit of the version parser
    integer, parameter :: max_limit = 3


    interface char
        module procedure :: as_string
    end interface char


    interface new_version
        module procedure :: new_version_from_string
        module procedure :: new_version_from_int
    end interface new_version


contains


    !> Create a new version from a string
    subroutine new_version_from_int(self, num)

        !> Instance of the versioning data
        type(version_t), intent(out) :: self

        !> Subversion numbers to define version data
        integer, intent(in) :: num(:)

        self%num = num

    end subroutine new_version_from_int


    !> Create a new version from a string
    subroutine new_version_from_string(self, string, error)

        !> Instance of the versioning data
        type(version_t), intent(out) :: self

        !> String describing the version information
        character(len=*), intent(in) :: string

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: istart, iend, stat, nn
        integer :: num(max_limit)
        logical :: is_number

        nn = 0
        iend = 0
        istart = 0
        is_number = .false.

        do while(iend < len(string))
            call next(string, istart, iend, is_number, error)
            if (allocated(error)) exit
            if (is_number) then
                if (nn >= max_limit) then
                    call token_error(error, string, istart, iend, &
                        & "Too many subversions found")
                    exit
                end if
                nn = nn + 1
                read(string(istart:iend), *, iostat=stat) num(nn)
                if (stat /= 0) then
                    call token_error(error, string, istart, iend, &
                        & "Failed to parse version number")
                    exit
                end if
            end if
        end do
        if (allocated(error)) return
        if (.not.is_number) then
            call token_error(error, string, istart, iend, &
                & "Expected version number, but no characters are left")
            return
        end if

        call new_version(self, num(:nn))

    end subroutine new_version_from_string


    !> Tokenize a version string
    subroutine next(string, istart, iend, is_number, error)

        !> String describing the version information
        character(len=*), intent(in) :: string

        !> Start of last token, start of next token on exit
        integer, intent(inout) :: istart

        !> End of last token on entry, end of next token on exit
        integer, intent(inout) :: iend

        !> Token produced is a number
        logical, intent(inout) :: is_number

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: ii, nn
        logical :: was_number
        character :: tok

        was_number = is_number
        nn = len(string)

        if (iend >= nn) then
            istart = nn
            iend = nn
            return
        end if

        ii = min(iend + 1, nn)
        tok = string(ii:ii)

        is_number = tok /= '.'
        if (is_number .eqv. was_number) then
            call token_error(error, string, istart, ii, &
                & "Unexpected token found")
            return
        end if

        if (.not.is_number) then
            is_number = .false.
            istart = ii
            iend = ii
            return
        end if

        istart = ii
        do ii = min(iend + 1, nn), nn
            tok = string(ii:ii)
            select case(tok)
            case default
                call token_error(error, string, istart, ii, &
                    & "Invalid character in version number")
                exit
            case('.')
                exit
            case('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
                iend = ii
                cycle
            end select
        end do

    end subroutine next


    !> Create an error on an invalid token, provide some visual context as well
    subroutine token_error(error, string, istart, iend, message)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> String describing the version information
        character(len=*), intent(in) :: string

        !> Start of last token, start of next token on exit
        integer, intent(in) :: istart

        !> End of last token on entry, end of next token on exit
        integer, intent(in) :: iend

        !> Error message
        character(len=*), intent(in) :: message

        character(len=*), parameter :: nl = new_line('a')

        allocate(error)
        error%message = message // nl // "  | " // string // nl // &
            & "  |" // repeat('-', istart) // repeat('^', iend - istart + 1)

    end subroutine token_error


    subroutine to_string(self, string)

        !> Version number
        class(version_t), intent(in) :: self

        !> Character representation of the version
        character(len=:), allocatable, intent(out) :: string

        integer, parameter :: buffersize = 64
        character(len=buffersize) :: buffer
        integer :: ii

        do ii = 1, size(self%num)
            if (allocated(string)) then
                write(buffer, '(".", i0)') self%num(ii)
                string = string // trim(buffer)
            else
                write(buffer, '(i0)') self%num(ii)
                string = trim(buffer)
            end if
        end do

        if (.not.allocated(string)) then
            string = '0'
        end if

    end subroutine to_string


    function as_string(self) result(string)

        !> Version number
        class(version_t), intent(in) :: self

        !> Character representation of the version
        character(len=:), allocatable :: string

        call self%to_string(string)

    end function as_string


    !> Check to version numbers for equality
    elemental function equals(lhs, rhs) result(is_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> Version match
        logical :: is_equal

        is_equal = .not.(lhs > rhs)
        if (is_equal) then
            is_equal = .not.(rhs > lhs)
        end if

    end function equals


    !> Check two versions for inequality
    elemental function not_equals(lhs, rhs) result(not_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> Version mismatch
        logical :: not_equal

        not_equal = lhs > rhs
        if (.not.not_equal) then
            not_equal = rhs > lhs
        end if

    end function not_equals


    !> Relative comparison of two versions
    elemental function greater(lhs, rhs) result(is_greater)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is greater
        logical :: is_greater

        integer :: ii

        do ii = 1, min(size(lhs%num), size(rhs%num))
            is_greater = lhs%num(ii) > rhs%num(ii)
            if (is_greater) exit
        end do
        if (is_greater) return

        is_greater = size(lhs%num) > size(rhs%num)
        if (is_greater) then
            do ii = size(rhs%num) + 1, size(lhs%num)
                is_greater = lhs%num(ii) > 0
                if (is_greater) exit
            end do
        end if

    end function greater


    !> Relative comparison of two versions
    elemental function less(lhs, rhs) result(is_less)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is less
        logical :: is_less

        is_less = rhs > lhs

    end function less


    !> Relative comparison of two versions
    elemental function greater_equals(lhs, rhs) result(is_greater_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is greater or equal
        logical :: is_greater_equal

        is_greater_equal = .not. (rhs > lhs)

    end function greater_equals


    !> Relative comparison of two versions
    elemental function less_equals(lhs, rhs) result(is_less_equal)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> First version is less or equal
        logical :: is_less_equal

        is_less_equal = .not. (lhs > rhs)

    end function less_equals


    !> Try to match first version against second version
    elemental function match(lhs, rhs)

        !> First version number
        class(version_t), intent(in) :: lhs

        !> Second version number
        class(version_t), intent(in) :: rhs

        !> Version match following semantic versioning rules
        logical :: match

        type(version_t) :: tmp

        match = .not.(rhs > lhs)
        if (match) then
            tmp%num = rhs%num
            tmp%num(size(tmp%num)) = tmp%num(size(tmp%num)) + 1
            match = tmp > lhs
        end if

    end function match


end module fpm_versioning
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/utils/convert.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Contains utilities to convert TOML raw values to actual Fortran datatypes
module tomlf_utils_convert
   use tomlf_constants
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_utils_verify
   implicit none
   private

   public :: convert_raw
   public :: toml_raw_to_string, toml_raw_to_float, toml_raw_to_bool
   public :: toml_raw_to_integer, toml_raw_to_timestamp


   !> Overloaded conversion interface
   interface convert_raw
      module procedure :: toml_raw_to_string
      module procedure :: toml_raw_to_float
      module procedure :: toml_raw_to_bool
      module procedure :: toml_raw_to_integer
      module procedure :: toml_raw_to_timestamp
   end interface convert_raw


contains


!> Attempt to convert TOML raw value to Fortran real
function toml_raw_to_float(raw, num) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> Real value represented by raw value
   real(tfr), intent(out) :: num

   !> Status of the evaluation
   logical :: stat

   character(len=len(raw)) :: inp
   integer :: i, j, err

   stat = toml_raw_verify_float(raw)
   if (stat) then
      inp = ''
      j = 0
      do i = 1, len(raw)
         if (raw(i:i) == '_') cycle
         j = j+1
         inp(j:j) = raw(i:i)
      end do
      read(inp, *, iostat=err) num
      stat = err == 0
   end if

end function


!> Attempt to convert TOML raw value to Fortran integer
function toml_raw_to_integer(raw, num) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> Integer value represented by raw value
   integer(tfi), intent(out) :: num

   !> Status of the evaluation
   logical :: stat

   character(kind=tfc, len=len(raw)) :: inp
   character(len=10) :: fmt
   logical :: minus
   integer :: i, j, err
   integer :: first

   stat = toml_raw_verify_integer(raw)
   if (stat) then
      minus = raw(1:1) == '-'
      inp = ''
      first = scan(raw, 'xob')+1
      j = 0
      do i = first, len(raw)
         if (raw(i:i) == '_') cycle
         j = j+1
         inp(j:j) = raw(i:i)
      end do
      if (first > 1) then
         select case(raw(first-1:first-1))
         case('x'); write(fmt, '("(z",i0,")")') j
         case('o'); write(fmt, '("(o",i0,")")') j
         case('b'); write(fmt, '("(b",i0,")")') j
         end select
         read(inp, fmt, iostat=err) num
         if (minus) num = -num
      else
         read(inp, *, iostat=err) num
      end if
      stat = err == 0
   end if

end function toml_raw_to_integer


!> Attempt to convert TOML raw value to Fortran logical
function toml_raw_to_bool(raw, bool) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> Logical value represented by raw value
   logical, intent(out) :: bool

   !> Status of the evaluation
   logical :: stat

   stat = toml_raw_verify_bool(raw)
   if (stat) then
      select case(raw)
      case('true'); bool = .true.
      case('false'); bool = .false.
      end select
   end if

end function toml_raw_to_bool


!> Attempt to convert TOML raw value to TOML datetime
function toml_raw_to_timestamp(raw, timestamp) result(stat)

   !> Raw value to convert
   character(kind=tfc, len=*), intent(in) :: raw

   !> TOML datetime value
   type(toml_datetime), intent(out) :: timestamp

   !> Status of the evaluation
   logical :: stat

   integer :: err, dot_pos, first

   stat = toml_raw_verify_timestamp(raw)
   first = 1
   if (toml_raw_verify_date(raw)) then
      timestamp%date = toml_date()
      read(raw(1:4), *, iostat=err) timestamp%date%year
      stat = err == 0
      read(raw(6:7), *, iostat=err) timestamp%date%month
      stat = stat .and. err == 0
      read(raw(9:10), *, iostat=err) timestamp%date%day
      stat = stat .and. err == 0
      if (.not.stat .or. len(raw) == 10) return
      first = 12
   end if

   if (toml_raw_verify_time(raw(first:))) then
      timestamp%time = toml_time()
      read(raw(first:first+1), *, iostat=err) timestamp%time%hour
      stat = err == 0
      read(raw(first+3:first+4), *, iostat=err) timestamp%time%minute
      stat = stat .and. err == 0
      read(raw(first+6:first+7), *, iostat=err) timestamp%time%second
      stat = stat .and. err == 0
      if (len(raw(first:)) > 8) then
         dot_pos = index(raw, '.')
         if (dot_pos > 0) then
            allocate(timestamp%time%millisec, source=0)
            read(raw(dot_pos+1:dot_pos+3), *, iostat=err) timestamp%time%millisec
            stat = stat .and. err == 0
         end if
         dot_pos = verify(raw(first:), TOML_DIGITS//'.:') + first - 1
         if (dot_pos > first) timestamp%time%zone = raw(dot_pos:)
      end if
   end if

end function toml_raw_to_timestamp


logical function toml_raw_to_string(raw, str) result(stat)

   character(kind=tfc, len=*), intent(in) :: raw
   character(kind=tfc, len=:), allocatable, intent(out) :: str
   character(kind=tfc, len=:), allocatable :: tmp
   logical :: multiline
   logical :: verbatim

   stat = toml_raw_verify_string(raw)
   if (stat) then
      verbatim = raw(1:1) == TOML_SQUOTE
      multiline = verify(raw(1:3), TOML_DQUOTE) == 0 &
         &   .or. verify(raw(1:3), TOML_SQUOTE) == 0
      if (multiline) then
         tmp = raw(4:len(raw)-3)
         call toml_normalize_multiline(tmp)
      else
         tmp = raw(2:len(raw)-1)
      end if
      if (.not.verbatim) call toml_normalize_string(tmp)
      call move_alloc(tmp, str)
   end if

end function toml_raw_to_string


subroutine toml_normalize_multiline(str)

   character(kind=tfc, len=:), allocatable, intent(inout) :: str
   character(kind=tfc, len=:), allocatable :: tmp
   integer :: i, j, bsl

   ! if there are no newlines, we are done here
   if (scan(str, TOML_NEWLINE) == 0) return
   ! check for leading newlines
   i = verify(str, TOML_NEWLINE)
   ! for the case everything is newline, we will go with an empty string
   if (i == 0) then
      str = ''
      return
   end if
   tmp = ''
   do while (i < len(str))
      ! find next backslash character
      bsl = index(str(i:), '\') - 1
      if (bsl < 0) then
         tmp = tmp // str(i:)
         i = len(str)
      else
         j = verify(str(i+bsl+1:), TOML_WHITESPACE//TOML_NEWLINE) - 1
         if (j < 0) then
            tmp = tmp // str(i:i+bsl)
            i = len(str)
         else if (j == 0) then
            tmp = tmp // str(i:i+bsl)
            i = i + bsl + 1
         else
            tmp = tmp // str(i:i+bsl-1)
            i = i + bsl + j + 1
         end if
      end if
   end do
   call move_alloc(tmp, str)

end subroutine toml_normalize_multiline


subroutine toml_normalize_string(str)
   character(kind=tfc, len=:), allocatable, intent(inout) :: str
   character(kind=tfc, len=:), allocatable :: tmp
   character :: ch
   integer :: i, ii
   logical :: escape
   integer, parameter :: x00 = int(z'00'), x08 = int(z'08'), x0b = int(z'0B'), &
      & x1f = int(z'1f'), x7f = int(z'7f')

   escape = .false.
   tmp = ''
   do i = 1, len(str)
      ch = str(i:i)
      ii = ichar(ch)
      if (escape) then
         escape = .false.
         select case(ch)
         case('b'); tmp = tmp // TOML_BACKSPACE
         case('t'); tmp = tmp // TOML_TABULATOR
         case('n'); tmp = tmp // TOML_NEWLINE
         case('f'); tmp = tmp // TOML_FORMFEED
         case('r'); tmp = tmp // TOML_CARRIAGE_RETURN
         case('"', '\'); tmp = tmp // ch
         case('u'); tmp = tmp // '\u' ! FIXME
         case('U'); tmp = tmp // '\U' ! FIXME
         end select
      else
         if (ch == '\') then
            escape = .true.
         else
            ! check for illegal control characters
            if ((x00 <= ii .and. ii <= x08) .or. &
               &(x0B <= ii .and. ii <= x1f) .or. ii == x7f) return
            tmp = tmp // ch
         end if
      end if
   end do
   call move_alloc(tmp, str)

end subroutine toml_normalize_string


end module tomlf_utils_convert
 
 
!>>>>> ././src/fpm_filesystem.F90
!> This module contains general routines for interacting with the file system
!!
module fpm_filesystem
    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
    use fpm_environment, only: get_os_type, &
                               OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                               OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    use fpm_environment, only: separator, get_env, os_is_unix
    use fpm_strings, only: f_string, replace, string_t, split, notabs, str_begins_with_str
    use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer
    use fpm_error, only : fpm_stop
    implicit none
    private
    public :: basename, canon_path, dirname, is_dir, join_path, number_of_rows, list_files, env_variable, &
            mkdir, exists, get_temp_filename, windows_path, unix_path, getline, delete_file
    public :: fileopen, fileclose, filewrite, warnwrite, parent_dir
    public :: is_hidden_file
    public :: read_lines, read_lines_expanded
    public :: which, run, LINE_BUFFER_LEN
    public :: os_delete_dir

    integer, parameter :: LINE_BUFFER_LEN = 1000

#ifndef FPM_BOOTSTRAP
    interface
        function c_opendir(dir) result(r) bind(c, name="c_opendir")
            import c_char, c_ptr
            character(kind=c_char), intent(in) :: dir(*)
            type(c_ptr) :: r
        end function c_opendir

        function c_readdir(dir) result(r) bind(c, name="c_readdir")
            import c_ptr
            type(c_ptr), intent(in), value :: dir
            type(c_ptr) :: r
        end function c_readdir

        function c_closedir(dir) result(r) bind(c, name="closedir")
            import c_ptr, c_int
            type(c_ptr), intent(in), value :: dir
            integer(kind=c_int) :: r
        end function c_closedir

        function c_get_d_name(dir) result(r) bind(c, name="get_d_name")
            import c_ptr
            type(c_ptr), intent(in), value :: dir
            type(c_ptr) :: r
        end function c_get_d_name

        function c_is_dir(path) result(r) bind(c, name="c_is_dir")
            import c_char, c_int
            character(kind=c_char), intent(in) :: path(*)
            integer(kind=c_int) :: r
        end function c_is_dir
    end interface
#endif

contains


!> return value of environment variable
subroutine env_variable(var, name)
   character(len=:), allocatable, intent(out) :: var
   character(len=*), intent(in) :: name
   integer :: length, stat

   call get_environment_variable(name, length=length, status=stat)
   if (stat /= 0) return

   allocate(character(len=length) :: var)

   if (length > 0) then
      call get_environment_variable(name, var, status=stat)
      if (stat /= 0) then
         deallocate(var)
         return
      end if
   end if

end subroutine env_variable


!> Extract filename from path with/without suffix
function basename(path,suffix) result (base)

    character(*), intent(In) :: path
    logical, intent(in), optional :: suffix
    character(:), allocatable :: base

    character(:), allocatable :: file_parts(:)
    logical :: with_suffix

    if (.not.present(suffix)) then
        with_suffix = .true.
    else
        with_suffix = suffix
    end if

    call split(path,file_parts,delimiters='\/')
    if(size(file_parts)>0)then
       base = trim(file_parts(size(file_parts)))
    else
       base = ''
    endif
    if(.not.with_suffix)then
        call split(base,file_parts,delimiters='.')
        if(size(file_parts)>=2)then
           base = trim(file_parts(size(file_parts)-1))
        endif
    endif

end function basename


!> Canonicalize path for comparison
!! * Handles path string redundancies
!! * Does not test existence of path
!!
!! To be replaced by realpath/_fullname in stdlib_os
!!
!! FIXME: Lot's of ugly hacks following here
function canon_path(path)
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: canon_path
    character(len=:), allocatable :: nixpath

    integer :: istart, iend, nn, last
    logical :: is_path, absolute

    nixpath = unix_path(path)

    istart = 0
    nn = 0
    iend = 0
    absolute = nixpath(1:1) == "/"
    if (absolute) then
        canon_path = "/"
    else
        canon_path = ""
    end if

    do while(iend < len(nixpath))
        call next(nixpath, istart, iend, is_path)
        if (is_path) then
            select case(nixpath(istart:iend))
            case(".", "") ! always drop empty paths
            case("..")
                if (nn > 0) then
                    last = scan(canon_path(:len(canon_path)-1), "/", back=.true.)
                    canon_path = canon_path(:last)
                    nn = nn - 1
                else
                    if (.not. absolute) then
                        canon_path = canon_path // nixpath(istart:iend) // "/"
                    end if
                end if
            case default
                nn = nn + 1
                canon_path = canon_path // nixpath(istart:iend) // "/"
            end select
        end if
    end do

    if (len(canon_path) == 0) canon_path = "."
    if (len(canon_path) > 1 .and. canon_path(len(canon_path):) == "/") then
        canon_path = canon_path(:len(canon_path)-1)
    end if

contains

    subroutine next(string, istart, iend, is_path)
        character(len=*), intent(in) :: string
        integer, intent(inout) :: istart
        integer, intent(inout) :: iend
        logical, intent(inout) :: is_path

        integer :: ii, nn
        character :: tok

        nn = len(string)

        if (iend >= nn) then
            istart = nn
            iend = nn
            return
        end if

        ii = min(iend + 1, nn)
        tok = string(ii:ii)

        is_path = tok /= '/'

        if (.not.is_path) then
            is_path = .false.
            istart = ii
            iend = ii
            return
        end if

        istart = ii
        do ii = min(iend + 1, nn), nn
            tok = string(ii:ii)
            select case(tok)
            case('/')
                exit
            case default
                iend = ii
                cycle
            end select
        end do

    end subroutine next
end function canon_path


!> Extract dirname from path
function dirname(path) result (dir)
    character(*), intent(in) :: path
    character(:), allocatable :: dir

    dir = path(1:scan(path,'/\',back=.true.))

end function dirname

!> Extract dirname from path
function parent_dir(path) result (dir)
    character(*), intent(in) :: path
    character(:), allocatable :: dir

    dir = path(1:scan(path,'/\',back=.true.)-1)

end function parent_dir


!> test if a name matches an existing directory path
logical function is_dir(dir)
    character(*), intent(in) :: dir
    integer :: stat

    select case (get_os_type())

    case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
        call execute_command_line("test -d " // dir , exitstat=stat)

    case (OS_WINDOWS)
        call execute_command_line('cmd /c "if not exist ' // windows_path(dir) // '\ exit /B 1"', exitstat=stat)

    end select

    is_dir = (stat == 0)

end function is_dir

!> test if a file is hidden
logical function is_hidden_file(file_basename) result(r)
    character(*), intent(in) :: file_basename
    if (len(file_basename) <= 2) then
        r = .false.
    else
        r = str_begins_with_str(file_basename, '.')
    end if
end function is_hidden_file

!> Construct path by joining strings with os file separator
function join_path(a1,a2,a3,a4,a5) result(path)

    character(len=*), intent(in)           :: a1, a2
    character(len=*), intent(in), optional :: a3, a4, a5
    character(len=:), allocatable          :: path
    character(len=1)                       :: filesep
    logical, save                          :: has_cache = .false.
    character(len=1), save                 :: cache = '/'
    !$omp threadprivate(has_cache, cache)

    if (has_cache) then
        filesep = cache
    else
        select case (get_os_type())
            case default
                filesep = '/'
            case (OS_WINDOWS)
                filesep = '\'
        end select

        cache = filesep
        has_cache = .true.
    end if

    if (a1 == "") then
        path = a2
    else
        path = a1 // filesep // a2
    end if

    if (present(a3)) then
        path = path // filesep // a3
    else
        return
    end if

    if (present(a4)) then
        path = path // filesep // a4
    else
        return
    end if

    if (present(a5)) then
        path = path // filesep // a5
    else
        return
    end if

end function join_path


!> Determine number or rows in a file given a LUN
integer function number_of_rows(s) result(nrows)
    integer,intent(in)::s
    integer :: ios
    rewind(s)
    nrows = 0
    do
        read(s, *, iostat=ios)
        if (ios /= 0) exit
        nrows = nrows + 1
    end do
    rewind(s)
end function number_of_rows

!> read lines into an array of TYPE(STRING_T) variables expanding tabs
function read_lines_expanded(fh) result(lines)
    integer, intent(in) :: fh
    type(string_t), allocatable :: lines(:)

    integer :: i
    integer :: ilen
    character(LINE_BUFFER_LEN) :: line_buffer_read, line_buffer_expanded

    allocate(lines(number_of_rows(fh)))
    do i = 1, size(lines)
        read(fh, '(A)') line_buffer_read
        call notabs(line_buffer_read, line_buffer_expanded, ilen)
        lines(i)%s = trim(line_buffer_expanded)
    end do

end function read_lines_expanded

!> read lines into an array of TYPE(STRING_T) variables
function read_lines(fh) result(lines)
    integer, intent(in) :: fh
    type(string_t), allocatable :: lines(:)

    integer :: i
    character(LINE_BUFFER_LEN) :: line_buffer

    allocate(lines(number_of_rows(fh)))
    do i = 1, size(lines)
        read(fh, '(A)') line_buffer
        lines(i)%s = trim(line_buffer)
    end do

end function read_lines

!> Create a directory. Create subdirectories as needed
subroutine mkdir(dir, echo)
    character(len=*), intent(in) :: dir
    logical, intent(in), optional :: echo

    integer :: stat
    logical :: echo_local

    if(present(echo))then
        echo_local=echo
      else
        echo_local=.true.
    end if

    if (is_dir(dir)) return

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
            call execute_command_line('mkdir -p ' // dir, exitstat=stat)

            if (echo_local) then
                write (*, *) '+ mkdir -p ' // dir
            end if

        case (OS_WINDOWS)
            call execute_command_line("mkdir " // windows_path(dir), exitstat=stat)

            if (echo_local) then
                write (*, *) '+ mkdir ' // windows_path(dir)
            end if

    end select

    if (stat /= 0) then
        call fpm_stop(1, '*mkdir*:directory creation failed')
    end if
end subroutine mkdir

#ifndef FPM_BOOTSTRAP
!> Get file & directory names in directory `dir` using iso_c_binding.
!!
!!  - File/directory names return are relative to cwd, ie. preprended with `dir`
!!  - Includes files starting with `.` except current directory and parent directory
!!
recursive subroutine list_files(dir, files, recurse)
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    logical, intent(in), optional :: recurse

    integer :: i
    type(string_t), allocatable :: dir_files(:)
    type(string_t), allocatable :: sub_dir_files(:)

    type(c_ptr) :: dir_handle
    type(c_ptr) :: dir_entry_c
    character(len=:,kind=c_char), allocatable :: fortran_name
    character(len=:), allocatable :: string_fortran
    integer, parameter :: N_MAX = 256
    type(string_t) :: files_tmp(N_MAX)
    integer(kind=c_int) :: r

    if (c_is_dir(dir(1:len_trim(dir))//c_null_char) == 0) then
        allocate (files(0))
        return
    end if

    dir_handle = c_opendir(dir(1:len_trim(dir))//c_null_char)
    if (.not. c_associated(dir_handle)) then
        print *, 'c_opendir() failed'
        error stop
    end if

    i = 0
    allocate(files(0))

    do
        dir_entry_c = c_readdir(dir_handle)
        if (.not. c_associated(dir_entry_c)) then
            exit
        else
            string_fortran = f_string(c_get_d_name(dir_entry_c))

            if ((string_fortran == '.' .or. string_fortran == '..')) then
                cycle
            end if

            i = i + 1

            if (i > N_MAX) then
                files = [files, files_tmp]
                i = 1
            end if

            files_tmp(i)%s = join_path(dir, string_fortran)
        end if
    end do

    r = c_closedir(dir_handle)

    if (r /= 0) then
        print *, 'c_closedir() failed'
        error stop
    end if

    if (i > 0) then
        files = [files, files_tmp(1:i)]
    end if

    if (present(recurse)) then
        if (recurse) then

            allocate(sub_dir_files(0))

            do i=1,size(files)
                if (c_is_dir(files(i)%s//c_null_char) /= 0) then
                    call list_files(files(i)%s, dir_files, recurse=.true.)
                    sub_dir_files = [sub_dir_files, dir_files]
                end if
            end do

            files = [files, sub_dir_files]
        end if
    end if
end subroutine list_files

#else
!> Get file & directory names in directory `dir`.
!!
!!  - File/directory names return are relative to cwd, ie. preprended with `dir`
!!  - Includes files starting with `.` except current directory and parent directory
!!
recursive subroutine list_files(dir, files, recurse)
    character(len=*), intent(in) :: dir
    type(string_t), allocatable, intent(out) :: files(:)
    logical, intent(in), optional :: recurse

    integer :: stat, fh, i
    character(:), allocatable :: temp_file
    type(string_t), allocatable :: dir_files(:)
    type(string_t), allocatable :: sub_dir_files(:)

    if (.not. is_dir(dir)) then
        allocate (files(0))
        return
    end if

    allocate (temp_file, source=get_temp_filename())

    select case (get_os_type())
        case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
            call execute_command_line('ls -A ' // dir // ' > ' // temp_file, &
                                      exitstat=stat)
        case (OS_WINDOWS)
            call execute_command_line('dir /b ' // windows_path(dir) // ' > ' // temp_file, &
                                      exitstat=stat)
    end select

    if (stat /= 0) then
        call fpm_stop(2,'*list_files*:directory listing failed')
    end if

    open (newunit=fh, file=temp_file, status='old')
    files = read_lines(fh)
    close(fh,status="delete")

    do i=1,size(files)
        files(i)%s = join_path(dir,files(i)%s)
    end do

    if (present(recurse)) then
        if (recurse) then

            allocate(sub_dir_files(0))

            do i=1,size(files)
                if (is_dir(files(i)%s)) then

                    call list_files(files(i)%s, dir_files, recurse=.true.)
                    sub_dir_files = [sub_dir_files, dir_files]

                end if
            end do

            files = [files, sub_dir_files]

        end if
    end if

end subroutine list_files

#endif


!> test if pathname already exists
logical function exists(filename) result(r)
    character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function


!> Get a unused temporary filename
!!  Calls posix 'tempnam' - not recommended, but
!!   we have no security concerns for this application
!!   and use here is temporary.
!! Works with MinGW
function get_temp_filename() result(tempfile)
    !
    use iso_c_binding, only: c_ptr, C_NULL_PTR, c_f_pointer
    character(:), allocatable :: tempfile

    type(c_ptr) :: c_tempfile_ptr
    character(len=1), pointer :: c_tempfile(:)

    interface

        function c_tempnam(dir,pfx) result(tmp) bind(c,name="tempnam")
            import
            type(c_ptr), intent(in), value :: dir
            type(c_ptr), intent(in), value :: pfx
            type(c_ptr) :: tmp
        end function c_tempnam

        subroutine c_free(ptr) BIND(C,name="free")
            import
            type(c_ptr), value :: ptr
        end subroutine c_free

    end interface

    c_tempfile_ptr = c_tempnam(C_NULL_PTR, C_NULL_PTR)
    call c_f_pointer(c_tempfile_ptr,c_tempfile,[LINE_BUFFER_LEN])

    tempfile = f_string(c_tempfile)

    call c_free(c_tempfile_ptr)

end function get_temp_filename


!> Replace file system separators for windows
function windows_path(path) result(winpath)

    character(*), intent(in) :: path
    character(:), allocatable :: winpath

    integer :: idx

    winpath = path

    idx = index(winpath,'/')
    do while(idx > 0)
        winpath(idx:idx) = '\'
        idx = index(winpath,'/')
    end do

end function windows_path


!> Replace file system separators for unix
function unix_path(path) result(nixpath)

    character(*), intent(in) :: path
    character(:), allocatable :: nixpath

    integer :: idx

    nixpath = path

    idx = index(nixpath,'\')
    do while(idx > 0)
        nixpath(idx:idx) = '/'
        idx = index(nixpath,'\')
    end do

end function unix_path


!> read a line of arbitrary length into a CHARACTER variable from the specified LUN
subroutine getline(unit, line, iostat, iomsg)

    !> Formatted IO unit
    integer, intent(in) :: unit

    !> Line to read
    character(len=:), allocatable, intent(out) :: line

    !> Status of operation
    integer, intent(out) :: iostat

    !> Error message
    character(len=:), allocatable, optional :: iomsg

    character(len=LINE_BUFFER_LEN) :: buffer
    character(len=LINE_BUFFER_LEN) :: msg
    integer :: size
    integer :: stat

    allocate(character(len=0) :: line)
    do
        read(unit, '(a)', advance='no', iostat=stat, iomsg=msg, size=size) &
            & buffer
        if (stat > 0) exit
        line = line // buffer(:size)
        if (stat < 0) then
            if (is_iostat_eor(stat)) then
                stat = 0
            end if
            exit
        end if
    end do

    if (stat /= 0) then
        if (present(iomsg)) iomsg = trim(msg)
    end if
    iostat = stat

end subroutine getline


!> delete a file by filename
subroutine delete_file(file)
    character(len=*), intent(in) :: file
    logical :: exist
    integer :: unit
    inquire(file=file, exist=exist)
    if (exist) then
        open(file=file, newunit=unit)
        close(unit, status="delete")
    end if
end subroutine delete_file

!> write trimmed character data to a file if it does not exist
subroutine warnwrite(fname,data)
character(len=*),intent(in) :: fname
character(len=*),intent(in) :: data(:)

    if(.not.exists(fname))then
        call filewrite(fname,data)
    else
        write(stderr,'(*(g0,1x))')'<INFO>  ',fname,&
        & 'already exists. Not overwriting'
    endif

end subroutine warnwrite

!> procedure to open filename as a sequential "text" file
subroutine fileopen(filename,lun,ier)

character(len=*),intent(in)   :: filename
integer,intent(out)           :: lun
integer,intent(out),optional  :: ier
integer                       :: ios
character(len=256)            :: message

    message=' '
    ios=0
    if(filename/=' ')then
        open(file=filename, &
        & newunit=lun, &
        & form='formatted', &    ! FORM    = FORMATTED | UNFORMATTED
        & access='sequential', & ! ACCESS  = SEQUENTIAL| DIRECT | STREAM
        & action='write', &      ! ACTION  = READ|WRITE| READWRITE
        & position='rewind', &   ! POSITION= ASIS      | REWIND | APPEND
        & status='new', &        ! STATUS  = NEW| REPLACE| OLD| SCRATCH| UNKNOWN
        & iostat=ios, &
        & iomsg=message)
    else
        lun=stdout
        ios=0
    endif
    if(ios/=0)then
        lun=-1
        if(present(ier))then
           ier=ios
        else
           call fpm_stop(3,'*fileopen*:'//filename//':'//trim(message))
        endif
    endif

end subroutine fileopen

!> simple close of a LUN.  On error show message and stop (by default)
subroutine fileclose(lun,ier)
integer,intent(in)    :: lun
integer,intent(out),optional :: ier
character(len=256)    :: message
integer               :: ios
    if(lun/=-1)then
        close(unit=lun,iostat=ios,iomsg=message)
        if(ios/=0)then
            if(present(ier))then
               ier=ios
            else
               call fpm_stop(4,'*fileclose*:'//trim(message))
            endif
        endif
    endif
end subroutine fileclose

!> procedure to write filedata to file filename
subroutine filewrite(filename,filedata)

character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
integer                               :: lun, i, ios
character(len=256)                    :: message
    call fileopen(filename,lun)
    if(lun/=-1)then ! program currently stops on error on open, but might
                      ! want it to continue so -1 (unallowed LUN) indicates error
       ! write file
       do i=1,size(filedata)
           write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
           if(ios/=0)then
               call fpm_stop(5,'*filewrite*:'//filename//':'//trim(message))
           endif
       enddo
    endif
    ! close file
    call fileclose(lun)

end subroutine filewrite

function which(command) result(pathname)
!>
!!##NAME
!!     which(3f) - [M_io:ENVIRONMENT] given a command name find the pathname by searching
!!                 the directories in the environment variable $PATH
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function which(command) result(pathname)
!!
!!    character(len=*),intent(in)  :: command
!!    character(len=:),allocatable :: pathname
!!
!!##DESCRIPTION
!!    Given a command name find the first file with that name in the directories
!!    specified by the environment variable $PATH.
!!
!!##OPTIONS
!!    COMMAND   the command to search for
!!
!!##RETURNS
!!    PATHNAME  the first pathname found in the current user path. Returns blank
!!              if the command is not found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!   Checking the error message and counting lines:
!!
!!     program demo_which
!!     use M_io, only : which
!!     implicit none
!!        write(*,*)'ls is ',which('ls')
!!        write(*,*)'dir is ',which('dir')
!!        write(*,*)'install is ',which('install')
!!     end program demo_which
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

character(len=*),intent(in)     :: command
character(len=:),allocatable    :: pathname, checkon, paths(:), exts(:)
integer                         :: i, j
   pathname=''
   call split(get_env('PATH'),paths,delimiters=merge(';',':',separator()=='\'))
   SEARCH: do i=1,size(paths)
      checkon=trim(join_path(trim(paths(i)),command))
      select case(separator())
      case('/')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
      case('\')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
         if(exists(checkon//'.bat'))then
            pathname=checkon//'.bat'
            exit SEARCH
         endif
         if(exists(checkon//'.exe'))then
            pathname=checkon//'.exe'
            exit SEARCH
         endif
         call split(get_env('PATHEXT'),exts,delimiters=';')
         do j=1,size(exts)
            if(exists(checkon//'.'//trim(exts(j))))then
               pathname=checkon//'.'//trim(exts(j))
               exit SEARCH
            endif
         enddo
      end select
   enddo SEARCH
end function which

!> echo command string and pass it to the system for execution
subroutine run(cmd,echo,exitstat,verbose,redirect)
    character(len=*), intent(in) :: cmd
    logical,intent(in),optional  :: echo
    integer, intent(out),optional  :: exitstat
    logical, intent(in), optional :: verbose
    character(*), intent(in), optional :: redirect

    logical :: echo_local, verbose_local
    character(:), allocatable :: redirect_str
    character(:), allocatable :: line
    integer :: stat, fh, ios


    if(present(echo))then
       echo_local=echo
    else
       echo_local=.true.
    end if

    if(present(verbose))then
        verbose_local=verbose
    else
        verbose_local=.true.
    end if

    if (present(redirect)) then
        redirect_str =  ">"//redirect//" 2>&1"
    else
        if(verbose_local)then
            ! No redirection but verbose output
            redirect_str = ""
        else
            ! No redirection and non-verbose output
            if (os_is_unix()) then
                redirect_str = ">/dev/null 2>&1"
            else
                redirect_str = ">NUL 2>&1"
            end if
        end if
    end if

    if(echo_local) print *, '+ ', cmd

    call execute_command_line(cmd//redirect_str, exitstat=stat)

    if (verbose_local.and.present(redirect)) then

        open(newunit=fh,file=redirect,status='old')
        do
            call getline(fh, line, ios)
            if (ios /= 0) exit
            write(*,'(A)') trim(line)
        end do
        close(fh)

    end if

    if (present(exitstat)) then
        exitstat = stat
    else
        if (stat /= 0) then
            call fpm_stop(1,'*run*:Command failed')
        end if
    end if

end subroutine run

!> Delete directory using system OS remove directory commands
subroutine os_delete_dir(unix, dir, echo)
    logical, intent(in) :: unix
    character(len=*), intent(in) :: dir
    logical, intent(in), optional :: echo

    logical :: echo_local

    if(present(echo))then
        echo_local=echo
      else
        echo_local=.true.
    end if

    if (unix) then
        call run('rm -rf ' // dir, .false.)

        if (echo_local) then
          write (*, *) '+ rm -rf ' // dir
        end if

    else
        call run('rmdir /s/q ' // dir, .false.)

        if (echo_local) then
          write (*, *) '+ rmdir /s/q ' // dir
        end if

    end if

end subroutine os_delete_dir

end module fpm_filesystem
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/utils.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module tomlf_utils
   use tomlf_constants
   use tomlf_datetime, only : toml_datetime, toml_date, toml_time
   use tomlf_utils_convert
   use tomlf_utils_verify
   implicit none
   private

   public :: convert_raw
   public :: toml_raw_to_string, toml_raw_to_float, toml_raw_to_bool
   public :: toml_raw_to_integer, toml_raw_to_timestamp
   public :: toml_raw_verify_string, toml_raw_verify_float, toml_raw_verify_bool
   public :: toml_raw_verify_integer, toml_raw_verify_timestamp
   public :: toml_raw_verify_date, toml_raw_verify_time
   public :: toml_escape_string, toml_get_value_type


contains


!> Determine TOML value type
function toml_get_value_type(raw) result(vtype)

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw

   !> Value type
   integer :: vtype

   if (toml_raw_verify_string(raw)) then
      vtype = toml_type%string
      return
   end if
   if (toml_raw_verify_bool(raw)) then
      vtype = toml_type%boolean
      return
   end if
   if (toml_raw_verify_integer(raw)) then
      vtype = toml_type%int
      return
   end if
   if (toml_raw_verify_float(raw)) then
      vtype = toml_type%float
      return
   end if
   if (toml_raw_verify_timestamp(raw)) then
      vtype = toml_type%datetime
      return
   end if
   vtype = toml_type%invalid

end function


!> Escape all special characters in a TOML string
subroutine toml_escape_string(raw, escaped, multiline)

   !> Raw representation of TOML string
   character(kind=tfc, len=*), intent(in) :: raw

   !> Escaped view of the TOML string
   character(kind=tfc, len=:), allocatable, intent(out) :: escaped

   !> Preserve newline characters
   logical, intent(in), optional :: multiline

   integer :: i
   logical :: preserve_newline

   preserve_newline = .false.
   if (present(multiline)) preserve_newline = multiline

   escaped = '"'
   do i = 1, len(raw)
      select case(raw(i:i))
      case default; escaped = escaped // raw(i:i)
      case('\'); escaped = escaped // '\\'
      case('"'); escaped = escaped // '\"'
      case(TOML_NEWLINE)
         if (preserve_newline) then
            escaped = escaped // raw(i:i)
         else
            escaped = escaped // '\n'
         end if
      case(TOML_FORMFEED); escaped = escaped // '\f'
      case(TOML_CARRIAGE_RETURN); escaped = escaped // '\r'
      case(TOML_TABULATOR); escaped = escaped // '\t'
      case(TOML_BACKSPACE); escaped = escaped // '\b'
      end select
   end do
   escaped = escaped // '"'

end subroutine toml_escape_string


end module tomlf_utils
 
 
!>>>>> ././src/fpm_command_line.f90
!># Definition of the command line interface
!>
!> This module uses [M_CLI2](https://github.com/urbanjost/M_CLI2) to define
!> the command line interface.
!> To define a command line interface create a new command settings type
!> from the [[fpm_cmd_settings]] base class or the respective parent command
!> settings.
!>
!> The subcommand is selected by the first non-option argument in the command
!> line. In the subcase block the actual command line is defined and transferred
!> to an instance of the [[fpm_cmd_settings]], the actual type is used by the
!> *fpm* main program to determine which command entry point is chosen.
!>
!> To add a new subcommand add a new case to select construct and specify the
!> wanted command line and the expected default values.
!> Some of the following points also apply if you add a new option or argument
!> to an existing *fpm* subcommand.
!> At this point you should create a help page for the new command in a simple
!> catman-like format as well in the ``set_help`` procedure.
!> Make sure to register new subcommands in the ``fpm-manual`` command by adding
!> them to the manual character array and in the help/manual case as well.
!> You should add the new command to the synopsis section of the ``fpm-list``,
!> ``fpm-help`` and ``fpm --list`` help pages below to make sure the help output
!> is complete and consistent as well.
module fpm_command_line
use fpm_environment,  only : get_os_type, get_env, os_is_unix, &
                             OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
use MM_CLI2,           only : set_args, lget, sget, unnamed, remaining, specified
use MM_CLI2,           only : get_subcommand, CLI_RESPONSE_FILE
use fpm_strings,      only : lower, split, to_fortran_name, is_fortran_name
use fpm_filesystem,   only : basename, canon_path, which, run
use fpm_environment,  only : get_command_arguments_quoted
use fpm_error,        only : fpm_stop, error_t
use fpm_os,           only : get_current_directory
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit

implicit none

private
public :: fpm_cmd_settings, &
          fpm_build_settings, &
          fpm_install_settings, &
          fpm_new_settings, &
          fpm_run_settings, &
          fpm_test_settings, &
          fpm_update_settings, &
          fpm_clean_settings, &
          get_command_line_settings

type, abstract :: fpm_cmd_settings
    character(len=:), allocatable :: working_dir
    logical                      :: verbose=.true.
end type

integer,parameter :: ibug=4096
type, extends(fpm_cmd_settings)  :: fpm_new_settings
    character(len=:),allocatable :: name
    logical                      :: with_executable=.false.
    logical                      :: with_test=.false.
    logical                      :: with_lib=.true.
    logical                      :: with_example=.false.
    logical                      :: with_full=.false.
    logical                      :: with_bare=.false.
    logical                      :: backfill=.true.
end type

type, extends(fpm_cmd_settings)  :: fpm_build_settings
    logical                      :: list=.false.
    logical                      :: show_model=.false.
    logical                      :: build_tests=.false.
    logical                      :: prune=.true.
    character(len=:),allocatable :: compiler
    character(len=:),allocatable :: c_compiler
    character(len=:),allocatable :: cxx_compiler
    character(len=:),allocatable :: archiver
    character(len=:),allocatable :: profile
    character(len=:),allocatable :: flag
    character(len=:),allocatable :: cflag
    character(len=:),allocatable :: cxxflag
    character(len=:),allocatable :: ldflag
end type

type, extends(fpm_build_settings)  :: fpm_run_settings
    character(len=ibug),allocatable :: name(:)
    character(len=:),allocatable :: args
    character(len=:),allocatable :: runner
    logical :: example
end type

type, extends(fpm_run_settings)  :: fpm_test_settings
end type

type, extends(fpm_build_settings) :: fpm_install_settings
    character(len=:), allocatable :: prefix
    character(len=:), allocatable :: bindir
    character(len=:), allocatable :: libdir
    character(len=:), allocatable :: includedir
    logical :: no_rebuild
end type

!> Settings for interacting and updating with project dependencies
type, extends(fpm_cmd_settings)  :: fpm_update_settings
    character(len=ibug),allocatable :: name(:)
    logical :: fetch_only
    logical :: clean
end type

type, extends(fpm_cmd_settings)   :: fpm_clean_settings
    logical                       :: unix
    character(len=:), allocatable :: calling_dir  ! directory clean called from
    logical                       :: clean_skip=.false.
    logical                       :: clean_call=.false.
end type

character(len=:),allocatable :: name
character(len=:),allocatable :: os_type
character(len=ibug),allocatable :: names(:)
character(len=:),allocatable :: tnames(:)

character(len=:), allocatable :: version_text(:)
character(len=:), allocatable :: help_new(:), help_fpm(:), help_run(:), &
                 & help_test(:), help_build(:), help_usage(:), help_runner(:), &
                 & help_text(:), help_install(:), help_help(:), help_update(:), &
                 & help_list(:), help_list_dash(:), help_list_nodash(:), &
                 & help_clean(:)
character(len=20),parameter :: manual(*)=[ character(len=20) ::&
&  ' ',     'fpm',    'new',     'build',  'run',    'clean',  &
&  'test',  'runner', 'install', 'update', 'list',   'help',   'version'  ]

character(len=:), allocatable :: val_runner, val_compiler, val_flag, val_cflag, val_cxxflag, val_ldflag, &
    val_profile

!   '12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
character(len=80), parameter :: help_text_build_common(*) = [character(len=80) ::      &
    ' --profile PROF    Selects the compilation profile for the build.               ',&
    '                   Currently available profiles are "release" for               ',&
    '                   high optimization and "debug" for full debug options.        ',&
    '                   If --flag is not specified the "debug" flags are the         ',&
    '                   default.                                                     ',&
    ' --no-prune        Disable tree-shaking/pruning of unused module dependencies   '&
    ]
!   '12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
character(len=80), parameter :: help_text_compiler(*) = [character(len=80) :: &
    ' --compiler NAME    Specify a compiler name. The default is "gfortran"          ',&
    '                    unless set by the environment variable FPM_FC.              ',&
    ' --c-compiler NAME  Specify the C compiler name. Automatically determined by    ',&
    '                    default unless set by the environment variable FPM_CC.      ',&
    ' --cxx-compiler NAME  Specify the C++ compiler name. Automatically determined by',&
    '                    default unless set by the environment variable FPM_CXX.     ',&
    ' --archiver NAME    Specify the archiver name. Automatically determined by      ',&
    '                    default unless set by the environment variable FPM_AR.      '&
    ]

!   '12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
character(len=80), parameter :: help_text_flag(*) = [character(len=80) :: &
    ' --flag  FFLAGS    selects compile arguments for the build, the default value is',&
    '                   set by the FPM_FFLAGS environment variable. These are added  ',&
    '                   to the profile options if --profile is specified, else these ',&
    '                   these options override the defaults. Note object and .mod    ',&
    '                   directory locations are always built in.                     ',&
    ' --c-flag CFLAGS   selects compile arguments specific for C source in the build.',&
    '                   The default value is set by the FPM_CFLAGS environment       ',&
    '                   variable.                                                    ',&
    ' --cxx-flag CFLAGS selects compile arguments specific for C++ source in the     ',&
    '                   build. The default value is set by the FPM_CXXFLAGS          ',&
    '                   environment variable.                                        ',&
    ' --link-flag LDFLAGS  select arguments passed to the linker for the build. The  ',&
    '                   default value is set by the FPM_LDFLAGS environment variable.'&
    ]


character(len=80), parameter :: help_text_environment(*) = [character(len=80) :: &
    'ENVIRONMENT VARIABLES',&
    ' FPM_FC            sets the path to the Fortran compiler used for the build,', &
    '                   will be overwritten by --compiler command line option', &
    '', &
    ' FPM_FFLAGS        sets the arguments for the Fortran compiler', &
    '                   will be overwritten by --flag command line option', &
    '', &
    ' FPM_CC            sets the path to the C compiler used for the build,', &
    '                   will be overwritten by --c-compiler command line option', &
    '', &
    ' FPM_CFLAGS        sets the arguments for the C compiler', &
    '                   will be overwritten by --c-flag command line option', &
    '', &
    ' FPM_CXX           sets the path to the C++ compiler used for the build,', &
    '                   will be overwritten by --cxx-compiler command line option', &
    '', &
    ' FPM_CXXFLAGS      sets the arguments for the C++ compiler', &
    '                   will be overwritten by --cxx-flag command line option', &
    '', &
    ' FPM_AR            sets the path to the archiver used for the build,', &
    '                   will be overwritten by --archiver command line option', &
    '', &
    ' FPM_LDFLAGS       sets additional link arguments for creating executables', &
    '                   will be overwritten by --link-flag command line option' &
    ]

contains
    subroutine get_command_line_settings(cmd_settings)
        class(fpm_cmd_settings), allocatable, intent(out) :: cmd_settings

        integer, parameter            :: widest = 256
        character(len=4096)           :: cmdarg
        integer                       :: i
        integer                       :: os
        logical                       :: unix
        type(fpm_install_settings), allocatable :: install_settings
        character(len=:), allocatable :: common_args, compiler_args, run_args, working_dir, &
            & c_compiler, cxx_compiler, archiver

        character(len=*), parameter :: fc_env = "FC", cc_env = "CC", ar_env = "AR", &
            & fflags_env = "FFLAGS", cflags_env = "CFLAGS", cxxflags_env = "CXXFLAGS", ldflags_env = "LDFLAGS", &
            & fc_default = "gfortran", cc_default = " ", ar_default = " ", flags_default = " ", &
            & cxx_env = "CXX", cxx_default = " "
        type(error_t), allocatable :: error

        call set_help()
        os = get_os_type()
        ! text for --version switch,
        select case (os)
            case (OS_LINUX);   os_type =  "OS Type:     Linux"
            case (OS_MACOS);   os_type =  "OS Type:     macOS"
            case (OS_WINDOWS); os_type =  "OS Type:     Windows"
            case (OS_CYGWIN);  os_type =  "OS Type:     Cygwin"
            case (OS_SOLARIS); os_type =  "OS Type:     Solaris"
            case (OS_FREEBSD); os_type =  "OS Type:     FreeBSD"
            case (OS_OPENBSD); os_type =  "OS Type:     OpenBSD"
            case (OS_UNKNOWN); os_type =  "OS Type:     Unknown"
            case default     ; os_type =  "OS Type:     UNKNOWN"
        end select
        unix = os_is_unix(os)
        version_text = [character(len=80) :: &
         &  'Version:     0.7.0, alpha',                               &
         &  'Program:     fpm(1)',                                     &
         &  'Description: A Fortran package manager and build system', &
         &  'Home Page:   https://github.com/fortran-lang/fpm',        &
         &  'License:     MIT',                                        &
         &  os_type]
        ! find the subcommand name by looking for first word on command
        ! not starting with dash
        CLI_RESPONSE_FILE=.true.
        cmdarg = get_subcommand()

        common_args = &
          ' --directory:C " "' // &
          ' --verbose F'

        run_args = &
          ' --target " "' // &
          ' --list F' // &
          ' --runner " "'

        compiler_args = &
          ' --profile " "' // &
          ' --no-prune F' // &
          ' --compiler "'//get_fpm_env(fc_env, fc_default)//'"' // &
          ' --c-compiler "'//get_fpm_env(cc_env, cc_default)//'"' // &
          ' --cxx-compiler "'//get_fpm_env(cxx_env, cxx_default)//'"' // &
          ' --archiver "'//get_fpm_env(ar_env, ar_default)//'"' // &
          ' --flag:: "'//get_fpm_env(fflags_env, flags_default)//'"' // &
          ' --c-flag:: "'//get_fpm_env(cflags_env, flags_default)//'"' // &
          ' --cxx-flag:: "'//get_fpm_env(cxxflags_env, flags_default)//'"' // &
          ' --link-flag:: "'//get_fpm_env(ldflags_env, flags_default)//'"'

        ! now set subcommand-specific help text and process commandline
        ! arguments. Then call subcommand routine
        select case(trim(cmdarg))

        case('run')
            call set_args(common_args // compiler_args // run_args //'&
            & --all F &
            & --example F&
            & --',help_run,version_text)

            call check_build_vals()

            if( size(unnamed) > 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif


            if(specified('target') )then
               call split(sget('target'),tnames,delimiters=' ,:')
               names=[character(len=max(len(names),len(tnames))) :: names,tnames]
            endif

            ! convert --all to '*'
            if(lget('all'))then
               names=[character(len=max(len(names),1)) :: names,'*' ]
            endif

            ! convert special string '..' to equivalent (shorter) '*'
            ! to allow for a string that does not require shift-key and quoting
            do i=1,size(names)
               if(names(i)=='..')names(i)='*'
            enddo

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(fpm_run_settings :: cmd_settings)
            val_runner=sget('runner')
            if(specified('runner') .and. val_runner=='')val_runner='echo'
            cmd_settings=fpm_run_settings(&
            & args=remaining,&
            & profile=val_profile,&
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & example=lget('example'), &
            & list=lget('list'),&
            & build_tests=.false.,&
            & name=names,&
            & runner=val_runner,&
            & verbose=lget('verbose') )

        case('build')
            call set_args(common_args // compiler_args //'&
            & --list F &
            & --show-model F &
            & --tests F &
            & --',help_build,version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate( fpm_build_settings :: cmd_settings )
            cmd_settings=fpm_build_settings(  &
            & profile=val_profile,&
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & list=lget('list'),&
            & show_model=lget('show-model'),&
            & build_tests=lget('tests'),&
            & verbose=lget('verbose') )

        case('new')
            call set_args(common_args // '&
            & --src F &
            & --lib F &
            & --app F &
            & --test F &
            & --example F &
            & --backfill F &
            & --full F &
            & --bare F', &
            & help_new, version_text)
            select case(size(unnamed))
            case(1)
                if(lget('backfill'))then
                   name='.'
                else
                   write(stderr,'(*(7x,g0,/))') &
                   & '<USAGE> fpm new NAME [[--lib|--src] [--app] [--test] [--example]]|[--full|--bare] [--backfill]'
                   call fpm_stop(1,'directory name required')
                endif
            case(2)
                name=trim(unnamed(2))
            case default
                write(stderr,'(7x,g0)') &
                & '<USAGE> fpm new NAME [[--lib|--src] [--app] [--test] [--example]]| [--full|--bare] [--backfill]'
                call fpm_stop(2,'only one directory name allowed')
            end select
            !*! canon_path is not converting ".", etc.
            if(name=='.')then
               call get_current_directory(name, error)
               if (allocated(error)) then
                  write(stderr, '("[Error]", 1x, a)') error%message
                  stop 1
               endif
            endif
            name=canon_path(name)
            if( .not.is_fortran_name(to_fortran_name(basename(name))) )then
                write(stderr,'(g0)') [ character(len=72) :: &
                & '<ERROR> the fpm project name must be made of up to 63 ASCII letters,', &
                & '        numbers, underscores, or hyphens, and start with a letter.']
                call fpm_stop(4,' ')
            endif


            allocate(fpm_new_settings :: cmd_settings)
            if (any( specified([character(len=10) :: 'src','lib','app','test','example','bare'])) &
            & .and.lget('full') )then
                write(stderr,'(*(a))')&
                &'<ERROR> --full and any of [--src|--lib,--app,--test,--example,--bare]', &
                &'        are mutually exclusive.'
                call fpm_stop(5,' ')
            elseif (any( specified([character(len=10) :: 'src','lib','app','test','example','full'])) &
            & .and.lget('bare') )then
                write(stderr,'(*(a))')&
                &'<ERROR> --bare and any of [--src|--lib,--app,--test,--example,--full]', &
                &'        are mutually exclusive.'
                call fpm_stop(3,' ')
            elseif (any( specified([character(len=10) :: 'src','lib','app','test','example']) ) )then
                cmd_settings=fpm_new_settings(&
                 & backfill=lget('backfill'),               &
                 & name=name,                               &
                 & with_executable=lget('app'),             &
                 & with_lib=any([lget('lib'),lget('src')]), &
                 & with_test=lget('test'),                  &
                 & with_example=lget('example'),            &
                 & verbose=lget('verbose') )
            else  ! default if no specific directories are requested
                cmd_settings=fpm_new_settings(&
                 & backfill=lget('backfill') ,           &
                 & name=name,                            &
                 & with_executable=.true.,               &
                 & with_lib=.true.,                      &
                 & with_test=.true.,                     &
                 & with_example=lget('full'),            &
                 & with_full=lget('full'),               &
                 & with_bare=lget('bare'),               &
                 & verbose=lget('verbose') )
            endif

        case('help','manual')
            call set_args(common_args, help_help,version_text)
            if(size(unnamed)<2)then
                if(unnamed(1)=='help')then
                   unnamed=['   ', 'fpm']
                else
                   unnamed=manual
                endif
            elseif(unnamed(2)=='manual')then
                unnamed=manual
            endif
            allocate(character(len=widest) :: help_text(0))
            do i=2,size(unnamed)
                select case(unnamed(i))
                case('       ' )
                case('fpm    ' )
                   help_text=[character(len=widest) :: help_text, help_fpm]
                case('new    ' )
                   help_text=[character(len=widest) :: help_text, help_new]
                case('build  ' )
                   help_text=[character(len=widest) :: help_text, help_build]
                case('install' )
                   help_text=[character(len=widest) :: help_text, help_install]
                case('run    ' )
                   help_text=[character(len=widest) :: help_text, help_run]
                case('test   ' )
                   help_text=[character(len=widest) :: help_text, help_test]
                case('runner' )
                   help_text=[character(len=widest) :: help_text, help_runner]
                case('list   ' )
                   help_text=[character(len=widest) :: help_text, help_list]
                case('update ' )
                   help_text=[character(len=widest) :: help_text, help_update]
                case('help   ' )
                   help_text=[character(len=widest) :: help_text, help_help]
                case('version' )
                   help_text=[character(len=widest) :: help_text, version_text]
                case('clean' )
                   help_text=[character(len=widest) :: help_text, help_clean]
                case default
                   help_text=[character(len=widest) :: help_text, &
                   & '<ERROR> unknown help topic "'//trim(unnamed(i))//'"']
                   !!& '<ERROR> unknown help topic "'//trim(unnamed(i)).'not found in:',manual]
                end select
            enddo
            call printhelp(help_text)

        case('install')
            call set_args(common_args // compiler_args // '&
                & --no-rebuild F --prefix " " &
                & --list F &
                & --libdir "lib" --bindir "bin" --includedir "include"', &
                help_install, version_text)

            call check_build_vals()

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(install_settings, source=fpm_install_settings(&
                list=lget('list'), &
                profile=val_profile,&
                prune=.not.lget('no-prune'), &
                compiler=val_compiler, &
                c_compiler=c_compiler, &
                cxx_compiler=cxx_compiler, &
                archiver=archiver, &
                flag=val_flag, &
                cflag=val_cflag, &
                cxxflag=val_cxxflag, &
                ldflag=val_ldflag, &
                no_rebuild=lget('no-rebuild'), &
                verbose=lget('verbose')))
            call get_char_arg(install_settings%prefix, 'prefix')
            call get_char_arg(install_settings%libdir, 'libdir')
            call get_char_arg(install_settings%bindir, 'bindir')
            call get_char_arg(install_settings%includedir, 'includedir')
            call move_alloc(install_settings, cmd_settings)

        case('list')
            call set_args(common_args // '&
            & --list F&
            &', help_list, version_text)
            if(lget('list'))then
                help_text = [character(widest) :: help_list_nodash, help_list_dash]
            else
                help_text = help_list_nodash
            endif
            call printhelp(help_text)

        case('test')
            call set_args(common_args // compiler_args // run_args // ' --', &
              help_test,version_text)

            call check_build_vals()

            if( size(unnamed) > 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            if(specified('target') )then
               call split(sget('target'),tnames,delimiters=' ,:')
               names=[character(len=max(len(names),len(tnames))) :: names,tnames]
            endif

            ! convert special string '..' to equivalent (shorter) '*'
            ! to allow for a string that does not require shift-key and quoting
            do i=1,size(names)
               if(names(i)=='..')names(i)='*'
            enddo

            c_compiler = sget('c-compiler')
            cxx_compiler = sget('cxx-compiler')
            archiver = sget('archiver')
            allocate(fpm_test_settings :: cmd_settings)
            val_runner=sget('runner')
            if(specified('runner') .and. val_runner=='')val_runner='echo'
            cmd_settings=fpm_test_settings(&
            & args=remaining, &
            & profile=val_profile, &
            & prune=.not.lget('no-prune'), &
            & compiler=val_compiler, &
            & c_compiler=c_compiler, &
            & cxx_compiler=cxx_compiler, &
            & archiver=archiver, &
            & flag=val_flag, &
            & cflag=val_cflag, &
            & cxxflag=val_cxxflag, &
            & ldflag=val_ldflag, &
            & example=.false., &
            & list=lget('list'), &
            & build_tests=.true., &
            & name=names, &
            & runner=val_runner, &
            & verbose=lget('verbose') )

        case('update')
            call set_args(common_args // ' --fetch-only F --clean F', &
                help_update, version_text)

            if( size(unnamed) > 1 )then
                names=unnamed(2:)
            else
                names=[character(len=len(names)) :: ]
            endif

            allocate(fpm_update_settings :: cmd_settings)
            cmd_settings=fpm_update_settings(name=names, &
                fetch_only=lget('fetch-only'), verbose=lget('verbose'), &
                clean=lget('clean'))

        case('clean')
            call set_args(common_args // &
            &   ' --skip'             // &
            &   ' --all',                &
                help_clean, version_text)
            allocate(fpm_clean_settings :: cmd_settings)
            call get_current_directory(working_dir, error)
            cmd_settings=fpm_clean_settings( &
            &   unix=unix,                   &
            &   calling_dir=working_dir,     &
            &   clean_skip=lget('skip'),     &
                clean_call=lget('all'))

        case default

            if(cmdarg.ne.''.and.which('fpm-'//cmdarg).ne.'')then
                call run('fpm-'//trim(cmdarg)//' '// get_command_arguments_quoted(),.false.)
                stop
            else
                call set_args('&
                & --list F&
                &', help_fpm, version_text)
                ! Note: will not get here if --version or --usage or --help
                ! is present on commandline
                if(lget('list'))then
                    help_text = help_list_dash
                elseif(len_trim(cmdarg)==0)then
                    write(stdout,'(*(a))')'Fortran Package Manager:'
                    write(stdout,'(*(a))')' '
                    help_text = [character(widest) :: help_list_nodash, help_usage]
                else
                    write(stderr,'(*(a))')'<ERROR> unknown subcommand [', &
                     & trim(cmdarg), ']'
                    help_text = [character(widest) :: help_list_dash, help_usage]
                endif
                call printhelp(help_text)
            endif

        end select

        if (allocated(cmd_settings)) then
            working_dir = sget("directory")
            call move_alloc(working_dir, cmd_settings%working_dir)
        end if

    contains

    subroutine check_build_vals()
        character(len=:), allocatable :: flags

        val_compiler=sget('compiler')
        if(val_compiler=='') then
            val_compiler='gfortran'
        endif

        val_flag = " " // sget('flag')
        val_cflag = " " // sget('c-flag')
        val_cxxflag = " "// sget('cxx-flag')
        val_ldflag = " " // sget('link-flag')
        val_profile = sget('profile')

    end subroutine check_build_vals

    !> Print help text and stop
    subroutine printhelp(lines)
    character(len=:),intent(in),allocatable :: lines(:)
    integer :: iii,ii
        if(allocated(lines))then
           ii=size(lines)
           if(ii > 0 .and. len(lines)> 0) then
               write(stdout,'(g0)')(trim(lines(iii)), iii=1, ii)
           else
               write(stdout,'(a)')'<WARNING> *printhelp* output requested is empty'
           endif
        endif
        stop
    end subroutine printhelp

    end subroutine get_command_line_settings

    subroutine set_help()
   help_list_nodash=[character(len=80) :: &
   'USAGE: fpm [ SUBCOMMAND [SUBCOMMAND_OPTIONS] ]|[--list|--help|--version]', &
   '       where SUBCOMMAND is commonly new|build|run|test                  ', &
   '                                                                        ', &
   ' subcommand may be one of                                               ', &
   '                                                                        ', &
   '  build     Compile the package placing results in the "build" directory', &
   '  help      Display help                                                ', &
   '  list      Display this list of subcommand descriptions                ', &
   '  new       Create a new Fortran package directory with sample files    ', &
   '  run       Run the local package application programs                  ', &
   '  test      Run the test programs                                       ', &
   '  update    Update and manage project dependencies                      ', &
   '  install   Install project                                             ', &
   '  clean     Delete the build                                            ', &
   '                                                                        ', &
   ' Enter "fpm --list" for a brief list of subcommand options. Enter       ', &
   ' "fpm --help" or "fpm SUBCOMMAND --help" for detailed descriptions.     ', &
   ' ']
   help_list_dash = [character(len=80) :: &
   '                                                                                ', &
   ' build [--compiler COMPILER_NAME] [--profile PROF] [--flag FFLAGS] [--list]     ', &
   '       [--tests] [--no-prune]                                                   ', &
   ' help [NAME(s)]                                                                 ', &
   ' new NAME [[--lib|--src] [--app] [--test] [--example]]|                         ', &
   '          [--full|--bare][--backfill]                                           ', &
   ' update [NAME(s)] [--fetch-only] [--clean] [--verbose]                          ', &
   ' list [--list]                                                                  ', &
   ' run  [[--target] NAME(s) [--example] [--profile PROF] [--flag FFLAGS] [--all]  ', &
   '      [--runner "CMD"] [--compiler COMPILER_NAME] [--list] [-- ARGS]            ', &
   ' test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--runner "CMD"]    ', &
   '      [--list] [--compiler COMPILER_NAME] [-- ARGS]                             ', &
   ' install [--profile PROF] [--flag FFLAGS] [--no-rebuild] [--prefix PATH]        ', &
   '         [options]                                                              ', &
   ' clean [--skip] [--all]                                                         ', &
   ' ']
    help_usage=[character(len=80) :: &
    '' ]
    help_runner=[character(len=80) :: &
   'NAME                                                                            ', &
   '   --runner(1) - a shared option for specifying an application to launch        ', &
   '                 executables.                                                   ', &
   '                                                                                ', &
   'SYNOPSIS                                                                        ', &
   '   fpm run|test --runner CMD ... -- SUFFIX_OPTIONS                              ', &
   '                                                                                ', &
   'DESCRIPTION                                                                     ', &
   '   The --runner option allows specifying a program to launch                    ', &
   '   executables selected via the fpm(1) subcommands "run" and "test". This       ', &
   '   gives easy recourse to utilities such as debuggers and other tools           ', &
   '   that wrap other executables.                                                 ', &
   '                                                                                ', &
   '   These external commands are not part of fpm(1) itself as they vary           ', &
   '   from platform to platform or require independent installation.               ', &
   '                                                                                ', &
   'OPTION                                                                          ', &
   ' --runner ''CMD''  quoted command used to launch the fpm(1) executables.          ', &
   '               Available for both the "run" and "test" subcommands.             ', &
   '               If the keyword is specified without a value the default command  ', &
   '               is "echo".                                                       ', &
   ' -- SUFFIX_OPTIONS  additional options to suffix the command CMD and executable ', &
   '                    file names with.                                            ', &
   'EXAMPLES                                                                        ', &
   '   Use cases for ''fpm run|test --runner "CMD"'' include employing                ', &
   '   the following common GNU/Linux and Unix commands:                            ', &
   '                                                                                ', &
   ' INTERROGATE                                                                    ', &
   '    + nm - list symbols from object files                                       ', &
   '    + size - list section sizes and total size.                                 ', &
   '    + ldd - print shared object dependencies                                    ', &
   '    + ls - list directory contents                                              ', &
   '    + stat - display file or file system status                                 ', &
   '    + file - determine file type                                                ', &
   ' PERFORMANCE AND DEBUGGING                                                      ', &
   '    + gdb - The GNU Debugger                                                    ', &
   '    + valgrind - a suite of tools for debugging and profiling                   ', &
   '    + time - time a simple command or give resource usage                       ', &
   '    + timeout - run a command with a time limit                                 ', &
   ' COPY                                                                           ', &
   '    + install - copy files and set attributes                                   ', &
   '    + tar - an archiving utility                                                ', &
   ' ALTER                                                                          ', &
   '    + rm - remove files or directories                                          ', &
   '    + chmod - change permissions of a file                                      ', &
   '    + strip - remove unnecessary information from strippable files              ', &
   '                                                                                ', &
   ' For example                                                                    ', &
   '                                                                                ', &
   '  fpm test --runner gdb                                                         ', &
   '  fpm run --runner "tar cvfz $HOME/bundle.tgz"                                  ', &
   '  fpm run --runner ldd                                                          ', &
   '  fpm run --runner strip                                                        ', &
   '  fpm run --runner ''cp -t /usr/local/bin''                                       ', &
   '                                                                                ', &
   '  # options after executable name can be specified after the -- option          ', &
   '  fpm --runner cp run -- /usr/local/bin/                                        ', &
   '  # generates commands of the form "cp $FILENAME /usr/local/bin/"               ', &
   '                                                                                ', &
   '  # bash(1) alias example:                                                      ', &
   '  alias fpm-install=\                                                           ', &
   '  "fpm run --profile release --runner ''install -vbp -m 0711 -t ~/.local/bin''" ', &
   '  fpm-install                                                           ', &
    '' ]
    help_fpm=[character(len=80) :: &
    'NAME                                                                   ', &
    '   fpm(1) - A Fortran package manager and build system                 ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    '   fpm SUBCOMMAND [SUBCOMMAND_OPTIONS]                                 ', &
    '                                                                       ', &
    '   fpm --help|--version|--list                                         ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    '   fpm(1) is a package manager that helps you create Fortran projects  ', &
    '   from source -- it automatically determines dependencies!            ', &
    '                                                                       ', &
    '   Most significantly fpm(1) lets you draw upon other fpm(1) packages  ', &
    '   in distributed git(1) repositories as if the packages were a basic  ', &
    '   part of your default programming environment, as well as letting    ', &
    '   you share your projects with others in a similar manner.            ', &
    '                                                                       ', &
    '   All output goes into the directory "build/" which can generally be  ', &
    '   removed and rebuilt if required. Note that if external packages are ', &
    '   being used you need network connectivity to rebuild from scratch.   ', &
    '                                                                       ', &
    'SUBCOMMANDS                                                            ', &
    '  Valid fpm(1) subcommands are:                                        ', &
    '                                                                       ', &
    '  + build    Compile the packages into the "build/" directory.         ', &
    '  + new      Create a new Fortran package directory with sample files. ', &
    '  + update   Update the project dependencies.                          ', &
    '  + run      Run the local package binaries. Defaults to all binaries  ', &
    '             for that release.                                         ', &
    '  + test     Run the tests.                                            ', &
    '  + help     Alternate to the --help switch for displaying help text.  ', &
    '  + list     Display brief descriptions of all subcommands.            ', &
    '  + install  Install project.                                          ', &
    '  + clean    Delete directories in the "build/" directory, except      ', &
    '             dependencies. Prompts for confirmation to delete.         ', &
    '                                                                       ', &
    '  Their syntax is                                                      ', &
    '                                                                                ', &
    '    build [--profile PROF] [--flag FFLAGS] [--list] [--compiler COMPILER_NAME]  ', &
    '          [--tests] [--no-prune]                                                ', &
    '    new NAME [[--lib|--src] [--app] [--test] [--example]]|                      ', &
    '             [--full|--bare][--backfill]                                        ', &
    '    update [NAME(s)] [--fetch-only] [--clean]                                   ', &
    '    run [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list] [--all]  ', &
    '        [--example] [--runner "CMD"] [--compiler COMPILER_NAME]                 ', &
    '        [--no-prune] [-- ARGS]                                                  ', &
    '    test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS] [--list]         ', &
    '         [--runner "CMD"] [--compiler COMPILER_NAME] [--no-prune] [-- ARGS]     ', &
    '    help [NAME(s)]                                                              ', &
    '    list [--list]                                                               ', &
    '    install [--profile PROF] [--flag FFLAGS] [--no-rebuild] [--prefix PATH]     ', &
    '            [options]                                                           ', &
    '    clean [--skip] [--all]                                                       ', &
    '                                                                                ', &
    'SUBCOMMAND OPTIONS                                                              ', &
    ' -C, --directory PATH', &
    '             Change working directory to PATH before running any command', &
    help_text_build_common, &
    help_text_compiler, &
    help_text_flag, &
    '  --list     List candidates instead of building or running them. On   ', &
    '             the fpm(1) command this shows a brief list of subcommands.', &
    '  --runner CMD   Provides a command to prefix program execution paths. ', &
    '  -- ARGS    Arguments to pass to executables.                         ', &
    '  --skip     Delete directories in the build/ directory without        ', &
    '             prompting, but skip dependencies.                         ', &
    '  --all      Delete directories in the build/ directory without        ', &
    '             prompting, including dependencies.                        ', &
    '                                                                       ', &
    'VALID FOR ALL SUBCOMMANDS                                              ', &
    '  --help     Show help text and exit                                   ', &
    '  --verbose  Display additional information when available             ', &
    '  --version  Show version information and exit.                        ', &
    '                                                                       ', &
    '@file                                                                  ', &
    '   You may replace the default options for the fpm(1) command from a   ', &
    '   file if your first options begin with @file. Initial options will   ', &
    '   then be read from the "response file" "file.rsp" in the current     ', &
    '   directory.                                                          ', &
    '                                                                       ', &
    '   If "file" does not exist or cannot be read, then an error occurs and', &
    '   the program stops. Each line of the file is prefixed with "options" ', &
    '   and interpreted as a separate argument. The file itself may not     ', &
    '   contain @file arguments. That is, it is not processed recursively.  ', &
    '                                                                       ', &
    '   For more information on response files see                          ', &
    '                                                                       ', &
    '      https://urbanjost.github.io/M_CLI2/set_args.3m_cli2.html         ', &
    '                                                                       ', &
    '   The basic functionality described here will remain the same, but    ', &
    '   other features described at the above reference may change.         ', &
    '                                                                       ', &
    '   An example file:                                                    ', &
    '                                                                       ', &
    '     # my build options                                                ', &
    '     options build                                                     ', &
    '     options --compiler gfortran                                       ', &
    '     options --flag "-pg -static -pthread -Wunreachable-code -Wunused  ', &
    '      -Wuninitialized -g -O -fbacktrace -fdump-core -fno-underscoring  ', &
    '      -frecord-marker=4 -L/usr/X11R6/lib -L/usr/X11R6/lib64 -lX11"     ', &
    '                                                                       ', &
    '   Note --flag would have to be on one line as response files do not   ', &
    '   (currently) allow for continued lines or multiple specifications of ', &
    '   the same option.                                                    ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    '   sample commands:                                                    ', &
    '                                                                       ', &
    '    fpm new mypackage --app --test                                     ', &
    '    fpm build                                                          ', &
    '    fpm test                                                           ', &
    '    fpm run                                                            ', &
    '    fpm run --example                                                  ', &
    '    fpm new --help                                                     ', &
    '    fpm run myprogram --profile release -- -x 10 -y 20 --title "my title"       ', &
    '    fpm install --prefix ~/.local                                               ', &
    '    fpm clean --all                                                             ', &
    '                                                                                ', &
    'SEE ALSO                                                                        ', &
    '                                                                                ', &
    ' + The fpm(1) home page is at https://github.com/fortran-lang/fpm               ', &
    ' + Registered fpm(1) packages are at https://fortran-lang.org/packages          ', &
    ' + The fpm(1) TOML file format is described at                                  ', &
    '   https://fpm.fortran-lang.org/en/spec/manifest.html                           ', &
    '']
    help_list=[character(len=80) :: &
    'NAME                                                                   ', &
    ' list(1) - list summary of fpm(1) subcommands                          ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm list [-list]                                                      ', &
    '                                                                       ', &
    ' fpm list --help|--version                                             ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Display a short description for each fpm(1) subcommand.               ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --list     display a list of command options as well. This is the     ', &
    '            same output as generated by "fpm --list".                  ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' display a short list of fpm(1) subcommands                            ', &
    '                                                                       ', &
    '  fpm list                                                             ', &
    '  fpm --list                                                           ', &
    '' ]
    help_run=[character(len=80) :: &
    'NAME                                                                   ', &
    ' run(1) - the fpm(1) subcommand to run project applications            ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm run [[--target] NAME(s) [--profile PROF] [--flag FFLAGS]', &
    '         [--compiler COMPILER_NAME] [--runner "CMD"] [--example]', &
    '         [--list] [--all] [-- ARGS]', &
    '                                                                       ', &
    ' fpm run --help|--version                                              ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Run the applications in your fpm(1) package. By default applications  ', &
    ' in /app or specified as "executable" in your "fpm.toml" manifest are  ', &
    ' used. Alternatively demonstration programs in example/ or specified in', &
    ' the "example" section in "fpm.toml" can be executed. The applications ', &
    ' are automatically rebuilt before being run if they are out of date.   ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --target NAME(s)  list of application names to execute. No name is    ', &
    '                   required if only one target exists. If no name is   ', &
    '                   supplied and more than one candidate exists or a    ', &
    '                   name has no match a list is produced and fpm(1)     ', &
    '                   exits.                                              ', &
    '                                                                       ', &
    '                   Basic "globbing" is supported where "?" represents  ', &
    '                   any single character and "*" represents any string. ', &
    '                   Note The glob string normally needs quoted to       ', &
    '                   the special characters from shell expansion.        ', &
    ' --all   Run all examples or applications. An alias for --target ''*''.  ', &
    ' --example  Run example programs instead of applications.              ', &
    help_text_build_common, &
    help_text_compiler, &
    help_text_flag, &
    ' --runner CMD  A command to prefix the program execution paths with.   ', &
    '               see "fpm help runner" for further details.              ', &
    ' --list     list basenames of candidates instead of running them. Note ', &
    '            out-of-date candidates will still be rebuilt before being  ', &
    '            listed.                                                    ', &
    ' -- ARGS    optional arguments to pass to the program(s). The same     ', &
    '            arguments are passed to all program names specified.       ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' fpm(1) - run or display project applications:                         ', &
    '                                                                       ', &
    '  fpm run        # run a target when only one exists or list targets   ', &
    '  fpm run --list # list basename of all targets, running nothing.      ', &
    '  fpm run "demo*" --list # list target basenames starting with "demo*".', &
    '  fpm run "psi*" --runner # list target pathnames starting with "psi*".', &
    '  fpm run --all  # run all targets, no matter how many there are.      ', &
    '                                                                       ', &
    '  # run default program built or to be built with the compiler command ', &
    '  # "f90". If more than one app exists a list displays and target names', &
    '  # are required.                                                      ', &
    '  fpm run --compiler f90                                               ', &
    '                                                                       ', &
    '  # run example programs instead of the application programs.          ', &
    '  fpm run --example "*"                                                ', &
    '                                                                       ', &
    '  # run a specific program and pass arguments to the command           ', &
    '  fpm run myprog -- -x 10 -y 20 --title "my title line"                ', &
    '                                                                       ', &
    '  # run production version of two applications                         ', &
    '  fpm run --target prg1,prg2 --profile release                         ', &
    '                                                                       ', &
    '  # install executables in directory (assuming install(1) exists)      ', &
    '  fpm run --runner ''install -b -m 0711 -p -t /usr/local/bin''         ', &
    '' ]
    help_build=[character(len=80) :: &
    'NAME                                                                   ', &
    ' build(1) - the fpm(1) subcommand to build a project                   ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm build [--profile PROF] [--flag FFLAGS] [--compiler COMPILER_NAME] ', &
    '           [--list] [--tests]                                          ', &
    '                                                                       ', &
    ' fpm build --help|--version                                            ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' The "fpm build" command                                               ', &
    '    o Fetches any dependencies                                         ', &
    '    o Scans your sources                                               ', &
    '    o Builds them in the proper order                                  ', &
    '                                                                       ', &
    ' The Fortran source files are assumed by default to be in              ', &
    '    o src/     for modules and procedure source                        ', &
    '    o app/     main program(s) for applications                        ', &
    '    o test/    main program(s) and support files for project tests     ', &
    '    o example/ main program(s) for example programs                    ', &
    ' Changed or new files found are rebuilt. The results are placed in     ', &
    ' the build/ directory.                                                 ', &
    '                                                                       ', &
    ' Non-default pathnames and remote dependencies are used if             ', &
    ' specified in the "fpm.toml" file.                                     ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    help_text_build_common,&
    help_text_compiler, &
    help_text_flag, &
    ' --list        list candidates instead of building or running them     ', &
    ' --tests       build all tests (otherwise only if needed)              ', &
    ' --show-model  show the model and exit (do not build)                  ', &
    ' --help        print this help and exit                                ', &
    ' --version     print program version information and exit              ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' Sample commands:                                                      ', &
    '                                                                       ', &
    '  fpm build                   # build with debug options               ', &
    '  fpm build --profile release # build with high optimization           ', &
    '' ]

    help_help=[character(len=80) :: &
    'NAME                                                                   ', &
    '   help(1) - the fpm(1) subcommand to display help                     ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    '   fpm help [fpm] [new] [build] [run] [test] [help] [version] [manual] ', &
    '   [runner]                                                            ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    '   The "fpm help" command is an alternative to the --help parameter    ', &
    '   on the fpm(1) command and its subcommands.                          ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    '   NAME(s)    A list of topic names to display. All the subcommands    ', &
    '              have their own page (new, build, run, test, ...).        ', &
    '                                                                       ', &
    '              The special name "manual" displays all the fpm(1)        ', &
    '              built-in documentation.                                  ', &
    '                                                                       ', &
    '              The default is to display help for the fpm(1) command    ', &
    '              itself.                                                  ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    '   Sample usage:                                                       ', &
    '                                                                       ', &
    '     fpm help           # general fpm(1) command help                  ', &
    '     fpm help version   # show program version                         ', &
    '     fpm help new       # display help for "new" subcommand            ', &
    '     fpm help manual    # All fpm(1) built-in documentation            ', &
    '                                                                       ', &
    '' ]
    help_new=[character(len=80) ::                                             &
    'NAME                                                                   ', &
    ' new(1) - the fpm(1) subcommand to initialize a new project            ', &
    'SYNOPSIS                                                               ', &
    '  fpm new NAME [[--lib|--src] [--app] [--test] [--example]]|           ', &
    '      [--full|--bare][--backfill]                                      ', &
    ' fpm new --help|--version                                              ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' "fpm new" creates and populates a new programming project directory.  ', &
    ' It                                                                    ', &
    '   o creates a directory with the specified name                       ', &
    '   o runs the command "git init" in that directory                     ', &
    '   o populates the directory with the default project directories      ', &
    '   o adds sample Fortran source files                                  ', &
    '                                                                       ', &
    ' The default file structure (that will be automatically scanned) is    ', &
    '                                                                       ', &
    '     NAME/                                                             ', &
    '       fpm.toml                                                        ', &
    '       src/                                                            ', &
    '           NAME.f90                                                    ', &
    '       app/                                                            ', &
    '           main.f90                                                    ', &
    '       test/                                                           ', &
    '           check.f90                                                   ', &
    '       example/                                                        ', &
    '           demo.f90                                                    ', &
    '                                                                       ', &
    ' Using this file structure is highly encouraged, particularly for      ', &
    ' small packages primarily intended to be used as dependencies.         ', &
    '                                                                       ', &
    ' If you find this restrictive and need to customize the package        ', &
    ' structure you will find using the --full switch creates a             ', &
    ' heavily annotated manifest file with references to documentation      ', &
    ' to aid in constructing complex package structures.                    ', &
    '                                                                       ', &
    ' Remember to update the information in the sample "fpm.toml"           ', &
    ' file with your name and e-mail address.                               ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' NAME   the name of the project directory to create. The name          ', &
    '        must be made of up to 63 ASCII letters, digits, underscores,   ', &
    '        or hyphens, and start with a letter.                           ', &
    '                                                                       ', &
    ' The default is to create the src/, app/, and test/ directories.       ', &
    ' If any of the following options are specified then only the           ', &
    ' selected subdirectories are generated:                                ', &
    '                                                                       ', &
    ' --lib,--src  create directory src/ and a placeholder module           ', &
    '              named "NAME.f90" for use with subcommand "build".        ', &
    ' --app        create directory app/ and a placeholder main             ', &
    '              program for use with subcommand "run".                   ', &
    ' --test       create directory test/ and a placeholder program         ', &
    '              for use with the subcommand "test". Note that sans       ', &
    '              "--lib" it really does not have anything to test.        ', &
    ' --example    create directory example/ and a placeholder program      ', &
    '              for use with the subcommand "run --example".             ', &
    '              It is only created by default if "--full is" specified.  ', &
    '                                                                       ', &
    ' So the default is equivalent to                                        ',&
    '                                                                       ', &
    '    fpm NAME --lib --app --test                                        ', &
    '                                                                       ', &
    ' --backfill   By default the directory must not exist. If this         ', &
    '              option is present the directory may pre-exist and        ', &
    '              only subdirectories and files that do not                ', &
    '              already exist will be created. For example, if you       ', &
    '              previously entered "fpm new myname --lib" entering       ', &
    '              "fpm new myname -full --backfill" will create any missing', &
    '              app/, example/, and test/ directories and programs.      ', &
    '                                                                       ', &
    ' --full       By default a minimal manifest file ("fpm.toml") is       ', &
    '              created that depends on auto-discovery. With this        ', &
    '              option a much more extensive manifest sample is written  ', &
    '              and the example/ directory is created and populated.     ', &
    '              It is designed to facilitate creating projects that      ', &
    '              depend extensively on non-default build options.         ', &
    '                                                                       ', &
    ' --bare       A minimal manifest file ("fpm.toml") is created and      ', &
    '              "README.md" file is created but no directories or        ', &
    '              sample Fortran are generated.                            ', &
    '                                                                       ', &
    ' --help       print this help and exit                                 ', &
    ' --version    print program version information and exit               ', &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    ' Sample use                                                            ', &
    '                                                                       ', &
    '   fpm new myproject  # create new project directory and seed it       ', &
    '   cd myproject       # Enter the new directory                        ', &
    '   # and run commands such as                                          ', &
    '   fpm build                                                           ', &
    '   fpm run            # run lone example application program           ', &
    '   fpm test           # run example test program(s)                    ', &
    '   fpm run --example  # run lone example program                       ', &
    '                                                                       ', &
    '   fpm new A --full # create example/ and an annotated fpm.toml as well', &
    '   fpm new A --bare # create no directories                            ', &
    '   create any missing files in current directory                       ', &
    '   fpm new --full --backfill                                           ', &
    '' ]
    help_test=[character(len=80) :: &
    'NAME                                                                   ', &
    ' test(1) - the fpm(1) subcommand to run project tests                  ', &
    '                                                                       ', &
    'SYNOPSIS                                                               ', &
    ' fpm test [[--target] NAME(s)] [--profile PROF] [--flag FFLAGS]', &
    '          [--compiler COMPILER_NAME ] [--runner "CMD"] [--list][-- ARGS]', &
    '                                                                       ', &
    ' fpm test --help|--version                                             ', &
    '                                                                       ', &
    'DESCRIPTION                                                            ', &
    ' Run applications you have built to test your project.                 ', &
    '                                                                       ', &
    'OPTIONS                                                                ', &
    ' --target NAME(s)  optional list of specific test names to execute.    ', &
    '                   The default is to run all the tests in test/        ', &
    '                   or the tests listed in the "fpm.toml" file.         ', &
    '                                                                       ', &
    '                   Basic "globbing" is supported where "?" represents  ', &
    '                   any single character and "*" represents any string. ', &
    '                   Note The glob string normally needs quoted to       ', &
    '                   protect the special characters from shell expansion.', &
    help_text_build_common,&
    help_text_compiler, &
    help_text_flag, &
    ' --runner CMD  A command to prefix the program execution paths with.   ', &
    '               see "fpm help runner" for further details.              ', &
    ' --list     list candidate basenames instead of running them. Note they', &
    ' --list     will still be built if not currently up to date.           ', &
    ' -- ARGS    optional arguments to pass to the test program(s).         ', &
    '            The same arguments are passed to all test names            ', &
    '            specified.                                                 ', &
    '                                                                       ', &
    help_text_environment, &
    '                                                                       ', &
    'EXAMPLES                                                               ', &
    'run tests                                                              ', &
    '                                                                       ', &
    ' # run default tests in /test or as specified in "fpm.toml"            ', &
    ' fpm test                                                              ', &
    '                                                                       ', &
    ' # run using compiler command "f90"                                    ', &
    ' fpm test --compiler f90                                               ', &
    '                                                                       ', &
    ' # run a specific test and pass arguments to the command               ', &
    ' fpm test mytest -- -x 10 -y 20 --title "my title line"                ', &
    '                                                                       ', &
    ' fpm test tst1 tst2 --profile PROF  # run production version of two tests', &
    '' ]
    help_update=[character(len=80) :: &
    'NAME', &
    ' update(1) - manage project dependencies', &
    '', &
    'SYNOPSIS', &
    ' fpm update [--fetch-only] [--clean] [--verbose] [NAME(s)]', &
    '', &
    'DESCRIPTION', &
    ' Manage and update project dependencies. If no dependency names are', &
    ' provided all the dependencies are updated automatically.', &
    '', &
    'OPTIONS', &
    ' --fetch-only  Only fetch dependencies, do not update existing projects', &
    ' --clean       Do not use previous dependency cache', &
    ' --verbose     Show additional printout', &
    '', &
    'SEE ALSO', &
    ' The fpm(1) home page at https://github.com/fortran-lang/fpm', &
    '' ]
    help_install=[character(len=80) :: &
    'NAME', &
    ' install(1) - install fpm projects', &
    '', &
    'SYNOPSIS', &
    ' fpm install [--profile PROF] [--flag FFLAGS] [--list] [--no-rebuild]', &
    '             [--prefix DIR] [--bindir DIR] [--libdir DIR] [--includedir DIR]', &
    '             [--verbose]', &
    '', &
    'DESCRIPTION', &
    ' Subcommand to install fpm projects. Running install will export the', &
    ' current project to the selected prefix, this will by default install all', &
    ' executables (tests and examples are excluded) which are part of the projects.', &
    ' Libraries and module files are only installed for projects requiring the', &
    ' installation of those components in the package manifest.', &
    '', &
    'OPTIONS', &
    ' --list            list all installable targets for this project,', &
    '                   but do not install any of them', &
    help_text_build_common,&
    help_text_flag, &
    ' --no-rebuild      do not rebuild project before installation', &
    ' --prefix DIR      path to installation directory (requires write access),', &
    '                   the default prefix on Unix systems is $HOME/.local', &
    '                   and %APPDATA%\local on Windows', &
    ' --bindir DIR      subdirectory to place executables in (default: bin)', &
    ' --libdir DIR      subdirectory to place libraries and archives in', &
    '                   (default: lib)', &
    ' --includedir DIR  subdirectory to place headers and module files in', &
    '                   (default: include)', &
    ' --verbose         print more information', &
    '', &
    help_text_environment, &
    '', &
    'EXAMPLES', &
    ' 1. Install release version of project:', &
    '', &
    '    fpm install --profile release', &
    '', &
    ' 2. Install the project without rebuilding the executables:', &
    '', &
    '    fpm install --no-rebuild', &
    '', &
    ' 3. Install executables to a custom prefix into the exe directory:', &
    '', &
    '    fpm install --prefix $PWD --bindir exe', &
    '' ]
    help_clean=[character(len=80) :: &
    'NAME', &
    ' clean(1) - delete the build', &
    '', &
    'SYNOPSIS', &
    ' fpm clean', &
    '', &
    'DESCRIPTION', &
    ' Prompts the user to confirm deletion of the build. If affirmative,', &
    ' directories in the build/ directory are deleted, except dependencies.', &
    '', &
    'OPTIONS', &
    ' --skip           delete the build without prompting but skip dependencies.', &
    ' --all            delete the build without prompting including dependencies.', &
    '' ]
     end subroutine set_help

    subroutine get_char_arg(var, arg)
      character(len=:), allocatable, intent(out) :: var
      character(len=*), intent(in) :: arg
      var = sget(arg)
      if (len_trim(var) == 0) deallocate(var)
    end subroutine get_char_arg


    !> Get an environment variable for fpm, this routine ensures that every variable
    !> used by fpm is prefixed with FPM_.
    function get_fpm_env(env, default) result(val)
      character(len=*), intent(in) :: env
      character(len=*), intent(in) :: default
      character(len=:), allocatable :: val

      character(len=*), parameter :: fpm_prefix = "FPM_"

      val = get_env(fpm_prefix//env, default)
    end function get_fpm_env

end module fpm_command_line
 
 
!>>>>> ././src/fpm/git.f90
!> Implementation for interacting with git repositories.
module fpm_git
    use fpm_error, only: error_t, fatal_error
    use fpm_filesystem, only : get_temp_filename, getline, join_path
    implicit none

    public :: git_target_t
    public :: git_target_default, git_target_branch, git_target_tag, &
        & git_target_revision
    public :: git_revision


    !> Possible git target
    type :: enum_descriptor

        !> Default target
        integer :: default = 200

        !> Branch in git repository
        integer :: branch = 201

        !> Tag in git repository
        integer :: tag = 202

        !> Commit hash
        integer :: revision = 203

    end type enum_descriptor

    !> Actual enumerator for descriptors
    type(enum_descriptor), parameter :: git_descriptor = enum_descriptor()


    !> Description of an git target
    type :: git_target_t

        !> Kind of the git target
        integer, private :: descriptor = git_descriptor%default

        !> Target URL of the git repository
        character(len=:), allocatable :: url

        !> Additional descriptor of the git object
        character(len=:), allocatable :: object

    contains

        !> Fetch and checkout in local directory
        procedure :: checkout

        !> Show information on instance
        procedure :: info

    end type git_target_t


contains


    !> Default target
    function git_target_default(url) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%default
        self%url = url

    end function git_target_default


    !> Target a branch in the git repository
    function git_target_branch(url, branch) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Name of the branch of interest
        character(len=*), intent(in) :: branch

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%branch
        self%url = url
        self%object = branch

    end function git_target_branch


    !> Target a specific git revision
    function git_target_revision(url, sha1) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Commit hash of interest
        character(len=*), intent(in) :: sha1

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%revision
        self%url = url
        self%object = sha1

    end function git_target_revision


    !> Target a git tag
    function git_target_tag(url, tag) result(self)

        !> Target URL of the git repository
        character(len=*), intent(in) :: url

        !> Tag name of interest
        character(len=*), intent(in) :: tag

        !> New git target
        type(git_target_t) :: self

        self%descriptor = git_descriptor%tag
        self%url = url
        self%object = tag

    end function git_target_tag


    subroutine checkout(self, local_path, error)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat
        character(len=:), allocatable :: object, workdir

        if (allocated(self%object)) then
            object = self%object
        else
            object = 'HEAD'
        end if
        workdir = "--work-tree="//local_path//" --git-dir="//join_path(local_path, ".git")

        call execute_command_line("git init "//local_path, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while initiating git repository for remote dependency')
            return
        end if

        call execute_command_line("git "//workdir//" fetch --depth=1 "// &
                                  self%url//" "//object, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while fetching git repository for remote dependency')
            return
        end if

        call execute_command_line("git "//workdir//" checkout -qf FETCH_HEAD", exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error,'Error while checking out git repository for remote dependency')
            return
        end if

    end subroutine checkout


    subroutine git_revision(local_path, object, error)

        !> Local path to checkout in
        character(*), intent(in) :: local_path

        !> Git object reference
        character(len=:), allocatable, intent(out) :: object

        !> Error
        type(error_t), allocatable, intent(out) :: error

        integer :: stat, unit, istart, iend
        character(len=:), allocatable :: temp_file, line, iomsg, workdir
        character(len=*), parameter :: hexdigits = '0123456789abcdef'

        workdir = "--work-tree="//local_path//" --git-dir="//join_path(local_path, ".git")
        allocate(temp_file, source=get_temp_filename())
        line = "git "//workdir//" log -n 1 > "//temp_file
        call execute_command_line(line, exitstat=stat)

        if (stat /= 0) then
            call fatal_error(error, "Error while retrieving commit information")
            return
        end if

        open(file=temp_file, newunit=unit)
        call getline(unit, line, stat, iomsg)

        if (stat /= 0) then
            call fatal_error(error, iomsg)
            return
        end if
        close(unit, status="delete")

        ! Tokenize:
        ! commit 0123456789abcdef (HEAD, ...)
        istart = scan(line, ' ') + 1
        iend = verify(line(istart:), hexdigits) + istart - 1
        if (iend < istart) iend = len(line)
        object = line(istart:iend)

    end subroutine git_revision


    !> Show information on git target
    subroutine info(self, unit, verbosity)

        !> Instance of the git target
        class(git_target_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Git target"
        if (allocated(self%url)) then
            write(unit, fmt) "- URL", self%url
        end if
        if (allocated(self%object)) then
            select case(self%descriptor)
            case default
                write(unit, fmt) "- object", self%object
            case(git_descriptor%tag)
                write(unit, fmt) "- tag", self%object
            case(git_descriptor%branch)
                write(unit, fmt) "- branch", self%object
            case(git_descriptor%revision)
                write(unit, fmt) "- sha1", self%object
            end select
        end if

    end subroutine info


end module fpm_git
 
 
!>>>>> ././src/fpm/installer.f90
!> Implementation of an installer object.
!>
!> The installer provides a way to install objects to their respective directories
!> in the installation prefix, a generic install command allows to install
!> to any directory within the prefix.
module fpm_installer
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm_environment, only : get_os_type, os_is_unix
  use fpm_error, only : error_t, fatal_error
  use fpm_filesystem, only : join_path, mkdir, exists, unix_path, windows_path, &
    env_variable
  implicit none
  private

  public :: installer_t, new_installer


  !> Declaration of the installer type
  type :: installer_t
    !> Path to installation directory
    character(len=:), allocatable :: prefix
    !> Binary dir relative to the installation prefix
    character(len=:), allocatable :: bindir
    !> Library directory relative to the installation prefix
    character(len=:), allocatable :: libdir
    !> Include directory relative to the installation prefix
    character(len=:), allocatable :: includedir
    !> Output unit for informative printout
    integer :: unit = output_unit
    !> Verbosity of the installer
    integer :: verbosity = 1
    !> Command to copy objects into the installation prefix
    character(len=:), allocatable :: copy
    !> Command to move objects into the installation prefix
    character(len=:), allocatable :: move
    !> Cached operating system
    integer :: os
  contains
    !> Install an executable in its correct subdirectory
    procedure :: install_executable
    !> Install a library in its correct subdirectory
    procedure :: install_library
    !> Install a header/module in its correct subdirectory
    procedure :: install_header
    !> Install a generic file into a subdirectory in the installation prefix
    procedure :: install
    !> Run an installation command, type-bound for unit testing purposes
    procedure :: run
    !> Create a new directory in the prefix, type-bound for unit testing purposes
    procedure :: make_dir
  end type installer_t

  !> Default name of the binary subdirectory
  character(len=*), parameter :: default_bindir = "bin"

  !> Default name of the library subdirectory
  character(len=*), parameter :: default_libdir = "lib"

  !> Default name of the include subdirectory
  character(len=*), parameter :: default_includedir = "include"

  !> Default name of the installation prefix on Unix platforms
  character(len=*), parameter :: default_prefix_unix = "/usr/local"

  !> Default name of the installation prefix on Windows platforms
  character(len=*), parameter :: default_prefix_win = "C:\"

  !> Copy command on Unix platforms
  character(len=*), parameter :: default_copy_unix = "cp"

  !> Copy command on Windows platforms
  character(len=*), parameter :: default_copy_win = "copy"

  !> Move command on Unix platforms
  character(len=*), parameter :: default_move_unix = "mv"

  !> Move command on Windows platforms
  character(len=*), parameter :: default_move_win = "move"


contains

  !> Create a new instance of an installer
  subroutine new_installer(self, prefix, bindir, libdir, includedir, verbosity, &
          copy, move)
    !> Instance of the installer
    type(installer_t), intent(out) :: self
    !> Path to installation directory
    character(len=*), intent(in), optional :: prefix
    !> Binary dir relative to the installation prefix
    character(len=*), intent(in), optional :: bindir
    !> Library directory relative to the installation prefix
    character(len=*), intent(in), optional :: libdir
    !> Include directory relative to the installation prefix
    character(len=*), intent(in), optional :: includedir
    !> Verbosity of the installer
    integer, intent(in), optional :: verbosity
    !> Copy command
    character(len=*), intent(in), optional :: copy
    !> Move command
    character(len=*), intent(in), optional :: move

    self%os = get_os_type()

    if (present(copy)) then
      self%copy = copy
    else
      if (os_is_unix(self%os)) then
        self%copy = default_copy_unix
      else
        self%copy = default_copy_win
      end if
    end if

    if (present(move)) then
      self%move = move
    else
      if (os_is_unix(self%os)) then
        self%move = default_move_unix
      else
        self%move = default_move_win
      end if
    end if

    if (present(includedir)) then
      self%includedir = includedir
    else
      self%includedir = default_includedir
    end if

    if (present(prefix)) then
      self%prefix = prefix
    else
      call set_default_prefix(self%prefix, self%os)
    end if

    if (present(bindir)) then
      self%bindir = bindir
    else
      self%bindir = default_bindir
    end if

    if (present(libdir)) then
      self%libdir = libdir
    else
      self%libdir = default_libdir
    end if

    if (present(verbosity)) then
      self%verbosity = verbosity
    else
      self%verbosity = 1
    end if

  end subroutine new_installer

  !> Set the default prefix for the installation
  subroutine set_default_prefix(prefix, os)
    !> Installation prefix
    character(len=:), allocatable :: prefix
    !> Platform identifier
    integer, intent(in), optional :: os

    character(len=:), allocatable :: home

    if (os_is_unix(os)) then
      call env_variable(home, "HOME")
      if (allocated(home)) then
        prefix = join_path(home, ".local")
      else
        prefix = default_prefix_unix
      end if
    else
      call env_variable(home, "APPDATA")
      if (allocated(home)) then
        prefix = join_path(home, "local")
      else
        prefix = default_prefix_win
      end if
    end if

  end subroutine set_default_prefix

  !> Install an executable in its correct subdirectory
  subroutine install_executable(self, executable, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the executable
    character(len=*), intent(in) :: executable
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: ll

    if (.not.os_is_unix(self%os)) then
        ll = len(executable)
        if (executable(max(1, ll-3):ll) /= ".exe") then
            call self%install(executable//".exe", self%bindir, error)
            return
        end if
    end if

    call self%install(executable, self%bindir, error)

  end subroutine install_executable

  !> Install a library in its correct subdirectory
  subroutine install_library(self, library, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the library
    character(len=*), intent(in) :: library
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call self%install(library, self%libdir, error)
  end subroutine install_library

  !> Install a header/module in its correct subdirectory
  subroutine install_header(self, header, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the header
    character(len=*), intent(in) :: header
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call self%install(header, self%includedir, error)
  end subroutine install_header

  !> Install a generic file into a subdirectory in the installation prefix
  subroutine install(self, source, destination, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Path to the original file
    character(len=*), intent(in) :: source
    !> Path to the destination inside the prefix
    character(len=*), intent(in) :: destination
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    character(len=:), allocatable :: install_dest

    install_dest = join_path(self%prefix, destination)
    if (os_is_unix(self%os)) then
      install_dest = unix_path(install_dest)
    else
      install_dest = windows_path(install_dest)
    end if
    call self%make_dir(install_dest, error)
    if (allocated(error)) return

    if (self%verbosity > 0) then
      if (exists(install_dest)) then
        write(self%unit, '("# Update:", 1x, a, 1x, "->", 1x, a)') &
          source, install_dest
      else
        write(self%unit, '("# Install:", 1x, a, 1x, "->", 1x, a)') &
          source, install_dest
      end if
    end if

    ! move instead of copy if already installed
    if (exists(install_dest)) then
      call self%run(self%move//' "'//source//'" "'//install_dest//'"', error)
    else
      call self%run(self%copy//' "'//source//'" "'//install_dest//'"', error)
    end if
    if (allocated(error)) return

  end subroutine install

  !> Create a new directory in the prefix
  subroutine make_dir(self, dir, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Directory to be created
    character(len=*), intent(in) :: dir
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    if (.not.exists(dir)) then
       if (self%verbosity > 1) then
          write(self%unit, '("# Dir:", 1x, a)') dir
       end if
       call mkdir(dir)
    end if
  end subroutine make_dir

  !> Run an installation command
  subroutine run(self, command, error)
    !> Instance of the installer
    class(installer_t), intent(inout) :: self
    !> Command to be launched
    character(len=*), intent(in) :: command
    !> Error handling
    type(error_t), allocatable, intent(out) :: error
    integer :: stat

    if (self%verbosity > 1) then
      write(self%unit, '("# Run:", 1x, a)') command
    end if
    call execute_command_line(command, exitstat=stat)

    if (stat /= 0) then
      call fatal_error(error, "Failed in command: '"//command//"'")
      return
    end if
  end subroutine run

end module fpm_installer
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/type/value.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Class definitions for basic data types used for handling TOML
module tomlf_type_value
   use tomlf_constants, only : tfc, TOML_BAREKEY
   use tomlf_utils, only : toml_escape_string
   implicit none
   private

   public :: toml_value, toml_visitor, toml_key


   !> Abstract base value for TOML data types
   type, abstract :: toml_value

      !> Raw representation of the key to the TOML value
      character(kind=tfc, len=:), allocatable :: key

   contains

      !> Accept a visitor to transverse the data structure
      procedure :: accept

      !> Get escaped key to TOML value
      procedure :: get_key

      !> Compare raw key of TOML value to input key
      procedure :: match_key

      !> Release allocation hold by TOML value
      procedure(destroy), deferred :: destroy

   end type toml_value


   !> Abstract visitor for TOML values
   type, abstract :: toml_visitor
   contains

      !> Visitor visiting a TOML value
      procedure(visit), deferred :: visit

   end type toml_visitor


   !> Thin wrapper around the deferred-size character intrinisc
   type :: toml_key

      !> Raw representation of the key to the TOML value
      character(kind=tfc, len=:), allocatable :: key

   end type toml_key


   abstract interface
      !> Accept a visitor to transverse the data structure
      recursive subroutine visit(self, val)
         import toml_value, toml_visitor

         !> Instance of the visitor
         class(toml_visitor), intent(inout) :: self

         !> Value to visit
         class(toml_value), intent(inout) :: val
      end subroutine visit

      !> Deconstructor to cleanup allocations (optional)
      subroutine destroy(self)
         import toml_value

         !> Instance of the TOML value
         class(toml_value), intent(inout) :: self

      end subroutine destroy

   end interface


contains


!> Accept a visitor to transverse the data structure
recursive subroutine accept(self, visitor)

   !> Instance of the TOML value
   class(toml_value), intent(inout) :: self

   !> Visitor for this value
   class(toml_visitor), intent(inout) :: visitor

   call visitor%visit(self)

end subroutine accept


!> Get escaped key to TOML value
subroutine get_key(self, key)

   !> TOML value instance.
   class(toml_value), intent(in) :: self

   !> Contains valid TOML key on exit
   character(kind=tfc, len=:), allocatable :: key

   if (allocated(self%key)) then
      if (verify(self%key, TOML_BAREKEY) == 0 .and. len(self%key) > 0) then
         key = self%key
      else
         call toml_escape_string(self%key, key)
      end if
   end if

end subroutine get_key


!> Compare raw key of TOML value to input key
pure function match_key(self, key) result(match)

   !> TOML value instance.
   class(toml_value), intent(in) :: self

   !> TOML raw key to compare to
   character(kind=tfc, len=*), intent(in) :: key

   logical :: match

   if (allocated(self%key)) then
      match = key == self%key
   else
      match = .false.
   end if

end function match_key


end module tomlf_type_value
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/type/keyval.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML key-value pair
module tomlf_type_keyval
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_visitor
   implicit none
   private

   public :: toml_keyval, new_keyval, new


   !> TOML key-value pair
   type, extends(toml_value) :: toml_keyval

      !> Raw content of the TOML value
      character(kind=tfc, len=:), allocatable :: raw

   contains

      !> Release allocation hold by TOML key-value pair
      procedure :: destroy

   end type toml_keyval


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_keyval
   end interface


contains


!> Constructor to create a new TOML key-value pair
subroutine new_keyval(self)

   !> Instance of the TOML key-value pair
   type(toml_keyval), intent(out) :: self

   associate(self => self); end associate

end subroutine new_keyval


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML key-value pair
   class(toml_keyval), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%raw)) then
      deallocate(self%raw)
   end if

end subroutine destroy


end module tomlf_type_keyval
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/structure/base.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Abstract base class definitions for data structures to store TOML values
module tomlf_structure_base
   use tomlf_constants, only : tfc
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_structure, toml_ordered


   !> Abstract data structure
   type, abstract :: toml_structure
   contains

      !> Find a TOML value based on its key
      procedure(find), deferred :: find

      !> Push back a TOML value to the structure
      procedure(push_back), deferred :: push_back

      !> Get list of all keys in the structure
      procedure(get_keys), deferred :: get_keys

      !> Delete TOML value at a given key
      procedure(delete), deferred :: delete

      !> Destroy the data structure
      procedure(destroy), deferred :: destroy

   end type toml_structure


   !> Ordered data structure, allows iterations
   type, abstract, extends(toml_structure) :: toml_ordered
   contains

      !> Get number of TOML values in the structure
      procedure(get_len), deferred :: get_len

      !> Remove the first element from the structure
      procedure(shift), deferred :: shift

      !> Remove the last element from the structure
      procedure(pop), deferred :: pop

      !> Get TOML value at a given index
      procedure(get), deferred :: get

   end type toml_ordered


   abstract interface
      !> Find a TOML value based on its key
      subroutine find(self, key, ptr)
         import :: toml_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

         !> Pointer to the stored value at given key
         class(toml_value), pointer, intent(out) :: ptr
      end subroutine find


      !> Get number of TOML values in the structure
      pure function get_len(self) result(length)
         import :: toml_ordered

         !> Instance of the structure
         class(toml_ordered), intent(in), target :: self

         !> Current length of the ordered structure
         integer :: length
      end function get_len


      !> Get TOML value at a given index
      subroutine get(self, idx, ptr)
         import :: toml_ordered, toml_value

         !> Instance of the structure
         class(toml_ordered), intent(inout), target :: self

         !> Position in the ordered structure
         integer, intent(in) :: idx

         !> Pointer to the stored value at given index
         class(toml_value), pointer, intent(out) :: ptr
      end subroutine get


      !> Push back a TOML value to the structure
      subroutine push_back(self, val)
         import :: toml_structure, toml_value

         !> Instance of the structure
         class(toml_structure), intent(inout), target :: self

         !> TOML value to be stored
         class(toml_value), allocatable, intent(inout) :: val

      end subroutine push_back


      !> Remove the first element from the data structure
      subroutine shift(self, val)
         import :: toml_ordered, toml_value

         !> Instance of the structure
         class(toml_ordered), intent(inout), target :: self

         !> TOML value to be retrieved
         class(toml_value), allocatable, intent(out) :: val

      end subroutine shift


      !> Remove the last element from the data structure
      subroutine pop(self, val)
         import :: toml_ordered, toml_value

         !> Instance of the structure
         class(toml_ordered), intent(inout), target :: self

         !> TOML value to be retrieved
         class(toml_value), allocatable, intent(out) :: val

      end subroutine pop


      !> Get list of all keys in the structure
      subroutine get_keys(self, list)
         import :: toml_structure, toml_key

         !> Instance of the structure
         class(toml_structure), intent(inout), target :: self

         !> List of all keys
         type(toml_key), allocatable, intent(out) :: list(:)

      end subroutine get_keys


      !> Delete TOML value at a given key
      subroutine delete(self, key)
         import :: toml_structure, toml_value, tfc

         !> Instance of the structure
         class(toml_structure), intent(inout), target :: self

         !> Key to the TOML value
         character(kind=tfc, len=*), intent(in) :: key

      end subroutine delete


      !> Deconstructor for data structure
      subroutine destroy(self)
         import :: toml_structure

         !> Instance of the structure
         class(toml_structure), intent(inout), target :: self

      end subroutine destroy

   end interface


end module tomlf_structure_base
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/utils/sort.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Sorting algorithms to work with hash maps
module tomlf_utils_sort
   use tomlf_type_value, only : toml_key
   implicit none
   private

   public :: sort, compare_less


   !> Create overloaded interface for export
   interface sort
      module procedure :: sort_keys
   end interface


   abstract interface
      !> Define order relation between two TOML keys
      pure function compare_less(lhs, rhs) result(less)
         import :: toml_key
         !> Left hand side TOML key in comparison
         type(toml_key), intent (in) :: lhs
         !> Right hand side TOML key in comparison
         type(toml_key), intent (in) :: rhs
         !> Comparison result
         logical :: less
      end function compare_less
   end interface


contains


   !> Entry point for sorting algorithm
   pure subroutine sort_keys(list, idx, compare)

      !> List of TOML keys to be sorted
      type(toml_key), intent(inout) :: list(:)

      !> Optionally, mapping from unsorted list to sorted list
      integer, intent(out), optional :: idx(:)

      !> Function implementing the order relation between two TOML keys
      procedure(compare_less), optional :: compare

      integer  :: low, high, i
      type(toml_key), allocatable  :: sorted(:)
      integer, allocatable :: indexarray(:)

      low = 1
      high = size(list)

      sorted = list

      allocate(indexarray(high), source=[(i, i=low, high)])

      if (present(compare)) then
         call quicksort(sorted, indexarray, low, high, compare)
      else
         call quicksort(sorted, indexarray, low, high, compare_keys_less)
      end if

      do i = low, high
         list(i) = sorted(indexarray(i))
      end do

      if (present(idx)) then
         idx = indexarray
      end if

   end subroutine sort_keys


   !> Actual quick sort implementation
   pure recursive subroutine quicksort(list, idx, low, high, less)
      type(toml_key), intent(inout) :: list(:)
      integer, intent(inout) :: idx(:)
      integer, intent(in) :: low, high
      procedure(compare_less) :: less

      integer :: i, last
      integer :: pivot

      if (low < high) then

         call swap(idx(low), idx((low + high)/2))
         last = low
         do i = low + 1, high
            if (less(list(idx(i)), list(idx(low)))) then
               last = last + 1
               call swap(idx(last), idx(i))
            end if
         end do
         call swap(idx(low), idx(last))
         pivot = last

         call quicksort(list, idx, low, pivot - 1, less)
         call quicksort(list, idx, pivot + 1, high, less)
      end if

   end subroutine quicksort


   !> Swap two integer values
   pure subroutine swap(lhs, rhs)
      integer, intent(inout) :: lhs
      integer, intent(inout) :: rhs

      integer :: tmp

      tmp = lhs
      lhs = rhs
      rhs = tmp

   end subroutine swap


   !> Default comparison between two TOML keys
   pure function compare_keys_less(lhs, rhs) result(less)
      type(toml_key), intent (in) :: lhs
      type(toml_key), intent (in) :: rhs
      logical :: less

      less = lhs%key < rhs%key

   end function compare_keys_less


end module tomlf_utils_sort
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/structure/vector.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a basic storage structure as pointer list of pointers.
!>
!> This implementation does purposely not use pointer attributes in the
!> datastructure to make it safer to work with.
module tomlf_structure_vector
   use tomlf_constants, only : tfc
   use tomlf_structure_base, only : toml_ordered
   use tomlf_type_value, only : toml_value, toml_key
   implicit none
   private

   public :: toml_vector, new_vector


   !> Wrapped TOML value to generate pointer list
   type :: toml_node

      !> TOML value payload
      class(toml_value), allocatable :: val

   end type toml_node


   !> Stores TOML values in a list of pointers
   type, extends(toml_ordered) :: toml_vector

      !> Current number of stored TOML values
      integer :: n = 0

      !> List of TOML values
      type(toml_node), allocatable :: lst(:)

   contains

      !> Get number of TOML values in the structure
      procedure :: get_len

      !> Find a TOML value based on its key
      procedure :: find

      !> Get TOML value at a given index
      procedure :: get

      !> Push back a TOML value to the structure
      procedure :: push_back

      !> Remove the first element from the structure
      procedure :: shift

      !> Remove the last element from the structure
      procedure :: pop

      !> Get list of all keys in the structure
      procedure :: get_keys

      !> Delete TOML value at a given key
      procedure :: delete

      !> Destroy the data structure
      procedure :: destroy

   end type toml_vector


   !> Initial storage capacity of the datastructure
   integer, parameter :: initial_size = 16


contains


!> Constructor for the storage data structure
subroutine new_vector(self, n)

   !> Instance of the structure
   type(toml_vector), intent(out) :: self

   !> Initial storage capacity
   integer, intent(in), optional :: n

   self%n = 0
   if (present(n)) then
      allocate(self%lst(min(1, n)))
   else
      allocate(self%lst(initial_size))
   end if

end subroutine new_vector


!> Get number of TOML values in the structure
pure function get_len(self) result(length)

   !> Instance of the structure
   class(toml_vector), intent(in), target :: self

   !> Current length of the ordered structure
   integer :: length

   length = self%n

end function get_len


!> Find a TOML value based on its key
subroutine find(self, key, ptr)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the stored value at given key
   class(toml_value), pointer, intent(out) :: ptr

   integer :: i

   nullify(ptr)

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (self%lst(i)%val%match_key(key)) then
            ptr => self%lst(i)%val
            exit
         end if
      end if
   end do

end subroutine find


!> Get TOML value at a given index
subroutine get(self, idx, ptr)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> Position in the ordered structure
   integer, intent(in) :: idx

   !> Pointer to the stored value at given index
   class(toml_value), pointer, intent(out) :: ptr

   nullify(ptr)

   if (idx > 0 .and. idx <= self%n) then
      if (allocated(self%lst(idx)%val)) then
         ptr => self%lst(idx)%val
      end if
   end if

end subroutine get


!> Push back a TOML value to the structure
subroutine push_back(self, val)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> TOML value to be stored
   class(toml_value), allocatable, intent(inout) :: val

   integer :: m

   if (.not.allocated(self%lst)) then
      call resize(self%lst, initial_size)
   end if

   m = size(self%lst)
   if (self%n >= m) then
      call resize(self%lst, m + m/2 + 1)
   end if

   self%n = self%n + 1
   call move_alloc(val, self%lst(self%n)%val)

end subroutine push_back


!> Remove the first element from the data structure
subroutine shift(self, val)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   integer :: i

   if (self%n > 0) then
      call move_alloc(self%lst(1)%val, val)
      do i = 2, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine shift


!> Remove the last element from the data structure
subroutine pop(self, val)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   if (self%n > 0) then
      call move_alloc(self%lst(self%n)%val, val)
      self%n = self%n - 1
   end if

end subroutine pop


!> Get list of all keys in the structure
subroutine get_keys(self, list)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   integer :: i

   allocate(list(self%n))

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (allocated(self%lst(i)%val%key)) then
            list(i)%key = self%lst(i)%val%key
         end if
      end if
   end do

end subroutine get_keys


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   integer :: idx, i

   idx = 0
   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         if (self%lst(i)%val%match_key(key)) then
            idx = i
            exit
         end if
      end if
   end do

   if (idx > 0) then
      call self%lst(idx)%val%destroy
      do i = idx+1, self%n
         call move_alloc(self%lst(i)%val, self%lst(i-1)%val)
      end do
      self%n = self%n - 1
   end if

end subroutine delete


!> Change size of the TOML value vector
subroutine resize(list, n)

   !> Array of TOML values to be resized
   type(toml_node), allocatable, intent(inout), target :: list(:)

   !> New size of the list
   integer, intent(in) :: n

   type(toml_node), allocatable, target :: tmp(:)
   integer :: i


   if (allocated(list)) then
      call move_alloc(list, tmp)
      allocate(list(n))

      do i = 1, min(size(tmp), n)
         if (allocated(tmp(i)%val)) then
            call move_alloc(tmp(i)%val, list(i)%val)
         end if
      end do

      do i = n+1, size(tmp)
         if (allocated(tmp(i)%val)) then
            call tmp(i)%val%destroy
            deallocate(tmp(i)%val)
         end if
      end do

      deallocate(tmp)
   else
      allocate(list(n))
   end if

end subroutine resize


!> Deconstructor for data structure
subroutine destroy(self)

   !> Instance of the structure
   class(toml_vector), intent(inout), target :: self

   integer :: i

   do i = 1, self%n
      if (allocated(self%lst(i)%val)) then
         call self%lst(i)%val%destroy
      end if
   end do

   deallocate(self%lst)
   self%n = 0

end subroutine destroy


end module tomlf_structure_vector
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/structure.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Abstraction layer for the actual storage of the data structure.
!>
!> The structure implementations provide the actual storage for TOML values, with
!> a generic enough interface to make the definition of the TOML data structures
!> independent of the actual algorithm used for storing the TOML values.
!>
!> Every data structure defined here should strive to only use allocatable
!> data types and limit the use of pointer attributes as they interfer with
!> the automatic memory management of Fortran. A well defined data structure
!> in allocatables allows deep-copying of TOML values by assignment, data structures
!> requiring pointer attributes have to define an assignment(=) interface to
!> allow deep-copying of TOML values.
module tomlf_structure
   use tomlf_structure_base, only : toml_structure, toml_ordered
   use tomlf_structure_vector, only : toml_vector, new_vector
   implicit none
   private

   public :: toml_structure, toml_ordered
   public :: new_structure, new_ordered
   public :: len


   !> Overload len function
   interface len
      module procedure :: get_len
   end interface


contains


!> Constructor for the storage data structure
subroutine new_structure(self)

   !> Instance of the structure
   class(toml_structure), allocatable, intent(out) :: self

   type(toml_vector), allocatable :: vect

   allocate(vect)
   call new_vector(vect)
   call move_alloc(vect, self)

end subroutine new_structure


!> Constructor for the ordered storage data structure
subroutine new_ordered(self)

   !> Instance of the structure
   class(toml_ordered), allocatable, intent(out) :: self

   type(toml_vector), allocatable :: vect

   allocate(vect)
   call new_vector(vect)
   call move_alloc(vect, self)

end subroutine new_ordered


!> Get number of TOML values in the structure
pure function get_len(self) result(length)

   !> Instance of the structure
   class(toml_ordered), intent(in) :: self

   !> Current length of the ordered structure
   integer :: length

   length = self%get_len()

end function get_len


end module tomlf_structure
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/type/array.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of the TOML array data type.
module tomlf_type_array
   use tomlf_error, only : toml_stat
   use tomlf_type_value, only : toml_value, toml_visitor
   use tomlf_structure, only : toml_ordered, new_ordered
   implicit none
   private

   public :: toml_array, new_array, new, len


   !> TOML array
   type, extends(toml_value) :: toml_array

      !> Is an inline array rather than an array of tables
      logical :: inline = .true.

      !> Storage unit for TOML values of this array
      class(toml_ordered), allocatable :: list

   contains

      !> Get the TOML value at a given index
      procedure :: get

      !> Append value to array
      procedure :: push_back

      !> Remove the first element from the array
      procedure :: shift

      !> Remove the last element from the array
      procedure :: pop

      !> Release allocation hold by TOML array
      procedure :: destroy

   end type toml_array


   !> Create standard constructor
   interface toml_array
      module procedure :: new_array_func
   end interface toml_array


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_array
   end interface


   !> Overload len function
   interface len
      module procedure :: get_len
   end interface


contains


!> Constructor to create a new TOML array and allocate the internal storage
subroutine new_array(self)

   !> Instance of the TOML array
   type(toml_array), intent(out) :: self

   call new_ordered(self%list)

end subroutine new_array


!> Default constructor for TOML array type
function new_array_func() result(self)

   !> Instance of the TOML array
   type(toml_array) :: self

   call new_array(self)

end function new_array_func


!> Get number of TOML values in the array
pure function get_len(self) result(length)

   !> Instance of the TOML array
   class(toml_array), intent(in) :: self

   !> Current length of the array
   integer :: length

   length = self%list%get_len()

end function get_len


!> Get the TOML value at the respective index
subroutine get(self, idx, ptr)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> Index to the TOML value
   integer, intent(in) :: idx

   !> Pointer to the TOML value
   class(toml_value), pointer, intent(out) :: ptr

   call self%list%get(idx, ptr)

end subroutine get


!> Push back a TOML value to the array
subroutine push_back(self, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to append to array
   class(toml_value), allocatable, intent(inout) :: val

   !> Status of operation
   integer, intent(out) :: stat

   if (allocated(val%key)) then
      stat = toml_stat%fatal
      return
   end if

   call self%list%push_back(val)

   stat = toml_stat%success

end subroutine push_back


!> Remove the first element from the data structure
subroutine shift(self, val)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   call self%list%shift(val)

end subroutine shift


!> Remove the last element from the data structure
subroutine pop(self, val)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   !> TOML value to be retrieved
   class(toml_value), allocatable, intent(out) :: val

   call self%list%pop(val)

end subroutine pop


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%list)) then
      call self%list%destroy
      deallocate(self%list)
   end if

end subroutine destroy


end module tomlf_type_array
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/type/table.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of the TOML table data type.
!>
!> Every TOML document contains at least one (root) table which holds key-value
!> pairs, arrays and other tables.
module tomlf_type_table
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   use tomlf_structure, only : toml_structure, new_structure
   implicit none
   private

   public :: toml_table, new_table, new


   !> TOML table
   type, extends(toml_value) :: toml_table

      !> Table was implictly created
      logical :: implicit = .false.

      !> Is an inline table and is therefore non-extendable
      logical :: inline = .false.

      !> Storage unit for TOML values of this table
      class(toml_structure), allocatable :: list

   contains

      !> Get the TOML value associated with the respective key
      procedure :: get

      !> Get list of all keys in this table
      procedure :: get_keys

      !> Check if key is already present in this table instance
      procedure :: has_key

      !> Append value to table (checks automatically for key)
      procedure :: push_back

      !> Delete TOML value at a given key
      procedure :: delete

      !> Release allocation hold by TOML table
      procedure :: destroy

   end type toml_table


   !> Create standard constructor
   interface toml_table
      module procedure :: new_table_func
   end interface toml_table


   !> Overloaded constructor for TOML values
   interface new
      module procedure :: new_table
   end interface


contains


!> Constructor to create a new TOML table and allocate the internal storage
subroutine new_table(self)

   !> Instance of the TOML table
   type(toml_table), intent(out) :: self

   call new_structure(self%list)

end subroutine new_table


!> Default constructor for TOML table type
function new_table_func() result(self)

   !> Instance of the TOML table
   type(toml_table) :: self

   call new_table(self)

end function new_table_func


!> Get the TOML value associated with the respective key
subroutine get(self, key, ptr)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the TOML value
   class(toml_value), pointer, intent(out) :: ptr

   call self%list%find(key, ptr)

end subroutine get


!> Get list of all keys in this table
subroutine get_keys(self, list)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> List of all keys
   type(toml_key), allocatable, intent(out) :: list(:)

   call self%list%get_keys(list)

end subroutine get_keys


!> Check if a key is present in the table
function has_key(self, key) result(found)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   !> TOML value is present in table
   logical :: found

   class(toml_value), pointer :: ptr

   call self%list%find(key, ptr)

   found = associated(ptr)

end function has_key


!> Push back a TOML value to the table
subroutine push_back(self, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> TOML value to append to table
   class(toml_value), allocatable, intent(inout) :: val

   !> Status of operation
   integer, intent(out) :: stat

   if (.not.allocated(val)) then
      stat = toml_stat%fatal
      return
   end if

   if (.not.allocated(val%key)) then
      stat = toml_stat%fatal
      return
   end if

   if (self%has_key(val%key)) then
      stat = toml_stat%duplicate_key
      return
   end if

   call self%list%push_back(val)

   stat = toml_stat%success

end subroutine push_back


!> Delete TOML value at a given key
subroutine delete(self, key)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   !> Key to the TOML value
   character(kind=tfc, len=*), intent(in) :: key

   call self%list%delete(key)

end subroutine delete


!> Deconstructor to cleanup allocations (optional)
subroutine destroy(self)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: self

   if (allocated(self%key)) then
      deallocate(self%key)
   end if

   if (allocated(self%list)) then
      call self%list%destroy
      deallocate(self%list)
   end if

end subroutine destroy


end module tomlf_type_table
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/type.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Collection of the central datatypes to define TOML data structures
!>
!> All TOML data types should inherit from an abstract value allowing to generate
!> a generic interface to deal with all more specialized TOML data types, while
!> the abstract value is interesting for developing algorithms in TOML-Fortran,
!> the user of TOML-Fortran will usually only care about TOML tables and possibly
!> arrays.
!>
!> The TOML types defined here should implement the TOML data structures (mostly)
!> without taking the actual implementation of the data structures into account.
!> This is done by providing a bare minimum interface using type bound procedures
!> to minimize the interdependencies between the datatypes.
!>
!> To make the data types extendable a visitor pattern allows access to the TOML
!> data types and can be used to implement further algorithms.
module tomlf_type
   use tomlf_constants, only : tfc
   use tomlf_error, only : toml_stat
   use tomlf_type_array, only : toml_array, new_array, new, len
   use tomlf_type_keyval, only : toml_keyval, new_keyval, new
   use tomlf_type_table, only : toml_table, new_table, new
   use tomlf_type_value, only : toml_value, toml_visitor, toml_key
   implicit none
   private

   public :: toml_value, toml_visitor, toml_table, toml_array, toml_keyval
   public :: toml_key
   public :: new, new_table, new_array, new_keyval, len
   public :: add_table, add_array, add_keyval
   public :: is_array_of_tables


   !> Interface to build new tables
   interface add_table
      module procedure :: add_table_to_table
      module procedure :: add_table_to_array
   end interface add_table


   !> Interface to build new arrays
   interface add_array
      module procedure :: add_array_to_table
      module procedure :: add_array_to_array
   end interface add_array


   !> Interface to build new key-value pairs
   interface add_keyval
      module procedure :: add_keyval_to_table
      module procedure :: add_keyval_to_array
   end interface add_keyval


contains


!> Create a new TOML table inside an existing table
subroutine add_table_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_table)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_table_to_table


!> Create a new TOML array inside an existing table
subroutine add_array_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new array
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_array_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_array)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_array_to_table


!> Create a new key-value pair inside an existing table
subroutine add_keyval_to_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new key-value pair
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(val)
   val%key = key
   call table%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call table%get(key, tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_keyval_to_table


!> Create a new TOML table inside an existing array
subroutine add_table_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_table_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_table)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_table_to_array


!> Create a new TOML array inside an existing array
subroutine add_array_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   allocate(toml_array :: val)
   call new_array_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_array)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_array_to_array


!> Create a new key-value pair inside an existing array
subroutine add_keyval_to_array(array, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Pointer to the newly created key-value pair
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), allocatable :: val
   class(toml_value), pointer :: tmp
   integer :: istat

   nullify(ptr)
   call new_keyval_(val)
   call array%push_back(val, istat)

   if (allocated(val)) then
      call val%destroy
      if (present(stat)) stat = toml_stat%fatal
      return
   end if

   if (istat == toml_stat%success) then
      call array%get(len(array), tmp)
      if (.not.associated(tmp)) then
         if (present(stat)) stat = toml_stat%fatal
         return
      end if

      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
      class default
         istat = toml_stat%fatal
      end select
   end if

   if (present(stat)) stat = istat

end subroutine add_keyval_to_array


!> Wrapped constructor to create a new TOML table on an abstract TOML value
subroutine new_table_(self)

   !> Newly created TOML table
   class(toml_value), allocatable, intent(out) :: self

   type(toml_table), allocatable :: val

   allocate(val)
   call new_table(val)
   call move_alloc(val, self)

end subroutine new_table_


!> Wrapped constructor to create a new TOML array on an abstract TOML value
subroutine new_array_(self)

   !> Newly created TOML array
   class(toml_value), allocatable, intent(out) :: self

   type(toml_array), allocatable :: val

   allocate(val)
   call new_array(val)
   call move_alloc(val, self)

end subroutine new_array_


!> Wrapped constructor to create a new TOML array on an abstract TOML value
subroutine new_keyval_(self)

   !> Newly created key-value pair
   class(toml_value), allocatable, intent(out) :: self

   type(toml_keyval), allocatable :: val

   allocate(val)
   call new_keyval(val)
   call move_alloc(val, self)

end subroutine new_keyval_


!> Determine if array contains only tables
function is_array_of_tables(array) result(only_tables)

   !> TOML value to visit
   class(toml_array), intent(inout) :: array

   !> Array contains only tables
   logical :: only_tables

   class(toml_value), pointer :: ptr
   integer :: i, n


   n = len(array)
   only_tables = n > 0

   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      type is(toml_table)
         cycle
      class default
         only_tables = .false.
         exit
      end select
   end do

end function is_array_of_tables


end module tomlf_type
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/ser.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> TOML serializer implementation
module tomlf_ser
   use tomlf_constants, only : tfc, tfout
   use tomlf_type, only : toml_value, toml_visitor, toml_key, toml_table, &
      & toml_array, toml_keyval, is_array_of_tables, len
   implicit none
   private

   public :: toml_serializer, new_serializer, new


   !> TOML serializer to produduce a TOML document from a datastructure
   type, extends(toml_visitor) :: toml_serializer

      !> Unit for output
      integer :: unit = tfout

      !> Special mode for printing array of tables
      logical, private :: array_of_tables = .false.

      !> Special mode for printing inline arrays
      logical, private :: inline_array = .false.

      !> Top of the key stack
      integer, private :: top = 0

      !> Key stack to create table headers
      type(toml_key), allocatable, private :: stack(:)

   contains

      !> Visit a TOML value
      procedure :: visit

   end type toml_serializer


   !> Create standard constructor
   interface toml_serializer
      module procedure :: new_serializer_func
   end interface toml_serializer


   !> Overloaded constructor for TOML serializers
   interface new
      module procedure :: new_serializer
   end interface


   !> Initial size of the key path stack
   integer, parameter :: initial_size = 8


contains


!> Constructor to create new serializer instance
subroutine new_serializer(self, unit)

   !> Instance of the TOML serializer
   type(toml_serializer), intent(out) :: self

   !> Unit for IO
   integer, intent(in), optional :: unit

   if (present(unit)) then
      self%unit = unit
   end if

end subroutine new_serializer


!> Default constructor for TOML serializer
function new_serializer_func(unit) result(self)

   !> Unit for IO
   integer, intent(in), optional :: unit

   !> Instance of the TOML serializer
   type(toml_serializer) :: self

   call new_serializer(self, unit)

end function new_serializer_func


!> Visit a TOML value
recursive subroutine visit(self, val)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: self

   !> TOML value to visit
   class(toml_value), intent(inout) :: val

   select type(val)
   class is(toml_keyval)
      call visit_keyval(self, val)
   class is(toml_array)
      call visit_array(self, val)
   class is(toml_table)
      call visit_table(self, val)
   end select

end subroutine visit


!> Visit a TOML key-value pair
subroutine visit_keyval(visitor, keyval)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_keyval), intent(inout) :: keyval

   character(kind=tfc, len=:), allocatable :: key

   call keyval%get_key(key)

   if (visitor%inline_array) then
      write(visitor%unit, '(1x,a,1x,"=",1x,a,",")', advance='no') &
         &  key, keyval%raw
   else
      write(visitor%unit, '(a,1x,"=",1x,a)') key, keyval%raw
   end if

end subroutine visit_keyval


!> Visit a TOML array
recursive subroutine visit_array(visitor, array)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML value to visit
   type(toml_array), intent(inout) :: array

   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   if (visitor%inline_array) write(visitor%unit, '(1x,"[")', advance='no')
   n = len(array)
   do i = 1, n
      call array%get(i, ptr)
      select type(ptr)
      class is(toml_keyval)
         write(visitor%unit, '(1x,a)', advance='no') ptr%raw
         if (i /= n) write(visitor%unit, '(",")', advance='no')
      class is(toml_array)
         call ptr%accept(visitor)
         if (i /= n) write(visitor%unit, '(",")', advance='no')
      class is(toml_table)
         if (visitor%inline_array) then
            write(visitor%unit, '(1x,"{")', advance='no')
            call ptr%accept(visitor)
            write(visitor%unit, '(1x,"}")', advance='no')
            if (i /= n) write(visitor%unit, '(",")', advance='no')
         else
            visitor%array_of_tables = .true.
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call array%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end if
      end select
   end do
   if (visitor%inline_array) write(visitor%unit, '(1x,"]")', advance='no')

end subroutine visit_array


!> Visit a TOML table
recursive subroutine visit_table(visitor, table)

   !> Instance of the TOML serializer
   class(toml_serializer), intent(inout) :: visitor

   !> TOML table to visit
   type(toml_table), intent(inout) :: table

   class(toml_value), pointer :: ptr
   type(toml_key), allocatable :: list(:)
   logical, allocatable :: defer(:)
   character(kind=tfc, len=:), allocatable :: key
   integer :: i, n

   call table%get_keys(list)

   n = size(list, 1)
   allocate(defer(n))

   if (.not.allocated(visitor%stack)) then
      call resize(visitor%stack)
   else
      if (.not.(visitor%inline_array .or. table%implicit)) then
         write(visitor%unit, '("[")', advance='no')
         if (visitor%array_of_tables) write(visitor%unit, '("[")', advance='no')
         do i = 1, visitor%top-1
            write(visitor%unit, '(a,".")', advance='no') visitor%stack(i)%key
         end do
         write(visitor%unit, '(a)', advance='no') visitor%stack(visitor%top)%key
         write(visitor%unit, '("]")', advance='no')
         if (visitor%array_of_tables) write(visitor%unit, '("]")', advance='no')
         write(visitor%unit, '(a)')
         visitor%array_of_tables = .false.
      end if
   end if

   do i = 1, n
      defer(i) = .false.
      call table%get(list(i)%key, ptr)
      select type(ptr)
      class is(toml_keyval)
         call ptr%accept(visitor)
      class is(toml_array)
         if (visitor%inline_array) then
            call ptr%get_key(key)
            write(visitor%unit, '(1x,a,1x,"=")', advance='no') key
            call ptr%accept(visitor)
            if (i /= n) write(visitor%unit, '(",")', advance='no')
         else
            if (is_array_of_tables(ptr)) then
               ! Array of tables open a new section
               ! -> cannot serialize them before all key-value pairs are done
               defer(i) = .true.
            else
               visitor%inline_array = .true.
               call ptr%get_key(key)
               write(visitor%unit, '(a,1x,"=")', advance='no') key
               call ptr%accept(visitor)
               visitor%inline_array = .false.
               write(visitor%unit, '(a)')
            end if
         end if
      class is(toml_table)
         ! Subtables open a new section
         ! -> cannot serialize them before all key-value pairs are done
         defer(i) = .true.
      end select
   end do

   do i = 1, n
      if (defer(i)) then
         call table%get(list(i)%key, ptr)
         select type(ptr)
         class is(toml_keyval)
            call ptr%accept(visitor)
         class is(toml_array)
            if (visitor%inline_array) then
               call ptr%get_key(key)
               write(visitor%unit, '(1x,a,1x,"=")', advance='no') key
               call ptr%accept(visitor)
               if (i /= n) write(visitor%unit, '(",")', advance='no')
            else
               if (is_array_of_tables(ptr)) then
                  call ptr%accept(visitor)
               else
                  visitor%inline_array = .true.
                  call ptr%get_key(key)
                  write(visitor%unit, '(a,1x,"=")', advance='no') key
                  call ptr%accept(visitor)
                  visitor%inline_array = .false.
                  write(visitor%unit, '(a)')
               end if
            end if
         class is(toml_table)
            if (size(visitor%stack, 1) <= visitor%top) call resize(visitor%stack)
            visitor%top = visitor%top + 1
            call ptr%get_key(key)
            visitor%stack(visitor%top)%key = key
            call ptr%accept(visitor)
            deallocate(visitor%stack(visitor%top)%key)
            visitor%top = visitor%top - 1
         end select
      end if
   end do

   if (.not.visitor%inline_array .and. visitor%top == 0) then
      deallocate(visitor%stack)
   end if

end subroutine visit_table


!> Change size of the stack
subroutine resize(stack, n)

   !> Stack of keys to be resized
   type(toml_key), allocatable, intent(inout) :: stack(:)

   !> New size of the stack
   integer, intent(in), optional :: n

   type(toml_key), allocatable :: tmp(:)
   integer :: m

   if (present(n)) then
      m = n
   else
      if (allocated(stack)) then
         m = size(stack)
         m = m + m/2 + 1
      else
         m = initial_size
      end if
   end if

   if (allocated(stack)) then
      call move_alloc(stack, tmp)
      allocate(stack(m))

      m = min(size(tmp), m)
      stack(:m) = tmp(:m)

      deallocate(tmp)
   else
      allocate(stack(m))
   end if

end subroutine resize


end module tomlf_ser
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/de/tokenizer.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Definition of the TOML tokens and the possible states of the tokenizer
!>
!> The tokenizer implementation has to produce tokens from any input source
!> and is usually only required for string tokens to provide an actual character
!> representation.
!>
!> The tokenization is partly dependent on the context, as the dot is not in
!> all states actually a token, also due to the rather complex syntax of
!> table headers, whitespace is precious and has to be reported as token.
!>
!> Not required but usually helpful is the creation of a context, usually
!> represented by the current line (or a chunk of lines for multiline strings),
!> which can be passed to the error handler to create more detailed output.
!> A tokenizer working with the complete TOML document as character sequence
!> can easily create the context, while it might be incomplete or missing in
!> case of a stream processing.
module tomlf_de_tokenizer
   use tomlf_constants, only : toml_escape, tfc, TOML_BAREKEY, toml_type
   use tomlf_error, only : toml_stat, toml_error, toml_context, &
      & syntax_error, duplicate_key_error, vendor_error
   use tomlf_utils
   use tomlf_type, only : toml_value, toml_key, toml_table, toml_array, &
      & toml_keyval, new_table, add_array, add_table, add_keyval, len
   implicit none
   private

   public :: toml_tokenizer, toml_token, toml_tokentype


   type :: enum_tokentype

      integer :: invalid = 0

      integer :: dot = 1

      integer :: comma = 2

      integer :: equal = 3

      integer :: lbrace = 4

      integer :: rbrace = 5

      integer :: whitespace = 6

      integer :: newline = 7

      integer :: lbracket = 8

      integer :: rbracket = 9

      integer :: string = 10

      integer :: comment = -1

   end type enum_tokentype

   type(enum_tokentype), parameter :: toml_tokentype = enum_tokentype()


   !> Basic TOML token, produced by a TOML tokenizer
   type :: toml_token

      !> Actual tokentype
      integer :: tok = toml_tokentype%invalid

      !> Character representation of the token
      character(len=:), pointer :: ptr => null()

      !> Length of the token at ptr
      integer :: len = 0

   end type toml_token


   !> Abstract TOML tokenizer
   type, abstract :: toml_tokenizer

      !> Signals if the tokenizer has finished (EOF has been reached)
      logical :: finished = .false.

      !> Current token
      type(toml_token) :: tok

      !> Root table
      type(toml_table), allocatable :: root

      !> Pointer to the current table while transversing a table path
      type(toml_table), pointer :: current => null()

      !> Current line (for error handling)
      type(toml_context) :: line

      !> Error buffer, if allocated an error has occurred
      type(toml_error), allocatable :: error

   contains

      !> Entry point for parsing the TOML document, creates the root table
      procedure :: parse => parse_root

      !> Parse a TOML table or array of tables header
      procedure, private :: parse_select

      !> Parse an inline TOML array
      procedure, private :: parse_array

      !> Parse an inline TOML table
      procedure, private :: parse_table

      !> Parse a key-value pair
      procedure, private :: parse_keyval

      !> Advance tokenizer
      procedure, private :: next

      !> Return next token
      procedure(next_token), deferred :: next_token

   end type toml_tokenizer


   abstract interface
      !> Return next token
      subroutine next_token(de, dot_is_token)
         import :: toml_tokenizer

         !> Instance of the tokenizer
         class(toml_tokenizer), intent(inout) :: de

         !> Dot should be handled as token
         logical, intent(in) :: dot_is_token
      end subroutine next_token
   end interface


contains


!> Entry point for parsing the TOML document, creates the root table
subroutine parse_root(de)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   allocate(de%root)
   call new_table(de%root)
   de%current => de%root

   do while(.not.de%finished)
      select case(de%tok%tok)
      case default
         call syntax_error(de%error, de%line, "syntax error")
         exit

      case(toml_tokentype%newline)
         call de%next(.true.)

      case(toml_tokentype%string)
         call de%parse_keyval(de%current)
         if (allocated(de%error)) exit
         if (de%tok%tok /= toml_tokentype%newline) then
            call syntax_error(de%error, de%line, "extra characters after value present")
            exit
         end if

      case(toml_tokentype%lbracket)
         call parse_select(de)
         if (allocated(de%error)) exit

      end select
   end do

end subroutine parse_root


!> Parse a key-value pair
recursive subroutine parse_keyval(de, table)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   !> Current TOML table
   type(toml_table), intent(inout) :: table

   type(toml_token) :: key
   type(toml_keyval), pointer :: vptr
   type(toml_array), pointer :: aptr
   type(toml_table), pointer :: tptr
   character(kind=tfc, len=:), allocatable :: new_key, this_key

   key = de%tok
   !@:ASSERT(de%tok%tok == STRING)
   call de%next(.true.)

   if (de%tok%tok == toml_tokentype%dot) then
      ! create new key from token
      call key_from_token(this_key, key)
      call get_table(table, this_key, tptr)
      deallocate(this_key)
      if (tptr%inline) then
         call syntax_error(de%error, de%line, "Cannot add keys to inline tables")
         return
      end if
      call de%next(.true.)
      if (de%tok%tok == toml_tokentype%string) then
         call de%parse_keyval(tptr)
      else
         call syntax_error(de%error, de%line, "invalid key")
      end if
      return
   end if

   if (de%tok%tok /= toml_tokentype%equal) then
      call syntax_error(de%error, de%line, "missing =")
      return
   end if

   call de%next(.false.)
   if (allocated(de%error)) return

   ! create new key from token
   call key_from_token(new_key, key)
   if (.not.allocated(new_key)) then
      call syntax_error(de%error, de%line, "invalid key")
      return
   end if

   select case(de%tok%tok)
   case default
      call syntax_error(de%error, de%line, "unexpected token")
      return

   case(toml_tokentype%string) ! key = "value"
      call add_keyval(table, new_key, vptr)
      if (.not.associated(vptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      vptr%raw = de%tok%ptr(:de%tok%len)
      if (toml_get_value_type(vptr%raw) == toml_type%invalid) then
         call syntax_error(de%error, de%line, "unknown value type")
         return
      end if
      call de%next(.true.)
      if (allocated(de%error)) return

   case(toml_tokentype%lbracket) ! key = [ array ]
      call add_array(table, new_key, aptr)
      if (.not.associated(aptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      aptr%inline = .true.
      call de%parse_array(aptr)
      if (allocated(de%error)) return

   case(toml_tokentype%lbrace) ! key = { table }
      call add_table(table, new_key, tptr)
      if (.not.associated(tptr)) then
         call duplicate_key_error(de%error, de%line, new_key)
         return
      end if
      call de%parse_table(tptr)
      tptr%inline = .true.
      if (allocated(de%error)) return

   end select

end subroutine parse_keyval


!> Parse a TOML table or array of tables header
subroutine parse_select(de)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   type(toml_array), pointer :: array
   type(toml_table), pointer :: table
   class(toml_value), pointer :: ptr
   character(kind=tfc, len=:), allocatable :: key
   logical :: llb

   integer, parameter :: initial_size = 8

   integer :: top
   type(toml_key), allocatable :: stack(:)

   nullify(table)

   !@:assert(de%tok%tok == toml_tokentype%lbracket)
   call de%next(.true., whitespace_is_precious=.true.)

   llb = de%tok%tok == toml_tokentype%lbracket

   if (llb .or. de%tok%tok == toml_tokentype%whitespace) then
      call de%next(.true.)
   end if

   call fill_stack(de, top, stack)
   if (allocated(de%error)) return

   ! remove topmost element from path
   call move_alloc(stack(top)%key, key)
   top = top - 1

   call walk_stack(de, top, stack)
   if (allocated(de%error)) return

   if (llb) then
      ! [[key.key.top]]
      call de%current%get(key, ptr)
      if (associated(ptr)) then
         select type(ptr)
         type is(toml_array)
            array => ptr
         class default
            call duplicate_key_error(de%error, de%line, key)
            return
         end select
      else
         call add_array(de%current, key, array)
         array%inline = .false.
      end if
      if (array%inline) then
         call syntax_error(de%error, de%line, "Cannot use inline array in array of tables")
         return
      end if
      call add_table(array, table)
   else
      ! [key.key.top]
      call de%current%get(key, ptr)
      if (associated(ptr)) then
         select type(ptr)
         type is(toml_table)
            if (ptr%implicit) then
               table => ptr
            else
               call duplicate_key_error(de%error, de%line, key)
               return
            end if
         class default
            call duplicate_key_error(de%error, de%line, key)
            return
         end select
      else
         call add_table(de%current, key, table)
      end if
   end if

   if (.not.associated(table)) then
      call syntax_error(de%error, de%line, "Cannot add table in this context")
      return
   end if
   de%current => table

   if (de%tok%tok /= toml_tokentype%rbracket) then
      call syntax_error(de%error, de%line, "expects ]")
      return
   end if
   call de%next(.true., whitespace_is_precious=llb)
   if (llb) then
      if (de%tok%tok /= toml_tokentype%rbracket) then
         call syntax_error(de%error, de%line, "expects ]]")
         return
      end if
      call de%next(.true.)
   end if

   if (de%tok%tok /= toml_tokentype%newline) then
      call syntax_error(de%error, de%line, "extra chars after ] or ]]")
      return
   end if


contains


   !> Fill the stack with tokens
   subroutine fill_stack(de, top, stack)

      !> Instance of the TOML deserializer
      class(toml_tokenizer), intent(inout), target :: de

      !> Depth of the table key stack
      integer, intent(out) :: top

      !> Stack of all keys in the table header
      type(toml_key), allocatable, intent(out) :: stack(:)

      top = 0
      allocate(stack(initial_size))

      do
         if (top >= size(stack)) then
            call resize(stack)
         end if

         if (de%tok%tok /= toml_tokentype%string) then
            call syntax_error(de%error, de%line, "invalid or missing key")
            return
         end if

         top = top + 1
         call key_from_token(stack(top)%key, de%tok)
         if (.not.allocated(stack(top)%key)) then
            call syntax_error(de%error, de%line, "invalid key")
            return
         end if

         call de%next(.true.)

         if (de%tok%tok == toml_tokentype%rbracket) exit

         if (de%tok%tok /= toml_tokentype%dot) then
            call syntax_error(de%error, de%line, "invalid key")
            return
         end if

         call de%next(.true.)
      end do

      if (top <= 0) then
         call syntax_error(de%error, de%line, "empty table selector")
      end if

   end subroutine fill_stack


   !> Walk the key stack to fetch the correct table, create implicit tables as
   !  necessary
   subroutine walk_stack(de, top, stack)

      !> Instance of the TOML deserializer
      class(toml_tokenizer), intent(inout), target :: de

      !> Depth of the table key stack
      integer, intent(in) :: top

      !> Stack of all keys in the table header
      type(toml_key), intent(in), target :: stack(:)

      type(toml_table), pointer :: table, tmp_tbl
      character(kind=tfc, len=:), pointer :: key
      class(toml_value), pointer :: ptr, tmp
      integer :: i

      table => de%root

      do i = 1, top
         key => stack(i)%key

         if (.not.table%has_key(key)) then
            call add_table(table, key, tmp_tbl)
            if (associated(tmp_tbl)) then
               tmp_tbl%implicit = .true.
            end if
         end if
         call table%get(key, ptr)

         select type(ptr)
         type is(toml_table)
            table => ptr

         type is(toml_array)
            call ptr%get(len(ptr), tmp)
            select type(tmp)
            type is(toml_table)
               table => tmp
            class default
               call vendor_error(de%error, de%line)
               return
            end select

         class default
            call duplicate_key_error(de%error, de%line, key)
            return
         end select
      end do

      de%current => table

   end subroutine walk_stack


   !> Change size of the stack
   subroutine resize(stack, n)

      !> Stack of keys to be resized
      type(toml_key), allocatable, intent(inout) :: stack(:)

      !> New size of the stack
      integer, intent(in), optional :: n

      type(toml_key), allocatable :: tmp(:)
      integer :: m

      if (present(n)) then
         m = n
      else
         if (allocated(stack)) then
            m = size(stack)
            m = m + m/2 + 1
         else
            m = initial_size
         end if
      end if

      if (allocated(stack)) then
         call move_alloc(stack, tmp)
         allocate(stack(m))

         m = min(size(tmp), m)
         stack(:m) = tmp(:m)

         deallocate(tmp)
      else
         allocate(stack(m))
      end if
   end subroutine resize


end subroutine parse_select


!> Parse an inline TOML array
recursive subroutine parse_array(de, array)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   !> TOML array to be filled
   type(toml_array), intent(inout) :: array

   type(toml_table), pointer :: tbl
   type(toml_keyval), pointer :: val
   type(toml_array), pointer :: arr

   !@:assert(de%tok%tok == toml_tokentype%lbracket)

   call de%next(.false.)
   do while(.not.allocated(de%error))
      do while(de%tok%tok == toml_tokentype%newline)
         call de%next(.false.)
      end do
      if (de%tok%tok == toml_tokentype%rbracket) then
         exit
      end if

      select case(de%tok%tok)
      case default
         call syntax_error(de%error, de%line, "unexpected token")
         return

      case(toml_tokentype%string) ! [ value, value ... ]
         call add_keyval(array, val)
         val%raw = de%tok%ptr(:de%tok%len)

         call de%next(.false.)

      case(toml_tokentype%lbracket) ! [ [array], [array] ...]
         call add_array(array, arr)
         arr%inline = .true.
         call de%parse_array(arr)
         if (allocated(de%error)) return

      case(toml_tokentype%lbrace) ! [ {table}, {table} ... ]
         call add_table(array, tbl)
         tbl%inline = .true.
         call de%parse_table(tbl)
         if (allocated(de%error)) return

      end select

      do while(de%tok%tok == toml_tokentype%newline)
         call de%next(.false.)
      end do

      if (de%tok%tok == toml_tokentype%comma) then
         call de%next(.false.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= toml_tokentype%rbracket) then
      call syntax_error(de%error, de%line, "expects ]")
      return
   end if

   call de%next(.true.)

end subroutine parse_array


!> Parse an inline TOML table
recursive subroutine parse_table(de, table)

   !> Instance of the TOML deserializer
   class(toml_tokenizer), intent(inout), target :: de

   !> TOML table to be filled
   type(toml_table), intent(inout) :: table

   !@:ASSERT(de%tok%tok == LBRACE)
   call de%next(.true.)
   do
      if (de%tok%tok == toml_tokentype%newline) then
         call syntax_error(de%error, de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == toml_tokentype%rbrace) exit

      if (de%tok%tok /= toml_tokentype%string) then
         call syntax_error(de%error, de%line, "expects string value")
         return
      end if

      call de%parse_keyval(table)
      if (allocated(de%error)) exit

      if (de%tok%tok == toml_tokentype%string) then
         call syntax_error(de%error, de%line, "newline not allowed in inline table")
         return
      end if

      if (de%tok%tok == toml_tokentype%comma) then
         call de%next(.true.)
         cycle
      end if
      exit
   end do

   if (de%tok%tok /= toml_tokentype%rbrace) then
      call syntax_error(de%error, de%line, "expects }")
      return
   end if

   call de%next(.true.)

end subroutine parse_table


!> Generate a key
subroutine key_from_token(key, tok)

   !> TOML raw key
   character(kind=tfc, len=:), allocatable, intent(out) :: key

   !> String token containing the possible key
   type(toml_token), intent(in) :: tok

   if (toml_raw_to_string(tok%ptr(:tok%len), key)) then
      if (index(key, toml_escape%newline) > 0) deallocate(key)
   else
      key = tok%ptr(:tok%len)
      if (verify(key, TOML_BAREKEY) > 0) deallocate(key)
   end if

end subroutine key_from_token


!> Try to retrieve TOML table with key or create it
subroutine get_table(table, key, ptr, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key for the new table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to the newly created table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_table)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      call add_table(table, key, ptr, stat)
   end if

end subroutine get_table


!> Return next token
subroutine next(de, dot_is_token, whitespace_is_precious)

   !> Instance of the tokenizer
   class(toml_tokenizer), intent(inout) :: de

   !> Dot should be handled as token
   logical, intent(in) :: dot_is_token

   !> Whitespace tokens should be skipped
   logical, intent(in), optional :: whitespace_is_precious

   logical :: skip_whitespace

   if (present(whitespace_is_precious)) then
      skip_whitespace = .not.whitespace_is_precious
   else
      skip_whitespace = .true.
   end if

   call de%next_token(dot_is_token)
   if (skip_whitespace) then
      do while(de%tok%tok == toml_tokentype%whitespace)
         if (allocated(de%error)) exit
         call de%next_token(dot_is_token)
      end do
   end if

end subroutine next


end module tomlf_de_tokenizer
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/build/keyval.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build a TOML values
!>
!> The build module defines an interface to work with TOML values instead
!> of accessing the raw value directly. Both setter and getter routines defined
!> here are rarely needed in any user context, but serve as a basic building
!> block to define uniform access methods for TOML tables and arrays.
module tomlf_build_keyval
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp, TOML_NEWLINE
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, len
   use tomlf_utils, only : toml_raw_to_string, toml_raw_to_float, &
      & toml_raw_to_bool, toml_raw_to_integer, toml_raw_to_timestamp, &
      & toml_raw_verify_string, toml_escape_string
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML values
   interface set_value
      module procedure :: set_value_float_sp
      module procedure :: set_value_float_dp
      module procedure :: set_value_integer_i1
      module procedure :: set_value_integer_i2
      module procedure :: set_value_integer_i4
      module procedure :: set_value_integer_i8
      module procedure :: set_value_bool
      module procedure :: set_value_string
   end interface set_value


   !> Getter functions to manipulate TOML values
   interface get_value
      module procedure :: get_value_float_sp
      module procedure :: get_value_float_dp
      module procedure :: get_value_integer_i1
      module procedure :: get_value_integer_i2
      module procedure :: get_value_integer_i4
      module procedure :: get_value_integer_i8
      module procedure :: get_value_bool
      module procedure :: get_value_string
   end interface get_value


   !> Length for the static character variables
   integer, parameter :: buffersize = 128


contains


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_value_float_sp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   real(tfr) :: dummy

   istat = toml_raw_to_float(self%raw, dummy)
   if (istat) then
      val = real(dummy, tf_sp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_value_float_dp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   real(tfr) :: dummy

   istat = toml_raw_to_float(self%raw, dummy)
   if (istat) then
      val = real(dummy, tf_dp)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_value_integer_i1(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i1)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_value_integer_i2(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i2)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_value_integer_i4(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i4)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_value_integer_i8(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat
   integer(tfi) :: dummy

   istat = toml_raw_to_integer(self%raw, dummy)
   if (istat) then
      val = int(dummy, tf_i8)
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_value_bool(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> Boolean value
   logical, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat

   istat = toml_raw_to_bool(self%raw, val)
   if (istat) then
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_bool


!> Retrieve TOML value as deferred-length character
subroutine get_value_string(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(in) :: self

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   logical :: istat

   istat = toml_raw_to_string(self%raw, val)
   if (istat) then
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_value_string


!> Set TOML value to single precision float
subroutine set_value_float_sp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(es30.6)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_float_sp


!> Set TOML value to double precision float
subroutine set_value_float_dp(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(es30.16)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_float_dp


!> Set TOML value to one byte integer
subroutine set_value_integer_i1(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_value_integer_i2(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_value_integer_i4(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_value_integer_i8(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(kind=tfc, len=buffersize) :: tmp
   integer :: istat

   write(tmp, '(i0)', iostat=istat) val
   if (istat == 0) then
      self%raw = trim(adjustl(tmp))
      if (present(stat)) stat = toml_stat%success
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_value_integer_i8


!> Set TOML value to logical
subroutine set_value_bool(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   if (val) then
      self%raw = 'true'
   else
      self%raw = 'false'
   end if

   if (present(stat)) stat = toml_stat%success

end subroutine set_value_bool


!> Set TOML value to deferred-length character
subroutine set_value_string(self, val, stat)

   !> Instance of the key-value pair
   class(toml_keyval), intent(inout) :: self

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   character(len=:), allocatable :: escaped

   if (toml_raw_verify_string(val)) then
      self%raw = val
   else
      call toml_escape_string(val, self%raw, .true.)
   end if

   if (present(stat)) stat = toml_stat%success

end subroutine set_value_string


end module tomlf_build_keyval
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/build/merge.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Merge TOML data structures.
!>
!> Merge policy:
!> - copy key-value pair in case it is not present in table
!> - copy subtable in case it is not present in table
!> - copy array in case it is not present in table
!> - merge subtable in case it is present in table
!> - append array in case it is present in table
module tomlf_build_merge
   use tomlf_constants, only : tfc
   use tomlf_type, only : toml_table, toml_array, toml_keyval, toml_value, &
      & toml_key, len
   implicit none
   private

   public :: merge_table, merge_array


contains


!> Merge TOML tables by appending their values
recursive subroutine merge_table(lhs, rhs)

   !> Instance of table to merge into
   class(toml_table), intent(inout) :: lhs

   !> Instance of table to be merged
   class(toml_table), intent(inout) :: rhs

   type(toml_key), allocatable :: list(:)
   class(toml_value), pointer :: ptr1, ptr2
   class(toml_value), allocatable :: tmp
   logical :: has_key
   integer :: i, n, stat

   call rhs%get_keys(list)
   n = size(list, 1)

   do i = 1, n
      if (allocated(tmp)) deallocate(tmp)
      call rhs%get(list(i)%key, ptr1)
      has_key = lhs%has_key(list(i)%key)
      select type(ptr1)
      class is(toml_keyval)
         if (.not.has_key) then
            allocate(tmp, source=ptr1)
            call lhs%push_back(tmp, stat)
         end if
      class is(toml_array)
         if (has_key) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_array)
               call merge_array(ptr2, ptr1)
            end select
         else
            allocate(tmp, source=ptr1)
            call lhs%push_back(tmp, stat)
         end if
      class is(toml_table)
         if (has_key) then
            call lhs%get(list(i)%key, ptr2)
            select type(ptr2)
            class is(toml_table)
               call merge_table(ptr2, ptr1)
            end select
         else
            allocate(tmp, source=ptr1)
            call lhs%push_back(tmp, stat)
         end if
      end select
   end do

end subroutine merge_table


!> Append values from one TOML array to another
recursive subroutine merge_array(lhs, rhs)

   !> Instance of array to merge into
   class(toml_array), intent(inout) :: lhs

   !> Instance of array to be merged
   class(toml_array), intent(inout) :: rhs

   class(toml_value), pointer :: ptr
   class(toml_value), allocatable :: tmp
   integer :: n, i, stat

   n = len(rhs)

   do i = 1, n
      call rhs%get(i, ptr)
      if (allocated(tmp)) deallocate(tmp)
      allocate(tmp, source=ptr)
      call lhs%push_back(tmp, stat)
   end do

end subroutine merge_array


end module tomlf_build_merge
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/de/character.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Implementation of a tokenizer for character variables
module tomlf_de_character
   use tomlf_constants
   use tomlf_error, only : syntax_error
   use tomlf_de_tokenizer
   use tomlf_utils
   implicit none
   private

   public :: toml_character_tokenizer, new_character_tokenizer, new


   !> Tokenizer for a sequence of characters
   type, extends(toml_tokenizer) :: toml_character_tokenizer

      !> Link to the input configuration.
      character(len=:), pointer :: conf

   contains

      !> Return next token
      procedure :: next_token

   end type toml_character_tokenizer


   interface new
      module procedure :: new_character_tokenizer
   end interface new


contains


!> Constructor for the deserializer implementation.
subroutine new_character_tokenizer(de, conf)
   type(toml_character_tokenizer), intent(out) :: de
   character(len=*), intent(in), target :: conf
   !> connect deserializer to configuration
   de%conf => conf
   de%line%ptr => conf
   de%line%num = 1
   !> first token is an artifical newline
   de%tok = new_token(toml_tokentype%newline, de%conf, 0)
end subroutine new_character_tokenizer


!> Return next token
subroutine next_token(de, dot_is_token)

   !> Instance of the tokenizer
   class(toml_character_tokenizer), intent(inout) :: de

   !> Dot should be handled as token
   logical, intent(in) :: dot_is_token

   character(len=:), pointer :: ptr
   integer :: i, skip
   if (de%finished) return
   ptr => de%tok%ptr

   !> consume token
   do i = 1, de%tok%len
      de%line%pos = de%line%pos + 1
      if (ptr(i:i) == TOML_NEWLINE) then
         de%line%ptr => ptr(min(i+1, len(ptr)):)
         de%line%num = de%line%num+1
         de%line%pos = 1
      end if
   end do
   ptr => ptr(de%tok%len+1:)

   !> make next token
   do while(len(ptr) > 0)
      select case(ptr(1:1))
      case('#')
         i = index(ptr, TOML_NEWLINE)
         if (i > 0) then
            ptr => ptr(i:)
            cycle
         end if
         exit
      case('.')
         if (dot_is_token) then
            de%tok = new_token(toml_tokentype%dot, ptr, 1)
            return
         end if
      case(','); de%tok = new_token(toml_tokentype%comma, ptr, 1); return
      case('='); de%tok = new_token(toml_tokentype%equal, ptr, 1); return
      case('{'); de%tok = new_token(toml_tokentype%lbrace, ptr, 1); return
      case('}'); de%tok = new_token(toml_tokentype%rbrace, ptr, 1); return
      case('['); de%tok = new_token(toml_tokentype%lbracket, ptr, 1); return
      case(']'); de%tok = new_token(toml_tokentype%rbracket, ptr, 1); return
      case(TOML_NEWLINE); de%tok = new_token(toml_tokentype%newline, ptr, 1); return
      case(' ', char(9));
         skip = verify(ptr, TOML_WHITESPACE)-1
         de%tok = new_token(toml_tokentype%whitespace, ptr, skip)
         return
      end select

      call scan_string(de, ptr, dot_is_token)
      return
   end do

   !> return with EOF token
   de%finished = .true.
   de%tok = new_token(toml_tokentype%newline, ptr, 0)

contains

   subroutine scan_string(de, ptr, dot_is_token)
   class(toml_character_tokenizer), intent(inout) :: de
   character(len=:), pointer, intent(inout) :: ptr
   logical, intent(in) :: dot_is_token
   character(len=:), pointer :: orig
   integer :: i, skip
   integer :: hexreq
   integer :: qcount
   logical :: escape
   orig => ptr

   if (len(ptr) >= 6) then
      if (ptr(1:3) == repeat(TOML_SQUOTE, 3)) then
         ptr => ptr(4:)
         i = index(ptr, repeat(TOML_SQUOTE, 3))
         if (i == 0) then
            call syntax_error(de%error, de%line, "unterminated triple-s-quote")
            return
         end if

         de%tok = new_token(toml_tokentype%string, orig, i+5)
         return
      end if

      if (ptr(1:3) == repeat(TOML_DQUOTE, 3)) then
         ptr => ptr(4:)
         escape = .false.
         hexreq = 0
         qcount = 0
         do i = 1, len(ptr)
            if (escape) then
               escape = .false.
               if (ptr(i:i) == 'u') then
                  hexreq = 4
                  cycle
               end if
               if (ptr(i:i) == 'U') then
                  hexreq = 8
                  cycle
               end if
               if (verify(ptr(i:i), 'btnfr"\') == 0) cycle
               ! allow for line ending backslash
               skip = verify(ptr(i:), TOML_WHITESPACE)-1
               if (ptr(i+skip:i+skip) == TOML_NEWLINE) cycle
               call syntax_error(de%error, de%line, "bad escape char")
               return
            end if
            if (hexreq > 0) then
               hexreq = hexreq - 1
               if (verify(ptr(i:i), TOML_HEXDIGITS) == 0) cycle
               call syntax_error(de%error, de%line, "expect hex char")
               return
            end if
            if (ptr(i:i) == TOML_DQUOTE) then
               if (qcount < 5) then
                  qcount = qcount + 1
               else
                  call syntax_error(de%error, de%line, "too many quotation marks")
                  return
               end if
            else
               if (qcount >= 3) then
                  ptr => ptr(i:)
                  exit
               end if
               qcount = 0
            end if
            if (ptr(i:i) == '\') then
               escape = .true.
               cycle
            end if
         end do
         if (qcount < 3) then
            call syntax_error(de%error, de%line, "unterminated triple-quote")
            return
         end if

         de%tok = new_token(toml_tokentype%string, orig, len(orig)-len(ptr))
         return
      end if
   end if

   if (ptr(1:1) == TOML_SQUOTE) then
      ptr => ptr(2:)
      i = index(ptr, TOML_NEWLINE)
      if (i == 0) i = len(ptr)
      i = index(ptr(:i), TOML_SQUOTE)
      if (i == 0) then
         call syntax_error(de%error, de%line, "unterminated s-quote")
         return
      end if

      de%tok = new_token(toml_tokentype%string, orig, i+1)
      return
   end if

   if (ptr(1:1) == TOML_DQUOTE) then
      ptr => ptr(2:)
      escape = .false.
      hexreq = 0
      do i = 1, len(ptr)
         if (escape) then
            escape = .false.
            if (ptr(i:i) == 'u') then
               hexreq = 4
               cycle
            end if
            if (ptr(i:i) == 'U') then
               hexreq = 8
               cycle
            end if
            if (verify(ptr(i:i), 'btnfr"\') == 0) cycle
            call syntax_error(de%error, de%line, "bad escape char")
            return
         end if
         if (hexreq > 0) then
            hexreq = hexreq - 1
            if (verify(ptr(i:i), TOML_HEXDIGITS) == 0) cycle
            call syntax_error(de%error, de%line, "expect hex char")
            return
         end if
         if (ptr(i:i) == '\') then
            escape = .true.
            cycle
         end if
         if (ptr(i:i) == TOML_NEWLINE) then
            ptr => ptr(i:)
            exit
         end if
         if (ptr(i:i) == TOML_DQUOTE) then
            ptr => ptr(i:)
            exit
         end if
      end do
      if (ptr(1:1) /= TOML_DQUOTE) then
            call syntax_error(de%error, de%line, "expect hex char")
         return
      end if

      de%tok = new_token(toml_tokentype%string, orig, len(orig)-len(ptr)+1)
      return
   end if

   if (toml_raw_verify_date(ptr) .or. toml_raw_verify_time(ptr)) then
      i = verify(ptr, TOML_TIMESTAMP)-1
      if (i < 0) i = len(ptr)
      de%tok = new_token(toml_tokentype%string, orig, i)
      return
   end if

   do i = 1, len(ptr)
      if (ptr(i:i) == '.' .and. dot_is_token) then
         ptr => ptr(i:)
         exit
      end if
      if (verify(ptr(i:i), TOML_LITERALS) == 0) cycle
      ptr => ptr(i:)
      exit
   end do

   de%tok = new_token(toml_tokentype%string, orig, len(orig) - len(ptr))

end subroutine scan_string

end subroutine next_token


!> custom constructor to get pointer assignment right
type(toml_token) function new_token(tok, ptr, len)
   integer, intent(in) :: tok
   character(len=:), pointer, intent(in) :: ptr
   integer, intent(in) :: len
   new_token%tok = tok
   new_token%ptr => ptr
   new_token%len = len
end function new_token


end module tomlf_de_character
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/build/array.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build TOML arrays.
!>
!> This build module defines a high level interface to work with TOML arrays
!> and construct them in a convenient way.
!>
!> The access to the array elements happens by position in the array, the indexing
!> is one based, following the language convention of Fortran. All functions
!> will only allow access of elements within the bounds of the array, specifying
!> indices out-of-bounds should be save, as it only sets the status of operation.
!> The getter functions allow access to other tables and arrays as well as
!> convenient wrappers to retrieve value data
!>
!> The setter functions are somewhat weaker compared to the setter functions
!> available for TOML tables. To limit the potential havoc this routines can
!> cause they can only access the array within its bounds. Setting a value to
!> another value will overwrite it, while setting a value to a table or an array
!> will fail, for safety reasons.
!>
!> To (re)build an array appending to it is the best choice, tables and arrays
!> should always be create by using the corresponding `add_table` and `add_array`
!> function. While this can become cumbersome for values, the setter routines
!> allow out-of-bound access to for the next element in an array and will indeed
!> just append a new value to it.
module tomlf_build_array
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, len
   use tomlf_utils, only : toml_raw_to_string, toml_raw_to_float, &
      & toml_raw_to_bool, toml_raw_to_integer, toml_raw_to_timestamp
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML arrays
   interface set_value
      module procedure :: set_elem_value_string
      module procedure :: set_elem_value_float_sp
      module procedure :: set_elem_value_float_dp
      module procedure :: set_elem_value_int_i1
      module procedure :: set_elem_value_int_i2
      module procedure :: set_elem_value_int_i4
      module procedure :: set_elem_value_int_i8
      module procedure :: set_elem_value_bool
   end interface set_value


   !> Getter functions to manipulate TOML arrays
   interface get_value
      module procedure :: get_elem_table
      module procedure :: get_elem_array
      module procedure :: get_elem_keyval
      module procedure :: get_elem_value_string
      module procedure :: get_elem_value_float_sp
      module procedure :: get_elem_value_float_dp
      module procedure :: get_elem_value_int_i1
      module procedure :: get_elem_value_int_i2
      module procedure :: get_elem_value_int_i4
      module procedure :: get_elem_value_int_i8
      module procedure :: get_elem_value_bool
   end interface get_value


contains


subroutine get_elem_table(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_table)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_table


subroutine get_elem_array(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_array)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_array


subroutine get_elem_keyval(array, pos, ptr, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp

   nullify(ptr)

   call array%get(pos, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_keyval


!> Retrieve TOML value as deferred-length character
subroutine get_elem_value_string(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_string


!> Retrieve TOML value as single precision floating point number
subroutine get_elem_value_float_sp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_sp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine get_elem_value_float_dp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_dp), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_float_dp


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i1(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i1


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i2(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i2


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i4(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i4


!> Retrieve TOML value as integer value
subroutine get_elem_value_int_i8(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_int_i8


!> Retrieve TOML value as boolean
subroutine get_elem_value_bool(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   logical, intent(out) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (associated(ptr)) then
      call get_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine get_elem_value_bool


!> Retrieve TOML value as deferred-length character
subroutine set_elem_value_string(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_string


!> Retrieve TOML value as single precision floating point number
subroutine set_elem_value_float_sp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_float_sp


!> Retrieve TOML value as double precision floating point number
subroutine set_elem_value_float_dp(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Floating point value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_float_dp


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i1(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i1


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i2(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i2


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i4(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i4


!> Retrieve TOML value as integer value
subroutine set_elem_value_int_i8(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_int_i8


!> Retrieve TOML value as boolean value
subroutine set_elem_value_bool(array, pos, val, stat)

   !> Instance of the TOML array
   class(toml_array), intent(inout) :: array

   !> Position in the array
   integer, intent(in) :: pos

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(array, pos, ptr, stat)

   if (.not.associated(ptr)) then
      if (pos == len(array) + 1) then
         call add_keyval(array, ptr, stat)
      end if
   end if

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) stat = toml_stat%fatal
   end if

end subroutine set_elem_value_bool


end module tomlf_build_array
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/build/table.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build TOML tables
!>
!> The build module defines a high level interface to work with TOML tables
!> and construct them in a convenient way.
!>
!> The getter functions allow to both retrieve and set values, to easily
!> support default values when reading from a TOML data structure.
!> Using the getter function with a default value specified will request
!> the respective setter function to add it to the table if it was not
!> found in the first place.
!>
!> This allows to build a TOML table using only the getter functions, which
!> represents the finally read values for the applications.
!>
!> Note that neither setter nor getter functions can overwrite existing
!> TOML values for safety reasons, request the deletion on the respective
!> key from the TOML table and than set it. The deletion of a subtable or
!> array will recursively destroy the contained data nodes.
module tomlf_build_table
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_constants, only : tfc, tfi, tfr, tf_i1, tf_i2, tf_i4, tf_i8, &
      & tf_sp, tf_dp
   use tomlf_error, only : toml_stat
   use tomlf_type, only : toml_value, toml_table, toml_array, toml_keyval, &
      & new_table, new_array, new_keyval, add_table, add_array, add_keyval, &
      & toml_key, len
   implicit none
   private

   public :: get_value, set_value


   !> Setter functions to manipulate TOML tables
   interface set_value
      module procedure :: set_child_value_float_sp
      module procedure :: set_child_value_float_dp
      module procedure :: set_child_value_integer_i1
      module procedure :: set_child_value_integer_i2
      module procedure :: set_child_value_integer_i4
      module procedure :: set_child_value_integer_i8
      module procedure :: set_child_value_bool
      module procedure :: set_child_value_string
      module procedure :: set_key_value_float_sp
      module procedure :: set_key_value_float_dp
      module procedure :: set_key_value_integer_i1
      module procedure :: set_key_value_integer_i2
      module procedure :: set_key_value_integer_i4
      module procedure :: set_key_value_integer_i8
      module procedure :: set_key_value_bool
      module procedure :: set_key_value_string
   end interface set_value


   !> Getter functions to manipulate TOML tables
   interface get_value
      module procedure :: get_child_table
      module procedure :: get_child_array
      module procedure :: get_child_keyval
      module procedure :: get_child_value_float_sp
      module procedure :: get_child_value_float_dp
      module procedure :: get_child_value_integer_i1
      module procedure :: get_child_value_integer_i2
      module procedure :: get_child_value_integer_i4
      module procedure :: get_child_value_integer_i8
      module procedure :: get_child_value_bool
      module procedure :: get_child_value_string
      module procedure :: get_key_table
      module procedure :: get_key_array
      module procedure :: get_key_keyval
      module procedure :: get_key_value_float_sp
      module procedure :: get_key_value_float_dp
      module procedure :: get_key_value_integer_i1
      module procedure :: get_key_value_integer_i2
      module procedure :: get_key_value_integer_i4
      module procedure :: get_key_value_integer_i8
      module procedure :: get_key_value_bool
      module procedure :: get_key_value_string
   end interface get_value


contains


subroutine get_key_table(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, ptr, requested, stat)

end subroutine get_key_table


subroutine get_key_array(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, ptr, requested, stat)

end subroutine get_key_array


subroutine get_key_keyval(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, ptr, requested, stat)

end subroutine get_key_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_key_value_float_sp(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_key_value_float_dp(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_key_value_integer_i1(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_key_value_integer_i2(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_key_value_integer_i4(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_key_value_integer_i8(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_key_value_bool(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_bool


!> Retrieve TOML value as deferred-length character
subroutine get_key_value_string(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   call get_value(table, key%key, val, default, stat)

end subroutine get_key_value_string


!> Set TOML value to single precision float
subroutine set_key_value_float_sp(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_float_sp


!> Set TOML value to double precision float
subroutine set_key_value_float_dp(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_float_dp


!> Set TOML value to one byte integer
subroutine set_key_value_integer_i1(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_key_value_integer_i2(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_key_value_integer_i4(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_key_value_integer_i8(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_integer_i8


!> Set TOML value to logical
subroutine set_key_value_bool(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_bool


!> Set TOML value to deferred-length character
subroutine set_key_value_string(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   type(toml_key), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   call set_value(table, key%key, val, stat)

end subroutine set_key_value_string


subroutine get_child_table(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child table
   type(toml_table), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_table)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (is_requested) then
         call add_table(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
   end if

end subroutine get_child_table


subroutine get_child_array(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child array
   type(toml_array), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_array)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (is_requested) then
         call add_array(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
   end if

end subroutine get_child_array


subroutine get_child_keyval(table, key, ptr, requested, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Pointer to child value
   type(toml_keyval), pointer, intent(out) :: ptr

   !> Child value must be present
   logical, intent(in), optional :: requested

   !> Status of operation
   integer, intent(out), optional :: stat

   class(toml_value), pointer :: tmp
   logical :: is_requested

   if (present(requested)) then
      is_requested = requested
   else
      is_requested = .true.
   end if

   nullify(ptr)

   call table%get(key, tmp)

   if (associated(tmp)) then
      select type(tmp)
      type is(toml_keyval)
         ptr => tmp
         if (present(stat)) stat = toml_stat%success
      class default
         if (present(stat)) stat = toml_stat%fatal
      end select
   else
      if (is_requested) then
         call add_keyval(table, key, ptr, stat)
      else
         if (present(stat)) stat = toml_stat%success
      end if
   end if

end subroutine get_child_keyval


!> Retrieve TOML value as single precision float (might lose accuracy)
subroutine get_child_value_float_sp(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(out) :: val

   !> Default real value
   real(tf_sp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_float_sp


!> Retrieve TOML value as double precision float
subroutine get_child_value_float_dp(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(out) :: val

   !> Default real value
   real(tf_dp), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_float_dp


!> Retrieve TOML value as one byte integer (might loose precision)
subroutine get_child_value_integer_i1(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(out) :: val

   !> Default integer value
   integer(tf_i1), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i1


!> Retrieve TOML value as two byte integer (might loose precision)
subroutine get_child_value_integer_i2(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(out) :: val

   !> Default integer value
   integer(tf_i2), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i2


!> Retrieve TOML value as four byte integer (might loose precision)
subroutine get_child_value_integer_i4(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(out) :: val

   !> Default integer value
   integer(tf_i4), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i4


!> Retrieve TOML value as eight byte integer
subroutine get_child_value_integer_i8(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(out) :: val

   !> Default integer value
   integer(tf_i8), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_integer_i8


!> Retrieve TOML value as logical
subroutine get_child_value_bool(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(out) :: val

   !> Default boolean value
   logical, intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_bool


!> Retrieve TOML value as deferred-length character
subroutine get_child_value_string(table, key, val, default, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=:), allocatable, intent(out) :: val

   !> Default string value
   character(kind=tfc, len=*), intent(in), optional :: default

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, present(default), stat)

   if (associated(ptr)) then
      if (allocated(ptr%raw)) then
         call get_value(ptr, val, stat)
      else
         if (present(default)) then
            call set_value(ptr, default)
            call get_value(ptr, val, stat=stat)
         else
            if (present(stat)) stat = toml_stat%fatal
         end if
      end if
   end if

end subroutine get_child_value_string


!> Set TOML value to single precision float
subroutine set_child_value_float_sp(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_sp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_sp


!> Set TOML value to double precision float
subroutine set_child_value_float_dp(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Real value
   real(tf_dp), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_float_dp


!> Set TOML value to one byte integer
subroutine set_child_value_integer_i1(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i1), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i1


!> Set TOML value to two byte integer
subroutine set_child_value_integer_i2(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i2), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i2


!> Set TOML value to four byte integer
subroutine set_child_value_integer_i4(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i4), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i4


!> Set TOML value to eight byte integer
subroutine set_child_value_integer_i8(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Integer value
   integer(tf_i8), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_integer_i8


!> Set TOML value to logical
subroutine set_child_value_bool(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> Boolean value
   logical, intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_bool


!> Set TOML value to deferred-length character
subroutine set_child_value_string(table, key, val, stat)

   !> Instance of the TOML table
   class(toml_table), intent(inout) :: table

   !> Key in this TOML table
   character(kind=tfc, len=*), intent(in) :: key

   !> String value
   character(kind=tfc, len=*), intent(in) :: val

   !> Status of operation
   integer, intent(out), optional :: stat

   type(toml_keyval), pointer :: ptr

   call get_value(table, key, ptr, .true., stat)

   if (associated(ptr)) then
      call set_value(ptr, val, stat)
   else
      if (present(stat)) then
         if (stat == toml_stat%success) stat = toml_stat%fatal
      end if
   end if

end subroutine set_child_value_string


end module tomlf_build_table
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/build.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Functions to build a TOML data structures
!>
!> The build module defines a high level interface to work with TOML data structures
!> and construct them in a convenient way.
module tomlf_build
   use tomlf_build_array, only : get_value, set_value
   use tomlf_build_keyval, only : get_value, set_value
   use tomlf_build_merge, only : merge_table, merge_array
   use tomlf_build_table, only : get_value, set_value
   implicit none
   private

   public :: get_value, set_value, merge_table, merge_array


end module tomlf_build
 
 
!>>>>> build/dependencies/toml-f/src/tomlf/de.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

module tomlf_de
   use tomlf_constants, only : TOML_NEWLINE
   use tomlf_de_character, only : toml_character_tokenizer, new
   use tomlf_error, only : toml_error, io_error
   use tomlf_type, only : toml_table
   implicit none
   private

   public :: toml_parse


   interface toml_parse
      module procedure :: toml_parse_unit
      module procedure :: toml_parse_string
   end interface toml_parse


contains


!> Parse a TOML input from a given IO unit.
subroutine toml_parse_unit(table, unit, error)
   use iso_fortran_env
   use tomlf_constants, only: TOML_NEWLINE
   type(toml_table), allocatable, intent(out) :: table
   integer, intent(in) :: unit
   type(toml_error), allocatable, intent(out), optional :: error
   character(len=:), allocatable :: conf
   integer, parameter :: bufsize = 512
   character(len=bufsize) :: buffer
   character(len=bufsize) :: error_msg
   integer :: size
   integer :: stat
   allocate(character(len=0) :: conf)
   do 
      read(unit, '(a)', advance='no', iostat=stat, iomsg=error_msg, size=size) &
         & buffer
      if (stat > 0) exit
      conf = conf // buffer(:size)
      if (stat < 0) then
         if (is_iostat_eor(stat)) then
            stat = 0
            conf = conf // TOML_NEWLINE
         end if
         if (is_iostat_end(stat)) then
            stat = 0
            exit
         end if
      end if
   end do

   if (stat /= 0) then
      if (present(error)) then
         call io_error(error, trim(error_msg))
      else
         write(error_unit, '(a, /, a)') "IO runtime error", trim(error_msg)
      end if
      return
   end if

   call toml_parse_string(table, conf, error)

end subroutine toml_parse_unit


!> Wrapper to parse a TOML string.
subroutine toml_parse_string(table, conf, error)
   use iso_fortran_env, only: error_unit
   type(toml_table), allocatable, intent(out) :: table
   character(len=*), intent(in), target :: conf
   type(toml_error), allocatable, intent(out), optional :: error
   type(toml_character_tokenizer) :: de

   !> connect deserializer to configuration
   call new(de, conf)

   call de%parse

   if (allocated(de%error)) then
      if (present(error)) then
         call move_alloc(de%error, error)
      else
         write(error_unit, '(a)') de%error%message
      end if
      return
   end if

   call move_alloc(de%root, table)

end subroutine toml_parse_string


end module tomlf_de
 
 
!>>>>> build/dependencies/toml-f/src/tomlf.f90
! This file is part of toml-f.
! SPDX-Identifier: Apache-2.0 OR MIT
!
! Licensed under either of Apache License, Version 2.0 or MIT license
! at your option; you may not use this file except in compliance with
! the License.
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Minimal public API for TOML-Fortran
module tomlf
   use tomlf_build, only : get_value, set_value
   use tomlf_de, only : toml_parse
   use tomlf_error, only : toml_error, toml_stat
   use tomlf_ser, only : toml_serializer
   use tomlf_type, only : toml_table, toml_array, toml_key, is_array_of_tables, &
      & new_table, add_table, add_array, len
   use tomlf_utils_sort, only : sort
   use tomlf_version, only : tomlf_version_string, tomlf_version_compact, &
      & get_tomlf_version
   implicit none
   public

end module tomlf
 
 
!>>>>> ././src/fpm/toml.f90
!># Interface to TOML processing library
!>
!> This module acts as a proxy to the `toml-f` public Fortran API and allows
!> to selectively expose components from the library to `fpm`.
!> The interaction with `toml-f` data types outside of this module should be
!> limited to tables, arrays and key-lists, most of the necessary interactions
!> are implemented in the building interface with the `get_value` and `set_value`
!> procedures.
!>
!> This module allows to implement features necessary for `fpm`, which are
!> not yet available in upstream `toml-f`.
!>
!> For more details on the library used see the
!> [TOML-Fortran](https://toml-f.github.io/toml-f) developer pages.
module fpm_toml
    use fpm_error, only : error_t, fatal_error, file_not_found_error
    use fpm_strings, only : string_t
    use tomlf, only : toml_table, toml_array, toml_key, toml_stat, get_value, &
        & set_value, toml_parse, toml_error, new_table, add_table, add_array, &
        & toml_serializer, len
    implicit none
    private

    public :: read_package_file
    public :: toml_table, toml_array, toml_key, toml_stat, get_value, set_value, get_list
    public :: new_table, add_table, add_array, len
    public :: toml_error, toml_serializer, toml_parse


contains


    !> Process the configuration file to a TOML data structure
    subroutine read_package_file(table, manifest, error)

        !> TOML data structure
        type(toml_table), allocatable, intent(out) :: table

        !> Name of the package configuration file
        character(len=*), intent(in) :: manifest

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        type(toml_error), allocatable :: parse_error
        integer :: unit
        logical :: exist

        inquire(file=manifest, exist=exist)

        if (.not.exist) then
            call file_not_found_error(error, manifest)
            return
        end if

        open(file=manifest, newunit=unit)
        call toml_parse(table, unit, parse_error)
        close(unit)

        if (allocated(parse_error)) then
            allocate(error)
            call move_alloc(parse_error%message, error%message)
            return
        end if

    end subroutine read_package_file


    subroutine get_list(table, key, list, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Key to read from
        character(len=*), intent(in) :: key

        !> List of strings to read
        type(string_t), allocatable, intent(out) :: list(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat, ilist, nlist
        type(toml_array), pointer :: children
        character(len=:), allocatable :: str

        call get_value(table, key, children, requested=.false.)
        if (associated(children)) then
            nlist = len(children)
            allocate(list(nlist))
            do ilist = 1, nlist
                call get_value(children, ilist, str, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Entry in "//key//" field cannot be read")
                    exit
                end if
                call move_alloc(str, list(ilist)%s)
            end do
            if (allocated(error)) return
        else
            call get_value(table, key, str, stat=stat)
            if (stat /= toml_stat%success) then
                call fatal_error(error, "Entry in "//key//" field cannot be read")
                return
            end if
            if (allocated(str)) then
                allocate(list(1))
                call move_alloc(str, list(1)%s)
            end if
        end if

    end subroutine get_list


end module fpm_toml
 
 
!>>>>> ././src/fpm/manifest/build.f90
!> Implementation of the build configuration data.
!>
!> A build table can currently have the following fields
!>
!>```toml
!>[build]
!>auto-executables = bool
!>auto-examples = bool
!>auto-tests = bool
!>link = ["lib"]
!>```
module fpm_manifest_build
    use fpm_error, only : error_t, syntax_error, fatal_error
    use fpm_strings, only : string_t
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: build_config_t, new_build_config


    !> Configuration data for build
    type :: build_config_t

        !> Automatic discovery of executables
        logical :: auto_executables

        !> Automatic discovery of examples
        logical :: auto_examples

        !> Automatic discovery of tests
        logical :: auto_tests

        !> Libraries to link against
        type(string_t), allocatable :: link(:)

        !> External modules to use
        type(string_t), allocatable :: external_modules(:)

    contains

        !> Print information on this instance
        procedure :: info

    end type build_config_t


contains


    !> Construct a new build configuration from a TOML data structure
    subroutine new_build_config(self, table, error)

        !> Instance of the build configuration
        type(build_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: stat

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "auto-executables", self%auto_executables, .true., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'auto-executables' in fpm.toml, expecting logical")
            return
        end if

        call get_value(table, "auto-tests", self%auto_tests, .true., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'auto-tests' in fpm.toml, expecting logical")
            return
        end if

        call get_value(table, "auto-examples", self%auto_examples, .true., stat=stat)

        if (stat /= toml_stat%success) then
            call fatal_error(error,"Error while reading value for 'auto-examples' in fpm.toml, expecting logical")
            return
        end if


        call get_list(table, "link", self%link, error)
        if (allocated(error)) return

        call get_list(table, "external-modules", self%external_modules, error)
        if (allocated(error)) return

    end subroutine new_build_config


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        integer :: ikey

        call table%get_keys(list)

        ! table can be empty
        if (size(list) < 1) return

        do ikey = 1, size(list)
            select case(list(ikey)%key)

            case("auto-executables", "auto-examples", "auto-tests", "link", "external-modules")
                continue

            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in [build]")
                exit

            end select
        end do

    end subroutine check


    !> Write information on build configuration instance
    subroutine info(self, unit, verbosity)

        !> Instance of the build configuration
        class(build_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ilink, imod
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Build configuration"
        write(unit, fmt) " - auto-discovery (apps) ", merge("enabled ", "disabled", self%auto_executables)
        write(unit, fmt) " - auto-discovery (examples) ", merge("enabled ", "disabled", self%auto_examples)
        write(unit, fmt) " - auto-discovery (tests) ", merge("enabled ", "disabled", self%auto_tests)
        if (allocated(self%link)) then
            write(unit, fmt) " - link against"
            do ilink = 1, size(self%link)
                write(unit, fmt) "   - " // self%link(ilink)%s
            end do
        end if
        if (allocated(self%external_modules)) then
            write(unit, fmt) " - external modules"
            do imod = 1, size(self%external_modules)
                write(unit, fmt) "   - " // self%external_modules(imod)%s
            end do
        end if

    end subroutine info

end module fpm_manifest_build
 
 
!>>>>> ././src/fpm/manifest/dependency.f90
!> Implementation of the meta data for dependencies.
!>
!> A dependency table can currently have the following fields
!>
!>```toml
!>[dependencies]
!>"dep1" = { git = "url" }
!>"dep2" = { git = "url", branch = "name" }
!>"dep3" = { git = "url", tag = "name" }
!>"dep4" = { git = "url", rev = "sha1" }
!>"dep0" = { path = "path" }
!>```
!>
!> To reduce the amount of boilerplate code this module provides two constructors
!> for dependency types, one basic for an actual dependency (inline) table
!> and another to collect all dependency objects from a dependencies table,
!> which is handling the allocation of the objects and is forwarding the
!> individual dependency tables to their respective constructors.
!> The usual entry point should be the constructor for the super table.
!>
!> This objects contains a target to retrieve required `fpm` projects to
!> build the target declaring the dependency.
!> Resolving a dependency will result in obtaining a new package configuration
!> data for the respective project.
module fpm_manifest_dependency
    use fpm_error, only : error_t, syntax_error
    use fpm_git, only : git_target_t, git_target_tag, git_target_branch, &
        & git_target_revision, git_target_default
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_filesystem, only: windows_path
    use fpm_environment, only: get_os_type, OS_WINDOWS
    implicit none
    private

    public :: dependency_config_t, new_dependency, new_dependencies


    !> Configuration meta data for a dependency
    type :: dependency_config_t

        !> Name of the dependency
        character(len=:), allocatable :: name

        !> Local target
        character(len=:), allocatable :: path

        !> Git descriptor
        type(git_target_t), allocatable :: git

    contains

        !> Print information on this instance
        procedure :: info

    end type dependency_config_t


contains


    !> Construct a new dependency configuration from a TOML data structure
    subroutine new_dependency(self, table, root, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: url, obj

        call check(table, error)
        if (allocated(error)) return

        call table%get_key(self%name)

        call get_value(table, "path", url)
        if (allocated(url)) then
            if (get_os_type() == OS_WINDOWS) url = windows_path(url)
            if (present(root)) url = root//url  ! Relative to the fpm.toml it’s written in
            call move_alloc(url, self%path)
        else
            call get_value(table, "git", url)

            call get_value(table, "tag", obj)
            if (allocated(obj)) then
                self%git = git_target_tag(url, obj)
            end if

            if (.not.allocated(self%git)) then
                call get_value(table, "branch", obj)
                if (allocated(obj)) then
                    self%git = git_target_branch(url, obj)
                end if
            end if

            if (.not.allocated(self%git)) then
                call get_value(table, "rev", obj)
                if (allocated(obj)) then
                    self%git = git_target_revision(url, obj)
                end if
            end if

            if (.not.allocated(self%git)) then
                self%git = git_target_default(url)
            end if

        end if

    end subroutine new_dependency


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        character(len=:), allocatable :: name
        type(toml_key), allocatable :: list(:)
        logical :: url_present, git_target_present, has_path
        integer :: ikey

        has_path = .false.
        url_present = .false.
        git_target_present = .false.

        call table%get_key(name)
        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Dependency "//name//" does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in dependency "//name)
                exit

            case("git", "path")
                if (url_present) then
                    call syntax_error(error, "Dependency "//name//" cannot have both git and path entries")
                    exit
                end if
                url_present = .true.
                has_path = list(ikey)%key == 'path'

            case("branch", "rev", "tag")
                if (git_target_present) then
                    call syntax_error(error, "Dependency "//name//" can only have one of branch, rev or tag present")
                    exit
                end if
                git_target_present = .true.

            end select
        end do
        if (allocated(error)) return

        if (.not.url_present) then
            call syntax_error(error, "Dependency "//name//" does not provide a method to actually retrieve itself")
            return
        end if

        if (has_path .and. git_target_present) then
            call syntax_error(error, "Dependency "//name//" uses a local path, therefore no git identifiers are allowed")
        end if

    end subroutine check


    !> Construct new dependency array from a TOML data structure
    subroutine new_dependencies(deps, table, root, error)

        !> Instance of the dependency configuration
        type(dependency_config_t), allocatable, intent(out) :: deps(:)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: node
        type(toml_key), allocatable :: list(:)
        integer :: idep, stat

        call table%get_keys(list)
        ! An empty table is okay
        if (size(list) < 1) return

        allocate(deps(size(list)))
        do idep = 1, size(list)
            call get_value(table, list(idep)%key, node, stat=stat)
            if (stat /= toml_stat%success) then
                call syntax_error(error, "Dependency "//list(idep)%key//" must be a table entry")
                exit
            end if
            call new_dependency(deps(idep), node, root, error)
            if (allocated(error)) exit
        end do

    end subroutine new_dependencies


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the dependency configuration
        class(dependency_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        write(unit, fmt) "Dependency"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if

        if (allocated(self%git)) then
            write(unit, fmt) "- kind", "git"
            call self%git%info(unit, pr - 1)
        end if

        if (allocated(self%path)) then
            write(unit, fmt) "- kind", "local"
            write(unit, fmt) "- path", self%path
        end if

    end subroutine info


end module fpm_manifest_dependency
 
 
!>>>>> ././src/fpm/manifest/install.f90
!> Implementation of the installation configuration.
!>
!> An install table can currently have the following fields
!>
!>```toml
!>library = bool
!>```
module fpm_manifest_install
  use fpm_error, only : error_t, fatal_error, syntax_error
  use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
  implicit none
  private

  public :: install_config_t, new_install_config

  !> Configuration data for installation
  type :: install_config_t

    !> Install library with this project
    logical :: library

  contains

    !> Print information on this instance
    procedure :: info

  end type install_config_t

contains

  !> Create a new installation configuration from a TOML data structure
  subroutine new_install_config(self, table, error)

    !> Instance of the install configuration
    type(install_config_t), intent(out) :: self

    !> Instance of the TOML data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    call check(table, error)
    if (allocated(error)) return

    call get_value(table, "library", self%library, .false.)

  end subroutine new_install_config


  !> Check local schema for allowed entries
  subroutine check(table, error)

    !> Instance of the TOML data structure
    type(toml_table), intent(inout) :: table

    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_key), allocatable :: list(:)
    integer :: ikey

    call table%get_keys(list)
    if (size(list) < 1) return

    do ikey = 1, size(list)
      select case(list(ikey)%key)
      case default
        call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in install table")
        exit
      case("library")
        continue
      end select
    end do
    if (allocated(error)) return

  end subroutine check

  !> Write information on install configuration instance
  subroutine info(self, unit, verbosity)

    !> Instance of the build configuration
    class(install_config_t), intent(in) :: self

    !> Unit for IO
    integer, intent(in) :: unit

    !> Verbosity of the printout
    integer, intent(in), optional :: verbosity

    integer :: pr
    character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

    if (present(verbosity)) then
      pr = verbosity
    else
      pr = 1
    end if

    if (pr < 1) return

    write(unit, fmt) "Install configuration"
    write(unit, fmt) " - library install", &
      & trim(merge("enabled ", "disabled", self%library))

  end subroutine info

end module fpm_manifest_install
 
 
!>>>>> ././src/fpm/manifest/library.f90
!> Implementation of the meta data for libraries.
!>
!> A library table can currently have the following fields
!>
!>```toml
!>[library]
!>source-dir = "path"
!>include-dir = ["path1","path2"]
!>build-script = "file"
!>```
module fpm_manifest_library
    use fpm_error, only : error_t, syntax_error
    use fpm_strings, only: string_t, string_cat
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: library_config_t, new_library


    !> Configuration meta data for a library
    type :: library_config_t

        !> Source path prefix
        character(len=:), allocatable :: source_dir

        !> Include path prefix
        type(string_t), allocatable :: include_dir(:)

        !> Alternative build script to be invoked
        character(len=:), allocatable :: build_script

    contains

        !> Print information on this instance
        procedure :: info

    end type library_config_t


contains


    !> Construct a new library configuration from a TOML data structure
    subroutine new_library(self, table, error)

        !> Instance of the library configuration
        type(library_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "source-dir", self%source_dir, "src")
        call get_value(table, "build-script", self%build_script)

        call get_list(table, "include-dir", self%include_dir, error)
        if (allocated(error)) return

        ! Set default value of include-dir if not found in manifest
        if (.not.allocated(self%include_dir)) then
            self%include_dir = [string_t("include")]
        end if

    end subroutine new_library


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        integer :: ikey

        call table%get_keys(list)

        ! table can be empty
        if (size(list) < 1) return

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in library")
                exit

            case("source-dir", "include-dir", "build-script")
                continue

            end select
        end do

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the library configuration
        class(library_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Library target"
        if (allocated(self%source_dir)) then
            write(unit, fmt) "- source directory", self%source_dir
        end if
        if (allocated(self%include_dir)) then
            write(unit, fmt) "- include directory", string_cat(self%include_dir,",")
        end if
        if (allocated(self%build_script)) then
            write(unit, fmt) "- custom build", self%build_script
        end if

    end subroutine info


end module fpm_manifest_library
 
 
!>>>>> ././src/fpm/manifest/preprocess.f90
!> Implementation of the meta data for preprocessing.
!>
!> A preprocess table can currently have the following fields
!>
!> ```toml
!> [preprocess]
!> [preprocess.cpp]
!> suffixes = ["F90", "f90"]
!> directories = ["src/feature1", "src/models"]
!> macros = []
!> ```

module fpm_mainfest_preprocess
   use fpm_error, only : error_t, syntax_error
   use fpm_strings, only : string_t
   use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
   implicit none
   private

   public :: preprocess_config_t, new_preprocess_config, new_preprocessors

   !> Configuration meta data for a preprocessor
   type :: preprocess_config_t

      !> Name of the preprocessor
      character(len=:), allocatable :: name

      !> Suffixes of the files to be preprocessed
      type(string_t), allocatable :: suffixes(:)

      !> Directories to search for files to be preprocessed
      type(string_t), allocatable :: directories(:)

      !> Macros to be defined for the preprocessor
      type(string_t), allocatable :: macros(:)

   contains

      !> Print information on this instance
      procedure :: info

   end type preprocess_config_t

contains

   !> Construct a new preprocess configuration from TOML data structure
   subroutine new_preprocess_config(self, table, error)

      !> Instance of the preprocess configuration
      type(preprocess_config_t), intent(out) :: self

      !> Instance of the TOML data structure.
      type(toml_table), intent(inout) :: table

      !> Error handling
      type(error_t), allocatable, intent(out) :: error

      call check(table, error)
      if (allocated(error)) return

      call table%get_key(self%name)

      call get_list(table, "suffixes", self%suffixes, error)
      if (allocated(error)) return

      call get_list(table, "directories", self%directories, error)
      if (allocated(error)) return

      call get_list(table, "macros", self%macros, error)
      if (allocated(error)) return

   end subroutine new_preprocess_config

   !> Check local schema for allowed entries
   subroutine check(table, error)

      !> Instance of the TOML data structure.
      type(toml_table), intent(inout) :: table

      !> Error handling
      type(error_t), allocatable, intent(inout) :: error

      character(len=:), allocatable :: name
      type(toml_key), allocatable :: list(:)
      logical :: suffixes_present, directories_present, macros_present
      integer :: ikey

      suffixes_present = .false.
      directories_present = .false.
      macros_present = .false.

      call table%get_key(name)
      call table%get_keys(list)

      do ikey = 1, size(list)
         select case(list(ikey)%key)
          case default
            call syntax_error(error, "Key " // list(ikey)%key // "is not allowed in preprocessor"//name)
            exit
          case("suffixes")
            suffixes_present = .true.
          case("directories")
            directories_present = .true.
          case("macros")
            macros_present = .true.
         end select
      end do
   end subroutine check

   !> Construct new preprocess array from a TOML data structure.
   subroutine new_preprocessors(preprocessors, table, error)

      !> Instance of the preprocess configuration
      type(preprocess_config_t), allocatable, intent(out) :: preprocessors(:)

      !> Instance of the TOML data structure
      type(toml_table), intent(inout) :: table

      !> Error handling
      type(error_t), allocatable, intent(out) :: error

      type(toml_table), pointer :: node
      type(toml_key), allocatable :: list(:)
      integer :: iprep, stat

      call table%get_keys(list)

      ! An empty table is not allowed
      if (size(list) == 0) then
         call syntax_error(error, "No preprocessors defined")
      end if

      allocate(preprocessors(size(list)))
      do iprep = 1, size(list)
         call get_value(table, list(iprep)%key, node, stat=stat)
         if (stat /= toml_stat%success) then
            call syntax_error(error, "Preprocessor "//list(iprep)%key//" must be a table entry")
            exit
         end if
         call new_preprocess_config(preprocessors(iprep), node, error)
         if (allocated(error)) exit
      end do

   end subroutine new_preprocessors

   !> Write information on this instance
   subroutine info(self, unit, verbosity)

      !> Instance of the preprocess configuration
      class(preprocess_config_t), intent(in) :: self

      !> Unit for IO
      integer, intent(in) :: unit

      !> Verbosity of the printout
      integer, intent(in), optional :: verbosity

      integer :: pr, ilink
      character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

      if (present(verbosity)) then
         pr = verbosity
      else
         pr = 1
      end if

      if (pr < 1) return 

      write(unit, fmt) "Preprocessor"
      if (allocated(self%name)) then
         write(unit, fmt) "- name", self%name
      end if
      if (allocated(self%suffixes)) then
         write(unit, fmt) " - suffixes"
         do ilink = 1, size(self%suffixes)
            write(unit, fmt) "   - " // self%suffixes(ilink)%s
         end do
      end if
      if (allocated(self%directories)) then
         write(unit, fmt) " - directories"
         do ilink = 1, size(self%directories)
            write(unit, fmt) "   - " // self%directories(ilink)%s
         end do
      end if
      if (allocated(self%macros)) then
         write(unit, fmt) " - macros"
         do ilink = 1, size(self%macros)
            write(unit, fmt) "   - " // self%macros(ilink)%s
         end do
      end if

   end subroutine info

end module fpm_mainfest_preprocess
 
 
!>>>>> ././src/fpm/manifest/profiles.f90
!> Implementation of the meta data for compiler flag profiles.
!>
!> A profiles table can currently have the following subtables:
!> Profile names - any string, if omitted, flags are appended to all matching profiles
!> Compiler - any from the following list, omitting it yields an error
!>
!> - "gfortran"
!> - "ifort"
!> - "ifx"
!> - "pgfortran"
!> - "nvfortran"
!> - "flang"
!> - "caf"
!> - "f95"
!> - "lfortran"
!> - "lfc"
!> - "nagfor"
!> - "crayftn"
!> - "xlf90"
!> - "ftn95"
!>
!> OS - any from the following list, if omitted, the profile is used if and only
!> if there is no profile perfectly matching the current configuration
!>
!> - "linux"
!> - "macos"
!> - "windows"
!> - "cygwin"
!> - "solaris"
!> - "freebsd"
!> - "openbsd"
!> - "unknown"
!>
!> Each of the subtables currently supports the following fields:
!>```toml
!>[profiles.debug.gfortran.linux]
!> flags="-Wall -g -Og"
!> c-flags="-g O1"
!> cxx-flags="-g O1"
!> link-time-flags="-xlinkopt"
!> files={"hello_world.f90"="-Wall -O3"}
!>```
!>
module fpm_manifest_profile
    use fpm_error, only : error_t, syntax_error, fatal_error, fpm_stop
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value
    use fpm_strings, only: lower
    use fpm_environment, only: get_os_type, OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, &
                             OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD
    use fpm_filesystem, only: join_path
    implicit none
    public :: profile_config_t, new_profile, new_profiles, get_default_profiles, &
            & info_profile, find_profile, DEFAULT_COMPILER

    !> Name of the default compiler
    character(len=*), parameter :: DEFAULT_COMPILER = 'gfortran' 
    integer, parameter :: OS_ALL = -1
    character(len=:), allocatable :: path

    !> Type storing file name - file scope compiler flags pairs
    type :: file_scope_flag

      !> Name of the file
      character(len=:), allocatable :: file_name

      !> File scope flags
      character(len=:), allocatable :: flags

    end type file_scope_flag

    !> Configuration meta data for a profile
    type :: profile_config_t
      !> Name of the profile
      character(len=:), allocatable :: profile_name

      !> Name of the compiler
      character(len=:), allocatable :: compiler

      !> Value repesenting OS
      integer :: os_type
      
      !> Fortran compiler flags
      character(len=:), allocatable :: flags

      !> C compiler flags
      character(len=:), allocatable :: c_flags

      !> C++ compiler flags
      character(len=:), allocatable :: cxx_flags

      !> Link time compiler flags
      character(len=:), allocatable :: link_time_flags

      !> File scope flags
      type(file_scope_flag), allocatable :: file_scope_flags(:)

      !> Is this profile one of the built-in ones?
      logical :: is_built_in

      contains

        !> Print information on this instance
        procedure :: info

    end type profile_config_t

    contains

      !> Construct a new profile configuration from a TOML data structure
      function new_profile(profile_name, compiler, os_type, flags, c_flags, cxx_flags, &
                           link_time_flags, file_scope_flags, is_built_in) &
                      & result(profile)
        
        !> Name of the profile
        character(len=*), intent(in) :: profile_name
        
        !> Name of the compiler
        character(len=*), intent(in) :: compiler
        
        !> Type of the OS
        integer, intent(in) :: os_type
        
        !> Fortran compiler flags
        character(len=*), optional, intent(in) :: flags

        !> C compiler flags
        character(len=*), optional, intent(in) :: c_flags

        !> C++ compiler flags
        character(len=*), optional, intent(in) :: cxx_flags

        !> Link time compiler flags
        character(len=*), optional, intent(in) :: link_time_flags

        !> File scope flags
        type(file_scope_flag), optional, intent(in) :: file_scope_flags(:)

        !> Is this profile one of the built-in ones?
        logical, optional, intent(in) :: is_built_in

        type(profile_config_t) :: profile

        profile%profile_name = profile_name
        profile%compiler = compiler
        profile%os_type = os_type
        if (present(flags)) then
          profile%flags = flags
        else
          profile%flags = ""
        end if
        if (present(c_flags)) then
          profile%c_flags = c_flags
        else
          profile%c_flags = ""
        end if
        if (present(cxx_flags)) then
          profile%cxx_flags = cxx_flags
        else
          profile%cxx_flags = ""
        end if
        if (present(link_time_flags)) then
          profile%link_time_flags = link_time_flags
        else
          profile%link_time_flags = ""
        end if
        if (present(file_scope_flags)) then
           profile%file_scope_flags = file_scope_flags
        end if
        if (present(is_built_in)) then
           profile%is_built_in = is_built_in
        else
           profile%is_built_in = .false.
        end if

      end function new_profile

      !> Check if compiler name is a valid compiler name
      subroutine validate_compiler_name(compiler_name, is_valid)

        !> Name of a compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> Boolean value of whether compiler_name is valid or not
        logical, intent(out) :: is_valid
        select case(compiler_name)
          case("gfortran", "ifort", "ifx", "pgfortran", "nvfortran", "flang", "caf", &
                        & "f95", "lfortran", "lfc", "nagfor", "crayftn", "xlf90", "ftn95")
            is_valid = .true.
          case default
            is_valid = .false.
        end select
      end subroutine validate_compiler_name
        
      !> Check if os_name is a valid name of a supported OS
      subroutine validate_os_name(os_name, is_valid)

        !> Name of an operating system
        character(len=:), allocatable, intent(in) :: os_name

        !> Boolean value of whether os_name is valid or not
        logical, intent(out) :: is_valid

        select case (os_name)
          case ("linux", "macos", "windows", "cygwin", "solaris", "freebsd", &
                          & "openbsd", "unknown")
            is_valid = .true.
          case default
            is_valid = .false.
        end select

      end subroutine validate_os_name

      !> Match os_type enum to a lowercase string with name of OS
      subroutine match_os_type(os_name, os_type)

        !> Name of operating system
        character(len=:), allocatable, intent(in) :: os_name

        !> Enum representing type of OS
        integer, intent(out) :: os_type

        select case (os_name)
          case ("linux");   os_type = OS_LINUX
          case ("macos");   os_type = OS_WINDOWS
          case ("cygwin");  os_type = OS_CYGWIN
          case ("solaris"); os_type = OS_SOLARIS
          case ("freebsd"); os_type = OS_FREEBSD
          case ("openbsd"); os_type = OS_OPENBSD
          case ("all");     os_type = OS_ALL
          case default;     os_type = OS_UNKNOWN
        end select

      end subroutine match_os_type

      subroutine validate_profile_table(profile_name, compiler_name, key_list, table, error, os_valid)

        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> List of keys in the table
        type(toml_key), allocatable, intent(in) :: key_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Was called with valid operating system
        logical, intent(in) :: os_valid

        character(len=:), allocatable :: flags, c_flags, cxx_flags, link_time_flags, key_name, file_name, file_flags, err_message
        type(toml_table), pointer :: files
        type(toml_key), allocatable :: file_list(:)
        integer :: ikey, ifile, stat
        logical :: is_valid

        if (size(key_list).ge.1) then
          do ikey=1,size(key_list)
            key_name = key_list(ikey)%key
            if (key_name.eq.'flags') then
              call get_value(table, 'flags', flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'c-flags') then
              call get_value(table, 'c-flags', c_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "c-flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'cxx-flags') then
              call get_value(table, 'cxx-flags', cxx_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "cxx-flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'link-time-flags') then
              call get_value(table, 'link-time-flags', link_time_flags, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "link-time-flags has to be a key-value pair")
                return
              end if
            else if (key_name.eq.'files') then
              call get_value(table, 'files', files, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "files has to be a table")
                return
              end if
              call files%get_keys(file_list)
              do ifile=1,size(file_list)
                file_name = file_list(ifile)%key
                call get_value(files, file_name, file_flags, stat=stat)
                if (stat /= toml_stat%success) then
                  call syntax_error(error, "file scope flags has to be a key-value pair")
                  return
                end if
              end do
            else if (.not. os_valid) then
                call validate_os_name(key_name, is_valid)
                err_message = "Unexpected key " // key_name // " found in profile table "//profile_name//" "//compiler_name//"."
                if (.not. is_valid) call syntax_error(error, err_message)
            else
                err_message = "Unexpected key " // key_name // " found in profile table "//profile_name//" "//compiler_name//"."
                call syntax_error(error, err_message)
            end if
          end do
        end if

        if (allocated(error)) return

      end subroutine validate_profile_table

      !> Look for flags, c-flags, link-time-flags key-val pairs
      !> and files table in a given table and create new profiles
      subroutine get_flags(profile_name, compiler_name, os_type, key_list, table, profiles, profindex, os_valid)

        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> OS type
        integer, intent(in) :: os_type

        !> List of keys in the table
        type(toml_key), allocatable, intent(in) :: key_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout) :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout) :: profindex

        !> Was called with valid operating system
        logical, intent(in) :: os_valid

        character(len=:), allocatable :: flags, c_flags, cxx_flags, link_time_flags, key_name, file_name, file_flags, err_message
        type(toml_table), pointer :: files
        type(toml_key), allocatable :: file_list(:)
        type(file_scope_flag), allocatable :: file_scope_flags(:)
        integer :: ikey, ifile, stat
        logical :: is_valid

        call get_value(table, 'flags', flags)
        call get_value(table, 'c-flags', c_flags)
        call get_value(table, 'cxx-flags', cxx_flags)
        call get_value(table, 'link-time-flags', link_time_flags)
        call get_value(table, 'files', files)
        if (associated(files)) then
          call files%get_keys(file_list)
          allocate(file_scope_flags(size(file_list)))
          do ifile=1,size(file_list)
            file_name = file_list(ifile)%key
            call get_value(files, file_name, file_flags)
            associate(cur_file=>file_scope_flags(ifile))
              if (.not.(path.eq."")) file_name = join_path(path, file_name)
              cur_file%file_name = file_name
              cur_file%flags = file_flags
            end associate
          end do
        end if

        profiles(profindex) = new_profile(profile_name, compiler_name, os_type, &
                 & flags, c_flags, cxx_flags, link_time_flags, file_scope_flags)
        profindex = profindex + 1
      end subroutine get_flags
      
      !> Traverse operating system tables to obtain number of profiles
      subroutine traverse_oss_for_size(profile_name, compiler_name, os_list, table, profiles_size, error)
        
        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> List of OSs in table with profile name and compiler name given
        type(toml_key), allocatable, intent(in) :: os_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> Number of profiles in list of profiles
        integer, intent(inout) :: profiles_size

        type(toml_key), allocatable :: key_list(:)
        character(len=:), allocatable :: os_name, l_os_name
        type(toml_table), pointer :: os_node
        integer :: ios, stat
        logical :: is_valid, key_val_added, is_key_val

        if (size(os_list)<1) return
        key_val_added = .false.
        do ios = 1, size(os_list)
          os_name = os_list(ios)%key
          call validate_os_name(os_name, is_valid)
          if (is_valid) then
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "os "//os_name//" has to be a table")
              return
            end if
            call os_node%get_keys(key_list)
            profiles_size = profiles_size + 1
            call validate_profile_table(profile_name, compiler_name, key_list, os_node, error, .true.)
          else
            ! Not lowercase OS name
            l_os_name = lower(os_name)
            call validate_os_name(l_os_name, is_valid)
            if (is_valid) then
              call fatal_error(error,'*traverse_oss*:Error: Name of the operating system must be a lowercase string.')
            end if
            if (allocated(error)) return

            ! Missing OS name
            is_key_val = .false.
            os_name = os_list(ios)%key
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              is_key_val = .true.
            end if
            os_node=>table
            if (is_key_val.and..not.key_val_added) then
              key_val_added = .true.
              is_key_val = .false.
              profiles_size = profiles_size + 1
            else if (.not.is_key_val) then
              profiles_size = profiles_size + 1
            end if
            call validate_profile_table(profile_name, compiler_name, os_list, os_node, error, .false.)
          end if
        end do
      end subroutine traverse_oss_for_size


      !> Traverse operating system tables to obtain profiles
      subroutine traverse_oss(profile_name, compiler_name, os_list, table, profiles, profindex, error)
        
        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(len=:), allocatable, intent(in) :: compiler_name

        !> List of OSs in table with profile name and compiler name given
        type(toml_key), allocatable, intent(in) :: os_list(:)

        !> Table containing OS tables
        type(toml_table), pointer, intent(in) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout) :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout) :: profindex
        
        type(toml_key), allocatable :: key_list(:)
        character(len=:), allocatable :: os_name, l_os_name
        type(toml_table), pointer :: os_node
        integer :: ios, stat, os_type
        logical :: is_valid, is_key_val

        if (size(os_list)<1) return
        do ios = 1, size(os_list)
          os_name = os_list(ios)%key
          call validate_os_name(os_name, is_valid)
          if (is_valid) then
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "os "//os_name//" has to be a table")
              return
            end if
            call os_node%get_keys(key_list)
            call match_os_type(os_name, os_type)
            call get_flags(profile_name, compiler_name, os_type, key_list, os_node, profiles, profindex, .true.)
          else
            ! Not lowercase OS name
            l_os_name = lower(os_name)
            call validate_os_name(l_os_name, is_valid)
            if (is_valid) then
              call fatal_error(error,'*traverse_oss*:Error: Name of the operating system must be a lowercase string.')
            end if
            if (allocated(error)) return

            ! Missing OS name
            is_key_val = .false.
            os_name = os_list(ios)%key
            call get_value(table, os_name, os_node, stat=stat)
            if (stat /= toml_stat%success) then
              is_key_val = .true.
            end if
            os_node=>table
            os_type = OS_ALL
            call get_flags(profile_name, compiler_name, os_type, os_list, os_node, profiles, profindex, .false.)
          end if
        end do
      end subroutine traverse_oss

      !> Traverse compiler tables
      subroutine traverse_compilers(profile_name, comp_list, table, error, profiles_size, profiles, profindex)
        
        !> Name of profile
        character(len=:), allocatable, intent(in) :: profile_name

        !> List of OSs in table with profile name given
        type(toml_key), allocatable, intent(in) :: comp_list(:)

        !> Table containing compiler tables
        type(toml_table), pointer, intent(in) :: table
        
        !> Error handling
        type(error_t), allocatable, intent(out) :: error
        
        !> Number of profiles in list of profiles
        integer, intent(inout), optional :: profiles_size

        !> List of profiles
        type(profile_config_t), allocatable, intent(inout), optional :: profiles(:)

        !> Index in the list of profiles
        integer, intent(inout), optional :: profindex
        
        character(len=:), allocatable :: compiler_name        
        type(toml_table), pointer :: comp_node
        type(toml_key), allocatable :: os_list(:)
        integer :: icomp, stat
        logical :: is_valid

        if (size(comp_list)<1) return
        do icomp = 1, size(comp_list)
          call validate_compiler_name(comp_list(icomp)%key, is_valid)
          if (is_valid) then  
            compiler_name = comp_list(icomp)%key
            call get_value(table, compiler_name, comp_node, stat=stat)
            if (stat /= toml_stat%success) then
              call syntax_error(error, "Compiler "//comp_list(icomp)%key//" must be a table entry")
              exit
            end if
            call comp_node%get_keys(os_list)
            if (present(profiles_size)) then
              call traverse_oss_for_size(profile_name, compiler_name, os_list, comp_node, profiles_size, error)
              if (allocated(error)) return
            else
              if (.not.(present(profiles).and.present(profindex))) then
                call fatal_error(error, "Both profiles and profindex have to be present")
                return
              end if
              call traverse_oss(profile_name, compiler_name, os_list, comp_node, &
                                & profiles, profindex, error)
              if (allocated(error)) return
            end if
          else
            call fatal_error(error,'*traverse_compilers*:Error: Compiler name not specified or invalid.')
          end if
        end do        
      end subroutine traverse_compilers

      !> Construct new profiles array from a TOML data structure
      subroutine new_profiles(profiles, table, error)

        !> Instance of the dependency configuration
        type(profile_config_t), allocatable, intent(out) :: profiles(:)

        !> Instance of the TOML data structure
        type(toml_table), target, intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: prof_node
        type(toml_key), allocatable :: prof_list(:)
        type(toml_key), allocatable :: comp_list(:)
        type(toml_key), allocatable :: os_list(:)
        character(len=:), allocatable :: profile_name, compiler_name
        integer :: profiles_size, iprof, stat, profindex
        logical :: is_valid
        type(profile_config_t), allocatable :: default_profiles(:)

        path = ''

        default_profiles = get_default_profiles(error)
        if (allocated(error)) return
        call table%get_keys(prof_list)
        
        if (size(prof_list) < 1) return
        
        profiles_size = 0

        do iprof = 1, size(prof_list)
          profile_name = prof_list(iprof)%key
          call validate_compiler_name(profile_name, is_valid)
          if (is_valid) then
            profile_name = "all"
            comp_list = prof_list(iprof:iprof)
            prof_node=>table
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles_size=profiles_size)
            if (allocated(error)) return
          else
            call validate_os_name(profile_name, is_valid)
            if (is_valid) then
              os_list = prof_list(iprof:iprof)
              profile_name = 'all'
              compiler_name = DEFAULT_COMPILER
              call traverse_oss_for_size(profile_name, compiler_name, os_list, table, profiles_size, error)
              if (allocated(error)) return
            else
              call get_value(table, profile_name, prof_node, stat=stat)
              if (stat /= toml_stat%success) then
                call syntax_error(error, "Profile "//prof_list(iprof)%key//" must be a table entry")
                exit
              end if
              call prof_node%get_keys(comp_list)
              call traverse_compilers(profile_name, comp_list, prof_node, error, profiles_size=profiles_size)
              if (allocated(error)) return
            end if
          end if
        end do

        profiles_size = profiles_size + size(default_profiles)
        allocate(profiles(profiles_size))
        
        do profindex=1, size(default_profiles)
          profiles(profindex) = default_profiles(profindex)
        end do

        do iprof = 1, size(prof_list)
          profile_name = prof_list(iprof)%key
          call validate_compiler_name(profile_name, is_valid)
          if (is_valid) then
            profile_name = "all"
            comp_list = prof_list(iprof:iprof)
            prof_node=>table
            call traverse_compilers(profile_name, comp_list, prof_node, error, profiles=profiles, profindex=profindex)
            if (allocated(error)) return
          else
            call validate_os_name(profile_name, is_valid)
            if (is_valid) then
              os_list = prof_list(iprof:iprof)
              profile_name = 'all'
              compiler_name = DEFAULT_COMPILER
              prof_node=>table
              call traverse_oss(profile_name, compiler_name, os_list, prof_node, profiles, profindex, error)
              if (allocated(error)) return
            else
              call get_value(table, profile_name, prof_node, stat=stat)
              call prof_node%get_keys(comp_list)
              call traverse_compilers(profile_name, comp_list, prof_node, error, profiles=profiles, profindex=profindex)
              if (allocated(error)) return
            end if
          end if
        end do

        ! Apply profiles with profile name 'all' to matching profiles
        do iprof = 1,size(profiles)
          if (profiles(iprof)%profile_name.eq.'all') then
            do profindex = 1,size(profiles)
              if (.not.(profiles(profindex)%profile_name.eq.'all') &
                      & .and.(profiles(profindex)%compiler.eq.profiles(iprof)%compiler) &
                      & .and.(profiles(profindex)%os_type.eq.profiles(iprof)%os_type)) then
                profiles(profindex)%flags=profiles(profindex)%flags// &
                        & " "//profiles(iprof)%flags
                profiles(profindex)%c_flags=profiles(profindex)%c_flags// &
                        & " "//profiles(iprof)%c_flags
                profiles(profindex)%cxx_flags=profiles(profindex)%cxx_flags// &
                        & " "//profiles(iprof)%cxx_flags
                profiles(profindex)%link_time_flags=profiles(profindex)%link_time_flags// &
                        & " "//profiles(iprof)%link_time_flags
              end if
            end do
          end if
        end do
      end subroutine new_profiles

      !> Construct an array of built-in profiles
      function get_default_profiles(error) result(default_profiles)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(profile_config_t), allocatable :: default_profiles(:)

        default_profiles = [ &
              & new_profile('release', &
                & 'caf', &
                & OS_ALL, &
                & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'gfortran', &
                & OS_ALL, &
                & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -funroll-loops -fcoarray=single', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'f95', &
                & OS_ALL, &
                & flags=' -O3 -Wimplicit-interface -fPIC -fmax-errors=1 -ffast-math -funroll-loops', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'nvfortran', &
                & OS_ALL, &
                & flags = ' -Mbackslash', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifort', &
                & OS_ALL, &
                & flags = ' -fp-model precise -pc64 -align all -error-limit 1 -reentrancy&
                          & threaded -nogen-interfaces -assume byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifort', &
                & OS_WINDOWS, &
                & flags = ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
                          & /nogen-interfaces /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifx', &
                & OS_ALL, &
                & flags = ' -fp-model=precise -pc64 -align all -error-limit 1 -reentrancy&
                          & threaded -nogen-interfaces -assume byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                & 'ifx', &
                & OS_WINDOWS, &
                & flags = ' /fp:precise /align:all /error-limit:1 /reentrancy:threaded&
                          & /nogen-interfaces /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('release', &
                &'nagfor', &
                & OS_ALL, &
                & flags = ' -O4 -coarray=single -PIC', &
                & is_built_in=.true.), &
              & new_profile('release', &
                &'lfortran', &
                & OS_ALL, &
                & flags = ' flag_lfortran_opt', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'caf', &
                & OS_ALL, &
                & flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -fbacktrace', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'gfortran', &
                & OS_ALL, &
                & flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -fbacktrace -fcoarray=single', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'f95', &
                & OS_ALL, &
                & flags = ' -Wall -Wextra -Wimplicit-interface -fPIC -fmax-errors=1 -g -fcheck=bounds&
                          & -fcheck=array-temps -Wno-maybe-uninitialized -Wno-uninitialized -fbacktrace', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'nvfortran', &
                & OS_ALL, &
                & flags = ' -Minform=inform -Mbackslash -g -Mbounds -Mchkptr -Mchkstk -traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifort', &
                & OS_ALL, &
                & flags = ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifort', &
                & OS_WINDOWS, &
                & flags = ' /warn:all /check:all /error-limit:1&
                          & /Od /Z7 /assume:byterecl /traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifx', &
                & OS_ALL, &
                & flags = ' -warn all -check all -error-limit 1 -O0 -g -assume byterecl -traceback', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifx', &
                & OS_WINDOWS, &
                & flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'ifx', &
                & OS_WINDOWS, &
                & flags = ' /warn:all /check:all /error-limit:1 /Od /Z7 /assume:byterecl', &
                & is_built_in=.true.), &
              & new_profile('debug', &
                & 'lfortran', &
                & OS_ALL, &
                & flags = '', &
                & is_built_in=.true.) &
              &]
      end function get_default_profiles

      !> Write information on instance
      subroutine info(self, unit, verbosity)

        !> Instance of the profile configuration
        class(profile_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        write(unit, fmt) "Profile"
        if (allocated(self%profile_name)) then
            write(unit, fmt) "- profile name", self%profile_name
        end if

        if (allocated(self%compiler)) then
            write(unit, fmt) "- compiler", self%compiler
        end if

        write(unit, fmt) "- os", self%os_type

        if (allocated(self%flags)) then
            write(unit, fmt) "- compiler flags", self%flags
        end if

      end subroutine info

      !> Print a representation of profile_config_t
      function info_profile(profile) result(s)

        !> Profile to be represented
        type(profile_config_t), intent(in) :: profile

        !> String representation of given profile
        character(:), allocatable :: s

        integer :: i

        s = "profile_config_t("
        s = s // 'profile_name="' // profile%profile_name // '"'
        s = s // ', compiler="' // profile%compiler // '"'
        s = s // ", os_type="
        select case(profile%os_type)
        case (OS_UNKNOWN)
          s = s // "OS_UNKNOWN"
        case (OS_LINUX)
          s = s // "OS_LINUX"
        case (OS_MACOS)
          s = s // "OS_MACOS"
        case (OS_WINDOWS)
          s = s // "OS_WINDOWS"
        case (OS_CYGWIN)
          s = s // "OS_CYGWIN"
        case (OS_SOLARIS)
          s = s // "OS_SOLARIS"
        case (OS_FREEBSD)
          s = s // "OS_FREEBSD"
        case (OS_OPENBSD)
          s = s // "OS_OPENBSD"
        case (OS_ALL)
          s = s // "OS_ALL"
        case default
          s = s // "INVALID"
        end select
        if (allocated(profile%flags)) s = s // ', flags="' // profile%flags // '"'
        if (allocated(profile%c_flags)) s = s // ', c_flags="' // profile%c_flags // '"'
        if (allocated(profile%cxx_flags)) s = s // ', cxx_flags="' // profile%cxx_flags // '"'
        if (allocated(profile%link_time_flags)) s = s // ', link_time_flags="' // profile%link_time_flags // '"'
        if (allocated(profile%file_scope_flags)) then
          do i=1,size(profile%file_scope_flags)
            s = s // ', flags for '//profile%file_scope_flags(i)%file_name// &
                    & ' ="' // profile%file_scope_flags(i)%flags // '"'
          end do
        end if
        s = s // ")"

      end function info_profile

      !> Look for profile with given configuration in array profiles
      subroutine find_profile(profiles, profile_name, compiler, os_type, found_matching, chosen_profile)

        !> Array of profiles
        type(profile_config_t), allocatable, intent(in) :: profiles(:)

        !> Name of profile
        character(:), allocatable, intent(in) :: profile_name

        !> Name of compiler
        character(:), allocatable, intent(in) :: compiler

        !> Type of operating system (enum)
        integer, intent(in) :: os_type

        !> Boolean value containing true if matching profile was found
        logical, intent(out) :: found_matching

        !> Last matching profile in the profiles array
        type(profile_config_t), intent(out) :: chosen_profile

        character(:), allocatable :: curr_profile_name
        character(:), allocatable :: curr_compiler
        integer :: curr_os
        integer :: i, priority, curr_priority

        found_matching = .false.
        if (size(profiles) < 1) return
        ! Try to find profile with matching OS type
        do i=1,size(profiles)
          curr_profile_name = profiles(i)%profile_name
          curr_compiler = profiles(i)%compiler
          curr_os = profiles(i)%os_type
          if (curr_profile_name.eq.profile_name) then
            if (curr_compiler.eq.compiler) then
              if (curr_os.eq.os_type) then
                chosen_profile = profiles(i)
                found_matching = .true.
              end if
            end if
          end if
        end do
        ! Try to find profile with OS type 'all'
        if (.not. found_matching) then
          do i=1,size(profiles)
            curr_profile_name = profiles(i)%profile_name
            curr_compiler = profiles(i)%compiler
            curr_os = profiles(i)%os_type
            if (curr_profile_name.eq.profile_name) then
              if (curr_compiler.eq.compiler) then
                if (curr_os.eq.OS_ALL) then
                  chosen_profile = profiles(i)
                  found_matching = .true.
                end if
              end if
            end if
          end do
        end if
      end subroutine find_profile
end module fpm_manifest_profile
 
 
!>>>>> ././src/fpm/manifest/executable.f90
!> Implementation of the meta data for an executables.
!>
!> An executable table can currently have the following fields
!>
!>```toml
!>[[ executable ]]
!>name = "string"
!>source-dir = "path"
!>main = "file"
!>link = ["lib"]
!>[executable.dependencies]
!>```
module fpm_manifest_executable
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_error, only : error_t, syntax_error, bad_name_error
    use fpm_strings, only : string_t
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: executable_config_t, new_executable


    !> Configuation meta data for an executable
    type :: executable_config_t

        !> Name of the resulting executable
        character(len=:), allocatable :: name

        !> Source directory for collecting the executable
        character(len=:), allocatable :: source_dir

        !> Name of the source file declaring the main program
        character(len=:), allocatable :: main

        !> Dependency meta data for this executable
        type(dependency_config_t), allocatable :: dependency(:)

        !> Libraries to link against
        type(string_t), allocatable :: link(:)

    contains

        !> Print information on this instance
        procedure :: info

    end type executable_config_t


contains


    !> Construct a new executable configuration from a TOML data structure
    subroutine new_executable(self, table, error)

        !> Instance of the executable configuration
        type(executable_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve executable name")
           return
        end if
        if (bad_name_error(error,'executable',self%name))then
           return
        endif
        call get_value(table, "source-dir", self%source_dir, "app")
        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error=error)
            if (allocated(error)) return
        end if

        call get_list(table, "link", self%link, error)
        if (allocated(error)) return

    end subroutine new_executable


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Executable section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed as executable entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies", "link")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Executable name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the executable configuration
        class(executable_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Executable target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir /= "app" .or. pr > 2) then
                write(unit, fmt) "- source directory", self%source_dir
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- program source", self%main
            end if
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


end module fpm_manifest_executable
 
 
!>>>>> ././src/fpm/manifest/test.f90
!> Implementation of the meta data for a test.
!>
!> The test data structure is effectively a decorated version of an executable
!> and shares most of its properties, except for the defaults and can be
!> handled under most circumstances just like any other executable.
!>
!> A test table can currently have the following fields
!>
!>```toml
!>[[ test ]]
!>name = "string"
!>source-dir = "path"
!>main = "file"
!>link = ["lib"]
!>[test.dependencies]
!>```
module fpm_manifest_test
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_manifest_executable, only : executable_config_t
    use fpm_error, only : error_t, syntax_error, bad_name_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: test_config_t, new_test


    !> Configuation meta data for an test
    type, extends(executable_config_t) :: test_config_t

    contains

        !> Print information on this instance
        procedure :: info

    end type test_config_t


contains


    !> Construct a new test configuration from a TOML data structure
    subroutine new_test(self, table, error)

        !> Instance of the test configuration
        type(test_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve test name")
           return
        end if
        if (bad_name_error(error,'test',self%name))then
           return
        endif
        call get_value(table, "source-dir", self%source_dir, "test")
        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error=error)
            if (allocated(error)) return
        end if

        call get_list(table, "link", self%link, error)
        if (allocated(error)) return

    end subroutine new_test


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Test section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in test entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies", "link")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Test name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the test configuration
        class(test_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Test target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir /= "test" .or. pr > 2) then
                write(unit, fmt) "- source directory", self%source_dir
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- test source", self%main
            end if
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


end module fpm_manifest_test
 
 
!>>>>> ././src/fpm/manifest/example.f90
!> Implementation of the meta data for an example.
!>
!> The example data structure is effectively a decorated version of an executable
!> and shares most of its properties, except for the defaults and can be
!> handled under most circumstances just like any other executable.
!>
!> A example table can currently have the following fields
!>
!>```toml
!>[[ example ]]
!>name = "string"
!>source-dir = "path"
!>main = "file"
!>link = ["lib"]
!>[example.dependencies]
!>```
module fpm_manifest_example
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_manifest_executable, only : executable_config_t
    use fpm_error, only : error_t, syntax_error, bad_name_error
    use fpm_toml, only : toml_table, toml_key, toml_stat, get_value, get_list
    implicit none
    private

    public :: example_config_t, new_example


    !> Configuation meta data for an example
    type, extends(executable_config_t) :: example_config_t

    contains

        !> Print information on this instance
        procedure :: info

    end type example_config_t


contains


    !> Construct a new example configuration from a TOML data structure
    subroutine new_example(self, table, error)

        !> Instance of the example configuration
        type(example_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_table), pointer :: child

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve example name")
           return
        end if
        if (bad_name_error(error,'example',self%name))then
           return
        endif
        call get_value(table, "source-dir", self%source_dir, "example")
        call get_value(table, "main", self%main, "main.f90")

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, error=error)
            if (allocated(error)) return
        end if

        call get_list(table, "link", self%link, error)
        if (allocated(error)) return

    end subroutine new_example


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Example section does not provide sufficient entries")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in example entry")
                exit

            case("name")
                name_present = .true.

            case("source-dir", "main", "dependencies", "link")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Example name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the example configuration
        class(example_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Example target"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if
        if (allocated(self%source_dir)) then
            if (self%source_dir /= "example" .or. pr > 2) then
                write(unit, fmt) "- source directory", self%source_dir
            end if
        end if
        if (allocated(self%main)) then
            if (self%main /= "main.f90" .or. pr > 2) then
                write(unit, fmt) "- example source", self%main
            end if
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


end module fpm_manifest_example
 
 
!>>>>> ././src/fpm/manifest/package.f90
!> Define the package data containing the meta data from the configuration file.
!>
!> The package data defines a Fortran type corresponding to the respective
!> TOML document, after creating it from a package file no more interaction
!> with the TOML document is required.
!>
!> Every configuration type provides it custom constructor (prefixed with `new_`)
!> and knows how to deserialize itself from a TOML document.
!> To ensure we find no untracked content in the package file all keywords are
!> checked and possible entries have to be explicitly allowed in the `check`
!> function.
!> If entries are mutally exclusive or interdependent inside the current table
!> the `check` function is required to enforce this schema on the data structure.
!>
!> The package file root allows the following keywords
!>
!>```toml
!>name = "string"
!>version = "string"
!>license = "string"
!>author = "string"
!>maintainer = "string"
!>copyright = "string"
!>[library]
!>[dependencies]
!>[dev-dependencies]
!>[profiles]
!>[build]
!>[install]
!>[[ executable ]]
!>[[ example ]]
!>[[ test ]]
!>[extra]
!>```
module fpm_manifest_package
    use fpm_manifest_build, only: build_config_t, new_build_config
    use fpm_manifest_dependency, only : dependency_config_t, new_dependencies
    use fpm_manifest_profile, only : profile_config_t, new_profiles, get_default_profiles
    use fpm_manifest_example, only : example_config_t, new_example
    use fpm_manifest_executable, only : executable_config_t, new_executable
    use fpm_manifest_library, only : library_config_t, new_library
    use fpm_manifest_install, only: install_config_t, new_install_config
    use fpm_manifest_test, only : test_config_t, new_test
    use fpm_mainfest_preprocess, only : preprocess_config_t, new_preprocessors
    use fpm_filesystem, only : exists, getline, join_path
    use fpm_error, only : error_t, fatal_error, syntax_error, bad_name_error
    use fpm_toml, only : toml_table, toml_array, toml_key, toml_stat, get_value, &
        & len
    use fpm_versioning, only : version_t, new_version
    use fpm_filesystem, only: join_path
    implicit none
    private

    public :: package_config_t, new_package


    interface unique_programs
        module procedure :: unique_programs1
        module procedure :: unique_programs2
    end interface unique_programs


    !> Package meta data
    type :: package_config_t

        !> Name of the package
        character(len=:), allocatable :: name

        !> Package version
        type(version_t) :: version

        !> Build configuration data
        type(build_config_t) :: build

        !> Installation configuration data
        type(install_config_t) :: install

        !> Library meta data
        type(library_config_t), allocatable :: library

        !> Executable meta data
        type(executable_config_t), allocatable :: executable(:)

        !> Dependency meta data
        type(dependency_config_t), allocatable :: dependency(:)

        !> Development dependency meta data
        type(dependency_config_t), allocatable :: dev_dependency(:)

        !> Profiles meta data
        type(profile_config_t), allocatable :: profiles(:)

        !> Example meta data
        type(example_config_t), allocatable :: example(:)

        !> Test meta data
        type(test_config_t), allocatable :: test(:)

        !> Preprocess meta data
        type(preprocess_config_t), allocatable :: preprocess(:)

    contains

        !> Print information on this instance
        procedure :: info

    end type package_config_t


contains


    !> Construct a new package configuration from a TOML data structure
    subroutine new_package(self, table, root, error)

        !> Instance of the package configuration
        type(package_config_t), intent(out) :: self

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Root directory of the manifest
        character(len=*), intent(in), optional :: root

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        ! Backspace (8), tabulator (9), newline (10), formfeed (12) and carriage
        ! return (13) are invalid in package names
        character(len=*), parameter :: invalid_chars = &
           achar(8) // achar(9) // achar(10) // achar(12) // achar(13)
        type(toml_table), pointer :: child, node
        type(toml_array), pointer :: children
        character(len=:), allocatable :: version, version_file
        integer :: ii, nn, stat, io

        call check(table, error)
        if (allocated(error)) return

        call get_value(table, "name", self%name)
        if (.not.allocated(self%name)) then
           call syntax_error(error, "Could not retrieve package name")
           return
        end if
        if (bad_name_error(error,'package',self%name))then
           return
        endif

        if (len(self%name) <= 0) then
            call syntax_error(error, "Package name must be a non-empty string")
            return
        end if

        ii = scan(self%name, invalid_chars)
        if (ii > 0) then
            call syntax_error(error, "Package name contains invalid characters")
            return
        end if

        call get_value(table, "build", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for build entry, must be a table")
            return
        end if
        call new_build_config(self%build, child, error)
        if (allocated(error)) return

        call get_value(table, "install", child, requested=.true., stat=stat)
        if (stat /= toml_stat%success) then
            call fatal_error(error, "Type mismatch for install entry, must be a table")
            return
        end if
        call new_install_config(self%install, child, error)
        if (allocated(error)) return

        call get_value(table, "version", version, "0")
        call new_version(self%version, version, error)
        if (allocated(error) .and. present(root)) then
            version_file = join_path(root, version)
            if (exists(version_file)) then
                deallocate(error)
                open(file=version_file, newunit=io, iostat=stat)
                if (stat == 0) then
                    call getline(io, version, iostat=stat)
                end if
                if (stat == 0) then
                    close(io, iostat=stat)
                end if
                if (stat == 0) then
                    call new_version(self%version, version, error)
                else
                    call fatal_error(error, "Reading version number from file '" &
                        & //version_file//"' failed")
                end if
            end if
        end if
        if (allocated(error)) return

        call get_value(table, "dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dependency, child, root, error)
            if (allocated(error)) return
        end if

        call get_value(table, "dev-dependencies", child, requested=.false.)
        if (associated(child)) then
            call new_dependencies(self%dev_dependency, child, root, error)
            if (allocated(error)) return
        end if

        call get_value(table, "library", child, requested=.false.)
        if (associated(child)) then
            allocate(self%library)
            call new_library(self%library, child, error)
            if (allocated(error)) return
        end if
        
        call get_value(table, "profiles", child, requested=.false.)
        if (associated(child)) then
            call new_profiles(self%profiles, child, error)
            if (allocated(error)) return
        else
            self%profiles = get_default_profiles(error)
            if (allocated(error)) return
        end if

        call get_value(table, "executable", children, requested=.false.)
        if (associated(children)) then
            nn = len(children)
            allocate(self%executable(nn))
            do ii = 1, nn
                call get_value(children, ii, node, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Could not retrieve executable from array entry")
                    exit
                end if
                call new_executable(self%executable(ii), node, error)
                if (allocated(error)) exit
            end do
            if (allocated(error)) return

            call unique_programs(self%executable, error)
            if (allocated(error)) return
        end if

        call get_value(table, "example", children, requested=.false.)
        if (associated(children)) then
            nn = len(children)
            allocate(self%example(nn))
            do ii = 1, nn
                call get_value(children, ii, node, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Could not retrieve example from array entry")
                    exit
                end if
                call new_example(self%example(ii), node, error)
                if (allocated(error)) exit
            end do
            if (allocated(error)) return

            call unique_programs(self%example, error)
            if (allocated(error)) return

            if (allocated(self%executable)) then
                call unique_programs(self%executable, self%example, error)
                if (allocated(error)) return
            end if
        end if

        call get_value(table, "test", children, requested=.false.)
        if (associated(children)) then
            nn = len(children)
            allocate(self%test(nn))
            do ii = 1, nn
                call get_value(children, ii, node, stat=stat)
                if (stat /= toml_stat%success) then
                    call fatal_error(error, "Could not retrieve test from array entry")
                    exit
                end if
                call new_test(self%test(ii), node, error)
                if (allocated(error)) exit
            end do
            if (allocated(error)) return

            call unique_programs(self%test, error)
            if (allocated(error)) return
        end if

        call get_value(table, "preprocess", child, requested=.false.)
        if (associated(child)) then
            call new_preprocessors(self%preprocess, child, error)
            if (allocated(error)) return
        end if
    end subroutine new_package


    !> Check local schema for allowed entries
    subroutine check(table, error)

        !> Instance of the TOML data structure
        type(toml_table), intent(inout) :: table

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        type(toml_key), allocatable :: list(:)
        logical :: name_present
        integer :: ikey

        name_present = .false.

        call table%get_keys(list)

        if (size(list) < 1) then
            call syntax_error(error, "Package file is empty")
            return
        end if

        do ikey = 1, size(list)
            select case(list(ikey)%key)
            case default
                call syntax_error(error, "Key "//list(ikey)%key//" is not allowed in package file")
                exit

            case("name")
                name_present = .true.

            case("version", "license", "author", "maintainer", "copyright", &
                    & "description", "keywords", "categories", "homepage", "build", &
                    & "dependencies", "dev-dependencies", "profiles", "test", "executable", &
                    & "example", "library", "install", "extra", "preprocess")
                continue

            end select
        end do
        if (allocated(error)) return

        if (.not.name_present) then
            call syntax_error(error, "Package name is not provided, please add a name entry")
        end if

    end subroutine check


    !> Write information on instance
    subroutine info(self, unit, verbosity)

        !> Instance of the package configuration
        class(package_config_t), intent(in) :: self

        !> Unit for IO
        integer, intent(in) :: unit

        !> Verbosity of the printout
        integer, intent(in), optional :: verbosity

        integer :: pr, ii
        character(len=*), parameter :: fmt = '("#", 1x, a, t30, a)', &
            & fmti = '("#", 1x, a, t30, i0)'

        if (present(verbosity)) then
            pr = verbosity
        else
            pr = 1
        end if

        if (pr < 1) return

        write(unit, fmt) "Package"
        if (allocated(self%name)) then
            write(unit, fmt) "- name", self%name
        end if

        call self%build%info(unit, pr - 1)

        call self%install%info(unit, pr - 1)

        if (allocated(self%library)) then
            write(unit, fmt) "- target", "archive"
            call self%library%info(unit, pr - 1)
        end if

        if (allocated(self%executable)) then
            if (size(self%executable) > 1 .or. pr > 2) then
                write(unit, fmti) "- executables", size(self%executable)
            end if
            do ii = 1, size(self%executable)
                call self%executable(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%dependency)) then
            if (size(self%dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- dependencies", size(self%dependency)
            end if
            do ii = 1, size(self%dependency)
                call self%dependency(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%example)) then
            if (size(self%example) > 1 .or. pr > 2) then
                write(unit, fmti) "- examples", size(self%example)
            end if
            do ii = 1, size(self%example)
                call self%example(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%test)) then
            if (size(self%test) > 1 .or. pr > 2) then
                write(unit, fmti) "- tests", size(self%test)
            end if
            do ii = 1, size(self%test)
                call self%test(ii)%info(unit, pr - 1)
            end do
        end if

        if (allocated(self%dev_dependency)) then
            if (size(self%dev_dependency) > 1 .or. pr > 2) then
                write(unit, fmti) "- development deps.", size(self%dev_dependency)
            end if
            do ii = 1, size(self%dev_dependency)
                call self%dev_dependency(ii)%info(unit, pr - 1)
            end do
        end if
        
        if (allocated(self%profiles)) then
            if (size(self%profiles) > 1 .or. pr > 2) then
                write(unit, fmti) "- profiles", size(self%profiles)
            end if
            do ii = 1, size(self%profiles)
                call self%profiles(ii)%info(unit, pr - 1)
            end do
        end if

    end subroutine info


    !> Check whether or not the names in a set of executables are unique
    subroutine unique_programs1(executable, error)

        !> Array of executables
        class(executable_config_t), intent(in) :: executable(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i, j

        do i = 1, size(executable)
            do j = 1, i - 1
                if (executable(i)%name == executable(j)%name) then
                    call fatal_error(error, "The program named '"//&
                        executable(j)%name//"' is duplicated. "//&
                        "Unique program names are required.")
                    exit
                end if
            end do
        end do
        if (allocated(error)) return

    end subroutine unique_programs1


    !> Check whether or not the names in a set of executables are unique
    subroutine unique_programs2(executable_i, executable_j, error)

        !> Array of executables
        class(executable_config_t), intent(in) :: executable_i(:)

        !> Array of executables
        class(executable_config_t), intent(in) :: executable_j(:)

        !> Error handling
        type(error_t), allocatable, intent(out) :: error

        integer :: i, j

        do i = 1, size(executable_i)
            do j = 1, size(executable_j)
                if (executable_i(i)%name == executable_j(j)%name) then
                    call fatal_error(error, "The program named '"//&
                        executable_j(j)%name//"' is duplicated. "//&
                        "Unique program names are required.")
                    exit
                end if
            end do
        end do
        if (allocated(error)) return

    end subroutine unique_programs2


end module fpm_manifest_package
 
 
!>>>>> ././src/fpm/manifest.f90
!> Package configuration data.
!>
!> This module provides the necessary procedure to translate a TOML document
!> to the corresponding Fortran type, while verifying it with respect to
!> its schema.
!>
!> Additionally, the required data types for users of this module are reexported
!> to hide the actual implementation details.
module fpm_manifest
    use fpm_manifest_build, only: build_config_t
    use fpm_manifest_example, only : example_config_t
    use fpm_manifest_executable, only : executable_config_t
    use fpm_manifest_dependency, only : dependency_config_t
    use fpm_manifest_library, only : library_config_t
    use fpm_mainfest_preprocess, only : preprocess_config_t
    use fpm_manifest_package, only : package_config_t, new_package
    use fpm_error, only : error_t, fatal_error
    use fpm_toml, only : toml_table, read_package_file
    use fpm_manifest_test, only : test_config_t
    use fpm_filesystem, only: join_path, exists, dirname, is_dir
    use fpm_strings, only: string_t
    implicit none
    private

    public :: get_package_data, default_executable, default_library, default_test
    public :: default_example
    public :: package_config_t, dependency_config_t, preprocess_config_t


contains


    !> Populate library in case we find the default src directory
    subroutine default_library(self)

        !> Instance of the library meta data
        type(library_config_t), intent(out) :: self

        self%source_dir = "src"
        self%include_dir = [string_t("include")]

    end subroutine default_library


    !> Populate executable in case we find the default app directory
    subroutine default_executable(self, name)

        !> Instance of the executable meta data
        type(executable_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name
        self%source_dir = "app"
        self%main = "main.f90"

    end subroutine default_executable

    !> Populate test in case we find the default example/ directory
    subroutine default_example(self, name)

        !> Instance of the executable meta data
        type(example_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name // "-demo"
        self%source_dir = "example"
        self%main = "main.f90"

    end subroutine default_example

    !> Populate test in case we find the default test/ directory
    subroutine default_test(self, name)

        !> Instance of the executable meta data
        type(test_config_t), intent(out) :: self

        !> Name of the package
        character(len=*), intent(in) :: name

        self%name = name // "-test"
        self%source_dir = "test"
        self%main = "main.f90"

    end subroutine default_test


    !> Obtain package meta data from a configuation file
    subroutine get_package_data(package, file, error, apply_defaults)

        !> Parsed package meta data
        type(package_config_t), intent(out) :: package

        !> Name of the package configuration file
        character(len=*), intent(in) :: file

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        !> Apply package defaults (uses file system operations)
        logical, intent(in), optional :: apply_defaults

        type(toml_table), allocatable :: table
        character(len=:), allocatable :: root

        call read_package_file(table, file, error)
        if (allocated(error)) return

        if (.not.allocated(table)) then
            call fatal_error(error, "Unclassified error while reading: '"//file//"'")
            return
        end if

        call new_package(package, table, dirname(file), error)
        if (allocated(error)) return

        if (present(apply_defaults)) then
            if (apply_defaults) then
                root = dirname(file)
                if (len_trim(root) == 0) root = "."
                call package_defaults(package, root, error)
                if (allocated(error)) return
            end if
        end if

    end subroutine get_package_data


    !> Apply package defaults
    subroutine package_defaults(package, root, error)

        !> Parsed package meta data
        type(package_config_t), intent(inout) :: package

        !> Current working directory
        character(len=*), intent(in) :: root

        !> Error status of the operation
        type(error_t), allocatable, intent(out) :: error

        ! Populate library in case we find the default src directory
        if (.not.allocated(package%library) .and. &
            & (is_dir(join_path(root, "src")) .or. &
            &  is_dir(join_path(root, "include")))) then

            allocate(package%library)
            call default_library(package%library)
        end if

        ! Populate executable in case we find the default app
        if (.not.allocated(package%executable) .and. &
            & exists(join_path(root, "app", "main.f90"))) then
            allocate(package%executable(1))
            call default_executable(package%executable(1), package%name)
        end if

        ! Populate example in case we find the default example directory
        if (.not.allocated(package%example) .and. &
            & exists(join_path(root, "example", "main.f90"))) then
            allocate(package%example(1))
            call default_example(package%example(1), package%name)
        endif

        ! Populate test in case we find the default test directory
        if (.not.allocated(package%test) .and. &
            & exists(join_path(root, "test", "main.f90"))) then
            allocate(package%test(1))
            call default_test(package%test(1), package%name)
        endif

        if (.not.(allocated(package%library) &
            & .or. allocated(package%executable) &
            & .or. allocated(package%example) &
            & .or. allocated(package%test))) then
            call fatal_error(error, "Neither library nor executable found, there is nothing to do")
            return
        end if

    end subroutine package_defaults


end module fpm_manifest
 
 
!>>>>> ././src/fpm/cmd/new.f90
module fpm_cmd_new
!># Definition of the "new" subcommand
!>
!> A type of the general command base class [[fpm_cmd_settings]]
!> was created for the "new" subcommand ==> type [[fpm_new_settings]].
!> This procedure read the values that were set on the command line
!> from this type to decide what actions to take.
!>
!> It is virtually self-contained and so independant of the rest of the
!> application that it could function as a separate program.
!>
!> The "new" subcommand options currently consist of a SINGLE top
!> directory name to create that must have a name that is an
!> allowable Fortran variable name. That should have been ensured
!> by the command line processing before this procedure is called.
!> So basically this routine has already had the options vetted and
!> just needs to conditionally create a few files.
!>
!> As described in the documentation it will selectively
!> create the subdirectories app/, test/, src/, and example/
!> and populate them with sample files.
!>
!> It also needs to create an initial manifest file "fpm.toml".
!>
!> It then calls the system command "git init".
!>
!> It should test for file existence and not overwrite existing
!> files and inform the user if there were conflicts.
!>
!> Any changes should be reflected in the documentation in
!> [[fpm_command_line.f90]]
!>
!> FUTURE
!> A filename like "." would need system commands or a standard routine
!> like realpath(3c) to process properly.
!>
!> Perhaps allow more than one name on a single command. It is an arbitrary
!> restriction based on a concensus preference, not a required limitation.
!>
!> Initially the name of the directory is used as the module name in the
!> src file so it must be an allowable Fortran variable name. If there are
!> complaints about it it might be changed. Handling unicode at this point
!> might be problematic as not all current compilers handle it. Other
!> utilities like content trackers (ie. git) or repositories like github
!> might also have issues with alternative names or names with spaces, etc.
!> So for the time being it seems prudent to encourage simple ASCII top directory
!> names (similiar to the primary programming language Fortran itself).
!>
!> Should be able to create or pull more complicated initial examples
!> based on various templates. It should place or mention other relevant
!> documents such as a description of the manifest file format in user hands;
!> or how to access registered packages and local packages,
!> although some other command might provide that (and the help command should
!> be the first go-to for a CLI utility).

use fpm_command_line, only : fpm_new_settings
use fpm_environment, only : OS_LINUX, OS_MACOS, OS_WINDOWS
use fpm_filesystem, only : join_path, exists, basename, mkdir, is_dir
use fpm_filesystem, only : fileopen, fileclose, filewrite, warnwrite, which, run
use fpm_strings, only : join, to_fortran_name
use fpm_error, only : fpm_stop

use,intrinsic :: iso_fortran_env, only : stderr=>error_unit
implicit none
private
public :: cmd_new

contains

subroutine cmd_new(settings)
type(fpm_new_settings), intent(in) :: settings
integer,parameter            :: tfc = selected_char_kind('DEFAULT')
character(len=:,kind=tfc),allocatable :: bname          ! baeename of NAME
character(len=:,kind=tfc),allocatable :: tomlfile(:)
character(len=:,kind=tfc),allocatable :: littlefile(:)

    !> TOP DIRECTORY NAME PROCESSING
    !> see if requested new directory already exists and process appropriately
    if(exists(settings%name) .and. .not.settings%backfill )then
        write(stderr,'(*(g0,1x))')&
        & '<ERROR>',settings%name,'already exists.'
        write(stderr,'(*(g0,1x))')&
        & '        perhaps you wanted to add --backfill ?'
        return
    elseif(is_dir(settings%name) .and. settings%backfill )then
        write(*,'(*(g0))')'backfilling ',settings%name
    elseif(exists(settings%name) )then
        write(stderr,'(*(g0,1x))')&
        & '<ERROR>',settings%name,'already exists and is not a directory.'
        return
    else
        ! make new directory
        call mkdir(settings%name)
    endif

    !> temporarily change to new directory as a test. NB: System dependent
    call run('cd '//settings%name)
    ! NOTE: need some system routines to handle filenames like "."
    ! like realpath() or getcwd().
    bname=basename(settings%name)

    littlefile=[character(len=80) :: '# '//bname, 'My cool new project!']

    ! create NAME/README.md
    call warnwrite(join_path(settings%name, 'README.md'), littlefile)

    ! start building NAME/fpm.toml
    if(settings%with_full)then
        tomlfile=[character(len=80) :: &
        &'  # This is your fpm(Fortran Package Manager) manifest file                     ',&
        &'  # ("fpm.toml"). It is heavily annotated to help guide you though              ',&
        &'  # customizing a package build, although the defaults are sufficient           ',&
        &'  # for many basic packages.                                                    ',&
        &'  #                                                                             ',&
        &'  # The manifest file is not only used to provide metadata identifying          ',&
        &'  # your project (so it can be used by others as a dependency). It can          ',&
        &'  # specify where your library and program sources live, what the name          ',&
        &'  # of the executable(s) will be, what files to build, dependencies on          ',&
        &'  # other fpm packages, and what external libraries are required.               ',&
        &'  #                                                                             ',&
        &'  # The manifest format must conform to the TOML configuration file             ',&
        &'  # standard.                                                                   ',&
        &'  #                                                                             ',&
        &'  # TOML files support flexible use of white-space and commenting of the        ',&
        &'  # configuration data, but for clarity in this sample active directives        ',&
        &'  # begin in column one. Inactive example directives are commented              ',&
        &'  # out with a pound character ("#") but begin in column one as well.           ',&
        &'  # Commentary begins with a pound character in column three.                   ',&
        &'  #                                                                             ',&
        &'  # This file draws heavily upon the following references:                      ',&
        &'  #                                                                             ',&
        &'  # The fpm home page at                                                        ',&
        &'  #     https://github.com/fortran-lang/fpm                                     ',&
        &'  # A complete list of keys and their attributes at                             ',&
        &'  #     https://github.com/fortran-lang/fpm/blob/main/manifest-reference.md     ',&
        &'  # examples of fpm project packaging at                                        ',&
        &'  #     https://github.com/fortran-lang/fpm/blob/main/PACKAGING.md              ',&
        &'  # The Fortran TOML file interface and it''s references at                     ',&
        &'  #     https://github.com/toml-f/toml-f                                        ',&
        &'  #                                                                             ',&
        &'  #-----------------------                                                      ',&
        &'  # project Identification                                                      ',&
        &'  #-----------------------                                                      ',&
        &'  # We begin with project metadata at the manifest root. This data is designed  ',&
        &'  # to aid others when searching for the project in a repository and to         ',&
        &'  # identify how and when to contact the package supporters.                    ',&
        &'                                                                                ',&
        &'name = "'//bname//'"',&
        &'  # The project name (required) is how the project will be referred to.         ',&
        &'  # The name is used by other packages using it as a dependency. It also        ',&
        &'  # is used as the default name of any library built and the optional           ',&
        &'  # default executable built from app/main.f90. It must conform to the rules    ',&
        &'  # for a Fortran variable name.                                                ',&
        &'                                                                                ',&
        &'version = "0.1.0"                                                               ',&
        &'  # The project version number is a string. A recommended scheme for            ',&
        &'  # specifying versions is the Semantic Versioning scheme.                      ',&
        &'                                                                                ',&
        &'license = "license"                                                             ',&
        &'  # Licensing information specified using SPDX identifiers is preferred         ',&
        &'  # (eg. "Apache-2.0 OR MIT" or "LGPL-3.0-or-later").                           ',&
        &'                                                                                ',&
        &'maintainer = "jane.doe@example.com"                                             ',&
        &'  # Information on the project maintainer and means to reach out to them.       ',&
        &'                                                                                ',&
        &'author = "Jane Doe"                                                             ',&
        &'  # Information on the project author.                                          ',&
        &'                                                                                ',&
        &'copyright = "Copyright 2020 Jane Doe"                                           ',&
        &'  # A statement clarifying the Copyright status of the project.                 ',&
        &'                                                                                ',&
        &'#description = "A short project summary in plain text"                          ',&
        &'  # The description provides a short summary on the project. It should be       ',&
        &'  # plain text and not use any markup formatting.                               ',&
        &'                                                                                ',&
        &'#categories = ["fortran", "graphics"]                                           ',&
        &'  # Categories associated with the project. Listing only one is preferred.      ',&
        &'                                                                                ',&
        &'#keywords = ["hdf5", "mpi"]                                                     ',&
        &'  # The keywords field is an array of strings describing the project.           ',&
        &'                                                                                ',&
        &'#homepage = "https://stdlib.fortran-lang.org"                                   ',&
        &'  # URL to the webpage of the project.                                          ',&
        &'                                                                                ',&
        &'  # -----------------------------------------                                   ',&
        &'  # We are done with identifying the project.                                   ',&
        &'  # -----------------------------------------                                   ',&
        &'  #                                                                             ',&
        &'  # Now lets start describing how the project should be built.                  ',&
        &'  #                                                                             ',&
        &'  # Note tables would go here but we will not be talking about them (much)!!'    ,&
        &'  #                                                                             ',&
        &'  # Tables are a way to explicitly specify large numbers of programs in         ',&
        &'  # a compact format instead of individual per-program entries in the           ',&
        &'  # [[executable]], [[test]], and [[example]] sections to follow but            ',&
        &'  # will not be discussed further except for the following notes:               ',&
        &'  #                                                                             ',&
        &'  # + Tables must appear (here) before any sections are declared. Once a        ',&
        &'  #   section is specified in a TOML file everything afterwards must be         ',&
        &'  #   values for that section or the beginning of a new section. A simple       ',&
        &'  #   example looks like:                                                       ',&
        &'                                                                                ',&
        &'#executable = [                                                                 ',&
        &'#  { name = "a-prog" },                                                         ',&
        &'#  { name = "app-tool", source-dir = "tool" },                                  ',&
        &'#  { name = "fpm-man", source-dir = "tool", main="fman.f90" }                   ',&
        &'#]                                                                              ',&
        &'                                                                                ',&
        &'  # This would be in lieue of the [[executable]] section found later in this    ',&
        &'  # configuration file.                                                         ',&
        &'  # + See the reference documents (at the beginning of this document)           ',&
        &'  #   for more information on tables if you have long lists of programs         ',&
        &'  #   to build and are not simply depending on auto-detection.                  ',&
        &'  #                                                                             ',&
        &'  # Now lets begin the TOML sections (lines beginning with "[") ...             ',&
        &'  #                                                                             ',&
        &'                                                                                ',&
        &'[install] # Options for the "install" subcommand                                ',&
        &'                                                                                ',&
        &'  # When you run the "install" subcommand only executables are installed by     ',&
        &'  # default on the local system. Library projects that will be used outside of  ',&
        &'  # "fpm" can set the "library" boolean to also allow installing the module     ',&
        &'  # files and library archive. Without this being set to "true" an "install"    ',&
        &'  # subcommand ignores parameters that specify library installation.            ',&
        &'                                                                                ',&
        &'library = false                                                                 ',&
        &'                                                                                ',&
        &'[build] # General Build Options                                                 ',&
        &'                                                                                ',&
        &'  ###  Automatic target discovery                                               ',&
        &'  #                                                                             ',&
        &'  # Normally fpm recursively searches the app/, example/, and test/ directories ',&
        &'  # for program sources and builds them. To disable this automatic discovery of ',&
        &'  # program targets set the following to "false":                               ',&
        &'                                                                                ',&
        &'#auto-executables = true                                                        ',&
        &'#auto-examples = true                                                           ',&
        &'#auto-tests = true                                                              ',&
        &'                                                                                ',&
        &'  ### Package-level External Library Links                                      ',&
        &'  #                                                                             ',&
        &'  # To declare link-time dependencies on external libraries a list of           ',&
        &'  # native libraries can be specified with the "link" entry. You may            ',&
        &'  # have one library name or a list of strings in case several                  ',&
        &'  # libraries should be linked. This list of library dependencies is            ',&
        &'  # exported to dependent packages. You may have to alter your library          ',&
        &'  # search-path to ensure the libraries can be accessed. Typically,             ',&
        &'  # this is done with the LD_LIBRARY_PATH environment variable on ULS           ',&
        &'  # (Unix-Like Systems). You only specify the core name of the library          ',&
        &'  # (as is typical with most programming environments, where you                ',&
        &'  # would specify "-lz" on your load command to link against the zlib           ',&
        &'  # compression library even though the library file would typically be         ',&
        &'  # a file called "libz.a" "or libz.so"). So to link against that library       ',&
        &'  # you would specify:                                                          ',&
        &'                                                                                ',&
        &'#link = "z"                                                                     ',&
        &'                                                                                ',&
        &'  # Note that in some cases the order of the libraries matters:                 ',&
        &'                                                                                ',&
        &'#link = ["blas", "lapack"]                                                      ',&
        &'']
    endif

    if(settings%with_bare)then
    elseif(settings%with_lib)then
        call mkdir(join_path(settings%name,'src') )
        ! create next section of fpm.toml
        if(settings%with_full)then
            tomlfile=[character(len=80) ::  tomlfile, &
            &'[library]                                                                       ',&
            &'                                                                                ',&
            &'  # You can change the name of the directory to search for your library         ',&
            &'  # source from the default of "src/". Library targets are exported             ',&
            &'  # and usable by other projects.                                               ',&
            &'                                                                                ',&
            &'source-dir="src"                                                                ',&
            &'                                                                                ',&
            &'  # this can be a list:                                                         ',&
            &'                                                                                ',&
            &'#source-dir=["src", "src2"]                                                     ',&
            &'                                                                                ',&
            &'  # More complex libraries may organize their modules in subdirectories.        ',&
            &'  # For modules in a top-level directory fpm requires (but does not             ',&
            &'  # enforce) that:                                                              ',&
            &'  #                                                                             ',&
            &'  #  + The module has the same name as the source file. This is important.      ',&
            &'  #  + There should be only one module per file.                                ',&
            &'  #                                                                             ',&
            &'  # These two requirements simplify the build process for fpm. As Fortran       ',&
            &'  # compilers emit module files (.mod) with the same name as the module         ',&
            &'  # itself (but not the source file, .f90), naming the module the same          ',&
            &'  # as the source file allows fpm to:                                           ',&
            &'  #                                                                             ',&
            &'  #  + Uniquely and exactly map a source file (.f90) to its object (.o)         ',&
            &'  #    and module (.mod) files.                                                 ',&
            &'  #  + Avoid conflicts with modules of the same name that could appear          ',&
            &'  #    in dependency packages.                                                  ',&
            &'  #                                                                             ',&
            &'  ### Multi-level library source                                                ',&
            &'  # You can place your module source files in any number of levels of           ',&
            &'  # subdirectories inside your source directory, but there are certain naming   ',&
            &'  # conventions to be followed -- module names must contain the path components ',&
            &'  # of the directory that its source file is in.                                ',&
            &'  #                                                                             ',&
            &'  # This rule applies generally to any number of nested directories and         ',&
            &'  # modules. For example, src/a/b/c/d.f90 must define a module called a_b_c_d.  ',&
            &'  # Again, this is not enforced but may be required in future releases.         ',&
            &'']
        endif
        ! create placeholder module src/bname.f90
        littlefile=[character(len=80) ::          &
        &'module '//to_fortran_name(bname),       &
        &'  implicit none',                       &
        &'  private',                             &
        &'',                                      &
        &'  public :: say_hello',                 &
        &'contains',                              &
        &'  subroutine say_hello',                &
        &'    print *, "Hello, '//bname//'!"',    &
        &'  end subroutine say_hello',            &
        &'end module '//to_fortran_name(bname)]
        ! create NAME/src/NAME.f90
        call warnwrite(join_path(settings%name, 'src', bname//'.f90'),&
         & littlefile)
    endif

    if(settings%with_full)then
        tomlfile=[character(len=80) ::  tomlfile ,&
        &'[dependencies]                                                                  ',&
        &'                                                                                ',&
        &'  # Inevitably, you will want to be able to include other packages in           ',&
        &'  # a project. Fpm makes this incredibly simple, by taking care of              ',&
        &'  # fetching and compiling your dependencies for you. You just tell it          ',&
        &'  # what your dependencies names are, and where to find them.                   ',&
        &'  #                                                                             ',&
        &'  # If you are going to distribute your package only place dependencies         ',&
        &'  # here someone using your package as a remote dependency needs built.         ',&
        &'  # You can define dependencies just for developer executables in the           ',&
        &'  # next section, or even for specific executables as we will see below         ',&
        &'  # (Then fpm will still fetch and compile it when building your                ',&
        &'  # developer executables, but users of your library will not have to).         ',&
        &'  #                                                                             ',&
        &'  ## GLOBAL DEPENDENCIES (exported with your project)                           ',&
        &'  #                                                                             ',&
        &'  # Typically, dependencies are defined by specifying the project''s            ',&
        &'  # git repository.                                                             ',&
        &'  #                                                                             ',&
        &'  # You can be specific about which version of a dependency you would           ',&
        &'  # like. By default the latest default branch is used. You can           ',&
        &'  # optionally specify a branch, a tag or a commit value.                       ',&
        &'  #                                                                             ',&
        &'  # So here are several alternates for specifying a remote dependency (you      ',&
        &'  # can have at most one of "branch", "rev" or "tag" present):                  ',&
        &'                                                                                ',&
        &'#stdlib = { git = "https://github.com/LKedward/stdlib-fpm.git" }                ',&
        &'#stdlib = {git="https://github.com/LKedward/stdlib-fpm.git",branch = "master" },',&
        &'#stdlib = {git="https://github.com/LKedward/stdlib-fpm.git", tag = "v0.1.0" },  ',&
        &'#stdlib = {git="https://github.com/LKedward/stdlib-fpm.git", rev = "5a9b7a8" }. ',&
        &'                                                                                ',&
        &'  # There may be multiple packages listed:                                      ',&
        &'                                                                                ',&
        &'#M_strings = { git = "https://github.com/urbanjost/M_strings.git" }             ',&
        &'#M_time    = { git = "https://github.com/urbanjost/M_time.git" }                ',&
        &'                                                                                ',&
        &'  #                                                                             ',&
        &'  # You can even specify the local path to another project if it is in          ',&
        &'  # a sub-folder (If for example you have got another fpm package **in          ',&
        &'  # the same repository**) like this:                                           ',&
        &'                                                                                ',&
        &'#M_strings = { path = "M_strings" }                                             ',&
        &'                                                                                ',&
        &'  # This tells fpm that we depend on a crate called M_strings which is found    ',&
        &'  # in the M_strings folder (relative to the fpm.toml it’s written in).         ',&
        &'  #                                                                             ',&
        &'  # For a more verbose layout use normal tables rather than inline tables       ',&
        &'  # to specify dependencies:                                                    ',&
        &'                                                                                ',&
        &'#[dependencies.toml-f]                                                          ',&
        &'#git = "https://github.com/toml-f/toml-f"                                       ',&
        &'#rev = "2f5eaba864ff630ba0c3791126a3f811b6e437f3"                               ',&
        &'                                                                                ',&
        &'  # Now you can use any modules from these libraries anywhere in your           ',&
        &'  # code -- whether is in your library source or a program source.              ',&
        &'                                                                                ',&
        &'[dev-dependencies]                                                              ',&
        &'                                                                                ',&
        &'  ## Dependencies Only for Development                                          ',&
        &'  #                                                                             ',&
        &'  # You can specify dependencies your library or application does not           ',&
        &'  # depend on in a similar way. The difference is that these will not           ',&
        &'  # be exported as part of your project to those using it as a remote           ',&
        &'  # dependency.                                                                 ',&
        &'  #                                                                             ',&
        &'  # Currently, like a global dependency it will still be available for          ',&
        &'  # all codes. It is up to the developer to ensure that nothing except          ',&
        &'  # developer test programs rely upon it.                                       ',&
        &'                                                                                ',&
        &'#M_msg    = { git = "https://github.com/urbanjost/M_msg.git" }                  ',&
        &'#M_verify = { git = "https://github.com/urbanjost/M_verify.git" }               ',&
        &'']
    endif
    if(settings%with_bare)then
    elseif(settings%with_executable)then
        ! create next section of fpm.toml
        call mkdir(join_path(settings%name, 'app'))
        ! create NAME/app or stop
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile, &
           &'  #-----------------------------------                                          ',&
           &'  ## Application-specific declarations                                          ',&
           &'  #-----------------------------------                                          ',&
           &'  # Now lets begin entries for the TOML tables (lines beginning with "[[")      ',&
           &'  # that describe the program sources -- applications, tests, and examples.     ',&
           &'  #                                                                             ',&
           &'  # First we will configuration individual applications run with "fpm run".     ',&
           &'  #                                                                             ',&
           &'  #   + the "name" entry for the executable to be built must always             ',&
           &'  #     be specified. The name must satisfy the rules for a Fortran             ',&
           &'  #     variable name. This will be the name of the binary installed by         ',&
           &'  #     the "install" subcommand and used on the "run" subcommand.              ',&
           &'  #   + The source directory for each executable can be adjusted by the         ',&
           &'  #     "source-dir" entry.                                                     ',&
           &'  #   + The basename of the source file containing the program body can         ',&
           &'  #     be specified with the "main" entry.                                     ',&
           &'  #   + Executables can also specify their own external package and             ',&
           &'  #     library link dependencies.                                              ',&
           &'  #                                                                             ',&
           &'  #     Currently, like a global dependency any external package dependency     ',&
           &'  #     will be available for all codes. It is up to the developer to ensure    ',&
           &'  #     that nothing except the application programs specified rely upon it.    ',&
           &'  #                                                                             ',&
           &'  # Note if your application needs to use a module internally, but you do not   ',&
           &'  # intend to build it as a library to be used in other projects, you can       ',&
           &'  # include the module in your program source file or directory as well.        ',&
           &'                                                                                ',&
           &'[[executable]]                                                                  ',&
           &'name="'//bname//'"',&
           &'source-dir="app"                                                                ',&
           &'main="main.f90"                                                                 ',&
           &'                                                                                ',&
           &'  # You may repeat this pattern to define additional applications. For instance,',&
           &'  # the following sample illustrates all accepted options, where "link" and     ',&
           &'  # "executable.dependencies" keys are the same as the global external library  ',&
           &'  # links and package dependencies described previously except they apply       ',&
           &'  # only to this executable:                                                    ',&
           &'                                                                                ',&
           &'#[[ executable ]]                                                               ',&
           &'#name = "app-name"                                                              ',&
           &'#source-dir = "prog"                                                            ',&
           &'#main = "program.f90"                                                           ',&
           &'#link = "z"                                                                     ',&
           &'#[executable.dependencies]                                                      ',&
           &'#M_CLI   = { git = "https://github.com/urbanjost/M_CLI.git" }                   ',&
           &'#helloff = { git = "https://gitlab.com/everythingfunctional/helloff.git" }      ',&
           &'#M_path  = { git = "https://github.com/urbanjost/M_path.git" }                  ',&
           &'']
        endif

        if(exists(bname//'/src/'))then
            littlefile=[character(len=80) ::          &
            &'program main',                          &
            &'  use '//to_fortran_name(bname)//', only: say_hello',    &
            &'  implicit none',                       &
            &'',                                      &
            &'  call say_hello()',                    &
            &'end program main']
        else
            littlefile=[character(len=80) ::                 &
            &'program main',                                 &
            &'  implicit none',                              &
            &'',                                             &
            &'  print *, "hello from project '//bname//'"',  &
            &'end program main']
        endif
        call warnwrite(join_path(settings%name, 'app/main.f90'), littlefile)
    endif

    if(settings%with_bare)then
    elseif(settings%with_test)then

       ! create NAME/test or stop
       call mkdir(join_path(settings%name, 'test'))
        ! create next section of fpm.toml
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile ,&
           &'[[test]]                                                                        ',&
           &'                                                                                ',&
           &'  # The same declarations can be made for test programs, which are              ',&
           &'  # executed with the "fpm test" command and are not build when your            ',&
           &'  # package is used as a dependency by other packages. These are                ',&
           &'  # typically unit tests of the package only used during package                ',&
           &'  # development.                                                                ',&
           &'                                                                                ',&
           &'name="runTests"                                                                 ',&
           &'source-dir="test"                                                               ',&
           &'main="check.f90"                                                                ',&
           &'                                                                                ',&
           &'  # you may repeat this pattern to add additional explicit test program         ',&
           &'  # parameters. The following example contains a sample of all accepted         ',&
           &'  # options.                                                                    ',&
           &'                                                                                ',&
           &'#[[ test ]]                                                                     ',&
           &'#name = "tester"                                                                ',&
           &'#source-dir="test"                                                              ',&
           &'#main="tester.f90"                                                              ',&
           &'#link = ["blas", "lapack"]                                                      ',&
           &'#[test.dependencies]                                                            ',&
           &'#M_CLI2  = { git = "https://github.com/urbanjost/M_CLI2.git" }                  ',&
           &'#M_io    = { git = "https://github.com/urbanjost/M_io.git" }                    ',&
           &'#M_system= { git = "https://github.com/urbanjost/M_system.git" }                ',&
           &'']
        endif

        littlefile=[character(len=80) ::       &
        &'program check',                      &
        &'implicit none',                      &
        &'',                                   &
        &'print *, "Put some tests in here!"', &
        &'end program check']
        ! create NAME/test/check.f90
        call warnwrite(join_path(settings%name, 'test/check.f90'), littlefile)
    endif

    if(settings%with_bare)then
    elseif(settings%with_example)then

       ! create NAME/example or stop
       call mkdir(join_path(settings%name, 'example'))
        ! create next section of fpm.toml
        if(settings%with_full)then
           tomlfile=[character(len=80) ::  tomlfile, &
           &'[[example]]                                                                     ',&
           &'                                                                                ',&
           &'  # Example applications for a project are defined here.                        ',&
           &'  # These are run via "fpm run --example NAME" and like the                     ',&
           &'  # test applications, are not built when this package is used as a             ',&
           &'  # dependency by other packages.                                               ',&
           &'                                                                                ',&
           &'name="demo"                                                                     ',&
           &'source-dir="example"                                                            ',&
           &'main="demo.f90"                                                                 ',&
           &'                                                                                ',&
           &'  #                                                                             ',&
           &'  # you may add additional programs to the example table. The following         ',&
           &'  # example contains a sample of all accepted options                           ',&
           &'                                                                                ',&
           &'#[[ example ]]                                                                  ',&
           &'#name = "example-tool"                                                          ',&
           &'#source-dir="example"                                                           ',&
           &'#main="tool.f90"                                                                ',&
           &'#link = "z"                                                                     ',&
           &'#[example.dependencies]                                                         ',&
           &'#M_kracken95  = { git = "https://github.com/urbanjost/M_kracken95.git" }        ',&
           &'#datetime = {git = "https://github.com/wavebitscientific/datetime-fortran.git" }',&
           &'']
        endif

        littlefile=[character(len=80) ::          &
        &'program demo',                          &
        &'implicit none',                         &
        &'',                                      &
        &'print *, "Put some examples in here!"', &
        &'end program demo']
        ! create NAME/example/demo.f90
        call warnwrite(join_path(settings%name, 'example/demo.f90'), littlefile)
    endif

    ! now that built it write NAME/fpm.toml
    if( allocated(tomlfile) )then
        call validate_toml_data(tomlfile)
        call warnwrite(join_path(settings%name, 'fpm.toml'), tomlfile)
    else
        call create_verified_basic_manifest(join_path(settings%name, 'fpm.toml'))
    endif
    ! assumes git(1) is installed and in path
    if(which('git')/='')then
      call run('git init ' // settings%name)
    endif
contains

function git_metadata(what) result(returned)
!> get metadata values such as email address and git name from git(1) or return appropriate default
  use fpm_filesystem, only : get_temp_filename, getline
  character(len=*), intent(in)  :: what     ! keyword designating what git metatdata to query
  character(len=:), allocatable :: returned ! value to return for requested keyword
  character(len=:), allocatable :: command
  character(len=:), allocatable :: temp_filename
  character(len=:), allocatable :: iomsg
  character(len=:), allocatable :: temp_value
  integer :: stat, unit
  temp_filename = get_temp_filename()
  ! for known keywords set default value for RETURNED and associated git(1) command for query
  select case(what)
  case('uname')
    returned = "Jane Doe"
    command = "git config --get user.name > " // temp_filename
  case('email')
    returned = "jane.doe@example.com"
    command = "git config --get user.email > " // temp_filename
  case default
    write(stderr,'(*(g0,1x))')&
    & '<ERROR> *git_metadata* unknown metadata name ',trim(what)
    returned=''
    return
  end select
  ! Execute command if git(1) is in command path
  if(which('git')/='')then
     call run(command, exitstat=stat)
     if (stat /= 0) then ! If command failed just return default
        return
     else ! Command did not return an error so try to read expected output file
        open(file=temp_filename, newunit=unit,iostat=stat)
        if(stat == 0)then
           ! Read file into a scratch variable until status of doing so is checked
           call getline(unit, temp_value, stat, iomsg)
           if (stat == 0 .and. temp_value /= '') then
              ! Return output from successful command
              returned=temp_value
           endif
        endif
        ! Always do the CLOSE because a failed open has unpredictable results.
        ! Add IOSTAT so a failed close does not cause program to stop
        close(unit, status="delete",iostat=stat)
     endif
  endif
end function git_metadata

subroutine create_verified_basic_manifest(filename)
!> create a basic but verified default manifest file
use fpm_toml, only : toml_table, toml_serializer, set_value
use fpm_manifest_package, only : package_config_t, new_package
use fpm_error, only : error_t
implicit none
character(len=*),intent(in) :: filename
   type(toml_table)            :: table
   type(toml_serializer)       :: ser
   type(package_config_t)      :: package
   type(error_t), allocatable  :: error
   integer                     :: lun
   character(len=8)            :: date

    if(exists(filename))then
       write(stderr,'(*(g0,1x))')'<INFO>  ',filename,&
       & 'already exists. Not overwriting'
       return
    endif
    !> get date to put into metadata in manifest file "fpm.toml"
    call date_and_time(DATE=date)
    table = toml_table()
    ser = toml_serializer()
    call fileopen(filename,lun) ! fileopen stops on error

    call set_value(table, "name",       BNAME)
    call set_value(table, "version",    "0.1.0")
    call set_value(table, "license",    "license")
    call set_value(table, "author",     git_metadata('uname'))
    call set_value(table, "maintainer", git_metadata('email'))
    call set_value(table, "copyright",  'Copyright '//date(1:4)//', '//git_metadata('uname'))
    ! continue building of manifest
    ! ...
    call new_package(package, table, error=error)
    if (allocated(error)) call fpm_stop( 3,'')
    if(settings%verbose)then
       call table%accept(ser)
    endif
    ser%unit=lun
    call table%accept(ser)
    call fileclose(lun) ! fileopen stops on error

end subroutine create_verified_basic_manifest


subroutine validate_toml_data(input)
!> verify a string array is a valid fpm.toml file
!
use tomlf, only : toml_parse
use fpm_toml, only : toml_table, toml_serializer
implicit none
character(kind=tfc,len=:),intent(in),allocatable :: input(:)
character(len=1), parameter                      :: nl = new_line('a')
type(toml_table), allocatable                    :: table
character(kind=tfc, len=:), allocatable          :: joined_string
type(toml_serializer)                            :: ser

! you have to add a newline character by using the intrinsic
! function `new_line("a")` to get the lines processed correctly.
joined_string = join(input,right=nl)

if (allocated(table)) deallocate(table)
call toml_parse(table, joined_string)
if (allocated(table)) then
   if(settings%verbose)then
      ! If the TOML file is successfully parsed the table will be allocated and
      ! can be written to the standard output by passing the `toml_serializer`
      ! as visitor to the table.
      call table%accept(ser)
   endif
   call table%destroy
endif

end subroutine validate_toml_data

end subroutine cmd_new

end module fpm_cmd_new
 
 
!>>>>> ././src/fpm_compiler.f90
!># Define compiler command options
!!
!! This module defines compiler options to use for the debug and release builds.

! vendor            Fortran   C         Module output   Module include OpenMP    Free for OSS
!                   compiler  compiler  directory       directory
! Gnu               gfortran   gcc     -J              -I            -fopenmp   X
! Intel             ifort      icc     -module         -I            -qopenmp   X
! Intel(Windows)    ifort      icc     /module:path    /I            /Qopenmp   X
! Intel oneAPI      ifx        icx     -module         -I            -qopenmp   X
! PGI               pgfortran  pgcc    -module         -I            -mp        X
! NVIDIA            nvfortran  nvc     -module         -I            -mp        X
! LLVM flang        flang      clang   -module         -I            -mp        X
! LFortran          lfortran   ---     -J              -I            --openmp   X
! Lahey/Futjitsu    lfc        ?       -M              -I            -openmp    ?
! NAG               nagfor     ?       -mdir           -I            -openmp    x
! Cray              crayftn    craycc  -J              -I            -homp      ?
! IBM               xlf90      ?       -qmoddir        -I            -qsmp      X
! Oracle/Sun        ?          ?       -moddir=        -M            -xopenmp   ?
! Silverfrost FTN95 ftn95      ?       ?               /MOD_PATH     ?          ?
! Elbrus            ?          lcc     -J              -I            -fopenmp   ?
! Hewlett Packard   ?          ?       ?               ?             ?          discontinued
! Watcom            ?          ?       ?               ?             ?          discontinued
! PathScale         ?          ?       -module         -I            -mp        discontinued
! G95               ?          ?       -fmod=          -I            -fopenmp   discontinued
! Open64            ?          ?       -module         -I            -mp        discontinued
! Unisys            ?          ?       ?               ?             ?          discontinued
module fpm_compiler
use,intrinsic :: iso_fortran_env, only: stderr=>error_unit
use fpm_environment, only: &
        get_env, &
        get_os_type, &
        OS_LINUX, &
        OS_MACOS, &
        OS_WINDOWS, &
        OS_CYGWIN, &
        OS_SOLARIS, &
        OS_FREEBSD, &
        OS_OPENBSD, &
        OS_UNKNOWN
use fpm_filesystem, only: join_path, basename, get_temp_filename, delete_file, unix_path, &
    & getline, run
use fpm_strings, only: split, string_cat, string_t, str_ends_with, str_begins_with_str
use fpm_manifest, only : package_config_t
use fpm_error, only: error_t
implicit none
public :: compiler_t, new_compiler, archiver_t, new_archiver, get_macros
public :: debug

enum, bind(C)
    enumerator :: &
        id_unknown, &
        id_gcc, &
        id_f95, &
        id_caf, &
        id_intel_classic_nix, &
        id_intel_classic_mac, &
        id_intel_classic_windows, &
        id_intel_llvm_nix, &
        id_intel_llvm_windows, &
        id_intel_llvm_unknown, &
        id_pgi, &
        id_nvhpc, &
        id_nag, &
        id_flang, &
        id_flang_new, &
        id_f18, &
        id_ibmxl, &
        id_cray, &
        id_lahey, &
        id_lfortran
end enum
integer, parameter :: compiler_enum = kind(id_unknown)


!> Definition of compiler object
type :: compiler_t
    !> Identifier of the compiler
    integer(compiler_enum) :: id = id_unknown
    !> Path to the Fortran compiler
    character(len=:), allocatable :: fc
    !> Path to the C compiler
    character(len=:), allocatable :: cc
    !> Path to the C++ compiler
    character(len=:), allocatable :: cxx
    !> Print all commands
    logical :: echo = .true.
    !> Verbose output of command
    logical :: verbose = .true.
contains
    !> Get default compiler flags
    procedure :: get_default_flags
    !> Get flag for module output directories
    procedure :: get_module_flag
    !> Get flag for include directories
    procedure :: get_include_flag
    !> Compile a Fortran object
    procedure :: compile_fortran
    !> Compile a C object
    procedure :: compile_c
    !> Compile a CPP object
    procedure :: compile_cpp
    !> Link executable
    procedure :: link
    !> Check whether compiler is recognized
    procedure :: is_unknown
    !> Enumerate libraries, based on compiler and platform
    procedure :: enumerate_libraries
end type compiler_t


!> Definition of archiver object
type :: archiver_t
    !> Path to archiver
    character(len=:), allocatable :: ar
    !> Use response files to pass arguments
    logical :: use_response_file = .false.
    !> Print all command
    logical :: echo = .true.
    !> Verbose output of command
    logical :: verbose = .true.
contains
    !> Create static archive
    procedure :: make_archive
end type archiver_t


!> Create debug printout
interface debug
    module procedure :: debug_compiler
    module procedure :: debug_archiver
end interface debug

character(*), parameter :: &
    flag_gnu_coarray = " -fcoarray=single", &
    flag_gnu_backtrace = " -fbacktrace", &
    flag_gnu_opt = " -O3 -funroll-loops", &
    flag_gnu_debug = " -g", &
    flag_gnu_pic = " -fPIC", &
    flag_gnu_warn = " -Wall -Wextra -Wimplicit-interface", &
    flag_gnu_check = " -fcheck=bounds -fcheck=array-temps", &
    flag_gnu_limit = " -fmax-errors=1", &
    flag_gnu_external = " -Wimplicit-interface"

character(*), parameter :: &
    flag_pgi_backslash = " -Mbackslash", &
    flag_pgi_traceback = " -traceback", &
    flag_pgi_debug = " -g", &
    flag_pgi_check = " -Mbounds -Mchkptr -Mchkstk", &
    flag_pgi_warn = " -Minform=inform"

character(*), parameter :: &
    flag_ibmxl_backslash = " -qnoescape"

character(*), parameter :: &
    flag_intel_backtrace = " -traceback", &
    flag_intel_warn = " -warn all", &
    flag_intel_check = " -check all", &
    flag_intel_debug = " -O0 -g", &
    flag_intel_fp = " -fp-model precise -pc64", &
    flag_intel_align = " -align all", &
    flag_intel_limit = " -error-limit 1", &
    flag_intel_pthread = " -reentrancy threaded", &
    flag_intel_nogen = " -nogen-interfaces", &
    flag_intel_byterecl = " -assume byterecl"

character(*), parameter :: &
    flag_intel_backtrace_win = " /traceback", &
    flag_intel_warn_win = " /warn:all", &
    flag_intel_check_win = " /check:all", &
    flag_intel_debug_win = " /Od /Z7", &
    flag_intel_fp_win = " /fp:precise", &
    flag_intel_align_win = " /align:all", &
    flag_intel_limit_win = " /error-limit:1", &
    flag_intel_pthread_win = " /reentrancy:threaded", &
    flag_intel_nogen_win = " /nogen-interfaces", &
    flag_intel_byterecl_win = " /assume:byterecl"

character(*), parameter :: &
    flag_nag_coarray = " -coarray=single", &
    flag_nag_pic = " -PIC", &
    flag_nag_check = " -C", &
    flag_nag_debug = " -g -O0", &
    flag_nag_opt = " -O4", &
    flag_nag_backtrace = " -gline"

character(*), parameter :: &
    flag_lfortran_opt = " --fast"

    
contains


function get_default_flags(self, release) result(flags)
    class(compiler_t), intent(in) :: self
    logical, intent(in) :: release
    character(len=:), allocatable :: flags

    if (release) then
        call get_release_compile_flags(self%id, flags)
    else
        call get_debug_compile_flags(self%id, flags)
    end if

end function get_default_flags

subroutine get_release_compile_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(out) :: flags


    select case(id)
    case default
        flags = ""
    case(id_caf)
        flags = &
            flag_gnu_opt//&
            flag_gnu_external//&
            flag_gnu_pic//&
            flag_gnu_limit

    case(id_gcc)
        flags = &
            flag_gnu_opt//&
            flag_gnu_external//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_coarray

    case(id_f95)
        flags = &
            flag_gnu_opt//&
            flag_gnu_external//&
            flag_gnu_pic//&
            flag_gnu_limit

    case(id_nvhpc)
        flags = &
            flag_pgi_backslash

    case(id_ibmxl)
        flags = &
            flag_ibmxl_backslash

    case(id_intel_classic_nix)
        flags = &
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_classic_mac)
        flags = &
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_classic_windows)
        flags = &
            & flag_intel_fp_win//&
            flag_intel_align_win//&
            flag_intel_limit_win//&
            flag_intel_pthread_win//&
            flag_intel_nogen_win//&
            flag_intel_byterecl_win

    case(id_intel_llvm_nix)
        flags = &
            flag_intel_fp//&
            flag_intel_align//&
            flag_intel_limit//&
            flag_intel_pthread//&
            flag_intel_nogen//&
            flag_intel_byterecl

    case(id_intel_llvm_windows)
        flags = &
            flag_intel_fp_win//&
            flag_intel_align_win//&
            flag_intel_limit_win//&
            flag_intel_pthread_win//&
            flag_intel_nogen_win//&
            flag_intel_byterecl_win

    case(id_nag)
        flags = &
            flag_nag_opt//&
            flag_nag_coarray//&
            flag_nag_pic

    case(id_lfortran)
        flags = &
            flag_lfortran_opt

    end select
end subroutine get_release_compile_flags

subroutine get_debug_compile_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(out) :: flags

    select case(id)
    case default
        flags = ""
    case(id_caf)
        flags = &
            flag_gnu_warn//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_debug//&
            flag_gnu_check//&
            flag_gnu_backtrace
    case(id_gcc)
        flags = &
            flag_gnu_warn//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_debug//&
            flag_gnu_check//&
            flag_gnu_backtrace//&
            flag_gnu_coarray
    case(id_f95)
        flags = &
            flag_gnu_warn//&
            flag_gnu_pic//&
            flag_gnu_limit//&
            flag_gnu_debug//&
            flag_gnu_check//&
            ' -Wno-maybe-uninitialized -Wno-uninitialized'//&
            flag_gnu_backtrace
    case(id_nvhpc)
        flags = &
            flag_pgi_warn//&
            flag_pgi_backslash//&
            flag_pgi_check//&
            flag_pgi_traceback
    case(id_ibmxl)
        flags = &
            flag_ibmxl_backslash
    case(id_intel_classic_nix)
        flags = &
            flag_intel_warn//&
            flag_intel_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace
    case(id_intel_classic_mac)
        flags = &
            flag_intel_warn//&
            flag_intel_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace
    case(id_intel_classic_windows)
        flags = &
            flag_intel_warn_win//&
            flag_intel_check_win//&
            flag_intel_limit_win//&
            flag_intel_debug_win//&
            flag_intel_byterecl_win//&
            flag_intel_backtrace_win
    case(id_intel_llvm_nix)
        flags = &
            flag_intel_warn//&
            flag_intel_check//&
            flag_intel_limit//&
            flag_intel_debug//&
            flag_intel_byterecl//&
            flag_intel_backtrace
    case(id_intel_llvm_windows)
        flags = &
            flag_intel_warn_win//&
            flag_intel_check_win//&
            flag_intel_limit_win//&
            flag_intel_debug_win//&
            flag_intel_byterecl_win
    case(id_nag)
        flags = &
            flag_nag_debug//&
            flag_nag_check//&
            flag_nag_backtrace//&
            flag_nag_coarray//&
            flag_nag_pic

    case(id_lfortran)
        flags = ""
    end select
end subroutine get_debug_compile_flags

pure subroutine set_cpp_preprocessor_flags(id, flags)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(inout) :: flags
    character(len=:), allocatable :: flag_cpp_preprocessor

    !> Modify the flag_cpp_preprocessor on the basis of the compiler.
    select case(id)
    case default
        flag_cpp_preprocessor = ""
    case(id_caf, id_gcc, id_f95, id_nvhpc)
        flag_cpp_preprocessor = "-cpp"
    case(id_intel_classic_windows, id_intel_llvm_windows)
        flag_cpp_preprocessor = "/fpp"
    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_llvm_nix, id_nag)
        flag_cpp_preprocessor = "-fpp"
    case(id_lfortran)
        flag_cpp_preprocessor = "--cpp"
    end select

    flags = flag_cpp_preprocessor// flags

end subroutine set_cpp_preprocessor_flags

!> This function will parse and read the macros list and 
!> return them as defined flags.
function get_macros(id, macros_list, version) result(macros)
    integer(compiler_enum), intent(in) :: id
    character(len=:), allocatable, intent(in) :: version
    type(string_t), allocatable, intent(in) :: macros_list(:)

    character(len=:), allocatable :: macros
    character(len=:), allocatable :: macro_definition_symbol
    character(:), allocatable :: valued_macros(:)
    

    integer :: i

    if (.not.allocated(macros_list)) then
        macros = ""
        return
    end if

    !> Set macro defintion symbol on the basis of compiler used
    select case(id)
    case default
        macro_definition_symbol = "-D"
    case (id_intel_classic_windows, id_intel_llvm_windows)
        macro_definition_symbol = "/D"
    end select

    !> Check if macros are not allocated.
    if (.not.allocated(macros)) then
        macros=""
    end if

    do i = 1, size(macros_list)
        
        !> Split the macro name and value.
        call split(macros_list(i)%s, valued_macros, delimiters="=")
 
        if (size(valued_macros) > 1) then
            !> Check if the value of macro starts with '{' character.
            if (str_begins_with_str(trim(valued_macros(size(valued_macros))), "{")) then

                !> Check if the value of macro ends with '}' character.
                if (str_ends_with(trim(valued_macros(size(valued_macros))), "}")) then

                    !> Check if the string contains "version" as substring.
                    if (index(valued_macros(size(valued_macros)), "version") /= 0) then
                    
                        !> These conditions are placed in order to ensure proper spacing between the macros.
                        if (len(macros) == 0) then
                            macros = macros//macro_definition_symbol//trim(valued_macros(1))//'='//version
                        else 
                            macros = macros//' '//macro_definition_symbol//trim(valued_macros(1))//'='//version
                        end if
                        cycle
                    end if
                end if
            end if 
        end if
         
        !> These conditions are placed in order to ensure proper spacing between the macros.
        if (len(macros) == 0) then
            macros = ' '//macros//macro_definition_symbol//macros_list(i)%s
        else 
            macros = macros//' '//macro_definition_symbol//macros_list(i)%s
        end if

    end do

end function get_macros

function get_include_flag(self, path) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: flags

    select case(self%id)
    case default
        flags = "-I "//path

    case(id_caf, id_gcc, id_f95, id_cray, id_nvhpc, id_pgi, &
        & id_flang, id_flang_new, id_f18, &
        & id_intel_classic_nix, id_intel_classic_mac, &
        & id_intel_llvm_nix, id_lahey, id_nag, id_ibmxl, &
        & id_lfortran)
        flags = "-I "//path

    case(id_intel_classic_windows, id_intel_llvm_windows)
        flags = "/I"//path

    end select
end function get_include_flag

function get_module_flag(self, path) result(flags)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: path
    character(len=:), allocatable :: flags

    select case(self%id)
    case default
        flags = "-module "//path

    case(id_caf, id_gcc, id_f95, id_cray, id_lfortran)
        flags = "-J "//path

    case(id_nvhpc, id_pgi, id_flang)
        flags = "-module "//path

    case(id_flang_new, id_f18)
        flags = "-module-dir "//path

    case(id_intel_classic_nix, id_intel_classic_mac, &
        & id_intel_llvm_nix)
        flags = "-module "//path

    case(id_intel_classic_windows, id_intel_llvm_windows)
        flags = "/module:"//path

    case(id_lahey)
        flags = "-M "//path

    case(id_nag)
        flags = "-mdir "//path

    case(id_ibmxl)
        flags = "-qmoddir "//path

    end select

end function get_module_flag


subroutine get_default_c_compiler(f_compiler, c_compiler)
    character(len=*), intent(in) :: f_compiler
    character(len=:), allocatable, intent(out) :: c_compiler
    integer(compiler_enum) :: id

    id = get_compiler_id(f_compiler)

    select case(id)

    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
        c_compiler = 'icc'

    case(id_intel_llvm_nix,id_intel_llvm_windows)
        c_compiler = 'icx'

    case(id_flang, id_flang_new, id_f18)
        c_compiler='clang'

    case(id_ibmxl)
        c_compiler='xlc'

    case(id_lfortran)
        c_compiler = 'cc'

    case(id_gcc)
        c_compiler = 'gcc'

    case default
        ! Fall-back to using Fortran compiler
        c_compiler = f_compiler
    end select

end subroutine get_default_c_compiler

!> Get C++ Compiler.
subroutine get_default_cxx_compiler(f_compiler, cxx_compiler)
    character(len=*), intent(in) :: f_compiler
    character(len=:), allocatable, intent(out) :: cxx_compiler
    integer(compiler_enum) :: id

    id = get_compiler_id(f_compiler)

    select case(id)

    case(id_intel_classic_nix, id_intel_classic_mac, id_intel_classic_windows)
        cxx_compiler = 'icpc'

    case(id_intel_llvm_nix,id_intel_llvm_windows)
        cxx_compiler = 'icpx'

    case(id_flang, id_flang_new, id_f18)
        cxx_compiler='clang++'

    case(id_ibmxl)
        cxx_compiler='xlc++'

    case(id_lfortran)
        cxx_compiler = 'cc'

    case(id_gcc)
        cxx_compiler = 'g++'

    case default
        ! Fall-back to using Fortran compiler
        cxx_compiler = f_compiler
    end select

end subroutine get_default_cxx_compiler


function get_compiler_id(compiler) result(id)
    character(len=*), intent(in) :: compiler
    integer(kind=compiler_enum) :: id

    character(len=:), allocatable :: full_command, full_command_parts(:), command, output
    integer :: stat, io

    ! Check whether we are dealing with an MPI compiler wrapper first
    if (check_compiler(compiler, "mpifort") &
        & .or. check_compiler(compiler, "mpif90") &
        & .or. check_compiler(compiler, "mpif77")) then
        output = get_temp_filename()
        call run(compiler//" -show > "//output//" 2>&1", &
            & echo=.false., exitstat=stat)
        if (stat == 0) then
            open(file=output, newunit=io, iostat=stat)
            if (stat == 0) call getline(io, full_command, stat)
            close(io, iostat=stat)

            ! If we get a command from the wrapper, we will try to identify it
            call split(full_command, full_command_parts, delimiters=' ')
            if(size(full_command_parts) > 0)then
               command = trim(full_command_parts(1))
            endif
            if (allocated(command)) then
                id = get_id(command)
                if (id /= id_unknown) return
            end if
        end if
    end if

    id = get_id(compiler)

end function get_compiler_id

function get_id(compiler) result(id)
    character(len=*), intent(in) :: compiler
    integer(kind=compiler_enum) :: id

    integer :: stat

    if (check_compiler(compiler, "gfortran")) then
        id = id_gcc
        return
    end if

    if (check_compiler(compiler, "f95")) then
        id = id_f95
        return
    end if

    if (check_compiler(compiler, "caf")) then
        id = id_caf
        return
    end if

    if (check_compiler(compiler, "ifort")) then
        select case (get_os_type())
        case default
            id = id_intel_classic_nix
        case (OS_MACOS)
            id = id_intel_classic_mac
        case (OS_WINDOWS, OS_CYGWIN)
            id = id_intel_classic_windows
        end select
        return
    end if

    if (check_compiler(compiler, "ifx")) then
        select case (get_os_type())
        case default
            id = id_intel_llvm_nix
        case (OS_WINDOWS, OS_CYGWIN)
            id = id_intel_llvm_windows
        end select
        return
    end if

    if (check_compiler(compiler, "nvfortran")) then
        id = id_nvhpc
        return
    end if

    if (check_compiler(compiler, "pgfortran") &
        & .or. check_compiler(compiler, "pgf90") &
        & .or. check_compiler(compiler, "pgf95")) then
        id = id_pgi
        return
    end if

    if (check_compiler(compiler, "nagfor")) then
        id = id_nag
        return
    end if

    if (check_compiler(compiler, "flang-new")) then
        id = id_flang_new
        return
    end if

    if (check_compiler(compiler, "f18")) then
        id = id_f18
        return
    end if

    if (check_compiler(compiler, "flang")) then
        id = id_flang
        return
    end if

    if (check_compiler(compiler, "xlf90")) then
        id = id_ibmxl
        return
    end if

    if (check_compiler(compiler, "crayftn")) then
        id = id_cray
        return
    end if

    if (check_compiler(compiler, "lfc")) then
        id = id_lahey
        return
    end if

    if (check_compiler(compiler, "lfortran")) then
        id = id_lfortran
        return
    end if

    id = id_unknown

end function get_id

function check_compiler(compiler, expected) result(match)
    character(len=*), intent(in) :: compiler
    character(len=*), intent(in) :: expected
    logical :: match
    match = compiler == expected
    if (.not. match) then
        match = index(basename(compiler), expected) > 0
    end if
end function check_compiler


pure function is_unknown(self)
    class(compiler_t), intent(in) :: self
    logical :: is_unknown
    is_unknown = self%id == id_unknown
end function is_unknown

!>
!> Enumerate libraries, based on compiler and platform
!>
function enumerate_libraries(self, prefix, libs) result(r)
    class(compiler_t), intent(in) :: self
    character(len=*), intent(in) :: prefix
    type(string_t), intent(in) :: libs(:)
    character(len=:), allocatable :: r

    if (self%id == id_intel_classic_windows .or. &
        self%id == id_intel_llvm_windows) then
        r = prefix // " " // string_cat(libs,".lib ")//".lib"
    else
        r = prefix // " -l" // string_cat(libs," -l")
    end if
end function enumerate_libraries


!> Create new compiler instance
subroutine new_compiler(self, fc, cc, cxx, echo, verbose)
    !> New instance of the compiler
    type(compiler_t), intent(out) :: self
    !> Fortran compiler name or path
    character(len=*), intent(in) :: fc
    !> C compiler name or path
    character(len=*), intent(in) :: cc
    !> C++ Compiler name or path
    character(len=*), intent(in) :: cxx
    !> Echo compiler command
    logical, intent(in) :: echo
    !> Verbose mode: dump compiler output
    logical, intent(in) :: verbose

    self%id = get_compiler_id(fc)
    
    self%echo = echo
    self%verbose = verbose
    self%fc = fc
    if (len_trim(cc) > 0) then
      self%cc = cc
    else
      call get_default_c_compiler(self%fc, self%cc)
    end if

    if (len_trim(cxx) > 0) then
      self%cxx = cxx
    else
      call get_default_cxx_compiler(self%fc, self%cxx)
    end if
end subroutine new_compiler


!> Create new archiver instance
subroutine new_archiver(self, ar, echo, verbose)
    !> New instance of the archiver
    type(archiver_t), intent(out) :: self
    !> User provided archiver command
    character(len=*), intent(in) :: ar
    !> Echo compiler command
    logical, intent(in) :: echo
    !> Verbose mode: dump compiler output
    logical, intent(in) :: verbose

    integer :: estat, os_type

    character(len=*), parameter :: arflags = " -rs ", libflags = " /OUT:"

    if (len_trim(ar) > 0) then
      ! Check first for ar-like commands
      if (check_compiler(ar, "ar")) then
        self%ar = ar//arflags
      end if

      ! Check for lib-like commands
      if (check_compiler(ar, "lib")) then
        self%ar = ar//libflags
      end if

      ! Fallback and assume ar-like behaviour
      self%ar = ar//arflags
    else
      os_type = get_os_type()
      if (os_type /= OS_WINDOWS .and. os_type /= OS_UNKNOWN) then
        self%ar = "ar"//arflags
      else
        call execute_command_line("ar --version > "//get_temp_filename()//" 2>&1", &
          & exitstat=estat)
        if (estat /= 0) then
          self%ar = "lib"//libflags
        else
          self%ar = "ar"//arflags
        end if
      end if
    end if
    self%use_response_file = os_type == OS_WINDOWS
    self%echo = echo
    self%verbose = verbose
end subroutine new_archiver


!> Compile a Fortran object
subroutine compile_fortran(self, input, output, args, log_file, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat

    call run(self%fc // " -c " // input // " " // args // " -o " // output, &
        & echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
end subroutine compile_fortran


!> Compile a C object
subroutine compile_c(self, input, output, args, log_file, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat

    call run(self%cc // " -c " // input // " " // args // " -o " // output, &
        & echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
end subroutine compile_c

!> Compile a CPP object
subroutine compile_cpp(self, input, output, args, log_file, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Source file input
    character(len=*), intent(in) :: input
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat

    call run(self%cxx // " -c " // input // " " // args // " -o " // output, &
        & echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
end subroutine compile_cpp

!> Link an executable
subroutine link(self, output, args, log_file, stat)
    !> Instance of the compiler object
    class(compiler_t), intent(in) :: self
    !> Output file of object
    character(len=*), intent(in) :: output
    !> Arguments for compiler
    character(len=*), intent(in) :: args
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat

    call run(self%fc // " " // args // " -o " // output, echo=self%echo, &
        & verbose=self%verbose, redirect=log_file, exitstat=stat)
end subroutine link


!> Create an archive
!> @todo An OMP critical section is added for Windows OS,
!> which may be related to a bug in Mingw64-openmp and is expected to be resolved in the future,
!> see issue #707 and #708.
subroutine make_archive(self, output, args, log_file, stat)
    !> Instance of the archiver object
    class(archiver_t), intent(in) :: self
    !> Name of the archive to generate
    character(len=*), intent(in) :: output
    !> Object files to include into the archive
    type(string_t), intent(in) :: args(:)
    !> Compiler output log file
    character(len=*), intent(in) :: log_file
    !> Status flag
    integer, intent(out) :: stat

    if (self%use_response_file) then
        !$omp critical
        call write_response_file(output//".resp" , args)
        call run(self%ar // output // " @" // output//".resp", echo=self%echo, &
            &  verbose=self%verbose, redirect=log_file, exitstat=stat)
        call delete_file(output//".resp")
        !$omp end critical
    else
        call run(self%ar // output // " " // string_cat(args, " "), &
            & echo=self%echo, verbose=self%verbose, redirect=log_file, exitstat=stat)
    end if
end subroutine make_archive


!> Response files allow to read command line options from files.
!> Whitespace is used to separate the arguments, we will use newlines
!> as separator to create readable response files which can be inspected
!> in case of errors.
subroutine write_response_file(name, argv)
    character(len=*), intent(in) :: name
    type(string_t), intent(in) :: argv(:)

    integer :: iarg, io

    open(file=name, newunit=io)
    do iarg = 1, size(argv)
        write(io, '(a)') unix_path(argv(iarg)%s)
    end do
    close(io)
end subroutine write_response_file


!> String representation of a compiler object
pure function debug_compiler(self) result(repr)
    !> Instance of the compiler object
    type(compiler_t), intent(in) :: self
    !> Representation as string
    character(len=:), allocatable :: repr

    repr = 'fc="'//self%fc//'", cc="'//self%cc//'"'
end function debug_compiler


!> String representation of an archiver object
pure function debug_archiver(self) result(repr)
    !> Instance of the archiver object
    type(archiver_t), intent(in) :: self
    !> Representation as string
    character(len=:), allocatable :: repr

    repr = 'ar="'//self%ar//'"'
end function debug_archiver


end module fpm_compiler
 
 
!>>>>> ././src/fpm/dependency.f90
!> # Dependency management
!>
!> ## Fetching dependencies and creating a dependency tree
!>
!> Dependencies on the top-level can be specified from:
!>
!> - `package%dependencies`
!> - `package%dev_dependencies`
!> - `package%executable(:)%dependencies`
!> - `package%test(:)%dependencies`
!>
!> Each dependency is fetched in some way and provides a path to its package
!> manifest.
!> The `package%dependencies` of the dependencies are resolved recursively.
!>
!> To initialize the dependency tree all dependencies are recursively fetched
!> and stored in a flat data structure to avoid retrieving a package twice.
!> The data structure used to store this information should describe the current
!> status of the dependency tree. Important information are:
!>
!> - name of the package
!> - version of the package
!> - path to the package root
!>
!> Additionally, for version controlled dependencies the following should be
!> stored along with the package:
!>
!> - the upstream url
!> - the current checked out revision
!>
!> Fetching a remote (version controlled) dependency turns it for our purpose
!> into a local path dependency which is handled by the same means.
!>
!> ## Updating dependencies
!>
!> For a given dependency tree all top-level dependencies can be updated.
!> We have two cases to consider, a remote dependency and a local dependency,
!> again, remote dependencies turn into local dependencies by fetching.
!> Therefore we will update remote dependencies by simply refetching them.
!>
!> For remote dependencies we have to refetch if the revision in the manifest
!> changes or the upstream HEAD has changed (for branches _and_ tags).
!>
!> @Note For our purpose a tag is just a fancy branch name. Tags can be delete and
!>       modified afterwards, therefore they do not differ too much from branches
!>       from our perspective.
!>
!> For the latter case we only know if we actually fetch from the upstream URL.
!>
!> In case of local (and fetched remote) dependencies we have to read the package
!> manifest and compare its dependencies against our dependency tree, any change
!> requires updating the respective dependencies as well.
!>
!> ## Handling dependency compatibilties
!>
!> Currenly ignored. First come, first serve.
module fpm_dependency
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm_environment, only : get_os_type, OS_WINDOWS
  use fpm_error, only : error_t, fatal_error
  use fpm_filesystem, only : exists, join_path, mkdir, canon_path, windows_path
  use fpm_git, only : git_target_revision, git_target_default, git_revision
  use fpm_manifest, only : package_config_t, dependency_config_t, &
    get_package_data
  use fpm_strings, only : string_t, operator(.in.)
  use fpm_toml, only : toml_table, toml_key, toml_error, toml_serializer, &
    toml_parse, get_value, set_value, add_table
  use fpm_versioning, only : version_t, new_version, char
  implicit none
  private

  public :: dependency_tree_t, new_dependency_tree
  public :: dependency_node_t, new_dependency_node
  public :: resize


  !> Overloaded reallocation interface
  interface resize
    module procedure :: resize_dependency_node
  end interface resize


  !> Dependency node in the projects dependency tree
  type, extends(dependency_config_t) :: dependency_node_t
    !> Actual version of this dependency
    type(version_t), allocatable :: version
    !> Installation prefix of this dependencies
    character(len=:), allocatable :: proj_dir
    !> Checked out revision of the version control system
    character(len=:), allocatable :: revision
    !> Dependency is handled
    logical :: done = .false.
    !> Dependency should be updated
    logical :: update = .false.
  contains
    !> Update dependency from project manifest
    procedure :: register
  end type dependency_node_t


  !> Respresentation of a projects dependencies
  !>
  !> The dependencies are stored in a simple array for now, this can be replaced
  !> with a binary-search tree or a hash table in the future.
  type :: dependency_tree_t
    !> Unit for IO
    integer :: unit = output_unit
    !> Verbosity of printout
    integer :: verbosity = 1
    !> Installation prefix for dependencies
    character(len=:), allocatable :: dep_dir
    !> Number of currently registered dependencies
    integer :: ndep = 0
    !> Flattend list of all dependencies
    type(dependency_node_t), allocatable :: dep(:)
    !> Cache file
    character(len=:), allocatable :: cache
  contains
    !> Overload procedure to add new dependencies to the tree
    generic :: add => add_project, add_project_dependencies, add_dependencies, &
      add_dependency
    !> Main entry point to add a project
    procedure, private :: add_project
    !> Add a project and its dependencies to the dependency tree
    procedure, private :: add_project_dependencies
    !> Add a list of dependencies to the dependency tree
    procedure, private :: add_dependencies
    !> Add a single dependency to the dependency tree
    procedure, private :: add_dependency
    !> Resolve dependencies
    generic :: resolve => resolve_dependencies, resolve_dependency
    !> Resolve dependencies
    procedure, private :: resolve_dependencies
    !> Resolve dependencies
    procedure, private :: resolve_dependency
    !> Find a dependency in the tree
    generic :: find => find_dependency, find_name
    !> Find a dependency from an dependency configuration
    procedure, private :: find_dependency
    !> Find a dependency by its name
    procedure, private :: find_name
    !> Depedendncy resolution finished
    procedure :: finished
    !> Reading of dependency tree
    generic :: load => load_from_file, load_from_unit, load_from_toml
    !> Read dependency tree from file
    procedure, private :: load_from_file
    !> Read dependency tree from formatted unit
    procedure, private :: load_from_unit
    !> Read dependency tree from TOML data structure
    procedure, private :: load_from_toml
    !> Writing of dependency tree
    generic :: dump => dump_to_file, dump_to_unit, dump_to_toml
    !> Write dependency tree to file
    procedure, private :: dump_to_file
    !> Write dependency tree to formatted unit
    procedure, private :: dump_to_unit
    !> Write dependency tree to TOML data structure
    procedure, private :: dump_to_toml
    !> Update dependency tree
    generic :: update => update_dependency
    !> Update a list of dependencies
    procedure, private :: update_dependency
  end type dependency_tree_t

  !> Common output format for writing to the command line
  character(len=*), parameter :: out_fmt = '("#", *(1x, g0))'

contains

  !> Create a new dependency tree
  subroutine new_dependency_tree(self, verbosity, cache)
    !> Instance of the dependency tree
    type(dependency_tree_t), intent(out) :: self
    !> Verbosity of printout
    integer, intent(in), optional :: verbosity
    !> Name of the cache file
    character(len=*), intent(in), optional :: cache

    call resize(self%dep)
    self%dep_dir = join_path("build", "dependencies")

    if (present(verbosity)) then
      self%verbosity = verbosity
    end if

    if (present(cache)) then
      self%cache = cache
    end if

  end subroutine new_dependency_tree

  !> Create a new dependency node from a configuration
  pure subroutine new_dependency_node(self, dependency, version, proj_dir, update)
    !> Instance of the dependency node
    type(dependency_node_t), intent(out) :: self
    !> Dependency configuration data
    type(dependency_config_t), intent(in) :: dependency
    !> Version of the dependency
    type(version_t), intent(in), optional :: version
    !> Installation prefix of the dependency
    character(len=*), intent(in), optional :: proj_dir
    !> Dependency should be updated
    logical, intent(in), optional :: update

    self%dependency_config_t = dependency

    if (present(version)) then
      self%version = version
    end if

    if (present(proj_dir)) then
      self%proj_dir = proj_dir
    end if

    if (present(update)) then
      self%update = update
    end if

  end subroutine new_dependency_node

  !> Add project dependencies, each depth level after each other.
  !>
  !> We implement this algorithm in an interative rather than a recursive fashion
  !> as a choice of design.
  subroutine add_project(self, package, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Project configuration to add
    type(package_config_t), intent(in) :: package
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(dependency_config_t) :: dependency
    character(len=:), allocatable :: root

    if (allocated(self%cache)) then
      call self%load(self%cache, error)
      if (allocated(error)) return
    end if

    if (.not.exists(self%dep_dir)) then
      call mkdir(self%dep_dir)
    end if

    root = "."

    ! Create this project as the first dependency node (depth 0)
    dependency%name = package%name
    dependency%path = root
    call self%add(dependency, error)
    if (allocated(error)) return

    ! Resolve the root project
    call self%resolve(root, error)
    if (allocated(error)) return

    ! Add the root project dependencies (depth 1)
    call self%add(package, root, .true., error)
    if (allocated(error)) return

    ! Now decent into the dependency tree, level for level
    do while(.not.self%finished())
       call self%resolve(root, error)
       if (allocated(error)) exit
    end do
    if (allocated(error)) return

    if (allocated(self%cache)) then
      call self%dump(self%cache, error)
      if (allocated(error)) return
    end if

  end subroutine add_project

  !> Add a project and its dependencies to the dependency tree
  recursive subroutine add_project_dependencies(self, package, root, main, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Project configuration to add
    type(package_config_t), intent(in) :: package
    !> Current project root directory
    character(len=*), intent(in) :: root
    !> Is the main project
    logical, intent(in) :: main
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii

    if (allocated(package%dependency)) then
      call self%add(package%dependency, error)
      if (allocated(error)) return
    end if

    if (main) then
      if (allocated(package%dev_dependency)) then
        call self%add(package%dev_dependency, error)
        if (allocated(error)) return
      end if

      if (allocated(package%executable)) then
        do ii = 1, size(package%executable)
          if (allocated(package%executable(ii)%dependency)) then
            call self%add(package%executable(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if

      if (allocated(package%example)) then
        do ii = 1, size(package%example)
          if (allocated(package%example(ii)%dependency)) then
            call self%add(package%example(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if

      if (allocated(package%test)) then
        do ii = 1, size(package%test)
          if (allocated(package%test(ii)%dependency)) then
            call self%add(package%test(ii)%dependency, error)
            if (allocated(error)) exit
          end if
        end do
        if (allocated(error)) return
      end if
    end if

  end subroutine add_project_dependencies

  !> Add a list of dependencies to the dependency tree
  subroutine add_dependencies(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency(:)
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii, ndep

    ndep = size(self%dep)
    if (ndep < size(dependency) + self%ndep) then
      call resize(self%dep, ndep + ndep/2 + size(dependency))
    end if

    do ii = 1, size(dependency)
      call self%add(dependency(ii), error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

  end subroutine add_dependencies

  !> Add a single dependency to the dependency tree
  pure subroutine add_dependency(self, dependency, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_config_t), intent(in) :: dependency
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id

    id = self%find(dependency)
    if (id == 0) then
      self%ndep = self%ndep + 1
      call new_dependency_node(self%dep(self%ndep), dependency)
    end if

  end subroutine add_dependency

  !> Update dependency tree
  subroutine update_dependency(self, name, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Name of the dependency to update
    character(len=*), intent(in) :: name
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: id
    character(len=:), allocatable :: proj_dir, root

    id = self%find(name)
    root = "."

    if (id <= 0) then
      call fatal_error(error, "Cannot update dependency '"//name//"'")
      return
    end if

    associate(dep => self%dep(id))
      if (allocated(dep%git) .and. dep%update) then
        if (self%verbosity > 1) then
          write(self%unit, out_fmt) "Update:", dep%name
        end if
        proj_dir = join_path(self%dep_dir, dep%name)
        call dep%git%checkout(proj_dir, error)
        if (allocated(error)) return

        ! Unset dependency and remove updatable attribute
        dep%done = .false.
        dep%update = .false.

        ! Now decent into the dependency tree, level for level
        do while(.not.self%finished())
          call self%resolve(root, error)
          if (allocated(error)) exit
        end do
        if (allocated(error)) return
      end if
    end associate

  end subroutine update_dependency

  !> Resolve all dependencies in the tree
  subroutine resolve_dependencies(self, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii

    do ii = 1, self%ndep
      call self%resolve(self%dep(ii), root, error)
      if (allocated(error)) exit
    end do

    if (allocated(error)) return

  end subroutine resolve_dependencies

  !> Resolve a single dependency node
  subroutine resolve_dependency(self, dependency, root, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Dependency configuration to add
    type(dependency_node_t), intent(inout) :: dependency
    !> Current installation prefix
    character(len=*), intent(in) :: root
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(package_config_t) :: package
    character(len=:), allocatable :: manifest, proj_dir, revision
    logical :: fetch

    if (dependency%done) return

    fetch = .false.
    if (allocated(dependency%proj_dir)) then
      proj_dir = dependency%proj_dir
    else
      if (allocated(dependency%path)) then
        proj_dir = join_path(root, dependency%path)
      else if (allocated(dependency%git)) then
        proj_dir = join_path(self%dep_dir, dependency%name)
        fetch = .not.exists(proj_dir)
        if (fetch) then
          call dependency%git%checkout(proj_dir, error)
          if (allocated(error)) return
        end if

      end if
    end if

    if (allocated(dependency%git)) then
      call git_revision(proj_dir, revision, error)
      if (allocated(error)) return
    end if

    manifest = join_path(proj_dir, "fpm.toml")
    call get_package_data(package, manifest, error)
    if (allocated(error)) return

    call dependency%register(package, proj_dir, fetch, revision, error)
    if (allocated(error)) return

    if (self%verbosity > 1) then
      write(self%unit, out_fmt) &
        "Dep:", dependency%name, "version", char(dependency%version), &
        "at", dependency%proj_dir
    end if

    call self%add(package, proj_dir, .false., error)
    if (allocated(error)) return

  end subroutine resolve_dependency

  !> Find a dependency in the dependency tree
  pure function find_dependency(self, dependency) result(pos)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to add
    class(dependency_config_t), intent(in) :: dependency
    !> Index of the dependency
    integer :: pos

    pos = self%find(dependency%name)

  end function find_dependency

  !> Find a dependency in the dependency tree
  pure function find_name(self, name) result(pos)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> Dependency configuration to add
    character(len=*), intent(in) :: name
    !> Index of the dependency
    integer :: pos

    integer :: ii

    pos = 0
    do ii = 1, self%ndep
      if (name == self%dep(ii)%name) then
        pos = ii
        exit
      end if
    end do

  end function find_name

  !> Check if we are done with the dependency resolution
  pure function finished(self)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(in) :: self
    !> All dependencies are updated
    logical :: finished

    finished = all(self%dep(:self%ndep)%done)

  end function finished

  !> Update dependency from project manifest
  subroutine register(self, package, root, fetch, revision, error)
    !> Instance of the dependency node
    class(dependency_node_t), intent(inout) :: self
    !> Package configuration data
    type(package_config_t), intent(in) :: package
    !> Project has been fetched
    logical, intent(in) :: fetch
    !> Root directory of the project
    character(len=*), intent(in) :: root
    !> Git revision of the project
    character(len=*), intent(in), optional :: revision
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    logical :: update

    update = .false.
    if (self%name /= package%name) then
      call fatal_error(error, "Dependency name '"//package%name// &
        & "' found, but expected '"//self%name//"' instead")
    end if

    self%version = package%version
    self%proj_dir = root

    if (allocated(self%git).and.present(revision)) then
      self%revision = revision
      if (.not.fetch) then
        ! git object is HEAD always allows an update
        update = .not.allocated(self%git%object)
        if (.not.update) then
          ! allow update in case the revision does not match the requested object
          update = revision /= self%git%object
        end if
      end if
    end if

    self%update = update
    self%done = .true.

  end subroutine register

  !> Read dependency tree from file
  subroutine load_from_file(self, file, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit
    logical :: exist

    inquire(file=file, exist=exist)
    if (.not.exist) return

    open(file=file, newunit=unit)
    call self%load(unit, error)
    close(unit)
  end subroutine load_from_file

  !> Read dependency tree from file
  subroutine load_from_unit(self, unit, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    integer, intent(in) :: unit
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_error), allocatable :: parse_error
    type(toml_table), allocatable :: table

    call toml_parse(table, unit, parse_error)

    if (allocated(parse_error)) then
      allocate(error)
      call move_alloc(parse_error%message, error%message)
      return
    end if

    call self%load(table, error)
    if (allocated(error)) return

  end subroutine load_from_unit

  !> Read dependency tree from TOML data structure
  subroutine load_from_toml(self, table, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ndep, ii
    logical :: unix
    character(len=:), allocatable :: version, url, obj, rev, proj_dir
    type(toml_key), allocatable :: list(:)
    type(toml_table), pointer :: ptr

    call table%get_keys(list)

    ndep = size(self%dep)
    if (ndep < size(list) + self%ndep) then
      call resize(self%dep, ndep + ndep/2 + size(list))
    end if

    unix = get_os_type() /= OS_WINDOWS

    do ii = 1, size(list)
      call get_value(table, list(ii)%key, ptr)
      call get_value(ptr, "version", version)
      call get_value(ptr, "proj-dir", proj_dir)
      call get_value(ptr, "git", url)
      call get_value(ptr, "obj", obj)
      call get_value(ptr, "rev", rev)
      if (.not.allocated(proj_dir)) cycle
      self%ndep = self%ndep + 1
      associate(dep => self%dep(self%ndep))
        dep%name = list(ii)%key
        if (unix) then
          dep%proj_dir = proj_dir
        else
          dep%proj_dir = windows_path(proj_dir)
        end if
        dep%done = .false.
        if (allocated(version)) then
          if (.not.allocated(dep%version)) allocate(dep%version)
          call new_version(dep%version, version, error)
          if (allocated(error)) exit
        end if
        if (allocated(version)) then
          call new_version(dep%version, version, error)
          if (allocated(error)) exit
        end if
        if (allocated(url)) then
          if (allocated(obj)) then
            dep%git = git_target_revision(url, obj)
          else
            dep%git = git_target_default(url)
          end if
          if (allocated(rev)) then
            dep%revision = rev
          end if
        else
          dep%path = proj_dir
        end if
      end associate
    end do
    if (allocated(error)) return

    self%ndep = size(list)
  end subroutine load_from_toml

  !> Write dependency tree to file
  subroutine dump_to_file(self, file, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> File name
    character(len=*), intent(in) :: file
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: unit

    open(file=file, newunit=unit)
    call self%dump(unit, error)
    close(unit)
    if (allocated(error)) return

  end subroutine dump_to_file

  !> Write dependency tree to file
  subroutine dump_to_unit(self, unit, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Formatted unit
    integer, intent(in) :: unit
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    type(toml_table) :: table
    type(toml_serializer) :: ser

    table = toml_table()
    ser = toml_serializer(unit)

    call self%dump(table, error)

    call table%accept(ser)

  end subroutine dump_to_unit

  !> Write dependency tree to TOML datastructure
  subroutine dump_to_toml(self, table, error)
    !> Instance of the dependency tree
    class(dependency_tree_t), intent(inout) :: self
    !> Data structure
    type(toml_table), intent(inout) :: table
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: ii
    type(toml_table), pointer :: ptr
    character(len=:), allocatable :: proj_dir

    do ii = 1, self%ndep
      associate(dep => self%dep(ii))
        call add_table(table, dep%name, ptr)
        if (.not.associated(ptr)) then
          call fatal_error(error, "Cannot create entry for "//dep%name)
          exit
        end if
        if (allocated(dep%version)) then
          call set_value(ptr, "version", char(dep%version))
        end if
        proj_dir = canon_path(dep%proj_dir)
        call set_value(ptr, "proj-dir", proj_dir)
        if (allocated(dep%git)) then
          call set_value(ptr, "git", dep%git%url)
          if (allocated(dep%git%object)) then
            call set_value(ptr, "obj", dep%git%object)
          end if
          if (allocated(dep%revision)) then
            call set_value(ptr, "rev", dep%revision)
          end if
        end if
      end associate
    end do
    if (allocated(error)) return

  end subroutine dump_to_toml

  !> Reallocate a list of dependencies
  pure subroutine resize_dependency_node(var, n)
    !> Instance of the array to be resized
    type(dependency_node_t), allocatable, intent(inout) :: var(:)
    !> Dimension of the final array size
    integer, intent(in), optional :: n

    type(dependency_node_t), allocatable :: tmp(:)
    integer :: this_size, new_size
    integer, parameter :: initial_size = 16

    if (allocated(var)) then
      this_size = size(var, 1)
      call move_alloc(var, tmp)
    else
      this_size = initial_size
    end if

    if (present(n)) then
      new_size = n
    else
      new_size = this_size + this_size/2 + 1
    end if

    allocate(var(new_size))

    if (allocated(tmp)) then
      this_size = min(size(tmp, 1), size(var, 1))
      var(:this_size) = tmp(:this_size)
      deallocate(tmp)
    end if

  end subroutine resize_dependency_node

end module fpm_dependency
 
 
!>>>>> ././src/fpm_model.f90
!># The fpm package model
!>
!> Defines the fpm model data types which encapsulate all information
!> required to correctly build a package and its dependencies.
!>
!> The process (see `[[build_model(subroutine)]]`) for generating a valid `[[fpm_model]]` involves
!>  source files discovery ([[fpm_sources]]) and parsing ([[fpm_source_parsing]]).
!>
!> Once a valid `[[fpm_model]]` has been constructed, it may be passed to `[[fpm_targets:targets_from_sources]]` to
!> generate a list of build targets for the backend.
!>
!>### Enumerations
!>
!> __Source type:__ `FPM_UNIT_*`
!> Describes the type of source file — determines build target generation
!>
!> The logical order of precedence for assigning `unit_type` is as follows:
!>
!>```
!> if source-file contains program then
!>   unit_type = FPM_UNIT_PROGRAM
!> else if source-file contains non-module subroutine/function then
!>   unit_type = FPM_UNIT_SUBPROGRAM
!> else if source-file contains submodule then
!>   unit_type = FPM_UNIT_SUBMODULE
!> else if source-file contains module then
!>   unit_type = FPM_UNIT_MODULE
!> end if
!>```
!>
!> @note A source file is only designated `FPM_UNIT_MODULE` if it **only** contains modules - no non-module subprograms.
!> (This allows tree-shaking/pruning of build targets based on unused module dependencies.)
!>
!> __Source scope:__ `FPM_SCOPE_*`
!> Describes the scoping rules for using modules — controls module dependency resolution
!>
module fpm_model
use iso_fortran_env, only: int64
use fpm_compiler, only: compiler_t, archiver_t, debug
use fpm_dependency, only: dependency_tree_t
use fpm_strings, only: string_t, str
implicit none

private
public :: fpm_model_t, srcfile_t, show_model

public :: FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
          FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, FPM_UNIT_CSOURCE, &
          FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, &
          FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST, &
          FPM_UNIT_CPPSOURCE

!> Source type unknown
integer, parameter :: FPM_UNIT_UNKNOWN = -1
!> Source contains a fortran program
integer, parameter :: FPM_UNIT_PROGRAM = 1
!> Source **only** contains one or more fortran modules
integer, parameter :: FPM_UNIT_MODULE = 2
!> Source contains one or more fortran submodules
integer, parameter :: FPM_UNIT_SUBMODULE = 3
!> Source contains one or more fortran subprogram not within modules
integer, parameter :: FPM_UNIT_SUBPROGRAM = 4
!> Source type is c source file
integer, parameter :: FPM_UNIT_CSOURCE = 5
!> Source type is c header file
integer, parameter :: FPM_UNIT_CHEADER = 6
!> Souce type is c++ source file.
integer, parameter :: FPM_UNIT_CPPSOURCE = 7

!> Source has no module-use scope
integer, parameter :: FPM_SCOPE_UNKNOWN = -1
!> Module-use scope is library/dependency modules only
integer, parameter :: FPM_SCOPE_LIB = 1
!> Module-use scope is library/dependency modules only
integer, parameter :: FPM_SCOPE_DEP = 2
!> Module-use scope is library/dependency and app modules
integer, parameter :: FPM_SCOPE_APP = 3
!> Module-use scope is library/dependency and test modules
integer, parameter :: FPM_SCOPE_TEST = 4
integer, parameter :: FPM_SCOPE_EXAMPLE = 5


!> Type for describing a source file
type srcfile_t
    !> File path relative to cwd
    character(:), allocatable :: file_name

    !> Name of executable for FPM_UNIT_PROGRAM
    character(:), allocatable :: exe_name

    !> Target module-use scope
    integer :: unit_scope = FPM_SCOPE_UNKNOWN

    !> Modules provided by this source file (lowerstring)
    type(string_t), allocatable :: modules_provided(:)

    !> Type of source unit
    integer :: unit_type = FPM_UNIT_UNKNOWN

    !> Parent modules (submodules only)
    type(string_t), allocatable :: parent_modules(:)

    !>  Modules USEd by this source file (lowerstring)
    type(string_t), allocatable :: modules_used(:)

    !> Files INCLUDEd by this source file
    type(string_t), allocatable :: include_dependencies(:)

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> Current hash
    integer(int64) :: digest

end type srcfile_t


!> Type for describing a single package
type package_t

    !> Name of package
    character(:), allocatable :: name

    !> Array of sources
    type(srcfile_t), allocatable :: sources(:)

    !> List of macros.
    type(string_t), allocatable :: macros(:)

    !> Package version number.
    character(:), allocatable :: version

end type package_t


!> Type describing everything required to build
!>  the root package and its dependencies.
type :: fpm_model_t

    !> Name of root package
    character(:), allocatable :: package_name

    !> Array of packages (including the root package)
    type(package_t), allocatable :: packages(:)

    !> Compiler object
    type(compiler_t) :: compiler

    !> Archiver object
    type(archiver_t) :: archiver

    !> Command line flags passed to fortran for compilation
    character(:), allocatable :: fortran_compile_flags

    !> Command line flags passed to C for compilation
    character(:), allocatable :: c_compile_flags

    !> Command line flags passed to C++ for compilation
    character(:), allocatable :: cxx_compile_flags

    !> Command line flags passed to the linker
    character(:), allocatable :: link_flags

    !> Base directory for build
    character(:), allocatable :: build_prefix

    !> Include directories
    type(string_t), allocatable :: include_dirs(:)

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> External modules used
    type(string_t), allocatable :: external_modules(:)

    !> Project dependencies
    type(dependency_tree_t) :: deps

    !> Whether tests should be added to the build list
    logical :: include_tests = .true.

end type fpm_model_t

contains


function info_package(p) result(s)
    ! Returns representation of package_t
    type(package_t), intent(in) :: p
    character(:), allocatable :: s

    integer :: i

    s = s // 'package_t('
    s = s // 'name="' // p%name //'"'
    s = s // ', sources=['
    do i = 1, size(p%sources)
        s = s // info_srcfile(p%sources(i))
        if (i < size(p%sources)) s = s // ", "
    end do
    s = s // "]"
    s = s // ")"

end function info_package

function info_srcfile(source) result(s)
    type(srcfile_t), intent(in) :: source
    character(:), allocatable :: s
    integer :: i
    !type srcfile_t
    s = "srcfile_t("
    !    character(:), allocatable :: file_name
    s = s // 'file_name="' // source%file_name // '"'
    !    character(:), allocatable :: exe_name
    s = s // ', exe_name="' // source%exe_name // '"'
    !    integer :: unit_scope = FPM_SCOPE_UNKNOWN
    s = s // ", unit_scope="
    select case(source%unit_scope)
    case (FPM_SCOPE_UNKNOWN)
        s = s // "FPM_SCOPE_UNKNOWN"
    case (FPM_SCOPE_LIB)
        s = s // "FPM_SCOPE_LIB"
    case (FPM_SCOPE_DEP)
        s = s // "FPM_SCOPE_DEP"
    case (FPM_SCOPE_APP)
        s = s // "FPM_SCOPE_APP"
    case (FPM_SCOPE_TEST)
        s = s // "FPM_SCOPE_TEST"
    case (FPM_SCOPE_EXAMPLE)
        s = s // "FPM_SCOPE_EXAMPLE"
    case default
        s = s // "INVALID"
    end select
    !    type(string_t), allocatable :: modules_provided(:)
    s = s // ", modules_provided=["
    do i = 1, size(source%modules_provided)
        s = s // '"' // source%modules_provided(i)%s // '"'
        if (i < size(source%modules_provided)) s = s // ", "
    end do
    s = s // "]"
    s = s // ", parent_modules=["
    do i = 1, size(source%parent_modules)
        s = s // '"' // source%parent_modules(i)%s // '"'
        if (i < size(source%parent_modules)) s = s // ", "
    end do
    s = s // "]"
    !    integer :: unit_type = FPM_UNIT_UNKNOWN
    s = s // ", unit_type="
    select case(source%unit_type)
    case (FPM_UNIT_UNKNOWN)
        s = s // "FPM_UNIT_UNKNOWN"
    case (FPM_UNIT_PROGRAM)
        s = s // "FPM_UNIT_PROGRAM"
    case (FPM_UNIT_MODULE)
        s = s // "FPM_UNIT_MODULE"
    case (FPM_UNIT_SUBMODULE)
        s = s // "FPM_UNIT_SUBMODULE"
    case (FPM_UNIT_SUBPROGRAM)
        s = s // "FPM_UNIT_SUBPROGRAM"
    case (FPM_UNIT_CSOURCE)
        s = s // "FPM_UNIT_CSOURCE"
    case (FPM_UNIT_CPPSOURCE)
        s = s // "FPM_UNIT_CPPSOURCE"
    case (FPM_UNIT_CHEADER)
        s = s // "FPM_UNIT_CHEADER"
    case default
        s = s // "INVALID"
    end select
    !    type(string_t), allocatable :: modules_used(:)
    s = s // ", modules_used=["
    do i = 1, size(source%modules_used)
        s = s // '"' // source%modules_used(i)%s // '"'
        if (i < size(source%modules_used)) s = s // ", "
    end do
    s = s // "]"
    !    type(string_t), allocatable :: include_dependencies(:)
    s = s // ", include_dependencies=["
    do i = 1, size(source%include_dependencies)
        s = s // '"' // source%include_dependencies(i)%s // '"'
        if (i < size(source%include_dependencies)) s = s // ", "
    end do
    s = s // "]"
    !    type(string_t), allocatable :: link_libraries(:)
    s = s // ", link_libraries=["
    do i = 1, size(source%link_libraries)
        s = s // '"' // source%link_libraries(i)%s // '"'
        if (i < size(source%link_libraries)) s = s // ", "
    end do
    s = s // "]"
    !    integer(int64) :: digest
    s = s // ", digest=" // str(source%digest)
    !end type srcfile_t
    s = s // ")"
end function info_srcfile

function info_srcfile_short(source) result(s)
    ! Prints a shortened version of srcfile_t
    type(srcfile_t), intent(in) :: source
    character(:), allocatable :: s
    s = "srcfile_t("
    s = s // 'file_name="' // source%file_name // '"'
    s = s // ", ...)"
end function info_srcfile_short

function info_model(model) result(s)
    type(fpm_model_t), intent(in) :: model
    character(:), allocatable :: s
    integer :: i
    !type :: fpm_model_t
    s = "fpm_model_t("
    !    character(:), allocatable :: package_name
    s = s // 'package_name="' // model%package_name // '"'
    !    type(srcfile_t), allocatable :: sources(:)
    s = s // ", packages=["
    do i = 1, size(model%packages)
        s = s // info_package(model%packages(i))
        if (i < size(model%packages)) s = s // ", "
    end do
    s = s // "]"
    s = s // ', compiler=(' // debug(model%compiler) // ')'
    s = s // ', archiver=(' // debug(model%archiver) // ')'
    !    character(:), allocatable :: fortran_compile_flags
    s = s // ', fortran_compile_flags="' // model%fortran_compile_flags // '"'
    s = s // ', c_compile_flags="' // model%c_compile_flags // '"'
    s = s // ', cxx_compile_flags="' // model%cxx_compile_flags // '"'
    s = s // ', link_flags="' // model%link_flags // '"'
    s = s // ', build_prefix="' // model%build_prefix // '"'
    !    type(string_t), allocatable :: link_libraries(:)
    s = s // ", link_libraries=["
    do i = 1, size(model%link_libraries)
        s = s // '"' // model%link_libraries(i)%s // '"'
        if (i < size(model%link_libraries)) s = s // ", "
    end do
    s = s // "]"
    !    type(string_t), allocatable :: external_modules(:)
    s = s // ", external_modules=["
    do i = 1, size(model%external_modules)
        s = s // '"' // model%external_modules(i)%s // '"'
        if (i < size(model%external_modules)) s = s // ", "
    end do
    s = s // "]"
    !    type(dependency_tree_t) :: deps
    ! TODO: print `dependency_tree_t` properly, which should become part of the
    !       model, not imported from another file
    s = s // ", deps=dependency_tree_t(...)"
    !end type fpm_model_t
    s = s // ")"
end function info_model

subroutine show_model(model)
    ! Prints a human readable representation of the Model
    type(fpm_model_t), intent(in) :: model
    print *, info_model(model)
end subroutine show_model

end module fpm_model
 
 
!>>>>> ././src/fpm/cmd/update.f90
module fpm_cmd_update
  use fpm_command_line, only : fpm_update_settings
  use fpm_dependency, only : dependency_tree_t, new_dependency_tree
  use fpm_error, only : error_t, fpm_stop
  use fpm_filesystem, only : exists, mkdir, join_path, delete_file, filewrite
  use fpm_manifest, only : package_config_t, get_package_data
  implicit none
  private
  public :: cmd_update

contains

  !> Entry point for the update subcommand
  subroutine cmd_update(settings)
    !> Representation of the command line arguments
    type(fpm_update_settings), intent(in) :: settings
    type(package_config_t) :: package
    type(dependency_tree_t) :: deps
    type(error_t), allocatable :: error

    integer :: ii
    character(len=:), allocatable :: cache

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)

    if (.not.exists("build")) then
      call mkdir("build")
      call filewrite(join_path("build", ".gitignore"),["*"])
    end if

    cache = join_path("build", "cache.toml")
    if (settings%clean) then
      call delete_file(cache)
    end if

    call new_dependency_tree(deps, cache=cache, &
      verbosity=merge(2, 1, settings%verbose))

    call deps%add(package, error)
    call handle_error(error)

    if (settings%fetch_only) return

    if (size(settings%name) == 0) then
      do ii = 1, deps%ndep
        call deps%update(deps%dep(ii)%name, error)
        call handle_error(error)
      end do
    else
      do ii = 1, size(settings%name)
        call deps%update(trim(settings%name(ii)), error)
        call handle_error(error)
      end do
    end if

  end subroutine cmd_update

  !> Error handling for this command
  subroutine handle_error(error)
    !> Potential error
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(1, error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_update
 
 
!>>>>> ././src/fpm_source_parsing.f90
!># Parsing of package source files
!>
!> This module exposes two functions, `[[parse_f_source]]` and `[[parse_c_source]]`,
!> which perform a rudimentary parsing of fortran and c source files
!> in order to extract information required for module dependency tracking.
!>
!> Both functions additionally calculate and store a file digest (hash) which
!> is used by the backend ([[fpm_backend]]) to skip compilation of unmodified sources.
!>
!> Both functions return an instance of the [[srcfile_t]] type.
!>
!> For more information, please read the documentation for each function:
!>
!> - `[[parse_f_source]]`
!> - `[[parse_c_source]]`
!>
module fpm_source_parsing
use fpm_error, only: error_t, file_parse_error, fatal_error, file_not_found_error
use fpm_strings, only: string_t, string_cat, len_trim, split, lower, str_ends_with, fnv_1a, is_fortran_name
use fpm_model, only: srcfile_t, &
                    FPM_UNIT_UNKNOWN, FPM_UNIT_PROGRAM, FPM_UNIT_MODULE, &
                    FPM_UNIT_SUBMODULE, FPM_UNIT_SUBPROGRAM, &
                    FPM_UNIT_CSOURCE, FPM_UNIT_CHEADER, FPM_SCOPE_UNKNOWN, &
                    FPM_SCOPE_LIB, FPM_SCOPE_DEP, FPM_SCOPE_APP, FPM_SCOPE_TEST, &
                    FPM_UNIT_CPPSOURCE
use fpm_filesystem, only: read_lines, read_lines_expanded, exists
implicit none

private
public :: parse_f_source, parse_c_source

character(15), parameter :: INTRINSIC_MODULE_NAMES(*) =  &
                             ['iso_c_binding  ', &
                              'iso_fortran_env', &
                              'ieee_arithmetic', &
                              'ieee_exceptions', &
                              'ieee_features  ', &
                              'omp_lib        ']

contains

!> Parsing of free-form fortran source files
!>
!> The following statements are recognised and parsed:
!>
!> - `Module`/`submodule`/`program` declaration
!> - Module `use` statement
!> - `include` statement
!>
!> @note Intrinsic modules used by sources are not listed in
!> the `modules_used` field of source objects.
!>
!> @note Submodules are treated as normal modules which `use` their
!> corresponding parent modules.
!>
!>### Parsing limitations
!>
!> __Statements must not continued onto another line
!>  except for an `only:` list in the `use` statement.__
!>
!> This is supported:
!>
!>```fortran
!> use my_module, only: &
!>      my_var, my_function, my_subroutine
!>```
!>
!> This is __NOT supported:__
!>
!>```fortran
!> use &
!>    my_module
!>```
!>
function parse_f_source(f_filename,error) result(f_source)
    character(*), intent(in) :: f_filename
    type(srcfile_t) :: f_source
    type(error_t), allocatable, intent(out) :: error

    logical :: inside_module, inside_interface
    integer :: stat
    integer :: fh, n_use, n_include, n_mod, n_parent, i, j, ic, pass
    type(string_t), allocatable :: file_lines(:), file_lines_lower(:)
    character(:), allocatable :: temp_string, mod_name, string_parts(:)

    if (.not. exists(f_filename)) then
        call file_not_found_error(error, f_filename)
        return
    end if

    f_source%file_name = f_filename

    open(newunit=fh,file=f_filename,status='old')
    file_lines = read_lines_expanded(fh)
    close(fh)

    ! for efficiency in parsing make a lowercase left-adjusted copy of the file
    ! Need a copy because INCLUDE (and #include) file arguments are case-sensitive
    file_lines_lower=file_lines
    do i=1,size(file_lines_lower)
       file_lines_lower(i)%s=adjustl(lower(file_lines_lower(i)%s))
    enddo

    ! fnv_1a can only be applied to non-zero-length arrays
    if (len_trim(file_lines_lower) > 0) f_source%digest = fnv_1a(file_lines)

    do pass = 1,2
        n_use = 0
        n_include = 0
        n_mod = 0
        n_parent = 0
        inside_module = .false.
        inside_interface = .false.
        file_loop: do i=1,size(file_lines_lower)

            ! Skip comment lines and preprocessor directives
            if (index(file_lines_lower(i)%s,'!') == 1 .or. &
                index(file_lines_lower(i)%s,'#') == 1 .or. &
                len_trim(file_lines_lower(i)%s) < 1) then
                cycle
            end if

            ! Detect exported C-API via bind(C)
            if (.not.inside_interface .and. &
                parse_subsequence(file_lines_lower(i)%s,'bind','(','c')) then
                
                do j=i,1,-1

                    if (index(file_lines_lower(j)%s,'function') > 0 .or. &
                        index(file_lines_lower(j)%s,'subroutine') > 0) then
                        f_source%unit_type = FPM_UNIT_SUBPROGRAM
                        exit
                    end if

                    if (j>1) then

                        ic = index(file_lines_lower(j-1)%s,'!')
                        if (ic < 1) then
                            ic = len(file_lines_lower(j-1)%s)
                        end if

                        temp_string = trim(file_lines_lower(j-1)%s(1:ic))
                        if (index(temp_string,'&') /= len(temp_string)) then
                            exit
                        end if

                    end if

                end do

            end if

            ! Skip lines that are continued: not statements
            if (i > 1) then
                ic = index(file_lines_lower(i-1)%s,'!')
                if (ic < 1) then
                    ic = len(file_lines_lower(i-1)%s)
                end if
                temp_string = trim(file_lines_lower(i-1)%s(1:ic))
                if (len(temp_string) > 0 .and. index(temp_string,'&') == len(temp_string)) then
                    cycle
                end if
            end if

            ! Detect beginning of interface block
            if (index(file_lines_lower(i)%s,'interface') == 1) then

                inside_interface = .true.
                cycle

            end if

            ! Detect end of interface block
            if (parse_sequence(file_lines_lower(i)%s,'end','interface')) then

                inside_interface = .false.
                cycle

            end if

            ! Process 'USE' statements
            if (index(file_lines_lower(i)%s,'use ') == 1 .or. &
                index(file_lines_lower(i)%s,'use::') == 1) then

                if (index(file_lines_lower(i)%s,'::') > 0) then

                    temp_string = split_n(file_lines_lower(i)%s,delims=':',n=2,stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,f_filename, &
                                'unable to find used module name',i, &
                                file_lines_lower(i)%s,index(file_lines_lower(i)%s,'::'))
                        return
                    end if

                    mod_name = split_n(temp_string,delims=' ,',n=1,stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,f_filename, &
                                 'unable to find used module name',i, &
                                 file_lines_lower(i)%s)
                        return
                    end if

                else

                    mod_name = split_n(file_lines_lower(i)%s,n=2,delims=' ,',stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,f_filename, &
                                'unable to find used module name',i, &
                                file_lines_lower(i)%s)
                        return
                    end if

                end if

                if (.not.is_fortran_name(mod_name)) then
                    cycle
                end if

                if (any([(index(mod_name,trim(INTRINSIC_MODULE_NAMES(j)))>0, &
                            j=1,size(INTRINSIC_MODULE_NAMES))])) then
                    cycle
                end if

                n_use = n_use + 1

                if (pass == 2) then

                    f_source%modules_used(n_use)%s = mod_name

                end if

                cycle

            end if

            ! Process 'INCLUDE' statements
            ic = index(file_lines_lower(i)%s,'include')
            if ( ic == 1 ) then
                ic = index(lower(file_lines(i)%s),'include')
                if (index(adjustl(file_lines(i)%s(ic+7:)),'"') == 1 .or. &
                    index(adjustl(file_lines(i)%s(ic+7:)),"'") == 1 ) then

                    n_include = n_include + 1

                    if (pass == 2) then
                        f_source%include_dependencies(n_include)%s = &
                         & split_n(file_lines(i)%s,n=2,delims="'"//'"',stat=stat)
                        if (stat /= 0) then
                            call file_parse_error(error,f_filename, &
                                  'unable to find include file name',i, &
                                  file_lines(i)%s)
                            return
                        end if
                    end if

                    cycle

                end if
            end if

            ! Extract name of module if is module
            if (index(file_lines_lower(i)%s,'module ') == 1) then

                ! Remove any trailing comments
                ic = index(file_lines_lower(i)%s,'!')-1
                if (ic < 1) then
                    ic = len(file_lines_lower(i)%s)
                end if
                temp_string = trim(file_lines_lower(i)%s(1:ic))

                ! R1405 module-stmt := "MODULE" module-name
                ! module-stmt has two space-delimited parts only
                ! (no line continuations)
                call split(temp_string,string_parts,' ')
                if (size(string_parts) /= 2) then
                    cycle
                end if

                mod_name = trim(adjustl(string_parts(2)))
                if (scan(mod_name,'=(&')>0 ) then
                    ! Ignore these cases:
                    ! module <something>&
                    ! module =*
                    ! module (i)
                    cycle
                end if

                if (.not.is_fortran_name(mod_name)) then
                    call file_parse_error(error,f_filename, &
                          'empty or invalid name for module',i, &
                          file_lines_lower(i)%s, index(file_lines_lower(i)%s,mod_name))
                    return
                end if

                n_mod = n_mod + 1

                if (pass == 2) then
                    f_source%modules_provided(n_mod) = string_t(mod_name)
                end if

                if (f_source%unit_type == FPM_UNIT_UNKNOWN) then
                    f_source%unit_type = FPM_UNIT_MODULE
                end if

                if (.not.inside_module) then    
                    inside_module = .true.
                else
                    ! Must have missed an end module statement (can't assume a pure module)
                    if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
                        f_source%unit_type = FPM_UNIT_SUBPROGRAM
                    end if
                end if

                cycle

            end if

            ! Extract name of submodule if is submodule
            if (index(file_lines_lower(i)%s,'submodule') == 1) then

                mod_name = split_n(file_lines_lower(i)%s,n=3,delims='()',stat=stat)
                if (stat /= 0) then
                    call file_parse_error(error,f_filename, &
                          'unable to get submodule name',i, &
                          file_lines_lower(i)%s)
                    return
                end if
                if (.not.is_fortran_name(mod_name)) then
                    call file_parse_error(error,f_filename, &
                          'empty or invalid name for submodule',i, &
                          file_lines_lower(i)%s, index(file_lines_lower(i)%s,mod_name))
                    return
                end if

                n_mod = n_mod + 1

                temp_string = split_n(file_lines_lower(i)%s,n=2,delims='()',stat=stat)
                if (stat /= 0) then
                    call file_parse_error(error,f_filename, &
                          'unable to get submodule ancestry',i, &
                          file_lines_lower(i)%s)
                    return
                end if
                
                if (f_source%unit_type /= FPM_UNIT_PROGRAM) then
                    f_source%unit_type = FPM_UNIT_SUBMODULE
                end if

                n_use = n_use + 1

                inside_module = .true.

                n_parent = n_parent + 1

                if (pass == 2) then

                    if (index(temp_string,':') > 0) then

                        temp_string = temp_string(index(temp_string,':')+1:)

                    end if

                    if (.not.is_fortran_name(temp_string)) then
                        call file_parse_error(error,f_filename, &
                          'empty or invalid name for submodule parent',i, &
                          file_lines_lower(i)%s, index(file_lines_lower(i)%s,temp_string))
                        return
                    end if

                    f_source%modules_used(n_use)%s = temp_string
                    f_source%parent_modules(n_parent)%s = temp_string
                    f_source%modules_provided(n_mod)%s = mod_name

                end if

                cycle

            end if

            ! Detect if contains a program
            !  (no modules allowed after program def)
            if (index(file_lines_lower(i)%s,'program ') == 1) then

                temp_string = split_n(file_lines_lower(i)%s,n=2,delims=' ',stat=stat)
                if (stat == 0) then

                    if (scan(temp_string,'=(')>0 ) then
                        ! Ignore:
                        ! program =*
                        ! program (i) =*
                        cycle
                    end if

                end if

                f_source%unit_type = FPM_UNIT_PROGRAM

                cycle

            end if

            ! Parse end module statement
            !  (to check for code outside of modules)
            if (parse_sequence(file_lines_lower(i)%s,'end','module') .or. &
                parse_sequence(file_lines_lower(i)%s,'end','submodule')) then
                
                inside_module = .false.
                cycle

            end if

            ! Any statements not yet parsed are assumed to be other code statements
            if (.not.inside_module .and. f_source%unit_type /= FPM_UNIT_PROGRAM) then

                f_source%unit_type = FPM_UNIT_SUBPROGRAM

            end if

        end do file_loop

        ! If unable to parse end of module statement, then can't assume pure module
        !  (there could be non-module subprograms present)
        if (inside_module .and. f_source%unit_type == FPM_UNIT_MODULE) then
            f_source%unit_type = FPM_UNIT_SUBPROGRAM
        end if

        if (pass == 1) then
            allocate(f_source%modules_used(n_use))
            allocate(f_source%include_dependencies(n_include))
            allocate(f_source%modules_provided(n_mod))
            allocate(f_source%parent_modules(n_parent))
        end if

    end do

end function parse_f_source


!> Parsing of c, cpp source files
!>
!> The following statements are recognised and parsed:
!>
!> - `#include` preprocessor statement
!>
function parse_c_source(c_filename,error) result(c_source)
    character(*), intent(in) :: c_filename
    type(srcfile_t) :: c_source
    type(error_t), allocatable, intent(out) :: error

    integer :: fh, n_include, i, pass, stat
    type(string_t), allocatable :: file_lines(:)

    c_source%file_name = c_filename

    if (str_ends_with(lower(c_filename), ".c")) then

        c_source%unit_type = FPM_UNIT_CSOURCE

    else if (str_ends_with(lower(c_filename), ".h")) then

        c_source%unit_type = FPM_UNIT_CHEADER

    else if (str_ends_with(lower(c_filename), ".cpp")) then 

        c_source%unit_type = FPM_UNIT_CPPSOURCE

    end if

    allocate(c_source%modules_used(0))
    allocate(c_source%modules_provided(0))
    allocate(c_source%parent_modules(0))

    open(newunit=fh,file=c_filename,status='old')
    file_lines = read_lines(fh)
    close(fh)

    ! Ignore empty files, returned as FPM_UNIT_UNKNOWN
    if (len_trim(file_lines) < 1) then
        c_source%unit_type = FPM_UNIT_UNKNOWN
        return
    end if

    c_source%digest = fnv_1a(file_lines)

    do pass = 1,2
        n_include = 0
        file_loop: do i=1,size(file_lines)

            ! Process 'INCLUDE' statements
            if (index(adjustl(lower(file_lines(i)%s)),'#include') == 1 .and. &
                index(file_lines(i)%s,'"') > 0) then

                n_include = n_include + 1

                if (pass == 2) then

                    c_source%include_dependencies(n_include)%s = &
                     &   split_n(file_lines(i)%s,n=2,delims='"',stat=stat)
                    if (stat /= 0) then
                        call file_parse_error(error,c_filename, &
                            'unable to get c include file',i, &
                            file_lines(i)%s,index(file_lines(i)%s,'"'))
                        return
                    end if

                end if

            end if

        end do file_loop

        if (pass == 1) then
            allocate(c_source%include_dependencies(n_include))
        end if

    end do

end function parse_c_source

!> Split a string on one or more delimeters
!>  and return the nth substring if it exists
!>
!> n=0  will return the last item
!> n=-1 will return the penultimate item etc.
!>
!> stat = 1 on return if the index
!>  is not found
!>
function split_n(string,delims,n,stat) result(substring)

    character(*), intent(in) :: string
    character(*), intent(in) :: delims
    integer, intent(in) :: n
    integer, intent(out) :: stat
    character(:), allocatable :: substring

    integer :: i
    character(:), allocatable :: string_parts(:)

    call split(string,string_parts,delims)

    if (n<1) then
        i = size(string_parts) + n
        if (i < 1) then
            stat = 1
            return
        end if
    else
        i = n
    end if

    if (i>size(string_parts)) then
        stat = 1
        return
    end if

    substring = trim(adjustl(string_parts(i)))
    stat = 0

end function split_n


!> Parse a subsequence of blank-separated tokens within a string
!>  (see parse_sequence)
function parse_subsequence(string,t1,t2,t3,t4) result(found)
    character(*), intent(in) :: string
    character(*), intent(in) :: t1
    character(*), intent(in), optional :: t2, t3, t4
    logical :: found

    integer :: offset, i

    found = .false.
    offset = 1

    do 

        i = index(string(offset:),t1)

        if (i == 0) return

        offset = offset + i - 1

        found = parse_sequence(string(offset:),t1,t2,t3,t4)

        if (found) return

        offset = offset + len(t1)

        if (offset > len(string)) return

    end do

end function parse_subsequence

!> Helper utility to parse sequences of tokens
!> that may be optionally separated by zero or more spaces
function parse_sequence(string,t1,t2,t3,t4) result(found)
    character(*), intent(in) :: string
    character(*), intent(in) :: t1
    character(*), intent(in), optional :: t2, t3, t4
    logical :: found

    integer :: post, n, incr, pos, token_n
    logical :: match

    n = len(string)
    found = .false.
    pos = 1

    do token_n=1,4

        do while (pos <= n)
            if (string(pos:pos) /= ' ') then
                exit
            end if
            pos = pos + 1
        end do

        select case(token_n)
        case(1)
            incr = len(t1)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t1
        case(2)
            if (.not.present(t2)) exit
            incr = len(t2)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t2
        case(3)
            if (.not.present(t3)) exit
            incr = len(t3)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t3
        case(4)
            if (.not.present(t4)) exit
            incr = len(t4)
            if (pos+incr-1>n) return
            match = string(pos:pos+incr-1) == t4
        case default
            exit
        end select

        if (.not.match) then
            return
        end if

        pos = pos + incr

    end do

    found = .true.

end function parse_sequence

end module fpm_source_parsing

 
 
!>>>>> ././src/fpm_targets.f90
!># Build target handling
!>
!> This module handles the construction of the build target list
!> from the sources list (`[[targets_from_sources]]`), the
!> resolution of module-dependencies between build targets
!> (`[[resolve_module_dependencies]]`), and the enumeration of
!> objects required for link targets (`[[resolve_target_linking]]`).
!>
!> A build target (`[[build_target_t]]`) is a file to be generated
!> by the backend (compilation and linking).
!>
!> @note The current implementation is ignorant to the existence of
!> module files (`.mod`,`.smod`). Dependencies arising from modules
!> are based on the corresponding object files (`.o`) only.
!>
!> For more information, please read the documentation for the procedures:
!>
!> - `[[build_target_list]]`
!> - `[[resolve_module_dependencies]]`
!>
!>### Enumerations
!>
!> __Target type:__ `FPM_TARGET_*`
!> Describes the type of build target — determines backend build rules
!>
module fpm_targets
use iso_fortran_env, only: int64
use fpm_error, only: error_t, fatal_error, fpm_stop
use fpm_model
use fpm_environment, only: get_os_type, OS_WINDOWS, OS_MACOS
use fpm_filesystem, only: dirname, join_path, canon_path
use fpm_strings, only: string_t, operator(.in.), string_cat, fnv_1a, resize, lower, str_ends_with
use fpm_compiler, only: get_macros
implicit none

private

public FPM_TARGET_UNKNOWN, FPM_TARGET_EXECUTABLE, &
       FPM_TARGET_ARCHIVE, FPM_TARGET_OBJECT, &
       FPM_TARGET_C_OBJECT, FPM_TARGET_CPP_OBJECT
public build_target_t, build_target_ptr
public targets_from_sources, resolve_module_dependencies
public resolve_target_linking, add_target, add_dependency
public filter_library_targets, filter_executable_targets, filter_modules



!> Target type is unknown (ignored)
integer, parameter :: FPM_TARGET_UNKNOWN = -1
!> Target type is executable
integer, parameter :: FPM_TARGET_EXECUTABLE = 1
!> Target type is library archive
integer, parameter :: FPM_TARGET_ARCHIVE = 2
!> Target type is compiled object
integer, parameter :: FPM_TARGET_OBJECT = 3
!> Target type is c compiled object
integer, parameter :: FPM_TARGET_C_OBJECT = 4
!> Target type is cpp compiled object
integer, parameter :: FPM_TARGET_CPP_OBJECT = 5

!> Wrapper type for constructing arrays of `[[build_target_t]]` pointers
type build_target_ptr

    type(build_target_t), pointer :: ptr => null()

end type build_target_ptr


!> Type describing a generated build target
type build_target_t

    !> File path of build target object relative to cwd
    character(:), allocatable :: output_file

    !> File path of build target object relative to output_dir
    character(:), allocatable :: output_name

    !> File path of output directory
    character(:), allocatable :: output_dir

    !> File path of build log file relative to cwd
    character(:), allocatable :: output_log_file

    !> Name of parent package
    character(:), allocatable :: package_name

    !> Primary source for this build target
    type(srcfile_t), allocatable :: source

    !> Resolved build dependencies
    type(build_target_ptr), allocatable :: dependencies(:)

    !> Target type
    integer :: target_type = FPM_TARGET_UNKNOWN

    !> Native libraries to link against
    type(string_t), allocatable :: link_libraries(:)

    !> Objects needed to link this target
    type(string_t), allocatable :: link_objects(:)

    !> Link flags for this build target
    character(:), allocatable :: link_flags

    !> Compile flags for this build target
    character(:), allocatable :: compile_flags

    !> Flag set when first visited to check for circular dependencies
    logical :: touched = .false.

    !> Flag set if build target is sorted for building
    logical :: sorted = .false.

    !> Flag set if build target will be skipped (not built)
    logical :: skip = .false.

    !> Targets in the same schedule group are guaranteed to be independent
    integer :: schedule = -1

    !> Previous source file hash
    integer(int64), allocatable :: digest_cached

    !> List of macros
    type(string_t), allocatable :: macros(:)

    !> Version number
    character(:), allocatable :: version

end type build_target_t


contains

!> High-level wrapper to generate build target information
subroutine targets_from_sources(targets,model,prune,error)

    !> The generated list of build targets
    type(build_target_ptr), intent(out), allocatable :: targets(:)

    !> The package model from which to construct the target list
    type(fpm_model_t), intent(inout), target :: model

    !> Enable tree-shaking/pruning of module dependencies
    logical, intent(in) :: prune
    
    !> Error structure
    type(error_t), intent(out), allocatable :: error

    call build_target_list(targets,model)

    call collect_exe_link_dependencies(targets)

    call resolve_module_dependencies(targets,model%external_modules,error)
    if (allocated(error)) return

    if (prune) then
        call prune_build_targets(targets,root_package=model%package_name)
    end if

    call resolve_target_linking(targets,model)

end subroutine targets_from_sources


!> Constructs a list of build targets from a list of source files
!>
!>### Source-target mapping
!>
!> One compiled object target (`FPM_TARGET_OBJECT`) is generated for each
!> non-executable source file (`FPM_UNIT_MODULE`,`FPM_UNIT_SUBMODULE`,
!>  `FPM_UNIT_SUBPROGRAM`,`FPM_UNIT_CSOURCE`).
!>
!> If any source file has scope `FPM_SCOPE_LIB` (*i.e.* there are library sources)
!> then the first target in the target list will be a library archive target
!> (`FPM_TARGET_ARCHIVE`). The archive target will have a dependency on every
!> compiled object target corresponding to a library source file.
!>
!> One compiled object target (`FPM_TARGET_OBJECT`) and one executable target (`FPM_TARGET_EXECUTABLE`) is
!> generated for each exectuable source file (`FPM_UNIT_PROGRAM`). The exectuble target
!> always has a dependency on the corresponding compiled object target. If there
!> is a library, then the executable target has an additional dependency on the library
!> archive target.
!>
subroutine build_target_list(targets,model)

    !> The generated list of build targets
    type(build_target_ptr), intent(out), allocatable :: targets(:)

    !> The package model from which to construct the target list
    type(fpm_model_t), intent(inout), target :: model

    integer :: i, j, n_source, exe_type
    character(:), allocatable :: xsuffix, exe_dir
    logical :: with_lib

    ! Check for empty build (e.g. header-only lib)
    n_source = sum([(size(model%packages(j)%sources), &
                      j=1,size(model%packages))])

    if (n_source < 1) then
        allocate(targets(0))
        return
    end if

    if (get_os_type() == OS_WINDOWS) then
        xsuffix = '.exe'
    else
        xsuffix = ''
    end if

    with_lib = any([((model%packages(j)%sources(i)%unit_scope == FPM_SCOPE_LIB, &
                      i=1,size(model%packages(j)%sources)), &
                      j=1,size(model%packages))])

    if (with_lib) call add_target(targets,package=model%package_name,type = FPM_TARGET_ARCHIVE,&
                            output_name = join_path(&
                                   model%package_name,'lib'//model%package_name//'.a'))

    do j=1,size(model%packages)

        associate(sources=>model%packages(j)%sources)

            do i=1,size(sources)

                if (.not. model%include_tests) then
                    if (sources(i)%unit_scope == FPM_SCOPE_TEST) cycle
                end if

                select case (sources(i)%unit_type)
                case (FPM_UNIT_MODULE,FPM_UNIT_SUBMODULE,FPM_UNIT_SUBPROGRAM,FPM_UNIT_CSOURCE)

                    call add_target(targets,package=model%packages(j)%name,source = sources(i), &
                                type = merge(FPM_TARGET_C_OBJECT,FPM_TARGET_OBJECT,&
                                               sources(i)%unit_type==FPM_UNIT_CSOURCE), &
                                output_name = get_object_name(sources(i)), &
                                macros = model%packages(j)%macros, &
                                version = model%packages(j)%version)
                                

                    if (with_lib .and. sources(i)%unit_scope == FPM_SCOPE_LIB) then
                        ! Archive depends on object
                        call add_dependency(targets(1)%ptr, targets(size(targets))%ptr)
                    end if

                case (FPM_UNIT_CPPSOURCE) 

                    call add_target(targets,package=model%packages(j)%name,source = sources(i), &
                                type = FPM_TARGET_CPP_OBJECT, &
                                output_name = get_object_name(sources(i)), &
                                macros = model%packages(j)%macros, &
                                version = model%packages(j)%version)

                    if (with_lib .and. sources(i)%unit_scope == FPM_SCOPE_LIB) then
                        ! Archive depends on object
                        call add_dependency(targets(1)%ptr, targets(size(targets))%ptr)
                    end if

                    !> Add stdc++ as a linker flag. If not already there.
                    if (.not. ("stdc++" .in. model%link_libraries)) then

                        if (get_os_type() == OS_MACOS) then
                            model%link_libraries = [model%link_libraries, string_t("c++")]
                        else
                            model%link_libraries = [model%link_libraries, string_t("stdc++")]
                        end if

                    end if

                case (FPM_UNIT_PROGRAM)

                    if (str_ends_with(lower(sources(i)%file_name), [".c"])) then
                        exe_type = FPM_TARGET_C_OBJECT
                    else if (str_ends_with(lower(sources(i)%file_name), [".cpp", ".cc "])) then
                        exe_type = FPM_TARGET_CPP_OBJECT
                    else    ! Default to a Fortran object
                        exe_type = FPM_TARGET_OBJECT
                    end if

                    call add_target(targets,package=model%packages(j)%name,type = exe_type,&
                                output_name = get_object_name(sources(i)), &
                                source = sources(i), &
                                macros = model%packages(j)%macros &
                                )

                    if (sources(i)%unit_scope == FPM_SCOPE_APP) then

                        exe_dir = 'app'

                    else if (sources(i)%unit_scope == FPM_SCOPE_EXAMPLE) then

                        exe_dir = 'example'

                    else

                        exe_dir = 'test'

                    end if

                    call add_target(targets,package=model%packages(j)%name,type = FPM_TARGET_EXECUTABLE,&
                                    link_libraries = sources(i)%link_libraries, &
                                    output_name = join_path(exe_dir, &
                                    sources(i)%exe_name//xsuffix))

                    ! Executable depends on object
                    call add_dependency(targets(size(targets))%ptr, targets(size(targets)-1)%ptr)

                    if (with_lib) then
                        ! Executable depends on library
                        call add_dependency(targets(size(targets))%ptr, targets(1)%ptr)
                    end if

                end select

            end do

        end associate

    end do

    contains

    function get_object_name(source) result(object_file)
        ! Generate object target path from source name and model params
        !
        !
        type(srcfile_t), intent(in) :: source
        character(:), allocatable :: object_file

        integer :: i
        character(1), parameter :: filesep = '/'

        object_file = canon_path(source%file_name)

        ! Convert any remaining directory separators to underscores
        i = index(object_file,filesep)
        do while(i > 0)
            object_file(i:i) = '_'
            i = index(object_file,filesep)
        end do

        object_file = join_path(model%package_name,object_file)//'.o'

    end function get_object_name

end subroutine build_target_list


!> Add non-library non-module dependencies for executable targets
!>
!>  Executable targets will link to any non-program non-module source files that
!>   are in the same directory or in a subdirectory.
!>
!>  (Note: Fortran module dependencies are handled separately in
!>    `resolve_module_dependencies` and `resolve_target_linking`.)
!>
subroutine collect_exe_link_dependencies(targets)
    type(build_target_ptr), intent(inout) :: targets(:)

    integer :: i, j
    character(:), allocatable :: exe_source_dir

    ! Add non-module dependencies for executables
    do j=1,size(targets)

        if (targets(j)%ptr%target_type == FPM_TARGET_EXECUTABLE) then

            do i=1,size(targets)

                if (i == j) cycle

                associate(exe => targets(j)%ptr, dep => targets(i)%ptr)

                    exe_source_dir = dirname(exe%dependencies(1)%ptr%source%file_name)

                    if (allocated(dep%source)) then

                        if (dep%source%unit_scope /= FPM_SCOPE_LIB .and. &
                            dep%source%unit_type /= FPM_UNIT_PROGRAM .and. &
                            dep%source%unit_type /= FPM_UNIT_MODULE .and. &
                            index(dirname(dep%source%file_name), exe_source_dir) == 1) then

                            call add_dependency(exe, dep) 

                        end if

                    end if

                end associate

            end do

        end if

    end do

end subroutine collect_exe_link_dependencies


!> Allocate a new target and append to target list
subroutine add_target(targets,package,type,output_name,source,link_libraries, macros, version)
    type(build_target_ptr), allocatable, intent(inout) :: targets(:)
    character(*), intent(in) :: package
    integer, intent(in) :: type
    character(*), intent(in) :: output_name
    type(srcfile_t), intent(in), optional :: source
    type(string_t), intent(in), optional :: link_libraries(:)
    type(string_t), intent(in), optional :: macros(:)
    character(*), intent(in), optional :: version

    integer :: i
    type(build_target_t), pointer :: new_target

    if (.not.allocated(targets)) allocate(targets(0))

    ! Check for duplicate outputs
    do i=1,size(targets)

        if (targets(i)%ptr%output_name == output_name) then

            write(*,*) 'Error while building target list: duplicate output object "',&
                        output_name,'"'
            if (present(source)) write(*,*) ' Source file: "',source%file_name,'"'
            call fpm_stop(1,' ')

        end if

    end do

    allocate(new_target)
    new_target%target_type = type
    new_target%output_name = output_name
    new_target%package_name = package
    if (present(source)) new_target%source = source
    if (present(link_libraries)) new_target%link_libraries = link_libraries
    if (present(macros)) new_target%macros = macros
    if (present(version)) new_target%version = version
    allocate(new_target%dependencies(0))

    targets = [targets, build_target_ptr(new_target)]

end subroutine add_target


!> Add pointer to dependeny in target%dependencies
subroutine add_dependency(target, dependency)
    type(build_target_t), intent(inout) :: target
    type(build_target_t) , intent(in), target :: dependency

    target%dependencies = [target%dependencies, build_target_ptr(dependency)]

end subroutine add_dependency


!> Add dependencies to source-based targets (`FPM_TARGET_OBJECT`)
!> based on any modules used by the corresponding source file.
!>
!>### Source file scoping
!>
!> Source files are assigned a scope of either `FPM_SCOPE_LIB`,
!> `FPM_SCOPE_APP` or `FPM_SCOPE_TEST`. The scope controls which
!> modules may be used by the source file:
!>
!> - Library sources (`FPM_SCOPE_LIB`) may only use modules
!>   also with library scope. This includes library modules
!>   from dependencies.
!>
!> - Executable sources (`FPM_SCOPE_APP`,`FPM_SCOPE_TEST`) may use
!>   library modules (including dependencies) as well as any modules
!>   corresponding to source files in the same directory or a
!>   subdirectory of the executable source file.
!>
!> @warning If a module used by a source file cannot be resolved to
!> a source file in the package of the correct scope, then a __fatal error__
!> is returned by the procedure and model construction fails.
!>
subroutine resolve_module_dependencies(targets,external_modules,error)
    type(build_target_ptr), intent(inout), target :: targets(:)
    type(string_t), intent(in) :: external_modules(:)
    type(error_t), allocatable, intent(out) :: error

    type(build_target_ptr) :: dep

    integer :: i, j

    do i=1,size(targets)

        if (.not.allocated(targets(i)%ptr%source)) cycle

            do j=1,size(targets(i)%ptr%source%modules_used)

                if (targets(i)%ptr%source%modules_used(j)%s .in. targets(i)%ptr%source%modules_provided) then
                    ! Dependency satisfied in same file, skip
                    cycle
                end if

                if (targets(i)%ptr%source%modules_used(j)%s .in. external_modules) then
                    ! Dependency satisfied in system-installed module
                    cycle
                end if

                if (any(targets(i)%ptr%source%unit_scope == &
                    [FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST])) then
                    dep%ptr => &
                        find_module_dependency(targets,targets(i)%ptr%source%modules_used(j)%s, &
                                            include_dir = dirname(targets(i)%ptr%source%file_name))
                else
                    dep%ptr => &
                        find_module_dependency(targets,targets(i)%ptr%source%modules_used(j)%s)
                end if

                if (.not.associated(dep%ptr)) then
                    call fatal_error(error, &
                            'Unable to find source for module dependency: "' // &
                            targets(i)%ptr%source%modules_used(j)%s // &
                            '" used by "'//targets(i)%ptr%source%file_name//'"')
                    return
                end if

                call add_dependency(targets(i)%ptr, dep%ptr)

            end do

    end do

end subroutine resolve_module_dependencies

function find_module_dependency(targets,module_name,include_dir) result(target_ptr)
    ! Find a module dependency in the library or a dependency library
    !
    ! 'include_dir' specifies an allowable non-library search directory
    !   (Used for executable dependencies)
    !
    type(build_target_ptr), intent(in), target :: targets(:)
    character(*), intent(in) :: module_name
    character(*), intent(in), optional :: include_dir
    type(build_target_t), pointer :: target_ptr

    integer :: k, l

    target_ptr => NULL()

    do k=1,size(targets)

        if (.not.allocated(targets(k)%ptr%source)) cycle

        do l=1,size(targets(k)%ptr%source%modules_provided)

            if (module_name == targets(k)%ptr%source%modules_provided(l)%s) then
                select case(targets(k)%ptr%source%unit_scope)
                case (FPM_SCOPE_LIB, FPM_SCOPE_DEP)
                    target_ptr => targets(k)%ptr
                    exit
                case default
                    if (present(include_dir)) then
                        if (index(dirname(targets(k)%ptr%source%file_name), include_dir) == 1) then ! source file is within the include_dir or a subdirectory
                            target_ptr => targets(k)%ptr
                            exit
                        end if
                    end if
                end select
            end if

        end do

    end do

end function find_module_dependency


!> Perform tree-shaking to remove unused module targets
subroutine prune_build_targets(targets, root_package)

    !> Build target list to prune
    type(build_target_ptr), intent(inout), allocatable :: targets(:)

    !> Name of root package
    character(*), intent(in) :: root_package 

    integer :: i, j, nexec
    type(string_t), allocatable :: modules_used(:)
    logical :: exclude_target(size(targets))
    logical, allocatable :: exclude_from_archive(:)
    
    if (size(targets) < 1) then
        return
    end if

    nexec = 0
    allocate(modules_used(0))

    ! Enumerate modules used by executables, non-module subprograms and their dependencies
    do i=1,size(targets)
            
        if (targets(i)%ptr%target_type == FPM_TARGET_EXECUTABLE) then

            nexec = nexec + 1
            call collect_used_modules(targets(i)%ptr)

        elseif (allocated(targets(i)%ptr%source)) then

            if (targets(i)%ptr%source%unit_type == FPM_UNIT_SUBPROGRAM) then

                call collect_used_modules(targets(i)%ptr)

            end if

        end if

    end do

    ! If there aren't any executables, then prune
    !  based on modules used in root package
    if (nexec < 1) then
        
        do i=1,size(targets)
            
            if (targets(i)%ptr%package_name == root_package .and. &
                 targets(i)%ptr%target_type /= FPM_TARGET_ARCHIVE) then
    
                call collect_used_modules(targets(i)%ptr)
    
            end if
            
        end do

    end if

    call reset_target_flags(targets)

    exclude_target(:) = .false.

    ! Exclude purely module targets if they are not used anywhere
    do i=1,size(targets)
        associate(target=>targets(i)%ptr)

            if (allocated(target%source)) then
                if (target%source%unit_type == FPM_UNIT_MODULE) then

                    exclude_target(i) = .true.
                    target%skip = .true.

                    do j=1,size(target%source%modules_provided)

                        if (target%source%modules_provided(j)%s .in. modules_used) then
                            
                            exclude_target(i) = .false.
                            target%skip = .false.

                        end if 

                    end do

                elseif (target%source%unit_type == FPM_UNIT_SUBMODULE) then
                    ! Remove submodules if their parents are not used

                    exclude_target(i) = .true.
                    target%skip = .true.
                    do j=1,size(target%source%parent_modules)

                        if (target%source%parent_modules(j)%s .in. modules_used) then
                            
                            exclude_target(i) = .false.
                            target%skip = .false.

                        end if 

                    end do

                end if
            end if

            ! (If there aren't any executables then we only prune modules from dependencies)
            if (nexec < 1 .and. target%package_name == root_package) then
                exclude_target(i) = .false.
                target%skip = .false.
            end if

        end associate        
    end do

    targets = pack(targets,.not.exclude_target)

    ! Remove unused targets from archive dependency list
    if (targets(1)%ptr%target_type == FPM_TARGET_ARCHIVE) then
        associate(archive=>targets(1)%ptr)

            allocate(exclude_from_archive(size(archive%dependencies)))
            exclude_from_archive(:) = .false.

            do i=1,size(archive%dependencies)

                if (archive%dependencies(i)%ptr%skip) then

                    exclude_from_archive(i) = .true.

                end if

            end do

            archive%dependencies = pack(archive%dependencies,.not.exclude_from_archive)

        end associate
    end if

    contains

    !> Recursively collect which modules are actually used
    recursive subroutine collect_used_modules(target)
        type(build_target_t), intent(inout) :: target

        integer :: j, k

        if (target%touched) then
            return
        else
            target%touched = .true.
        end if

        if (allocated(target%source)) then

            ! Add modules from this target and from any of it's children submodules
            do j=1,size(target%source%modules_provided)

                if (.not.(target%source%modules_provided(j)%s .in. modules_used)) then

                    modules_used = [modules_used, target%source%modules_provided(j)]

                end if

                ! Recurse into child submodules
                do k=1,size(targets)
                    if (allocated(targets(k)%ptr%source)) then
                        if (targets(k)%ptr%source%unit_type == FPM_UNIT_SUBMODULE) then
                            if (target%source%modules_provided(j)%s .in. targets(k)%ptr%source%parent_modules) then
                                call collect_used_modules(targets(k)%ptr)
                            end if
                        end if
                    end if
                end do

            end do
        end if

        ! Recurse into dependencies
        do j=1,size(target%dependencies)

            if (target%dependencies(j)%ptr%target_type /= FPM_TARGET_ARCHIVE) then
                call collect_used_modules(target%dependencies(j)%ptr)
            end if

        end do

    end subroutine collect_used_modules

    !> Reset target flags after recursive search
    subroutine reset_target_flags(targets)
        type(build_target_ptr), intent(inout) :: targets(:)

        integer :: i

        do i=1,size(targets)

            targets(i)%ptr%touched = .false.

        end do

    end subroutine reset_target_flags

end subroutine prune_build_targets


!> Construct the linker flags string for each target
!>  `target%link_flags` includes non-library objects and library flags
!>
subroutine resolve_target_linking(targets, model)
    type(build_target_ptr), intent(inout), target :: targets(:)
    type(fpm_model_t), intent(in) :: model

    integer :: i
    character(:), allocatable :: global_link_flags, local_link_flags
    character(:), allocatable :: global_include_flags

    if (size(targets) == 0) return

    global_link_flags = ""
    if (allocated(model%link_libraries)) then
        if (size(model%link_libraries) > 0) then
            global_link_flags = model%compiler%enumerate_libraries(global_link_flags, model%link_libraries)
        end if
    end if

    allocate(character(0) :: global_include_flags)
    if (allocated(model%include_dirs)) then
        if (size(model%include_dirs) > 0) then
            global_include_flags = global_include_flags // &
            & " -I" // string_cat(model%include_dirs," -I")
        end if
    end if

    do i=1,size(targets)

        associate(target => targets(i)%ptr)
            if (target%target_type /= FPM_TARGET_C_OBJECT .and. target%target_type /= FPM_TARGET_CPP_OBJECT) then
                target%compile_flags = model%fortran_compile_flags
            else if (target%target_type == FPM_TARGET_C_OBJECT) then
                target%compile_flags = model%c_compile_flags
            else if(target%target_type == FPM_TARGET_CPP_OBJECT) then
                target%compile_flags = model%cxx_compile_flags
            end if

            !> Get macros as flags.
            target%compile_flags = target%compile_flags // get_macros(model%compiler%id, &
                                                            target%macros, &
                                                            target%version)
 
            if (len(global_include_flags) > 0) then
                target%compile_flags = target%compile_flags//global_include_flags
            end if
            target%output_dir = get_output_dir(model%build_prefix, target%compile_flags)
            target%output_file = join_path(target%output_dir, target%output_name)
            target%output_log_file = join_path(target%output_dir, target%output_name)//'.log'
        end associate

    end do

    call add_include_build_dirs(model, targets)

    do i=1,size(targets)

        associate(target => targets(i)%ptr)
            allocate(target%link_objects(0))

            if (target%target_type == FPM_TARGET_ARCHIVE) then
                global_link_flags = target%output_file // global_link_flags

                call get_link_objects(target%link_objects,target,is_exe=.false.)

                allocate(character(0) :: target%link_flags)

            else if (target%target_type == FPM_TARGET_EXECUTABLE) then

                call get_link_objects(target%link_objects,target,is_exe=.true.)

                local_link_flags = model%link_flags
                target%link_flags = model%link_flags//" "//string_cat(target%link_objects," ")

                if (allocated(target%link_libraries)) then
                    if (size(target%link_libraries) > 0) then
                        target%link_flags = model%compiler%enumerate_libraries(target%link_flags, target%link_libraries)
                        local_link_flags = model%compiler%enumerate_libraries(local_link_flags, target%link_libraries)
                    end if
                end if

                target%link_flags = target%link_flags//" "//global_link_flags

                target%output_dir = get_output_dir(model%build_prefix, &
                   & target%compile_flags//local_link_flags)
                target%output_file = join_path(target%output_dir, target%output_name)
                target%output_log_file = join_path(target%output_dir, target%output_name)//'.log'
        end if

        end associate

    end do

contains

    !> Wrapper to build link object list
    !>
    !>  For libraries: just list dependency objects of lib target
    !>
    !>  For executables: need to recursively discover non-library
    !>   dependency objects. (i.e. modules in same dir as program)
    !>
    recursive subroutine get_link_objects(link_objects,target,is_exe)
        type(string_t), intent(inout), allocatable :: link_objects(:)
        type(build_target_t), intent(in) :: target
        logical, intent(in) :: is_exe

        integer :: i
        type(string_t) :: temp_str

        if (.not.allocated(target%dependencies)) return

        do i=1,size(target%dependencies)

            associate(dep => target%dependencies(i)%ptr)

                if (.not.allocated(dep%source)) cycle

                ! Skip library dependencies for executable targets
                !  since the library archive will always be linked
                if (is_exe.and.(dep%source%unit_scope == FPM_SCOPE_LIB)) cycle

                ! Skip if dependency object already listed
                if (dep%output_file .in. link_objects) cycle

                ! Add dependency object file to link object list
                temp_str%s = dep%output_file
                link_objects = [link_objects, temp_str]

                ! For executable objects, also need to include non-library
                !  dependencies from dependencies (recurse)
                if (is_exe) call get_link_objects(link_objects,dep,is_exe=.true.)

            end associate

        end do

    end subroutine get_link_objects

end subroutine resolve_target_linking


subroutine add_include_build_dirs(model, targets)
    type(fpm_model_t), intent(in) :: model
    type(build_target_ptr), intent(inout), target :: targets(:)

    integer :: i
    type(string_t), allocatable :: build_dirs(:)
    type(string_t) :: temp

    allocate(build_dirs(0))
    do i = 1, size(targets)
        associate(target => targets(i)%ptr)
            if (target%target_type /= FPM_TARGET_OBJECT) cycle
            if (target%output_dir .in. build_dirs) cycle
            temp%s = target%output_dir
            build_dirs = [build_dirs, temp]
        end associate
    end do

    do i = 1, size(targets)
        associate(target => targets(i)%ptr)
            if (target%target_type /= FPM_TARGET_OBJECT) cycle

            target%compile_flags = target%compile_flags // &
                " " // model%compiler%get_module_flag(target%output_dir) // &
                " -I" // string_cat(build_dirs, " -I")
        end associate
    end do

end subroutine add_include_build_dirs


function get_output_dir(build_prefix, args) result(path)
    character(len=*), intent(in) :: build_prefix
    character(len=*), intent(in) :: args
    character(len=:), allocatable :: path

    character(len=16) :: build_hash

    write(build_hash, '(z16.16)') fnv_1a(args)
    path = build_prefix//"_"//build_hash
end function get_output_dir


subroutine filter_library_targets(targets, list)
    type(build_target_ptr), intent(in) :: targets(:)
    type(string_t), allocatable, intent(out) :: list(:)

    integer :: i, n

    n = 0
    call resize(list)
    do i = 1, size(targets)
        if (targets(i)%ptr%target_type == FPM_TARGET_ARCHIVE) then
            if (n >= size(list)) call resize(list)
            n = n + 1
            list(n)%s = targets(i)%ptr%output_file
        end if
    end do
    call resize(list, n)
end subroutine filter_library_targets

subroutine filter_executable_targets(targets, scope, list)
    type(build_target_ptr), intent(in) :: targets(:)
    integer, intent(in) :: scope
    type(string_t), allocatable, intent(out) :: list(:)

    integer :: i, n

    n = 0
    call resize(list)
    do i = 1, size(targets)
        if (is_executable_target(targets(i)%ptr, scope)) then
            if (n >= size(list)) call resize(list)
            n = n + 1
            list(n)%s = targets(i)%ptr%output_file
        end if
    end do
    call resize(list, n)
end subroutine filter_executable_targets


elemental function is_executable_target(target_ptr, scope) result(is_exe)
    type(build_target_t), intent(in) :: target_ptr
    integer, intent(in) :: scope
    logical :: is_exe
    is_exe = target_ptr%target_type == FPM_TARGET_EXECUTABLE .and. &
        allocated(target_ptr%dependencies)
    if (is_exe) then
        is_exe = target_ptr%dependencies(1)%ptr%source%unit_scope == scope
    end if
end function is_executable_target


subroutine filter_modules(targets, list)
    type(build_target_ptr), intent(in) :: targets(:)
    type(string_t), allocatable, intent(out) :: list(:)

    integer :: i, j, n

    n = 0
    call resize(list)
    do i = 1, size(targets)
        associate(target => targets(i)%ptr)
            if (.not.allocated(target%source)) cycle
            if (target%source%unit_type == FPM_UNIT_SUBMODULE) cycle
            if (n + size(target%source%modules_provided) >= size(list)) call resize(list)
            do j = 1, size(target%source%modules_provided)
                n = n + 1
                list(n)%s = join_path(target%output_dir, &
                    target%source%modules_provided(j)%s)
            end do
        end associate
    end do
    call resize(list, n)
end subroutine filter_modules


end module fpm_targets
 
 
!>>>>> ././src/fpm_backend_output.f90
!># Build Backend Progress Output
!> This module provides a derived type `build_progress_t` for printing build status
!> and progress messages to the console while the backend is building the package.
!>
!> The `build_progress_t` type supports two modes: `normal` and `plain`
!> where the former does 'pretty' output and the latter does not.
!> The `normal` mode is intended for typical interactive usage whereas
!> 'plain' mode is used with the `--verbose` flag or when `stdout` is not attached
!> to a terminal (e.g. when piping or redirecting `stdout`). In these cases,
!> the pretty output must be suppressed to avoid control codes being output.

module fpm_backend_output
use iso_fortran_env, only: stdout=>output_unit
use fpm_filesystem, only: basename
use fpm_targets, only: build_target_ptr
use fpm_backend_console, only: console_t, LINE_RESET, COLOR_RED, COLOR_GREEN, COLOR_YELLOW, COLOR_RESET
implicit none

private
public build_progress_t

!> Build progress object
type build_progress_t
    !> Console object for updating console lines
    type(console_t) :: console
    !> Number of completed targets
    integer :: n_complete
    !> Total number of targets scheduled
    integer :: n_target
    !> 'Plain' output (no colors or updating)
    logical :: plain_mode = .true.
    !> Store needed when updating previous console lines
    integer, allocatable :: output_lines(:)
    !> Queue of scheduled build targets
    type(build_target_ptr), pointer :: target_queue(:)
contains
    !> Output 'compiling' status for build target
    procedure :: compiling_status => output_status_compiling
    !> Output 'complete' status for build target
    procedure :: completed_status => output_status_complete
    !> Output finished status for whole package
    procedure :: success => output_progress_success
end type build_progress_t

!> Constructor for build_progress_t
interface build_progress_t
    procedure :: new_build_progress
end interface build_progress_t

contains
    
    !> Initialise a new build progress object
    function new_build_progress(target_queue,plain_mode) result(progress)
        !> The queue of scheduled targets
        type(build_target_ptr), intent(in), target :: target_queue(:)
        !> Enable 'plain' output for progress object
        logical, intent(in), optional :: plain_mode
        !> Progress object to initialise
        type(build_progress_t) :: progress

        progress%n_target = size(target_queue,1)
        progress%target_queue => target_queue
        progress%plain_mode = plain_mode
        progress%n_complete = 0

        allocate(progress%output_lines(progress%n_target))

    end function new_build_progress

    !> Output 'compiling' status for build target and overall percentage progress
    subroutine output_status_compiling(progress, queue_index)
        !> Progress object
        class(build_progress_t), intent(inout) :: progress
        !> Index of build target in the target queue
        integer, intent(in) :: queue_index

        character(:), allocatable :: target_name
        character(100) :: output_string
        character(7) :: overall_progress

        associate(target=>progress%target_queue(queue_index)%ptr)

            if (allocated(target%source)) then
                target_name = basename(target%source%file_name)
            else
                target_name = basename(target%output_file)
            end if

            write(overall_progress,'(A,I3,A)') '[',100*progress%n_complete/progress%n_target,'%] '

            if (progress%plain_mode) then ! Plain output

                !$omp critical
                write(*,'(A7,A30)') overall_progress,target_name
                !$omp end critical

            else ! Pretty output

                write(output_string,'(A,T40,A,A)') target_name, COLOR_YELLOW//'compiling...'//COLOR_RESET

                call progress%console%write_line(trim(output_string),progress%output_lines(queue_index))

                call progress%console%write_line(overall_progress//'Compiling...',advance=.false.)

            end if

        end associate

    end subroutine output_status_compiling

    !> Output 'complete' status for build target and update overall percentage progress
    subroutine output_status_complete(progress, queue_index, build_stat)
        !> Progress object
        class(build_progress_t), intent(inout) :: progress
        !> Index of build target in the target queue
        integer, intent(in) :: queue_index
        !> Build status flag
        integer, intent(in) :: build_stat

        character(:), allocatable :: target_name
        character(100) :: output_string
        character(7) :: overall_progress

        !$omp critical 
        progress%n_complete = progress%n_complete + 1
        !$omp end critical

        associate(target=>progress%target_queue(queue_index)%ptr)

            if (allocated(target%source)) then
                target_name = basename(target%source%file_name)
            else
                target_name = basename(target%output_file)
            end if

            if (build_stat == 0) then
                write(output_string,'(A,T40,A,A)') target_name,COLOR_GREEN//'done.'//COLOR_RESET
            else
                write(output_string,'(A,T40,A,A)') target_name,COLOR_RED//'failed.'//COLOR_RESET
            end if

            write(overall_progress,'(A,I3,A)') '[',100*progress%n_complete/progress%n_target,'%] '

            if (progress%plain_mode) then  ! Plain output

                !$omp critical
                write(*,'(A7,A30,A7)') overall_progress,target_name, 'done.'
                !$omp end critical

            else ! Pretty output

                call progress%console%update_line(progress%output_lines(queue_index),trim(output_string))

                call progress%console%write_line(overall_progress//'Compiling...',advance=.false.)

            end if

        end associate

    end subroutine output_status_complete

    !> Output finished status for whole package
    subroutine output_progress_success(progress)
        class(build_progress_t), intent(inout) :: progress

        if (progress%plain_mode) then ! Plain output

            write(*,'(A)') '[100%] Project compiled successfully.'

        else ! Pretty output

            write(*,'(A)') LINE_RESET//COLOR_GREEN//'[100%] Project compiled successfully.'//COLOR_RESET

        end if

    end subroutine output_progress_success

end module fpm_backend_output 
 
!>>>>> ././src/fpm_sources.f90
!># Discovery of sources
!>
!> This module implements subroutines for building a list of
!> `[[srcfile_t]]` objects by looking for source files in the filesystem.
!>
module fpm_sources
use fpm_error, only: error_t
use fpm_model, only: srcfile_t, FPM_UNIT_PROGRAM
use fpm_filesystem, only: basename, canon_path, dirname, join_path, list_files, is_hidden_file
use fpm_strings, only: lower, str_ends_with, string_t, operator(.in.)
use fpm_source_parsing, only: parse_f_source, parse_c_source
use fpm_manifest_executable, only: executable_config_t
implicit none

private
public :: add_sources_from_dir, add_executable_sources

character(4), parameter :: fortran_suffixes(2) = [".f90", &
                                                  ".f  "]
character(4), parameter :: c_suffixes(4) = [".c  ", ".h  ", ".cpp", ".hpp"]

contains

!> Wrapper to source parsing routines.
!> Selects parsing routine based on source file name extension
function parse_source(source_file_path,error) result(source)
    character(*), intent(in) :: source_file_path
    type(error_t), allocatable, intent(out) :: error
    type(srcfile_t)  :: source

    if (str_ends_with(lower(source_file_path), fortran_suffixes)) then

        source = parse_f_source(source_file_path, error)

        if (source%unit_type == FPM_UNIT_PROGRAM) then
            source%exe_name = basename(source_file_path,suffix=.false.)
        end if

    else if (str_ends_with(lower(source_file_path), c_suffixes)) then

        source = parse_c_source(source_file_path,error)

    end if

    if (allocated(error)) then
        return
    end if

end function parse_source

!> Add to `sources` by looking for source files in `directory`
subroutine add_sources_from_dir(sources,directory,scope,with_executables,recurse,error)
    !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> Directory in which to search for source files
    character(*), intent(in) :: directory
    !> Scope to apply to the discovered sources, see [[fpm_model]] for enumeration
    integer, intent(in) :: scope
    !> Executable sources (fortran `program`s) are ignored unless `with_executables=.true.`
    logical, intent(in), optional :: with_executables
    !> Whether to recursively search subdirectories, default is `.true.`
    logical, intent(in), optional :: recurse
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: i
    logical, allocatable :: is_source(:), exclude_source(:)
    logical :: recurse_
    type(string_t), allocatable :: file_names(:)
    type(string_t), allocatable :: src_file_names(:)
    type(string_t), allocatable :: existing_src_files(:)
    type(srcfile_t), allocatable :: dir_sources(:)

    recurse_ = .true.
    if (present(recurse)) recurse_ = recurse
    ! Scan directory for sources
    call list_files(directory, file_names,recurse=recurse_)

    if (allocated(sources)) then
        allocate(existing_src_files(size(sources)))
        do i=1,size(sources)
            existing_src_files(i)%s = canon_path(sources(i)%file_name)
        end do
    else
        allocate(existing_src_files(0))
    end if

    is_source = [(.not.(is_hidden_file(basename(file_names(i)%s))) .and. &
                 .not.(canon_path(file_names(i)%s) .in. existing_src_files) .and. &
                 (str_ends_with(lower(file_names(i)%s), fortran_suffixes) .or. &
                 str_ends_with(lower(file_names(i)%s), c_suffixes) ),i=1,size(file_names))]
    src_file_names = pack(file_names,is_source)

    allocate(dir_sources(size(src_file_names)))
    allocate(exclude_source(size(src_file_names)))

    do i = 1, size(src_file_names)

        dir_sources(i) = parse_source(src_file_names(i)%s,error)
        if (allocated(error)) return

        dir_sources(i)%unit_scope = scope
        allocate(dir_sources(i)%link_libraries(0))

        ! Exclude executables unless specified otherwise
        exclude_source(i) = (dir_sources(i)%unit_type == FPM_UNIT_PROGRAM)
        if (dir_sources(i)%unit_type == FPM_UNIT_PROGRAM .and. &
            & present(with_executables)) then
            if (with_executables) then

                exclude_source(i) = .false.

            end if
        end if

    end do

    if (.not.allocated(sources)) then
        sources = pack(dir_sources,.not.exclude_source)
    else
        sources = [sources, pack(dir_sources,.not.exclude_source)]
    end if

end subroutine add_sources_from_dir


!> Add to `sources` using the executable and test entries in the manifest and
!> applies any executable-specific overrides such as `executable%name`.
!> Adds all sources (including modules) from each `executable%source_dir`
subroutine add_executable_sources(sources,executables,scope,auto_discover,error)
    !> List of `[[srcfile_t]]` objects to append to. Allocated if not allocated
    type(srcfile_t), allocatable, intent(inout), target :: sources(:)
    !> List of `[[executable_config_t]]` entries from manifest
    class(executable_config_t), intent(in) :: executables(:)
    !> Scope to apply to the discovered sources: either `FPM_SCOPE_APP` or `FPM_SCOPE_TEST`, see [[fpm_model]]
    integer, intent(in) :: scope
    !> If `.false.` only executables and tests specified in the manifest are added to `sources`
    logical, intent(in) :: auto_discover
    !> Error handling
    type(error_t), allocatable, intent(out) :: error

    integer :: i, j

    type(string_t), allocatable :: exe_dirs(:)
    type(srcfile_t) :: exe_source

    call get_executable_source_dirs(exe_dirs,executables)

    do i=1,size(exe_dirs)
        call add_sources_from_dir(sources,exe_dirs(i)%s, scope, &
                     with_executables=auto_discover, recurse=.false., error=error)

        if (allocated(error)) then
            return
        end if
    end do

    exe_loop: do i=1,size(executables)

        ! Check if executable already discovered automatically
        !  and apply any overrides
        do j=1,size(sources)

            if (basename(sources(j)%file_name,suffix=.true.) == executables(i)%main .and.&
                 canon_path(dirname(sources(j)%file_name)) == &
                 canon_path(executables(i)%source_dir) ) then

                sources(j)%exe_name = executables(i)%name
                if (allocated(executables(i)%link)) then
                    sources(j)%link_libraries = executables(i)%link
                end if
                sources(j)%unit_type = FPM_UNIT_PROGRAM
                cycle exe_loop

            end if

        end do

        ! Add if not already discovered (auto_discovery off)
        associate(exe => executables(i))
            exe_source = parse_source(join_path(exe%source_dir,exe%main),error)
            exe_source%exe_name = exe%name
            if (allocated(exe%link)) then
                exe_source%link_libraries = exe%link
            end if
            exe_source%unit_type = FPM_UNIT_PROGRAM
            exe_source%unit_scope = scope
        end associate

        if (allocated(error)) return

        if (.not.allocated(sources)) then
            sources = [exe_source]
        else
            sources = [sources, exe_source]
        end if

    end do exe_loop

end subroutine add_executable_sources

!> Build a list of unique source directories
!>  from executables specified in manifest
subroutine get_executable_source_dirs(exe_dirs,executables)
    type(string_t), allocatable, intent(inout) :: exe_dirs(:)
    class(executable_config_t), intent(in) :: executables(:)

    type(string_t) :: dirs_temp(size(executables))

    integer :: i, n

    n = 0

    do i=1,size(executables)
       dirs_temp(i)%s=' '
    enddo

    do i=1,size(executables)
        if (.not.(executables(i)%source_dir .in. dirs_temp)) then

            n = n + 1
            dirs_temp(n)%s = executables(i)%source_dir

        end if
    end do

    if (.not.allocated(exe_dirs)) then
        exe_dirs = dirs_temp(1:n)
    else
        exe_dirs = [exe_dirs,dirs_temp(1:n)]
    end if

end subroutine get_executable_source_dirs

end module fpm_sources
 
 
!>>>>> ././src/fpm_backend.F90
!># Build backend
!> Uses a list of `[[build_target_ptr]]` and a valid `[[fpm_model]]` instance
!> to schedule and execute the compilation and linking of package targets.
!>
!> The package build process (`[[build_package]]`) comprises three steps:
!>
!> 1. __Target sorting:__ topological sort of the target dependency graph (`[[sort_target]]`)
!> 2. __Target scheduling:__ group targets into schedule regions based on the sorting (`[[schedule_targets]]`)
!> 3. __Target building:__ generate targets by compilation or linking
!>
!> @note If compiled with OpenMP, targets will be build in parallel where possible.
!>
!>### Incremental compilation
!> The backend process supports *incremental* compilation whereby targets are not
!> re-compiled if their corresponding dependencies have not been modified.
!>
!> - Source-based targets (*i.e.* objects) are not re-compiled if the corresponding source
!>   file is unmodified AND all of the target dependencies are not marked for re-compilation
!>
!> - Link targets (*i.e.* executables and libraries) are not re-compiled if the
!>   target output file already exists AND all of the target dependencies are not marked for
!>   re-compilation
!>
!> Source file modification is determined by a file digest (hash) which is calculated during
!> the source parsing phase ([[fpm_source_parsing]]) and cached to disk after a target is
!> successfully generated.
!>
module fpm_backend

use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use fpm_error, only : fpm_stop
use fpm_filesystem, only: basename, dirname, join_path, exists, mkdir, run, getline
use fpm_model, only: fpm_model_t
use fpm_strings, only: string_t, operator(.in.)
use fpm_targets, only: build_target_t, build_target_ptr, FPM_TARGET_OBJECT, &
                       FPM_TARGET_C_OBJECT, FPM_TARGET_ARCHIVE, FPM_TARGET_EXECUTABLE, &
                       FPM_TARGET_CPP_OBJECT
use fpm_backend_output
implicit none

private
public :: build_package, sort_target, schedule_targets

#ifndef FPM_BOOTSTRAP
interface
    function c_isatty() bind(C, name = 'c_isatty')
        use, intrinsic :: iso_c_binding, only: c_int
        integer(c_int) :: c_isatty
    end function
end interface
#endif

contains

!> Top-level routine to build package described by `model`
subroutine build_package(targets,model,verbose)
    type(build_target_ptr), intent(inout) :: targets(:)
    type(fpm_model_t), intent(in) :: model
    logical, intent(in) :: verbose

    integer :: i, j
    type(build_target_ptr), allocatable :: queue(:)
    integer, allocatable :: schedule_ptr(:), stat(:)
    logical :: build_failed, skip_current
    type(string_t), allocatable :: build_dirs(:)
    type(string_t) :: temp

    type(build_progress_t) :: progress
    logical :: plain_output

    ! Need to make output directory for include (mod) files
    allocate(build_dirs(0))
    do i = 1, size(targets)
       associate(target => targets(i)%ptr)
          if (target%output_dir .in. build_dirs) cycle
          temp%s = target%output_dir
          build_dirs = [build_dirs, temp]
       end associate
    end do

    do i = 1, size(build_dirs)
       call mkdir(build_dirs(i)%s,verbose)
    end do

    ! Perform depth-first topological sort of targets
    do i=1,size(targets)

        call sort_target(targets(i)%ptr)

    end do

    ! Construct build schedule queue
    call schedule_targets(queue, schedule_ptr, targets)

    ! Check if queue is empty
    if (.not.verbose .and. size(queue) < 1) then
        write(stderr, '(a)') 'Project is up to date'
        return
    end if

    ! Initialise build status flags
    allocate(stat(size(queue)))
    stat(:) = 0
    build_failed = .false.

    ! Set output mode
#ifndef FPM_BOOTSTRAP
    plain_output = (.not.(c_isatty()==1)) .or. verbose
#else
    plain_output = .true.
#endif

    progress = build_progress_t(queue,plain_output)

    ! Loop over parallel schedule regions
    do i=1,size(schedule_ptr)-1

        ! Build targets in schedule region i
        !$omp parallel do default(shared) private(skip_current) schedule(dynamic,1)
        do j=schedule_ptr(i),(schedule_ptr(i+1)-1)

            ! Check if build already failed
            !$omp atomic read
            skip_current = build_failed

            if (.not.skip_current) then
                call progress%compiling_status(j)
                call build_target(model,queue(j)%ptr,verbose,stat(j))
                call progress%completed_status(j,stat(j))
            end if

            ! Set global flag if this target failed to build
            if (stat(j) /= 0) then
                !$omp atomic write
                build_failed = .true.
            end if

        end do

        ! Check if this schedule region failed: exit with message if failed
        if (build_failed) then
            write(*,*)
            do j=1,size(stat)
                if (stat(j) /= 0) Then
                    call print_build_log(queue(j)%ptr)
                end if
            end do
            do j=1,size(stat)
                if (stat(j) /= 0) then
                    write(stderr,'(*(g0:,1x))') '<ERROR> Compilation failed for object "',basename(queue(j)%ptr%output_file),'"'
                end if
            end do
            call fpm_stop(1,'stopping due to failed compilation')
        end if

    end do

    call progress%success()

end subroutine build_package


!> Topologically sort a target for scheduling by
!>  recursing over its dependencies.
!>
!> Checks disk-cached source hashes to determine if objects are
!>  up-to-date. Up-to-date sources are tagged as skipped.
!>
!> On completion, `target` should either be marked as
!> sorted (`target%sorted=.true.`) or skipped (`target%skip=.true.`)
!>
!> If `target` is marked as sorted, `target%schedule` should be an
!> integer greater than zero indicating the region for scheduling
!>
recursive subroutine sort_target(target)
    type(build_target_t), intent(inout), target :: target

    integer :: i, fh, stat

    ! Check if target has already been processed (as a dependency)
    if (target%sorted .or. target%skip) then
        return
    end if

    ! Check for a circular dependency
    ! (If target has been touched but not processed)
    if (target%touched) then
        call fpm_stop(1,'(!) Circular dependency found with: '//target%output_file)
    else
        target%touched = .true.  ! Set touched flag
    end if

    ! Load cached source file digest if present
    if (.not.allocated(target%digest_cached) .and. &
         exists(target%output_file) .and. &
         exists(target%output_file//'.digest')) then

        allocate(target%digest_cached)
        open(newunit=fh,file=target%output_file//'.digest',status='old')
        read(fh,*,iostat=stat) target%digest_cached
        close(fh)

        if (stat /= 0) then    ! Cached digest is not recognized
            deallocate(target%digest_cached)
        end if

    end if

    if (allocated(target%source)) then

        ! Skip if target is source-based and source file is unmodified
        if (allocated(target%digest_cached)) then
            if (target%digest_cached == target%source%digest) target%skip = .true.
        end if

    elseif (exists(target%output_file)) then

        ! Skip if target is not source-based and already exists
        target%skip = .true.

    end if

    ! Loop over target dependencies
    target%schedule = 1
    do i=1,size(target%dependencies)

        ! Sort dependency
        call sort_target(target%dependencies(i)%ptr)

        if (.not.target%dependencies(i)%ptr%skip) then

            ! Can't skip target if any dependency is not skipped
            target%skip = .false.

            ! Set target schedule after all of its dependencies
            target%schedule = max(target%schedule,target%dependencies(i)%ptr%schedule+1)

        end if

    end do

    ! Mark flag as processed: either sorted or skipped
    target%sorted = .not.target%skip

end subroutine sort_target


!> Construct a build schedule from the sorted targets.
!>
!> The schedule is broken into regions, described by `schedule_ptr`,
!>  where targets in each region can be compiled in parallel.
!>
subroutine schedule_targets(queue, schedule_ptr, targets)
    type(build_target_ptr), allocatable, intent(out) :: queue(:)
    integer, allocatable :: schedule_ptr(:)
    type(build_target_ptr), intent(in) :: targets(:)

    integer :: i, j
    integer :: n_schedule, n_sorted

    n_schedule = 0   ! Number of schedule regions
    n_sorted = 0     ! Total number of targets to build
    do i=1,size(targets)

        if (targets(i)%ptr%sorted) then
            n_sorted = n_sorted + 1
        end if
        n_schedule = max(n_schedule, targets(i)%ptr%schedule)

    end do

    allocate(queue(n_sorted))
    allocate(schedule_ptr(n_schedule+1))

    ! Construct the target queue and schedule region pointer
    n_sorted = 1
    schedule_ptr(n_sorted) = 1
    do i=1,n_schedule

        do j=1,size(targets)

            if (targets(j)%ptr%sorted) then
                if (targets(j)%ptr%schedule == i) then

                    queue(n_sorted)%ptr => targets(j)%ptr
                    n_sorted = n_sorted + 1
                end if
            end if

        end do

        schedule_ptr(i+1) = n_sorted

    end do

end subroutine schedule_targets


!> Call compile/link command for a single target.
!>
!> If successful, also caches the source file digest to disk.
!>
subroutine build_target(model,target,verbose,stat)
    type(fpm_model_t), intent(in) :: model
    type(build_target_t), intent(in), target :: target
    logical, intent(in) :: verbose
    integer, intent(out) :: stat

    integer :: fh

    !$omp critical
    if (.not.exists(dirname(target%output_file))) then
        call mkdir(dirname(target%output_file),verbose)
    end if
    !$omp end critical

    select case(target%target_type)

    case (FPM_TARGET_OBJECT)
        call model%compiler%compile_fortran(target%source%file_name, target%output_file, &
            & target%compile_flags, target%output_log_file, stat)

    case (FPM_TARGET_C_OBJECT)
        call model%compiler%compile_c(target%source%file_name, target%output_file, &
            & target%compile_flags, target%output_log_file, stat)

    case (FPM_TARGET_CPP_OBJECT)
        call model%compiler%compile_cpp(target%source%file_name, target%output_file, &
            & target%compile_flags, target%output_log_file, stat)

    case (FPM_TARGET_EXECUTABLE)
        call model%compiler%link(target%output_file, &
            & target%compile_flags//" "//target%link_flags, target%output_log_file, stat)

    case (FPM_TARGET_ARCHIVE)
        call model%archiver%make_archive(target%output_file, target%link_objects, &
            & target%output_log_file, stat)

    end select

    if (stat == 0 .and. allocated(target%source)) then
        open(newunit=fh,file=target%output_file//'.digest',status='unknown')
        write(fh,*) target%source%digest
        close(fh)
    end if

end subroutine build_target


!> Read and print the build log for target
!>
subroutine print_build_log(target)
    type(build_target_t), intent(in), target :: target

    integer :: fh, ios
    character(:), allocatable :: line

    if (exists(target%output_log_file)) then

        open(newunit=fh,file=target%output_log_file,status='old')
        do
            call getline(fh, line, ios)
            if (ios /= 0) exit
            write(*,'(A)') trim(line)
        end do
        close(fh)

    else

        write(stderr,'(*(g0:,1x))') '<ERROR> Unable to find build log "',basename(target%output_log_file),'"'

    end if

end subroutine print_build_log

end module fpm_backend
 
 
!>>>>> ././src/fpm.f90
module fpm
use fpm_strings, only: string_t, operator(.in.), glob, join, string_cat, &
                      lower, str_ends_with
use fpm_backend, only: build_package
use fpm_command_line, only: fpm_build_settings, fpm_new_settings, &
                      fpm_run_settings, fpm_install_settings, fpm_test_settings, &
                      fpm_clean_settings
use fpm_dependency, only : new_dependency_tree
use fpm_environment, only: get_env
use fpm_filesystem, only: is_dir, join_path, list_files, exists, &
                   basename, filewrite, mkdir, run, os_delete_dir
use fpm_model, only: fpm_model_t, srcfile_t, show_model, &
                    FPM_SCOPE_UNKNOWN, FPM_SCOPE_LIB, FPM_SCOPE_DEP, &
                    FPM_SCOPE_APP, FPM_SCOPE_EXAMPLE, FPM_SCOPE_TEST
use fpm_compiler, only: new_compiler, new_archiver, set_cpp_preprocessor_flags


use fpm_sources, only: add_executable_sources, add_sources_from_dir
use fpm_targets, only: targets_from_sources, &
                        resolve_target_linking, build_target_t, build_target_ptr, &
                        FPM_TARGET_EXECUTABLE, FPM_TARGET_ARCHIVE
use fpm_manifest, only : get_package_data, package_config_t
use fpm_error, only : error_t, fatal_error, fpm_stop
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,   &
                                       & stdout=>output_unit, &
                                       & stderr=>error_unit
use iso_c_binding, only: c_char, c_ptr, c_int, c_null_char, c_associated, c_f_pointer
implicit none
private
public :: cmd_build, cmd_run, cmd_clean
public :: build_model, check_modules_for_duplicates

contains


subroutine build_model(model, settings, package, error)
    ! Constructs a valid fpm model from command line settings and toml manifest
    !
    type(fpm_model_t), intent(out) :: model
    type(fpm_build_settings), intent(in) :: settings
    type(package_config_t), intent(in) :: package
    type(error_t), allocatable, intent(out) :: error

    integer :: i, j
    type(package_config_t) :: dependency
    character(len=:), allocatable :: manifest, lib_dir, flags, cflags, cxxflags, ldflags
    character(len=:), allocatable :: version
    logical :: has_cpp

    logical :: duplicates_found = .false.
    type(string_t) :: include_dir

    model%package_name = package%name

    allocate(model%include_dirs(0))
    allocate(model%link_libraries(0))
    allocate(model%external_modules(0))

    call new_dependency_tree(model%deps, cache=join_path("build", "cache.toml"))
    call model%deps%add(package, error)
    if (allocated(error)) return

    ! build/ directory should now exist
    if (.not.exists("build/.gitignore")) then
      call filewrite(join_path("build", ".gitignore"),["*"])
    end if

    call new_compiler(model%compiler, settings%compiler, settings%c_compiler, &
        & settings%cxx_compiler, echo=settings%verbose, verbose=settings%verbose)
    call new_archiver(model%archiver, settings%archiver, &
        & echo=settings%verbose, verbose=settings%verbose)

    if (settings%flag == '') then
        flags = model%compiler%get_default_flags(settings%profile == "release")
    else
        flags = settings%flag
        select case(settings%profile)
        case("release", "debug")
            flags = flags // model%compiler%get_default_flags(settings%profile == "release")
        end select
    end if

    cflags = trim(settings%cflag)
    cxxflags = trim(settings%cxxflag)
    ldflags = trim(settings%ldflag)

    if (model%compiler%is_unknown()) then
        write(*, '(*(a:,1x))') &
            "<WARN>", "Unknown compiler", model%compiler%fc, "requested!", &
            "Defaults for this compiler might be incorrect"
    end if
    model%build_prefix = join_path("build", basename(model%compiler%fc))

    model%include_tests = settings%build_tests

    allocate(model%packages(model%deps%ndep))

    has_cpp = .false.
    do i = 1, model%deps%ndep
        associate(dep => model%deps%dep(i))
            manifest = join_path(dep%proj_dir, "fpm.toml")

            call get_package_data(dependency, manifest, error, &
                apply_defaults=.true.)
            if (allocated(error)) exit

            model%packages(i)%name = dependency%name
            call package%version%to_string(version)
            model%packages(i)%version = version
            
            if (allocated(dependency%preprocess)) then
                do j = 1, size(dependency%preprocess)
                    if (dependency%preprocess(j)%name == "cpp") then
                        if (.not. has_cpp) has_cpp = .true.
                        if (allocated(dependency%preprocess(j)%macros)) then
                        model%packages(i)%macros = dependency%preprocess(j)%macros
                        end if
                    else
                        write(stderr, '(a)') 'Warning: Preprocessor ' // package%preprocess(i)%name // &
                            ' is not supported; will ignore it'
                    end if
                end do
            end if

            if (.not.allocated(model%packages(i)%sources)) allocate(model%packages(i)%sources(0))

            if (allocated(dependency%library)) then

                if (allocated(dependency%library%source_dir)) then
                    lib_dir = join_path(dep%proj_dir, dependency%library%source_dir)
                    if (is_dir(lib_dir)) then
                        call add_sources_from_dir(model%packages(i)%sources, lib_dir, FPM_SCOPE_LIB, &
                            error=error)
                        if (allocated(error)) exit
                    end if
                end if

                if (allocated(dependency%library%include_dir)) then
                    do j=1,size(dependency%library%include_dir)
                        include_dir%s = join_path(dep%proj_dir, dependency%library%include_dir(j)%s)
                        if (is_dir(include_dir%s)) then
                            model%include_dirs = [model%include_dirs, include_dir]
                        end if
                    end do
                end if

            end if

            if (allocated(dependency%build%link)) then
                model%link_libraries = [model%link_libraries, dependency%build%link]
            end if

            if (allocated(dependency%build%external_modules)) then
                model%external_modules = [model%external_modules, dependency%build%external_modules]
            end if
        end associate
    end do
    if (allocated(error)) return

    if (has_cpp) call set_cpp_preprocessor_flags(model%compiler%id, flags)
    model%fortran_compile_flags = flags
    model%c_compile_flags = cflags
    model%cxx_compile_flags = cxxflags
    model%link_flags = ldflags

    ! Add sources from executable directories
    if (is_dir('app') .and. package%build%auto_executables) then
        call add_sources_from_dir(model%packages(1)%sources,'app', FPM_SCOPE_APP, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('example') .and. package%build%auto_examples) then
        call add_sources_from_dir(model%packages(1)%sources,'example', FPM_SCOPE_EXAMPLE, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (is_dir('test') .and. package%build%auto_tests) then
        call add_sources_from_dir(model%packages(1)%sources,'test', FPM_SCOPE_TEST, &
                                   with_executables=.true., error=error)

        if (allocated(error)) then
            return
        endif

    end if
    if (allocated(package%executable)) then
        call add_executable_sources(model%packages(1)%sources, package%executable, FPM_SCOPE_APP, &
                                     auto_discover=package%build%auto_executables, &
                                     error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%example)) then
        call add_executable_sources(model%packages(1)%sources, package%example, FPM_SCOPE_EXAMPLE, &
                                     auto_discover=package%build%auto_examples, &
                                     error=error)

        if (allocated(error)) then
            return
        end if

    end if
    if (allocated(package%test)) then
        call add_executable_sources(model%packages(1)%sources, package%test, FPM_SCOPE_TEST, &
                                     auto_discover=package%build%auto_tests, &
                                     error=error)

        if (allocated(error)) then
            return
        endif

    endif


    if (settings%verbose) then
        write(*,*)'<INFO> BUILD_NAME: ',model%build_prefix
        write(*,*)'<INFO> COMPILER:  ',model%compiler%fc
        write(*,*)'<INFO> C COMPILER:  ',model%compiler%cc
        write(*,*)'<INFO> CXX COMPILER: ',model%compiler%cxx
        write(*,*)'<INFO> COMPILER OPTIONS:  ', model%fortran_compile_flags
        write(*,*)'<INFO> C COMPILER OPTIONS:  ', model%c_compile_flags
        write(*,*)'<INFO> CXX COMPILER OPTIONS: ', model%cxx_compile_flags
        write(*,*)'<INFO> LINKER OPTIONS:  ', model%link_flags
        write(*,*)'<INFO> INCLUDE DIRECTORIES:  [', string_cat(model%include_dirs,','),']'
     end if

    ! Check for duplicate modules
    call check_modules_for_duplicates(model, duplicates_found)
    if (duplicates_found) then
        call fpm_stop(1,'*build_model*:Error: One or more duplicate module names found.')
    end if
end subroutine build_model

! Check for duplicate modules
subroutine check_modules_for_duplicates(model, duplicates_found)
    type(fpm_model_t), intent(in) :: model
    integer :: maxsize
    integer :: i,j,k,l,m,modi
    type(string_t), allocatable :: modules(:)
    logical :: duplicates_found
    ! Initialise the size of array
    maxsize = 0
    ! Get number of modules provided by each source file of every package
    do i=1,size(model%packages)
      do j=1,size(model%packages(i)%sources)
        if (allocated(model%packages(i)%sources(j)%modules_provided)) then
          maxsize = maxsize + size(model%packages(i)%sources(j)%modules_provided)
        end if
      end do
    end do
    ! Allocate array to contain distinct names of modules
    allocate(modules(maxsize))

    ! Initialise index to point at start of the newly allocated array
    modi = 1

    ! Loop through modules provided by each source file of every package
    ! Add it to the array if it is not already there
    ! Otherwise print out warning about duplicates
    do k=1,size(model%packages)
      do l=1,size(model%packages(k)%sources)
        if (allocated(model%packages(k)%sources(l)%modules_provided)) then
          do m=1,size(model%packages(k)%sources(l)%modules_provided)
            if (model%packages(k)%sources(l)%modules_provided(m)%s.in.modules(:modi-1)) then
              write(stderr, *) "Warning: Module ",model%packages(k)%sources(l)%modules_provided(m)%s, &
                " in ",model%packages(k)%sources(l)%file_name," is a duplicate"
              duplicates_found = .true.
            else
              modules(modi) = model%packages(k)%sources(l)%modules_provided(m)
              modi = modi + 1
            end if
          end do
        end if
      end do
    end do
end subroutine check_modules_for_duplicates

subroutine cmd_build(settings)
type(fpm_build_settings), intent(in) :: settings
type(package_config_t) :: package
type(fpm_model_t) :: model
type(build_target_ptr), allocatable :: targets(:)
type(error_t), allocatable :: error

integer :: i

call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build*:package error:'//error%message)
end if

call build_model(model, settings, package, error)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build*:model error:'//error%message)
end if

call targets_from_sources(targets, model, settings%prune, error)
if (allocated(error)) then
    call fpm_stop(1,'*cmd_build*:target error:'//error%message)
end if

if(settings%list)then
    do i=1,size(targets)
        write(stderr,*) targets(i)%ptr%output_file
    enddo
else if (settings%show_model) then
    call show_model(model)
else
    call build_package(targets,model,verbose=settings%verbose)
endif

end subroutine cmd_build

subroutine cmd_run(settings,test)
    class(fpm_run_settings), intent(in) :: settings
    logical, intent(in) :: test

    integer :: i, j, col_width
    logical :: found(size(settings%name))
    type(error_t), allocatable :: error
    type(package_config_t) :: package
    type(fpm_model_t) :: model
    type(build_target_ptr), allocatable :: targets(:)
    type(string_t) :: exe_cmd
    type(string_t), allocatable :: executables(:)
    type(build_target_t), pointer :: exe_target
    type(srcfile_t), pointer :: exe_source
    integer :: run_scope
    integer, allocatable :: stat(:)
    character(len=:),allocatable :: line
    logical :: toomany

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run*:package error:'//error%message)
    end if

    call build_model(model, settings%fpm_build_settings, package, error)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run*:model error:'//error%message)
    end if

    call targets_from_sources(targets, model, settings%prune, error)
    if (allocated(error)) then
        call fpm_stop(1, '*cmd_run*:targets error:'//error%message)
    end if

    if (test) then
       run_scope = FPM_SCOPE_TEST
    else
       run_scope = merge(FPM_SCOPE_EXAMPLE, FPM_SCOPE_APP, settings%example)
    end if

    ! Enumerate executable targets to run
    col_width = -1
    found(:) = .false.
    allocate(executables(0))
    do i=1,size(targets)

        exe_target => targets(i)%ptr

        if (exe_target%target_type == FPM_TARGET_EXECUTABLE .and. &
             allocated(exe_target%dependencies)) then

            exe_source => exe_target%dependencies(1)%ptr%source

            if (exe_source%unit_scope == run_scope) then

                col_width = max(col_width,len(basename(exe_target%output_file))+2)

                if (size(settings%name) == 0) then

                    exe_cmd%s = exe_target%output_file
                    executables = [executables, exe_cmd]

                else

                    do j=1,size(settings%name)

                        if (glob(trim(exe_source%exe_name),trim(settings%name(j)))) then

                            found(j) = .true.
                            exe_cmd%s = exe_target%output_file
                            executables = [executables, exe_cmd]

                        end if

                    end do

                end if

            end if

        end if

    end do

    ! Check if any apps/tests were found
    if (col_width < 0) then
        if (test) then
            call fpm_stop(0,'No tests to run')
        else
            call fpm_stop(0,'No executables to run')
        end if
    end if

    ! Check all names are valid
    ! or no name and found more than one file
    toomany= size(settings%name)==0 .and. size(executables)>1
    if ( any(.not.found) &
    & .or. &
    & ( (toomany .and. .not.test) .or.  (toomany .and. settings%runner /= '') ) &
    & .and. &
    & .not.settings%list) then
        line=join(settings%name)
        if(line/='.')then ! do not report these special strings
           if(any(.not.found))then
              write(stderr,'(A)',advance="no")'<ERROR>*cmd_run*:specified names '
              do j=1,size(settings%name)
                  if (.not.found(j)) write(stderr,'(A)',advance="no") '"'//trim(settings%name(j))//'" '
              end do
              write(stderr,'(A)') 'not found.'
              write(stderr,*)
           else if(settings%verbose)then
              write(stderr,'(A)',advance="yes")'<INFO>when more than one executable is available'
              write(stderr,'(A)',advance="yes")'      program names must be specified.'
           endif
        endif

        call compact_list_all()

        if(line=='.' .or. line==' ')then ! do not report these special strings
           call fpm_stop(0,'')
        else
           call fpm_stop(1,'')
        endif

    end if

    call build_package(targets,model,verbose=settings%verbose)

    if (settings%list) then
         call compact_list()
    else

        allocate(stat(size(executables)))
        do i=1,size(executables)
            if (exists(executables(i)%s)) then
                if(settings%runner /= ' ')then
                    if(.not.allocated(settings%args))then
                       call run(settings%runner//' '//executables(i)%s, &
                             echo=settings%verbose, exitstat=stat(i))
                    else
                       call run(settings%runner//' '//executables(i)%s//" "//settings%args, &
                             echo=settings%verbose, exitstat=stat(i))
                    endif
                else
                    if(.not.allocated(settings%args))then
                       call run(executables(i)%s,echo=settings%verbose, exitstat=stat(i))
                    else
                       call run(executables(i)%s//" "//settings%args,echo=settings%verbose, &
                             exitstat=stat(i))
                    endif
                endif
            else
                call fpm_stop(1,'*cmd_run*:'//executables(i)%s//' not found')
            end if
        end do

        if (any(stat /= 0)) then
            do i=1,size(stat)
                if (stat(i) /= 0) then
                    write(stderr,'(*(g0:,1x))') '<ERROR> Execution failed for object "',basename(executables(i)%s),'"'
                end if
            end do
            call fpm_stop(1,'*cmd_run*:stopping due to failed executions')
        end if

    endif
    contains
    subroutine compact_list_all()
    integer, parameter :: LINE_WIDTH = 80
    integer :: i, j, nCol
        j = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Available names:'
        do i=1,size(targets)

            exe_target => targets(i)%ptr

            if (exe_target%target_type == FPM_TARGET_EXECUTABLE .and. &
                allocated(exe_target%dependencies)) then

                exe_source => exe_target%dependencies(1)%ptr%source

                if (exe_source%unit_scope == run_scope) then

                    write(stderr,'(A)',advance=(merge("yes","no ",modulo(j,nCol)==0))) &
                        & [character(len=col_width) :: basename(exe_target%output_file, suffix=.false.)]
                    j = j + 1

                end if
            end if
        end do
        write(stderr,*)
    end subroutine compact_list_all

    subroutine compact_list()
    integer, parameter :: LINE_WIDTH = 80
    integer :: i, j, nCol
        j = 1
        nCol = LINE_WIDTH/col_width
        write(stderr,*) 'Matched names:'
        do i=1,size(executables)
            write(stderr,'(A)',advance=(merge("yes","no ",modulo(j,nCol)==0))) &
                & [character(len=col_width) :: basename(executables(i)%s, suffix=.false.)]
            j = j + 1
        enddo
        write(stderr,*)
    end subroutine compact_list

end subroutine cmd_run

subroutine delete_skip(unix)
    !> delete directories in the build folder, skipping dependencies
    logical, intent(in) :: unix
    character(len=:), allocatable :: dir
    type(string_t), allocatable :: files(:)
    integer :: i
    call list_files('build', files, .false.)
    do i = 1, size(files)
        if (is_dir(files(i)%s)) then
            dir = files(i)%s
            if (.not.str_ends_with(dir,'dependencies')) call os_delete_dir(unix, dir)
        end if
    end do
end subroutine delete_skip

subroutine cmd_clean(settings)
    !> fpm clean called
    class(fpm_clean_settings), intent(in) :: settings
    ! character(len=:), allocatable :: dir
    ! type(string_t), allocatable :: files(:)
    character(len=1) :: response
    if (is_dir('build')) then
        ! remove the entire build directory
        if (settings%clean_call) then
            call os_delete_dir(settings%unix, 'build')
            return
        end if
        ! remove the build directory but skip dependencies
        if (settings%clean_skip) then
            call delete_skip(settings%unix)
            return
        end if
        ! prompt to remove the build directory but skip dependencies
        write(stdout, '(A)', advance='no') "Delete build, excluding dependencies (y/n)? "
        read(stdin, '(A1)') response
        if (lower(response) == 'y') call delete_skip(settings%unix)
    else
        write (stdout, '(A)') "fpm: No build directory found."
    end if
end subroutine cmd_clean

end module fpm
 
 
!>>>>> ././src/fpm/cmd/install.f90
module fpm_cmd_install
  use, intrinsic :: iso_fortran_env, only : output_unit
  use fpm, only : build_model
  use fpm_backend, only : build_package
  use fpm_command_line, only : fpm_install_settings
  use fpm_error, only : error_t, fatal_error, fpm_stop
  use fpm_filesystem, only : join_path, list_files
  use fpm_installer, only : installer_t, new_installer
  use fpm_manifest, only : package_config_t, get_package_data
  use fpm_model, only : fpm_model_t, FPM_SCOPE_APP
  use fpm_targets, only: targets_from_sources, build_target_t, &
                         build_target_ptr, FPM_TARGET_EXECUTABLE, &
                         filter_library_targets, filter_executable_targets, filter_modules
  use fpm_strings, only : string_t, resize
  implicit none
  private

  public :: cmd_install

contains

  !> Entry point for the fpm-install subcommand
  subroutine cmd_install(settings)
    !> Representation of the command line settings
    type(fpm_install_settings), intent(in) :: settings
    type(package_config_t) :: package
    type(error_t), allocatable :: error
    type(fpm_model_t) :: model
    type(build_target_ptr), allocatable :: targets(:)
    type(installer_t) :: installer
    character(len=:), allocatable :: lib, dir
    type(string_t), allocatable :: list(:)
    logical :: installable

    call get_package_data(package, "fpm.toml", error, apply_defaults=.true.)
    call handle_error(error)

    call build_model(model, settings%fpm_build_settings, package, error)
    call handle_error(error)

    call targets_from_sources(targets, model, settings%prune, error)
    call handle_error(error)

    installable = (allocated(package%library) .and. package%install%library) &
      .or. allocated(package%executable)
    if (.not.installable) then
      call fatal_error(error, "Project does not contain any installable targets")
      call handle_error(error)
    end if

    if (settings%list) then
      call install_info(output_unit, package, model, targets)
      return
    end if

    if (.not.settings%no_rebuild) then
      call build_package(targets,model,verbose=settings%verbose)
    end if

    call new_installer(installer, prefix=settings%prefix, &
      bindir=settings%bindir, libdir=settings%libdir, &
      includedir=settings%includedir, &
      verbosity=merge(2, 1, settings%verbose))

    if (allocated(package%library) .and. package%install%library) then
      call filter_library_targets(targets, list)

      if (size(list) > 0) then
        call installer%install_library(list(1)%s, error)
        call handle_error(error)

        call install_module_files(installer, targets, error)
        call handle_error(error)
      end if
    end if

    if (allocated(package%executable)) then
      call install_executables(installer, targets, error)
      call handle_error(error)
    end if

  end subroutine cmd_install

  subroutine install_info(unit, package, model, targets)
    integer, intent(in) :: unit
    type(package_config_t), intent(in) :: package
    type(fpm_model_t), intent(in) :: model
    type(build_target_ptr), intent(in) :: targets(:)

    integer :: ii, ntargets
    character(len=:), allocatable :: lib
    type(string_t), allocatable :: install_target(:), temp(:)

    allocate(install_target(0))

    call filter_library_targets(targets, temp)
    install_target = [install_target, temp]

    call filter_executable_targets(targets, FPM_SCOPE_APP, temp)
    install_target = [install_target, temp]

    ntargets = size(install_target)

    write(unit, '("#", *(1x, g0))') &
      "total number of installable targets:", ntargets
    do ii = 1, ntargets
      write(unit, '("-", *(1x, g0))') install_target(ii)%s
    end do

  end subroutine install_info

  subroutine install_module_files(installer, targets, error)
    type(installer_t), intent(inout) :: installer
    type(build_target_ptr), intent(in) :: targets(:)
    type(error_t), allocatable, intent(out) :: error
    type(string_t), allocatable :: modules(:)
    integer :: ii

    call filter_modules(targets, modules)

    do ii = 1, size(modules)
      call installer%install_header(modules(ii)%s//".mod", error)
      if (allocated(error)) exit
    end do
    if (allocated(error)) return

  end subroutine install_module_files

  subroutine install_executables(installer, targets, error)
    type(installer_t), intent(inout) :: installer
    type(build_target_ptr), intent(in) :: targets(:)
    type(error_t), allocatable, intent(out) :: error
    integer :: ii

    do ii = 1, size(targets)
      if (is_executable_target(targets(ii)%ptr)) then
        call installer%install_executable(targets(ii)%ptr%output_file, error)
        if (allocated(error)) exit
      end if
    end do
    if (allocated(error)) return

  end subroutine install_executables

  elemental function is_executable_target(target_ptr) result(is_exe)
    type(build_target_t), intent(in) :: target_ptr
    logical :: is_exe
    is_exe = target_ptr%target_type == FPM_TARGET_EXECUTABLE .and. &
      allocated(target_ptr%dependencies)
    if (is_exe) then
      is_exe = target_ptr%dependencies(1)%ptr%source%unit_scope == FPM_SCOPE_APP
    end if
  end function is_executable_target

  subroutine handle_error(error)
    type(error_t), intent(in), optional :: error
    if (present(error)) then
      call fpm_stop(1,error%message)
    end if
  end subroutine handle_error

end module fpm_cmd_install
 
 
!>>>>> app/main.f90
program main
use, intrinsic :: iso_fortran_env, only : error_unit, output_unit
use fpm_command_line, only: &
        fpm_cmd_settings, &
        fpm_new_settings, &
        fpm_build_settings, &
        fpm_run_settings, &
        fpm_test_settings, &
        fpm_install_settings, &
        fpm_update_settings, &
        fpm_clean_settings, &
        get_command_line_settings
use fpm_error, only: error_t
use fpm_filesystem, only: exists, parent_dir, join_path
use fpm, only: cmd_build, cmd_run, cmd_clean
use fpm_cmd_install, only: cmd_install
use fpm_cmd_new, only: cmd_new
use fpm_cmd_update, only : cmd_update
use fpm_os,  only: change_directory, get_current_directory

implicit none

class(fpm_cmd_settings), allocatable :: cmd_settings
type(error_t), allocatable :: error
character(len=:), allocatable :: pwd_start, pwd_working, working_dir, project_root

call get_command_line_settings(cmd_settings)

call get_current_directory(pwd_start, error)
call handle_error(error)

call get_working_dir(cmd_settings, working_dir)
if (allocated(working_dir)) then
    ! Change working directory if requested
    if (len_trim(working_dir) > 0) then
        call change_directory(working_dir, error)
        call handle_error(error)

        call get_current_directory(pwd_working, error)
        call handle_error(error)
        write(output_unit, '(*(a))') "fpm: Entering directory '"//pwd_working//"'"
    else
        pwd_working = pwd_start
    end if
else
    pwd_working = pwd_start
end if

select type (settings => cmd_settings)
type is (fpm_new_settings)
class default
    if (.not.has_manifest(pwd_working)) then
        project_root = pwd_working
        do while(.not.has_manifest(project_root))
            working_dir = parent_dir(project_root)
            if (len(working_dir) == 0) exit
            project_root = working_dir
        end do

        if (has_manifest(project_root)) then
            call change_directory(project_root, error)
            call handle_error(error)
            write(output_unit, '(*(a))') "fpm: Entering directory '"//project_root//"'"
        end if
    end if
end select

select type(settings=>cmd_settings)
type is (fpm_new_settings)
    call cmd_new(settings)
type is (fpm_build_settings)
    call cmd_build(settings)
type is (fpm_run_settings)
    call cmd_run(settings,test=.false.)
type is (fpm_test_settings)
    call cmd_run(settings,test=.true.)
type is (fpm_install_settings)
    call cmd_install(settings)
type is (fpm_update_settings)
    call cmd_update(settings)
type is (fpm_clean_settings)
    call cmd_clean(settings)
end select

if (allocated(project_root)) then
    write(output_unit, '(*(a))') "fpm: Leaving directory '"//project_root//"'"
end if

if (pwd_start /= pwd_working) then
    write(output_unit, '(*(a))') "fpm: Leaving directory '"//pwd_working//"'"
end if

contains

    function has_manifest(dir)
        character(len=*), intent(in) :: dir
        logical :: has_manifest

        has_manifest = exists(join_path(dir, "fpm.toml"))
    end function has_manifest

    subroutine handle_error(error)
        type(error_t), optional, intent(in) :: error
        if (present(error)) then
            write(error_unit, '("[Error]", 1x, a)') error%message
            stop 1
        end if
    end subroutine handle_error

    !> Save access to working directory in settings, in case setting have not been allocated
    subroutine get_working_dir(settings, working_dir)
        class(fpm_cmd_settings), optional, intent(in) :: settings
        character(len=:), allocatable, intent(out) :: working_dir
        if (present(settings)) then
            working_dir = settings%working_dir
        end if
    end subroutine get_working_dir

end program main
 
