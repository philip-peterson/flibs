!******************************************************************************!
!>                             mod_regex module
!------------------------------------------------------------------------------!
!>
!> High-level bindings for the PCRE library, uses the lower-level bindings in
!> fcre.f90.
!>
!> Written by Paul Fossati, <paul.fossati@gmail.com>
!> Copyright (c) 2014 Paul Fossati
!>
!> PCRE library written by Philip Hazel,
!> Copyright (c) 1997-2014 University of Cambridge
!> available at http://www.pcre.org
!------------------------------------------------------------------------------!
! Redistribution and use in source and binary forms, with or without           !
! modification, are permitted provided that the following conditions are met:  !
!                                                                              !
!     * Redistributions of source code must retain the above copyright notice, !
!       this list of conditions and the following disclaimer.                  !
!                                                                              !
!     * Redistributions in binary form must reproduce the above copyright      !
!       notice, this list of conditions and the following disclaimer in the    !
!       documentation and/or other materials provided with the distribution.   !
!                                                                              !
!     * The name of the author may not be used to endorse or promote products  !
!      derived from this software without specific prior written permission    !
!      from the author.                                                        !
!                                                                              !
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  !
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    !
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   !
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     !
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          !
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         !
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     !
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      !
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      !
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   !
! POSSIBILITY OF SUCH DAMAGE.                                                  !
!******************************************************************************!
module mod_regex
    use fcre
    use iso_c_binding
    implicit none
    private
    save

!******************************************************************************!
!> Type encapsulating an allocatable character variable
!******************************************************************************!
    type, public :: FString
        character(:), allocatable :: string
    end type

!******************************************************************************!
!> Type encapsulating a PCRE regular expression
!******************************************************************************!
    type, public :: FRegex
        private
        type(c_ptr)     :: re                 = C_NULL_PTR   !< Compiled pattern
        type(c_ptr)     :: study              = C_NULL_PTR   !< Extra informations from the study of the regex
        integer(c_int)  :: execOptions        = 0            !< Options to be passed when executing the regex
        integer(c_int),public  :: compilationOptions = 0     !< Options to be passed when compiling the pattern
        character(:,c_char), allocatable :: pattern          !< Pattern (human-readable)

        ! Informations about the captured substrings
        integer, public :: captureCount       = 0            !< Number of captured substrings
        integer, public :: namedCaptureCount  = 0            !< Number of named captured substrings
        integer(c_int), allocatable, dimension(:) :: ovector !< Output data vector

        ! Informations about substitutions
        integer         :: backReferenceCount = 0                   !< Number of back references in the substitution expression
        integer, allocatable, dimension(:) :: backReferenceIndex    !< Indices of the back references in the substitution string
        integer, allocatable, dimension(:) :: backReferencePosition !< Positions of the back references in the substitution string
        character(:), allocatable          :: substitute            !< Substitution string

        ! Informations used for global mode
        logical         :: global = .false.                 !< Global mode
        integer         :: offset = 0                       !< Offset to use as new starting position during successive matches
        character(:), allocatable          :: lastSubject   !< Subject string of the last match
    contains
        ! Pattern-related procedures
        procedure, public  :: setPattern
        procedure, public  :: getPattern
        procedure, public  :: setSubstitution
        procedure, private :: studyPattern
        procedure, public  :: compilePattern

        ! Options
        procedure, public :: setGlobal
        procedure, public :: setCaseSensitive
        procedure, public :: setMultiline
        procedure, public :: setExtended
        procedure, public :: setReluctant

        ! Lower-level procedures
        procedure, public :: setCompilationOption
        procedure, public :: unsetCompilationOption
        procedure, public :: getPatternInfo

        ! Matching procedures
        procedure, private :: matchSimple
        procedure, private :: matchAndCaptureAll
        procedure, private :: matchAndCaptureWithIndex
        procedure, private :: matchAndCaptureWithName
        generic :: match => matchSimple, matchAndCaptureAll, matchAndCaptureWithIndex, matchAndCaptureWithName
        procedure, public :: match_all
        procedure, public :: doesMatch
        procedure, public :: replace
        procedure, public :: replace_all

        final :: fregex_finalise
    end type

    ! Constructor
    interface FRegex
        module procedure fcre_new_regex
    end interface

    ! Public subroutines
    public :: fcre_new_regex
    public :: c_f_string
    public :: fcre_error_message
contains
!******************************************************************************!
!>\brief Conversion from C to Fortran strings
!>
!> Conversion from C to Fortran strings. Cannot be used with gfortran when
!> using the -fcheck=bond option.
!******************************************************************************!
    subroutine c_f_string(stringptr, fstring)
        type(c_ptr), intent(in) :: stringptr
        character(:), allocatable, intent(out) :: fstring
!------
        character(:,c_char), pointer :: buffer
        integer :: i
!------
        if (.not.c_associated(stringptr)) return
        call c_f_pointer(stringptr, buffer)
        i = 1
        do while(buffer(i:i) /= C_NULL_CHAR)
            i = i + 1
        end do
        fstring = buffer(:i-1)
    end subroutine
!******************************************************************************!
!>\brief Constructor for FRegex variables
!>
!> Constructor for FRegex variables. Simply calls setPattern and
!> setSubstitution.
!>
!>\see setPattern
!>\see setSubstitution
!******************************************************************************!
    function fcre_new_regex(pattern, substitution) result(this)
        type(FRegex) :: this
        character(*), intent(in), optional :: pattern
        character(*), intent(in), optional :: substitution
!------
        ! Set the pattern if present
        if (present(pattern)) then
            call this%setPattern(pattern)
        end if

        ! Set the substitution pattern if present
        if (present(substitution)) then
            call this%setSubstitution(substitution)
        end if
    end function
!******************************************************************************!
!>\brief Destructor for FRegex variables
!>
!> Destructor for FRegex variables. Automatically frees the internal pcre
!> regex if needed, as well as the study data.
!******************************************************************************!
    !AM impure elemental subroutine fregex_finalise(this)
    subroutine fregex_finalise(this)
        type(FRegex), intent(inout) :: this
!------
        ! Free the compiled pattern if needed
        if (c_associated(this%re)) then
            call pcre_free(this%re)
            this%re = C_NULL_PTR
        end if

        ! Free the study information if needed
        if (c_associated(this%study)) then
            call pcre_free_study(this%study)
            this%study = C_NULL_PTR
        end if
    end subroutine
!******************************************************************************!
!>\brief Set the pattern matching mode to be switch between case sensitive and
!> insensitive
!>
!> Set the pattern matching mode to be switch between case sensitive and
!> insensitive. This affects only the compilation options. The pattern will
!> need to be recompiled for the change to take effect.
!> If the mode variable is not present, the option will be set to case
!> insensitive.
!>
!>\see setCompilationOption, unsetCompilationOption
!******************************************************************************!
    subroutine setCaseSensitive(this, mode)
        class(FRegex), intent(inout) :: this
        logical, intent(in), optional :: mode
!------
        logical :: mode_
!------
        if (present(mode)) then
            mode_ = mode
        else
            mode_ = .true.
        end if

        ! Case sensitivity is the first bit of the compilation options
        if (mode_) then
            call this%unsetCompilationOption(PCRE_CASELESS)
        else
            call this%setCompilationOption(PCRE_CASELESS)
        end if
    end subroutine
!******************************************************************************!
!>\brief Set the pattern matching mode to be switch between single-line and
!> multi-line
!>
!> Set the pattern matching mode to be switch between single-line and
!> multi-line. This affects only the compilation options. The pattern will
!> need to be recompiled for the change to take effect.
!> If the mode variable is not present, the option will be set to multiline.
!>
!>\see setCompilationOption, unsetCompilationOption
!******************************************************************************!
    subroutine setMultiline(this, mode)
        class(FRegex), intent(inout) :: this
        logical, intent(in), optional :: mode
!------
        logical :: mode_
!------
        if (present(mode)) then
            mode_ = mode
        else
            mode_ = .true.
        end if

        ! Multiline mode is the second bit of the compilation options
        if (mode_) then
            call this%setCompilationOption(PCRE_MULTILINE)
        else
            call this%unsetCompilationOption(PCRE_MULTILINE)
        end if
    end subroutine
!******************************************************************************!
!>\brief Set the pattern matching mode to be switch between simple and extended
!>
!> Set the pattern matching mode to be switch between simple and extended. This
!> affects only the compilation options. The pattern will need to be recompiled
!> for the change to take effect.
!> If the mode variable is not present, the option will be set to extended.
!>
!>\see setCompilationOption, unsetCompilationOption
!******************************************************************************!
    subroutine setExtended(this, mode)
        class(FRegex), intent(inout) :: this
        logical, intent(in), optional :: mode
!------
        logical :: mode_
!------
        if (present(mode)) then
            mode_ = mode
        else
            mode_ = .true.
        end if

        ! Extended mode is the fourth bit of the compilation options
        if (mode_) then
            call this%setCompilationOption(PCRE_EXTENDED)
            this%execOptions = ibset(this%execOptions,fcre_option_position(PCRE_EXTENDED))
        else
            call this%unsetCompilationOption(PCRE_EXTENDED)
            this%execOptions = ibclr(this%execOptions,fcre_option_position(PCRE_EXTENDED))
        end if
    end subroutine
!******************************************************************************!
!>\brief  Set the pattern matching mode to be switch between greedy and
!> reluctant
!>
!> Set the pattern matching mode to be switch between greedy and not greedy.
!> This affects only the compilation options. The pattern will need to be
!> recompiled for the change to take effect.
!> If the mode variable is not present, the option will be set to greedy.
!>
!>\see setCompilationOption, unsetCompilationOption
!******************************************************************************!
    subroutine setReluctant(this, mode)
        class(FRegex), intent(inout) :: this
        logical, intent(in), optional :: mode
!------
        logical :: mode_
!------
        if (present(mode)) then
            mode_ = mode
        else
            mode_ = .true.
        end if

        ! Greedy mode is the tenth bit of the compilation options
        if (mode_) then
            call this%setCompilationOption(PCRE_UNGREEDY)
        else
            call this%unsetCompilationOption(PCRE_UNGREEDY)
        end if
    end subroutine
!******************************************************************************!
!>\brief Set the pattern matching mode to be switch between standard and global
!>
!> Set the pattern matching mode to be switch between standard and global. No
!> pattern recompilation is required.
!******************************************************************************!
    subroutine setGlobal(this, mode)
        class(FRegex), intent(inout) :: this
        logical, intent(in), optional :: mode
!------
        if (present(mode)) then
            ! Reset the offset when changing mode
            if (mode .neqv. this%global) this%offset = 0

            this%global = mode
        else
            this%global = .true.
        end if
    end subroutine
!******************************************************************************!
!>\brief Set regex compilation options
!>
!> Set regex compilation options. Valid options are defined in fcre.f90. This
!> gives a direct access to all the compilation options accepted by the PCRE
!> library, other procedures provide simpler access to the most commonly-used
!> options.
!>
!>\see unsetCompilationOption
!******************************************************************************!
    subroutine setCompilationOption(this, option, status)
        class(FRegex), intent(inout) :: this
        integer, intent(in) :: option             !< option to be set (valid values are defined in fcre.f90)
        integer, intent(out), optional :: status  !< Error code if something goes wrong
!-------
        integer :: p
!------
        ! Get the position of the bit corresponding to the option to set
        p = fcre_option_position(option)

        ! Test the option validity
        if (p < 0) then
            if (present(status)) then
                status = PCRE_ERROR_BADOPTION
                return
            else
                write(*,*) "Error while setting compilation options: ", fcre_error_message(PCRE_ERROR_BADOPTION)
                stop
            end if
        end if

        ! Set the option bit
        this%compilationOptions = ibset(this%compilationOptions, p)

        ! The extra data is no longer valid once the compilation options have been changed
        if (c_associated(this%re)) then
            call pcre_free(this%re)
            this%re = C_NULL_PTR
        end if
        if (c_associated(this%study)) then
            call pcre_free(this%study)
            this%study = C_NULL_PTR
        end if
        if (allocated(this%lastSubject)) deallocate(this%lastSubject)
        this%offset = 0
    end subroutine
!******************************************************************************!
!>\brief Unset regex compilation options
!>
!> Unset regex compilation options. Valid options are defined in fcre.f90.
!> This gives a direct access to all the compilation options accepted by the
!> PCRE library, other procedures provide simpler access to the most commonly-
!> used options.
!>
!>\see setCompilationOption
!******************************************************************************!
    subroutine unsetCompilationOption(this, option, status)
        class(FRegex), intent(inout) :: this
        integer, intent(in) :: option         !< option to be unset (valid values are defined in fcre.f90)
        integer, intent(out), optional :: status  !< Error code if something goes wrong
!------
        integer :: p
!------
        ! Get the position of the bit corresponding to the option to un-set
        p = fcre_option_position(option)

        ! Test the option validity
        if (p < 0) then
            if (present(status)) then
                status = PCRE_ERROR_BADOPTION
                return
            else
                write(*,*) "Error while changing compilation options: ", fcre_error_message(PCRE_ERROR_BADOPTION)
                stop
            end if
        end if

        ! Set the option bit
        this%compilationOptions = ibclr(this%compilationOptions, fcre_option_position(option))

        ! The extra data is no longer valid once the compilation options have been changed
        if (c_associated(this%re)) then
            call pcre_free(this%re)
            this%re = C_NULL_PTR
        end if
        if (c_associated(this%study)) then
            call pcre_free(this%study)
            this%study = C_NULL_PTR
        end if
        if (allocated(this%lastSubject)) deallocate(this%lastSubject)
        this%offset = 0
    end subroutine
!******************************************************************************!
!>\brief Study the pattern of a regular expression
!>
!> Study the pattern of a regular expression. If status is provided and the
!> compilation fails, it is set to a non-zero value.
!******************************************************************************!
    subroutine studyPattern(this, status, errorMessage)
        class(FRegex), intent(inout)  :: this
        integer, intent(out), optional :: status                         !< error number
        character(:), allocatable, intent(out), optional :: errorMessage !< error message
!------
        integer(c_int) :: options
        type(c_ptr) :: errorPointer
        character(:), allocatable :: errorMessage_
        integer :: status_
!------
        ! Try to compile the pattern if needed
        if (.not.c_associated(this%re)) then
            if (allocated(this%pattern)) call this%compilePattern(status_, errorMessage_)

            ! Test whether the pattern was compiled
            if (.not.c_associated(this%re)) then
                if (present(status).or.present(errorMessage)) then
                    if (present(status)) status = status_
                    if (present(errorMessage)) errorMessage = errorMessage_
                    return
                else
                    write(*,'(a)') "The pattern "//this%pattern//" could not be compiled: " // errorMessage_
                    stop
                end if
            end if
        end if

        ! Use the JIT compiler if available
        options = PCRE_STUDY_JIT_COMPILE

        ! Deallocate the previous study if needed
        if (c_associated(this%study)) then
            call pcre_free_study(this%study)
            this%study = C_NULL_PTR
        end if

        ! Study the pattern
        this%study = pcre_study(code=this%re, &
                             options=options, &
                              errptr=errorPointer)

        ! Something failed
        if (.not.c_associated(this%study)) then
            call c_f_string(errorPointer, errorMessage_)

            ! Return and let the caller handle the failure if status or errorMessage is present
            if (present(status) .or. present(errorMessage)) then
                if (present(status)) status = FCRE_ERROR
                if (present(errorMessage)) errormessage = errorMessage_
                return
            end if

            ! Otherwise, just abort
            write(*,'(a)') "Study failed with error: " // errorMessage_
            stop
        end if

        ! The study succeeded
        if (present(status)) status = 0
        if (present(errorMessage)) errorMessage = ""
    end subroutine
!******************************************************************************!
!>\brief Get information about the pattern of a regular expression
!>
!> Get information about the pattern of a regular expression. If status is
!> provided and the information can not be obtained, it is set to the error
!> number returned by pcre.
!> The result is a c_ptr variable that should be interpreted differently
!> depending on the requested information.
!>
!>FIXME: add specific procedures for the unsupported info
!******************************************************************************!
    subroutine getPatternInfo(this, info, result, status)
        class(FRegex), intent(inout)   :: this
        integer(c_int), intent(in)     :: info      !< requested information
        integer(c_long),    intent(out):: result    !< request result
        integer, intent(out), optional :: status    !< error number
!------
        integer(c_int) :: errorCode
        integer(c_size_t), target :: size_t_
        integer(c_int), target :: int_
        integer(c_int32_t), target :: int32_
        integer(c_long), target :: long_
        integer(c_signed_char), target :: unit_t_
        type(c_ptr) resultPtr
!------
        ! Some info are not supported in this procedure
        if (info == PCRE_INFO_FIRSTTABLE .or. info == PCRE_INFO_NAMETABLE .or. &
            info == PCRE_INFO_DEFAULT_TABLES) then

            if (present(status)) then
                status = FCRE_ERROR_UNSUPPORTED_OPTION
            else
                write(*,*) fcre_error_message(FCRE_ERROR_UNSUPPORTED_OPTION)
            end if
            return
        end if

        select case(info)
            case (PCRE_INFO_OPTIONS)
                resultPtr = c_loc(long_)
            case (PCRE_INFO_SIZE)
                resultPtr = c_loc(size_t_)
            case (PCRE_INFO_CAPTURECOUNT)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_BACKREFMAX)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_FIRSTBYTE) ! Also matches PCRE_INFO_FIRSTCHAR
                resultPtr = c_loc(int_)
            case (PCRE_INFO_LASTLITERAL)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_NAMEENTRYSIZE)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_NAMECOUNT)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_STUDYSIZE)
                resultPtr = c_loc(size_t_)
            case (PCRE_INFO_OKPARTIAL)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_JCHANGED)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_HASCRORLF)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_MINLENGTH)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_JIT)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_JITSIZE)
                resultPtr = c_loc(size_t_)
            case (PCRE_INFO_MAXLOOKBEHIND)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_FIRSTCHARACTER)
                resultPtr = c_loc(unit_t_)
            case (PCRE_INFO_FIRSTCHARACTERFLAGS)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_REQUIREDCHAR)
                resultPtr = c_loc(unit_t_)
            case (PCRE_INFO_REQUIREDCHARFLAGS)
                resultPtr = c_loc(int_)
            case (PCRE_INFO_MATCHLIMIT)
                resultPtr = c_loc(int32_)
            case (PCRE_INFO_RECURSIONLIMIT)
                resultPtr = c_loc(int32_)
            case (PCRE_INFO_MATCH_EMPTY)
                resultPtr = c_loc(int_)
            case default
        end select

        errorCode = pcre_fullinfo(code=this%re,    &
                                 extra=this%study, &
                                  what=info,       &
                                 where=resultPtr)

        ! Set the error number if a problem happened
        if (errorCode /= 0) then
            if (present(status)) status = errorCode
            return
        end if

        select case(info)
            case (PCRE_INFO_OPTIONS)
                result = long_
            case (PCRE_INFO_SIZE)
                result = size_t_
            case (PCRE_INFO_CAPTURECOUNT)
                result = int_
            case (PCRE_INFO_BACKREFMAX)
                result = int_
            case (PCRE_INFO_FIRSTBYTE) ! Also matches PCRE_INFO_FIRSTCHAR
                result = int_
            case (PCRE_INFO_LASTLITERAL)
                result = int_
            case (PCRE_INFO_NAMEENTRYSIZE)
                result = int_
            case (PCRE_INFO_NAMECOUNT)
                result = int_
            case (PCRE_INFO_STUDYSIZE)
                result = size_t_
            case (PCRE_INFO_OKPARTIAL)
                result = int_
            case (PCRE_INFO_JCHANGED)
                result = int_
            case (PCRE_INFO_HASCRORLF)
                result = int_
            case (PCRE_INFO_MINLENGTH)
                result = int_
            case (PCRE_INFO_JIT)
                result = int_
            case (PCRE_INFO_JITSIZE)
                result = size_t_
            case (PCRE_INFO_MAXLOOKBEHIND)
                result = int_
            case (PCRE_INFO_FIRSTCHARACTER)
                result = unit_t_
            case (PCRE_INFO_FIRSTCHARACTERFLAGS)
                result = int_
            case (PCRE_INFO_REQUIREDCHAR)
                result = unit_t_
            case (PCRE_INFO_REQUIREDCHARFLAGS)
                result = int_
            case (PCRE_INFO_MATCHLIMIT)
                result = int32_
            case (PCRE_INFO_RECURSIONLIMIT)
                result = int32_
            case (PCRE_INFO_MATCH_EMPTY)
                result = int_
            case default
        end select

        if (present(status)) status = 0
    end subroutine
!******************************************************************************!
!>\brief Compile the pattern of a regular expression
!>
!> Compile the pattern of a regular expression. If the pattern is an
!> empty string, status is set to FCRE_ERROR_EMPTY_PATTERN if present, otherwise
!> the subroutine returns. If another compilation error occurs, status is set
!> to the error code returned by the PCRE library if it is present, otherwise
!> the program aborts.
!******************************************************************************!
    subroutine compilePattern(this, status, errorMessage)
        class(FRegex), intent(inout)  :: this
        integer, intent(out), optional :: status                         !< error number
        character(:), allocatable, intent(out), optional :: errorMessage !< error message
!------
        type(c_ptr) :: errorPointer
        integer(c_int) :: erroffset, errorCode
        integer(c_long), target :: captureCount
        character(:), allocatable :: errorMessage_
!------
        ! Try to compile the regex
        errorCode = 0

        ! Special case: empty pattern
        if (this%pattern == "") then
            if (present(status)) status = FCRE_ERROR_EMPTY_PATTERN
            return
        end if

        this%re = pcre_compile2(pattern=this%pattern,            &
                                options=this%compilationOptions, &
                                  error=errorCode,               &
                                 errptr=errorPointer,            &
                              erroffset=erroffset,               &
                               tableptr=C_NULL_PTR)

        ! Compilation failed: print the error message and exit
        if (.not.c_associated(this%re)) then
            call c_f_string(errorPointer, errorMessage_)

            ! Return and let the caller handle the failure if status or errorMessage is present
            if (present(status) .or. present(errorMessage)) then
                if (present(status)) status = errorCode
                if (present(errorMessage)) errorMessage =  errorMessage_
                return
            end if

            ! Otherwise, just abort
            write(*,'(a,i0,a)') "Regular expression compilation failed at offset ", erroffset, ": " // errorMessage_
            stop
        end if

        ! Study the pattern by default
        call this%studyPattern

        ! Get the number of captured substrings
        call this%getPatternInfo(PCRE_INFO_CAPTURECOUNT, captureCount, errorCode)
        if (errorCode == 0) then
            this%captureCount = captureCount

            ! Prepare the result array
            if (allocated(this%ovector)) deallocate(this%ovector)
            allocate(this%ovector((this%captureCount + 1) * 3))
            this%ovector = 0
        else
            ! This really should not happen

            ! Nonetheless, if it does, return with an error status if possible
            if (present(status) .or. present(errorMessage)) then
                if (present(status)) status = -1
                if (present(errorMessage)) errorMessage = "Error while getting the number of captured substrings"
                return
            end if

            ! Otherwise, abort
            write(*,*) "Error while getting the number of captured substrings", errorCode
            stop
        end if

        !FIXME: Get the table of the named captured strings

        ! Compilation succeeded
        if (present(status)) status = 0
        if (present(errorMessage)) errorMessage = ""
    end subroutine
!******************************************************************************!
!>\brief Set the pattern of a regular expression
!>
!> Set the pattern of a regular expression. If no pattern is provided, the
!> pattern is reset, with the study data if set in the FRegex object.
!******************************************************************************!
    subroutine setPattern(this, pattern, status)
        class(FRegex), intent(inout)  :: this
        character(*),  intent(in),  optional  :: pattern                 !< pattern to be compiled
        integer, intent(out), optional :: status
!------
        if (present(pattern)) then
            ! Set the pattern
            if (pattern(len(pattern):len(pattern)) == C_NULL_CHAR) then
                this%pattern = pattern
            else
                this%pattern = pattern//C_NULL_CHAR
            end if
        else
            ! Clear the pattern
            if (allocated(this%pattern)) deallocate(this%pattern)

            ! Remove the compiled regex
            if (c_associated(this%re)) then
                call pcre_free(this%re)
                this%re = C_NULL_PTR
            end if

            ! Invalidate the study
            if (c_associated(this%study)) then
                call pcre_free(this%study)
                this%study = C_NULL_PTR
            end if
        end if

        ! Not really useful for now; might be in the future
        if (present(status)) status = 0
    end subroutine
!******************************************************************************!
!>\brief Get the pattern of a regular expression
!>
!> Get the pattern of a regular expression.
!******************************************************************************!
    subroutine getPattern(this, pattern)
        class(FRegex), intent(inout) :: this
        character(:), allocatable, intent(out) :: pattern

        pattern = this%pattern
    end subroutine
!******************************************************************************!
!>\brief Prepare a regex for substitution
!>
!> Prepare a regex for substitution. The pattern must have been set
!> previously, and will be compiled if needed, as the substitution informations
!> are required.
!>
!> Informations about the substitution are stored in the FRegex object:
!> backReferenceCount: number of back references in the substitution string
!> backReferenceIndex: index of each back reference
!> backReferencePosition: position in the substitution string at which each back
!>   reference should be added
!> substitution: substitution string without the back references.
!>
!>FIXME: add consistency checks for the substitution string
!******************************************************************************!
    subroutine setSubstitution(this, substitution, status)
        class(FRegex), intent(inout) :: this
        character(*), intent(in), optional :: substitution
        integer, intent(out), optional :: status
!------
        type(FRegex) :: backReferenceRegex
        integer, dimension(:), allocatable :: index, length
        integer :: c, n, iostat, sLength, status_
        character(:), allocatable :: buffer
!------
        ! Cleanup if the substitution string is not present
        if (.not.present(substitution)) then
            this%backReferenceCount = 0
            if (allocated(this%backReferenceIndex)) deallocate(this%backReferenceIndex)
            if (allocated(this%substitute)) deallocate(this%substitute)

            if (present(status)) status = 0
            return
        end if

        ! Compile the pattern if needed
        if (.not.c_associated(this%re) .and. allocated(this%pattern)) then
            call this%compilePattern(status_)
        end if

        ! Do not set the substitution if the pattern has not been compiled
        if (status_ /= 0) then

            ! Return if the status can be set
            if (present(status)) then
                status = FCRE_ERROR_UNSET_PATTERN
                return
            end if

            ! Otherwise, just abort
            write(*,*) "Error while setting substitution string: ", fcre_error_message(FCRE_ERROR_UNSET_PATTERN)
            stop
        end if

        ! Detect the back references in the substitution string
        call backReferenceRegex%setPattern("\$([0-9]+|\`|\'|\+|\&)")
        call backReferenceRegex%match_all(substitution, index, length)

        if (allocated(index)) then
            ! There is at least one back reference
            if (allocated(this%backReferenceIndex)) deallocate(this%backReferenceIndex)
            allocate(this%backReferenceIndex(size(index)))
            this%backReferenceIndex = 0
            if (allocated(this%backReferencePosition)) deallocate(this%backReferencePosition)
            allocate(this%backReferencePosition(size(index)))
            this%backReferencePosition = 0

            allocate(character(len(substitution)) :: buffer)
            buffer = substitution
            do c=1, size(index)
                read(buffer(index(c)+1:index(c)+length(c)-1),*,iostat=iostat) n
                if (iostat == 0) then
                    ! The character after the dollar sign is an integer
                    buffer(index(c):) = buffer(index(c)+length(c):)
                    this%backReferenceIndex(c) = n
                    this%backReferencePosition(c) = index(c)
                    index(:) = index(:) - length(c)
                else
                    ! Special cases
                    select case(buffer(index(c)+1:index(c)+1))
                        case ("`")
                            buffer(index(c):) = buffer(index(c)+length(c):)
                            this%backReferenceIndex(c) = FCRE_REF_BEFORE
                            this%backReferencePosition(c) = index(c)
                            index(:) = index(:) - length(c)
                        case ("'")
                            buffer(index(c):) = buffer(index(c)+length(c):)
                            this%backReferenceIndex(c) = FCRE_REF_AFTER
                            this%backReferencePosition(c) = index(c)
                            index(:) = index(:) - length(c)
                        case ("+")
                            buffer(index(c):) = buffer(index(c)+length(c):)
                            this%backReferenceIndex(c) = FCRE_REF_LAST
                            this%backReferencePosition(c) = index(c)
                            index(:) = index(:) - length(c)
                        case ("&")
                            buffer(index(c):) = buffer(index(c)+length(c):)
                            this%backReferenceIndex(c) = FCRE_REF_MATCH
                            this%backReferencePosition(c) = index(c)
                            index(:) = index(:) - length(c)
                        case default

                    end select
                end if
            end do

            this%backReferenceCount = size(index) ! Number of back references

            sLength = len(substitution) - sum(length)
            this%substitute = buffer(:sLength) ! Cleaned substitution string
        else
            ! Constant substitution
            this%substitute = substitution
        end if
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable
!>
!> Match a regular expression to a character variable. The returned
!> index is 0 if no match is found, < 0 in case of an error, and > 0 if no match
!> is found.
!> If a substitution pattern is set in the regex, it will be applied to the
!> subject string
!>
!>\see match
!******************************************************************************!
    subroutine replace(this, subject, index, length, status)
        class(FRegex), intent(inout) :: this
        character(:), allocatable, intent(inout) :: subject   !< input character variable
        integer, intent(out) :: index                         !< index of the first match substring
        integer, intent(out), optional :: length              !< length of the first matched substring
        integer, intent(out), optional :: status              !< error code
!------
        integer :: l, istat, sLength, c, n, start, end
        character(:), allocatable :: buffer
!------

        call this%match(subject, index, l, istat)

        ! Length of the substituted string
        sLength = 0

        ! Do the substitution
        if (istat == 0 .and. allocated(this%substitute)) then

            if (len(this%substitute) > 0) then
                ! Get a buffer with the correct length
                sLength = 0 ! Length of the back reference
                do c=1, this%backReferenceCount
                    n = this%backReferenceIndex(c)
                    if (n > 0) then
                        sLength = sLength + this%ovector(n*2+2) - this%ovector(n*2+1)
                    else if (n == FCRE_REF_BEFORE) then
                        sLength = sLength + this%ovector(1)
                    else if (n == FCRE_REF_AFTER) then
                        sLength = sLength + len(subject) - this%ovector(2)
                    else if (n == FCRE_REF_LAST) then
                        !FIXME: $+ is unsupported
                    else if (n == FCRE_REF_MATCH) then
                        sLength = sLength + this%ovector(2) - this%ovector(1)
                    end if
                end do
                sLength = sLength + len(this%substitute)
                allocate(character(sLength) :: buffer)

                ! Add the back references content in the substitution string
                buffer(:len(this%substitute)) = this%substitute
                buffer(len(this%substitute)+1:) = ""
                do c=this%backReferenceCount, 1, -1
                    start = this%backReferencePosition(c)
                    n = this%backReferenceIndex(c)
                    if (n > 0) then ! Simple back reference
                        end = start + this%ovector(n*2+2) - this%ovector(n*2+1) - 1
                        buffer(start:) = subject(this%ovector(n*2+1)+1:this%ovector(n*2+2)) // buffer(start:)
                    else if (n == FCRE_REF_BEFORE) then ! Special variable $``
                        end = start + this%ovector(1)
                        buffer(start:) = subject(:this%ovector(1)) // buffer(start:)
                    else if (n == FCRE_REF_AFTER) then ! Special variable $'
                        end = start + len(subject) - this%ovector(2) - 1
                        buffer(start:) = subject(this%ovector(2)+1:) // buffer(start:)
                    else if (n == FCRE_REF_LAST) then ! Special variable $+
                        !FIXME: $+ is unsupported
                    else if (n == FCRE_REF_MATCH) then ! Special variable $&
                        end = start + this%ovector(2) - this%ovector(1) - 1
                        buffer(start:) = subject(this%ovector(1)+1:this%ovector(2)) // buffer(start:)
                    end if
                end do
            else
                allocate(character(0) :: buffer)
            end if

            ! Replace the whole matched part of the string if a replacement character variable is provided
            subject = subject(:index-1)//buffer//subject(index+l:)

            ! Update the FRegex if it is in global mode
            if (this%global) then
                this%offset = index + len(buffer)
                this%lastSubject = subject
            end if
        end if

        ! Set the optional outputs
        if (present(length)) length = l
        if (present(status)) status = istat
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable, and get the
!> captured substrings
!>
!> Match a regular expression to a character variable. and get the captured
!> substrings. The returned index is 0 if no match is found. The program will
!> stop if an error occurs and no actual argument is present for status.
!> The substrings array is allocated and filled  with the captured substrings.
!>
!>\see replace
!******************************************************************************!
    subroutine matchAndCaptureAll(this, subject, index, capture_array, length, status)
        class(FRegex), intent(inout) :: this
        character(*),  intent(in)    :: subject                !< input character variable
        integer,       intent(out)   :: index                  !< index of the first match substring
        type(FString), dimension(:), allocatable, intent(out) :: capture_array
        integer,       intent(out), optional :: length         !< length of the first matched substring
        integer,       intent(out), optional :: status         !< error code
!------
        call matchInternal(this, subject, index, capture_array=capture_array, length=length, status=status)
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable, and get the
!> captured substring corresponding to a given index
!>
!> Match a regular expression to a character variable. and get the captured
!> substring corresponding to a given index. The returned index is 0 if no
!> match is found. The program will stop if an error occurs and no actual
!> argument is present for status.
!>
!>\see replace
!******************************************************************************!
    subroutine matchAndCaptureWithIndex(this, subject, index, capture, capture_index, length, status)
        class(FRegex), intent(inout) :: this
        character(*),  intent(in)    :: subject                !< input character variable
        integer,       intent(out)   :: index                  !< index of the first match substring
        character(:), allocatable, intent(out) :: capture
        integer, intent(in) :: capture_index
        integer,       intent(out), optional :: length         !< length of the first matched substring
        integer,       intent(out), optional :: status         !< error code
!------
        call matchInternal(this, subject, index, length=length, status=status, capture=capture, capture_index=capture_index)
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable, and get the
!> captured substring corresponding to a given name
!>
!> Match a regular expression to a character variable. and get the captured
!> substring corresponding to a given index. The returned index is 0 if no
!> match is found. The program will stop if an error occurs and no actual
!> argument is present for status.
!>
!>\see replace
!******************************************************************************!
    subroutine matchAndCaptureWithName(this, subject, index, capture, capture_name, length, status)
        class(FRegex), intent(inout) :: this
        character(*),  intent(in)    :: subject                !< input character variable
        integer,       intent(out)   :: index                  !< index of the first match substring
        character(:), allocatable, intent(out) :: capture
        character(*), intent(in) :: capture_name
        integer,       intent(out), optional :: length         !< length of the first matched substring
        integer,       intent(out), optional :: status         !< error code
!------
        call matchInternal(this, subject, index, length=length, status=status, capture=capture, capture_name=capture_name)
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable
!>
!> Match a regular expression to a character variable. The returned index is 0
!> if no match is found. The program will stop if an error occurs and no actual
!> argument is present for status.
!>
!>\see replace
!******************************************************************************!
    subroutine matchSimple(this, subject, index, length, status)
        class(FRegex), intent(inout) :: this
        character(*),  intent(in)    :: subject                !< input character variable
        integer,       intent(out)   :: index                  !< index of the first match substring
        integer,       intent(out), optional :: length         !< length of the first matched substring
        integer,       intent(out), optional :: status         !< error code
!------
        call matchInternal(this, subject, index, length=length, status=status)
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable, simple function
!>
!> Match a regular expression to a character variable. This is a simple function
!> wrapper around matchInternal.
!******************************************************************************!
    function doesMatch(this, subject)
        class(FRegex), intent(inout) :: this
        character(*), intent(in) :: subject
        logical :: doesMatch
!------
        integer :: index
!------
        call matchInternal(this, subject, index)
        doesMatch = .false.
        if (index > 0) doesMatch = .true.
    end function
!******************************************************************************!
!>\brief Match a regular expression to a character variable
!>
!> Internal subroutine doing the pattern matching, not visible from outside the
!> module.
!******************************************************************************!
    subroutine matchInternal(this, subject, index, length, capture_array, capture, capture_index, capture_name, status)
        class(FRegex), intent(inout) :: this
        character(*),  intent(in)    :: subject                !< input character variable
        integer,       intent(out)   :: index                  !< index of the first match substring
        integer,       intent(out), optional :: length         !< length of the first matched substring
        type(FString), dimension(:), allocatable, intent(out), optional :: capture_array !< array to store the captured substrings
        character(:), allocatable, intent(out), optional :: capture            !< variable to store a captured substring
        integer,       intent(in),  optional :: capture_index                  !< number of the substring to be returned
        character(*),  intent(in),  optional :: capture_name                   !< name of the substring to be returned
        integer,       intent(out), optional :: status         !< error code
!------
        character(:,c_char), allocatable :: c_name, c_string
        integer(c_int) :: rc
        integer :: i, captureIndex, err
!------
        ! Default values for the index and length
        index = 0
        if (present(length)) length = 0
        if (present(status)) status = 0

        ! Special case: empty pattern
        if (.not.allocated(this%pattern)) then
            if (present(status)) then
                status = FCRE_ERROR_EMPTY_PATTERN
            else
                write(*,*) "Error while matching pattern: ", fcre_error_message(FCRE_ERROR_UNSET_PATTERN)
            end if
            return
        end if
        if (this%pattern == C_NULL_CHAR) then
            if (present(status)) status = FCRE_ERROR_EMPTY_PATTERN
            return
        end if

        ! Make sure the pattern has been compiled
        if (.not.c_associated(this%re)) then
            if (allocated(this%pattern)) then
                ! If the pattern is set, try comiling it
                call this%compilePattern(err)

                if (present(status)) status = err
            else
                ! If not, return after having set the status to FCRE_ERROR_UNSET_PATTERN to let the caller a chance to understand what happened
                ! The index is 0 as no match has been found
                if (present(status)) status = FCRE_ERROR_UNSET_PATTERN
                return
            end if
        end if

        ! C-interoperable version of the input string
        c_string = subject

        ! If we are in global mode, make sure that the subject has not changed
        if (this%global .and. allocated(this%lastSubject)) then
            if (subject /= this%lastSubject) this%offset = 0
        end if

        ! Try to execute the regular expression
        this%ovector = 0
        rc = pcre_exec(        &
            this%re,           &
            this%study,        &
            c_string,          &
            len(c_string),     &
            this%offset,       &
            this%execOptions,  &
            this%ovector,      &
            size(this%ovector))

        ! Matching failed: handle error cases
        if (rc < 0) then
            select case(rc)
                case (PCRE_ERROR_NOMATCH) ! No match found
                    ! Set the status if present, otherwise just return
                    if (present(status)) status = PCRE_ERROR_NOMATCH
                    return
                case default ! Other error
                    if (present(status)) then
                        status = rc
                        return
                    else
                        write(*,*) "Error while matching pattern: ", fcre_error_message(rc)
                        stop
                    end if
            end select
        end if

        ! Matching succeded
        index = this%ovector(1)+1
        if (present(length)) length = this%ovector(2)-this%ovector(1)

        ! Update the FRegex object if we are in global mode
        if (this%global) then
            ! Update the offset
            this%offset = this%ovector(2)
            this%lastSubject = subject
        end if

!------ Extract all the captured substrings
        if (present(capture_array)) then
            if (rc > 1 ) then
                ! Some substrings were captured

                ! Allocate the array containing the captured substrings
                allocate(capture_array(rc-1))

                ! Extract the substrings
                do i=1, rc-1
                    capture_array(i)%string = subject(this%ovector((i+1)*2-1)+1:this%ovector((i+1)*2))
                end do
            else
                ! No substring was captured
                allocate(capture_array(0))
            end if
        end if

!------ Extract a single captured substring
        if (present(capture)) then
            captureIndex = -1

            ! Get the index of the substring to return
            if (rc > 1 ) then
                ! The first captured string will be returned if no index is provided
                if (present(capture_index)) then
                    captureIndex = capture_index
                else if (present(capture_name)) then
                    allocate(character(len(capture_name)+1) :: c_name)
                    c_name = capture_name // C_NULL_CHAR
                    captureIndex = pcre_get_stringnumber(this%re, c_name)
                    if (captureIndex <= 0) write(*,*) "No captured string was found with the name "//capture_name
                else
                    captureIndex = 1
                end if
            end if

            ! Get the captured substring
            if (captureIndex > 0) then
                capture = subject(this%ovector((captureIndex+1)*2-1)+1:this%ovector((captureIndex+1)*2))
            else
                allocate(character(0) :: capture)
            end if
        end if
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable
!>
!> Match a regular expression to a character variable. The returned
!> index is 0 if no match is found, < 0 in case of an error, and > 0 if no match
!> is found.
!>
!> The difference with the match subroutine is that if a match is found, it will
!> be substituted with the replaceWith string.
!>
!>\see match
!******************************************************************************!
    subroutine replace_all(this, subject, index, length, status)
        class(FRegex), intent(inout) :: this
        character(:), allocatable, intent(inout) :: subject
        integer, dimension(:), allocatable, intent(out) :: index
        integer, dimension(:), allocatable, intent(out), optional :: length
        integer, intent(out), optional :: status
!------
        integer, dimension(:), allocatable :: index_, length_
        integer :: status_, i, l, c
        logical :: global
!------

        ! Setup temporary arrays
        allocate(index_(len(subject)))
        index_ = 0
        allocate(length_(len(subject)))
        length_ = 0

        ! Save the global mode
        global = this%global


        call this%setGlobal(.true.)
        this%offset = 0
        call this%replace(subject, i, l, status_)

        c = 0
        do while(i /= 0 .and. status_ == 0)

            ! Keep the index and length values
            c = c + 1
            index_(c) = i
            length_(c) = l

            ! Try another match
            call this%replace(subject, i, l, status_)
        end do

        ! An error occurred
        if (status_ /= 0) then
            if (present(status)) then
                status = status_
                call this%setGlobal(global)
                return
            else
                write(*,*) "Error while matching pattern: ", fcre_error_message(status_)
                stop
            end if
        end if

        ! Output arrays
        index = index_(:c)
        if (present(length)) length = length_(:c)

        if (present(status)) then
            if (c == 0) then
                status = PCRE_ERROR_NOMATCH
            end if
        end if

        ! Restore the global mode
        call this%setGlobal(global)
    end subroutine
!******************************************************************************!
!>\brief Match a regular expression to a character variable
!>
!> Match a regular expression to a character variable. If no match is found,
!> the index and lengths arrays are not allocated.
!>
!******************************************************************************!
    subroutine match_all(this, subject, index, length, status)
        class(FRegex), intent(inout) :: this
        character(*), intent(in) :: subject
        integer, dimension(:), allocatable, intent(out) :: index
        integer, dimension(:), allocatable, intent(out), optional :: length
        integer, intent(out), optional :: status
!------
        integer, dimension(:), allocatable :: index_, length_
        integer :: status_, i, l, c
        logical :: global
!------

        ! Setup temporary arrays
        allocate(index_(len(subject)))
        index_ = 0
        allocate(length_(len(subject)))
        length_ = 0

        ! Save the global mode
        global = this%global

        call this%setGlobal(.true.)
        this%offset = 0
        call this%match(subject, i, l, status_)
        c = 0
        do while(i /= 0)

            ! An error occurred
            if (status_ /= 0) then
                if (present(status)) then
                    status = status_
                    call this%setGlobal(global)
                    return
                else
                    write(*,*) "Error while matching pattern: ", fcre_error_message(status_)
                    stop
                end if
            end if

            ! Keep the index and length values
            c = c + 1
            index_(c) = i
            length_(c) = l

            ! Try another match
            call this%match(subject, i, l, status_)
        end do

        ! Output arrays
        index = index_(:c)
        if (present(length)) length = length_(:c)

        if (present(status)) then
            if (c == 0) then
                status = PCRE_ERROR_NOMATCH
            end if
        end if

        ! Restore the global mode
        call this%setGlobal(global)
    end subroutine
!******************************************************************************!
!> Get the error message corresponding to an error code
!******************************************************************************!
    function fcre_error_message(code) result(message)
        integer,intent(in) :: code           !< Error code
        character(:), allocatable :: message
!------
        select case(code)
            case (PCRE_ERROR_NOMATCH)
                message = "No match found."
            case (PCRE_ERROR_NULL)
                message = "NULL code or subject or ovector."
            case (PCRE_ERROR_BADOPTION)
                message = "Unrecognized option bit."
            case (FCRE_ERROR_BADOPTION)
                message = "Unrecognized option code."
            case (PCRE_ERROR_BADMAGIC)
                message = "Bad magic number in code."
            case (PCRE_ERROR_UNKNOWN_NODE)
                message = "Bad node in pattern."
            case (FCRE_ERROR_UNSET_PATTERN)
                message = "Pattern not set."
            case (FCRE_ERROR_EMPTY_PATTERN)
                message = "Empty pattern."
            case (FCRE_ERROR_UNSUPPORTED_OPTION)
                message = "Unsupported option in getPatternInfo."
            case default
                message = "Unknown error."
        end select
    end function
!******************************************************************************!
!> Get the position of the bit corresponding to a given option
!******************************************************************************!
    function fcre_option_position(code, status) result(position)
        integer, intent(in) :: code !< Option code
        integer :: position         !< Position in the compilation options
        integer, intent(out), optional :: status         !< Optional error code
!------
        select case(code)
            case (PCRE_CASELESS) !
                position = 0
            case(PCRE_MULTILINE) !
                position = 1
            case(PCRE_DOTALL) !
                position = 2
            case(PCRE_EXTENDED) !
                position = 3
            case(PCRE_ANCHORED) !
                position = 4
            case(PCRE_DOLLAR_ENDONLY) !
                position = 5
            case(PCRE_EXTRA_) !
                position = 6
            case(PCRE_NOTBOL)
                position = 7
            case(PCRE_NOTEOL)
                position = 8
            case(PCRE_UNGREEDY) !
                position = 9
            case(PCRE_NOTEMPTY)
                position = 10
            case(PCRE_UTF8) !
                position = 11
            case(PCRE_NO_AUTO_CAPTURE) !
                position = 12
            case(PCRE_NO_UTF8_CHECK) !
                position = 13
            case(PCRE_AUTO_CALLOUT) !
                position = 14
            case(PCRE_PARTIAL_SOFT) ! also PCRE_PARTIAL
                position = 15
            case(PCRE_NEVER_UTF) ! alse PCRE_DFA_SHORTEST
                position = 16
            case(PCRE_NO_AUTO_POSSESS) ! also PCRE_DFA_RESTART
                position = 17
            case(PCRE_FIRSTLINE) !
                position = 18
            case(PCRE_DUPNAMES) !
                position = 19
            case(PCRE_NEWLINE_CR) !
                position = 20
            case(PCRE_NEWLINE_LF) !
                position = 21
            case(PCRE_NEWLINE_CRLF) !
                position = -1 !FIXME: Combined options are not supported yet
            case(PCRE_NEWLINE_ANY) !
                position = 22
            case(PCRE_NEWLINE_ANYCRLF) !
                position = -1 !FIXME: Combined options are not supported yet
            case(PCRE_BSR_ANYCRLF) !
                position = 23
            case(PCRE_BSR_UNICODE) !
                position = 24
            case(PCRE_JAVASCRIPT_COMPAT) !
                position = 25
            case(PCRE_NO_START_OPTIMISE) ! also PCRE_NO_START_OPTIMIZE
                position = 26
            case(PCRE_PARTIAL_HARD)
                position = 27
            case(PCRE_NOTEMPTY_ATSTART)
                position = 28
            case(PCRE_UCP) !
                position = 29
            case default
                position = -1
        end select

        ! Error check
        if (position < 0) then
            if (present(status)) then
                status = FCRE_ERROR_BADOPTION
                return
            else
                write(*,*) fcre_error_message(FCRE_ERROR_BADOPTION)
                stop
            end if
        end if
    end function
end module
