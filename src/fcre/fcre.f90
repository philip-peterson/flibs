!******************************************************************************!
!>                   Fortran-Compatible Regular Expressions
!------------------------------------------------------------------------------!
!>
!> Fortran bindings for the PCRE library.
!>
!> Written by Paul Fossati, <paul.fossati@gmail.com>
!> Copyright (c) 2014 Paul Fossati
!>
!> Based on the pcre.h header from the PCRE library written by Philip Hazel,
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
module fcre
  use iso_c_binding
  implicit none
  public
  save
!******************************************************************************!
! FCRE-specific error codes
!******************************************************************************!
  integer(c_int), parameter :: FCRE_ERROR = -100
  integer(c_int), parameter :: FCRE_ERROR_BADOPTION = -101
  integer(c_int), parameter :: FCRE_ERROR_UNSET_PATTERN = -102
  integer(c_int), parameter :: FCRE_ERROR_EMPTY_PATTERN = -103
  integer(c_int), parameter :: FCRE_ERROR_UNSUPPORTED_OPTION= -104
  integer(c_int), parameter :: FCRE_REF_BEFORE = -1
  integer(c_int), parameter :: FCRE_REF_AFTER = -2
  integer(c_int), parameter :: FCRE_REF_LAST = -3
  integer(c_int), parameter :: FCRE_REF_MATCH = -4
!******************************************************************************!
! Parameters from pcre.h
!******************************************************************************!
integer(c_int), parameter :: PCRE_CASELESS = z'00000001'
integer(c_int), parameter :: PCRE_MULTILINE = z'00000002'
integer(c_int), parameter :: PCRE_DOTALL = z'00000004'
integer(c_int), parameter :: PCRE_EXTENDED = z'00000008'
integer(c_int), parameter :: PCRE_ANCHORED = z'00000010'
integer(c_int), parameter :: PCRE_DOLLAR_ENDONLY = z'00000020'
integer(c_int), parameter :: PCRE_EXTRA_ = z'00000040'
integer(c_int), parameter :: PCRE_NOTBOL = z'00000080'
integer(c_int), parameter :: PCRE_NOTEOL = z'00000100'
integer(c_int), parameter :: PCRE_UNGREEDY = z'00000200'
integer(c_int), parameter :: PCRE_NOTEMPTY = z'00000400'
integer(c_int), parameter :: PCRE_UTF8 = z'00000800'
integer(c_int), parameter :: PCRE_UTF16 = z'00000800'
integer(c_int), parameter :: PCRE_UTF32 = z'00000800'
integer(c_int), parameter :: PCRE_NO_AUTO_CAPTURE = z'00001000'
integer(c_int), parameter :: PCRE_NO_UTF8_CHECK = z'00002000'
integer(c_int), parameter :: PCRE_NO_UTF16_CHECK = z'00002000'
integer(c_int), parameter :: PCRE_NO_UTF32_CHECK = z'00002000'
integer(c_int), parameter :: PCRE_AUTO_CALLOUT = z'00004000'
integer(c_int), parameter :: PCRE_PARTIAL_SOFT = z'00008000'
integer(c_int), parameter :: PCRE_PARTIAL = z'00008000'
integer(c_int), parameter :: PCRE_NEVER_UTF = z'00010000'
integer(c_int), parameter :: PCRE_DFA_SHORTEST = z'00010000'
integer(c_int), parameter :: PCRE_NO_AUTO_POSSESS = z'00020000'
integer(c_int), parameter :: PCRE_DFA_RESTART = z'00020000'
integer(c_int), parameter :: PCRE_FIRSTLINE = z'00040000'
integer(c_int), parameter :: PCRE_DUPNAMES = z'00080000'
integer(c_int), parameter :: PCRE_NEWLINE_CR = z'00100000'
integer(c_int), parameter :: PCRE_NEWLINE_LF = z'00200000'
integer(c_int), parameter :: PCRE_NEWLINE_CRLF = z'00300000'
integer(c_int), parameter :: PCRE_NEWLINE_ANY = z'00400000'
integer(c_int), parameter :: PCRE_NEWLINE_ANYCRLF = z'00500000'
integer(c_int), parameter :: PCRE_BSR_ANYCRLF = z'00800000'
integer(c_int), parameter :: PCRE_BSR_UNICODE = z'01000000'
integer(c_int), parameter :: PCRE_JAVASCRIPT_COMPAT = z'02000000'
integer(c_int), parameter :: PCRE_NO_START_OPTIMIZE = z'04000000'
integer(c_int), parameter :: PCRE_NO_START_OPTIMISE = z'04000000'
integer(c_int), parameter :: PCRE_PARTIAL_HARD = z'08000000'
integer(c_int), parameter :: PCRE_NOTEMPTY_ATSTART = z'10000000'
integer(c_int), parameter :: PCRE_UCP = z'20000000'
integer(c_int), parameter :: PCRE_ERROR_NOMATCH = -1
integer(c_int), parameter :: PCRE_ERROR_NULL = -2
integer(c_int), parameter :: PCRE_ERROR_BADOPTION = -3
integer(c_int), parameter :: PCRE_ERROR_BADMAGIC = -4
integer(c_int), parameter :: PCRE_ERROR_UNKNOWN_OPCODE = -5
integer(c_int), parameter :: PCRE_ERROR_UNKNOWN_NODE = -5
integer(c_int), parameter :: PCRE_ERROR_NOMEMORY = -6
integer(c_int), parameter :: PCRE_ERROR_NOSUBSTRING = -7
integer(c_int), parameter :: PCRE_ERROR_MATCHLIMIT = -8
integer(c_int), parameter :: PCRE_ERROR_CALLOUT = -9
integer(c_int), parameter :: PCRE_ERROR_BADUTF8 = -10
integer(c_int), parameter :: PCRE_ERROR_BADUTF16 = -10
integer(c_int), parameter :: PCRE_ERROR_BADUTF32 = -10
integer(c_int), parameter :: PCRE_ERROR_BADUTF8_OFFSET = -11
integer(c_int), parameter :: PCRE_ERROR_BADUTF16_OFFSET = -11
integer(c_int), parameter :: PCRE_ERROR_PARTIAL = -12
integer(c_int), parameter :: PCRE_ERROR_BADPARTIAL = -13
integer(c_int), parameter :: PCRE_ERROR_INTERNAL = -14
integer(c_int), parameter :: PCRE_ERROR_BADCOUNT = -15
integer(c_int), parameter :: PCRE_ERROR_DFA_UITEM = -16
integer(c_int), parameter :: PCRE_ERROR_DFA_UCOND = -17
integer(c_int), parameter :: PCRE_ERROR_DFA_UMLIMIT = -18
integer(c_int), parameter :: PCRE_ERROR_DFA_WSSIZE = -19
integer(c_int), parameter :: PCRE_ERROR_DFA_RECURSE = -20
integer(c_int), parameter :: PCRE_ERROR_RECURSIONLIMIT = -21
integer(c_int), parameter :: PCRE_ERROR_NULLWSLIMIT = -22
integer(c_int), parameter :: PCRE_ERROR_BADNEWLINE = -23
integer(c_int), parameter :: PCRE_ERROR_BADOFFSET = -24
integer(c_int), parameter :: PCRE_ERROR_SHORTUTF8 = -25
integer(c_int), parameter :: PCRE_ERROR_SHORTUTF16 = -25
integer(c_int), parameter :: PCRE_ERROR_RECURSELOOP = -26
integer(c_int), parameter :: PCRE_ERROR_JIT_STACKLIMIT = -27
integer(c_int), parameter :: PCRE_ERROR_BADMODE = -28
integer(c_int), parameter :: PCRE_ERROR_BADENDIANNESS = -29
integer(c_int), parameter :: PCRE_ERROR_DFA_BADRESTART = -30
integer(c_int), parameter :: PCRE_ERROR_JIT_BADOPTION = -31
integer(c_int), parameter :: PCRE_ERROR_BADLENGTH = -32
integer(c_int), parameter :: PCRE_ERROR_UNSET = -33
integer(c_int), parameter :: PCRE_UTF8_ERR0 = 0
integer(c_int), parameter :: PCRE_UTF8_ERR1 = 1
integer(c_int), parameter :: PCRE_UTF8_ERR2 = 2
integer(c_int), parameter :: PCRE_UTF8_ERR3 = 3
integer(c_int), parameter :: PCRE_UTF8_ERR4 = 4
integer(c_int), parameter :: PCRE_UTF8_ERR5 = 5
integer(c_int), parameter :: PCRE_UTF8_ERR6 = 6
integer(c_int), parameter :: PCRE_UTF8_ERR7 = 7
integer(c_int), parameter :: PCRE_UTF8_ERR8 = 8
integer(c_int), parameter :: PCRE_UTF8_ERR9 = 9
integer(c_int), parameter :: PCRE_UTF8_ERR10 = 10
integer(c_int), parameter :: PCRE_UTF8_ERR11 = 11
integer(c_int), parameter :: PCRE_UTF8_ERR12 = 12
integer(c_int), parameter :: PCRE_UTF8_ERR13 = 13
integer(c_int), parameter :: PCRE_UTF8_ERR14 = 14
integer(c_int), parameter :: PCRE_UTF8_ERR15 = 15
integer(c_int), parameter :: PCRE_UTF8_ERR16 = 16
integer(c_int), parameter :: PCRE_UTF8_ERR17 = 17
integer(c_int), parameter :: PCRE_UTF8_ERR18 = 18
integer(c_int), parameter :: PCRE_UTF8_ERR19 = 19
integer(c_int), parameter :: PCRE_UTF8_ERR20 = 20
integer(c_int), parameter :: PCRE_UTF8_ERR21 = 21
integer(c_int), parameter :: PCRE_UTF8_ERR22 = 22
integer(c_int), parameter :: PCRE_UTF16_ERR0 = 0
integer(c_int), parameter :: PCRE_UTF16_ERR1 = 1
integer(c_int), parameter :: PCRE_UTF16_ERR2 = 2
integer(c_int), parameter :: PCRE_UTF16_ERR3 = 3
integer(c_int), parameter :: PCRE_UTF16_ERR4 = 4
integer(c_int), parameter :: PCRE_UTF32_ERR0 = 0
integer(c_int), parameter :: PCRE_UTF32_ERR1 = 1
integer(c_int), parameter :: PCRE_UTF32_ERR2 = 2
integer(c_int), parameter :: PCRE_UTF32_ERR3 = 3
integer(c_int), parameter :: PCRE_INFO_OPTIONS = 0
integer(c_int), parameter :: PCRE_INFO_SIZE = 1
integer(c_int), parameter :: PCRE_INFO_CAPTURECOUNT = 2
integer(c_int), parameter :: PCRE_INFO_BACKREFMAX = 3
integer(c_int), parameter :: PCRE_INFO_FIRSTBYTE = 4
integer(c_int), parameter :: PCRE_INFO_FIRSTCHAR = 4
integer(c_int), parameter :: PCRE_INFO_FIRSTTABLE = 5
integer(c_int), parameter :: PCRE_INFO_LASTLITERAL = 6
integer(c_int), parameter :: PCRE_INFO_NAMEENTRYSIZE = 7
integer(c_int), parameter :: PCRE_INFO_NAMECOUNT = 8
integer(c_int), parameter :: PCRE_INFO_NAMETABLE = 9
integer(c_int), parameter :: PCRE_INFO_STUDYSIZE = 10
integer(c_int), parameter :: PCRE_INFO_DEFAULT_TABLES = 11
integer(c_int), parameter :: PCRE_INFO_OKPARTIAL = 12
integer(c_int), parameter :: PCRE_INFO_JCHANGED = 13
integer(c_int), parameter :: PCRE_INFO_HASCRORLF = 14
integer(c_int), parameter :: PCRE_INFO_MINLENGTH = 15
integer(c_int), parameter :: PCRE_INFO_JIT = 16
integer(c_int), parameter :: PCRE_INFO_JITSIZE = 17
integer(c_int), parameter :: PCRE_INFO_MAXLOOKBEHIND = 18
integer(c_int), parameter :: PCRE_INFO_FIRSTCHARACTER = 19
integer(c_int), parameter :: PCRE_INFO_FIRSTCHARACTERFLAGS = 20
integer(c_int), parameter :: PCRE_INFO_REQUIREDCHAR = 21
integer(c_int), parameter :: PCRE_INFO_REQUIREDCHARFLAGS = 22
integer(c_int), parameter :: PCRE_INFO_MATCHLIMIT = 23
integer(c_int), parameter :: PCRE_INFO_RECURSIONLIMIT = 24
integer(c_int), parameter :: PCRE_INFO_MATCH_EMPTY = 25
integer(c_int), parameter :: PCRE_CONFIG_UTF8 = 0
integer(c_int), parameter :: PCRE_CONFIG_NEWLINE = 1
integer(c_int), parameter :: PCRE_CONFIG_LINK_SIZE = 2
integer(c_int), parameter :: PCRE_CONFIG_POSIX_MALLOC_THRESHOLD = 3
integer(c_int), parameter :: PCRE_CONFIG_MATCH_LIMIT = 4
integer(c_int), parameter :: PCRE_CONFIG_STACKRECURSE = 5
integer(c_int), parameter :: PCRE_CONFIG_UNICODE_PROPERTIES = 6
integer(c_int), parameter :: PCRE_CONFIG_MATCH_LIMIT_RECURSION = 7
integer(c_int), parameter :: PCRE_CONFIG_BSR = 8
integer(c_int), parameter :: PCRE_CONFIG_JIT = 9
integer(c_int), parameter :: PCRE_CONFIG_UTF16 = 10
integer(c_int), parameter :: PCRE_CONFIG_JITTARGET = 11
integer(c_int), parameter :: PCRE_CONFIG_UTF32 = 12
integer(c_int), parameter :: PCRE_STUDY_JIT_COMPILE = z'0001'
integer(c_int), parameter :: PCRE_STUDY_JIT_PARTIAL_SOFT_COMPILE = z'0002'
integer(c_int), parameter :: PCRE_STUDY_JIT_PARTIAL_HARD_COMPILE = z'0004'
integer(c_int), parameter :: PCRE_STUDY_EXTRA_NEEDED = z'0008'
integer(c_int), parameter :: PCRE_EXTRA_STUDY_DATA = z'0001'
integer(c_int), parameter :: PCRE_EXTRA_MATCH_LIMIT = z'0002'
integer(c_int), parameter :: PCRE_EXTRA_CALLOUT_DATA = z'0004'
integer(c_int), parameter :: PCRE_EXTRA_TABLES = z'0008'
integer(c_int), parameter :: PCRE_EXTRA_MATCH_LIMIT_RECURSION = z'0010'
integer(c_int), parameter :: PCRE_EXTRA_MARK = z'0020'
integer(c_int), parameter :: PCRE_EXTRA_EXECUTABLE_JIT = z'0040'
!******************************************************************************!
! Derived types
!******************************************************************************!
! pcre_extra { unsigned long int flags; void *study_data; unsigned long int match_limit; void *callout_data; const unsigned char *tables; unsigned long int match_limit_recursion; unsigned char **mark; void *executable_jit;}
  type, bind(C) :: pcre_extra
    integer(c_long) :: flags
    type(c_ptr) :: study_data
    integer(c_long) :: match_limit
    type(c_ptr) :: callout_data
    type(c_ptr) :: tables
    integer(c_long) :: match_limit_recursion
    type(c_ptr) :: mark
    type(c_ptr) :: executable_jit
  end type pcre_extra
! pcre_callout_block { int version; int callout_number; int *offset_vector; const char * subject; int subject_length; int start_match; int current_position; int capture_top; int capture_last; void *callout_data; int pattern_position; int next_item_length; const unsigned char *mark;}
  type, bind(C) :: pcre_callout_block
    integer(c_int) :: version
    integer(c_int) :: callout_number
    type(c_ptr) :: offset_vector
    type(c_ptr) :: subject
    integer(c_int) :: subject_length
    integer(c_int) :: start_match
    integer(c_int) :: current_position
    integer(c_int) :: capture_top
    integer(c_int) :: capture_last
    type(c_ptr) :: callout_data
    integer(c_int) :: pattern_position
    integer(c_int) :: next_item_length
    type(c_ptr) :: mark
  end type pcre_callout_block
  interface
!******************************************************************************!
! Wrapper interfaces
!******************************************************************************!
  function pcre_malloc(size) bind(C, name="pcre_malloc_wrapper")
    import
    type(c_ptr) :: pcre_malloc
    integer(c_size_t), value :: size
  end function
  
  function pcre_stack_malloc(size) bind(C, name="pcre_stack_malloc_wrapper")
    import
    integer(c_size_t), value :: size
    type(c_ptr) :: pcre_stack_malloc
  end function

  function pcre_callout(block) bind(C, name="cre_callout_wrapper")
    import
    type(c_ptr), value :: block
    integer(c_int) :: pcre_callout
  end function

  subroutine pcre_free(regex) bind(C, name="pcre_free_wrapper")
    import
    type(c_ptr), value :: regex
  end subroutine

  subroutine pcre_stack_free(regex) bind(C, name="pcre_stack_free_wrapper")
    import
    type(c_ptr), value :: regex
  end subroutine

!******************************************************************************!
! Subroutine interfaces
!******************************************************************************!
  ! pcre_free_substring (const char *)
      subroutine pcre_free_substring(arg) &
          bind(C)
        import
        character(kind=c_char), dimension(*) :: arg
      end subroutine pcre_free_substring
  ! pcre_free_substring_list (const char **)
      subroutine pcre_free_substring_list(arg) &
          bind(C)
        import
        type(c_ptr), value :: arg
      end subroutine pcre_free_substring_list
  ! pcre_free_study (pcre_extra *)
      subroutine pcre_free_study(arg) &
          bind(C)
        import
        type(c_ptr), value :: arg
      end subroutine pcre_free_study
  ! pcre_jit_stack_free (pcre_jit_stack *)
      subroutine pcre_jit_stack_free(arg) &
          bind(C)
        import
        type(c_ptr), value :: arg
      end subroutine pcre_jit_stack_free

!******************************************************************************!
! Function interfaces
!******************************************************************************!
! pcre * pcre_compile (const char *, int, const char **, int *, const unsigned char *)
    function pcre_compile(pattern, options, errptr, erroffset, tableptr) &
        bind(C)
      import
      type(c_ptr) :: pcre_compile
      character(kind=c_char), dimension(*) :: pattern
      integer(c_int), value :: options
      type(c_ptr) :: errptr
      integer(c_int), intent(inout) :: erroffset
      type(c_ptr), value :: tableptr
    end function pcre_compile
! pcre * pcre_compile2 (const char *, int, int *, const char **, int *, const unsigned char *)
    function pcre_compile2(pattern, options, error, errptr, erroffset, tableptr) &
        bind(C)
      import
      type(c_ptr) :: pcre_compile2
      character(kind=c_char), dimension(*) :: pattern
      integer(c_int), value :: options
      integer(c_int) :: error
      type(c_ptr) :: errptr
      integer(c_int), intent(inout) :: erroffset
      type(c_ptr), value :: tableptr
    end function pcre_compile2
! int  pcre_config (int, void *)
    function pcre_config(what, where) &
        bind(C)
      import
      integer(c_int) :: pcre_config
      integer(c_int), value :: what
      type(c_ptr), value :: where
    end function pcre_config
! int  pcre_copy_named_substring (const pcre *, const char *, int *, int, const char *, char *, int)
    function pcre_copy_named_substring(arg1, arg2, arg3, arg4, arg5, arg6, arg7) &
        bind(C)
      import
      integer(c_int) :: pcre_copy_named_substring
      type(c_ptr), value :: arg1
      character(kind=c_char), dimension(*) :: arg2
      type(c_ptr), value :: arg3
      integer(c_int), value :: arg4
      character(kind=c_char), dimension(*) :: arg5
      type(c_ptr), value :: arg6
      integer(c_int), value :: arg7
    end function pcre_copy_named_substring
! int  pcre_copy_substring (const char *, int *, int, int, char *, int)
    function pcre_copy_substring(arg1, arg2, arg3, arg4, arg5, arg6) &
        bind(C)
      import
      integer(c_int) :: pcre_copy_substring
      character(kind=c_char), dimension(*) :: arg1
      type(c_ptr), value :: arg2
      integer(c_int), value :: arg3
      integer(c_int), value :: arg4
      type(c_ptr), value :: arg5
      integer(c_int), value :: arg6
    end function pcre_copy_substring
! int  pcre_dfa_exec (const pcre *, const pcre_extra *, const char *, int, int, int, int *, int , int *, int)
    function pcre_dfa_exec(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10) &
        bind(C)
      import
      integer(c_int) :: pcre_dfa_exec
      type(c_ptr), value :: arg1
      type(c_ptr), value :: arg2
      character(kind=c_char), dimension(*) :: arg3
      integer(c_int), value :: arg4
      integer(c_int), value :: arg5
      integer(c_int), value :: arg6
      type(c_ptr), value :: arg7
      integer(c_int), value :: arg8
      type(c_ptr), value :: arg9
      integer(c_int), value :: arg10
    end function pcre_dfa_exec
    ! int  pcre_exec (const pcre *, const pcre_extra *, const char *, int, int, int, int *, int)
    function pcre_exec(code, extra, subject, length, startoffset, options, ovector, ovecsize) &
        bind(C)
      import
      integer(c_int) :: pcre_exec
      type(c_ptr), value :: code
      type(c_ptr), value :: extra
      character(kind=c_char), dimension(*) :: subject
      integer(c_int), value :: length
      integer(c_int), value :: startoffset
      integer(c_int), value :: options
      integer(c_int), dimension(*) :: ovector
      integer(c_int), value :: ovecsize
    end function pcre_exec
! int  pcre_jit_exec (const pcre *, const pcre_extra *, const char *, int, int, int, int *, int, pcre_jit_stack *)
    function pcre_jit_exec(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) &
        bind(C)
      import
      integer(c_int) :: pcre_jit_exec
      type(c_ptr), value :: arg1
      type(c_ptr), value :: arg2
      character(kind=c_char), dimension(*) :: arg3
      integer(c_int), value :: arg4
      integer(c_int), value :: arg5
      integer(c_int), value :: arg6
      type(c_ptr), value :: arg7
      integer(c_int), value :: arg8
      type(c_ptr), value :: arg9
    end function pcre_jit_exec
! int  pcre_fullinfo (const pcre *, const pcre_extra *, int, void *)
    function pcre_fullinfo(code, extra, what, where) &
        bind(C)
      import
      integer(c_int) :: pcre_fullinfo
      type(c_ptr), value :: code
      type(c_ptr), value :: extra
      integer(c_int), value :: what
      type(c_ptr), value :: where
    end function pcre_fullinfo
! int  pcre_get_named_substring (const pcre *, const char *, int *, int, const char *, const char **)
    function pcre_get_named_substring(arg1, arg2, arg3, arg4, arg5, arg6) &
        bind(C)
      import
      integer(c_int) :: pcre_get_named_substring
      type(c_ptr), value :: arg1
      character(kind=c_char), dimension(*) :: arg2
      type(c_ptr), value :: arg3
      integer(c_int), value :: arg4
      character(kind=c_char), dimension(*) :: arg5
      type(c_ptr), value :: arg6
    end function pcre_get_named_substring
! int  pcre_get_stringnumber (const pcre *, const char *)
    function pcre_get_stringnumber(code, name) &
        bind(C)
      import
      integer(c_int) :: pcre_get_stringnumber
      type(c_ptr), value :: code
      character(kind=c_char), dimension(*) :: name
    end function pcre_get_stringnumber
! int  pcre_get_stringtable_entries (const pcre *, const char *, char **, char **)
    function pcre_get_stringtable_entries(arg1, arg2, arg3, arg4) &
        bind(C)
      import
      integer(c_int) :: pcre_get_stringtable_entries
      type(c_ptr), value :: arg1
      character(kind=c_char), dimension(*) :: arg2
      type(c_ptr), value :: arg3
      type(c_ptr), value :: arg4
    end function pcre_get_stringtable_entries
! int  pcre_get_substring (const char *, int *, int, int, const char **)
    function pcre_get_substring(subject, ovector, stringcount, stringnumber, stringptr) &
        bind(C)
      import
      integer(c_int) :: pcre_get_substring
      character(kind=c_char), dimension(*) :: subject
      integer(c_int), dimension(*) :: ovector
      integer(c_int), value :: stringcount
      integer(c_int), value :: stringnumber
      type(c_ptr), value :: stringptr
    end function pcre_get_substring
! int  pcre_refcount (pcre *, int)
    function pcre_refcount(arg1, arg2) &
        bind(C)
      import
      integer(c_int) :: pcre_refcount
      type(c_ptr), value :: arg1
      integer(c_int), value :: arg2
    end function pcre_refcount
! pcre_extra * pcre_study (const pcre *, int, const char **)
    function pcre_study(code, options, errptr) &
        bind(C)
      import
      type(c_ptr) :: pcre_study
      type(c_ptr), value :: code
      integer(c_int), value :: options
      type(c_ptr) :: errptr
    end function pcre_study
! const char * pcre_version (void)
    function pcre_version() &
        bind(C)
      import
      type(c_ptr) :: pcre_version
    end function pcre_version
! int  pcre_pattern_to_host_byte_order (pcre *, pcre_extra *, const unsigned char *)
    function pcre_pattern_to_host_byte_order(arg1, arg2, arg3) &
        bind(C)
      import
      integer(c_int) :: pcre_pattern_to_host_byte_order
      type(c_ptr), value :: arg1
      type(c_ptr), value :: arg2
      character(kind=c_char), dimension(*) :: arg3
    end function pcre_pattern_to_host_byte_order
! pcre_jit_stack * pcre_jit_stack_alloc (int, int)
    function pcre_jit_stack_alloc(arg1, arg2) &
        bind(C)
      import
      type(c_ptr) :: pcre_jit_stack_alloc
      integer(c_int), value :: arg1
      integer(c_int), value :: arg2
    end function pcre_jit_stack_alloc
  end interface
contains
end module fcre
