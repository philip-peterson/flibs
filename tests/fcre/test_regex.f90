!******************************************************************************!
!>                             test_regex module
!------------------------------------------------------------------------------!
!>
!> Unit tests for the mod_regex module of the FCRE library. Uses FUnit from the
!> FLIBS project, available at http://flibs.sourceforge.net
!>
!> Written by Paul Fossati, <paul.fossati@gmail.com>
!> Copyright (c) 2014 Paul Fossati
!>
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
module test_regex
    use mod_regex
    use ftnunit
    implicit none
    private
    save

    public :: test_all
contains
    subroutine test_all()

        call test(test_match_simple, "simple pattern matching")
        call test(test_match_caseless, "case insensitive pattern matching")
        call test(test_match_reluctant, "reluctant pattern matching")
        call test(test_match_global, "global mode pattern matching")
        call test(test_substitution_simple, "substitution: simple")
        call test(test_substitution_backref, "substitution: back references")
        call test(test_substitution_variable_1, "substitution: $`")
        call test(test_substitution_variable_2, "substitution: $'")
        call test(test_substitution_variable_3, "substitution: $+")
        call test(test_substitution_variable_4, "substitution: $&")
        call test(test_substitution_caseless, "case insensitive substring substitution")
    end subroutine

    subroutine test_match_simple()
        type(FRegex) :: re
        integer :: index, length, status
        type(FString), dimension(:), allocatable :: captures

        ! Not accepted by Intel Fortran
        ! re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b')
        call re%setCaseSensitive(.true.)
        call re%match('sd dmoir DZ54 po32 54 era312', index, captures, length, status)

        call assert_equal(status, 0, "error while matching")
        call assert_equal(index, 15, "matched substring index")
        call assert_equal(length, 4, "matched substring length")
        call assert_equal(size(captures), 3, "number of captured substrings")
        call assert_true(captures(1)%string == "po", "first captured substring")
        call assert_true(captures(2)%string == "3", "second captured substring")
        call assert_true(captures(3)%string == "2", "third captured substring")
    end subroutine

    subroutine test_substitution_simple()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '____')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '____')
        call re%setCaseSensitive(.true.)
        call re%replace(subject, index)

        call assert_equal(index, 15, "matched substring index")
        call assert_true(subject == "sd dmoir DZ54 ____ 54 era312", "substituted string")
    end subroutine

    subroutine test_substitution_backref()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '$2 $3 $1')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '$2 $3 $1')
        call re%setCaseSensitive(.true.)
        call re%replace(subject, index)

        call assert_equal(index, 15, "matched substring index")
        call assert_true(subject == "sd dmoir DZ54 3 2 po 54 era312", "substituted string")
    end subroutine

    subroutine test_substitution_variable_1()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$`/')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$`/')
        call re%replace(subject, index)

        call assert_equal(index, 15, "matched substring index")
        call assert_true(subject == "sd dmoir DZ54 /sd dmoir DZ54 / 54 era312", "correct substituted string")
    end subroutine

    subroutine test_substitution_variable_2()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$''/')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$''/')
        call re%replace(subject, index)

        call assert_equal(index, 15, "matched substring index")
        call assert_true(subject == "sd dmoir DZ54 / 54 era312/ 54 era312", "substituted string")
    end subroutine

    subroutine test_substitution_variable_3()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$+/')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$+/')
        call re%replace(subject, index)

        call assert_equal(index, 15, "matched substring index")
        call assert_true(subject == "sd dmoir DZ54 /2/ 54 era312", "substituted string")
    end subroutine

    subroutine test_substitution_variable_4()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$&/')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '/$&/')
        call re%replace(subject, index)

        call assert_equal(index, 15, "matched substring index")
        call assert_true(subject == "sd dmoir DZ54 /po32/ 54 era312", "substituted string")
    end subroutine

    subroutine test_match_caseless()
        type(FRegex) :: re
        integer :: index, length

        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b')
        call re%setCaseSensitive(.false.)
        call re%match('sd dmoir DZ54 po32 54 era312', index, length)
        call assert_equal(index, 10, "matched substring index")
        call assert_equal(length, 4, "matched substring length")
    end subroutine

    subroutine test_substitution_caseless()
        type(FRegex) :: re
        integer :: index
        character(:), allocatable :: subject

        subject = 'sd dmoir DZ54 po32 54 era312'
        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '$2 $3 $1')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b', '$2 $3 $1')
        call re%setCaseSensitive(.false.)
        call re%replace(subject, index)
        call assert_equal(index, 10, "matched substring index")
        call assert_true(subject == "sd dmoir 5 4 DZ po32 54 era312", "substituted string")
    end subroutine

    subroutine test_match_reluctant()
        type(FRegex) :: re
        integer :: index, length

        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b\s*')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b\s*')
        call re%setReluctant
        call re%match('sd dmoir DZ54 po32 54 era312', index, length)
        call assert_equal(index, 15, "matched substring index")
        call assert_equal(length, 4, "matched substring length (reluctant)")

        call re%setReluctant(.false.)
        call re%match('sd dmoir DZ54 po32 54 era312', index, length)
        call assert_equal(length, 5, "matched substring length (greedy)")
    end subroutine

    subroutine test_match_global()
        type(FRegex) :: re
        integer :: index, length, i
        integer, dimension(2), parameter :: vi = [15, 20]
        integer, dimension(2), parameter :: vl = [4, 2]

        !re = FRegex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b')
        re = fcre_new_regex('\b(?<c>[a-z]*)(?<a>[0-9])(?<b>[0-9])\b')
        call re%setGlobal
        do i=1, 2
            call re%match('sd dmoir DZ54 po32 54 era312', index, length)
            call assert_equal(index, vi(i), "matched substring index")
            call assert_equal(length, vl(i), "matched substring length")
        end do
    end subroutine
end module

! Main program
program test_program
    use test_regex
    use ftnunit
    implicit none

    call runtests(test_all)
end program
