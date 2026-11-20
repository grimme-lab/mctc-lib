! This file is part of mctc-lib.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.

!> Unit tests for `.CHRG` and `.UHF` auxiliary dot-file readers
module test_dotfiles
   use mctc_env, only : wp, dirname, join_path, is_windows
   use mctc_env_testing, only : new_unittest, unittest_type, error_type, &
      & check, test_failed
   use mctc_io, only : structure_type, read_dotfiles, read_chrg, read_uhf
   use testsuite_structure, only : get_structure
   implicit none
   private

   public :: collect_dotfiles

   real(wp), parameter :: thr = 100 * epsilon(1.0_wp)


contains


!> Collect all exported unit tests
subroutine collect_dotfiles(testsuite)

   !> Collection of tests
   type(unittest_type), allocatable, intent(out) :: testsuite(:)

   testsuite = [ &
      & new_unittest("path-helpers", test_path_helpers), &
      & new_unittest("chrg-valid", test_chrg_valid), &
      & new_unittest("uhf-valid", test_uhf_valid), &
      & new_unittest("dotfiles-lookup", test_dotfiles_lookup), &
      & new_unittest("chrg-empty", test_chrg_empty, should_fail=.true.), &
      & new_unittest("chrg-invalid", test_chrg_invalid), &
      & new_unittest("chrg-trailing", test_chrg_trailing, should_fail=.true.), &
      & new_unittest("chrg-missing", test_chrg_missing, should_fail=.true.), &
      & new_unittest("uhf-float", test_uhf_float, should_fail=.true.), &
      & new_unittest("uhf-trailing", test_uhf_trailing, should_fail=.true.), &
      & new_unittest("uhf-missing", test_uhf_missing, should_fail=.true.) &
      & ]

end subroutine collect_dotfiles


!> Verify dirname and join_path exported from mctc_env
subroutine test_path_helpers(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character :: sep

   sep = merge("\", "/", is_windows())

   call check(error, dirname("mol.xyz"), ".")
   if (allocated(error)) return

   call check(error, dirname("-"), ".")
   if (allocated(error)) return

   call check(error, dirname(""), ".")
   if (allocated(error)) return

   call check(error, dirname("sub/mol.xyz"), "sub")
   if (allocated(error)) return

   call check(error, dirname("a/b//mol.xyz"), "a/b")
   if (allocated(error)) return

   call check(error, dirname("/mol.xyz"), "/")
   if (allocated(error)) return

   call check(error, dirname("C:\mol.xyz"), "C:\")
   if (allocated(error)) return

   call check(error, dirname("C:\\mol.xyz"), "C:\")
   if (allocated(error)) return

   call check(error, join_path("", ".CHRG"), ".CHRG")
   if (allocated(error)) return

   call check(error, join_path(".", ".CHRG"), "." // sep // ".CHRG")
   if (allocated(error)) return

   call check(error, join_path("sub/", ".CHRG"), "sub/.CHRG")
   if (allocated(error)) return

   call check(error, join_path("/", ".CHRG"), "/.CHRG")

end subroutine test_path_helpers


!> Verify integer, decimal, scientific, comment, and no-newline .CHRG inputs
subroutine test_chrg_valid(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: fname
   integer :: unit
   real(wp) :: q

   call parse_chrg(" -2 ", q, error)
   if (allocated(error)) return

   call check(error, q, -2.0_wp, thr=thr)
   if (allocated(error)) return

   call parse_chrg("-1.0", q, error)
   if (allocated(error)) return

   call check(error, q, -1.0_wp, thr=thr)
   if (allocated(error)) return

   open(status="scratch", newunit=unit)
   write(unit, "(a)") &
      & "# header comment", &
      & "   ", &
      & "  ! another comment", &
      & "  +2.5D-01   # quarter electron"
   rewind(unit)
   call read_chrg(q, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, q, 0.25_wp, thr=thr)
   if (allocated(error)) return

   ! File overload on exact byte stream without final newline
   fname = get_name()
   call write_raw(fname, "-0.5")
   call read_chrg(q, fname, error)
   call delete_file(fname)
   if (allocated(error)) return

   call check(error, q, -0.5_wp, thr=thr)

end subroutine test_chrg_valid


!> Verify integer .UHF inputs from units and no-newline files
subroutine test_uhf_valid(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=:), allocatable :: fname
   integer :: unit, uhf

   open(status="scratch", newunit=unit)
   write(unit, "(a)") "# quartet", "+3  ! spin"
   rewind(unit)
   call read_uhf(uhf, unit, error)
   close(unit)
   if (allocated(error)) return

   call check(error, uhf, 3)
   if (allocated(error)) return

   fname = get_name()
   call write_raw(fname, "  2")
   call read_uhf(uhf, fname, error)
   call delete_file(fname)
   if (allocated(error)) return

   call check(error, uhf, 2)

end subroutine test_uhf_valid


!> Verify read_dotfiles in an isolated temporary directory
subroutine test_dotfiles_lookup(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(error_type), allocatable :: cleanup_error
   character(len=:), allocatable :: tmpdir

   call make_temp_dir(tmpdir, error)
   if (allocated(error)) return

   call run_dotfiles_lookup(tmpdir, error)
   call delete_file(join_path(tmpdir, ".CHRG"))
   call delete_file(join_path(tmpdir, ".UHF"))
   call remove_temp_dir(tmpdir, cleanup_error)
   if (.not. allocated(error) .and. allocated(cleanup_error)) then
      call move_alloc(cleanup_error, error)
   end if

end subroutine test_dotfiles_lookup


!> Run directory-lookup, flag, and error-rollback checks inside tmpdir
subroutine run_dotfiles_lookup(tmpdir, error)

   !> Isolated temporary directory
   character(len=*), intent(in) :: tmpdir

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   type(structure_type) :: mol
   type(error_type), allocatable :: parse_err
   character(len=:), allocatable :: geom_path, chrg_path, uhf_path
   character(len=:), allocatable :: chrg_file, uhf_file

   geom_path = join_path(tmpdir, "mol.xyz")
   chrg_path = join_path(tmpdir, ".CHRG")
   uhf_path = join_path(tmpdir, ".UHF")

   ! Missing dot-files leave embedded fractional charge and spin untouched
   call get_structure(mol, "mindless01")
   mol%charge = 0.5_wp
   mol%uhf = 1
   call read_dotfiles(mol, geom_path, error, chrg_file=chrg_file, uhf_file=uhf_file)
   if (allocated(error)) return

   call check(error, mol%charge, 0.5_wp, thr=thr)
   if (allocated(error)) return

   call check(error, mol%uhf, 1)
   if (allocated(error)) return

   call check(error, .not. allocated(chrg_file) .and. .not. allocated(uhf_file))
   if (allocated(error)) return

   ! Selective flags skip existing dot-files
   call write_raw(chrg_path, "-1.5" // new_line("a"))
   call write_raw(uhf_path, "2" // new_line("a"))
   call read_dotfiles(mol, geom_path, error, read_chrg=.false., read_uhf=.false., &
      & chrg_file=chrg_file, uhf_file=uhf_file)
   if (allocated(error)) return

   call check(error, mol%charge, 0.5_wp, thr=thr)
   if (allocated(error)) return

   call check(error, mol%uhf, 1)
   if (allocated(error)) return

   ! Default flags load both files (with and without optional path outputs)
   call read_dotfiles(mol, geom_path, error)
   if (allocated(error)) return

   call read_dotfiles(mol, geom_path, error, chrg_file=chrg_file, uhf_file=uhf_file)
   if (allocated(error)) return

   call check(error, mol%charge, -1.5_wp, thr=thr)
   if (allocated(error)) return

   call check(error, mol%uhf, 2)
   if (allocated(error)) return

   call check(error, chrg_file, chrg_path)
   if (allocated(error)) return

   call check(error, uhf_file, uhf_path)
   if (allocated(error)) return

   ! Malformed .UHF after valid .CHRG returns an error without mutating mol
   mol%charge = 0.25_wp
   mol%uhf = 1
   call write_raw(uhf_path, "invalid" // new_line("a"))
   call read_dotfiles(mol, geom_path, parse_err, chrg_file=chrg_file, uhf_file=uhf_file)
   if (.not. allocated(parse_err)) then
      call test_failed(error, "Malformed .UHF was not rejected")
      return
   end if
   deallocate(parse_err)

   call check(error, mol%charge, 0.25_wp, thr=thr)
   if (allocated(error)) return

   call check(error, mol%uhf, 1)
   if (allocated(error)) return

   ! Malformed .CHRG is rejected cleanly
   call write_raw(chrg_path, "invalid" // new_line("a"))
   call read_dotfiles(mol, geom_path, parse_err)
   if (.not. allocated(parse_err)) then
      call test_failed(error, "Malformed .CHRG was not rejected")
   end if

end subroutine run_dotfiles_lookup


subroutine test_chrg_empty(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: q

   call parse_chrg("# comment only", q, error)

end subroutine test_chrg_empty


subroutine test_chrg_invalid(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=*), parameter :: cases(*) = [character(len=8) :: &
      & "abc", "1/2", "1,2", "2*1.0", "NaN", "Inf", "1e400"]
   type(error_type), allocatable :: parse_err
   integer :: i
   real(wp) :: q

   do i = 1, size(cases)
      call parse_chrg(trim(cases(i)), q, parse_err)
      if (.not. allocated(parse_err)) then
         call test_failed(error, "Accepted invalid charge '"//trim(cases(i))//"'")
         return
      end if
      deallocate(parse_err)
   end do

end subroutine test_chrg_invalid


subroutine test_chrg_trailing(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: q

   call parse_chrg("0 1", q, error)

end subroutine test_chrg_trailing


subroutine test_chrg_missing(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   real(wp) :: q

   call read_chrg(q, get_name(), error)

end subroutine test_chrg_missing


subroutine test_uhf_float(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: uhf

   call parse_uhf("1.0", uhf, error)

end subroutine test_uhf_float


subroutine test_uhf_trailing(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: uhf

   call parse_uhf("1 2", uhf, error)

end subroutine test_uhf_trailing


subroutine test_uhf_missing(error)

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: uhf

   call read_uhf(uhf, get_name(), error)

end subroutine test_uhf_missing


!> Parse a single-line .CHRG string via a scratch unit
subroutine parse_chrg(text, q, error)

   !> Input text line
   character(len=*), intent(in) :: text

   !> Parsed charge
   real(wp), intent(out) :: q

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") text
   rewind(unit)
   call read_chrg(q, unit, error)
   close(unit)

end subroutine parse_chrg


!> Parse a single-line .UHF string via a scratch unit
subroutine parse_uhf(text, uhf, error)

   !> Input text line
   character(len=*), intent(in) :: text

   !> Parsed spin
   integer, intent(out) :: uhf

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: unit

   open(status="scratch", newunit=unit)
   write(unit, "(a)") text
   rewind(unit)
   call read_uhf(uhf, unit, error)
   close(unit)

end subroutine parse_uhf


!> Generate a unique temporary file or directory name
function get_name() result(name)

   !> Generated temporary name
   character(len=18) :: name

   real :: val

   call random_number(val)
   write(name, "(a, z8.8)") "mctc-test-", int(val * 1.0e9)

end function get_name


!> Create a fresh isolated temporary directory
subroutine make_temp_dir(tmpdir, error)

   !> Created temporary directory path
   character(len=:), allocatable, intent(out) :: tmpdir

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character(len=18) :: candidate
   integer :: attempt, stat, cstat

   do attempt = 1, 16
      candidate = get_name()
      call execute_command_line("mkdir " // candidate, exitstat=stat, cmdstat=cstat)
      if (cstat == 0 .and. stat == 0) then
         tmpdir = candidate
         return
      end if
   end do

   call test_failed(error, "Could not create temporary directory")

end subroutine make_temp_dir


!> Remove an empty temporary directory
subroutine remove_temp_dir(tmpdir, error)

   !> Temporary directory path
   character(len=*), intent(in) :: tmpdir

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer :: stat, cstat

   call execute_command_line("rmdir " // tmpdir, exitstat=stat, cmdstat=cstat)
   if (cstat /= 0 .or. stat /= 0) then
      call test_failed(error, "Could not remove temporary directory: " // tmpdir)
   end if

end subroutine remove_temp_dir


!> Write exact bytes to a file without adding a trailing newline
subroutine write_raw(fname, content)

   !> File path
   character(len=*), intent(in) :: fname

   !> Exact file contents
   character(len=*), intent(in) :: content

   integer :: unit

   open(file=fname, newunit=unit, status="replace", &
      & access="stream", form="unformatted", action="write")
   if (len(content) > 0) write(unit) content
   close(unit)

end subroutine write_raw


!> Delete a temporary file if it exists
subroutine delete_file(fname)

   !> File path
   character(len=*), intent(in) :: fname

   integer :: unit, stat

   open(file=fname, newunit=unit, status="old", iostat=stat)
   if (stat == 0) close(unit, status="delete")

end subroutine delete_file


end module test_dotfiles
