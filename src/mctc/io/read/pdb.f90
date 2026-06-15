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

module mctc_io_read_pdb
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : pdb_data, resize
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, filename, &
      read_token, to_string
   implicit none
   private

   public :: read_pdb


contains


subroutine read_pdb(self, unit, error)

   !> Instance of the molecular structure data
   type(structure_type),intent(out) :: self

   !> File handle
   integer,intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   integer, parameter :: p_initial_size = 1000 ! this is going to be a protein

   integer :: iatom, jatom, iresidue, try, stat, atom_type, pos, lnum, nat
   integer, allocatable :: map(:)
   real(wp) :: occ, temp, coords(3)
   real(wp), allocatable :: occ_values(:)
   real(wp), allocatable :: xyz(:,:)
   logical, allocatable :: keep(:)
   type(token_type) :: token
   character(len=4) :: a_charge
   character(len=:), allocatable :: line
   character(len=symbol_length), allocatable :: sym(:)
   type(pdb_data), allocatable :: pdb(:)

   allocate(sym(p_initial_size), source=repeat(' ', symbol_length))
   allocate(xyz(3, p_initial_size), source=0.0_wp)
   allocate(occ_values(p_initial_size), source=1.0_wp)
   allocate(pdb(p_initial_size), source=pdb_data())

   iatom = 0
   iresidue = 0

   stat = 0
   do while(stat == 0)
      call next_line(unit, line, pos, lnum, stat)
      if (index(line, 'END') == 1) exit
      if (index(line, 'ATOM') == 1 .or. index(line, 'HETATM') == 1) then
         if (iatom >= size(xyz, 2)) call resize(xyz)
         if (iatom >= size(sym)) call resize(sym)
         if (iatom >= size(occ_values)) call resize(occ_values)
         if (iatom >= size(pdb)) call resize(pdb)
         iatom = iatom + 1
         pdb(iatom)%het = index(line, 'HETATM') == 1

         if (len(line) >= 78) then
            ! a4: 13:16, a1: 17:17, a3: 18:20, a1: 22:22
            ! a1: 27:27, a4: 73:76, a2: 77:78, a2: 79:80
            pdb(iatom)%name = line(13:16)
            pdb(iatom)%loc = line(17:17)
            pdb(iatom)%residue = line(18:20)
            pdb(iatom)%chains = line(22:22)
            pdb(iatom)%code = line(27:27)
            pdb(iatom)%segid = line(72:74)
            sym(iatom) = line(77:78)
         else
            token = token_type(len(line)+1, len(line)+1)
            call io_error(error, "Too few entries provided in record", &
               & line, token, filename(unit), lnum, "record too short")
            return
         end if
         if (len(line) >= 80) then
            a_charge = line(79:80)
         else
            a_charge = ""
         end if
         if (stat == 0) then
            ! i5: 7-11
            token = token_type(7, 11)
            call read_token(line, token, jatom, stat)
         end if
         if (stat == 0) then
            ! i4: 23-26
            token = token_type(23, 26)
            call read_token(line, token, pdb(iatom)%residue_number, stat)
         end if
         if (stat == 0) then
            ! f8: 31-38
            token = token_type(31, 38)
            call read_token(line, token, coords(1), stat)
         end if
         if (stat == 0) then
            ! f8: 39-46
            token = token_type(39, 46)
            call read_token(line, token, coords(2), stat)
         end if
         if (stat == 0) then
            ! f8: 47-54
            token = token_type(47, 54)
            call read_token(line, token, coords(3), stat)
         end if
         if (stat == 0) then
            ! f6: 55-60
            token = token_type(55, 60)
            call read_token(line, token, occ, stat)
         end if
         if (stat == 0) then
            ! f6: 61-66
            token = token_type(60, 66)
            call read_token(line, token, temp, stat)
         end if
         if (stat /= 0) then
            call io_error(error, "Cannot read coordinates from record", &
               & line, token, filename(unit), lnum, "unexpected value")
            return
         end if
         if (occ < 0.0_wp .or. occ > 1.0_wp) then
            call io_error(error, "Invalid occupancy in record", &
               & line, token_type(55, 60), filename(unit), lnum, "expected value in [0.0, 1.0]")
            return
         end if
         occ_values(iatom) = occ
         pdb(iatom)%occupancy = occ

         xyz(:,iatom) = coords * aatoau
         atom_type = to_number(sym(iatom))
         if (atom_type == 0) then
            try = scan(pdb(iatom)%name, 'HCNOSPF')
            if (try > 0) sym(iatom) = pdb(iatom)%name(try:try)//' '
            pdb(iatom)%charge = 0
         else
            read(a_charge(1:1), *, iostat=stat) pdb(iatom)%charge
            if (stat /= 0) then
               stat = 0
               pdb(iatom)%charge = 0
            else
               if (a_charge(2:2) == '-') pdb(iatom)%charge = -pdb(iatom)%charge
            end if
         end if
         if (to_number(sym(iatom)) == 0) then
            call io_error(error, "Cannot map symbol to atomic number", &
               & line, token_type(77, 78), filename(unit), lnum, "unknown element")
            return
         end if
      end if
   end do

   call validate_partial_occupancy_sites(pdb(:iatom), occ_values(:iatom), error)
   if (allocated(error)) return

   allocate(keep(iatom), source=.true.)
   call drop_low_occupancy_alternatives(pdb(:iatom), occ_values(:iatom), keep)

   nat = count(keep)
   allocate(map(nat))
   jatom = 0
   do iatom = 1, size(keep)
      if (.not.keep(iatom)) cycle
      jatom = jatom + 1
      map(jatom) = iatom
   end do

   call new(self, sym(map), xyz(:, map))
   self%pdb = pdb(map)
   self%charge = sum(pdb(map)%charge)

end subroutine read_pdb


subroutine validate_partial_occupancy_sites(pdb, occ_values, error)

   type(pdb_data), intent(in) :: pdb(:)
   real(wp), intent(in) :: occ_values(:)
   type(error_type), allocatable, intent(out) :: error

   real(wp), parameter :: occ_thr = 10 * epsilon(1.0_wp)

   integer :: iatom, jatom
   logical :: has_alternative, has_duplicate

   do iatom = 1, size(pdb)
      if (occ_values(iatom) >= 1.0_wp - occ_thr) cycle

      has_alternative = .false.
      has_duplicate = .false.
      do jatom = 1, size(pdb)
         if (iatom == jatom) cycle
         if (.not.is_same_site(pdb(iatom), pdb(jatom))) cycle
         has_duplicate = .true.
         if (is_alternative_site(pdb(iatom), pdb(jatom))) then
            has_alternative = .true.
         end if
      end do

      if (has_duplicate .and. .not.has_alternative) then
         call fatal_error(error, "Partial occupancy requires alternative location records")
         return
      end if
   end do

end subroutine validate_partial_occupancy_sites


pure logical function is_same_site(lhs, rhs)

   type(pdb_data), intent(in) :: lhs
   type(pdb_data), intent(in) :: rhs

   is_same_site = lhs%name == rhs%name .and. &
      & lhs%residue == rhs%residue .and. &
      & lhs%chains == rhs%chains .and. &
      & lhs%residue_number == rhs%residue_number .and. &
      & lhs%code == rhs%code .and. &
      & lhs%segid == rhs%segid .and. &
      & (lhs%het .eqv. rhs%het)

end function is_same_site


subroutine drop_low_occupancy_alternatives(pdb, occ_values, keep)

   type(pdb_data), intent(in) :: pdb(:)
   real(wp), intent(in) :: occ_values(:)
   logical, intent(out) :: keep(:)

   integer :: iatom, jatom, best

   keep = .true.
   do iatom = 1, size(pdb)
      if (.not.keep(iatom)) cycle
      best = iatom
      do jatom = iatom + 1, size(pdb)
         if (.not.is_alternative_site(pdb(iatom), pdb(jatom))) cycle
         if (has_higher_occupancy(pdb(jatom), occ_values(jatom), pdb(best), occ_values(best))) then
            keep(best) = .false.
            best = jatom
         else
            keep(jatom) = .false.
         end if
      end do
   end do

end subroutine drop_low_occupancy_alternatives


pure logical function is_alternative_site(lhs, rhs)

   type(pdb_data), intent(in) :: lhs
   type(pdb_data), intent(in) :: rhs

   is_alternative_site = is_same_site(lhs, rhs) .and. &
      & (lhs%loc /= ' ' .or. rhs%loc /= ' ')

end function is_alternative_site


pure logical function has_higher_occupancy(candidate, candidate_occ, current, current_occ)

   type(pdb_data), intent(in) :: candidate
   real(wp), intent(in) :: candidate_occ
   type(pdb_data), intent(in) :: current
   real(wp), intent(in) :: current_occ

   real(wp), parameter :: occ_thr = 10 * epsilon(1.0_wp)

   has_higher_occupancy = .false.
   if (candidate_occ > current_occ + occ_thr) then
      has_higher_occupancy = .true.
      return
   end if

   if (abs(candidate_occ - current_occ) <= occ_thr) then
      has_higher_occupancy = candidate%loc == ' ' .and. current%loc /= ' '
   end if

end function has_higher_occupancy


end module mctc_io_read_pdb
