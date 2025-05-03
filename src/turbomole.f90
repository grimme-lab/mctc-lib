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

module mctc_io_read_turbomole
   use mctc_env_accuracy, only : wp
   use mctc_env_error, only : error_type
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : next_line, token_type, next_token, io_error, io2_error, &
      filename, read_next_token, to_string
   implicit none
   private

   public :: read_coord


   logical, parameter :: debug = .false.


contains


subroutine read_coord(mol, unit, error)

   !> Instance of the molecular structure data
   type(structure_type), intent(out) :: mol

   !> File handle
   integer, intent(in) :: unit

   !> Error handling
   type(error_type), allocatable, intent(out) :: error

   character, parameter :: flag = '$'
   integer, parameter :: p_initial_size = 100
   integer, parameter :: p_nlv(3) = [1, 4, 9], p_ncp(3) = [1, 3, 6]

   logical :: has_coord, has_periodic, has_lattice, has_cell, has_eht
   logical :: cartesian, coord_in_bohr, lattice_in_bohr, pbc(3)
   integer :: stat, iatom, i, j, natoms, periodic, cell_vectors, icharge, unpaired
   integer :: lnum, pos, lcell, llattice, lperiodic, lcoord, leht
   type(token_type) :: token, token2
   real(wp) :: latvec(9), conv, cellpar(6), lattice(3, 3)
   real(wp), allocatable :: coord(:, :), xyz(:, :), charge
   character(len=:), allocatable :: line, cell_string, lattice_string, &
      & line_cell, line_lattice, line_periodic, line_coord, line_eht
   character(len=symbol_length), allocatable :: sym(:)
   type(structure_info) :: info

   allocate(sym(p_initial_size), source=repeat(' ', symbol_length))
   allocate(coord(3, p_initial_size), source=0.0_wp)

   lnum = 0
   iatom = 0
   periodic = 0
   cell_vectors = 0
   has_eht = .false.
   has_coord = .false.
   has_periodic = .false.
   has_lattice = .false.
   has_cell = .false.
   cartesian = .true.
   coord_in_bohr = .true.
   lattice_in_bohr = .true.
   lattice(:, :) = 0.0_wp
   pbc(:) = .false.
   charge = 0.0_wp
   unpaired = 0

   stat = 0
   call next_line(unit, line, pos, lnum, stat)
   do while(stat == 0)
      if (index(line, flag) == 1) then
         call next_token(line, pos, token)
         select case(line(token%first:token%last))
         case('$end')
            exit

         case('$eht')
            if (has_eht) then
               pos = 0
               call next_token(line_eht, pos, token2)
               call io2_error(error, "Duplicated eht data group", &
                  & line_eht, line, token2, token, &
                  & filename(unit), leht, lnum, &
                  & "charge/multiplicity first defined here", "duplicated eht data")
               return
            end if
            has_eht = .true.
            leht = lnum
            line_eht = line
            i = index(line, 'charge=')
            if (i > 0) then
               pos = i + 6
               call read_next_token(line, pos, token, icharge, stat)
               charge = real(icharge, wp)
            end if
            j = index(line, 'unpaired=')
            if (j > 0 .and. stat == 0) then
               pos = j + 8
               call read_next_token(line, pos, token, unpaired, stat)
            end if
            if (stat /= 0) then
               call io_error(error, "Cannot read eht entry", &
                  & line, token, filename(unit), lnum, "expected integer value")
               return
            end if

         case('$coord')
            if (has_coord) then
               pos = 0
               call next_token(line_coord, pos, token2)
               call io2_error(error, "Duplicated coord data group", &
                  & line_coord, line, token2, token, &
                  & filename(unit), lcoord, lnum, &
                  & "coordinates first defined here", "duplicated coordinate group")
               return
            end if
            lcoord = lnum
            line_coord = line
            has_coord = .true.
            ! $coord angs / $coord bohr / $coord frac
            call select_unit(line, coord_in_bohr, cartesian)
            coord_group: do while(stat == 0)
               call next_line(unit, line, pos, lnum, stat)
               if (index(line, flag) == 1) exit coord_group
               if (iatom >= size(coord, 2)) call resize(coord)
               if (iatom >= size(sym)) call resize(sym)
               iatom = iatom + 1
               call read_next_token(line, pos, token, coord(1, iatom), stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, coord(2, iatom), stat)
               if (stat == 0) &
                  call read_next_token(line, pos, token, coord(3, iatom), stat)
               if (stat == 0) &
                  call next_token(line, pos, token)
               if (stat /= 0) then
                  call io_error(error, "Cannot read coordinates", &
                     & line, token, filename(unit), lnum, "expected real value")
                  return
               end if

               token%last = min(token%last, token%first + symbol_length - 1)
               sym(iatom) = line(token%first:token%last)
               if (to_number(sym(iatom)) == 0) then
                  call io_error(error, "Cannot map symbol to atomic number", &
                     & line, token, filename(unit), lnum, "unknown element")
                  return
               end if
            end do coord_group
            cycle

         case('$periodic')
            if (has_periodic) then
               pos = 0
               call next_token(line_periodic, pos, token2)
               call io2_error(error, "Duplicated periodic data group", &
                  & line_periodic, line, token2, token, &
                  & filename(unit), lperiodic, lnum, &
                  & "periodicity first defined here", "duplicated periodicity data")
               return
            end if
            lperiodic = lnum
            line_periodic = line
            has_periodic = .true.
            ! $periodic 0/1/2/3
            call read_next_token(line, pos, token, periodic, stat)
            if (stat /= 0 .or. periodic < 0 .or. periodic > 3) then
               call io_error(error, "Cannot read periodicity of system", &
                  & line, token, filename(unit), lnum, "expected integer (0 to 3)")
               return
            end if

         case('$lattice')
            if (has_lattice) then
               pos = 0
               call next_token(line_lattice, pos, token2)
               call io2_error(error, "Duplicated lattice data group", &
                  & line_lattice, line, token2, token, &
                  & filename(unit), llattice, lnum, &
                  & "lattice parameters first defined here", "duplicated lattice group")
               return
            end if
            llattice = lnum
            line_lattice = line
            has_lattice = .true.
            ! $lattice bohr / $lattice angs
            call select_unit(line, lattice_in_bohr)
            cell_vectors = 0
            lattice_string = ''
            lattice_group: do while(stat == 0)
               call next_line(unit, line, pos, lnum, stat)
               if (index(line, flag) == 1) exit lattice_group
               cell_vectors = cell_vectors + 1
               lattice_string = lattice_string // ' ' // line
            end do lattice_group
            cycle

         case('$cell')
            if (has_cell) then
               pos = 0
               call next_token(line_cell, pos, token2)
               call io2_error(error, "Duplicated cell data group", &
                  & line_cell, line, token2, token, &
                  & filename(unit), lcell, lnum, &
                  & "cell parameters first defined here", "duplicated cell group")
               return
            end if
            lcell = lnum
            line_cell = line
            has_cell = .true.
            ! $cell bohr / $cell angs
            call select_unit(line, lattice_in_bohr)
            call next_line(unit, cell_string, pos, lnum, stat)
            if (debug) print*, cell_string

         end select
      end if
      token = token_type(0, 0)
      call next_line(unit, line, pos, lnum, stat)
   end do
   if (allocated(error)) return

   if (.not.has_coord .or. iatom == 0) then
      call io_error(error, "coordinates not present, cannot work without coordinates", &
         & line, token, filename(unit), lnum, "unexpected end of input")
      return
   end if

   if (has_cell .and. has_lattice) then
      block
         type(token_type) :: tcell, tlattice
         pos = 0
         call next_token(line_cell, pos, tcell)
         pos = 0
         call next_token(line_lattice, pos, tlattice)
         tlattice = token_type(1, len(line_lattice))
         if (lcell > llattice) then
            call io2_error(error, "Conflicting lattice and cell groups", &
               & line_lattice, line_cell, tlattice, tcell, &
               & filename(unit), llattice, lcell, &
               & "lattice first defined here", "conflicting cell group")
         else
            call io2_error(error, "Conflicting lattice and cell groups", &
               & line_cell, line_lattice, tcell, tlattice, &
               & filename(unit), lcell, llattice, &
               & "cell first defined here", "conflicting lattice group")
         end if
      end block
      return
   end if

   if (.not.has_periodic .and. (has_cell .or. has_lattice)) then
      pos = 0
      if (has_cell) then
         call next_token(line_cell, pos, token)
         call io_error(error, "Cell parameters defined without periodicity", &
            & line_cell, token, filename(unit), &
            & lcell, "cell defined here")
      end if
      if (has_lattice) then
         call next_token(line_lattice, pos, token)
         call io_error(error, "Lattice parameters defined without periodicity", &
            & line_lattice, token, filename(unit), &
            & llattice, "lattice defined here")
      end if
      return
   end if

   if (periodic > 0 .and. .not.(has_cell .or. has_lattice)) then
      pos = 0
      call next_token(line_periodic, pos, token)
      call io_error(error, "Missing lattice or cell data", &
         & line_periodic, token, filename(unit), &
         & lperiodic, "periodic system defined here")
      return
   end if

   if (.not.cartesian .and. periodic == 0) then
      pos = 0
      call next_token(line_coord, pos, token)
      call next_token(line_coord, pos, token)
      call io_error(error, "Molecular systems cannot have fractional coordinates", &
         & line_coord, token, filename(unit), &
         & lcoord, "fractional modifier found")
      return
   end if

   natoms = iatom
   allocate(xyz(3, natoms))

   if (periodic > 0) pbc(:periodic) = .true.

   if (has_cell) then
      read(cell_string, *, iostat=stat) latvec(:p_ncp(periodic))
      if (debug) print*, latvec(:p_ncp(periodic))
      if (lattice_in_bohr) then
         conv = 1.0_wp
      else
         conv = aatoau
      end if
      select case(periodic)
      case(1)
         cellpar = [latvec(1)*conv, 1.0_wp, 1.0_wp, &
            &       pi/2, pi/2, pi/2]
      case(2)
         cellpar = [latvec(1)*conv, latvec(2)*conv, 1.0_wp, &
            &       pi/2, pi/2, latvec(3)*pi/180.0_wp]
      case(3)
         cellpar = [latvec(1:3)*conv, latvec(4:6)*pi/180.0_wp]
      end select
      call cell_to_dlat(cellpar, lattice)
   end if

   if (has_lattice) then
      if (cell_vectors /= periodic) then
         pos = 0
         call next_token(line_lattice, pos, token)
         pos = len_trim(line_periodic)
         call io2_error(error, "Number of lattice vectors does not match periodicity", &
            & line_lattice, line_periodic, token, token_type(pos, pos), &
            & filename(unit), llattice, lperiodic, &
            & "lattice vectors defined here", "conflicting periodicity")
         return
      end if
      read(lattice_string, *, iostat=stat) latvec(:p_nlv(periodic))
      if (lattice_in_bohr) then
         conv = 1.0_wp
      else
         conv = aatoau
      end if
      j = 0
      do i = 1, periodic
         lattice(:periodic,  i) = latvec(j+1:j+periodic) * conv
         j = j + periodic
      end do
   end if

   if (cartesian) then
      if (coord_in_bohr) then
         conv = 1.0_wp
      else
         conv = aatoau
      end if
      xyz(:, :) = coord(:, :natoms) * conv
   else
      ! Non-periodic coordinates are in Bohr
      xyz(periodic+1:3, :) = coord(periodic+1:3, :natoms)
      ! Periodic coordinates must still be transformed with lattice
      xyz(:periodic, :) = matmul(lattice(:periodic, :periodic), coord(:periodic, :natoms))
   end if

   ! save data on input format
   info = structure_info(cartesian=cartesian, lattice=has_lattice, &
      & angs_lattice=.not.lattice_in_bohr, angs_coord=.not.coord_in_bohr)
   call new(mol, sym(:natoms), xyz, charge=charge, uhf=unpaired, &
      & lattice=lattice, periodic=pbc, info=info)

contains

   subroutine select_unit(line, in_bohr, cartesian)
      character(len=*), intent(in) :: line
      logical, intent(out) :: in_bohr
      logical, intent(out), optional :: cartesian
      in_bohr = index(line, ' angs') == 0
      if (present(cartesian)) cartesian = index(line, ' frac') == 0
   end subroutine select_unit

end subroutine read_coord


!> Calculate the lattice vectors from a set of cell parameters
pure subroutine cell_to_dlat(cellpar, lattice)

   !> Cell parameters
   real(wp), intent(in)  :: cellpar(6)

   !> Direct lattice
   real(wp), intent(out) :: lattice(:, :)

   real(wp) :: dvol

   dvol = cell_to_dvol(cellpar)

   associate(alen => cellpar(1), blen => cellpar(2), clen => cellpar(3), &
         &   alp  => cellpar(4), bet  => cellpar(5), gam  => cellpar(6))

      lattice(1, 1) = alen
      lattice(2, 1) = 0.0_wp
      lattice(3, 1) = 0.0_wp
      lattice(3, 2) = 0.0_wp
      lattice(1, 2) = blen*cos(gam)
      lattice(2, 2) = blen*sin(gam)
      lattice(1, 3) = clen*cos(bet)
      lattice(2, 3) = clen*(cos(alp) - cos(bet)*cos(gam))/sin(gam);
      lattice(3, 3) = dvol/(alen*blen*sin(gam))

   end associate

end subroutine cell_to_dlat


!> Calculate the cell volume from a set of cell parameters
pure function cell_to_dvol(cellpar) result(dvol)

   !> Cell parameters
   real(wp), intent(in) :: cellpar(6)

   !> Cell volume
   real(wp) :: dvol

   real(wp) :: vol2

   associate(alen => cellpar(1), blen => cellpar(2), clen => cellpar(3), &
         &   alp  => cellpar(4), bet  => cellpar(5), gam  => cellpar(6) )

      vol2 = 1.0_wp - cos(alp)**2 - cos(bet)**2 - cos(gam)**2 &
         & + 2.0_wp*cos(alp)*cos(bet)*cos(gam)

      dvol = sqrt(abs(vol2))*alen*blen*clen
      ! return negative volume instead of imaginary one (means bad cell parameters)
      if (vol2 < 0.0_wp) dvol = -dvol ! this should not happen, but who knows...

   end associate
end function cell_to_dvol


end module mctc_io_read_turbomole
