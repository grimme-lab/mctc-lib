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
   use mctc_env_error, only : error_type, fatal_error
   use mctc_io_constants, only : pi
   use mctc_io_convert, only : aatoau
   use mctc_io_resize, only : resize
   use mctc_io_structure, only : structure_type, new
   use mctc_io_structure_info, only : structure_info
   use mctc_io_symbols, only : to_number, symbol_length
   use mctc_io_utils, only : getline
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

   logical :: has_coord, has_periodic, has_lattice, has_cell
   logical :: cartesian, coord_in_bohr, lattice_in_bohr, pbc(3)
   integer :: stat, iatom, i, j, natoms, periodic, cell_vectors
   real(wp) :: latvec(9), conv, cellpar(6), lattice(3, 3)
   real(wp), allocatable :: coord(:, :), xyz(:, :)
   character(len=:), allocatable :: line, cell_string, lattice_string
   character(len=symbol_length), allocatable :: sym(:)
   type(structure_info) :: info

   allocate(sym(p_initial_size), source=repeat(' ', symbol_length))
   allocate(coord(3, p_initial_size), source=0.0_wp)

   iatom = 0
   periodic = 0
   cell_vectors = 0
   has_coord = .false.
   has_periodic = .false.
   has_lattice = .false.
   has_cell = .false.
   cartesian = .true.
   coord_in_bohr = .true.
   lattice_in_bohr = .true.
   lattice = 0.0_wp
   pbc = .false.

   stat = 0
   do while(stat == 0)
      call getline(unit, line, stat)
      if (index(line, flag) == 1) then
         if (index(line, 'end') == 2) then
            exit

         else if (.not.has_coord .and. index(line, 'coord') == 2) then
            has_coord = .true.
            ! $coord angs / $coord bohr / $coord frac
            call select_unit(line, coord_in_bohr, cartesian)
            coord_group: do while(stat == 0)
               call getline(unit, line, stat)
               if (index(line, flag) == 1) then
                  backspace(unit)
                  exit coord_group
               end if
               if (iatom >= size(coord, 2)) call resize(coord)
               if (iatom >= size(sym)) call resize(sym)
               iatom = iatom + 1
               read(line, *, iostat=stat) coord(:, iatom), sym(iatom)
            end do coord_group

         else if (.not.has_periodic .and. index(line, 'periodic') == 2) then
            has_periodic = .true.
            ! $periodic 0/1/2/3
            read(line(10:), *, iostat=stat) periodic

         else if (.not.has_lattice .and. index(line, 'lattice') == 2) then
            has_lattice = .true.
            ! $lattice bohr / $lattice angs
            call select_unit(line, lattice_in_bohr)
            cell_vectors = 0
            lattice_string = ''
            lattice_group: do while(stat == 0)
               call getline(unit, line, stat)
               if (index(line, flag) == 1) then
                  backspace(unit)
                  exit lattice_group
               end if
               cell_vectors = cell_vectors + 1
               lattice_string = lattice_string // ' ' // line
            end do lattice_group

         else if (.not.has_cell .and. index(line, 'cell') == 2) then
            has_cell = .true.
            ! $cell bohr / $cell angs
            call select_unit(line, lattice_in_bohr)
            call getline(unit, cell_string, stat)
            if (debug) print*, cell_string

         end if
      end if
   end do

   if (.not.has_coord .or. iatom == 0) then
      call fatal_error(error, "coordinates not present, cannot work without coordinates")
      return
   end if

   if (has_cell .and. has_lattice) then
      call fatal_error(error, "both lattice and cell group are present")
      return
   end if

   if (.not.has_periodic .and. (has_cell .or. has_lattice)) then
      call fatal_error(error, "cell and lattice definition is present, but periodicity is not given")
      return
   end if

   if (periodic > 0 .and. .not.(has_cell .or. has_lattice)) then
      call fatal_error(error, "system is periodic but definition of lattice/cell is missing")
      return
   end if

   if (.not.cartesian .and. periodic == 0) then
      call fatal_error(error, "fractional coordinates do not work for molecular systems")
      return
   end if

   natoms = iatom
   allocate(xyz(3, natoms))

   if (any(to_number(sym(:natoms)) == 0)) then
      call fatal_error(error, "unknown element present")
      return
   end if

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
         call fatal_error(error, "number of cell vectors does not match periodicity")
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
      xyz = matmul(lattice, coord)
   end if

   ! save data on input format
   info = structure_info(cartesian=cartesian, lattice=has_lattice, &
      & angs_lattice=.not.lattice_in_bohr, angs_coord=.not.coord_in_bohr)
   call new(mol, sym(:natoms), xyz, lattice=lattice, periodic=pbc, info=info)

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
   real(wp), intent(out) :: lattice(3, 3)

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
