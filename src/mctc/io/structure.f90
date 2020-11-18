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

!> Basic structure representation of the system of interest
module mctc_io_structure
   use mctc_env_accuracy, only : wp
   use mctc_io_symbols, only : to_number, to_symbol, symbol_length, get_identity, &
      & collect_identical
   use mctc_io_structure_info, only : structure_info, pdb_data, sdf_data
   implicit none
   private

   public :: structure_type, new_structure, new, resize


   !> Structure representation
   type :: structure_type

      !> Number of atoms
      integer :: nat = 0

      !> Number of unique species
      integer :: nid = 0

      !> Species identifier
      integer, allocatable :: id(:)

      !> Atomic number for each species
      integer, allocatable :: num(:)

      !> Element symbol for each species
      character(len=symbol_length), allocatable :: sym(:)

      !> Cartesian coordinates, in Bohr
      real(wp), allocatable :: xyz(:, :)

      !> Number of unpaired electrons
      integer :: uhf = 0

      !> Total charge
      real(wp) :: charge = 0.0_wp

      !> Lattice parameters
      real(wp), allocatable :: lattice(:, :)

      !> Periodic directions
      logical, allocatable :: periodic(:)

      !> Vendor specific structure annotations
      type(structure_info) :: info = structure_info()

      !> SDF atomic data annotations
      type(sdf_data), allocatable :: sdf(:)

      !> PDB atomic data annotations
      type(pdb_data), allocatable :: pdb(:)

   end type structure_type


   interface new
      module procedure :: new_structure
      module procedure :: new_structure_num
      module procedure :: new_structure_sym
   end interface


   !> Overloaded resize interface
   interface resize
      module procedure :: resize_structure_1d
   end interface resize


   integer, parameter :: initial_size = 32


contains


!> Constructor for structure representations
subroutine new_structure(self, num, sym, xyz, charge, uhf, lattice, periodic, info)

   !> Instance of the structure representation
   type(structure_type), intent(out) :: self

   !> Atomic numbers
   integer, intent(in) :: num(:)

   !> Element symbols
   character(len=*), intent(in) :: sym(:)

   !> Cartesian coordinates
   real(wp), intent(in) :: xyz(:, :)

   !> Total charge
   real(wp), intent(in), optional :: charge

   !> Number of unpaired electrons
   integer, intent(in), optional :: uhf

   !> Lattice parameters
   real(wp), intent(in), optional :: lattice(:, :)

   !> Periodic directions
   logical, intent(in), optional :: periodic(:)

   !> Vendor specific structure information
   type(structure_info), intent(in), optional :: info

   integer :: ndim, iid
   integer, allocatable :: map(:)

   ndim = min(size(num, 1), size(xyz, 2), size(sym, 1))

   self%nat = ndim
   allocate(self%id(ndim))
   allocate(self%xyz(3, ndim))

   if (present(lattice)) then
      self%lattice = lattice
   end if

   if (present(periodic)) then
      self%periodic = periodic
   else
      if (present(lattice)) then
         allocate(self%periodic(3))
         self%periodic(:) = .true.
      else
         allocate(self%periodic(1))
         self%periodic(:) = .false.
      end if
   end if

   call get_identity(self%nid, self%id, sym)
   allocate(map(self%nid))
   call collect_identical(self%id, map)

   allocate(self%num(self%nid))
   allocate(self%sym(self%nid))
   do iid = 1, self%nid
      self%num(iid) = num(map(iid))
      self%sym(iid) = sym(map(iid))
   end do
   self%xyz(:, :) = xyz(:, :ndim)

   if (present(charge)) then
      self%charge = charge
   else
      self%charge = 0.0_wp
   end if

   if (present(uhf)) then
      self%uhf = uhf
   else
      self%uhf = 0
   end if

   if (present(info)) then
      self%info = info
   else
      self%info = structure_info()
   end if

end subroutine new_structure


!> Simplified constructor for structure representations
subroutine new_structure_num(self, num, xyz, charge, uhf, lattice, periodic, info)

   !> Instance of the structure representation
   type(structure_type), intent(out) :: self

   !> Atomic numbers
   integer, intent(in) :: num(:)

   !> Cartesian coordinates
   real(wp), intent(in) :: xyz(:, :)

   !> Total charge
   real(wp), intent(in), optional :: charge

   !> Number of unpaired electrons
   integer, intent(in), optional :: uhf

   !> Lattice parameters
   real(wp), intent(in), optional :: lattice(:, :)

   !> Periodic directions
   logical, intent(in), optional :: periodic(:)

   !> Vendor specific structure information
   type(structure_info), intent(in), optional :: info

   integer :: ndim, iat
   character(len=symbol_length), allocatable :: sym(:)

   ndim = min(size(num, 1), size(xyz, 2))
   allocate(sym(ndim))
   do iat = 1, ndim
      sym(iat) = to_symbol(num(iat))
   end do

   call new_structure(self, num, sym, xyz, charge, uhf, lattice, periodic, info)

end subroutine new_structure_num


!> Simplified constructor for structure representations
subroutine new_structure_sym(self, sym, xyz, charge, uhf, lattice, periodic, info)

   !> Instance of the structure representation
   type(structure_type), intent(out) :: self

   !> Element symbols
   character(len=*), intent(in) :: sym(:)

   !> Cartesian coordinates
   real(wp), intent(in) :: xyz(:, :)

   !> Total charge
   real(wp), intent(in), optional :: charge

   !> Number of unpaired electrons
   integer, intent(in), optional :: uhf

   !> Lattice parameters
   real(wp), intent(in), optional :: lattice(:, :)

   !> Periodic directions
   logical, intent(in), optional :: periodic(:)

   !> Vendor specific structure information
   type(structure_info), intent(in), optional :: info

   integer :: ndim, iat
   integer, allocatable :: num(:)

   ndim = min(size(sym, 1), size(xyz, 2))
   allocate(num(ndim))
   do iat = 1, ndim
      num(iat) = to_number(sym(iat))
   end do

   call new_structure(self, num, sym, xyz, charge, uhf, lattice, periodic, info)

end subroutine new_structure_sym


pure subroutine resize_structure_1d(var, n)

   !> Instance of the array to be resized
   type(structure_type), allocatable, intent(inout) :: var(:)

   !> Dimension of the final array size
   integer, intent(in), optional :: n

   type(structure_type), allocatable :: tmp(:)
   integer :: this_size, new_size

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

end subroutine resize_structure_1d


end module mctc_io_structure
