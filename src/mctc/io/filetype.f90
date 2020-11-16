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

!> File type support
module mctc_io_filetype
   implicit none
   private

   public :: filetype, get_filetype


   !> Possible file types
   type :: enum_filetype

      !> Unknown file type
      integer :: unknown = 0

      !> xyz-format
      integer :: xyz = 1

      !> Turbomole coordinate format
      integer :: tmol = 2

      !> mol-format
      integer :: molfile = 3

      !> Vasp coordinate input
      integer :: vasp = 4

      !> Protein database format
      integer :: pdb = 5

      !> Structure data format
      integer :: sdf = 6

      !> GenFormat of DFTB+
      integer :: gen = 7

      !> Gaussian external format
      integer :: gaussian = 8

   end type enum_filetype

   !> File type enumerator
   type(enum_filetype), parameter :: filetype = enum_filetype()


contains


elemental function get_filetype(file) result(ftype)

   !> Name of the file
   character(len=*), intent(in) :: file

   !> File type from extension
   integer :: ftype

   integer :: iext, isep

   ftype = filetype%unknown
   iext = index(file, '.', back=.true.)
   isep = scan(file, '/\', back=.true.)

   if (iext > isep .and. iext > 0) then
      select case(to_lower(file(iext+1:)))
      case('coord', 'tmol')
         ftype = filetype%tmol
      case('xyz', 'log')
         ftype = filetype%xyz
      case('mol')
         ftype = filetype%molfile
      case('sdf')
         ftype = filetype%sdf
      case('poscar', 'contcar', 'vasp')
         ftype = filetype%vasp
      case('pdb')
         ftype = filetype%pdb
      case('gen')
         ftype = filetype%gen
      case('ein')
         ftype = filetype%gaussian
      end select
      if (ftype /= filetype%unknown) return
   else
      iext = len(file) + 1
   end if

   if (iext > isep) then
      select case(to_lower(file(isep+1:iext-1)))
      case('coord')
         ftype = filetype%tmol
      case('poscar', 'contcar')
         ftype = filetype%vasp
      end select
   end if

end function get_filetype


!> Convert input string to lowercase
elemental function to_lower(str) result(lcstr)

   !> Input string
   character(len=*), intent(in) :: str

   !> Lowercase version of string
   character(len=len(str)):: lcstr

   integer :: ilen, iquote, i, iav, iqc
   integer, parameter :: offset = iachar('A') - iachar('a')

   ilen = len(str)
   iquote = 0
   lcstr = str

   do i = 1, ilen
      iav = iachar(str(i:i))
      if (iquote == 0 .and. (iav == 34 .or.iav == 39)) then
         iquote = 1
         iqc = iav
         cycle
      end if
      if (iquote == 1 .and. iav==iqc) then
         iquote=0
         cycle
      end if
      if (iquote == 1) cycle
      if (iav >= iachar('A') .and. iav <= iachar('Z')) then
         lcstr(i:i) = achar(iav - offset)
      else
         lcstr(i:i) = str(i:i)
      end if
   end do

end function to_lower


end module mctc_io_filetype
