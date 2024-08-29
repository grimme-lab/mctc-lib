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

module mctc_io_write_ctfile
   use mctc_env_accuracy, only : wp
   use mctc_io_convert, only : autoaa
   use mctc_io_structure, only : structure_type
   implicit none
   private

   public :: write_molfile, write_sdf


contains


subroutine write_sdf(self, unit, energy, gnorm)
   class(structure_type), intent(in) :: self
   integer, intent(in) :: unit
   real(wp), intent(in), optional :: energy
   real(wp), intent(in), optional :: gnorm
   !type(tb_buffer) :: sd_values
   character(len=:), allocatable :: line
   character(len=*), parameter :: sd_format = &
      & '("> <", a, ">", /, f20.12, /)'

   call write_molfile(self, unit)

!   sd_values = self%info
!   call sd_values%reset
!   do while(sd_values%next())
!      call sd_values%getline(line)
!      write(unit, '(a)') line
!   enddo

   if (present(energy)) then
      write(unit, sd_format) "total energy / Eh", energy
   endif

   if (present(gnorm)) then
      write(unit, sd_format) "gradient norm / Eh/a0", gnorm
   endif

   write(unit, '("$$$$")')

end subroutine write_sdf


subroutine write_molfile(self, unit, comment_line)
   class(structure_type), intent(in) :: self
   integer, intent(in) :: unit
   character(len=*), intent(in), optional :: comment_line
   integer, parameter :: list4(4) = 0
   integer :: iatom, ibond, iatoms(3), list12(12)
   logical :: has_sdf_data
   integer, parameter :: charge_to_ccc(-3:3) = [7, 6, 5, 0, 3, 2, 1]
   character(len=8)  :: date
   character(len=10) :: time

   call date_and_time(date, time)

   if (present(comment_line)) then
      write(unit, '(a)') comment_line
   else
      if (allocated(self%comment)) then
         write(unit, '(a)') self%comment
      else
         write(unit, '(a)')
      end if
   end if
   write(unit, '(2x, 3x, 5x, 3a2, a4, "3D")') &
      &  date(5:6), date(7:8), date(3:4), time(:4)
   write(unit, '(a)')
   write(unit, '(3i3, 3x, 2i3, 12x, i3, 1x, a5)') &
      &  self%nat, self%nbd, 0, 0, 0, 999, 'V2000'

   has_sdf_data = allocated(self%sdf)

   do iatom = 1, self%nat
      if (has_sdf_data) then
         list12 = [self%sdf(iatom)%isotope, 0, 0, 0, 0, self%sdf(iatom)%valence, &
            & 0, 0, 0, 0, 0, 0]
      else
         list12 = 0
      endif
      write(unit, '(3f10.4, 1x, a3, i2, 11i3)') &
         & self%xyz(:, iatom)*autoaa, self%sym(self%id(iatom)), list12
   enddo

   if (self%nbd > 0) then
      if (size(self%bond, 1) > 2) then
         do ibond = 1, self%nbd
            write(unit, '(7i3)') self%bond(:3, ibond), list4
         end do
      else
         do ibond = 1, self%nbd
            write(unit, '(7i3)') self%bond(:2, ibond), 1, list4
         end do
      end if
   end if

   if (has_sdf_data) then
      if (sum(self%sdf%charge) /= nint(self%charge)) then
         write(unit, '(a, *(i3, 1x, i3, 1x, i3))') "M  CHG", 1, 1, nint(self%charge)
      else
         do iatom = 1, self%nat
            if (self%sdf(iatom)%charge /= 0) then
               write(unit, '(a, *(i3, 1x, i3, 1x, i3))') &
                  & "M  CHG", 1, iatom, self%sdf(iatom)%charge
            end if
         end do
      end if
   else
      if (nint(self%charge) /= 0) then
         write(unit, '(a, *(i3, 1x, i3, 1x, i3))') "M  CHG", 1, 1, nint(self%charge)
      end if
   end if

   write(unit, '(a)') "M  END"

end subroutine write_molfile


end module mctc_io_write_ctfile
