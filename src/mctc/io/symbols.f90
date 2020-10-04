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

!> Handle conversion between element symbols and atomic numbers
module mctc_io_symbols
   use mctc_io_resize, only : resize
   implicit none
   private

   public :: symbol_length
   public :: symbol_to_number, number_to_symbol, number_to_lcsymbol
   public :: to_number, to_symbol, to_lcsymbol
   public :: get_identity, collect_identical


   !> Get chemical identity
   interface get_identity
      module procedure :: get_identity_number
      module procedure :: get_identity_symbol
   end interface get_identity


   !> Maximum allowed length of element symbols
   integer, parameter :: symbol_length = 4


   !> Periodic system of elements
   character(len=2), parameter :: pse(118) = [ &
      & 'H ','He', &
      & 'Li','Be','B ','C ','N ','O ','F ','Ne', &
      & 'Na','Mg','Al','Si','P ','S ','Cl','Ar', &
      & 'K ','Ca', &
      & 'Sc','Ti','V ','Cr','Mn','Fe','Co','Ni','Cu','Zn', &
      &           'Ga','Ge','As','Se','Br','Kr', &
      & 'Rb','Sr', &
      & 'Y ','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag','Cd', &
      &           'In','Sn','Sb','Te','I ','Xe', &
      & 'Cs','Ba', &
      & 'La','Ce','Pr','Nd','Pm','Sm','Eu','Gd','Tb','Dy','Ho','Er','Tm','Yb', &
      & 'Lu','Hf','Ta','W ','Re','Os','Ir','Pt','Au','Hg', &
      &           'Tl','Pb','Bi','Po','At','Rn', &
      & 'Fr','Ra', &
      & 'Ac','Th','Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es','Fm','Md','No', &
      & 'Lr','Rf','Db','Sg','Bh','Hs','Mt','Ds','Rg','Cn', &
      &           'Nh','Fl','Mc','Lv','Ts','Og' ]


   !> Lower case version of the periodic system of elements
   character(len=2), parameter :: lcpse(118) = [ &
      & 'h ','he', &
      & 'li','be','b ','c ','n ','o ','f ','ne', &
      & 'na','mg','al','si','p ','s ','cl','ar', &
      & 'k ','ca', &
      & 'sc','ti','v ','cr','mn','fe','co','ni','cu','zn', &
      &           'ga','ge','as','se','br','kr', &
      & 'rb','sr', &
      & 'y ','zr','nb','mo','tc','ru','rh','pd','ag','cd', &
      &           'in','sn','sb','te','i ','xe', &
      & 'cs','ba','la', &
      & 'ce','pr','nd','pm','sm','eu','gd','tb','dy','ho','er','tm','yb', &
      & 'lu','hf','ta','w ','re','os','ir','pt','au','hg', &
      &           'tl','pb','bi','po','at','rn', &
      & 'fr','ra','ac', &
      & 'th','pa','u ','np','pu','am','cm','bk','cf','es','fm','md','no', &
      & 'lr','rf','db','sg','bh','hs','mt','ds','rg','cn', &
      &           'nh','fl','mc','lv','ts','og' ]


   !> ASCII offset between lowercase and uppercase letters
   integer, parameter :: offset = iachar('a') - iachar('A')


contains


!> Convert element symbol to atomic number
elemental subroutine symbol_to_number(number, symbol)

   !> Element symbol
   character(len=*), intent(in) :: symbol

   !> Atomic number
   integer, intent(out) :: number

   character(len=2) :: lcsymbol
   integer :: i, j, k, l

   number = 0
   lcsymbol = '  '

   k = 0
   do j = 1, len_trim(symbol)
      if (k > 2) exit
      l = iachar(symbol(j:j))
      if (k >= 1 .and. l == iachar(' ')) exit
      if (k >= 1 .and. l == 9) exit
      if (l >= iachar('A') .and. l <= iachar('Z')) l = l + offset
      if (l >= iachar('a') .and. l <= iachar('z')) then
         k = k+1
         if (k > 2) exit
         lcsymbol(k:k) = achar(l)
      endif
   enddo

   do i = 1, size(lcpse)
      if (lcsymbol == lcpse(i)) then
         number = i
         exit
      endif
   enddo

   if (number == 0) then
      select case(lcsymbol)
      case('d ', 't ')
         number = 1
      end select
   end if

end subroutine symbol_to_number


!> Convert atomic number to element symbol
elemental subroutine number_to_symbol(symbol, number)

   !> Atomic number
   integer, intent(in) :: number

   !> Element symbol
   character(len=2), intent(out) :: symbol

   if (number <= 0 .or. number > size(pse)) then
      symbol = '--'
   else
      symbol = pse(number)
   endif

end subroutine number_to_symbol


!> Convert atomic number to element symbol
elemental subroutine number_to_lcsymbol(symbol, number)

   !> Atomic number
   integer, intent(in) :: number

   !> Element symbol
   character(len=2), intent(out) :: symbol

   if (number <= 0 .or. number > size(lcpse)) then
      symbol = '--'
   else
      symbol = lcpse(number)
   endif

end subroutine number_to_lcsymbol


!> Convert element symbol to atomic number
elemental function to_number(symbol) result(number)

   !> Element symbol
   character(len=*), intent(in) :: symbol

   !> Atomic number
   integer :: number

   call symbol_to_number(number, symbol)

end function to_number


!> Convert atomic number to element symbol
elemental function to_symbol(number) result(symbol)

   !> Atomic number
   integer,intent(in) :: number

   !> Element symbol
   character(len=2) :: symbol

   call number_to_symbol(symbol, number)

end function to_symbol


!> Convert atomic number to element symbol
elemental function to_lcsymbol(number) result(symbol)

   !> Atomic number
   integer,intent(in) :: number

   !> Element symbol
   character(len=2) :: symbol

   call number_to_lcsymbol(symbol, number)

end function to_lcsymbol


!> Get chemical identity from a list of atomic numbers
pure subroutine get_identity_number(nid, identity, number)

   !> Number of unique species
   integer, intent(out) :: nid

   !> Ordinal numbers
   integer, intent(in) :: number(:)

   !> Chemical identity
   integer, intent(out) :: identity(:)

   integer, allocatable :: itmp(:)
   integer :: nat, iat, iid

   nat = size(identity)
   allocate(itmp(nat))
   nid = 0
   do iat = 1, nat
      iid = find_number(itmp(:nid), number(iat))
      if (iid == 0) then
         call append_number(itmp, nid, number(iat))
         iid = nid
      end if
      identity(iat) = iid
   end do

end subroutine get_identity_number


!> Get chemical identity from a list of element symbols
pure subroutine get_identity_symbol(nid, identity, symbol)

   !> Number of unique species
   integer, intent(out) :: nid

   !> Element symbols
   character(len=symbol_length), intent(in) :: symbol(:)

   !> Chemical identity
   integer, intent(out) :: identity(:)

   character(len=symbol_length), allocatable :: stmp(:)
   integer :: nat, iat, iid

   nat = size(identity)
   allocate(stmp(nat))
   nid = 0
   do iat = 1, nat
      iid = find_symbol(stmp(:nid), symbol(iat))
      if (iid == 0) then
         call append_symbol(stmp, nid, symbol(iat))
         iid = nid
      end if
      identity(iat) = iid
   end do

end subroutine get_identity_symbol


!> Establish a mapping between unique atom types and species
pure subroutine collect_identical(identity, mapping)

   !> Chemical identity
   integer, intent(in) :: identity(:)

   !> Mapping from unique atoms
   integer, intent(out) :: mapping(:)

   integer :: iid, iat

   do iid = 1, size(mapping)
      do iat = 1, size(identity)
         if (identity(iat) == iid) then
            mapping(iid) = iat
            exit
         end if
      end do
   end do

end subroutine collect_identical


!> Find element symbol in an unordered list, all entries are required to be unique
pure function find_symbol(list, symbol) result(position)

   !> List of element symbols
   character(len=*), intent(in) :: list(:)

   !> Element symbol
   character(len=*), intent(in) :: symbol

   !> Position of the symbol in list if found, otherwise zero
   integer :: position
   integer :: isym

   position = 0
   do isym = 1, size(list)
      if (symbol == list(isym)) then
         position = isym
         exit
      end if
   end do

end function find_symbol


!> Find atomic number in an unordered list, all entries are required to be unique
pure function find_number(list, number) result(position)

   !> List of atomic numbers
   integer, intent(in) :: list(:)

   !> Atomic number
   integer, intent(in) :: number

   !> Position of the number in list if found, otherwise zero
   integer :: position
   integer :: inum

   position = 0
   do inum = 1, size(list)
      if (number == list(inum)) then
         position = inum
         exit
      end if
   end do

end function find_number


!> Append an element symbol to an unsorted list, to ensure no dublicates search
!> for the element symbol first
pure subroutine append_symbol(list, nlist, symbol)

   !> List of element symbols
   character(len=*), allocatable, intent(inout) :: list(:)

   !> Current occupied size of list
   integer, intent(inout) :: nlist

   !> Elements symbol
   character(len=*), intent(in) :: symbol

   if (nlist >= size(list)) then
      call resize(list)
   end if

   nlist = nlist + 1
   list(nlist) = symbol

end subroutine append_symbol


!> Append an atomic number to an unsorted list, to ensure no dublicates search
!> for the atomic number first
pure subroutine append_number(list, nlist, number)

   !> List of atomic number
   integer, allocatable, intent(inout) :: list(:)

   !> Current occupied size of list
   integer, intent(inout) :: nlist

   !> Atomic number
   integer, intent(in) :: number

   if (nlist >= size(list)) then
      call resize(list)
   end if

   nlist = nlist + 1
   list(nlist) = number

end subroutine append_number


end module mctc_io_symbols
