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

!> Environment module providing common utilities and error handling.
!>
!> This module exports fundamental types and constants used throughout mctc-lib:
!>
!> - [[error_type]]: Allocatable error type for error propagation
!> - [[fatal_error]]: Subroutine to create error messages
!> - [[wp]]: Working precision for real numbers (double precision)
!> - [[sp]], [[dp]]: Single and double precision kind parameters
!> - [[i1]], [[i2]], [[i4]], [[i8]]: Integer kind parameters (1, 2, 4, 8 bytes)
!>
!> Error handling follows a simple pattern: pass an allocatable [[error_type]]
!> to library routines, then check if it is allocated to detect errors.
!>
!> Example usage:
!>
!>```f90
!> use mctc_env
!> type(error_type), allocatable :: error
!>
!> call some_routine(result, error)
!> if (allocated(error)) then
!>    print '(a)', error%message
!>    error stop
!> end if
!>```
module mctc_env
   use mctc_env_accuracy, only : sp, dp, wp, i1, i2, i4, i8
   use mctc_env_error, only : error_type, fatal_error, mctc_stat
   use mctc_env_system, only : get_argument, get_variable, &
      & is_unix, is_windows
   implicit none
   public

end module mctc_env
