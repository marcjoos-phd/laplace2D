!===============================================================================
!> \file commons.f90
!! \brief
!! This is the commons modules.
!! \author
!! Marc Joos <marc.joos@cea.fr>
!! \copyright
!! Copyrights 2015, CEA.
!! This file is distributed under the CeCILL-A & GNU/GPL licenses, see
!! <http://www.cecill.info/licences/Licence_CeCILL_V2.1-en.html> and
!! <http://www.gnu.org/licenses/>
!! \date
!! \b created:          02-09-2015 
!! \b last \b modified: 02-20-2015
!<
!===============================================================================
module prec
  integer, parameter :: dp=kind(1.0d0)
end module prec

module commons
  integer :: nx=1024, ny=1024
  integer :: iter_max=100
  logical :: write_io=.false.
end module commons

module cuda_commons
  integer, parameter :: block_dimx=16
  integer, parameter :: block_dimy=32
  integer, parameter :: tile_dimx=16
  integer, parameter :: tile_dimy=16
  logical :: shmem=.false.
end module cuda_commons
