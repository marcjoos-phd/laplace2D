!===============================================================================
!> \file io.f90
!! \brief
!! This is the Input & Output modules.
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
module readIO
contains

  subroutine read_input
    use commons
    use cuda_commons
    implicit none

    namelist /params/ nx, ny, iter_max, write_io
    namelist /cuda_params/ shmem

    open(unit=1, file='input', status='old')
    read(1, params)
#ifdef CUDA
    read(1, cuda_params)
#endif
    close(1)

  end subroutine read_input

end module readIO

module writeIO
contains

#ifdef HDF5
  subroutine write_hdf5(array, nx, ny, nz, fname)
    use hdf5
    use prec
    implicit none

    integer, intent(in) :: nx, ny, nz
    real(dp), dimension(nx,ny,nz), intent(in) :: array
    character(len=*) :: fname


    integer(HID_T) :: fid
    integer(HID_T) :: h5dspace, h5dset
    integer(hsize_t), dimension(3) :: dim
    integer :: rank
    integer :: ierr

    call h5open_f(ierr)
    call h5Fcreate_f(trim(fname), H5F_ACC_TRUNC_F, fid, ierr)
    
    dim = (/ nx, ny, nz /)
    rank = 3

    call h5Screate_simple_f(rank, dim, h5dspace, ierr)
    call h5Dcreate_f(fid, 'array', H5T_NATIVE_DOUBLE, h5dspace, h5dset, ierr)
    call h5Dwrite_f(h5dset, H5T_NATIVE_DOUBLE, array, dim, ierr)
    call h5Dclose_f(h5dset, ierr)
    call h5Sclose_f(h5dspace, ierr)

    call h5Fclose_f(fid, ierr)
    call h5close_f(ierr)
    
    return
  end subroutine write_hdf5
#endif

  subroutine write_posix(array, nx, ny, nz, fname)
    use prec
    implicit none

    integer, intent(in) :: nx, ny, nz
    real(dp), dimension(nx,ny,nz), intent(in) :: array
    character(len=*) :: fname

    fname = trim(fname)//'.bin'

    open(unit=10, file=trim(fname), status='unknown', form='unformatted')
    write(10) nx, ny, nz
    write(10) array
    close(10)

    return
  end subroutine write_posix
  
  subroutine convtoasc(number, sstring)
    implicit none
    integer, intent(in) :: number
    integer :: istring, num, nums10, i
    character(LEN=6), intent(out) :: sstring
    character(LEN=10), parameter  :: nstring="0123456789"
    
    num    = 1000000
    nums10 = num/10
    do i = 1, 6
       istring      = 1 + mod(number,num)/nums10
       sstring(i:i) = nstring(istring:istring)
       num    = num/10
       nums10 = nums10/10
    enddo
  
    return
  end subroutine convtoasc
end module writeIO
