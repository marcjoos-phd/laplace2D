!===============================================================================
!> \file laplace.f90
!! \brief
!! \b laplace2d:
!! This is the main program.
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
program laplace
#ifdef OMP
  use OMP_LIB
#endif
#ifdef OACC
  use openacc
#endif
#ifdef CUDA
  use cudafor
  use cuda_commons
#endif
  use commons
  use kernels
  use readIO
  use writeIO
  implicit none

  real(dp), allocatable, dimension(:,:) :: a, anew
  real(dp), parameter :: tolerance=1.d-6
  real(dp) :: error=1.d0
  integer  :: nbthreads=1
  integer  :: i, j, iter, c1, c2, rate

  character(len=20) :: fname, numout

#ifdef CUDA
  real(dp), device, allocatable, dimension(:,:) :: adev, adevnew
#ifndef FASTREDUCE
  real(dp), device, allocatable, dimension(:,:) :: edev
#else
  real(dp), device, allocatable, dimension(:) :: edev
#endif
  real(dp), device :: error_dev
  integer    :: num_device, h_status
  type(dim3) :: dimGrid, dimBlock
  integer    :: ierrSync, ierrAsync
#endif

#ifdef OACC
  print '("Execution with OpenACC")'
#endif
#ifdef OMP
  !$OMP PARALLEL
  !$OMP MASTER
  nbthreads = omp_get_num_threads()
  print '("Execution with OpenMP on ", I2, " threads")', nbthreads
  !$OMP END MASTER
  !$OMP END PARALLEL
#endif
#ifdef CUDA
  print '("Execution with CUDA")'

  num_device = 0
  h_status   = cudaSetDevice(num_device)

  dimGrid  = dim3(nx/block_dimx+1, ny/block_dimy+1, 1)
  dimBlock = dim3(block_dimx, block_dimy, 1)
#endif

  call read_input

  allocate(a(nx,ny), anew(nx,ny))
  
  call init(a, anew, nx, ny, 1.d0)

#ifdef CUDA
  allocate(adev(nx,ny), adevnew(nx,ny))
#ifndef FASTREDUCE
  allocate(edev(dimGrid%x, dimGrid%y))
#else
  allocate(edev(dimGrid%x*dimGrid%y))
#endif
  adev    = a
  adevnew = anew
#endif
  
  iter = 1
#ifdef OACC
  !$acc data copyin(a, anew)
#endif
  call system_clock(count=c1)
  do while ((error > tolerance) .and. (iter <= iter_max))
#ifdef CUDA
     error = 0.d0
#ifdef WITHCUFPRAGMA
     call kernel_gpu<<<dimGrid, dimBlock>>>(adev, adevnew, nx, ny)

     !$CUF kernel do <<<*,*>>>
     do j = 2, ny-1
        do i = 1, nx-1
           error = max(error, abs(adev(i,j) - adevnew(i,j)))
        enddo
     enddo
#else
     error_dev = error

     if (shmem) then
        call kernel_gpu_shmem<<<dimGrid, dimBlock>>>(adev, adevnew, edev, nx, ny)
     else
        call kernel_gpu_reduce<<<dimGrid, dimBlock>>>(adev, adevnew, edev, nx, ny)
     endif

#ifdef DEBUG_CUDA
     ierrSync  = cudaGetLastError()
     ierrAsync = cudaDeviceSynchronize()
     if (ierrSync /= cudaSuccess) write(*,*) &
          'Sync kernel error:', cudaGetErrorString(ierrSync)
     if (ierrAsync /= cudaSuccess) write(*,*) &
          'Async kernel error:', cudaGetErrorString(ierrAsync)
#endif

#ifndef FASTREDUCE
     call max_reduce<<<1, dimGrid%x>>>(edev, error_dev, dimGrid%x, dimGrid%y)
#else
     call max_reduce<<<1, dimGrid%x>>>(edev, error_dev, dimGrid%x*dimGrid%y)
#endif

#ifdef DEBUG_CUDA
     ierrSync  = cudaGetLastError()
     ierrAsync = cudaDeviceSynchronize()
     if (ierrSync /= cudaSuccess) write(*,*) &
          'Sync kernel error:', cudaGetErrorString(ierrSync)
     if (ierrAsync /= cudaSuccess) write(*,*) &
          'Async kernel error:', cudaGetErrorString(ierrAsync)
#endif

     error = error_dev
#endif
     adev = adevnew
#else
     error = 0.d0
     call kernel_cpu(a, anew, error, nx, ny)
     
     !$acc kernels
     !$OMP PARALLEL WORKSHARE
     a = anew
     !$OMP END PARALLEL WORKSHARE
     !$acc end kernels
#endif

     if (mod(iter,iter_max/10) == 0) then
        print '("Iteration # ", I5, ", error: ", E14.8)', iter, error
        ! Write I/O
        if (write_io) then
           call convtoasc(iter, numout)
           fname = "data_seq_"//trim(numout)//'.h5'
#ifdef OACC
           fname = "data_oacc_"//trim(numout)//'.h5'
           !$acc update host(a)
#endif
#ifdef OMP
           fname = "data_omp_"//trim(numout)//'.h5'
#endif
#ifdef CUDA
           a = adev
           fname = "data_cuda_"//trim(numout)//'.h5'
#endif
#ifdef HDF5
           call write_hdf5(a, nx, ny, 1, trim(fname))
#else
           call write_posix(a, nx, ny, 1, trim(fname))
#endif
        endif
     endif
     iter = iter + 1
  enddo
  call system_clock(count=c2, count_rate=rate)
#ifdef OACC
  !$acc end data
#endif

  deallocate(a, anew)
#ifdef CUDA
  deallocate(adev, adevnew)
  deallocate(edev)
#endif  

  print '("\nElapsed time/iteration: ", E14.8, " s")', (c2-c1)/iter_max/(rate*1.d0)
  
end program laplace
