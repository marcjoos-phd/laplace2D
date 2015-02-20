module kernels
  use prec
  use cuda_commons

contains
  subroutine init(a, anew, nxi, nyi, init_val)
    implicit none

    integer, intent(in) :: nxi, nyi
    real(dp), dimension(nxi,nyi), intent(inout) :: a
    real(dp), dimension(nxi,nyi), intent(inout) :: anew
    real(dp), intent(in) :: init_val
    integer :: i

    do i = 1, nxi
       a(i,1)    = init_val
       anew(i,1) = init_val
    enddo

    return
  end subroutine init

  subroutine kernel_cpu(a, anew, error, nxi, nyi)
    implicit none

    integer, intent(in) :: nxi, nyi
    real(dp), dimension(nxi,nyi), intent(in) :: a
    real(dp), dimension(nxi,nyi), intent(inout) :: anew
    real(dp), intent(inout) :: error
    integer :: i, j

    !$acc kernels loop 
    !$OMP PARALLEL DO SCHEDULE(RUNTIME) REDUCTION(MAX: error)
    do j = 2, nyi-1
       do i = 2, nxi-1
          anew(i,j) = 0.25d0*(a(i-1,j) + a(i+1,j) &
                          & + a(i,j-1) + a(i,j+1))
          error = max(error, abs(anew(i,j) - a(i,j)))
       enddo
    enddo
    !$OMP END PARALLEL DO
    
    return
  end subroutine kernel_cpu

#ifdef CUDA
  attributes(global) subroutine kernel_gpu(a, anew, nxi, nyi)
    implicit none

    integer, value, intent(in) :: nxi, nyi
    real(dp), dimension(nxi,nyi), intent(in) :: a
    real(dp), dimension(nxi,nyi), intent(inout) :: anew
    integer :: i, j

    i = (blockIdx%x - 1)*blockDim%x + threadIdx%x
    j = (blockIdx%y - 1)*blockDim%y + threadIdx%y
    
    if (i > 1 .and. i < nxi-1 .and. j > 1 .and. j < nyi-1) then
       anew(i,j) = 0.25d0*(a(i-1,j) + a(i+1,j) &
                       & + a(i,j-1) + a(i,j+1))
    endif

  end subroutine kernel_gpu

#ifndef FASTREDUCE
  attributes(global) subroutine kernel_gpu_reduce(a, anew, error, nxi, nyi)
    implicit none

    integer, value, intent(in) :: nxi, nyi
    real(dp), dimension(nxi,nyi), intent(in) :: a
    real(dp), dimension(nxi,nyi), intent(inout) :: anew
    real(dp), dimension(nxi/block_dimx+1,nyi/block_dimy+1), intent(inout) :: error
    real(dp), shared, dimension(block_dimx,block_dimy) :: err_sh
    integer :: i, j, k, tx, ty

    i = (blockIdx%x - 1)*blockDim%x + threadIdx%x
    j = (blockIdx%y - 1)*blockDim%y + threadIdx%y
    tx = threadIdx%x
    ty = threadIdx%y
    err_sh(tx,ty) = 0.d0
    call syncthreads()

    if (i > 1 .and. i < nxi .and. j > 1 .and. j < nyi) then
       anew(i,j) = 0.25d0*(a(i-1,j) + a(i+1,j) &
                       & + a(i,j-1) + a(i,j+1))
       err_sh(tx,ty) = abs(anew(i,j) - a(i,j))
    endif
    call syncthreads()
    
    error(blockIdx%x,blockIdx%y) = maxval(err_sh)

  end subroutine kernel_gpu_reduce

  attributes(global) subroutine max_reduce(local_error, error, nxi, nyi)
    implicit none

    integer, value, intent(in) :: nxi, nyi
    real(dp), dimension(nxi,nyi), intent(in) :: local_error
    real(dp), intent(out) :: error
    real(dp), shared, dimension(128) :: shared_error
    integer :: tx, i

    tx = threadIdx%x
    if (tx < nxi .and. tx > 0) &
         & shared_error(tx) = maxval(local_error(tx,:))
    call syncthreads()
    
    error = maxval(shared_error)

  end subroutine max_reduce
#else
  attributes(global) subroutine kernel_gpu_reduce(a, anew, error, nxi, nyi)
    implicit none

    integer, value, intent(in) :: nxi, nyi
    real(dp), dimension(nxi,nyi), intent(in) :: a
    real(dp), dimension(nxi,nyi), intent(inout) :: anew
    real(dp), shared, dimension(block_dimx*block_dimy) :: berr
    real(dp), dimension((nxi/block_dimx+1)*(nyi/block_dimy+1)), intent(inout) :: error
    integer :: i, j, k, tx, ty, tindex, bindex

    i = (blockIdx%x - 1)*blockDim%x + threadIdx%x
    j = (blockIdx%y - 1)*blockDim%y + threadIdx%y
    tx = threadIdx%x
    ty = threadIdx%y
    tindex = tx + (ty - 1)*blockDim%x
    bindex = blockIdx%x + (blockIdx%y - 1)*gridDim%x
    berr(tindex) = 0.d0
    call syncthreads()

    if (i > 1 .and. i < nxi .and. j > 1 .and. j < nyi) then
       anew(i,j) = 0.25d0*(a(i-1,j) + a(i+1,j) &
                       & + a(i,j-1) + a(i,j+1))
       berr(tindex) = abs(anew(i,j) - a(i,j))
    endif
    call syncthreads()
    
    k = block_dimx*block_dimy/2
    do while (k > 0)
       if (tindex < k) then
          berr(tindex) = max(berr(tindex), berr(tindex+k))
       endif
       k = k/2
       call syncthreads()
    enddo
    
    if (tindex == 1) error(bindex) = berr(1)

  end subroutine kernel_gpu_reduce

  attributes(global) subroutine max_reduce(local_error, error, nxi)
    implicit none

    integer, value, intent(in) :: nxi
    real(dp), dimension(nxi), intent(in) :: local_error
    real(dp), intent(out) :: error
    real(dp), shared, dimension(128) :: shared_error
    integer :: tx, i, k

    tx = threadIdx%x
    shared_error(tx) = 0.d0

    do i = tx, nxi, blockDim%x
       shared_error(tx) = max(shared_error(tx), local_error(i))
    enddo
    call syncthreads()

    k = block_dimx/2
    do while (k > 0)
       if (tx < k) then
          shared_error(tx) = max(shared_error(tx), shared_error(tx+k))
       endif
       k = k/2
       call syncthreads()
    enddo
    
    if (tx == 1) error = shared_error(1)

  end subroutine max_reduce
#endif
#endif
end module kernels
