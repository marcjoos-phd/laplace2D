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
