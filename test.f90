program murmur_test

use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64

use murmur3, only : murmur 

implicit none

integer(int32), allocatable :: x(:), y(:)

! Empty array
allocate(x(0))
print '("Value   ", 4x, "Expected")'
print '(z8, 4x, "       0")', murmur(x)
print '(z8, 4x, "514E28B7")', murmur(x, seed=1)
print '(z8, 4x, "81F16F39")', murmur(x, seed=int(z'ffffffff', kind=int32))

! Single element
print '(z8, 4x, "2362F9DE")', murmur([0])

allocate(y(1))
y = [int(z'ffffffff', kind=int32)]
print '(z8, 4x, "76293B50")', murmur(y)
print*
print '("[1,2,3,4]: ", z8)', murmur([1, 2, 3, 4])
print '("[4,3,2,1]: ", z8)', murmur([4, 3, 2, 1])

! Some dumb tests
print '(z8, 4x, z8)', murmur([0]), murmur([0._4])
print '(z8, 4x, z8)', murmur([0, 0]), murmur([0._8])

! Reform as unit tests?

call test_empty_array

contains

subroutine test_empty_array
  integer(kind=int32) :: a_i32(0)
  integer(kind=int64) :: a_i64(0)
  real(kind=real32) :: a_r32_1d(0)
  real(kind=real32) :: a_r32_2d(0,0)
  real(kind=real32) :: a_r32_3d(0,0,0)
  real(kind=real32) :: a_r32_4d(0,0,0,0)
  real(kind=real64) :: a_r64_1d(0)
  real(kind=real64) :: a_r64_2d(0,0)
  real(kind=real64) :: a_r64_3d(0,0,0)
  real(kind=real64) :: a_r64_4d(0,0,0,0)

  integer(kind=int32) :: hash
  integer(kind=int32) :: seed

  ! Implicit seed = 0
  hash = 0
  print *, murmur(a_i32) == hash
  print *, murmur(a_i64) == hash
  print *, murmur(a_r32_1d) == hash
  print *, murmur(a_r32_2d) == hash
  print *, murmur(a_r32_3d) == hash
  print *, murmur(a_r32_4d) == hash
  print *, murmur(a_r64_1d) == hash
  print *, murmur(a_r64_2d) == hash
  print *, murmur(a_r64_3d) == hash
  print *, murmur(a_r64_4d) == hash

  seed = 1
  hash = int(z'514e28b7', kind=int32)
  print *, murmur(a_i32, seed=seed) == hash
  print *, murmur(a_i64, seed=seed) == hash
  print *, murmur(a_r32_1d, seed=seed) == hash
  print *, murmur(a_r32_2d, seed=seed) == hash
  print *, murmur(a_r32_3d, seed=seed) == hash
  print *, murmur(a_r32_4d, seed=seed) == hash
  print *, murmur(a_r64_1d, seed=seed) == hash
  print *, murmur(a_r64_2d, seed=seed) == hash
  print *, murmur(a_r64_3d, seed=seed) == hash
  print *, murmur(a_r64_4d, seed=seed) == hash

  seed = int(z'ffffffff', kind=int32)
  hash = int(z'81f16f39', kind=int32)
  print *, murmur(a_i32, seed=seed) == hash
  print *, murmur(a_i64, seed=seed) == hash
  print *, murmur(a_r32_1d, seed=seed) == hash
  print *, murmur(a_r32_2d, seed=seed) == hash
  print *, murmur(a_r32_3d, seed=seed) == hash
  print *, murmur(a_r32_4d, seed=seed) == hash
  print *, murmur(a_r64_1d, seed=seed) == hash
  print *, murmur(a_r64_2d, seed=seed) == hash
  print *, murmur(a_r64_3d, seed=seed) == hash
  print *, murmur(a_r64_4d, seed=seed) == hash
end subroutine test_empty_array

end program murmur_test
