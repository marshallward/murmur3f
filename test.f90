program murmur_test

use, intrinsic :: iso_fortran_env, only : int32, real32, real64

use murmur, only : murmur3

implicit none

integer(int32), allocatable :: x(:), y(:)

! Empty array
allocate(x(0))
print '("Value   ", 4x, "Expected")'
print '(z8, 4x, "       0")', murmur3(x)
print '(z8, 4x, "514E28B7")', murmur3(x, seed=1)
print '(z8, 4x, "81F16F39")', murmur3(x, seed=int(z'ffffffff', kind=int32))

! Single element
print '(z8, 4x, "2362F9DE")', murmur3([0])

allocate(y(1))
y = [int(z'ffffffff', kind=int32)]
print '(z8, 4x, "76293B50")', murmur3(y)
print*
print '("[1,2,3,4]: ", z8)', murmur3([1, 2, 3, 4])
print '("[4,3,2,1]: ", z8)', murmur3([4, 3, 2, 1])

! Some dumb tests
print '(z8, 4x, z8)', murmur3([0]), murmur3([0._4])
print '(z8, 4x, z8)', murmur3([0, 0]), murmur3([0._8])

! Reform as unit tests?

call test_empty_array

contains

subroutine test_empty_array
  integer(kind=real32) :: a_i32(0)
  integer(kind=real64) :: a_i64(0)
  real(kind=real32) :: a_r32(0)
  real(kind=real64) :: a_r64(0)

  integer(kind=int32) :: hash
  integer(kind=int32) :: seed

  print *, murmur3(a_i32) == 0
  print *, murmur3(a_i64) == 0
  print *, murmur3(a_r32) == 0
  print *, murmur3(a_r64) == 0

  print *, murmur3(a_i32) == 0
  print *, murmur3(a_i64) == 0
  print *, murmur3(a_r32) == 0
  print *, murmur3(a_r64) == 0

  hash = murmur3(a_i32, seed=1)
  print *, hash == int(z'514e28b7', kind=int32)

  seed = int(z'ffffffff', kind=int32)
  hash = murmur3(a_i32, seed=seed)
  print *, hash == int(z'81f16f39', kind=int32)

end subroutine test_empty_array

end program murmur_test
