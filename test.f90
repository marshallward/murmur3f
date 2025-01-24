program murmur_test

use, intrinsic :: iso_fortran_env, only : int32, real32, real64

use murmur, only : murmur3, murmur3f

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
print *, murmur3([0]), murmur3f([0.0_4])
print *, murmur3([0, 0]), murmur3f([0._8])

end program murmur_test
