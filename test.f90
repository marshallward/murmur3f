program murmur_test

use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64

use murmur3, only : murmur 

implicit none

integer(int32), allocatable :: x(:), y(:)

print *, test_empty_array()
!!allocate(x(0))
!!print '("Value   ", 4x, "Expected")'
!!print '(z8, 4x, "       0")', murmur(x)
!!print '(z8, 4x, "514E28B7")', murmur(x, seed=1)
!!print '(z8, 4x, "81F16F39")', murmur(x, seed=int(z'ffffffff', kind=int32))

print *, test_zero()
!print '(z8, 4x, "2362F9DE")', murmur([0])
print '(z8, 4x, z8)', murmur([0]), murmur([0._4])
print '(z8, 4x, z8)', murmur([0, 0]), murmur([0._8])

print *, test_unsigned_int()
!!allocate(y(1))
!!y = [int(z'ffffffff', kind=int32)]
!!print '(z8, 4x, "76293B50")', murmur(y)

! Test ordering?
print '("[1,2,3,4]: ", z8)', murmur([1, 2, 3, 4])
print '("[4,3,2,1]: ", z8)', murmur([4, 3, 2, 1])

print *, test_xor_elim()

contains

!! These tests are based on a Stack Overflow answer by Ian Boyd (@ian-boyd).
!! https://stackoverflow.com/questions/14747343/murmurhash3-test-vectors

function test_empty_array() result(success)
  !! Verify that empty arrays produce unique hashes for given seeds.
  !!
  !!  s=0 : A zero seed with zero data produces a zero hash.
  !!  s=1 : A seed of one eliminates most hash calculations.
  !!  s=0xffffffff: Ensure that seed is handled as an unsigned integer.
  integer(kind=int32), parameter :: seeds(3) = [ &
    0, &
    1, &
    int(z'ffffffff', kind=int32) ]

  integer(kind=int32), parameter :: hashes(3) = [ &
      0, &
      int(z'514e28b7', kind=int32), &
      int(z'81f16f39', kind=int32) ]

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

  logical :: success
  integer :: i

  ! Adcroft "method"

  success = .true.

  ! Implicit seed = 0 test
  success = success .and. murmur(a_i32) == 0

  do i = 1, size(seeds)
    success = success .and. murmur(a_i32, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_i64, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r32_1d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r32_2d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r32_3d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r32_4d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r64_1d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r64_2d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r64_3d, seed=seeds(i)) == hashes(i)
    success = success .and. murmur(a_r64_4d, seed=seeds(i)) == hashes(i)
  end do
end function test_empty_array


function test_unsigned_int() result(success)
  integer(kind=int32), parameter :: a(1) = [int(z'ffffffff', kind=int32)]
  integer(kind=int32), parameter :: hash = int(z'76293b50', kind=int32)
  logical :: success

  success = murmur(a) == hash
end function test_unsigned_int

function test_zero() result(success)
  integer(kind=int32), parameter :: a(1) = [0]
  integer(kind=int32), parameter :: hash = int(z'2362f9de', kind=int32)
  logical :: success

  success = murmur(a) == hash
end function test_zero

function test_xor_elim() result(success)
  !< Quoting Ian Boyd, "Special seed value eliminates initial key with xor".
  !<
  !< Select the seed such that ieor(hash, c2*ishftc(k*c1, 15)) == 0, and the
  !< byte iteration equals c3.

  !< NOTE: First byte is 0x21, highest byte is 0x87
  integer(kind=int32), parameter :: a(1) = [int(z'87654321', kind=int32)]
  integer(kind=int32), parameter :: seed = int(z'5082edee', kind=int32)
  integer(kind=int32), parameter :: hash = int(z'2362f9de', kind=int32)
  logical :: success

  !print '(*(z8,4x))', a, seed, hash, murmur(a, seed=seed)
  success = hash == murmur(a, seed=seed)
end function test_xor_elim

end program murmur_test
