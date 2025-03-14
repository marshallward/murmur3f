module murmur3

use iso_fortran_env, only : int32, int64, real32, real64
implicit none

interface murmur
  procedure murmur3_i32
  procedure murmur3_i64
  procedure murmur3_r32
  procedure murmur3_r32_2d
  procedure murmur3_r32_3d
  procedure murmur3_r32_4d
  procedure murmur3_r64
  procedure murmur3_r64_2d
  procedure murmur3_r64_3d
  procedure murmur3_r64_4d
end interface murmur

contains

function murmur3_i32(key, seed) result(hash)
  integer(int32), intent(in) :: key(:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32), parameter :: c1 = int(z'cc9e2d51', kind=int32)
  integer(int32), parameter :: c2 = int(z'1b873593', kind=int32)
  integer(int32), parameter :: c3 = int(z'e6546b64', kind=int32)

  integer(int32), parameter :: c4 = int(z'85ebca6b', kind=int32)
  integer(int32), parameter :: c5 = int(z'c2b2ae35', kind=int32)

  integer :: i
  integer(int32) :: k

  if (present(seed)) then
    hash = seed
  else
    hash = 0
  endif

  do i = 1, size(key)
    k = key(i)
    k = k * c1
    k = ishftc(k, 15)
    k = k * c2

    hash = ieor(hash, k)
    hash = ishftc(hash, 13)
    hash = 5 * hash + c3
  enddo

  ! NOTE: We don't need to handle trailing bytes, since we are converting
  ! floats to arrays of 32-byte integers.

  hash = ieor(hash, 4*size(key))

  hash = ieor(hash, ishft(hash, -16))
  hash = hash * c4
  hash = ieor(hash, ishft(hash, -13))
  hash = hash * c5
  hash = ieor(hash, ishft(hash, -16))
end function murmur3_i32


function murmur3_i64(key, seed) result(hash)
  integer(int64), intent(in) :: key(:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(2*size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_i64


function murmur3_r32(key, seed) result(hash)
  real(real32), intent(in) :: key(:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r32


function murmur3_r32_2d(key, seed) result(hash)
  real(real32), intent(in) :: key(:,:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r32_2d


function murmur3_r32_3d(key, seed) result(hash)
  real(real32), intent(in) :: key(:,:,:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r32_3d


function murmur3_r32_4d(key, seed) result(hash)
  real(real32), intent(in) :: key(:,:,:,:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r32_4d


function murmur3_r64(key, seed) result(hash)
  real(real64), intent(in) :: key(:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(2*size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r64


function murmur3_r64_2d(key, seed) result(hash)
  real(real64), intent(in) :: key(:,:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(2*size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r64_2d


function murmur3_r64_3d(key, seed) result(hash)
  real(real64), intent(in) :: key(:,:,:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(2*size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r64_3d


function murmur3_r64_4d(key, seed) result(hash)
  real(real64), intent(in) :: key(:,:,:,:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32) :: ikey(2*size(key))

  hash = murmur(transfer(key, ikey), seed=seed)
end function murmur3_r64_4d

end module murmur3
