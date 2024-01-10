use iso_fortran_env, only : int32
implicit none

!integer(int32) :: x(1) = [int(z'ffff', kind=int32)]
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

contains

function murmur3(key, seed) result(hash)
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

  hash = ieor(hash, 4*size(key))

  !call fmix32(h1)
  hash = ieor(hash, shiftr(hash, 16))
  hash = hash * c4
  hash = ieor(hash, shiftr(hash, 13))
  hash = hash * c5
  hash = ieor(hash, shiftr(hash, 16))
end function murmur3

end
