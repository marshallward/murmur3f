use iso_fortran_env, only : int32

!integer(int32) :: x(1) = [int(z'ffff', kind=int32)]
integer(int32), allocatable :: x(:), y(:)

! Empty array
allocate(x(0))
print '(z8, 4x, "       0")', murmur3(x)
print '(z8, 4x, "514E28B7")', murmur3(x, seed=1)
print '(z8, 4x, "81F16F39")', murmur3(x, seed=int(z'ffffffff', kind=int32))

! Single element
print '(z8)', murmur3([0])
allocate(y(1))
y = [int(z'ffffffff', kind=int32)]
print '(z8)', murmur3(y)

contains

function murmur3(key, seed) result(h1)
  integer(int32), intent(in) :: key(:)
  integer(int32), intent(in), optional :: seed
  integer(int32) :: hash

  integer(int32), parameter :: c1 = int(z'cc9e2d51', kind=int32)
  integer(int32), parameter :: c2 = int(z'1b873593', kind=int32)
  integer(int32), parameter :: c3 = int(z'e6546b64', kind=int32)

  integer :: i
  integer(int32) :: k1, h1

  if (present(seed)) then
    h1 = seed
  else
    h1 = 0
  endif

  do i = 1, size(key)
    k1 = key(i)
    k1 = k1 * c1
    k1 = ishftc(k1, 15)
    k1 = k1 * c2

    h1 = ieor(h1, k1)
    h1 = ishftc(h1, 13)
    h1 = h1*5 + c3
  enddo

  ! Tail?  Do I need it?

  h1 = xor(h1, size(key))

  call fmix32(h1)
end function murmur3

subroutine fmix32(h)
  integer(int32), intent(inout) :: h

  integer(int32), parameter :: c1 = int(z'85ebca6b', kind=int32)
  integer(int32), parameter :: c2 = int(z'c2b2ae35', kind=int32)

  h = ieor(h, shiftr(h, 16))
  h = h * c1
  h = ieor(h, shiftr(h, 13))
  h = h * c2
  h = ieor(h, shiftr(h, 16))
end subroutine fmix32

end
