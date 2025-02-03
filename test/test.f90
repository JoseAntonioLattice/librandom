program test

  use iso_fortran_env, only : dp => real64
  use random

  implicit none
  integer :: outunit, i

  open(newunit = outunit, file = 'test/random.dat' )


  do i = 1, 10**5
     write(outunit,*) uniform(), normal(), exponential()
  end do

  
end program test
