! Define the Ordinary Differential Equations(ODEs) in this file
subroutine diff(t, x, dx)
   implicit none
   
   integer, parameter :: n = 3
   real(kind=8) :: t
   real(kind=8) :: x(n), dx(n)
   real(kind=8), parameter :: k1 = 0.2
   real(kind=8), parameter :: k2 = 0.2
   
   ! Define the ODE or ODEs here   
   dx(1) = -k1*x(1)
   dx(2) = k1*x(1) - k2*x(2)
   dx(3) = k2*x(2)
end subroutine
