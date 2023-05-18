program main
    implicit none
    
    integer, parameter :: n = 3          ! n is the number differential equations
    integer i
    
    real(kind=8):: t0, ti, dt, tmax
    real(kind=8):: x0(n), xi(n), xt(n) 
   
    ! Define initial conditions
    t0 = 0.0
    x0(1) = 50.0
    x0(2) = 0.0
    x0(3) = 0.0
    
    ! Time-step
    dt = 0.5
    tmax = 40.0
    
        
    ! This loop is used to integration of ODEs using RK4 numerical method
    do while(ti <= tmax)
        ti = t0 + dt
        
        call ODE_RK4(t0, ti, x0, xi, n)
        print*, ti, xi(1), xi(2), xi(3)
           
        ! Adjust time step and initial values for the next time step of integration
        t0 = ti
        do i = 1, n
            x0(i) = xi(i)
        end do
    end do    
end program
