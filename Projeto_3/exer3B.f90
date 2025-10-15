program exer3B

    !Declaração de variáveis
    implicit none
    integer :: i, dim_array, max_i
    real(8) :: delta_t, pi, m, l, theta_0, theta_0_rad, g = 9.8d0, omega_0 = 0.0d0, T_sim
    real(8), allocatable, dimension(:) :: t, theta, omega, E_T, E_p, E_c

    pi = 4*atan(1.0d0)
    max_i = 1000000

    read *, m
    read *, l
    read *, theta_0
    read *, delta_t
    read *, T_sim

    allocate(t(max_i), theta(max_i), omega(max_i), E_T(max_i), E_c(max_i), E_p(max_i))

    theta_0_rad = (pi/180.0d0)*theta_0

    call calculo_metodo_euler(T_sim, delta_t, l, m, theta_0_rad, t, theta, dim_array, max_i, g, omega_0, pi, omega)

    open(unit=2, file="exer3B_out.dat", status="Replace", action="Write")

        do i = 1, dim_array - 1

            write(2,*) t(i), theta(i)

        end do

    close(2)

        !Cálculo da energia total
    do i = 1, max_i

        E_c(i) = 1/2*(m*l*(omega(i)**2)) 
        E_p(i) = m*g*l*(1 - cos(theta(i)))
        E_T(i) = E_c(i) + E_p(i)

    end do

    open(unit=5, file="Energia_Euler_Cromer.dat", status="Replace", action="Write")

        do i = 1, dim_array

            write(5, *) t(i), E_T(i)

        end do

    close(5)

    deallocate(t, theta)

    


    contains

        subroutine calculo_metodo_euler(T_sim, delta_t, l, m, theta_0_rad, t, theta, dim_array, max_i, g, omega_0, pi, omega)

            real(8), intent(in) :: delta_t, l, m, theta_0_rad, g, omega_0, T_sim, pi
            integer, intent(in) :: max_i
            real(8), dimension(:), intent(out) :: t
            real(8), dimension(:), intent(inout) :: theta, omega
            integer :: i
            integer, intent(out) :: dim_array

            theta(1) = theta_0_rad
            omega(1) = omega_0
            t(1) = 0.0d0

            do i = 1, max_i-1
                
                t(i+1) = i*delta_t

                omega(i+1) = omega(i) - (g/l)*theta(i)*delta_t

                theta(i+1) = theta(i) + omega(i+1)*delta_t

                if (t(1+i) > T_sim) then

                    dim_array = i+1
                    exit

                end if

                do while (theta(i+1) > pi)
                    
                    theta(i+1) = theta(i+1) - 2.0d0*pi

                end do

                do while (theta(i+1) < -pi)

                    theta(i+1) = theta(i+1) + 2.0d0*pi

                end do


            end do 

        end subroutine

end program exer3B