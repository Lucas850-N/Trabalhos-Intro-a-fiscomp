program solucao_exata

    !Declaração de variáveis
    implicit none
    integer :: i, dim_array, max_i
    real(8) :: delta_t, pi, m, l, theta_0, theta_0_rad, g = 9.8d0, omega_0, T_sim
    real(8), allocatable, dimension(:) :: t, theta

    pi = 4*atan(1.0d0)
    max_i = 1000000

    read *, m
    read *, l
    read *, theta_0
    read *, delta_t
    read *, T_sim

    allocate(t(max_i), theta(max_i))

    theta_0_rad = (pi/180.0d0)*theta_0

    call calculo_metodo_euler(T_sim, delta_t, l, m, theta_0_rad, t, theta, dim_array, max_i, g, pi)

    open(unit=3, file="solucao_exata_out.dat", status="Replace", action="Write")

        do i = 1, dim_array - 1

            write(3,*) t(i), theta(i)

        end do

    close(3)

    deallocate(t, theta)

    


    contains

        subroutine calculo_metodo_euler(T_sim, delta_t, l, m, theta_0_rad, t, theta, dim_array, max_i, g, pi)

            real(8), intent(in) :: delta_t, l, m, theta_0_rad, g, T_sim, pi
            integer, intent(in) :: max_i
            real(8), dimension(:), intent(out) :: t
            real(8), dimension(:), intent(inout) :: theta
            integer :: i
            real(8) :: omega_0
            real(8), allocatable, dimension(:) :: omega
            integer, intent(out) :: dim_array

            allocate(omega(max_i))

            omega_0 = sqrt(g/l)

            theta(1) = theta_0_rad
            omega(1) = omega_0
            t(1) = 0.0d0

            do i = 1, max_i-1
                
                t(i+1) = i*delta_t
                
                theta(i+1) = theta(1)*cos(omega(1)*t(i+1))

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

            deallocate(omega)

        end subroutine

end program solucao_exata