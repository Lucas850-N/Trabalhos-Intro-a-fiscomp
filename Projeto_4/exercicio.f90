program exercicio

    !Declaração de variáveis
    implicit none
    integer :: max_i, dim_array, i
    real(8) :: delta_t, g, m_b, par, theta_0, phi_0
    real(8), allocatable, dimension(:) :: x, y, z, v, theta, phi

    max_i = 10000000

    read *, par
    read *, theta_0
    read *, phi_0

    allocate(x(max_i), y(max_i), z(max_i), theta(max_i), phi(max_i))

    call metodo_euler_efeito_magnus(par, max_i, x, y, z, dim_array, theta_0, phi_0)

    open(unit=1, file="chute_out_3d.dat", status="Replace", action="Write")

        do i = 1, dim_array

            write(1, *) x(i), y(i), z(i)

        end do

    close(1)

    if (y(dim_array) > 10.0d0) then

        print *, "O jogador não fará gol. nao"

    elseif (y(dim_array) < 4.0d0) then

        print *, "O jogador não fará gol. nao"

    elseif (z(dim_array) > 2.5d0) then

        print *, "O jogador não fará gol. nao"

    elseif(z(dim_array) < 0.0d0) then

        print *, "O jogador não fará gol. nao"

    else 

        print *, "O jogador fará gol. sim"

    end if



    contains

        subroutine metodo_euler_efeito_magnus(par, max_i, x, y, z, dim_array, theta_0, phi_0)

            real(8), intent(in) ::  par, theta_0, phi_0
            real(8), allocatable, dimension(:) :: v_x, v_y, v_z, v, gamma_2
            integer, intent(in) :: max_i
            integer, intent(out) :: dim_array
            integer :: i
            real(8) :: delta_t, omega, g, m_b, a_1, a_2, delta, v_0, v_0_ms, v_0_x, v_0_y, v_0_z, v_d, omega_si
            real(8), dimension(:), intent(out) :: x, y, z

            allocate(v_x(max_i), v_y(max_i), v_z(max_i), v(max_i), gamma_2(max_i))

            g = 9.8d0
            m_b = 1.0d0

            !Primeiramente, vamos definir as velocidades iniciais em cada eixo
            v_0 = 100.0d0
            v_0_ms = v_0/3.6d0

            v_0_x = v_0_ms*sin(theta_0)*cos(phi_0)
            v_0_y = v_0_ms*sin(theta_0)*sin(phi_0)
            v_0_z = v_0_ms*cos(theta_0)

            v_x(1) = v_0_x
            v_y(1) = v_0_y
            v_z(1) = v_0_z

            x(1) = 0.0d0
            y(1) = 0.0d0
            z(1) = 0.0d0
            v(1) = v_0_ms

            delta_t = 0.01d0
            omega = 39.0d0
            a_1 = 0.0039d0
            a_2 = 0.0058d0
            v_d = 35.0d0
            delta = 5.0d0

            omega_si = omega*2.0d0*acos(-1.0d0)

            do i = 1, max_i-1

                !Agora, implementaremos o cálculo da velocidade
                v(i) = sqrt(v_x(i)**2 + v_y(i)**2 + v_z(i)**2)

                !Vamos calcular o coeficiente gamma_2
                gamma_2(i) = a_1+((a_2)/(1+exp((v(i)-v_d)/delta))) !Esse gamma_2 já contém o m_b no denominador

                !Cálculo velocidades
                v_x(i+1) = v_x(i) - (gamma_2(i)*v(i)*v_x(i) + par*omega_si*v_y(i))*delta_t
                v_y(i+1) = v_y(i) - (gamma_2(i)*v(i)*v_y(i) - par*omega_si*v_x(i))*delta_t
                v_z(i+1) = v_z(i) - (g + gamma_2(i)*v(i)*v_z(i))*delta_t

                !Cálculo posições
                x(i+1) = x(i) + v_x(i)*delta_t
                y(i+1) = y(i) + v_y(i)*delta_t
                z(i+1) = z(i) + v_z(i)*delta_t

                if ( x(i+1) > 40.0d0 ) then

                    dim_array = i+1
                    exit

                end if


            end do

            deallocate(v_x, v_y, v_z, v, gamma_2)

        end subroutine


end program exercicio