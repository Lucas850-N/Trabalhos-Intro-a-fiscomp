program exer2

    !Declaração de variáveis
    implicit none
    integer :: angulacao_graus, i, max_i, dim_array, dim_array_direto
    real(8) :: pi, delta_t = 0.1d0, v_0 = 500.0d0, g = 9.8d0, angulacao_rad
    real(8), dimension(:), allocatable :: v_x, v_y
    real(8), dimension(:), allocatable :: x, y, x_direto, y_direto   


    max_i = 10000000
    allocate(x(max_i), y(max_i), v_x(max_i), v_y(max_i), x_direto(max_i), y_direto(max_i))
    pi = 4*atan(1.0d0)

    read *, angulacao_graus

    angulacao_rad = (pi/180.0)*angulacao_graus

    call calculo_metodo_euler(x, y, delta_t, g, angulacao_rad, v_0, max_i, dim_array)
    
    open(unit=1, file="output2.txt", status="Replace", action="Write")

        do i = 1, dim_array

            write(1, *) x(i), y(i)

        end do

    close(1)

    call calculo_metodo_direto(x_direto, y_direto, delta_t, g, max_i, dim_array_direto, v_0, pi)

    open(unit=2, file="trajexata.txt", status="Replace", action="Write")

        do i = 1, dim_array_direto

            write(2, *) x_direto(i), y_direto(i)

        end do

    close(2)
   

    contains

        subroutine calculo_metodo_euler(x, y, delta_t, g, angulacao_rad, v_0, max_i, dim_array)

            real(8), dimension(:), allocatable :: v_x, v_y
            real(8), dimension(:), intent(inout) :: x, y  
            real(8), intent(in) :: delta_t, g, angulacao_rad, v_0
            integer, intent(in) :: max_i
            integer :: i
            integer, intent(out) :: dim_array

            allocate(v_x(max_i), v_y(max_i))

            v_x(1) = v_0*cos(angulacao_rad)
            v_y(1) = v_0*sin(angulacao_rad)

            x(1) = 0.0d0
            y(1) = 0.0d0

            do i = 1, max_i - 1

                !Primeiro, as contas p/ a coordenada x
                v_x(i+1) = v_x(i)
                x(i+1) = x(i) + v_x(i)*delta_t

                !Agora, serão feitas as contas p/ coordenada y
                v_y(i+1) = v_y(i) -(g*delta_t)
                y(i+1) = y(i) + v_y(i)*delta_t

                if (y(i+1) < 0.0d0) then

                    dim_array = i
                    exit

                end if
            end do

        deallocate(v_x, v_y)

        end subroutine

        subroutine calculo_metodo_direto(x, y, delta_t, g, max_i, dim_array, v_0, pi)

            real(8) :: v_x_inicial, v_y_inicial
            real(8), dimension(:), intent(inout) :: x, y  
            real(8), intent(in) :: delta_t, g, v_0, pi
            integer, intent(in) :: max_i
            integer :: i
            real(8) :: angulo_melhor, t
            integer, intent(out) :: dim_array

            angulo_melhor = (pi/180.0)*45

            v_x_inicial = v_0*cos(angulo_melhor)
            v_y_inicial = v_0*sin(angulo_melhor)

            x(1) = 0.0d0
            y(1) = 0.0d0

            do i = 1, max_i - 1

                t = i*delta_t

                !Primeiro, as contas p/ a coordenada x
                x(i+1) = v_x_inicial*t

                !Agora, serão feitas as contas p/ coordenada y
                y(i+1) = v_y_inicial*t - ((g/2)*(t**2))

                if (y(i+1) < 0.0d0) then

                    dim_array = i
                    exit

                end if
            end do

        end subroutine

end program exer2