program exer1


    !Declaração de variáveis
    implicit none
    real(8) :: T, delta_t, v_0, t_0 = 0.0d0
    integer :: dim_arrays, i
    integer, parameter :: P = 400, m = 80
    real(8), allocatable, dimension(:) :: v_f, t_f, v_i


    read *, T, delta_t, v_0

    dim_arrays = 1 + int(T/delta_t)

    allocate(v_f(dim_arrays), t_f(dim_arrays), v_i(dim_arrays))



    call calculo_velocidade(T, delta_t, P, m, v_0, dim_arrays, v_f, t_f, v_i)


    open(unit=10, file='vel1_out.dat')

    write(10, *) t_0, v_0

    do i = 1, dim_arrays - 1

        write(10, *) t_f(i), v_f(i)

    end do
    close(10)

    contains
    subroutine calculo_velocidade(T, delta_t, P, m, v_0, dim_arrays, v_f, t_f, v_i)

        implicit none 
        real(8), dimension(:) :: v_i
        real(8), intent(in) :: T, delta_t, v_0
        integer, intent(in) :: dim_arrays, P, m
        real(8), dimension(:), intent(out) :: v_f, t_f
        integer :: i

        v_i(1) = v_0

        do i = 1, dim_arrays - 1

            t_f(i) = i * delta_t

            v_f(i) = v_i(i) + (P/(m*v_i(i)))*delta_t

            v_i(i+1) = v_f(i)
         
        end do 



    end subroutine

    

end program exer1
