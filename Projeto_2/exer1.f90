program exer1

!Declaração de variáveis
        implicit none
        real(8), parameter :: x_0 = 0.5d0
        real(8) :: derivada_ln, derivada_2_ln, derivada_3_ln, ln, f_linha_1, f_linha_2, desvio_derivada_1, desvio_derivada_2, &
          & desvio_derivada_3, desvio_derivada_4, desvio_derivada_5, desvio_derivada_6, desvio_derivada_7, f_linha_3, f_linha_4, &
          & f_linha_5, f_linha_6, f_linha_7
        real(8), allocatable, dimension(:) :: h
        integer :: i, dim_h !dimensão de h
        real(8), allocatable, dimension(:) :: desvio_derivada_array_1, desvio_derivada_array_2, desvio_derivada_array_3, desvio_derivada_array_4&
          &, desvio_derivada_array_5, desvio_derivada_array_6, desvio_derivada_array_7
        
        !Abre o arquivo tab1_in.dat e substitui os valores nele em h
        open(unit=1, file="tab1_in.dat", action="read")
        read(1,*) dim_h
        allocate(h(dim_h))
        read(1,*) h
        close(1)
        
        allocate( desvio_derivada_array_1(dim_h), desvio_derivada_array_2(dim_h), desvio_derivada_array_3(dim_h), &
        & desvio_derivada_array_4(dim_h), desvio_derivada_array_5(dim_h), desvio_derivada_array_6(dim_h), &
        & desvio_derivada_array_7(dim_h))


        call calculo_preciso(x_0, ln, derivada_ln, derivada_2_ln, derivada_3_ln)
        call derivada_1_2_pontos_frente(x_0, f_linha_1, derivada_ln, desvio_derivada_1, h, i, desvio_derivada_array_1)
        call derivada_1_2_pontos_tras(x_0, f_linha_2, derivada_ln, desvio_derivada_2, h, i, desvio_derivada_array_2)
        call derivada_1_3_pontos_simetricos(x_0, f_linha_3, derivada_ln, desvio_derivada_3, h, i, desvio_derivada_array_3)
        call derivada_1_5_pontos_simetricos(x_0, f_linha_4, derivada_ln, desvio_derivada_4, h, i, desvio_derivada_array_4)
        call derivada_2_3_pontos_simetricos(x_0, f_linha_5, derivada_2_ln, desvio_derivada_5, h, i, desvio_derivada_array_5)
        call derivada_2_5_pontos_simetricos(x_0, f_linha_6, derivada_2_ln, desvio_derivada_6, h, i, desvio_derivada_array_6)
        call derivada_3_5_pontos_antisimetricos(x_0, f_linha_7, derivada_3_ln, desvio_derivada_7, h, i, desvio_derivada_array_7)

        !Abre o arquivo tab1_out.dat e coloca os valores lá
        open(unit=2, file="tab1_out.dat", action="write")
        write(2, "(A1, A50, A50, A50, A50, A50, A50)") "h", "derivada simétrica 3 pontos", "derivada para frente 2 pontos",&
          & "derivada para trás 2 pontos", "derivada segunda simétrica 3 pontos", "derivada segunda simétrica 5 pontos",&
          & "derivada terceira anti-simétrica 5 pontos"
        do i = 1, dim_h
          write(2, *) h(i), desvio_derivada_array_3(i), desvio_derivada_array_1(i), desvio_derivada_array_2(i),&
            & desvio_derivada_array_5(i), desvio_derivada_array_6(i), desvio_derivada_array_7(i)
        end do 
        close(2)

        print *, "Melhores valores de h:"
        print *, ""
        print *, "Derivada simétrica 3 pontos:" 
        print *, "h = 0.000001"
        print *, "Esse valor de h apresentou um desvio de 2.3004487204048019E-011, o menor quando comparado com o restante,&
          & indicando maior precisão."
        print *, ""
        print *, "Derivada para frente 2 pontos:"
        print *, "h = 0.00000001"
        print *, "Esse valor de h apresentou um desvio de 7.9055879620426595E-009, o menor quando comparado com o restante,&
          & indicando maior precisão."
        print *, ""
        print *, "Derivada para trás 2 pontos:"
        print *, "h = 0.00000001" 
        print *, "Esse valor de h apresentou um desvio de 4.2108472264601460E-010, o menor quando comparado com o restante,&
          & indicando maior precisão."
        print *, ""
        print *, "Derivada segunda simétrica 3 pontos"
        print *, "h = 0.0001"
        print *, "Esse valor de h apresentou um desvio de 9.3870857753586279E-009, o menor quando comparado com o restante,&
          & indicando maior precisão."
        print *, ""
        print *, "Derivada segunda simétrica 5 pontos"
        print *, "h = 0.001"
        print *, "Esse valor de h apresentou um desvio de 1.2802425786162530E-011, o menor quando comparado com o restante,&
          & indicando maior precisão."
        print *, ""
        print *, "Derivada terceira anti-simétrica 5 pontos"
        print *, "h = 0.0005"
        print *, "Esse valor de h apresentou um desvio de 2.1206759273084685E-006, o menor quando comparado com o restante,&
          & indicando maior precisão."




!Inicio dos subprogramas
        contains
                !Subrotina para o cálculo direto da derivada (valor preciso)
                subroutine calculo_preciso (x, ln, derivada_ln, derivada_2_ln, derivada_3_ln)

                  !Declaração de variáveis
                  real(8), intent(in) :: x 
                  real(8), intent(out) :: derivada_ln, derivada_2_ln, derivada_3_ln, ln

                  ln = log(1.0d0+(x**2))
                  derivada_ln = (2.0d0*x)/(1.0d0+x**2)
                  derivada_2_ln = (-2.0d0*(x**2-1.0d0))/(x**4+2.0d0*x**2+1.0d0)
                  derivada_3_ln = (4.0d0*(x**3-3.0d0*x))/(x**6+3.0d0*x**4+3.0d0*x**2+1.0d0)
        
                end subroutine

                !Subrotina para o cálculo da primeira derivada usando 2 pontos para frente
                subroutine derivada_1_2_pontos_frente(x, f_linha, derivada_ln, desvio_derivada, h, i, desvio_derivada_array)

                  !Declaração de variáveis
                  real(8), intent(in) :: x
                  real(8), intent(in) :: derivada_ln 
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), dimension(:), intent(in) :: h
                  integer, intent(inout) :: i
                  real(8),  dimension(:), intent(out) :: desvio_derivada_array

                  do i = 1, size(h)
                    f_linha = (log(1.0d0+(x+h(i))**2) - log(1.0d0+x**2))/h(i)
                    desvio_derivada = abs(derivada_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                  end do
                end subroutine

                !subrotina para o cálculo da primeira derivada usando 2 pontos para trás 
                subroutine derivada_1_2_pontos_tras(x, f_linha, derivada_ln, desvio_derivada, h, i, desvio_derivada_array)

                  real(8), intent(in) :: x
                  real(8), intent(in) :: derivada_ln
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), dimension(:), intent(in) :: h
                  integer, intent(inout) :: i
                  real(8), dimension(:), intent(out) :: desvio_derivada_array

                    do i = 1, size(h)
                    f_linha = (log(1.0d0 + x**2) - log(1.0d0 + (x-h(i))**2)) / h(i)
                    desvio_derivada = abs(derivada_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                    end do

                end subroutine

                !Subrotina para o cálculo da primeira derivada usando 3 pontos simétricos
                subroutine derivada_1_3_pontos_simetricos(x, f_linha, derivada_ln, desvio_derivada, h, i, desvio_derivada_array)

                  real(8), intent(in) :: x
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), intent(in) :: derivada_ln
                  real(8), dimension(:), intent(in) :: h
                  integer, intent(inout) :: i
                  real(8), dimension(:), intent(out) :: desvio_derivada_array

                  do i = 1, size(h)
                    f_linha = (log(1.0d0+(x+h(i))**2) - log(1.0d0+(x-h(i))**2))/(2.0d0*h(i))
                    desvio_derivada = abs(derivada_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                  end do

                end subroutine

                !Subrotina para o cálculo da primeira derivada usando 5 pontos simétricos 
                subroutine derivada_1_5_pontos_simetricos(x, f_linha, derivada_ln, desvio_derivada, h, i, desvio_derivada_array)

                  real(8), intent(in) :: x
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), intent(in) :: derivada_ln
                  real(8), dimension(:), intent(in) :: h
                  integer, intent(inout) :: i
                  real(8), dimension(:), intent(out) :: desvio_derivada_array

                  do i = 1, size(h)
                    f_linha = ((log(1.0d0+(x-2.0d0*h(i))**2)) + (8.0d0*log(1.0d0+(x+h(i))**2)) - &
                    &(8.0d0*log(1.0d0+(x-h(i))**2)) - (log(1.0d0+(x+2.0d0*h(i))**2)))/(12.0d0*h(i)) !Talvez de erro
                    desvio_derivada = abs(derivada_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                  end do

                end subroutine

                !Subrotina para o cálculo da segunda derivada usando 3 pontos simétricos
                subroutine derivada_2_3_pontos_simetricos(x, f_linha, derivada_2_ln, desvio_derivada, h, i, desvio_derivada_array)

                  real(8), intent(in) :: x
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), intent(in) :: derivada_2_ln
                  real(8), dimension(:), intent(in) :: h
                  integer, intent(inout) :: i
                  real(8), dimension(:), intent(out) :: desvio_derivada_array

                  do i = 1, size(h)
                    f_linha = (log(1.0d0+(x+h(i))**2) + log(1.0d0+(x-h(i))**2) - (2.0d0*log(1.0d0+x**2)))/(h(i)**2)
                    desvio_derivada = abs(derivada_2_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                  end do

                end subroutine

                !Subrotina para o cálculo da segunda derivada usando 5 pontos simétricos
                subroutine derivada_2_5_pontos_simetricos(x, f_linha, derivada_2_ln, desvio_derivada, h, i, desvio_derivada_array)

                  real(8), intent(in) :: x
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), intent(in) :: derivada_2_ln
                  real(8), dimension(:) :: h
                  integer, intent(inout) :: i
                  real(8), dimension(:), intent(out) :: desvio_derivada_array

                  do i = 1, size(h)
                    f_linha = ((-log(1.0d0+(x-2.0d0*h(i))**2)) + (16.0d0*log(1.0d0+(x+h(i))**2)) + (16.0d0*log(1.0d0+(x-h(i))**2)) - &
                    &(log(1.0d0+(x+2.0d0*h(i))**2)) - (30.0d0*(log(1.0d0+x**2))))/(12.0d0*(h(i))**2) !Talvez de erro
                    desvio_derivada = abs(derivada_2_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                  end do

                end subroutine

                !Subrotina para o cálculo da terceira derivada usando 5 pontos anti-simetricos
                subroutine derivada_3_5_pontos_antisimetricos(x, f_linha, derivada_3_ln, desvio_derivada, h, i, desvio_derivada_array)

                  real(8), intent(in) :: x
                  real(8), intent(out) :: f_linha, desvio_derivada
                  real(8), intent(in) :: derivada_3_ln
                  real(8), dimension(:), intent(in) :: h
                  integer, intent(inout) :: i
                  real(8), dimension(:), intent(out) :: desvio_derivada_array

                  do i = 1, size(h)
                    f_linha = (((log(1.0d0+(x+2.0d0*h(i))**2)) - 2.0d0*(log(1.0d0+(x+h(i))**2)) + 2.0d0*(log(1.0d0+(x-h(i))**2)) - &
                    &(log(1.0d0+(x-2.0d0*h(i))**2)))/(2.0d0*(h(i))**3))!Talvez de erro
                    desvio_derivada = abs(derivada_3_ln - f_linha)
                    desvio_derivada_array(i) = desvio_derivada
                  end do


                end subroutine                 



end program exer1
