program exer1

!Declaração de variáveis
        implicit none
        real, parameter :: x_0 = 0.5
        real :: derivada_ln, derivada_2_ln, derivada_3_ln, ln

        call calculo_preciso(x_0, ln, derivada_ln, derivada_2_ln, derivada_3_ln)

!Inicio dos subprogramas
        contains
                !Subrotina para o cálculo direto da derivada (valor preciso)
                subroutine calculo_preciso (x, ln, derivada_ln, derivada_2_ln, derivada_3_ln)


                  !Declaração de variáveis
                  real :: x 
                  real :: derivada_ln, derivada_2_ln, derivada_3_ln, ln
                  ln = log(1+(x**2))
                  derivada_ln = (2*x_0)/(1+x**2)
                  derivada_2_ln = (-2*(x**2-1))/(x**4+2*x**2+1)
                  derivada_3_ln = (4*(x**3-3*x))/(x**6+3*x**4+3*x**2+1)

                  print *, derivada_ln, derivada_2_ln, derivada_3_ln
        
                end subroutine

                !Subrotina para o cálculo da primeira derivada usando 2 pontos para frente
                subroutine derivada_1_2_pontos_frente

                  !Declaração de variáveis
                  real :: x, f_linha
                  real(8), dimension(14), h
                  integer :: i

                  do i = 1, 14
                    f_linha = (log(1+(x+h(i))**2) - log(1+x))/h(i)
                  end do





                end subroutine

                !subrotina para o cálculo da primeira derivada usando 2 pontos para trás 
                subroutine derivada_1_2_pontos_trás

                end subroutine

                !Subrotina para o cálculo da primeira derivada usando 3 pontos simétricos
                subroutine derivada_1_3_pontos_simetricos

                end subroutine

                !Subrotina para o cálculo da primeira derivada usando 5 pontos simétricos 
                subroutine derivada_1_5_pontos_simetricos

                end subroutine

                !Subrotina para o cálculo da segunda derivada usando 3 pontos simétricos
                subroutine derivada_2_3_pontos_simetricos

                end subroutine

                !Subrotina para o cálculo da segunda derivada usando 5 pontos simétricos
                subroutine derivada_2_5_pontos_simetricos

                end subroutine

                !Subrotina para o cálculo da terceira derivada usando 5 pontos anti-simetricos
                subroutine derivada_3_5_pontos_antisimetricos

                end subroutine                 



end program exer1
