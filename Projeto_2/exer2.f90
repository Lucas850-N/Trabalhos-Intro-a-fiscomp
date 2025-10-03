program exer2

  !Declaração de variáveis
  implicit none
  integer, parameter :: a = 0, b = 1
  integer :: dim_N, i, j
  real(8), allocatable, dimension(:) :: h, desvio_integral_simpson, desvio_integral_Bode, desvio_integral_trapezio
  integer, allocatable, dimension(:) :: N
  real(8) :: integral_precisa 

  !Abertura do arquivo tab2_in.dat para substituição de valores
  open(unit=1, file="tab2_in.dat", status="Old", action="Read")
  read(1, *) dim_N
  allocate(N(dim_N))
  read(1, *) N
  close(1)

  allocate(h(dim_N), desvio_integral_trapezio(dim_N), desvio_integral_simpson(dim_N), desvio_integral_Bode(dim_N))

  do i = 1, dim_N
    h(i) = Real((b - a), 8)/Real(N(i), 8)
  end do

  call calculo_preciso_integral(integral_precisa)

  call regra_do_trapezio(integral_precisa, desvio_integral_trapezio, h, N, dim_N)

  call regra_de_Simpson(integral_precisa, desvio_integral_simpson, h, N, dim_N)

  call regra_de_Bode(integral_precisa, desvio_integral_Bode, h, N, dim_N)

  open(unit=2, file="tab2_out.dat", status="Replace", action="write")
  write(2, "(10X, A1, 10X, A1, 22X, A18, 10X, A16, 9X, A13)") "N", "h", "Regra do trapézio", "Regra de Simpson", "Regra de Bode"
  do i = 1, dim_N
   write(2, *) N(i), h(i), desvio_integral_trapezio(i), desvio_integral_simpson(i), desvio_integral_Bode(i) 
  end do
  close(2)

   print *, "Melhores valores de N:"
   print *, ""
   print *, "Regra do trapézio" 
   print *, "N = 4096"
   print *, "Justificativa: Esse valor de N apresentou maior precisão por ter o menor desvio, no valor de", desvio_integral_trapezio(11) 
   print *, ""
   print *, "Regra de Simpson"
   print *, "N = 4096"
   print *, "Justificativa: Esse valor de N apresentou maior precisão por ter o menor desvio, no valor de", desvio_integral_simpson(11)
   print *, ""
   print *, "Regra de Bode"
   print *, "N = 2048" 
   print *, "Justificativa: Esse valor de N apresentou maior precisão por ter o menor desvio, no valor de", desvio_integral_Bode(10)

  contains
     
    !Subrotina para o cálculo bruto da integral 
    subroutine calculo_preciso_integral(integral_precisa)
      real(8) :: integral_precisa

      integral_precisa = 2.0d0*cos(1.0d0) - sin(1.0d0)

    end subroutine

    !Subrotina para a regra do Trapézio
    subroutine regra_do_trapezio(integral_precisa, desvio_integral, h, N, dim_N)
      
      !Declaração de variáveis
      real(8) :: integral_trapezio, meio, meio_soma, extremidade_1, extremidade_2, x_j
      real(8), intent(in) :: integral_precisa
      real(8), dimension(:), intent(out) :: desvio_integral
      real(8), dimension(:), intent(in) :: h
      integer, dimension(:), intent(in) :: N
      integer :: i, j
      integer, intent(in) :: dim_N

       do i = 1, dim_N
       meio_soma = 0.0d0
       integral_trapezio = 0.0d0

        do j = 0, N(i)
        x_j = j*h(i) 
        
        !Aqui, a expressão foi aberta e foi achado um padrão em que os extremos são multiplicados por 1 e todos os termos no meio
        !por 2
        if (j == 0) then
          extremidade_1 = (h(i)/2.0d0)*((x_j**2)*cos(x_j))
        else if (j == N(i)) then
          extremidade_2 = (h(i)/2.0d0)*((x_j**2)*cos(x_j))
        else
          meio = h(i)*((x_j**2)*cos(x_j))
          meio_soma = meio + meio_soma !Guarda e soma os valores do meio da expressão
        end if
          end do
        integral_trapezio = extremidade_1 + extremidade_2 + meio_soma
        desvio_integral(i) = abs(integral_trapezio - integral_precisa)
      end do  
    end subroutine

    subroutine regra_de_Simpson(integral_precisa, desvio_integral, h, N, dim_N)

      real(8) :: integral_Simpson, meio_soma_impar, meio_soma_par, extremidade_1, extremidade_2, meio_impar, meio_par, &
        &meio_soma_pares, x_j
      real(8), intent(in) :: integral_precisa
      real(8), dimension(:), intent(out) :: desvio_integral
      real(8), dimension(:), intent(in) :: h
      integer, dimension(:), intent(in) :: N
      integer :: i, j
      integer, intent(in) :: dim_N


       do i = 1, dim_N

        integral_Simpson = 0.0d0
        meio_soma_impar = 0.0d0
        meio_soma_par = 0.0d0

        do j = 0, N(i)
        x_j = j*h(i) 
        
        if (j == 0) then
          extremidade_1 = (h(i)/3.0d0)*((x_j**2)*cos(x_j))
        else if (j == N(i)) then
          extremidade_2 = (h(i)/3.0d0)*((x_j**2)*cos(x_j))
        else if (mod(j,2) == 0) then !A diferença nessa regra para a última é que agora o coeficiente dos termos do meio são diferentes
        !índices pares e ímpares (nossa condição confere se o índice é par)
          meio_par = (2*h(i)/3)*((x_j**2)*cos(x_j))
          meio_soma_par = meio_par + meio_soma_par
        else if (mod(j,2) /= 0) then
          meio_impar = (4*h(i)/3)*((x_j**2)*cos(x_j))
          meio_soma_impar = meio_impar + meio_soma_impar
        end if
          end do
        integral_Simpson = extremidade_1 + extremidade_2 + meio_soma_par + meio_soma_impar
        desvio_integral(i) = abs(integral_Simpson - integral_precisa)
      end do  
    end subroutine 

    !Subrotina para a regra de Bode
    subroutine regra_de_Bode(integral_precisa, desvio_integral, h, N, dim_N)

      real(8) :: integral_Bode, meio_impar, meio_par_sem4, meio_par_com4, meio_soma_impar, meio_soma_pares_com4, &
      &extremidade_1, extremidade_2, meio_soma, meio_soma_pares_sem4, x_j
      real(8), intent(in) :: integral_precisa
      real(8), dimension(:), intent(out) :: desvio_integral
      real(8), dimension(:), intent(in) :: h
      integer, dimension(:), intent(in) :: N
      integer :: i, j
      integer, intent(in) :: dim_N

       do i = 1, dim_N
        integral_Bode = 0.0d0
        meio_soma = 0.0d0
        meio_soma_impar = 0.0d0
        meio_soma_pares_com4 = 0.0d0
        meio_soma_pares_sem4 = 0.0d0

        do j = 0, N(i)
        x_j = j*h(i) 
        
        if (j == 0) then
          extremidade_1 = (7.0d0*2.0d0*h(i)/45.0d0)*((x_j**2)*cos(x_j))

        else if (j == N(i)) then
          extremidade_2 = (7.0d0*2.0d0*h(i)/45.0d0)*((x_j**2)*cos(x_j)) !Os termos nas extremidades serão multiplicados por 7

        else if (mod(j,2) /= 0) then
          meio_impar = (32.0d0*2.0d0*h(i)/45.0d0)*((x_j**2)*cos(x_j))
          meio_soma_impar = meio_impar + meio_soma_impar

        else if (mod(j,4) == 0) then 
          meio_par_com4 = (14.0d0*2.0*h(i)/45.0d0)*((x_j**2)*cos(x_j))
          meio_soma_pares_com4 = meio_par_com4 + meio_soma_pares_com4
        
        else if (mod(j,2) == 0) then !Aqui verificamos se o número é par e se é múltiplo de 4 para obedecer o padrão achado na
        !expressão
          meio_par_sem4 = (12.0d0*2.0d0*h(i)/45.0d0)*((x_j**2)*cos(x_j))
          meio_soma_pares_sem4 = meio_par_sem4 + meio_soma_pares_sem4

        end if
          end do

        integral_Bode = extremidade_1 + extremidade_2 + meio_soma_pares_sem4 + meio_soma_pares_com4 + meio_soma_impar
        desvio_integral(i) = abs(integral_Bode - integral_precisa)
      end do  
  
    end subroutine 
end program exer2
