program exer3

  implicit none
  integer :: interacoes, i
  integer, allocatable, dimension(:) :: interacoes_array
  real(8), allocatable, dimension(:) :: m_1, m_2, m_3  !Uma array para cada raíz do método de busca
  real(8), allocatable, dimension(:) :: x_novo_N_1, x_novo_N_2, x_novo_N_3  !Uma array para cada raíz de Newton-Raphson
  real(8), allocatable, dimension(:) :: x_novo_s_1, x_novo_s_2, x_novo_s_3  !Uma array para cada raíz de secantes
  real(8) :: a = -2.0d0, b = -0.5d0  , d = 2.0d0, c = 0.5d0, e = 1.1d0, f = 3.1d0 !Chutes p/ método de busca direta
  real(8) :: g = -2.0d0, h = 1.5d0, k = 4.0d0 !Chutes p/ Newton-Raphson
  real(8) :: l = -3.0d0, o = -0.5d0, p = 0.2d0, q= 2.0d0, r = 1.5d0, s = 5.0d0 !Chutes p/ secante
  read *, interacoes

  allocate(m_1(interacoes), m_2(interacoes), m_3(interacoes), x_novo_N_1(interacoes), x_novo_N_2(interacoes), &
    &x_novo_N_3(interacoes), x_novo_s_1(interacoes), x_novo_s_2(interacoes), x_novo_s_3(interacoes), interacoes_array(interacoes))

  do i = 1, interacoes

   interacoes_array(i) = i 

  end do 

  call metodo_busca_direta(interacoes, a, b, m_1)
  call metodo_busca_direta(interacoes, c, d, m_2)
  call metodo_busca_direta(interacoes, e, f, m_3)

  call metodo_Newton_Raphson(interacoes, g, x_novo_N_1)
  call metodo_Newton_Raphson(interacoes, h, x_novo_N_2)
  call metodo_Newton_Raphson(interacoes, k, x_novo_N_3)

  call metodo_secantes(interacoes, l, o, x_novo_s_1)
  call metodo_secantes(interacoes, p, q, x_novo_s_2)
  call metodo_secantes(interacoes, r, s, x_novo_s_3)

  open(unit=1, file="tab3_out.dat", status="replace", action="write")
  write(1, "(10X, A5, 10X, A4, 20X, A4, 20X, A4, 23X, A3, 23X, A3, 22X, A3, 22X, A4, 22X, A4, 22X, A4)") "inter", "dir1", "dir2", &
    &"dir3", "NR1", "NR2", "NR3", "sec1", "sec2", "sec3"

  do i = 1, interacoes
    
    write(1, *) interacoes_array(i), m_1(i), m_2(i), m_3(i), x_novo_N_1(i), x_novo_N_2(i), x_novo_N_3(i), x_novo_s_1(i), &
      &x_novo_s_2(i), x_novo_s_3(i)

  end do  
  

  contains
    
    subroutine metodo_busca_direta(interacoes, a, b, m)

    implicit none
    integer, intent(in) :: interacoes
    integer :: i
    real(8), intent(in) :: a, b
    real(8) :: a_local, b_local
    real(8) :: f_a, f_b, f_m
    real(8), intent(out), dimension(:) :: m

    a_local = a
    b_local = b

    f_a = a_local**3 - 3*a_local**2 - a_local + 3
    f_b = b_local**3 - 3*b_local**2 - b_local + 3

    do i = 1, interacoes
    
    m(i) = (a_local + b_local)/2
    f_m = m(i)**3 - 3*m(i)**2 - m(i) + 3

    if (f_m * f_a > 0) then !Verifica se f_a e f_m tem o mesmo sinal
      a_local = m(i)
      f_a = f_m
     
    else if (f_m * f_a < 0) then !Verifica se f_a e f_m tem sinais opostos
      
      b_local = m(i)
      f_b = f_m

    end if 

    end do

    end subroutine

                        
    subroutine metodo_Newton_Raphson(interacoes, c, x_novo)

      implicit none
      integer, intent(in) :: interacoes
      integer :: i
      real(8), dimension(:), intent(out) :: x_novo
      real(8) :: f_c, f_c_linha
      real(8), intent(in) :: c !Nosso chute único
      real(8) :: c_local

      c_local = c

      do i = 1, interacoes

      f_c = c_local**3 - 3*c_local**2 - c_local + 3
      f_c_linha = 3*c_local**2 - 6*c_local - 1
                  
      x_novo(i) = c_local - (f_c/f_c_linha) 

      c_local = x_novo(i)

      end do

    end subroutine

    subroutine metodo_secantes(interacoes, c_1, c_2, x_novo)

      implicit none
      integer, intent(in) :: interacoes
      integer :: i
      real(8), intent(in) :: c_1, c_2 !Nossos 2 chutes
      real(8) :: c_1_local, c_2_local !variáveis para serem usadas somente nessa subrotina
      real(8), dimension(:), intent(out) :: x_novo
      real(8) :: f_c_1, f_c_2

      c_1_local = c_1
      c_2_local = c_2

      do i = 1, interacoes
      f_c_1 = c_1_local**3 - 3*c_1_local**2 - c_1_local + 3
      f_c_2 = c_2_local**3 - 3*c_2_local**2 - c_2_local + 3

      x_novo(i) = c_2_local - f_c_2*((c_2_local - c_1_local)/(f_c_2 - f_c_1))

      c_1_local = c_2_local
      c_2_local = x_novo(i)

      end do

    end subroutine
end program exer3
