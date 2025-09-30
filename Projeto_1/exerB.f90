program exerB
  implicit none
  integer :: n
  Real(4) :: ln_simples, serie_de_taylor_simples, termo_de_taylor_simples, serie_de_taylor_nova_simples, epsilon_simples
  Real(8) :: ln_dupla, serie_de_taylor_dupla, termo_de_taylor_dupla, serie_de_taylor_nova_dupla, epsilon_dupla
  Real(4) :: a = 0.1e0, precisao_a
  Real(4) :: b = 0.2e0, precisao_b
  Real(4) :: c = 0.3e0, precisao_c  
  Real(4) :: d = 0.4e0, precisao_d
  Real(8) :: e = 0.1d0, precisao_e
  Real(8) :: f = 0.2d0, precisao_f
  Real(8) :: g = 0.3d0, precisao_g  
  Real(8) :: h = 0.4d0, precisao_h
  print *, "Para Precisão Simples"
  print '(A8, 7X, A9)', "Epsilon", "Precisão"
  call calculo_serie_de_taylor_simples(a, n, ln_simples, serie_de_taylor_simples, precisao_a, termo_de_taylor_simples,&
    &serie_de_taylor_nova_simples, epsilon_simples)
  call calculo_serie_de_taylor_simples(b, n, ln_simples, serie_de_taylor_simples, precisao_b, termo_de_taylor_simples,&
    &serie_de_taylor_nova_simples, epsilon_simples)
  call calculo_serie_de_taylor_simples(c, n, ln_simples, serie_de_taylor_simples, precisao_c, termo_de_taylor_simples,&
    &serie_de_taylor_nova_simples, epsilon_simples)
  call calculo_serie_de_taylor_simples(d, n, ln_simples, serie_de_taylor_simples, precisao_d, termo_de_taylor_simples,&
    &serie_de_taylor_nova_simples, epsilon_simples)
  print *, "----------------------------------------------------------------------------------------------------------------------"
  print *, "Para Precisão Dupla"
  print '(A8, 7X, A9)', "Epsilon", "Precisão"
  call calculo_serie_de_taylor_dupla(e, n, ln_dupla, serie_de_taylor_dupla, precisao_e, termo_de_taylor_dupla,&
    &serie_de_taylor_nova_dupla, epsilon_dupla)
   call calculo_serie_de_taylor_dupla(f, n, ln_dupla, serie_de_taylor_dupla, precisao_f, termo_de_taylor_dupla,&
    &serie_de_taylor_nova_dupla, epsilon_dupla)
  call calculo_serie_de_taylor_dupla(g, n, ln_dupla, serie_de_taylor_dupla, precisao_g, termo_de_taylor_dupla,&
    &serie_de_taylor_nova_dupla, epsilon_dupla)
  call calculo_serie_de_taylor_dupla(h, n, ln_dupla, serie_de_taylor_dupla, precisao_h, termo_de_taylor_dupla,&
    &serie_de_taylor_nova_dupla, epsilon_dupla)

  contains

    subroutine calculo_serie_de_taylor_simples(x, n, ln, serie_de_taylor, precisao_x, termo_de_taylor, serie_de_taylor_nova&
     & ,epsilon )
      Real(4) :: x, ln, termo_de_taylor, precisao_x, serie_de_taylor, serie_de_taylor_nova, epsilon
      integer :: n
      n = 1
      serie_de_taylor = 0
      serie_de_taylor_nova = 0
      ln = log(1+x)
      do  !Esse loop será responsável por calcular a série de Taylor
        termo_de_taylor = (((-1)**(n+1))*(x**n))/n
        serie_de_taylor = serie_de_taylor_nova
        serie_de_taylor_nova = serie_de_taylor + termo_de_taylor
        if (serie_de_taylor_nova /= serie_de_taylor) then
          n = n + 1
          epsilon = termo_de_taylor
        else if (serie_de_taylor_nova == serie_de_taylor) then
          exit
        end if 
      end do
      precisao_x = epsilon/ln
      print '(ES12.4, 2X, ES12.4)', x, precisao_x

    end subroutine 
    subroutine calculo_serie_de_taylor_dupla(x, n, ln, serie_de_taylor, precisao_x, termo_de_taylor, serie_de_taylor_nova&
     & ,epsilon )
      Real(8) :: x, ln, termo_de_taylor, precisao_x, serie_de_taylor, serie_de_taylor_nova, epsilon
      integer :: n
      n = 1
      serie_de_taylor = 0
      serie_de_taylor_nova = 0
      ln = log(1+x)
      do  !Esse loop será responsável por calcular a série de Taylor
        termo_de_taylor = (((-1)**(n+1))*(x**n))/n
        serie_de_taylor = serie_de_taylor_nova
        serie_de_taylor_nova = serie_de_taylor + termo_de_taylor
        if (serie_de_taylor_nova /= serie_de_taylor) then
          n = n + 1
          epsilon = termo_de_taylor
        else if (serie_de_taylor_nova == serie_de_taylor) then
          exit
        end if 
      end do 
      precisao_x = epsilon/ln
      print '(ES12.4, 2X, ES12.4)', x, precisao_x
    end subroutine
end program exerB
