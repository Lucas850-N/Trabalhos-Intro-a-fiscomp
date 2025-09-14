program exerB
   
     !Declaração de variáveis      
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
  
    !Implementação da subrotina correspondente para precisão simples
    print *, "Precisão Simples"
    print '(4X ,A1, 5X, A9)', "X", "Precisão"
    call calculo_serie_de_taylor_simples(a, n, ln_simples, serie_de_taylor_simples, precisao_a, termo_de_taylor_simples,&
      &serie_de_taylor_nova_simples, epsilon_simples)
    call calculo_serie_de_taylor_simples(b, n, ln_simples, serie_de_taylor_simples, precisao_b, termo_de_taylor_simples,&
      &serie_de_taylor_nova_simples, epsilon_simples)
    call calculo_serie_de_taylor_simples(c, n, ln_simples, serie_de_taylor_simples, precisao_c, termo_de_taylor_simples,&
      &serie_de_taylor_nova_simples, epsilon_simples)
    call calculo_serie_de_taylor_simples(d, n, ln_simples, serie_de_taylor_simples, precisao_d, termo_de_taylor_simples,&
      &serie_de_taylor_nova_simples, epsilon_simples)
  
    print *, "" !Print vazio somente para fazer separação de linhas
 
    !Implementação da subrotina correspondente para precisão dupla
    print *, "Precisão Dupla"
    print '(4X ,A1, 5X, A9)', "X", "Precisão"
    call calculo_serie_de_taylor_dupla(e, n, ln_dupla, serie_de_taylor_dupla, precisao_e, termo_de_taylor_dupla,&
      &serie_de_taylor_nova_dupla, epsilon_dupla)
     call calculo_serie_de_taylor_dupla(f, n, ln_dupla, serie_de_taylor_dupla, precisao_f, termo_de_taylor_dupla,&
      &serie_de_taylor_nova_dupla, epsilon_dupla)
    call calculo_serie_de_taylor_dupla(g, n, ln_dupla, serie_de_taylor_dupla, precisao_g, termo_de_taylor_dupla,&
      &serie_de_taylor_nova_dupla, epsilon_dupla)
    call calculo_serie_de_taylor_dupla(h, n, ln_dupla, serie_de_taylor_dupla, precisao_h, termo_de_taylor_dupla,&
      &serie_de_taylor_nova_dupla, epsilon_dupla)
  
    print *, ""
  
    Print *, "Para os valores exigidos para x, a função logarítmica pode ser aproximada a partir de uma série como a série de"
    print *, "Taylor, uma vez que os valores obtidos a partir do cálculo da função logarítmica e da série de Taylor apresentam"
    print *, "uma disparidade pequena. Isso pode ser visto a partir da análise dos resultados das precisões em cada caso. Porém,"
    print *, "vale ressaltar que a aproximação de funções logarítmicas pode não ser muito precisa para valores de x que sejam"
    print *, "maiores do que 1, uma vez que nesses casos a aproximação não iria convergir."
  
    contains
  
      !Subrotina responsável por realizar a série de Taylor para precisão simples
      subroutine calculo_serie_de_taylor_simples(x, n, ln, serie_de_taylor, precisao_x, termo_de_taylor, serie_de_taylor_nova&
       & ,epsilon )
  
       !Declaração de variáveis
        Real(4) :: x, ln, termo_de_taylor, precisao_x, serie_de_taylor, serie_de_taylor_nova, epsilon
        integer :: n
  
        !Determinação inicial de valores das variáveis para evitar "lixo" de memória que possa atrapalhar os cálculos
        n = 1
        serie_de_taylor = 0
        serie_de_taylor_nova = 0
        ln = log(1+x)
  
        !Esse loop será responsável por calcular a série de Taylor
        do
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
  
        !Cálculo da precisão assim como foi pedido no exercício 
        precisao_x = epsilon/ln
        print '(F6.1, 2X, ES12.4)', x, precisao_x
  
      end subroutine 
  
      !Subrotina responsável pelo cálculo da série de Taylor para precisão dupla
      subroutine calculo_serie_de_taylor_dupla(x, n, ln, serie_de_taylor, precisao_x, termo_de_taylor, serie_de_taylor_nova&
      & ,epsilon )
  
       !Declaração de variáveis
        Real(8) :: x, ln, termo_de_taylor, precisao_x, serie_de_taylor, serie_de_taylor_nova, epsilon
        integer :: n
  
        !Determinação das variáveis pelo mesmo motivo da outra subrotina
        n = 1
        serie_de_taylor = 0
        serie_de_taylor_nova = 0
        ln = log(1+x)
  
       !Loop da série de Taylor assim como foi feito para a outra subrotina
       do
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
       print '(F6.1, 2X, ES12.4)', x, precisao_x
     end subroutine
end program exerB
