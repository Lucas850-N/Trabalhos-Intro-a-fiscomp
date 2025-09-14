  program exerA
   
     !Declaração de variáveis
     implicit none
     Integer :: loops_a, loops_b, loops_c
     Real(Kind=4) :: a = 1.0e0, resultado_a, a_final
     Real(Kind=8) :: b = 1.0d0, resultado_b, b_final
     Real(Kind=16) :: c = 1.0_16, resultado_c, c_final
   
    !Determinação de valores para as variáveis para que não tenha "lixo" na memória que afete os cálculos
    loops_a = 0
    loops_b = 0
    loops_c = 0
    resultado_a = 0.0
    resultado_b = 0.0
    resultado_c = 0.0
  
    !Cálculo da precisão para precisão simples
    print *, "Precisão simples:"
    do     !Esse "do" começa o loop para a obtenção da precisão
    resultado_a = 1.0e0 + a
    loops_a = loops_a + 1  !Esse comando vai oferecer quantas vezes o loop foi feito
    if (resultado_a /= 1.0) then
      print *, a, resultado_a
      a = a/2.0
    elseif (resultado_a == 1.0) then
      loops_a = loops_a - 1  !É necessário subtrair 1 dos loops para que ele não conte o loop de erro também
      a_final = a
      print *, a_final, resultado_a
     exit    
    end if  
    end do
  
    !Cálculo da precisão para precisão dupla
    print *, "Precisão dupla: "
    do     !Esse "do" começa o loop para a obtenção da precisão
    resultado_b = 1.0d0 + b 
    loops_b = loops_b + 1  !Esse comando vai oferecer quantas vezes o loop foi feito
      if (resultado_b /= 1) then
        print *, b, resultado_b      
        b = b/2.0
      elseif (resultado_b == 1) then
        loops_b = loops_b - 1
        b_final = b
        print *, b_final, resultado_b
        exit
      end if
      end do
    
      !Cálculo da precisão para precisão quádrupla
      print *, "Precisão quádrupla:"
   do     !Esse "do" começa o loop para a obtenção da precisão
    resultado_c = 1.0_16 + c
    loops_c = loops_c + 1  !Esse comando vai oferecer quantas vezes o loop foi feito
    if (resultado_c /= 1) then
      print *, c, resultado_c
      c = c/2.0
    elseif (resultado_c == 1) then
     loops_c = loops_c - 1
      c_final = c
      print *, c_final, resultado_c
      exit
    end if
    end do
  
    !Aqui pedimos para o programa printar os valores dos bits (iterações) e da precisão em cada caso
    print *, loops_a, a_final
    print *, loops_b, b_final
    print *, loops_c, c_final
 
  end program exerA 
