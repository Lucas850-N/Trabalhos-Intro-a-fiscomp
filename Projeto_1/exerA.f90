program exerA
  implicit none
  Integer :: loops_a, loops_b, loops_c
  Real(Kind=4) :: a = 1.0e0, resultado_a
  Real(Kind=8) :: b = 1.0d0, resultado_b
  Real(Kind=16) :: c = 1.0_16, resultado_c
  loops_a = 0
  loops_b = 0
  loops_c = 0
  resultado_a = 0.0
  resultado_b = 0.0
  resultado_c = 0.0
  print *, "Para Precisão simples:"
  do     !Esse "do" começa o loop para a obtenção da precisão
  resultado_a = 1.0e0 + a
  loops_a = loops_a + 1  !Esse comando vai oferecer quantas vezes o loop foi feito
  if (resultado_a == 1) then
    loops_a = loops_a - 1  !É necessário subtrair 1 dos loops para que ele não conte o loop de erro também
    print *, resultado_a
    print *, "O número de loops foi: ", loops_a
    exit
  else
    a = a/2
    print *, resultado_a  

  end if
  end do
print *, "----------------------------------------------------------------------------------------------------------"
print *, "Para Precisão dupla: "
  do     !Esse "do" começa o loop para a obtenção da precisão
  resultado_b = 1.0d0 + b 
  loops_b = loops_b + 1  !Esse comando vai oferecer quantas vezes o loop foi feito
  if (resultado_b == 1) then
    loops_b = loops_b - 1
    print *, resultado_b
    print *, "O número de loops foi: ", loops_b
    exit
  else
    b = b/2
    print *, resultado_b
  end if
  end do
print *, "----------------------------------------------------------------------------------------------------------"
print *, "Para Precisão quádrupla:"
 do     !Esse "do" começa o loop para a obtenção da precisão
  resultado_c = 1.0_16 + c
  loops_c = loops_c + 1  !Esse comando vai oferecer quantas vezes o loop foi feito
  if (resultado_c == 1) then
    loops_c = loops_c - 1
    print *, resultado_c
    print *, "O número de loops foi: ", loops_c
    exit
  else
    c = c/2
    print *, resultado_c  

  end if
  end do

end program exerA 
