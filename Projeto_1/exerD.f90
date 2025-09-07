!Nesse código, foi feito o cálculo de pi a partir do método de monte Carlo
program exerD
  implicit none
  integer :: contador_q, contador_c, teto = 10000000, i 
  Real(4), dimension(2) :: c_1, c_2, c_3, c_4, n
  Real(4) :: lado_quadrado, raio_circulo, distancia, pi
  !Tomamos nesse exercício um quadrado de lado 1 centrado na origem
  contador_q = 0
  contador_c = 0
  !Para o círculo estar inscrito no quadrado de lado 1, o seu raio precisa ser a metade disso (0,5)
  raio_circulo = 0.5
  !Esse loop cria diversos pontos aleatórios e verifica se eles estão dentro do quadrado e do círculo ao mesmo tempo
   do i = 1, teto
    call random_number(n)
    n(1) = n(1) - 0.5
    n(2) = n(2) - 0.5
    contador_q = contador_q + 1
    distancia = sqrt((n(2)**2)+((n(1))**2))
    if (distancia <= raio_circulo) then
      contador_c = contador_c + 1
    end if 
  end do
  pi = 4*(real(contador_c)/real(contador_q))
  print *, "O valor de pi é:"
  print *, pi
end program exerD

