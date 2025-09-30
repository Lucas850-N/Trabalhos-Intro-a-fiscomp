program exerE
  implicit none
  integer :: n, i, j
  real, dimension(:,:), allocatable  :: M
<<<<<<< HEAD
  real :: epsilon 
  print *, "Por favor, escolha a precisão"
  read *, epsilon
  print *, "Escolha a dimensão da matriz"
  read *, n
  allocate(M(n,n))
  do i = 1, n
   print *, "Escolha os termos da linha", i
   read *, M(i,:)
   end do
  do i = 1, n
   print *, M(i,:)
  end do
        
=======
  real, dimension(:), allocatable :: x, x_k
  real :: epsilon, lambda, lambda_k, delta_lambda, x_norma, lambda_anterior
  call determinacao_matriz(n, i, j, epsilon, M)
  call metodo_de_potencias(M, x, x_k, lambda, lambda_k, delta_lambda, n, epsilon, x_norma, lambda_anterior)
  deallocate(M)
  deallocate(x)
  deallocate(x_k)

  contains
    subroutine determinacao_matriz(n, i, j, epsilon, M)

      !Declaração de variáveis
      integer :: n, i, j
      real :: epsilon
      real, dimension(:,:), allocatable :: M

      !Parte em que o usuário vai inserir os termos da matriz M
      print *, "insira a precisão"
      read *, epsilon
      print *, "insira a dimensão"
      read *, n
      allocate(M(n,n))
      
      !!Loop para que o conteúdo seja inserido linha por linha, como pedido
      do i = 1, n
        print *, "insira os termos de ", i
        read *, M(i,:)
      end do

    end subroutine

    subroutine metodo_de_potencias(M, x, x_k, lambda, lambda_k, delta_lambda, n, epsilon, x_norma, lambda_anterior)

      !Declaração de variáveis
      real, dimension(n, n) :: M
      real, dimension(:), allocatable :: x, x_k
      real :: lambda, lambda_k, delta_lambda, epsilon, x_norma, lambda_anterior
      integer :: n

      !Alocação de n para os vetores x e x_k
      allocate(x(n))
      allocate(x_k(n))
      x = 1.0
      lambda = 0.0
      lambda_k = 1.0
      delta_lambda = lambda_k - lambda

      !Loop para a implementação do método de potências
      do
        if (delta_lambda > epsilon) then
          lambda_anterior = lambda_k
          x_k = matmul(M,x)
          lambda_k = (dot_product(x,x_k))/(dot_product(x,x))
          x_norma = sqrt(dot_product(x_k, x_k))
          x = x_k/x_norma
          delta_lambda = lambda_k - lambda_anterior
        else
          exit
        end if
      end do

      !Print do maior autovalor e do autovetor correspondente
      print *, lambda_k
      do i = 1, n 
        print *, x(i)
      end do
      end subroutine
      
      
>>>>>>> 6a18ec5eccb0e47f9d4201aac8eeb4484db7df61

end program exerE
