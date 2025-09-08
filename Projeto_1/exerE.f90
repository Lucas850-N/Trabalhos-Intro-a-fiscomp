program exerE
  implicit none
  integer :: a, b, i, j
  real, dimension(:,:), allocatable  :: M
  real :: epsilon 
  call determinacao_matriz(a, b, i, j, epsilon, Matriz)

  contains
    subroutine determinacao_matriz(a, b, i, j, epsilon, M)  !Essa subrotina será responsável por pedir por usuario digitar os elementos da matriz
      integer :: a, b, i, j
      real :: epsilon
      real, dimension(:,:), allocatable :: M
      print *, "Por favor, escolha a precisão"
      read *, epsilon
      print *, "Escolha as dimensões da matriz"
      read *, a, b
      allocate(Matriz(a,b))
      do i = 1, a
        print *, "Escolha os termos da linha", i
        read *, Matriz(i,:)
      end do
      do i = 1, a
        print *, Matriz(i,:)
      end do

    end subroutine
        

end program exerE
