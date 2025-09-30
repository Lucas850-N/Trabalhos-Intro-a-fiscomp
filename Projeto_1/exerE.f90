program exerE
  implicit none
  integer :: n, i, j
  real, dimension(:,:), allocatable  :: M
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
        

end program exerE
