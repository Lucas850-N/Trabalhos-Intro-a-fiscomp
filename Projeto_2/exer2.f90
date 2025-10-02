program exer2

  !Declaração de variáveis
  implicit none 
  integer :: dim_N
  real(8), allocatable, dimension(:) :: N

  !Abertura do arquivo tab2_in.dat para substituição de valores
  open(unit=1, file="tab2_in.dat", status="Old", action="Read")
  read(1, *) dim_N
  allocate(N(dim_N))
  read(1, *) N
  close(1)


  contains
    subroutine calculo_preciso_integral
      real(8) :: x, integral_precisa

      integral_precisa = 2*cos(1) - sen(1)

    end subroutine

end program exer2
