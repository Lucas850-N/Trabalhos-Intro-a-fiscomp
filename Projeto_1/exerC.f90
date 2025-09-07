program exerC
  implicit none
  integer :: i, j, divisores_i
  integer, parameter :: m_1 = 100, m_2 = 1000, m_3 = 10000
  real :: resto
  print *, "Os números primos de 1 à 100 são:"
  do i = 1,m_1
    divisores_i = 0
    do j = 1,m_1
    resto = mod(i,j)
    if (resto == 0) then
      divisores_i = 1 + divisores_i
    end if
    end do
    if (divisores_i == 2) then
       print *, i
    end if
  end do
  print *, "-------------------------------------------------------------------------------------------------"
  print *, "Os números primos de 1 a 1000 são:"
  do i = 1,m_2
    divisores_i = 0
    do j = 1,m_2
    resto = mod(i,j)
    if (resto == 0) then
      divisores_i = 1 + divisores_i
    end if
    end do
    if (divisores_i == 2) then
       print *, i
    end if
  end do
  print *, "-------------------------------------------------------------------------------------------------"
  print *, "Os números primos de 1 a 10000 são:"
  do i = 1,m_3
    divisores_i = 0
    do j = 1,m_3                    
    resto = mod(i,j)
    if (resto == 0) then
      divisores_i = 1 + divisores_i
    end if
    end do
    if (divisores_i == 2) then
       print *, i
    end if
  end do       
end program exerC


