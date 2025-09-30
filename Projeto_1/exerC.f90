program exerc
     implicit none
     integer :: i, j, divisores_i
     integer, parameter :: m_1 = 100, m_2 = 1000, m_3 = 10000
     real :: resto
     open(unit=10, file="primos_out.dat", status="replace", action="write")
     do i = 1,m_1
       divisores_i = 0
      do j = 1,m_1
      resto = mod(i,j)
      if (resto == 0) then
        divisores_i = 1 + divisores_i
      end if
      end do
      if (divisores_i == 2) then
         write(10,*) i
      end if
    end do
    do i = 1,m_2
      divisores_i = 0
      do j = 1,m_2
      resto = mod(i,j)
      if (resto == 0) then
        divisores_i = 1 + divisores_i
      end if
      end do
      if (divisores_i == 2) then
         write(10,*) i
      end if
    end do
    do i = 1,m_3
      divisores_i = 0
      do j = 1,m_3                    
      resto = mod(i,j)
      if (resto == 0) then
        divisores_i = 1 + divisores_i
      end if
      end do
      if (divisores_i == 2) then
         write(10,*) i
      end if
      end do       
 close(10)
 end program exerc 

