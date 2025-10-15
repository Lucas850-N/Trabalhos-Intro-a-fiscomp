set terminal qt font 'Montserrat, 12'
set title 'Gráfico da angulação em relação ao tempo' font 'Montserrat Bold, 17'
set ylabel 'Ângulo {/Symbol q} (rad)' font 'Montserrat Bold, 12'
set xlabel 'Tempo (s)' font 'Montserrat Bold, 12'
set grid
set xrange [0:10]
set xtics 0, 1, 10
set bmargin 4
set lmargin 10
set rmargin 5
plot 'exer3A_out.dat' using 1:2 with lines lw 2 lc 'black' title 'Euler', \
'exer3B_out.dat' using 1:2 with lines lw 2 lc 'red' title 'Euler-Cromer', \
'solucao_exata_out.dat' using 1:2 with lines lw 2 dashtype 2 title 'Solução exata'
pause -1