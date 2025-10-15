set terminal pdfcairo size 7,10 font 'Montserrat, 12'
set output 'graf3.pdf'
set multiplot layout 2,1 title 'Gráficos exer3'
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
set title 'Gráfico da Energia em relação ao tempo' font 'Montserrat Bold, 17'
set ylabel 'Energia (J)' font 'Montserrat Bold, 12'
set xlabel 'Tempo (s)' font 'Montserrat Bold, 12'
set grid
set xrange [0:10]
set xtics 0, 1, 10
set bmargin 4
set lmargin 10
set rmargin 5
plot 'Energia_Euler.dat' using 1:2 with lines lw 2 lc 'black' title 'Euler', \
'Energia_Euler_Cromer.dat' using 1:2 with lines lw 2 lc 'red' title 'Euler-Cromer'