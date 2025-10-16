set terminal pdfcairo font 'Montserrat, 12'
set output 'grafA.pdf'
set title 'Gráfico da trajetória da bola' font 'Montserrat Bold, 17'
set ylabel 'y (m)' font 'Montserrat Bold, 12'
set xlabel 'x (m)' font 'Montserrat Bold, 12'
set grid
set xrange [0:41]
set xtics 0, 5, 41
set bmargin 4
set lmargin 10
set rmargin 5
plot 'chute_out.dat' using 1:2 with lines lw 2 lc 'green' title '{/Symbol q}0 = 1.45 e {/Symbol f}0 = 0.15', \
'chute_out_2.dat' using 1:2 with lines lw 2 lc 'red' title '{/Symbol q}0 = 1.45 e {/Symbol f}0 = -0.1', \
'chute_out_3.dat' using 1:2 with lines lw 2 lc 'blue' title '{/Symbol q}0 = 1.52 e {/Symbol f}0 = 0.125'
