set terminal pdfcairo font 'Montserrat, 12'
set output 'grafB.pdf'
set title 'Gráfico da trajetória da bola com diferentes parâmetros' font 'Montserrat Bold, 17'
set ylabel 'y (m)' font 'Montserrat Bold, 12'
set xlabel 'x (m)' font 'Montserrat Bold, 12'
set grid
set xrange [0:41]
set xtics 0, 5, 41
set bmargin 4
set lmargin 10
set rmargin 5
plot 'chute_out_4.dat' using 1:2 with lines lw 2 lc 'green' title '{/Symbol q}_0 = 1.15, {/Symbol f}_0 = 0.10 e {/Symbol b}_0/m_b = 0.00001', \
'chute_out_5.dat' using 1:2 with lines lw 2 lc 'red' title '{/Symbol q}_0 = 1.15, {/Symbol f}_0 = 0.10 e {/Symbol b}_0/m_b = 0.0005', \
'chute_out_6.dat' using 1:2 with lines lw 2 lc 'blue' title '{/Symbol q}_0 = 1.15, {/Symbol f}_0 = 0.10 e {/Symbol b}_0/m_b = 0.0010'
