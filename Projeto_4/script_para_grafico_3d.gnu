set terminal pdfcairo font 'Montserrat, 12'
set output 'grafC.pdf'
set title 'Gráfico da trajetória da bola' font 'Montserrat Bold, 17'
set ylabel 'y (m)' font 'Montserrat Bold, 12'
set xlabel 'x (m)' font 'Montserrat Bold, 12'
set grid
set xrange [0:41]
set xtics 0, 5, 41
set bmargin 4
set lmargin 10
set rmargin 5
set autoscale z
set object 1 rectangle from 0,0,0 to 40,14,0
set object 1 fillcolor rgb "#10A010" fillstyle solid 0.5 behind
set xrange [0:42]
set yrange [0:14]
set zrange [0:10]
set view 60, 25
set pm3d
unset colorbox
set ticslevel 0
set parametric
set urange [0:42] # Corresponde ao xrange do campo
set vrange [0:14] # Corresponde ao yrange do campo
set style fill solid 1.0
splot u, v, 0 with pm3d fillcolor "#10A010" title "Campo", \
'chute_out_3d.dat' using 1:2:3 with lines lw 2 lc "red" title '{/Symbol q}0 = 1.45 e {/Symbol f}0 = 0.15'
unset parametric

