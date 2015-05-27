#!/bin/bash
for j in circuits/*.hide.csg
do
	rm $j
done

for j in circuits/*.csg
do
	./StateFromCircuit/petrify -hide .internal $j | ./StateFromCircuit/write_sg | tail -n+3 >  ${j::-4}.hide.csg;
done

for i in circuits/*.csg
do
	./StateFromCircuit/genet -k 3 $i -o ${i::-4}.genet
	./StateFromCircuit/genet2async  ${i::-4}.genet $i > ${i::-4}.pn
	./StateFromCircuit/draw_astg -ip -nonames ${i::-4}.pn -o ${i::-4}.ps
	ps2pdf ${i::-4}.ps ${i::-4}.pdf
done




 
