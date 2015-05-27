#!/bin/bash

for i in circuits/*.new
do
	./petrify/bin/petrify $i.csg | ./petrify/bin/draw_astg -ip -o $i.ps;
	./petrify/bin/petrify $i.csg -hide .internal | ./petrify/bin/draw_astg -ip -o $i.hide.ps;
	ps2pdf $i.hide.ps $i.hide.pdf;
done


