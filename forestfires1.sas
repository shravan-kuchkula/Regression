data forestfires;
	infile "Z:\Desktop\forestfires.csv" dlm=',' firstobs=2;
	input X Y month $ day $ FFMC DMC DC ISI temp RH wind rain area;
run;

proc print data=forestfires;
var area;
run;

* Step 1: Add 1 to area and take a log transform;

data forestfires1;
	set forestfires;
	area = area + 1;
	logArea = log(area);
run;

proc print data=forestfires1;
var area logArea;
run;
