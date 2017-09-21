
data forestfires;
	infile "Z:\Desktop\forestfires.csv" dlm=',' firstobs=2;
	input X Y month $ day $ FFMC DMC DC ISI temp RH wind rain area;
run;

proc print data=forestfires;
run;

data logforestfires;
	set forestfires;
	logArea = log(area);
run;

proc print data=logforestfires;
run;


proc sgscatter data = logforestfires;
matrix logArea FFMC DMC X Y;
run;

proc sgscatter data = logforestfires;
matrix logArea DC ISI temp RH;
run;

proc sgscatter data = logforestfires;
matrix logArea wind rain;
run;
