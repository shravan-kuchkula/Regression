data forestfires;
	infile "Z:\Desktop\forestfires.csv" dlm=',' firstobs=2;
	input X Y month $ day $ FFMC DMC DC ISI temp RH wind rain area;
run;

* Step 1: Show the distribution of area burned;

title 'Burned area (in hectares)';
proc univariate data=forestfires;
histogram area;
run;
title;

* Step 2: Show the distribution of log(area + 1);

data forestfires1;
	set forestfires;
	area = area + 1;
	logArea = log(area);
run;

title 'log(area + 1)';
proc univariate data=forestfires1;
histogram logArea;
run;
title;

* Step3: Show the distribution of log(area + 1) > 0;

data filterforestfires;
	set forestfires1;
	if logArea ^= 0;
run;

title 'log(area + 1) > 0';
proc univariate data=filterforestfires;
histogram logArea;
run;
title;

* Step 4a: Box plot for spatial predictors;

proc sort data=filterforestfires out=Xfilterforestfires;
by X;

*ods graphics off;
title 'logArea vs X';
proc boxplot data=Xfilterforestfires;
plot logArea*X;
inset mean stddev / header = 'Overall Statistics' pos = tm;
insetgroup mean stddev / header = 'By group';
run;
title;

proc sort data=filterforestfires out=Yfilterforestfires;
by Y;

*ods graphics off;
title 'logArea vs Y';
proc boxplot data=Yfilterforestfires;
plot logArea*Y;
inset mean stddev / header = 'Overall Statistics' pos = tm;
insetgroup mean stddev / header = 'By group';
run;
title;

* Step 4b: Box plot for temporal predictors;

proc sort data=filterforestfires out=Mfilterforestfires;
by month;

*ods graphics off;
title 'logArea vs month';
proc boxplot data=Mfilterforestfires;
plot logArea*month;
inset mean stddev / header = 'Overall Statistics' pos = tm;
insetgroup mean stddev / header = 'By group';
run;
title;

proc sort data=filterforestfires out=Dfilterforestfires;
by day;

*ods graphics off;
title 'logArea vs day';
proc boxplot data=Dfilterforestfires;
plot logArea*day;
inset mean stddev / header = 'Overall Statistics' pos = tm;
insetgroup mean stddev / header = 'By group';
run;
title;

* Step 5: Recode the month variable into season;

data ff;
	set filterforestfires;
	if month in ("dec", "jan", "feb") then season = "winter";
	else if month in ("sep", "oct", "nov") then season = "fall";
	else if month in ("jun", "jul", "aug") then season = "summer";
	else season = "spring";
run;
