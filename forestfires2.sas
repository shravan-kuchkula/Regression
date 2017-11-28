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

* Draw a boxplot of logArea vs season;

proc sort data=ff out=sortedff;
by season;

*ods graphics off;
title 'logArea vs season';
proc boxplot data=sortedff;
plot logArea*season;
inset mean stddev / header = 'Overall Statistics' pos = tm;
insetgroup n mean stddev / header = 'By group';
run;
title;

* Step 6: Run the regression model;
* proc reg requires to code the categorical variables.
* proc glm needs class to be specified;

proc print data=ff; run;

* Using proc glm;
proc glm data=ff;
	class season day;
	model logArea = season day FFMC DMC DC ISI temp rain wind RH;
run;

* To use proc reg, we need to code season and day variables;

data ff1;
	set ff;
	DumWinter = (season='winter');
	DumFall = (season='fall');
	DumSummer = (season='summer');
	DumSpring = (season='spring');
	DumMon = (day='mon');
	DumTue = (day='tue');
	DumWed = (day='wed');
	DumThu = (day='thu');
	DumFri = (day='fri');
	DumSat = (day='sat');
	DumSun = (day='sun');
run;

proc print data=ff1; run;

* run proc reg;

ods graphics on;
proc reg data = ff1 plots(unpack label);
model logArea =  DumWinter DumFall DumSummer DumSpring DumMon DumTue DumWed DumThu DumFri DumSat DumSun FFMC DMC DC ISI temp RH wind rain / VIF;
run;
ods graphics off;

* Step 7: Add interaction terms and squared terms;
* Run proc reg with interaction and squared terms;
* Run proc glm with interaction and squared terms;

data ff2;
	set ff1;
	FFMC_DMC = FFMC*DMC;
	FFMC_DC = FFMC*DC;
	FFMC_ISI = FFMC*ISI;
	DMC_DC = DMC*DC;
	DMC_ISI = DMC*ISI;
	DC_ISI = DC*ISI;
	RH2 = RH**2;
	wind2 = wind**2;
run;


ods graphics on;
proc reg data = ff2 plots(unpack label);
model logArea =  DumWinter DumFall DumSummer DumSpring DumMon DumTue DumWed DumThu DumFri DumSat DumSun FFMC DMC DC ISI temp RH wind rain RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI/ VIF;
run;
ods graphics off;


ods graphics on;
proc reg data = ff2 plots(unpack label);
model logArea =  DumWinter DumFall DumSummer DumSpring DumMon DumTue DumWed DumThu DumFri DumSat DumSun FFMC DMC DC ISI temp RH wind rain RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI/ selection=stepwise VIF;
run;
ods graphics off;


* Create another data set for glmselect and glm with categorical dummy variables;
data ff3;
	set ff;
	FFMC_DMC = FFMC*DMC;
	FFMC_DC = FFMC*DC;
	FFMC_ISI = FFMC*ISI;
	DMC_DC = DMC*DC;
	DMC_ISI = DMC*ISI;
	DC_ISI = DC*ISI;
	RH2 = RH**2;
	wind2 = wind**2;
run;


proc glm data = ff3;
class season day;
model logArea =  season day FFMC DMC DC ISI temp RH wind rain RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI;
run;

* Run proc glmselect with forward selection;

ods graphics on;
proc glmselect data=ff3 plots=all;
class season day;
model logArea =  season day FFMC DMC DC ISI temp RH wind rain RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI / selection=stepwise details=all stats=all  ;
run;
quit;
ods graphics off;


ods graphics on;
proc glmselect data=ff3
               seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
class season day;
model logArea = season day FFMC DMC DC ISI temp RH wind rain RH2 wind2
					FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI / selection=LASSO(choose=AIC stop=CV) CVdetails  ;
run;
quit;
ods graphics off;
