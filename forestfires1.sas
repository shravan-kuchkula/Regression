data forestfires;
	infile "Z:\Desktop\forestfires.csv" dlm=',' firstobs=2;
	input X Y month $ day $ FFMC DMC DC ISI temp RH wind rain area;
run;

* Step 1: Add 1 to area and take a log transform;

data forestfires1;
	set forestfires;
	area = area + 1;
	logArea = log(area);
run;

* Step 2: Filter observations which have logArea = 0;

data filterforestfires;
	set forestfires1;
	if logArea ^= 0;
run;

* Step 3: Recode the month variable into season;

data ff;
	set filterforestfires;
	if month in ("dec", "jan", "feb") then season = "winter";
	else if month in ("sep", "oct", "nov") then season = "fall";
	else if month in ("jun", "jul", "aug") then season = "summer";
	else season = "spring";
run;

proc print data=ff; run;

* Step 4: Include RH^2 and Wind^2 predictors;

data ff1;
	set ff;
	RH2 = RH**2;
	wind2 = wind**2;
run;

proc print data =ff1; run;

* Step 5: Include interactions between predictors;
* FFMC*DMC
* FFMC*DC
* FFMC*ISI
* DMC*DC
* DMC*ISI
* DC*ISI
;

data ff2;
	set ff1;
	FFMC_DMC = FFMC*DMC;
	FFMC_DC = FFMC*DC;
	FFMC_ISI = FFMC*ISI;
	DMC_DC = DMC*DC;
	DMC_ISI = DMC*ISI;
	DC_ISI = DC*ISI;
run;

proc print data = ff2; run;

* Step 6: Create dummy variables for categorical variables;

data ff3;
	set ff2;
	DumWinter = (season='winter');
	DumFall = (season='fall');
	DumSummer = (season='summer');
	DumSpring = (season='spring');
run;

proc print data = ff3; run;

* Step 7: Create dummy variables for day categorical variable;

data ff4;
	set ff3;
	DumMon = (day='mon');
	DumTue = (day='tue');
	DumWed = (day='wed');
	DumThu = (day='thu');
	DumFri = (day='fri');
	DumSat = (day='sat');
	DumSun = (day='sun');
run;



* Run the model with interactions and squared terms;


proc reg data = ff4 plots(unpack label);
model logArea =  DumWinter DumFall DumSummer DumSpring DumMon DumTue DumWed DumThu DumFri DumSat DumSun FFMC DMC DC ISI temp RH wind rain RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI/ VIF;
run;


* Try using proc glm;
*proc glm data = ff2;
*model logArea =  FFMC DMC DC ISI temp RH wind rain RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI;
*class season day;
*run;


ods graphics on;
proc glmselect data=ff4 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
class season;
model logArea = FFMC DMC DC ISI temp rain wind RH RH2 wind2 FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI / selection=LASSO(choose=CV stop=AIC) CVdetails  ;
run;
quit;
ods graphics off;

proc print data=ff2; run;

ods graphics on;
proc glmselect data=ff2 plots=none;
	class season;
	model logArea = FFMC DMC DC ISI temp RH wind rain RH2 wind2 season 
					FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI / details=all stats=all;
run;
ods graphics off;

proc glmselect data=ff2 plots=none;
	class season;
	model logArea = FFMC_DMC FFMC DMC /details=all stats=all;
run;


ods graphics on;
proc glmselect data=ff2
               seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
class season;
model logArea = season FFMC DMC DC ISI temp RH wind rain RH2 wind2
					FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI / selection=LASSO(choose=CV stop=AIC) CVdetails  ;
run;
quit;
ods graphics off;


ods graphics on;
proc glmselect data=ff2
               seed=1 plots(stepAxis=number)=(criterionPanel ASEPlot CRITERIONPANEL);
class season day;
model logArea = season day FFMC DMC DC ISI temp RH wind rain RH2 wind2
					FFMC_DMC FFMC_DC FFMC_ISI DMC_DC DMC_ISI DC_ISI / selection=LASSO(choose=AIC stop=CV) CVdetails  ;
run;
quit;
ods graphics off;
