data forestfires;
	infile "Z:\Desktop\forestfires.csv" dlm=',' firstobs=2;
	input X Y month $ day $ FFMC DMC DC ISI temp RH wind rain area;
run;

proc print data=forestfires;
run;

* Take a log of area since the data is very close to zero;
* Note: got some warnings that there are missing values, since log(0) is undefined;
* Does it make sense to convert 0's to 1's in the original dataset, so that log(1) will be 0 ? ;
data logforestfires;
	set forestfires;
	logArea = log(area);
run;

proc print data=logforestfires;
run;

* Step 1: Exploratory data analysis;
** a. Generate scatter plot matrices.
** b. Get correlation numbers for each of the predictors

* Scatter plot of logArea vs other predictors;
proc sgscatter data = logforestfires;
matrix logArea FFMC DMC X Y;
run;

proc sgscatter data = logforestfires;
matrix logArea DC ISI temp RH;
run;

proc sgscatter data = logforestfires;
matrix logArea wind rain;
run;

* Approach 1: Removing area = 0 obs.
* Filter observations with area = 0;
* Re-code the month variable;
* Draw box plots for X, Y, Months. ;

* filter observations which have area=0;
data filterforestfires;
	set forestfires;
	if area ^= 0;
run;

proc print data=filterforestfires;
run;

* Take a log of area;
data logfilterforestfires;
	set filterforestfires;
	logArea = log(area);
run;


proc print data=logfilterforestfires; run;

* Draw box plot of log(area) and X, Y, Month;

proc sort data=logfilterforestfires out=Xlogfilterforestfires;
by X;

*ods graphics off;
proc boxplot data=Xlogfilterforestfires;
plot logArea*X;
run;


proc sort data=logfilterforestfires out=Ylogfilterforestfires;
by Y;

*ods graphics off;
proc boxplot data=Ylogfilterforestfires;
plot logArea*Y;
run;

proc sort data=logfilterforestfires out=Mlogfilterforestfires;
by month;

*ods graphics off;
proc boxplot data=Mlogfilterforestfires;
plot logArea*month;
run;

proc sort data=logfilterforestfires out=Dlogfilterforestfires;
by day;

*ods graphics off;
proc boxplot data=Dlogfilterforestfires;
plot logArea*day;
run;


* Recode the month variable;

