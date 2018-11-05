libname survival "C:\Users\senor\Documents\Survival_Analysis"; 
run; 


/* if survive equals one then I am censoring it because I only want to look at survive=0 
since I am investigating failures */ 


/* Get summary statistics for failures all together and stratified by reason */ 
proc lifetest data=survival.katrina plots=s(cl cb=ep); *shows confidence bands and limits; 
	time hour*survive(1);
	*strata reason;
run;


/* Log-rank test */ 

proc lifetest data=survival.katrina plots=s(cl) notable;
	time hour*survive(1);
	strata reason / test=peto; *test option tests that all survival curves are equal despite the reason; 
run;


/*Pairwise comparison to see what reasons are different 
flood vs motor, flood vs surge, motor vs jammed, surge vs jammed are all sig.*/ 

proc lifetest data=survival.katrina plots=s(cl) notable;
	time hour*survive(1);
	strata reason  / diff=all; 
run;


/*Hazard function create a dataset one value past time which is 49 hours */ 

data katrina2;
	set survival.katrina;
	if survive=1 then hour=49;
run;

/* Hazard function of all pumps without grouping by reason */ 

proc lifetest data=katrina2 method=life plots=h(cl) width=1;
	time hour*survive(1);
run;

/*Hazard function of all pumps by reason 
Reason4 (trash) dies the quickest starting around hour 25 with high hazard 
Reason1 (flood) has the most variation*/ 

proc lifetest data=katrina2 method=life plots=h(cl) width=1;
	time hour*survive(1);
	strata reason;
run;

/*Cumulative hazard estimates */ 
proc lifetest data=survival.katrina nelson;
	time hour*survive(1);
run;
