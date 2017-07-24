
 
proc import datafile = '' 
dbms = xls out = CaseStudy_test replace; run;


    /*** d. Check the distribution of the dependent variable and get a bundle of candidate models. ***/
	proc univariate data = CaseStudy_test;
	var Prob_Default;
	histogram Prob_Default/normal;
	run;

   /**** e. Check the relationship between dependent variables and independent variables. If not linear, use some techniques.***/
   /****** Categorical variable****/
	proc tabulate data = CaseStudy_test order = freq; 
    class occupancy;
	var Prob_Default;
	tables ALL occupancy, N mean*Prob_Default;
    run;

	/***** Continuous variable ***/
	/***Way 1: buckets****/
proc format;
value propval 0 - 100000 = '0 - 100K'
			  100000 <- 200000 = '100K - 200K'
			  200000 <- 500000 = '200K - 500K'
			  500000 <- 1000000 = '500K - 1M'
		      1000000 <- high = '> 1M';
run;	

proc tabulate data = CaseStudy_test;
class orig_prop_value;
var Prob_Default;
format orig_prop_value propval.;
tables ALL orig_prop_value, N mean*(Prob_Default*f = 7.4);
run;
    
	/***Way 2: nonparametric***/
	proc sgplot data = CaseStudy_test;
	scatter y = Prob_Default x = orig_prop_value;
	where orig_prop_value < 20000000;
	run;
    
	ods graphics on;
	proc loess data = CaseStudy_test plots=residuals(smooth) PLOTS(MAXPOINTS= 10000);
	model Prob_Default = orig_prop_value; 
    where orig_prop_value < 2000000;
    run;
	ods graphics off;

	/**** f. Split the whole data into a few parts: training, in sample testing and out of time testing. ******/
	data training oot;
	set CaseStudy_test;
	if orig_prop_value < 2000000;

	if loan_origination < '01Jan2008'd then output training;
	else output oot;
	run;


    /**** g. Start the training: Variable selection & Model selection. [Mean error, MSE, R^2, Error Distribution; KS if logistic] ***/
	proc genmod data = training;
	class occupancy prpy_ste_prvc_cde;
	model Prob_Default = occupancy prpy_ste_prvc_cde/dist = Normal link = identity;
		ods output ParameterEstimates = est;
    output out = Resid pred = pred;
    run;
	
	/******Questions: how can we get the estimate for testing data?******/
	/*******Way 1: store + plm***/
	proc genmod data = training;
	class occupancy prpy_ste_prvc_cde;
	model Prob_Default = occupancy prpy_ste_prvc_cde/dist = Normal;
		ods output ParameterEstimates = est;
    output out = Resid pred = pred;
	store simple;
    run;

	proc plm source= simple;
    score data=oot out=oot_score;
    run;

	/******Way 2: Set w = 1/0 ***/
	data whole;
	set training(in = train) oot(in = oot);
	if train then w = 1;
	if oot then w = 0;
	run;
	
	proc freq data = whole; tables w; run;
	proc genmod data = whole;
	class occupancy prpy_ste_prvc_cde;
	weight w;
	model Prob_Default = occupancy prpy_ste_prvc_cde/dist = Normal;
		ods output ParameterEstimates = est;
    output out = Resid pred = pred;
    run;

    /******* i. Monitoring and Tracking [Error, MSE, R^2, Error Distribution. PSI.]. Update the model if needed. ****/
	/*** Mean Error, MSE and Error Distribution and straightforward, PSI is showed as follows ****/
	%include 'C:\Users\rqchen\Desktop\Training for Jobhunting\programs\PSI_macro.sas';
	%PSI(training, Prob_Default, oot); /****Note: let us go through the program to see the idea of PSI*********/

	population stability index
	
	/********For 0-1 model, KS test is the key measurement************/
	proc npar1way data=oot_score  D;
	class YYY;		/****At least one class variable must be specified ***/
	var predicted;
	run;




 

     
	
