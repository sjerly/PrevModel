/*Preface: Assigning Libraries and Establishing Useful Functions*/

/****Update the Following***/
libname document "A:\DATABASE\eHars\SAS_Datasets\document_dataset";
libname person "A:\DATABASE\eHars\SAS_Datasets\person_dataset";
%let state_cd=WA;

/********************No Updates Needed Below******************************/
options nofmterr;
options cmplib=work.function;
proc fcmp outlib=work.function.eharsdate;
   function eharsdate(var_in $);
		if substr(var_in,1,1)="." or var_in="" then var_out=.;
		else if substr(var_in,5,1)="." and substr(var_in,7,1)="." then var_out=input("06/15/"||strip(substr(var_in,1,4)),mmddyy10.);
		else if substr(var_in,5,1)="." then var_out=input("06/"||strip(substr(var_in,7,2))||"/"||strip(substr(var_in,1,4)),mmddyy10.);
		else if substr(var_in,7,1)="." then var_out=input(strip(substr(var_in,5,2)||"/15/"||strip(substr(var_in,1,4))),mmddyy10.);
		else var_out=input(strip(substr(var_in,5,2)||"/"||strip(substr(var_in,7,2))||"/"||strip(substr(var_in,1,4))),mmddyy10.);
   return(var_out);
   endsub;
run;

/*Part 1: Identifying people who are out of care in state*/

	/*Part 1A: List of labs that occured in state during our analysis period. We will eventually subset these
	           to just those that appeared to be OOC 365 days later*/
	proc sql;
		create table Labs as
			select distinct ehars_uid, eharsdate(sample_dt) as sample_dt format mmddyy10., 
				eharsdate(enter_dt) as enter_dt format mmddyy10., lab_test_cd as type,
				input(compress(result,".","kd"),best12.) as result, coalesce(address.state_cd, 
				facility_code.state_cd) as state_cd
			from document.document inner join document.lab
			on document.document_uid=lab.document_uid
					   			   left join document.facility_code
			on lab.facility_uid=facility_code.facility_uid
					 			   left join document.address
			on document.document_uid=address.document_uid
			where lab_test_cd in ("EC-014" "EC-015" "EC-016" "EC-017"  "L-010" "L-011")
				  and document_type_cd not in ('000' '003')
				  and coalesce(address.state_cd, facility_code.state_cd)="&state_cd."
				  and substr(sample_dt,1,4) in ('2014' '2015' '2016' '2017')
			order by ehars_uid, sample_dt;
	quit;

	/*Part 1B: Getting a list of all the subsequent events that would indicate someone is not OOC:
										-A new lab
										-A death report
										-A report indicating the person lives in another state*/
	proc sql;
		create table res as
		/*Death Reports*/
		select ehars_uid, '' as document_uid, min(eharsdate(dod)) as date format mmddyy10., min(eharsdate(enter_dt)) as enter_dt format mmddyy10.,
						"Died" as type, . as result, max(state_cd) as state_cd
		from document.document right join document.death
				on document.document_uid=death.document_uid
		where dod ne ''
			and document_type_cd not in ('000' '003')
			group by document.ehars_uid
		union all
		/*VL in State*/
				select distinct ehars_uid, document.document_uid, eharsdate(sample_dt) as date, eharsdate(enter_dt) as enter_dt, "&state_cd. VL" as type,
				input(compress(result,".","kd"),best12.) as result, coalesce(address.state_cd, facility_code.state_cd) as state_cd
				from document.document inner join document.lab
				on document.document_uid=lab.document_uid
						   left join document.facility_code
				on lab.facility_uid=facility_code.facility_uid
						   left join document.address
				on document.document_uid=address.document_uid
				where lab_test_cd in ("EC-014" "L-010" "L-011")
					  and document_type_cd not in ('000' '003')
					  and coalesce(address.state_cd, facility_code.state_cd)="&state_cd."
					  and input(substr(sample_dt,1,4),?4.0) ge 2010
		union all
		/*CD4 in State*/
				select distinct ehars_uid, document.document_uid, eharsdate(sample_dt) as date, eharsdate(enter_dt) as enter_dt, "&state_cd. CD4" as type,
				input(compress(result,".","kd"),best12.) as result, coalesce(address.state_cd, facility_code.state_cd) as state_cd
				from document.document inner join document.lab
				on document.document_uid=lab.document_uid
						   left join document.facility_code
				on lab.facility_uid=facility_code.facility_uid
						   left join document.address
				on document.document_uid=address.document_uid
				where lab_test_cd in ("EC-015" "EC-016" "EC-017" )
					  and document_type_cd not in ('000' '003')
					  and coalesce(address.state_cd, facility_code.state_cd)="&state_cd."
					  and input(substr(sample_dt,1,4),?4.0) ge 2010
		union all

		/*Residence in Another State*/
				select distinct ehars_uid, document.document_uid, eharsdate(address_dt) as date, eharsdate(enter_dt) as enter_dt, "OOS Report" as type,
								. as result, address.state_cd as state_cd
				from document.document left join document.address
				on document.document_uid=address.document_uid
				where document_type_cd not in ('000' '003')
					  and address.state_cd not in ("&state_cd." '') 
					  and address_dt ne '' and input(substr(address_dt,1,4),?4.0) ge 2010

		union all
		/*Labs in Another State*/
				select distinct ehars_uid, document.document_uid, eharsdate(sample_dt) as date, eharsdate(enter_dt) as enter_dt, "OOS Report" as type,
								. as result, facility_code.state_cd as state_cd
				from document.document left join document.lab
				on document.document_uid=lab.document_uid
						   left join document.facility_code
				on lab.facility_uid=facility_code.facility_uid
				where document_type_cd not in ('000' '003')
					  and facility_code.state_cd not in ("&state_cd." '') 
					  and sample_dt ne '' and input(substr(sample_dt,1,4),?4.0) ge 2010
				order by ehars_uid, date;
	quit;

	/*Part 1C: Subsetting List of Labs to just those that appeared to be OOC immediately after*/
	proc sql;
		create table ic as
		select labs.ehars_uid, labs.sample_dt, res.*
		from labs inner join res
		on labs.ehars_uid=res.ehars_uid
		where res.date-labs.sample_dt>0				/*Event Brought Person Back to Care*/
			and 0<res.date-labs.enter_dt<365.25	; 	/*It was reported in time to keep the person appearing in care*/

		create table ooc as	
		select distinct ehars_uid, sample_dt
		from labs
		except
		select ehars_uid, sample_dt
		from ic;
	quit;


/*Part 2: Identifying what happened to people who went OOC*/

	/*Part 2A: For those who were OOC identifying how long they appeared OOC*/
		proc sql;
			create table ooc2 as
			select ooc.*, res.document_uid as enter_doc_uid, res.enter_dt as enter_dt, enter_dt-sample_dt as timedl, type as enter_type
			from ooc left join res
			on ooc.ehars_uid=res.ehars_uid and ooc.sample_dt<res.enter_dt and ooc.sample_dt<res.date
			order by ooc.ehars_uid, ooc.sample_dt, enter_dt;
		quit;

		proc sort data=ooc2 nodupkey;
		by ehars_uid sample_dt;
		run;


	/*Part 2B: For those who were OOC, what was their ultimate outcome*/
		proc sql;
			create table ooc3 as
			select ooc2.*, type, b.state_cd as next_state_cd, b.result as next_val, b.date as next_date
			from ooc2 left join
				(select *
				from res
				where type in ("Died" "OOS Report" "&state_cd. VL")) as b
			on ooc2.ehars_uid=b.ehars_uid and ooc2.sample_dt<b.date
			order by ehars_uid, sample_dt, date;
		quit;

		proc sort data=ooc3 nodupkey;
		by ehars_uid sample_dt;
		run;

	/*Part 2C: Identifying the value of a person's last viral load before going OOC*/
	proc sql;
		create table ooc4 as
		select ooc3.*, b.result as prior_vl_val, b.date as prior_vl_date
		from ooc3 left join
			(select *
			from res
			where type="&state_cd. VL") as b
		on ooc3.ehars_uid=b.ehars_uid and b.date le ooc3.sample_dt
		order by ehars_uid, sample_dt, prior_vl_date;
	quit;

	proc sort data=ooc4 nodupkey;
	by ehars_uid sample_dt;
	run;

/*Part 3: Calculating Probability weights for people who are OOC based on their time
			OOC and the result of their final viral load test*/

	/*Part 3A: What is the probability that a person is still in the state?*/
		data ooc5;
		set ooc4;
		if .<prior_vl_val<200 then presupp=1;
		else if prior_vl_val ge 200 then presupp=0;
		if timedl>365.25*5 then timedl=365.25*5;
		if type in ("OOS Report" "Died") then instate=2;
		else instate=1;
		run;

		proc lifetest data=ooc5 plots=none outcif=palive noprint;
		   time timedl*instate(0)/eventcode=2;
		   strata presupp;
		run;

		proc sort data=palive;
		by stratum descending timedl ;
		run;

		data palive2;
		set palive;
		by stratum;
		retain cumevent 0;
		if first.stratum then cumevent=0;
		cumevent=cumevent+event;
		cuminc=cumevent/atrisk;
		cl_low=round((1-betainv(.975,(atrisk-cumevent+1),cumevent)),.0001); 
		cl_high=round((1-betainv(.025,(atrisk-cumevent),cumevent+1)),.0001); 
		run;

		Title1 "Probability of a person being reported to be in another state or dead";
		proc sgplot data=palive2;
		series x=timedl y=cuminc/group=presupp;
		band x=timedl lower=cl_low upper=cl_high / group=presupp transparency=0.5;
		where timedl>365 and cuminc>0;
			label timedl="Days Since Last Lab" cuminc="Probability of Being Reported Dead or Living Elsewhere"
				  presupp="Viral Suppression at Last Lab";
		run;

	/*Part 3B: For those in the state what is the probability that they will be viremic 
				when they return?*/
		data ooc5a;
		set ooc4;
		where type not in  ('OOS Report' 'Died');
		if .<prior_vl_val<200 then presupp=1;
		else if prior_vl_val ge 200 then presupp=0;
		if timedl>365.25*5 then timedl=365.25*5;
		if type="&state_cd. VL" and .<next_val<200 then supp=2;
		else supp=1;
		run;

		proc lifetest data=ooc5a plots=none outcif=pvir noprint;
		   time timedl*supp(0)/eventcode=1;
		   strata presupp;
		run;

		proc sort data=pvir;
		by stratum descending timedl ;
		run;

		data pvir2;
		set pvir;
		by stratum;
		retain cumevent 0;
		if first.stratum then cumevent=0;
		cumevent=cumevent+event;
		cuminc=cumevent/atrisk;
		cl_low=round((1-betainv(.975,(atrisk-cumevent+1),cumevent)),.0001); 
		cl_high=round((1-betainv(.025,(atrisk-cumevent),cumevent+1)),.0001); 
		run;

		Title1 "Probability of a person bring viremic when they get return and get a viral load";
		proc sgplot data=pvir2;
		series x=timedl y=cuminc/group=presupp;
		band x=timedl lower=cl_low upper=cl_high / group=presupp transparency=0.5;
		where timedl>365 and cuminc>0;
		label timedl="Days Since Last Lab" cuminc="Probability of Being Viremic Upon Return"
			  presupp="Viral Suppression at Last Lab";
		run;

	
/*Part 4: Applying Cumulative Incidence Estimates to Current Data (12/31/2022)*/

	/*Part 4A: Identifying Prevalent Cases and most recent VL date/value*/
		/*Prevalence Definition: Anyone who has had a CD4 or VL in the state in the past 10 years and their most recent
		 viral load was in the state*/

		/*All people who got labs in the P5Y and haven't died*/
		proc sql;
			create table prev1 as
			select ehars_uid, max(date) as sample_dt format mmddyy10.
			from res
			where type in ("&state_cd. CD4" "&state_cd. VL") and
				0<input("12/31/2022",mmddyy10.)-date<365.25*10 and
				ehars_uid not in
					(select ehars_uid
					 from res
					 where type="Died" and year(date)<2023)
			group by ehars_uid;

		/*Excluding those who have since moved OOS*/
			create table prev2 as
			select distinct prev1.ehars_uid, min(input("12/31/2022",mmddyy10.)-prev1.sample_dt, 1826) as timedl, a.ehars_uid as ind
			from prev1 left join
				(select ehars_uid, date
				 from res
				 where type="OOS Report" and year(date)<2023) as a
			on prev1.ehars_uid=a.ehars_uid and prev1.sample_dt<a.date
			having ind=''
			order by prev1.ehars_uid, prev1.sample_dt desc;

		/*Getting Most Recent VL Value*/
			create table prev3 as
			select prev2.ehars_uid, timedl, date as vl_date, result as vl_result, case  
				when result ge 200 then 0
				when .<result<200 then 1
				else 0 end as presupp		/*People who never got a VL are not suppressed*/
			from prev2 left join
				(select ehars_uid, date, result
			 	from res
			 	where type="&state_cd. VL" and year(date)<2023) as b
			on prev2.ehars_uid=b.ehars_uid
			order by ehars_uid, date desc;

		proc sort data=prev3 nodupkey;
		by ehars_uid;
		run;

	/*Part 4B: Applying Cumulative Incidence Estimates to Prevalence Estimates*/
		/*Retaining CI estimates for times that were not represented in initial dataset*/
		proc sort data=palive2;
		by presupp timedl;
		run;

		proc sort data=pvir2;
		by presupp timedl;
		run;

		data frame;
		do presupp=0 to 1;
			do timedl=1 to 1826;
				output;
			end;
		end;
		run;

		data ptot;
		retain cuminc2a cuminc2v;
		merge frame palive2 (rename=(cuminc=cuminca)) pvir2 (rename=(cuminc=cumincv));
		by presupp timedl;
		if first.presupp then do; cuminca=.; cumincv=.; end;
		if cuminca ne . then cuminc2a=cuminca;
		if cumincv ne . then cuminc2v=cumincv;

		drop cuminca cumincv;
		rename cuminc2a=cuminca cuminc2v=cumincv;
		keep presupp timedl cuminc2a cuminc2v;
		run;

		/*Merging CI Estimates onto prevalence data*/
		proc sql;
			create table prev4 as
			select prev3.*, cuminca, cumincv, 1 as case,
				  case when prev3.timedl<365 and prev3.presupp=0 then "In Care: Viremic"
				       when prev3.timedl<365 and prev3.presupp=1 then "In Care: Suppressed"
					   else "Out of Care: Viremic" end as OOC_Ind,
				  case when prev3.timedl<365 then .
					   else (1-cuminca)*(1-cumincv) end as p_nsupp,
				  case when prev3.timedl<365 then .
					   else (1-cuminca)*cumincv end as p_supp

			from prev3 left join ptot
			on prev3.timedl=ptot.timedl and prev3.presupp=ptot.presupp;
			quit;

	proc freq data=prev4 noprint;
	table OOC_Ind/out=popvals;
	run;

	title1;
	ods output statistics=nsuppest;
	proc surveymeans data=prev4 SUM CLSUM plots=none;
		var case;
		weight p_nsupp; 
		run;

	ods output statistics=suppest;
	proc surveymeans data=prev4 SUM CLSUM plots=none;
		var case;
		weight p_supp; 
		run;

Title1 "Time Since Last Lab of Population Currently OOC";
proc sgplot data=prev4;
histogram timedl/group=presupp transperancy=.2;
label timedl="Days Since Last Lab" 
presupp="Viral Suppression at Last Lab";
run;
Title1;

  data report;
  merge popvals nsuppest (rename=(sum=nvsum lowerclsum=nvlow upperclsum=nvhigh))
		suppest (rename=(sum=vsum lowerclsum=vlow upperclsum=vhigh));
length Population vsum2 nvsum2 Estimated $40.;
retain vsum2 nvsum2;
if vsum ne . then vsum2=strip(put(round(vsum,1)||" ("||strip(put(round(vlow,1),$8.0))||"-"||strip(put(round(vhigh,1),$8.0))||")",$40.)); 
if nvsum ne . then  nvsum2=strip(put(round(nvsum,1)||" ("||strip(put(round(nvlow,1),$8.0))||"-"||strip(put(round(nvhigh,1),$8.0))||")",$40.)); 
if OOC_Ind="In Care: Suppressed" then do;
		Population="In Care: Suppressed";
		Observed=Count; Estimated=put(Count,8.0);
		output; end;
if OOC_Ind="In Care: Viremic" then do;
		Population="In Care: Viremic";
		Observed=Count; Estimated=put(Count,8.0);
		output; end;
if OOC_Ind="Out of Care: Viremic" then do;
		Population="Out of Care: Viremic";
		Observed=Count; Estimated=nvsum2;
		output;
		Population="Out of Care: Suppressed";
		Observed=0; Estimated=vsum2;
		output;	end;
keep Population Observed Estimated;
run;

proc print noobs;
var Population Observed Estimated;
run;
