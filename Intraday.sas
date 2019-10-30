/* This SAS code intend to calculate the intrady stock returns for S&P 500 constituents */
/* Author: Zhuoran */

%let wrds=wrds.wharton.upenn.edu 4016;
options comamid=TCP remote=WRDS;
signon username= "your_user_name" password="your_password"; *_prompt_;

options nolabel;

rsubmit;
libname home '/home';
endrsubmit;
libname home '/project';


libname rhome slibref=home server=wrds;
libname rcrsp slibref=crsp server=wrds;
libname rcomp slibref=comp server=wrds;
libname ribes slibref=ibes server=wrds;
libname rff slibref=ff server=wrds;
libname rwork slibref=work server=wrds;


rsubmit;
options nomprint source;
endrsubmit;

DM "log; clear; ";

rsubmit;
proc datasets lib=work kill nolist memtype=data;
quit;

Data MVars ( Keep = Name ) ;
  Set SasHelp.VMacro ;
  Where Scope = 'GLOBAL' ;
Run ;

Data _Null_ ;
  Set MVars ;
  Call Symdel( Name ) ;
Run ;
endrsubmit;


/********************************************************************************************
      PART 0: define macros
********************************************************************************************/
%let path = \home;

rsubmit;
%let bdate=01jan1980;        
%let edate=31dec2017;       

%let regbdate=01jan1998;        
%let regedate=31dec2017;        

%let start_yr = %sysfunc(year("&bdate"d));
%let end_yr = %sysfunc(year("&edate"d));

endrsubmit;


/********************************************************************************************
      PART 1: link compustat and crsp
********************************************************************************************/
rsubmit;
proc sql; 
create table gvkeys
  as select distinct a.gvkey, b.lpermno as permno,
  coalesce (b.linkenddt,'31dec9999'd) as linkenddt format yymmddn8.,
  b.linkdt format yymmddn8., a.from as idxfrom, coalesce(a.thru,'31dec9999'd) as idxthru format yymmddn8.
  from comp.idxcst_his (where=(gvkeyx='000003')) a
  left join crsp.ccmxpf_linktable
           (where=(usedflag=1 and linkprim in ('P','C','J'))) b
  on a.gvkey=b.gvkey
 order by gvkey, linkdt;
quit;
endrsubmit;

rsubmit;
data gvkeys; set gvkeys;
  by gvkey linkdt linkenddt;
  if last.linkdt;
run;
endrsubmit;

rsubmit;
proc sql;
  create table comp_EA
  (keep=gvkey fyearq fqtr datadate
        rdq datafqtr permno idxfrom idxthru )
as select *
from comp.fundq
    (where=((not missing(saleq) or atq>0) and consol='C' and
    popsrc='D' and indfmt='INDL' and datafmt='STD' and not missing(datafqtr))) a
    inner join
    (select distinct gvkey, min(linkdt) as mindate,
    max(linkenddt) as maxdate from gvkeys group by gvkey) b
    on a.gvkey=b.gvkey and b.mindate<=a.datadate<=b.maxdate and "&bdate"d<=a.datadate<="&edate"d
	left join
	(select gvkey, permno, idxfrom, idxthru from gvkeys) c
	on a.gvkey=c.gvkey;
quit;
endrsubmit;

rsubmit;
data comp_EA;
set comp_EA;
where rdq>.;
run;

proc sort data=comp_EA nodupkey;
by gvkey permno datadate;
run;
endrsubmit;

rsubmit;
data comp_EA;
set comp_EA;
where rdq between idxfrom and idxthru;
run;
endrsubmit;


rsubmit;
proc datasets nowarn nolist nodetails;
      delete gvkeys;
run; quit;
endrsubmit;


/********************************************************************************************
      PART 2: Calculate the intrady returns
********************************************************************************************/
rsubmit;
proc sql;
create table crsp_prc 
as select distinct a.PERMNO,a.DATE,a.OPENPRC,a.BID,a.ASK,a.PRC,a.SHROUT
from crsp.dsf as a
inner join
comp_EA as b
on a.PERMNO=b.PERMNO and b.idxfrom <= a.date <= b.idxthru;
quit;
endrsubmit;

rsubmit;
proc sort data=crsp_prc out=crsp_prc;
by date permno;
run;
endrsubmit;

rsubmit;
proc sql;
create table crsp_prc_1
as select *, prc*shrout/sum(prc*shrout) as weights
from crsp_prc
group by date;
endrsubmit;

rsubmit;
proc sort data=crsp_prc_1 out=crsp_prc_1;
by permno;
run;
endrsubmit;

/* Calculate the intraday open to open returns */
rsubmit;
data crsp_oo;
set crsp_prc_1;
by permno;
nextday_openprc=lag(openprc);
run;
endrsubmit;

rsubmit;
data crsp_oo;
set crsp_oo;
oo_ret=nextday_openprc/openprc-1.0;
w_oo_ret=oo_ret*weights;
run;
endrsubmit;

rsubmit;
proc sql;
create table crsp_oo_o
as select date, sum(w_oo_ret) as oo_ret_o
from crsp_oo
group by date;
endrsubmit;

/* Calculate the intrady close to close returns using bid*/
rsubmit;
data crsp_cc;
set crsp_prc_1;
by permno;
nextday_bid=lag(bid);
run;
endrsubmit;

rsubmit;
data crsp_cc;
set crsp_cc;
cc_ret=nextday_bid/bid-1.0;
w_cc_ret=cc_ret*weights;
run;
endrsubmit;

rsubmit;
proc sql;
create table crsp_cc_o
as select date, sum(w_cc_ret) as cc_ret_o
from crsp_cc
group by date;
endrsubmit;


/* Calculate the intrady open to close returns using bid */
rsubmit;
data crsp_oc;
set crsp_prc_1;
by permno;
nextday_bid=lag(bid);
run;
endrsubmit;

rsubmit;
data crsp_oc;
set crsp_oc;
oc_ret=nextday_bid/openprc-1.0;
w_oc_ret=oc_ret*weights;
run;
endrsubmit;

rsubmit;
proc sql;
create table crsp_oc_o
as select date, sum(w_oc_ret) as oc_ret_o
from crsp_oc
group by date;
endrsubmit;
