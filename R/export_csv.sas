libname rfl04 "X:/HAB-RFL-Mise-a-disposition/HAB_A1223040_DMMICSAS";
libname rfl05 "X:/HAB-RFL-Mise-a-disposition/HAB_A1223050_DMMICSAS";
libname rfl06 "X:/HAB-RFL-Mise-a-disposition/HAB_A1223060_DMMICSAS";
libname rfl07 "X:/HAB-RFL-Mise-a-disposition/HAB_A1223070_DMMICSAS";
libname rfl08 "X:/HAB-RFL-Mise-a-disposition/HAB_A1223080_DMMICSAS";
libname rfl09 "X:/HAB-RFL-Mise-a-disposition/HAB_A1223090_DMMICSAS"; 

libname rfl10 "X:/HAB-RFL-PROD/2010/INFRA/MENAGE/MENIR"; 
libname rfl11 "X:/HAB-RFL-PROD/2011/INFRA/MENAGE/MENIR"; 

/*libname filo12 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2012/Table M?nages/donn?es";  
libname filo13 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2013/Table M?nages/donn?es"; */ 

libname filo12pd "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\passage_id_filo\12\N_infra";  
libname filo13pd "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\passage_id_filo\13\n_infra";  


libname filo14 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2014/Table M?nages/donn?es"; 
libname filo15 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2015/Table M?nages/donn?es"; 
libname filo16 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2016/Table M?nages/donn?es"; 
libname filo17 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2017/Table M?nages/donn?es"; 
libname filo18 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2018/Table M?nages/donn?es"; 
libname filo19 "X:/HAB-FILOSOFI-DONNEES-NBL/Filosofi 2019/Table M?nages/donn?es"; 


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl04.MENIR;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men04.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl05.MENIR;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men05.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;



PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl06.MENIR;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men06.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, ZFISCUCMDS AS revdecucm, nbpersmMDS AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl07.MENIR;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men07.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl08.MENIR;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men08.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl09.Menirmet09;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men09.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl10.Menirmet10;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men10.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, zfiscuc AS revdecucm, nbpersm AS nbpersm, depcom AS depcom, plg_x AS x , plg_y AS y
        FROM     rfl11.Menirmet11;
;QUIT;


PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men11.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


/* ========================= 2012 ========================= */
/*PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id, revdecucm AS revdecucm, nbpersm AS nbpersm,depcom AS depcom , x AS x ,y AS y
        FROM     filo12.menages12;
;QUIT;*/

PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, revdecucm AS revdecucm, nbpersm AS nbpersm,depcom AS depcom , x AS x ,y AS y
        FROM     filo12pd.n_revdisp12;
;QUIT;

PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men12.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

/* ========================= 2013 ========================= */
/*PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id, revdecucm AS revdecucm, nbpersm AS nbpersm, depcom AS depcom , x AS x ,y AS y
        FROM     filo13.menages13;
;QUIT;*/

PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  dirnoseq AS id, (revdecm/nb_uc) AS revdecucm, nbpersm AS nbpersm,depcom AS depcom , x AS x ,y AS y
        FROM     filo13pd.n_revdisp13;
;QUIT;

PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men13.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id,  revdecucm AS revdecucm, nbpersm AS nbpersm,depcom AS depcom , x AS x ,y AS y
        FROM     filo14.menages14;
;QUIT;
PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men14.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id, revdecucm AS revdecucm, nbpersm AS nbpersm, depcom AS depcom , x AS x ,y AS y
        FROM     filo15.menages15;
;QUIT;
PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men15.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id,  revdecucm AS revdecucm, nbpersm AS nbpersm,depcom AS depcom , x AS x ,y AS y
        FROM     filo16.menages16;
;QUIT;
PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men16.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id, revdecucm AS revdecucm, nbpersm AS nbpersm, depcom AS depcom , x AS x ,y AS y
        FROM     filo17.menages17;
;QUIT;
PROC EXPORT DATA= tabmen 
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men17.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;


PROC SQL;
    CREATE TABLE tabmen AS
        SELECT   identifiant AS id, revdecucm AS revdecucm, nbpersm AS nbpersm, depcom AS depcom , x AS x ,y AS y
        FROM     filo18.menages18;
;QUIT;
PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men18.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;

PROC SQL;
    CREATE TABLE tabmen AS
        SELECT  identifiant AS id, revdecucm AS revdecucm, nbpersm AS nbpersm, depcom AS depcom , x AS x ,y AS y
        FROM     filo19.menages19;
;QUIT;
PROC EXPORT DATA= tabmen
            OUTFILE= "X:\HAB-PSAR-AU-AU33-DEV\au33_v2\data\input\bases_menages\men19.csv"
            DBMS=CSV REPLACE;
     PUTNAMES=YES;
RUN;




