/* ------------------------------------------------------------ */
/* TITLE:        FFS claims extraction — bene-year utilization   */
/*               and observed Part A/B cost-sharing              */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        MEDPAR.MEDPAR_<yyyy>                            */
/*               RIF<yyyy>.HHA_CLAIMS_<MM>     +HHA_REVENUE_<MM>  */
/*               RIF<yyyy>.OUTPATIENT_CLAIMS_<MM> +OUTPATIENT_REVENUE_<MM> */
/*               RIF<yyyy>.BCARRIER_LINE_<MM>                    */
/*               PL027710.bene_panel  (FFS bene-year filter)     */
/* OUTPUT:       PL027710.ffs_util_panel  (BENE_ID x year)        */
/*               PL027710.ffs_util_<svc>   (per-svc stacked)      */
/* ------------------------------------------------------------ */
/* Mirrors script 4 for the FFS side. DME omitted: no DME on the */
/* seat (not in RIF, not in MEDPAR). Drop matching DME columns   */
/* from script 4 if you want symmetric output.                   */
/* ------------------------------------------------------------ */
/* Variable names verified against ResDAC RIF documentation:     */
/* MEDPAR file, Carrier RIF, Outpatient RIF, HHA RIF (2026-05-06).*/
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Helper: stack 12 monthly RIF files for one service+year       */
/* ------------------------------------------------------------ */
/* Used for HHA and Outpatient only (smaller files). Carrier     */
/* line is too big to stack annually; aggregate per-month below. */
/* ============================================================ */

%MACRO stack_rif(file_root, year);
    DATA WORK.&file_root._stack_&year;
        SET
        %DO m = 1 %TO 12;
            RIF&year..&file_root._%SYSFUNC(PUTN(&m, Z2.))
        %END;
        ;
    RUN;
%MEND stack_rif;


/* ============================================================ */
/* 5a. FFS bene lookup (analytic sample only)                    */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.ffs_benes AS
    SELECT BENE_ID, year
    FROM PL027710.bene_panel
    WHERE is_ffs_mbsf = 1;
QUIT;
%row_count(WORK.ffs_benes, FFS bene-years);


/* ============================================================ */
/* 5b. IP + SNF — MEDPAR (annual, one row per stay)              */
/* ------------------------------------------------------------ */
/* SS_LS_SNF_IND_CD = "S" -> SNF; everything else -> IP.         */
/* IP cost-sharing: deductible + Part A coinsurance + blood.     */
/* SNF cost-sharing: Part A coinsurance only (no deductible).    */
/* ============================================================ */

%MACRO ip_snf_util(year);
    PROC SQL;
        CREATE TABLE WORK.ipsnf_&year AS
        SELECT a.BENE_ID, &year AS year,
               SUM(SS_LS_SNF_IND_CD NE "S")                                AS n_ip_stays,
               SUM(CASE WHEN SS_LS_SNF_IND_CD NE "S" THEN UTLZTN_DAY_CNT ELSE 0 END)  AS n_ip_days,
               SUM(SS_LS_SNF_IND_CD = "S")                                 AS n_snf_stays,
               SUM(CASE WHEN SS_LS_SNF_IND_CD = "S" THEN UTLZTN_DAY_CNT ELSE 0 END)   AS n_snf_days,
               SUM(BENE_IP_DDCTBL_AMT + BENE_PTA_COINSRNC_AMT + BENE_BLOOD_DDCTBL_AMT) AS c_ipsnf
        FROM MEDPAR.MEDPAR_&year AS a
        INNER JOIN WORK.ffs_benes AS b
            ON a.BENE_ID = b.BENE_ID AND b.year = &year
        GROUP BY a.BENE_ID;
    QUIT;
    %row_count(WORK.ipsnf_&year, IP+SNF &year);
%MEND ip_snf_util;


/* ============================================================ */
/* 5c. HHA — RIF, monthly stack + revenue join                   */
/* ------------------------------------------------------------ */
/* Visit count = SUM(REV_CNTR_UNIT_CNT) from revenue file.       */
/* HHA carries no FFS cost-sharing -> c_hha set to 0.            */
/* ============================================================ */

%MACRO hha_util(year);
    %stack_rif(HHA_CLAIMS,  &year);
    %stack_rif(HHA_REVENUE, &year);

    PROC SQL;
        CREATE TABLE WORK.hha_visits_&year AS
        SELECT CLM_ID, SUM(REV_CNTR_UNIT_CNT) AS n_visits
        FROM WORK.HHA_REVENUE_stack_&year
        GROUP BY CLM_ID;
    QUIT;

    PROC SQL;
        CREATE TABLE WORK.hha_&year AS
        SELECT a.BENE_ID, &year AS year,
               COUNT(DISTINCT a.CLM_ID)             AS n_hha_episodes,
               COALESCE(SUM(v.n_visits), 0)         AS n_hha_visits,
               0                                    AS c_hha
        FROM WORK.HHA_CLAIMS_stack_&year AS a
        INNER JOIN WORK.ffs_benes AS b
            ON a.BENE_ID = b.BENE_ID AND b.year = &year
        LEFT JOIN WORK.hha_visits_&year AS v
            ON a.CLM_ID = v.CLM_ID
        GROUP BY a.BENE_ID;
    QUIT;

    PROC DELETE DATA=WORK.HHA_CLAIMS_stack_&year;  RUN;
    PROC DELETE DATA=WORK.HHA_REVENUE_stack_&year; RUN;
    PROC DELETE DATA=WORK.hha_visits_&year;        RUN;
    %row_count(WORK.hha_&year, HHA &year);
%MEND hha_util;


/* ============================================================ */
/* 5d. OP — RIF, monthly stack, ER flag from REV_CNTR 045X       */
/* ------------------------------------------------------------ */
/* OP cost-sharing: Part B deductible + Part B coinsurance       */
/* (claim-level NCH_* fields on the base claim file).            */
/* ============================================================ */

%MACRO op_util(year);
    %stack_rif(OUTPATIENT_CLAIMS,  &year);
    %stack_rif(OUTPATIENT_REVENUE, &year);

    PROC SQL;
        CREATE TABLE WORK.op_er_&year AS
        SELECT DISTINCT CLM_ID
        FROM WORK.OUTPATIENT_REVENUE_stack_&year
        WHERE REV_CNTR BETWEEN "0450" AND "0459";
    QUIT;

    PROC SQL;
        CREATE TABLE WORK.op_&year AS
        SELECT a.BENE_ID, &year AS year,
               COUNT(*)                                                  AS n_op_visits,
               SUM(CASE WHEN er.CLM_ID IS NOT NULL THEN 1 ELSE 0 END)    AS n_op_er_visits,
               SUM(NCH_BENE_PTB_DDCTBL_AMT + NCH_BENE_PTB_COINSRNC_AMT)  AS c_op
        FROM WORK.OUTPATIENT_CLAIMS_stack_&year AS a
        INNER JOIN WORK.ffs_benes AS b
            ON a.BENE_ID = b.BENE_ID AND b.year = &year
        LEFT JOIN WORK.op_er_&year AS er
            ON a.CLM_ID = er.CLM_ID
        GROUP BY a.BENE_ID;
    QUIT;

    PROC DELETE DATA=WORK.OUTPATIENT_CLAIMS_stack_&year;  RUN;
    PROC DELETE DATA=WORK.OUTPATIENT_REVENUE_stack_&year; RUN;
    PROC DELETE DATA=WORK.op_er_&year;                    RUN;
    %row_count(WORK.op_&year, OP &year);
%MEND op_util;


/* ============================================================ */
/* 5e. Carrier — RIF line file, per-month aggregation            */
/* ------------------------------------------------------------ */
/* Annual stack of BCARRIER_LINE is too big for WORK. Aggregate  */
/* monthly to FFS benes, then re-aggregate to bene-year.         */
/* PCP specialty codes per CMS: 01,08,11,38.                     */
/* Cost-sharing: line-level Part B deductible + coinsurance.     */
/* ============================================================ */

%MACRO car_month(year, mm);
    PROC SQL;
        CREATE TABLE WORK.car_&year._m&mm AS
        SELECT a.BENE_ID,
               COUNT(*)                                                AS n_lines,
               SUM(CASE WHEN PRVDR_SPCLTY IN ("01","08","11","38")
                        THEN 1 ELSE 0 END)                             AS n_pcp,
               SUM(CASE WHEN PRVDR_SPCLTY NOT IN ("01","08","11","38")
                            AND PRVDR_SPCLTY NE ""
                        THEN 1 ELSE 0 END)                             AS n_spec,
               SUM(LINE_BENE_PTB_DDCTBL_AMT + LINE_COINSRNC_AMT)       AS c
        FROM RIF&year..BCARRIER_LINE_&mm AS a
        INNER JOIN WORK.ffs_benes AS b
            ON a.BENE_ID = b.BENE_ID AND b.year = &year
        GROUP BY a.BENE_ID;
    QUIT;
%MEND car_month;

%MACRO car_util(year);
    %DO m = 1 %TO 12;
        %car_month(&year, %SYSFUNC(PUTN(&m, Z2.)));
    %END;

    DATA WORK.car_stack_&year;
        SET %DO m = 1 %TO 12; WORK.car_&year._m%SYSFUNC(PUTN(&m, Z2.)) %END; ;
    RUN;

    PROC SQL;
        CREATE TABLE WORK.car_&year AS
        SELECT BENE_ID, &year AS year,
               SUM(n_lines) AS n_car_lines,
               SUM(n_pcp)   AS n_car_pcp_lines,
               SUM(n_spec)  AS n_car_spec_lines,
               SUM(c)       AS c_car
        FROM WORK.car_stack_&year
        GROUP BY BENE_ID;
    QUIT;

    %DO m = 1 %TO 12;
        PROC DELETE DATA=WORK.car_&year._m%SYSFUNC(PUTN(&m, Z2.)); RUN;
    %END;
    PROC DELETE DATA=WORK.car_stack_&year; RUN;
    %row_count(WORK.car_&year, Carrier &year);
%MEND car_util;


/* ============================================================ */
/* 5f. Run all macros across 4 years, then stack per service     */
/* ============================================================ */

%MACRO pull_all_ffs;
    %DO yr = &mcbs_start %TO &mcbs_end;
        %ip_snf_util(&yr);
        %hha_util(&yr);
        %op_util(&yr);
        %car_util(&yr);
    %END;
%MEND pull_all_ffs;
%pull_all_ffs;

%MACRO stack_svc_ffs(svc);
    DATA PL027710.ffs_util_&svc;
        SET
        %DO yr = &mcbs_start %TO &mcbs_end;
            WORK.&svc._&yr
        %END;
        ;
    RUN;
    %row_count(PL027710.ffs_util_&svc, ffs_util_&svc);
%MEND stack_svc_ffs;
%stack_svc_ffs(ipsnf);
%stack_svc_ffs(hha);
%stack_svc_ffs(op);
%stack_svc_ffs(car);


/* ============================================================ */
/* 5g. Combined wide bene-year panel + observed total cost-share */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.spine AS
    SELECT BENE_ID, year FROM PL027710.ffs_util_ipsnf UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_hha   UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_op    UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_car;
QUIT;

PROC SQL;
    CREATE TABLE PL027710.ffs_util_panel AS
    SELECT s.BENE_ID, s.year,
           COALESCE(ipsnf.n_ip_stays,    0)  AS n_ip_stays,
           COALESCE(ipsnf.n_ip_days,     0)  AS n_ip_days,
           COALESCE(ipsnf.n_snf_stays,   0)  AS n_snf_stays,
           COALESCE(ipsnf.n_snf_days,    0)  AS n_snf_days,
           COALESCE(hha.n_hha_episodes,  0)  AS n_hha_episodes,
           COALESCE(hha.n_hha_visits,    0)  AS n_hha_visits,
           COALESCE(op.n_op_visits,      0)  AS n_op_visits,
           COALESCE(op.n_op_er_visits,   0)  AS n_op_er_visits,
           COALESCE(car.n_car_lines,     0)  AS n_car_lines,
           COALESCE(car.n_car_pcp_lines, 0)  AS n_car_pcp_lines,
           COALESCE(car.n_car_spec_lines,0)  AS n_car_spec_lines,
           COALESCE(ipsnf.c_ipsnf, 0)
         + COALESCE(hha.c_hha,    0)
         + COALESCE(op.c_op,      0)
         + COALESCE(car.c_car,    0)         AS c_observed_ffs
    FROM WORK.spine                    AS s
    LEFT JOIN PL027710.ffs_util_ipsnf  AS ipsnf
        ON s.BENE_ID = ipsnf.BENE_ID AND s.year = ipsnf.year
    LEFT JOIN PL027710.ffs_util_hha    AS hha
        ON s.BENE_ID = hha.BENE_ID   AND s.year = hha.year
    LEFT JOIN PL027710.ffs_util_op     AS op
        ON s.BENE_ID = op.BENE_ID    AND s.year = op.year
    LEFT JOIN PL027710.ffs_util_car    AS car
        ON s.BENE_ID = car.BENE_ID   AND s.year = car.year ;
QUIT;
PROC DELETE DATA=WORK.spine; RUN;
%row_count(PL027710.ffs_util_panel, ffs_util_panel);


/* ============================================================ */
/* 5h. Diagnostics                                                */
/* ============================================================ */

TITLE "FFS utilization — distinct benes by year";
PROC SQL;
    SELECT year,
           COUNT(*)                                AS n_bene_years,
           SUM(n_ip_stays > 0)                     AS n_with_ip,
           SUM(n_snf_stays > 0)                    AS n_with_snf,
           SUM(n_hha_episodes > 0)                 AS n_with_hha,
           SUM(n_op_visits > 0)                    AS n_with_op,
           SUM(n_car_lines > 0)                    AS n_with_car,
           MEAN(n_ip_days)        FORMAT=6.2       AS mean_ip_days,
           MEAN(n_snf_days)       FORMAT=6.2       AS mean_snf_days,
           MEAN(n_op_visits)      FORMAT=6.2       AS mean_op_visits,
           MEAN(n_car_pcp_lines)  FORMAT=6.2       AS mean_pcp,
           MEAN(n_car_spec_lines) FORMAT=6.2       AS mean_spec,
           MEAN(c_observed_ffs)   FORMAT=DOLLAR10. AS mean_c_obs
    FROM PL027710.ffs_util_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;


/* ============================================================ */
/* Cleanup WORK                                                   */
/* ============================================================ */

PROC DELETE DATA=WORK.ffs_benes; RUN;

%MACRO cleanup_ffs;
    %DO yr = &mcbs_start %TO &mcbs_end;
        PROC DELETE DATA=WORK.ipsnf_&yr; RUN;
        PROC DELETE DATA=WORK.hha_&yr;   RUN;
        PROC DELETE DATA=WORK.op_&yr;    RUN;
        PROC DELETE DATA=WORK.car_&yr;   RUN;
    %END;
%MEND cleanup_ffs;
%cleanup_ffs;
