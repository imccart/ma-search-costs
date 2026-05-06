/* ------------------------------------------------------------ */
/* TITLE:        FFS claims extraction — bene-year utilization   */
/*               and observed Part A/B cost-sharing              */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        RIF<yyyy>.INPATIENT_CLAIMS_<MM>                 */
/*               RIF<yyyy>.SNF_CLAIMS_<MM>                       */
/*               RIF<yyyy>.HHA_CLAIMS_<MM>     +HHA_REVENUE_<MM>  */
/*               RIF<yyyy>.OUTPATIENT_CLAIMS_<MM> +OUTPATIENT_REVENUE_<MM> */
/*               RIF<yyyy>.BCARRIER_LINE_<MM>                    */
/*               PL027710.bene_panel  (FFS bene-year filter)     */
/* OUTPUT:       PL027710.ffs_util_panel  (BENE_ID x year)        */
/*               PL027710.ffs_util_<svc>   (per-svc stacked)      */
/* ------------------------------------------------------------ */
/* Mirrors script 4 for the FFS side. DME omitted (not on seat). */
/* ------------------------------------------------------------ */
/* Variable names verified against existing seat-tested code in  */
/* healthcare-vi/physician-agency/physician-hospital-VI/         */
/* data-code/8_Episodes.sas (IP, SNF, HHA, OP, Carrier) and      */
/* against ResDAC RIF documentation.                             */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Helper: stack 12 monthly RIF files for one service+year       */
/* ------------------------------------------------------------ */
/* Used for IP, SNF, HHA, OP. Carrier line is too big to stack   */
/* annually; aggregate per-month below.                          */
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
/* 5b. IP — INPATIENT_CLAIMS (12-month stack)                    */
/* ------------------------------------------------------------ */
/* Interim bills: a single stay may span multiple monthly claims */
/* sharing one CLM_ADMSN_DT. COUNT DISTINCT CLM_ADMSN_DT counts  */
/* stays correctly; SUM(CLM_UTLZTN_DAY_CNT) and SUM(NCH_IP_TOT_  */
/* DDCTN_AMT) sum across interim bills correctly.                */
/* NCH_IP_TOT_DDCTN_AMT = bene IP deductible + Part A coins +    */
/* blood deductible (single combined field per CMS).             */
/* ============================================================ */

%MACRO ip_util(year);
    %stack_rif(INPATIENT_CLAIMS, &year);
    PROC SQL;
        CREATE TABLE WORK.ip_&year AS
        SELECT a.BENE_ID, &year AS year,
               COUNT(DISTINCT a.CLM_ADMSN_DT) AS n_ip_stays,
               SUM(a.CLM_UTLZTN_DAY_CNT)      AS n_ip_days,
               SUM(a.NCH_IP_TOT_DDCTN_AMT)    AS c_ip
        FROM WORK.INPATIENT_CLAIMS_stack_&year AS a
        INNER JOIN WORK.ffs_benes AS b
            ON a.BENE_ID = b.BENE_ID AND b.year = &year
        GROUP BY a.BENE_ID;
    QUIT;
    PROC DELETE DATA=WORK.INPATIENT_CLAIMS_stack_&year; RUN;
    %row_count(WORK.ip_&year, IP &year);
%MEND ip_util;


/* ============================================================ */
/* 5c. SNF — SNF_CLAIMS (12-month stack)                          */
/* ------------------------------------------------------------ */
/* Same field structure as INPATIENT_CLAIMS. Part A cost-sharing */
/* in NCH_IP_TOT_DDCTN_AMT.                                      */
/* ============================================================ */

%MACRO snf_util(year);
    %stack_rif(SNF_CLAIMS, &year);
    PROC SQL;
        CREATE TABLE WORK.snf_&year AS
        SELECT a.BENE_ID, &year AS year,
               COUNT(DISTINCT a.CLM_ADMSN_DT) AS n_snf_stays,
               SUM(a.CLM_UTLZTN_DAY_CNT)      AS n_snf_days,
               SUM(a.NCH_IP_TOT_DDCTN_AMT)    AS c_snf
        FROM WORK.SNF_CLAIMS_stack_&year AS a
        INNER JOIN WORK.ffs_benes AS b
            ON a.BENE_ID = b.BENE_ID AND b.year = &year
        GROUP BY a.BENE_ID;
    QUIT;
    PROC DELETE DATA=WORK.SNF_CLAIMS_stack_&year; RUN;
    %row_count(WORK.snf_&year, SNF &year);
%MEND snf_util;


/* ============================================================ */
/* 5d. HHA — HHA_CLAIMS + HHA_REVENUE                             */
/* ------------------------------------------------------------ */
/* Visit count = SUM(REV_CNTR_UNIT_CNT) per CLM_ID.              */
/* HHA carries no FFS bene cost-sharing (per existing pipeline   */
/* in 8_Episodes.sas: HHA_Spend = CLM_PMT + primary payer only). */
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
/* 5e. OP — OUTPATIENT_CLAIMS + OUTPATIENT_REVENUE                */
/* ------------------------------------------------------------ */
/* ER flag from REV_CNTR codes 0450-0459. Bene cost-sharing on   */
/* the base claim file: NCH_BENE_PTB_DDCTBL_AMT +                */
/* NCH_BENE_PTB_COINSRNC_AMT (matches 8_Episodes.sas line 108).  */
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
/* 5f. Carrier — BCARRIER_LINE, per-month aggregation             */
/* ------------------------------------------------------------ */
/* Annual stack of BCARRIER_LINE is too big for WORK. Aggregate  */
/* monthly to FFS benes, then re-aggregate to bene-year.         */
/* PCP specialty codes per CMS: 01,08,11,38.                     */
/* Cost-sharing matches 8_Episodes.sas line 163:                 */
/*   LINE_COINSRNC_AMT + LINE_BENE_PTB_DDCTBL_AMT                */
/* ============================================================ */

%MACRO car_month(year, mm);
    PROC SQL;
        CREATE TABLE WORK.car_&year._m&mm AS
        SELECT a.BENE_ID,
               COUNT(*)                                            AS n_lines,
               SUM(CASE WHEN PRVDR_SPCLTY IN ("01","08","11","38")
                        THEN 1 ELSE 0 END)                         AS n_pcp,
               SUM(CASE WHEN PRVDR_SPCLTY NOT IN ("01","08","11","38")
                            AND PRVDR_SPCLTY NE ""
                        THEN 1 ELSE 0 END)                         AS n_spec,
               SUM(LINE_COINSRNC_AMT + LINE_BENE_PTB_DDCTBL_AMT)   AS c
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
/* 5g. Run all macros across 4 years, then stack per service     */
/* ============================================================ */

%MACRO pull_all_ffs;
    %DO yr = &mcbs_start %TO &mcbs_end;
        %ip_util(&yr);
        %snf_util(&yr);
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
%stack_svc_ffs(ip);
%stack_svc_ffs(snf);
%stack_svc_ffs(hha);
%stack_svc_ffs(op);
%stack_svc_ffs(car);


/* ============================================================ */
/* 5h. Combined wide bene-year panel + observed total cost-share */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.spine AS
    SELECT BENE_ID, year FROM PL027710.ffs_util_ip   UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_snf  UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_hha  UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_op   UNION
    SELECT BENE_ID, year FROM PL027710.ffs_util_car;
QUIT;

PROC SQL;
    CREATE TABLE PL027710.ffs_util_panel AS
    SELECT s.BENE_ID, s.year,
           COALESCE(ip.n_ip_stays,       0)  AS n_ip_stays,
           COALESCE(ip.n_ip_days,        0)  AS n_ip_days,
           COALESCE(snf.n_snf_stays,     0)  AS n_snf_stays,
           COALESCE(snf.n_snf_days,      0)  AS n_snf_days,
           COALESCE(hha.n_hha_episodes,  0)  AS n_hha_episodes,
           COALESCE(hha.n_hha_visits,    0)  AS n_hha_visits,
           COALESCE(op.n_op_visits,      0)  AS n_op_visits,
           COALESCE(op.n_op_er_visits,   0)  AS n_op_er_visits,
           COALESCE(car.n_car_lines,     0)  AS n_car_lines,
           COALESCE(car.n_car_pcp_lines, 0)  AS n_car_pcp_lines,
           COALESCE(car.n_car_spec_lines,0)  AS n_car_spec_lines,
           COALESCE(ip.c_ip,   0)
         + COALESCE(snf.c_snf, 0)
         + COALESCE(hha.c_hha, 0)
         + COALESCE(op.c_op,   0)
         + COALESCE(car.c_car, 0)            AS c_observed_ffs
    FROM WORK.spine                  AS s
    LEFT JOIN PL027710.ffs_util_ip   AS ip
        ON s.BENE_ID = ip.BENE_ID  AND s.year = ip.year
    LEFT JOIN PL027710.ffs_util_snf  AS snf
        ON s.BENE_ID = snf.BENE_ID AND s.year = snf.year
    LEFT JOIN PL027710.ffs_util_hha  AS hha
        ON s.BENE_ID = hha.BENE_ID AND s.year = hha.year
    LEFT JOIN PL027710.ffs_util_op   AS op
        ON s.BENE_ID = op.BENE_ID  AND s.year = op.year
    LEFT JOIN PL027710.ffs_util_car  AS car
        ON s.BENE_ID = car.BENE_ID AND s.year = car.year ;
QUIT;
PROC DELETE DATA=WORK.spine; RUN;
%row_count(PL027710.ffs_util_panel, ffs_util_panel);


/* ============================================================ */
/* 5i. Diagnostics                                                */
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
        PROC DELETE DATA=WORK.ip_&yr;  RUN;
        PROC DELETE DATA=WORK.snf_&yr; RUN;
        PROC DELETE DATA=WORK.hha_&yr; RUN;
        PROC DELETE DATA=WORK.op_&yr;  RUN;
        PROC DELETE DATA=WORK.car_&yr; RUN;
    %END;
%MEND cleanup_ffs;
%cleanup_ffs;
