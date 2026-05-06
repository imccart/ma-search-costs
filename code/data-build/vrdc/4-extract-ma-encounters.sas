/* ------------------------------------------------------------ */
/* TITLE:        MA encounter extraction — bene-year utilization */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        ENRFPL<yr>.{IP,SNF,HHA,OP,CARRIER,DME}_BASE_ENC  */
/*               ENRFPL<yr>.{IP,SNF,HHA,OP}_REVENUE_ENC           */
/*               ENRFPL<yr>.{CARRIER,DME}_LINE_ENC                */
/* OUTPUT:       PL027710.ma_util_panel  (wide BENE_ID x year)    */
/*               PL027710.ma_util_<svc>  (per-svc stacked panels) */
/* ------------------------------------------------------------ */
/* Per-bene-year utilization counts to feed the R-side EC and    */
/* Var(C) projection through each plan's PBP cost-sharing        */
/* schedule. Encounter records have no payment fields so we do   */
/* the projection counterfactually in R.                         */
/*                                                                */
/* Variable names verified against CCW Encounter Records         */
/* Codebook v1.4 (Nov 2020); confirmed stable for 2015-2018.     */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* 4a. Inpatient — stays + days                                  */
/* ============================================================ */

%MACRO ip_util(yr);
    PROC SQL;
        CREATE TABLE WORK.ip_&yr AS
        SELECT BENE_ID,
               20&yr                AS year,
               COUNT(*)             AS n_ip_stays,
               SUM(CLM_DAY_CNT)     AS n_ip_days
        FROM ENRFPL&yr..IP_BASE_ENC
        GROUP BY BENE_ID;
    QUIT;
    %row_count(WORK.ip_&yr, IP &yr);
%MEND ip_util;


/* ============================================================ */
/* 4b. SNF — stays + days                                        */
/* ============================================================ */

%MACRO snf_util(yr);
    PROC SQL;
        CREATE TABLE WORK.snf_&yr AS
        SELECT BENE_ID,
               20&yr                AS year,
               COUNT(*)             AS n_snf_stays,
               SUM(CLM_DAY_CNT)     AS n_snf_days
        FROM ENRFPL&yr..SNF_BASE_ENC
        GROUP BY BENE_ID;
    QUIT;
    %row_count(WORK.snf_&yr, SNF &yr);
%MEND snf_util;


/* ============================================================ */
/* 4c. HHA — episodes + visits (revenue REV_CNTR_UNIT_CNT)       */
/* ============================================================ */

%MACRO hha_util(yr);
    PROC SQL;
        CREATE TABLE WORK.hha_visits_&yr AS
        SELECT CLM_CNTL_NUM,
               SUM(REV_CNTR_UNIT_CNT) AS n_visits
        FROM ENRFPL&yr..HHA_REVENUE_ENC
        GROUP BY CLM_CNTL_NUM;
    QUIT;

    PROC SQL;
        CREATE TABLE WORK.hha_&yr AS
        SELECT b.BENE_ID,
               20&yr                  AS year,
               COUNT(DISTINCT b.CLM_CNTL_NUM)  AS n_hha_episodes,
               COALESCE(SUM(v.n_visits), 0)    AS n_hha_visits
        FROM ENRFPL&yr..HHA_BASE_ENC AS b
        LEFT JOIN WORK.hha_visits_&yr AS v
            ON b.CLM_CNTL_NUM = v.CLM_CNTL_NUM
        GROUP BY b.BENE_ID;
    QUIT;
    PROC DELETE DATA=WORK.hha_visits_&yr; RUN;
    %row_count(WORK.hha_&yr, HHA &yr);
%MEND hha_util;


/* ============================================================ */
/* 4d. OP — visits, ER (rev_cntr 045X) split                     */
/* ------------------------------------------------------------ */
/* One row per (BENE_ID, claim) so a multi-line outpatient stay  */
/* with both ER and non-ER lines counts as a single ER visit.    */
/* ============================================================ */

%MACRO op_util(yr);
    PROC SQL;
        CREATE TABLE WORK.op_er_&yr AS
        SELECT DISTINCT CLM_CNTL_NUM
        FROM ENRFPL&yr..OP_REVENUE_ENC
        WHERE REV_CNTR BETWEEN "0450" AND "0459";
    QUIT;

    PROC SQL;
        CREATE TABLE WORK.op_&yr AS
        SELECT b.BENE_ID,
               20&yr                  AS year,
               COUNT(*)               AS n_op_visits,
               SUM(CASE WHEN er.CLM_CNTL_NUM IS NOT NULL THEN 1 ELSE 0 END)
                                      AS n_op_er_visits
        FROM ENRFPL&yr..OP_BASE_ENC AS b
        LEFT JOIN WORK.op_er_&yr AS er
            ON b.CLM_CNTL_NUM = er.CLM_CNTL_NUM
        GROUP BY b.BENE_ID;
    QUIT;
    PROC DELETE DATA=WORK.op_er_&yr; RUN;
    %row_count(WORK.op_&yr, OP &yr);
%MEND op_util;


/* ============================================================ */
/* 4e. Carrier — visits, PCP vs specialist (PRVDR_SPCLTY)        */
/* ------------------------------------------------------------ */
/* PCP specialty codes per CMS: 01 (general practice),           */
/* 08 (family practice), 11 (internal medicine), 38 (geriatric). */
/* Aggregate at line level — multiple lines per claim is fine    */
/* because each line is its own service event for PBP purposes.  */
/* ============================================================ */

%MACRO car_util(yr);
    PROC SQL;
        CREATE TABLE WORK.car_&yr AS
        SELECT b.BENE_ID,
               20&yr                            AS year,
               COUNT(*)                         AS n_car_lines,
               SUM(CASE WHEN l.PRVDR_SPCLTY IN ("01","08","11","38")
                        THEN 1 ELSE 0 END)      AS n_car_pcp_lines,
               SUM(CASE WHEN l.PRVDR_SPCLTY NOT IN ("01","08","11","38")
                            AND l.PRVDR_SPCLTY NE ""
                        THEN 1 ELSE 0 END)      AS n_car_spec_lines
        FROM ENRFPL&yr..CARRIER_BASE_ENC AS b
        INNER JOIN ENRFPL&yr..CARRIER_LINE_ENC AS l
            ON b.CLM_CNTL_NUM = l.CLM_CNTL_NUM
        GROUP BY b.BENE_ID;
    QUIT;
    %row_count(WORK.car_&yr, Carrier &yr);
%MEND car_util;


/* ============================================================ */
/* 4f. DME — line items (LINE_SRVC_CNT)                          */
/* ============================================================ */

%MACRO dme_util(yr);
    PROC SQL;
        CREATE TABLE WORK.dme_&yr AS
        SELECT b.BENE_ID,
               20&yr                       AS year,
               COUNT(*)                    AS n_dme_lines,
               SUM(l.LINE_SRVC_CNT)        AS n_dme_units
        FROM ENRFPL&yr..DME_BASE_ENC AS b
        INNER JOIN ENRFPL&yr..DME_LINE_ENC AS l
            ON b.CLM_CNTL_NUM = l.CLM_CNTL_NUM
        GROUP BY b.BENE_ID;
    QUIT;
    %row_count(WORK.dme_&yr, DME &yr);
%MEND dme_util;


/* ============================================================ */
/* 4g. Run all six macros across 4 years, then stack             */
/* ============================================================ */

%MACRO pull_all_enc;
    %DO yr = 15 %TO 18;
        %ip_util(&yr);
        %snf_util(&yr);
        %hha_util(&yr);
        %op_util(&yr);
        %car_util(&yr);
        %dme_util(&yr);
    %END;
%MEND pull_all_enc;
%pull_all_enc;

%MACRO stack_svc(svc);
    DATA PL027710.ma_util_&svc;
        SET WORK.&svc._15 WORK.&svc._16 WORK.&svc._17 WORK.&svc._18;
    RUN;
    %row_count(PL027710.ma_util_&svc, ma_util_&svc);
%MEND stack_svc;
%stack_svc(ip);
%stack_svc(snf);
%stack_svc(hha);
%stack_svc(op);
%stack_svc(car);
%stack_svc(dme);


/* ============================================================ */
/* 4h. Combined wide bene-year panel                             */
/* ------------------------------------------------------------ */
/* Build a spine = union of (BENE_ID, year) across all 6         */
/* categories, then LEFT JOIN each category back. Benes with     */
/* utilization in one category but not another keep a row with   */
/* 0 in the empty columns.                                       */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.spine AS
    SELECT BENE_ID, year FROM PL027710.ma_util_ip   UNION
    SELECT BENE_ID, year FROM PL027710.ma_util_snf  UNION
    SELECT BENE_ID, year FROM PL027710.ma_util_hha  UNION
    SELECT BENE_ID, year FROM PL027710.ma_util_op   UNION
    SELECT BENE_ID, year FROM PL027710.ma_util_car  UNION
    SELECT BENE_ID, year FROM PL027710.ma_util_dme;
QUIT;
%row_count(WORK.spine, util spine);

PROC SQL;
    CREATE TABLE PL027710.ma_util_panel AS
    SELECT s.BENE_ID, s.year,
           COALESCE(ip.n_ip_stays,        0)  AS n_ip_stays,
           COALESCE(ip.n_ip_days,         0)  AS n_ip_days,
           COALESCE(snf.n_snf_stays,      0)  AS n_snf_stays,
           COALESCE(snf.n_snf_days,       0)  AS n_snf_days,
           COALESCE(hha.n_hha_episodes,   0)  AS n_hha_episodes,
           COALESCE(hha.n_hha_visits,     0)  AS n_hha_visits,
           COALESCE(op.n_op_visits,       0)  AS n_op_visits,
           COALESCE(op.n_op_er_visits,    0)  AS n_op_er_visits,
           COALESCE(car.n_car_lines,      0)  AS n_car_lines,
           COALESCE(car.n_car_pcp_lines,  0)  AS n_car_pcp_lines,
           COALESCE(car.n_car_spec_lines, 0)  AS n_car_spec_lines,
           COALESCE(dme.n_dme_lines,      0)  AS n_dme_lines,
           COALESCE(dme.n_dme_units,      0)  AS n_dme_units
    FROM WORK.spine                AS s
    LEFT JOIN PL027710.ma_util_ip  AS ip  ON s.BENE_ID = ip.BENE_ID  AND s.year = ip.year
    LEFT JOIN PL027710.ma_util_snf AS snf ON s.BENE_ID = snf.BENE_ID AND s.year = snf.year
    LEFT JOIN PL027710.ma_util_hha AS hha ON s.BENE_ID = hha.BENE_ID AND s.year = hha.year
    LEFT JOIN PL027710.ma_util_op  AS op  ON s.BENE_ID = op.BENE_ID  AND s.year = op.year
    LEFT JOIN PL027710.ma_util_car AS car ON s.BENE_ID = car.BENE_ID AND s.year = car.year
    LEFT JOIN PL027710.ma_util_dme AS dme ON s.BENE_ID = dme.BENE_ID AND s.year = dme.year ;
QUIT;
PROC DELETE DATA=WORK.spine; RUN;
%row_count(PL027710.ma_util_panel, ma_util_panel);


/* ============================================================ */
/* 4i. Diagnostics                                                */
/* ============================================================ */

TITLE "MA encounter utilization — distinct benes by year";
PROC SQL;
    SELECT year,
           COUNT(*)                                         AS n_bene_years,
           SUM(n_ip_stays > 0)                              AS n_with_ip,
           SUM(n_snf_stays > 0)                             AS n_with_snf,
           SUM(n_hha_episodes > 0)                          AS n_with_hha,
           SUM(n_op_visits > 0)                             AS n_with_op,
           SUM(n_car_lines > 0)                             AS n_with_car,
           SUM(n_dme_lines > 0)                             AS n_with_dme,
           MEAN(n_ip_stays)         FORMAT=6.2              AS mean_ip_stays,
           MEAN(n_ip_days)          FORMAT=6.2              AS mean_ip_days,
           MEAN(n_snf_days)         FORMAT=6.2              AS mean_snf_days,
           MEAN(n_op_visits)        FORMAT=6.2              AS mean_op_visits,
           MEAN(n_op_er_visits)     FORMAT=6.2              AS mean_op_er,
           MEAN(n_car_pcp_lines)    FORMAT=6.2              AS mean_pcp,
           MEAN(n_car_spec_lines)   FORMAT=6.2              AS mean_spec
    FROM PL027710.ma_util_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;


/* ============================================================ */
/* Cleanup WORK                                                   */
/* ============================================================ */

%MACRO cleanup_enc;
    %DO yr = 15 %TO 18;
        PROC DELETE DATA=WORK.ip_&yr;  RUN;
        PROC DELETE DATA=WORK.snf_&yr; RUN;
        PROC DELETE DATA=WORK.hha_&yr; RUN;
        PROC DELETE DATA=WORK.op_&yr;  RUN;
        PROC DELETE DATA=WORK.car_&yr; RUN;
        PROC DELETE DATA=WORK.dme_&yr; RUN;
    %END;
%MEND cleanup_enc;
%cleanup_enc;
