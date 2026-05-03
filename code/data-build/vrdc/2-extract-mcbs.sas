/* ------------------------------------------------------------ */
/* TITLE:        MCBS extraction — 2015-2018 multi-segment join  */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        MCBS<yr>.SURVEY_DEMO_<yr>                       */
/*               MCBS<yr>.SURVEY_HISUMRY_<yr>                    */
/*               MCBS<yr>.SURVEY_MYENROLL_<yr>                   */
/*               MCBS<yr>.SURVEY_MAPLANQX_<yr>                   */
/*               MCBS<yr>.SURVEY_MCREPLNQ_<yr>                   */
/*               MCBS<yr>.SURVEY_GENHLTH_<yr>                    */
/*               MCBS<yr>.SURVEY_CENWGTS_<yr>                    */
/*               for <yr> in 2015..2018                          */
/* OUTPUT:       PL027710.mcbs_panel                             */
/* ------------------------------------------------------------ */
/* MCBS is delivered as ~46 segment-specific datasets per year. */
/* For the structural model we pull seven of them and join on   */
/* BASE_ID per year, then stack across 2015-2018.                */
/*                                                                */
/* Variable names verified against the 2023 MCBS Survey File    */
/* codebooks (.../Codebooks/{demo,hisumry,myenroll,maplanqx,    */
/* mcreplnq,genhlth,cenwgts}_2023.txt). The 2015-2018 schema    */
/* uses the same names per CMS user guide.                      */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* 2a. Extract one year — multi-segment join                     */
/* ============================================================ */

%MACRO extract_mcbs_year(yr);

    /* ---- Demographics (DEMO) ---- */
    PROC SQL;
        CREATE TABLE WORK.demo_&yr AS
        SELECT
            BASE_ID,
            INT_TYPE,
            H_AGE                       AS age,
            H_SEX                       AS sex_cd,
            H_RTIRCE                    AS race_cd,
            HISPORIG                    AS hispanic,
            SPDEGRCV                    AS education_cat,
            INCOME                      AS income_cat,
            INCOME_H                    AS income_continuous,
            IPR                         AS poverty_ratio,
            SPMARSTA                    AS marital_cat,
            H_RESST                     AS state_ssa,
            H_RESCTY                    AS county_ssa,
            H_ZIP                       AS zip_cd,
            H_RUCA                      AS ruca,
            H_CBSA                      AS cbsa_type,
            ADINATNL                    AS adi_national_pct,
            ADISTATE                    AS adi_state_decile
        FROM MCBS&yr..SURVEY_DEMO_&yr
        WHERE INT_TYPE IN ("C","B")     /* community-dwelling only */
          AND H_AGE >= 65 ;
    QUIT;
    %row_count(WORK.demo_&yr, demo &yr);

    /* ---- Health insurance summary (HISUMRY) ---- */
    PROC SQL;
        CREATE TABLE WORK.hisumry_&yr AS
        SELECT
            BASE_ID,
            H_MEDSTA                    AS medstatus,
            H_OPMDCD                    AS dual_annual,
            H_DUAL01, H_DUAL02, H_DUAL03, H_DUAL04, H_DUAL05, H_DUAL06,
            H_DUAL07, H_DUAL08, H_DUAL09, H_DUAL10, H_DUAL11, H_DUAL12,
            H_PDLS01, H_PDLS02, H_PDLS03, H_PDLS04, H_PDLS05, H_PDLS06,
            H_PDLS07, H_PDLS08, H_PDLS09, H_PDLS10, H_PDLS11, H_PDLS12
        FROM MCBS&yr..SURVEY_HISUMRY_&yr ;
    QUIT;

    /* ---- Monthly enrollment / MA flag (MYENROLL) ---- */
    PROC SQL;
        CREATE TABLE WORK.myenroll_&yr AS
        SELECT
            BASE_ID,
            PTA_MONS                    AS partA_mons,
            PTB_MONS                    AS partB_mons,
            H_MAFF01, H_MAFF02, H_MAFF03, H_MAFF04, H_MAFF05, H_MAFF06,
            H_MAFF07, H_MAFF08, H_MAFF09, H_MAFF10, H_MAFF11, H_MAFF12,
            H_PTD01,  H_PTD02,  H_PTD03,  H_PTD04,  H_PTD05,  H_PTD06,
            H_PTD07,  H_PTD08,  H_PTD09,  H_PTD10,  H_PTD11,  H_PTD12
        FROM MCBS&yr..SURVEY_MYENROLL_&yr ;
    QUIT;

    /* ---- MA plan questions (MAPLANQX) ---- */
    PROC SQL;
        CREATE TABLE WORK.maplanqx_&yr AS
        SELECT
            BASE_ID,
            D_MADV                      AS madv_self_report,
            MADVYRS                     AS madv_years_enrolled,
            MAMONPRM                    AS madv_monthly_premium
        FROM MCBS&yr..SURVEY_MAPLANQX_&yr ;
    QUIT;

    /* ---- Medicare plan questions (MCREPLNQ) — search behavior ---- */
    PROC SQL;
        CREATE TABLE WORK.mcreplnq_&yr AS
        SELECT
            BASE_ID,
            INTERNET                    AS has_internet,
            USENET                      AS uses_internet_for_info,
            COMPDESK                    AS has_desktop,
            COMPPHON                    AS has_smartphone,
            COMPTAB                     AS has_tablet,
            KNOWMC                      AS medicare_easy_understand,
            KCARKNOW                    AS medicare_self_knowledge,
            KNCOVOPT                    AS easy_compare_options,
            KNINFMCR                    AS tried_find_info,
            KVSTSITE                    AS visited_medicare_site,
            KCPHINFO                    AS called_800_medicare,
            RVWCOST                     AS reviewed_costs,
            RVWSRVC                     AS reviewed_services,
            CMPRPLN                     AS compared_plans,
            CPLNTYPC                    AS compared_ma,
            CPLNTYME                    AS compared_medigap,
            KCHIHELP                    AS who_decides_insurance
        FROM MCBS&yr..SURVEY_MCREPLNQ_&yr ;
    QUIT;

    /* ---- General health (GENHLTH) ---- */
    PROC SQL;
        CREATE TABLE WORK.genhlth_&yr AS
        SELECT
            BASE_ID,
            GENHELTH                    AS srh,
            COMPHLTH                    AS health_vs_year_ago
        FROM MCBS&yr..SURVEY_GENHLTH_&yr ;
    QUIT;

    /* ---- Sample weights + variance design (CENWGTS) ---- */
    PROC SQL;
        CREATE TABLE WORK.cenwgts_&yr AS
        SELECT
            BASE_ID,
            CEYRSWGT                    AS wgt_full_sample,
            SUDSTRAT                    AS variance_stratum,
            SUDUNIT                     AS variance_psu
        FROM MCBS&yr..SURVEY_CENWGTS_&yr ;
    QUIT;

    /* ---- Inner-join on BASE_ID (DEMO is the spine) ---- */
    PROC SQL;
        CREATE TABLE WORK.mcbs_&yr AS
        SELECT
            d.*,
            &yr                         AS year,
            h.medstatus,
            h.dual_annual,
            h.H_DUAL01, h.H_DUAL02, h.H_DUAL03, h.H_DUAL04,
            h.H_DUAL05, h.H_DUAL06, h.H_DUAL07, h.H_DUAL08,
            h.H_DUAL09, h.H_DUAL10, h.H_DUAL11, h.H_DUAL12,
            h.H_PDLS01, h.H_PDLS02, h.H_PDLS03, h.H_PDLS04,
            h.H_PDLS05, h.H_PDLS06, h.H_PDLS07, h.H_PDLS08,
            h.H_PDLS09, h.H_PDLS10, h.H_PDLS11, h.H_PDLS12,
            m.partA_mons,
            m.partB_mons,
            m.H_MAFF01, m.H_MAFF02, m.H_MAFF03, m.H_MAFF04,
            m.H_MAFF05, m.H_MAFF06, m.H_MAFF07, m.H_MAFF08,
            m.H_MAFF09, m.H_MAFF10, m.H_MAFF11, m.H_MAFF12,
            m.H_PTD01,  m.H_PTD02,  m.H_PTD03,  m.H_PTD04,
            m.H_PTD05,  m.H_PTD06,  m.H_PTD07,  m.H_PTD08,
            m.H_PTD09,  m.H_PTD10,  m.H_PTD11,  m.H_PTD12,
            mp.madv_self_report,
            mp.madv_years_enrolled,
            mp.madv_monthly_premium,
            mr.has_internet,
            mr.uses_internet_for_info,
            mr.has_desktop,
            mr.has_smartphone,
            mr.has_tablet,
            mr.medicare_easy_understand,
            mr.medicare_self_knowledge,
            mr.easy_compare_options,
            mr.tried_find_info,
            mr.visited_medicare_site,
            mr.called_800_medicare,
            mr.reviewed_costs,
            mr.reviewed_services,
            mr.compared_plans,
            mr.compared_ma,
            mr.compared_medigap,
            mr.who_decides_insurance,
            g.srh,
            g.health_vs_year_ago,
            w.wgt_full_sample,
            w.variance_stratum,
            w.variance_psu
        FROM        WORK.demo_&yr     AS d
        LEFT JOIN   WORK.hisumry_&yr  AS h  ON d.BASE_ID = h.BASE_ID
        LEFT JOIN   WORK.myenroll_&yr AS m  ON d.BASE_ID = m.BASE_ID
        LEFT JOIN   WORK.maplanqx_&yr AS mp ON d.BASE_ID = mp.BASE_ID
        LEFT JOIN   WORK.mcreplnq_&yr AS mr ON d.BASE_ID = mr.BASE_ID
        LEFT JOIN   WORK.genhlth_&yr  AS g  ON d.BASE_ID = g.BASE_ID
        LEFT JOIN   WORK.cenwgts_&yr  AS w  ON d.BASE_ID = w.BASE_ID ;
    QUIT;
    %row_count(WORK.mcbs_&yr, mcbs joined &yr);

    /* Clean up segment tables */
    PROC DELETE DATA=WORK.demo_&yr;     RUN;
    PROC DELETE DATA=WORK.hisumry_&yr;  RUN;
    PROC DELETE DATA=WORK.myenroll_&yr; RUN;
    PROC DELETE DATA=WORK.maplanqx_&yr; RUN;
    PROC DELETE DATA=WORK.mcreplnq_&yr; RUN;
    PROC DELETE DATA=WORK.genhlth_&yr;  RUN;
    PROC DELETE DATA=WORK.cenwgts_&yr;  RUN;

%MEND extract_mcbs_year;


/* ============================================================ */
/* 2b. Loop over 2015-2018                                       */
/* ============================================================ */

%MACRO extract_all_mcbs;
    %DO yr = &mcbs_start %TO &mcbs_end;
        %extract_mcbs_year(&yr);
    %END;
%MEND extract_all_mcbs;
%extract_all_mcbs;


/* ============================================================ */
/* 2c. Stack into a single MCBS panel                            */
/* ------------------------------------------------------------ */
/* Apply the on-the-fly model-sample filters (Medicare status,   */
/* full-year Part A/B). Search-behavior derived indicators are   */
/* computed here so they're consistent across years.             */
/* ============================================================ */

DATA PL027710.mcbs_panel;
    SET
    %DO yr = &mcbs_start %TO &mcbs_end;
        WORK.mcbs_&yr
    %END;
    ;

    /* Sample restrictions */
    full_year_partAB = (partA_mons = 12 AND partB_mons = 12);
    not_esrd         = (medstatus IN ("10","20"));    /* exclude ESRD codes 11/21/31 */

    /* Derived: ever-MA in the year (admin-source monthly flags) */
    ARRAY maff[12] $ H_MAFF01-H_MAFF12;
    ma_months = 0;
    DO i = 1 TO 12;
        IF maff[i] = "1" THEN ma_months + 1;
    END;
    is_ma_admin = (ma_months > 0);
    is_ffs_admin = (ma_months = 0);

    /* Derived: search behavior — any direct evidence of search */
    searched =
        (tried_find_info       = 1) OR
        (visited_medicare_site = 1) OR
        (called_800_medicare   = 1) OR
        (compared_plans        = 1) ;

    /* Derived: ever-dual in the year */
    ARRAY dual[12] $ H_DUAL01-H_DUAL12;
    dual_months = 0;
    DO i = 1 TO 12;
        IF dual[i] NOT IN ("", "NA") THEN dual_months + 1;
    END;
    is_dual_ever = (dual_months > 0);

    DROP i;
RUN;

%row_count(PL027710.mcbs_panel, MCBS panel stacked);


/* ============================================================ */
/* 2d. Diagnostics                                                */
/* ============================================================ */

TITLE "MCBS panel — counts by year";
PROC SQL;
    SELECT
        year,
        COUNT(*)                              AS n,
        SUM(full_year_partAB)                 AS n_full_AB,
        SUM(is_ma_admin)                      AS n_ma,
        MEAN(is_ma_admin)                     AS pct_ma          FORMAT=PERCENT8.1,
        MEAN(searched)                        AS pct_searched    FORMAT=PERCENT8.1,
        MEAN(visited_medicare_site = 1)       AS pct_visited     FORMAT=PERCENT8.1,
        MEAN(compared_plans = 1)              AS pct_compared    FORMAT=PERCENT8.1,
        MEAN(has_internet = 1)                AS pct_internet    FORMAT=PERCENT8.1,
        MEAN(is_dual_ever)                    AS pct_dual        FORMAT=PERCENT8.1
    FROM PL027710.mcbs_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;

TITLE "Non-missing on key model variables (full panel)";
PROC SQL;
    SELECT
        SUM(income_cat IS NOT NULL)           AS n_income,
        SUM(education_cat IS NOT NULL)        AS n_education,
        SUM(has_internet IS NOT NULL)         AS n_internet,
        SUM(srh IS NOT NULL)                  AS n_srh,
        SUM(adi_national_pct IS NOT NULL)     AS n_adi,
        COUNT(*)                              AS n_total
    FROM PL027710.mcbs_panel
    WHERE full_year_partAB = 1 AND not_esrd = 1;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up per-year stacks                                      */
/* ============================================================ */

%MACRO cleanup_yr;
    %DO yr = &mcbs_start %TO &mcbs_end;
        PROC DELETE DATA=WORK.mcbs_&yr; RUN;
    %END;
%MEND cleanup_yr;
%cleanup_yr;
