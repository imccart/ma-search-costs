/* ------------------------------------------------------------ */
/* TITLE:        MCBS extraction — 2015-2018 multi-segment join  */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        MCBS<yr>.SURVEY_DEMO_<yr>                       */
/*               MCBS<yr>.SURVEY_HISUMRY_<yr>                    */
/*               MCBS<yr>.SURVEY_MAPLANQX_<yr>                   */
/*               MCBS<yr>.SURVEY_MCREPLNQ_<yr>                   */
/*               MCBS<yr>.SURVEY_HITLINE_<yr>                    */
/*               MCBS<yr>.SURVEY_GENHLTH_<yr>                    */
/*               MCBS<yr>.SURVEY_CENWGTS_<yr>                    */
/*               for <yr> in 2015..2018                          */
/* OUTPUT:       PL027710.mcbs_panel                             */
/* ------------------------------------------------------------ */
/* Variable names verified against the 2018 MCBS Survey File    */
/* LDS codebooks (Codebooks/{demo,hisumry,maplanqx,mcreplnq,    */
/* genhlth,cenwgts}_2018.txt). 2015-2017 schema is the same     */
/* per the CMS PUF Data User's Guide.                           */
/*                                                                */
/* What changed vs. the 2023-codebook-derived first draft:       */
/*   - MYENROLL didn't exist in 2018; H_MAFF<MM> monthly MA       */
/*     flags live in HISUMRY in 2015-2018, not MYENROLL.         */
/*   - PTA_MONS / PTB_MONS / H_PTD<MM> not in MCBS 2015-2018.    */
/*     Part A/B months come from MBSF (BENE_HI_CVRAGE_TOT_MONS / */
/*     BENE_SMI_CVRAGE_TOT_MONS) instead.                        */
/*   - DEMO IPR (continuous) is post-2020; 2015-2018 has         */
/*     IPR_IND (5-bucket categorical) — pulled as poverty_ind.   */
/*   - MAPLANQX MAMONPRM (monthly) is post-2018; 2015-2018 has  */
/*     D_ANHMO (annual) — pulled as madv_annual_premium.        */
/*   - MCREPLNQ underwent a major redesign post-2018. Items       */
/*     dropped from this extract because they don't exist in    */
/*     2015-2018: INTERNET, USENET, COMPDESK, COMPPHON, COMPTAB, */
/*     RVWCOST, RVWSRVC, CMPRPLN, CPLNTYPC, CPLNTYME, KVSTSITE.  */
/*     Renamed: KVSTSITE -> KVSITWEB, USENET -> KNETPERS.        */
/*     New 2015-2018 items kept: KNETFRND, KNHAVCOM, KBOKRECD,   */
/*     KBOKREAD, KBOKUNDR, KREELINE.                              */
/*   - DEMO geography & ADI variable names are NOT stable in    */
/*     2015-2018 — they were renamed mid-decade. Year-specific  */
/*     names are resolved via macro variables below; the output */
/*     columns are always `cbsa_type`, `ruca`, `adi_raw`.       */
/*     Mapping (verified against 2015/2016/2017/2018 codebooks): */
/*        2015     2016        2017       2018                   */
/*        H_URBRUR H_URBRUR    H_CBSA    H_CBSA   -> cbsa_type   */
/*        (none)   (none)      H_RUCA    H_RUCA   -> ruca        */
/*        ADI      CENSADI     CENSADI   ADINATNL -> adi_raw     */
/*        CS1YRWGT CS1YRWGT    CEYRSWGT  CEYRSWGT -> wgt_full    */
/*     ADISTATE only exists in 2018; we don't pull it.           */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* 2a. Extract one year — multi-segment join                     */
/* ============================================================ */

%MACRO extract_mcbs_year(yr);

    /* ---- Resolve year-specific variable names ----           */
    /* H_URBRUR / H_CBSA, H_RUCA presence, ADI naming, weight   */
    /* name all changed within 2015-2018. Set macro vars per yr.*/
    %LOCAL urbrur_var ruca_select adi_var weight_var;
    %IF &yr LE 2016 %THEN %DO;
        %LET urbrur_var  = H_URBRUR;
        %LET ruca_select = '' AS ruca,;           /* H_RUCA absent 2015-2016 — character empty to match H_RUCA's character type in 2017-2018 */
        %IF &yr = 2015 %THEN %LET adi_var = ADI;
                       %ELSE %LET adi_var = CENSADI;
        %LET weight_var  = CS1YRWGT;
    %END;
    %ELSE %DO;
        %LET urbrur_var  = H_CBSA;
        %LET ruca_select = H_RUCA AS ruca,;
        %IF &yr = 2018 %THEN %LET adi_var = ADINATNL;
                       %ELSE %LET adi_var = CENSADI;
        %LET weight_var  = CEYRSWGT;
    %END;

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
            IPR_IND                     AS poverty_ratio_ind,    /* 5-bucket categorical */
            SPMARSTA                    AS marital_cat,
            H_RESST                     AS state_ssa,
            H_RESCTY                    AS county_ssa,
            H_ZIP                       AS zip_cd,
            &ruca_select
            &urbrur_var                 AS cbsa_type,
            &adi_var                    AS adi_raw          /* ADI 2015 / CENSADI 2016-17 / ADINATNL 2018 */
        FROM MCBS&yr..SURVEY_DEMO_&yr
        WHERE INT_TYPE IN ("C","B")     /* community-dwelling only */
          AND H_AGE >= 65 ;
    QUIT;
    %row_count(WORK.demo_&yr, demo &yr);

    /* ---- Health insurance summary (HISUMRY) ---- */
    /* In 2015-2018, monthly MA flags H_MAFF01..12 live here, not in   */
    /* MYENROLL (which doesn't exist as a 2015-2018 segment).          */
    PROC SQL;
        CREATE TABLE WORK.hisumry_&yr AS
        SELECT
            BASE_ID,
            H_MEDSTA                    AS medstatus,
            H_OPMDCD                    AS dual_annual,
            H_DUAL01, H_DUAL02, H_DUAL03, H_DUAL04, H_DUAL05, H_DUAL06,
            H_DUAL07, H_DUAL08, H_DUAL09, H_DUAL10, H_DUAL11, H_DUAL12,
            H_PDLS01, H_PDLS02, H_PDLS03, H_PDLS04, H_PDLS05, H_PDLS06,
            H_PDLS07, H_PDLS08, H_PDLS09, H_PDLS10, H_PDLS11, H_PDLS12,
            H_MAFF01, H_MAFF02, H_MAFF03, H_MAFF04, H_MAFF05, H_MAFF06,
            H_MAFF07, H_MAFF08, H_MAFF09, H_MAFF10, H_MAFF11, H_MAFF12
        FROM MCBS&yr..SURVEY_HISUMRY_&yr ;
    QUIT;

    /* ---- MA plan questions (MAPLANQX) ---- */
    /* MAMONPRM (monthly) is post-2018. 2015-2018 carries D_ANHMO       */
    /* (annual added cost for MA coverage). Convert later in R.         */
    PROC SQL;
        CREATE TABLE WORK.maplanqx_&yr AS
        SELECT
            BASE_ID,
            D_MADV                      AS madv_self_report,
            MADVYRS                     AS madv_years_enrolled,
            D_ANHMO                     AS madv_annual_premium
        FROM MCBS&yr..SURVEY_MAPLANQX_&yr ;
    QUIT;

    /* ---- Medicare plan questions (MCREPLNQ) — search behavior ---- */
    /* 2015-2018 era. Items dropped from the post-2018 redesign       */
    /* (INTERNET, USENET, COMPDESK, COMPPHON, COMPTAB, RVWCOST,       */
    /* RVWSRVC, CMPRPLN, CPLNTYPC, CPLNTYME, KVSTSITE) are not pulled. */
    PROC SQL;
        CREATE TABLE WORK.mcreplnq_&yr AS
        SELECT
            BASE_ID,
            KNOWMC                      AS medicare_easy_understand,
            KCARKNOW                    AS medicare_self_knowledge,
            KNCOVOPT                    AS easy_compare_options,
            KNINFMCR                    AS tried_find_info,
            KVSITWEB                    AS visited_medicare_site,    /* predecessor of KVSTSITE */
            KCPHINFO                    AS called_800_medicare,
            KCHIHELP                    AS who_decides_insurance,
            KNHAVCOM                    AS has_personal_computer,    /* coarser than COMPDESK/etc. */
            KNETPERS                    AS uses_internet_for_info,   /* predecessor of USENET */
            KNETFRND                    AS net_info_via_other,
            KBOKRECD                    AS book_received,
            KBOKREAD                    AS book_read_amount,
            KBOKUNDR                    AS book_understood,
            KREELINE                    AS aware_800_medicare
        FROM MCBS&yr..SURVEY_MCREPLNQ_&yr ;
    QUIT;

    /* ---- HI Type & Premium (HITLINE) — per-plan obtain-channel ---- */
    /* HITLINE has multiple rows per BASE_ID (one per insurance plan).  */
    /* PLANTYPE codes: 1=Mcare A, 2=Mcare B, 3=Mcare C/MA, 4=Mcare D,   */
    /* 5=Medicaid, 20-21=ESI, 30-31=Self-purchased private, 40=VA,     */
    /* 50=Tricare, 60=Retiree Drug Subsidy, 70=Other, 6=Other public.  */
    /* S_OBTNP = how plan was obtained (1=Directly, 2-9=via institution).*/
    /* We aggregate to one row per BASE_ID to flag institutional        */
    /* coverage and MA-via-institutional-channel — used downstream to   */
    /* restrict to direct-purchase active shoppers.                     */
    PROC SQL;
        CREATE TABLE WORK.hitline_raw_&yr AS
        SELECT
            BASE_ID,
            PLANTYPE,
            S_OBTNP,
            S_INS
        FROM MCBS&yr..SURVEY_HITLINE_&yr ;
    QUIT;

    /* Note: PLANTYPE and S_OBTNP are both NUMERIC on the seat          */
    /* (verified 2026-05-05). S_OBTNP includes a SAS special-missing    */
    /* value .N (formatted as "N"), which is sorted before any regular  */
    /* numeric and so is never matched by the institutional-channel    */
    /* IN list — desired behavior (treat .N as no-channel, not          */
    /* institutional).                                                  */
    /*                                                                  */
    /* MCBS HITLINE PLANTYPE codes (per CMS HITLINE codebook):          */
    /*   1=Medicare A, 2=Medicare B, 3=Medicare C/MA, 4=Medicare D /    */
    /*   Part D / MAPD, 5=Medicaid, 6=Other public, 20/21=ESI,          */
    /*   30/31=Self-purchased, 40=VA, 50=Tricare, 60=RDS, 70=Other.     */
    /* S_OBTNP ("how did you obtain this plan") is INAPPLICABLE for      */
    /* Medicare A/B/C/D rows — those rows always have S_OBTNP = . , so   */
    /* ma_obtained_directly and ma_obtained_inst computed below are     */
    /* zero by construction. Kept as columns for diagnostic continuity   */
    /* but not used in the active-shopper filter (which simplifies to   */
    /* NOT inst_coverage given ma_obtained_inst = 0 always).             */
    PROC SQL;
        CREATE TABLE WORK.hitline_&yr AS
        SELECT
            BASE_ID,
            MAX(PLANTYPE IN (20, 21))                       AS has_esi,
            MAX(PLANTYPE = 40)                              AS has_va,
            MAX(PLANTYPE = 50)                              AS has_tricare,
            MAX(PLANTYPE = 60)                              AS has_rds,
            MAX(PLANTYPE = 30 OR PLANTYPE = 31)             AS has_self_purch,
            MAX(PLANTYPE = 3)                               AS has_ma_row,
            /* Always zero by construction — S_OBTNP inapplicable for MA */
            MAX(PLANTYPE = 3 AND S_OBTNP = 1)               AS ma_obtained_directly,
            MAX(PLANTYPE = 3 AND S_OBTNP IN (2,3,4,5,6,7,8,9,91))
                                                            AS ma_obtained_inst
        FROM WORK.hitline_raw_&yr
        GROUP BY BASE_ID ;
    QUIT;
    PROC DELETE DATA=WORK.hitline_raw_&yr; RUN;

    /* ---- General health (GENHLTH) ---- */
    PROC SQL;
        CREATE TABLE WORK.genhlth_&yr AS
        SELECT
            BASE_ID,
            GENHELTH                    AS srh,
            COMPHLTH                    AS health_vs_year_ago
        FROM MCBS&yr..SURVEY_GENHLTH_&yr ;
    QUIT;

    /* ---- Sample weights + variance design (CENWGTS) ----        */
    /* Continuously-enrolled annual weight: CS1YRWGT in 2015-2016,  */
    /* CEYRSWGT in 2017-2018. Both are "Continuously enrolled full  */
    /* sample weight" per their codebook labels — same construct,   */
    /* renamed mid-decade. Resolved via &weight_var (set above).   */
    PROC SQL;
        CREATE TABLE WORK.cenwgts_&yr AS
        SELECT
            BASE_ID,
            &weight_var                 AS wgt_full_sample,
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
            h.H_MAFF01, h.H_MAFF02, h.H_MAFF03, h.H_MAFF04,
            h.H_MAFF05, h.H_MAFF06, h.H_MAFF07, h.H_MAFF08,
            h.H_MAFF09, h.H_MAFF10, h.H_MAFF11, h.H_MAFF12,
            mp.madv_self_report,
            mp.madv_years_enrolled,
            mp.madv_annual_premium,
            mr.medicare_easy_understand,
            mr.medicare_self_knowledge,
            mr.easy_compare_options,
            mr.tried_find_info,
            mr.visited_medicare_site,
            mr.called_800_medicare,
            mr.who_decides_insurance,
            mr.has_personal_computer,
            mr.uses_internet_for_info,
            mr.net_info_via_other,
            mr.book_received,
            mr.book_read_amount,
            mr.book_understood,
            mr.aware_800_medicare,
            ht.has_esi,
            ht.has_va,
            ht.has_tricare,
            ht.has_rds,
            ht.has_self_purch,
            ht.has_ma_row,
            ht.ma_obtained_directly,
            ht.ma_obtained_inst,
            g.srh,
            g.health_vs_year_ago,
            w.wgt_full_sample,
            w.variance_stratum,
            w.variance_psu
        FROM        WORK.demo_&yr     AS d
        LEFT JOIN   WORK.hisumry_&yr  AS h  ON d.BASE_ID = h.BASE_ID
        LEFT JOIN   WORK.maplanqx_&yr AS mp ON d.BASE_ID = mp.BASE_ID
        LEFT JOIN   WORK.mcreplnq_&yr AS mr ON d.BASE_ID = mr.BASE_ID
        LEFT JOIN   WORK.hitline_&yr  AS ht ON d.BASE_ID = ht.BASE_ID
        LEFT JOIN   WORK.genhlth_&yr  AS g  ON d.BASE_ID = g.BASE_ID
        LEFT JOIN   WORK.cenwgts_&yr  AS w  ON d.BASE_ID = w.BASE_ID ;
    QUIT;
    %row_count(WORK.mcbs_&yr, mcbs joined &yr);

    /* Clean up segment tables */
    PROC DELETE DATA=WORK.demo_&yr;     RUN;
    PROC DELETE DATA=WORK.hisumry_&yr;  RUN;
    PROC DELETE DATA=WORK.maplanqx_&yr; RUN;
    PROC DELETE DATA=WORK.mcreplnq_&yr; RUN;
    PROC DELETE DATA=WORK.hitline_&yr;  RUN;
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
/* Compute derived indicators once across all years so they're   */
/* consistent. Note that not_esrd / full_year_partAB are now     */
/* placeholders — they get filled in script 3 once we have the   */
/* MBSF Part A/B months and ESRD info via the bene-panel join.   */
/* ============================================================ */

%MACRO stack_mcbs;
DATA PL027710.mcbs_panel;
    LENGTH ruca $ 8;     /* force consistent length across stacked years */
    SET
    %DO yr = &mcbs_start %TO &mcbs_end;
        WORK.mcbs_&yr
    %END;
    ;

    /* ESRD-exclusion via Medicare status (10/20 = age, 11/21/31 = ESRD) */
    not_esrd = (medstatus IN ("10","20"));

    /* Derived: ever-MA in the year (admin-source monthly flags).      */
    /* H_MAFF<MM> values on the seat: "MA" (enrolled in MA),           */
    /* "FF" (FFS), "NO" (not enrolled). Verified 2026-05-05.           */
    ARRAY maff[12] $ H_MAFF01-H_MAFF12;
    ma_months = 0;
    DO i = 1 TO 12;
        IF maff[i] = "MA" THEN ma_months + 1;
    END;
    is_ma_admin = (ma_months > 0);
    is_ffs_admin = (ma_months = 0);

    /* Derived: search behavior — any direct evidence of search.        */
    /* CMPRPLN was post-2018, so the index is built from the three      */
    /* search items that exist 2015-2018: tried-find-info, visited-     */
    /* medicare-site, called-800-medicare.                              */
    searched =
        (tried_find_info       = 1) OR
        (visited_medicare_site = 1) OR
        (called_800_medicare   = 1) ;

    /* Derived: ever-dual in the year */
    ARRAY dual[12] $ H_DUAL01-H_DUAL12;
    dual_months = 0;
    DO i = 1 TO 12;
        IF dual[i] NOT IN ("", "NA") THEN dual_months + 1;
    END;
    is_dual_ever = (dual_months > 0);

    /* Internet flag: 2015-2018 has KNETPERS (uses internet for info), */
    /* not the post-2022 INTERNET (access). KNETPERS = 1 implies       */
    /* access; we treat it as the closest 2015-2018 proxy.             */
    has_internet = (uses_internet_for_info = 1);

    /* Active-shopper flag (used downstream to restrict analysis        */
    /* sample): bene has no institutional non-Medicare coverage AND,    */
    /* if MA, did not obtain MA via institutional channel. Note that   */
    /* the SAS data-build does NOT apply this filter; it's applied in   */
    /* code/analysis/vrdc/1-load-bene-panel.R. Diagnostic only here.    */
    inst_coverage = (has_esi = 1 OR has_va = 1 OR has_tricare = 1 OR has_rds = 1);
    active_shopper =
        ( inst_coverage  = 0 )
        AND ( ma_obtained_inst = 0 ) ;

    DROP i;
RUN;
%MEND stack_mcbs;
%stack_mcbs;

%row_count(PL027710.mcbs_panel, MCBS panel stacked);


/* ============================================================ */
/* 2d. Diagnostics                                                */
/* ============================================================ */

TITLE "MCBS panel — counts by year";
PROC SQL;
    SELECT
        year,
        COUNT(*)                              AS n,
        SUM(is_ma_admin)                      AS n_ma,
        MEAN(is_ma_admin)                     AS pct_ma          FORMAT=PERCENT8.1,
        MEAN(searched)                        AS pct_searched    FORMAT=PERCENT8.1,
        MEAN(visited_medicare_site = 1)       AS pct_visited     FORMAT=PERCENT8.1,
        MEAN(tried_find_info = 1)             AS pct_tried_info  FORMAT=PERCENT8.1,
        MEAN(has_internet = 1)                AS pct_internet    FORMAT=PERCENT8.1,
        MEAN(is_dual_ever)                    AS pct_dual        FORMAT=PERCENT8.1
    FROM PL027710.mcbs_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;

TITLE "Active-shopper sample restriction — counts by year";
PROC SQL;
    SELECT
        year,
        COUNT(*)                              AS n_total,
        SUM(has_esi)                          AS n_esi,
        SUM(has_va)                           AS n_va,
        SUM(has_tricare)                      AS n_tricare,
        SUM(has_rds)                          AS n_rds,
        SUM(inst_coverage)                    AS n_any_inst,
        SUM(ma_obtained_inst)                 AS n_ma_inst,
        SUM(ma_obtained_directly)             AS n_ma_direct,
        SUM(active_shopper)                   AS n_active,
        MEAN(active_shopper)                  AS pct_active FORMAT=PERCENT8.1
    FROM PL027710.mcbs_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;

TITLE "Non-missing on key model variables (full panel)";
PROC SQL;
    SELECT
        SUM(income_cat IS NOT NULL)             AS n_income,
        SUM(education_cat IS NOT NULL)          AS n_education,
        SUM(uses_internet_for_info IS NOT NULL) AS n_internet,
        SUM(srh IS NOT NULL)                    AS n_srh,
        SUM(adi_raw IS NOT NULL)                AS n_adi,
        SUM(poverty_ratio_ind IS NOT NULL)      AS n_ipr_ind,
        COUNT(*)                                AS n_total
    FROM PL027710.mcbs_panel
    WHERE not_esrd = 1;
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
