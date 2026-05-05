/* ------------------------------------------------------------ */
/* TITLE:        Build final bene-year analysis panel            */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        PL027710.mcbs_panel                             */
/*               PL027710.bene_mbsf_panel                        */
/*               MCBSXWLK.MCBSXWLK   (BASE_ID -> BENE_ID xwalk)   */
/* OUTPUT:       PL027710.bene_panel                             */
/* ------------------------------------------------------------ */
/* Joins MCBS respondents (BASE_ID, year) to MBSF (BENE_ID, year) */
/* via the MCBSXWLK crosswalk. Attaches:                         */
/*   - this year's annual contract+PBP (from script 1)           */
/*   - last year's annual contract+PBP (lagged within MBSF)      */
/*   - bene-specific incumbent flag                              */
/* Output is one row per MCBS respondent-year, ready for export. */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* 3a. Crosswalk MCBSXWLK.MCBSXWLK : BASE_ID -> BENE_ID          */
/* ------------------------------------------------------------ */
/* Single dataset, one row per beneficiary (stable mapping —     */
/* BASE_ID and BENE_ID are both per-bene IDs that don't vary by  */
/* year, so no year column is needed in the join).               */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.xwalk AS
    SELECT BASE_ID, BENE_ID
    FROM MCBSXWLK.MCBSXWLK ;
QUIT;
%row_count(WORK.xwalk, xwalk);


/* ============================================================ */
/* 3b. Build a 1-year-lagged view of MBSF                        */
/* ------------------------------------------------------------ */
/* For each (BENE_ID, year) we want last year's annual contract  */
/* and PBP. Shift the year forward by 1 so that joining on       */
/* (BENE_ID, year) gives last year's plan.                       */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.mbsf_lag1 AS
    SELECT
        BENE_ID,
        (year + 1)                AS year,
        ann_contract              AS prior_contract,
        ann_pbp                   AS prior_pbp,
        is_ffs                    AS prior_was_ffs
    FROM PL027710.bene_mbsf_panel
    WHERE year BETWEEN &mbsf_start AND (&mbsf_end - 1) ;
QUIT;


/* ============================================================ */
/* 3c. Join MBSF (this year) + MBSF (lag) on BENE_ID×year        */
/* ============================================================ */

PROC SQL;
    CREATE TABLE WORK.mbsf_with_lag AS
    SELECT
        m.BENE_ID,
        m.year,
        m.state_cnty_fips,
        m.moved_within_year,
        m.zip_cd                AS mbsf_zip_cd,
        m.age                   AS age_mbsf,
        m.sex_cd                AS sex_cd_mbsf,
        m.race_cd               AS race_cd_mbsf,
        m.dual_mons             AS dual_mons_mbsf,
        m.hmo_mons              AS hmo_mons_mbsf,
        m.partA_mons,
        m.partB_mons,
        m.is_ffs                AS is_ffs_mbsf,
        m.ann_contract,
        m.ann_pbp,
        m.switched_within_year,
        l.prior_contract,
        l.prior_pbp,
        l.prior_was_ffs,
        CASE
            WHEN m.is_ffs = 0
                AND l.prior_contract = m.ann_contract
                AND l.prior_pbp      = m.ann_pbp
            THEN 1 ELSE 0
        END                     AS incumbent_bene
    FROM PL027710.bene_mbsf_panel AS m
    LEFT JOIN WORK.mbsf_lag1 AS l
        ON m.BENE_ID = l.BENE_ID
        AND m.year   = l.year ;
QUIT;
%row_count(WORK.mbsf_with_lag, MBSF + lag);


/* ============================================================ */
/* 3d. MCBS panel + xwalk + MBSF                                  */
/* ------------------------------------------------------------ */
/* MCBS spine -> attach BENE_ID -> attach MBSF/lag.              */
/* Use LEFT JOIN throughout so unmatched respondents survive but */
/* are flagged via link_status for diagnostic review.            */
/* ============================================================ */

PROC SQL;
    CREATE TABLE PL027710.bene_panel AS
    SELECT
        /* MCBS side — survey + admin + derived items */
        mc.BASE_ID,
        mc.year,

        /* Identifiers from xwalk */
        x.BENE_ID,

        /* Demographics */
        mc.age,
        mc.sex_cd,
        mc.race_cd,
        mc.hispanic,
        mc.education_cat,
        mc.income_cat,
        mc.income_continuous,
        mc.poverty_ratio_ind,
        mc.marital_cat,
        mc.state_ssa,
        mc.county_ssa,
        mc.zip_cd,
        mc.ruca,
        mc.cbsa_type,
        mc.adi_raw,                 /* ADI 2015 / CENSADI 2016-17 / ADINATNL 2018 — different scales */

        /* Insurance / coverage */
        mc.medstatus,
        mc.dual_annual,
        mc.is_dual_ever,
        mc.ma_months,
        mc.is_ma_admin,
        mc.is_ffs_admin,
        mc.not_esrd,
        mc.madv_self_report,
        mc.madv_years_enrolled,
        mc.madv_annual_premium,

        /* Channel flags (HITLINE) — for active-shopper sample */
        mc.has_esi,
        mc.has_va,
        mc.has_tricare,
        mc.has_rds,
        mc.has_self_purch,
        mc.has_ma_row,
        mc.ma_obtained_directly,
        mc.ma_obtained_inst,
        mc.inst_coverage,
        mc.active_shopper,

        /* Search behavior + internet (the main behavioral inputs) */
        mc.has_internet,
        mc.uses_internet_for_info,
        mc.has_personal_computer,
        mc.medicare_easy_understand,
        mc.medicare_self_knowledge,
        mc.easy_compare_options,
        mc.tried_find_info,
        mc.visited_medicare_site,
        mc.called_800_medicare,
        mc.who_decides_insurance,
        mc.net_info_via_other,
        mc.book_received,
        mc.book_read_amount,
        mc.book_understood,
        mc.aware_800_medicare,
        mc.searched,

        /* Health */
        mc.srh,
        mc.health_vs_year_ago,

        /* Weights / variance design */
        mc.wgt_full_sample,
        mc.variance_stratum,
        mc.variance_psu,

        /* MBSF side — FIPS geography + annual plan ID + lag.           */
        /* partA_mons / partB_mons come from MBSF (BENE_HI_CVRAGE_TOT_  */
        /* MONS / BENE_SMI_CVRAGE_TOT_MONS); they are NOT in MCBS       */
        /* 2015-2018 (no MYENROLL segment in that era).                  */
        b.state_cnty_fips,
        b.moved_within_year,
        b.age_mbsf,
        b.sex_cd_mbsf,
        b.race_cd_mbsf,
        b.dual_mons_mbsf,
        b.hmo_mons_mbsf,
        b.partA_mons,
        b.partB_mons,
        CASE WHEN b.partA_mons = 12 AND b.partB_mons = 12 THEN 1 ELSE 0 END
                                       AS full_year_partAB,
        b.is_ffs_mbsf,
        b.ann_contract,
        b.ann_pbp,
        b.switched_within_year,
        b.prior_contract,
        b.prior_pbp,
        b.prior_was_ffs,
        b.incumbent_bene,

        /* Linkage diagnostic */
        CASE
            WHEN x.BENE_ID IS NULL                              THEN "no_xwalk"
            WHEN b.BENE_ID IS NULL                              THEN "no_mbsf"
            WHEN b.is_ffs_mbsf = 1 AND mc.is_ma_admin  = 1      THEN "mismatch_mcbs_ma_mbsf_ffs"
            WHEN b.is_ffs_mbsf = 0 AND mc.is_ffs_admin = 1      THEN "mismatch_mcbs_ffs_mbsf_ma"
            ELSE "ok"
        END                       AS link_status

    FROM PL027710.mcbs_panel AS mc
    LEFT JOIN WORK.xwalk            AS x  ON mc.BASE_ID  = x.BASE_ID
    LEFT JOIN WORK.mbsf_with_lag    AS b  ON x.BENE_ID  = b.BENE_ID
                                          AND mc.year    = b.year ;
QUIT;
%row_count(PL027710.bene_panel, bene panel);


/* ============================================================ */
/* 3e. Diagnostics                                                */
/* ============================================================ */

TITLE "Bene panel — link status by year";
PROC SQL;
    SELECT year, link_status, COUNT(*) AS n
    FROM PL027710.bene_panel
    GROUP BY year, link_status
    ORDER BY year, link_status;
QUIT;
TITLE;

TITLE "Bene panel — counts by year and FFS/MA, after sample restrictions";
PROC SQL;
    SELECT year,
           COUNT(*)                                    AS n_total,
           SUM(full_year_partAB AND not_esrd)          AS n_inscope,
           SUM(full_year_partAB AND not_esrd AND
               link_status = "ok")                     AS n_clean,
           SUM(full_year_partAB AND not_esrd AND is_ffs_mbsf = 1) AS n_ffs,
           SUM(full_year_partAB AND not_esrd AND is_ffs_mbsf = 0) AS n_ma,
           SUM(full_year_partAB AND not_esrd AND incumbent_bene = 1) AS n_incumbent
    FROM PL027710.bene_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up                                                       */
/* ============================================================ */

PROC DELETE DATA=WORK.xwalk;          RUN;
PROC DELETE DATA=WORK.mbsf_lag1;      RUN;
PROC DELETE DATA=WORK.mbsf_with_lag;  RUN;
