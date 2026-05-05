/* ------------------------------------------------------------ */
/* TITLE:        MBSF extraction — beneficiary panel             */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        MBSF.MBSF_ABCD_&yr  for &yr in 2014..2018       */
/* OUTPUT:       PL027710.bene_mbsf_panel                        */
/* ------------------------------------------------------------ */
/* Pulls one row per BENE_ID per year, with demographics,       */
/* coverage months, and the modal annual Part C contract+PBP    */
/* (or FFS flag). We pull MBSF for 2014-2018 — one year before  */
/* the 2015 estimation start so we can compute year-over-year   */
/* incumbent flags at the bene level for all 4 estimation years.*/
/* ------------------------------------------------------------ */

/* Variable names verified against ResDAC MBSF Base data        */
/* documentation as of 2026-05-03. Confirmed names:             */
/*   BENE_ID, BENE_ENROLLMT_REF_YR, AGE_AT_END_REF_YR,          */
/*   SEX_IDENT_CD, BENE_RACE_CD, ZIP_CD, BENE_DEATH_DT,         */
/*   COVSTART, DUAL_ELGBL_MONS, BENE_HI_CVRAGE_TOT_MONS,        */
/*   BENE_SMI_CVRAGE_TOT_MONS, BENE_HMO_CVRAGE_TOT_MONS,        */
/*   PTC_CNTRCT_ID_01..12, PTC_PBP_ID_01..12,                   */
/*   HMO_IND_01..12.                                            */
/* TODO at first run: verify state+county field. Likely         */
/* STATE_CNTY_FIPS_CD (5-digit) or split STATE_CD + CNTY_CD.    */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* 1a. Pull MBSF per year                                        */
/* ============================================================ */

%MACRO pull_mbsf(yr);
    PROC SQL;
        CREATE TABLE WORK.mbsf_&yr AS
        SELECT
            BENE_ID,
            BENE_ENROLLMT_REF_YR             AS year,
            AGE_AT_END_REF_YR                AS age,
            SEX_IDENT_CD                     AS sex_cd,
            BENE_RACE_CD                     AS race_cd,
            ZIP_CD                           AS zip_cd,

            /* Geography. STATE_CODE + COUNTY_CD are SSA-coded     */
            /* annual residence as of 12/31. (NB: codebook docs    */
            /* show STATE_CD but the actual seat field name is     */
            /* STATE_CODE — verified on seat 2026-05-05.)          */
            /* STATE_CNTY_FIPS_CD_<MM> are monthly FIPS codes (the */
            /* actual FIPS our public structural_panel.csv joins   */
            /* on). Use the December monthly value as the annual   */
            /* FIPS; pull a few other months to flag within-year   */
            /* moves.                                              */
            STATE_CODE                       AS state_cd_ssa,
            COUNTY_CD                        AS county_cd_ssa,
            STATE_CNTY_FIPS_CD_12            AS state_cnty_fips,
            STATE_CNTY_FIPS_CD_01            AS state_cnty_fips_jan,
            STATE_CNTY_FIPS_CD_06            AS state_cnty_fips_jun,

            BENE_DEATH_DT                    AS death_dt,
            COVSTART                         AS coverage_start,
            DUAL_ELGBL_MONS                  AS dual_mons,
            BENE_HI_CVRAGE_TOT_MONS          AS partA_mons,
            BENE_SMI_CVRAGE_TOT_MONS         AS partB_mons,
            BENE_HMO_CVRAGE_TOT_MONS         AS hmo_mons,

            /* Monthly Part C — kept for the modal_partc macro   */
            PTC_CNTRCT_ID_01, PTC_CNTRCT_ID_02, PTC_CNTRCT_ID_03,
            PTC_CNTRCT_ID_04, PTC_CNTRCT_ID_05, PTC_CNTRCT_ID_06,
            PTC_CNTRCT_ID_07, PTC_CNTRCT_ID_08, PTC_CNTRCT_ID_09,
            PTC_CNTRCT_ID_10, PTC_CNTRCT_ID_11, PTC_CNTRCT_ID_12,
            PTC_PBP_ID_01,    PTC_PBP_ID_02,    PTC_PBP_ID_03,
            PTC_PBP_ID_04,    PTC_PBP_ID_05,    PTC_PBP_ID_06,
            PTC_PBP_ID_07,    PTC_PBP_ID_08,    PTC_PBP_ID_09,
            PTC_PBP_ID_10,    PTC_PBP_ID_11,    PTC_PBP_ID_12

        FROM MBSF.MBSF_ABCD_&yr
        WHERE %mbsf_age65_filter ;
    QUIT;

    /* Flag bene-years that moved counties within the year       */
    DATA WORK.mbsf_&yr;
        SET WORK.mbsf_&yr;
        moved_within_year =
            (state_cnty_fips_jan NE state_cnty_fips)
         OR (state_cnty_fips_jun NE state_cnty_fips);
    RUN;

    /* Collapse the monthly Part C arrays into ann_contract,     */
    /* ann_pbp, is_ffs, ma_months, switched_within_year.         */
    %modal_partc(WORK.mbsf_&yr, WORK.mbsf_collapsed_&yr);

    /* Drop the monthly columns now that we have annual fields.  */
    DATA WORK.mbsf_collapsed_&yr;
        SET WORK.mbsf_collapsed_&yr;
        DROP PTC_CNTRCT_ID_01-PTC_CNTRCT_ID_12
             PTC_PBP_ID_01-PTC_PBP_ID_12;
    RUN;

    %row_count(WORK.mbsf_collapsed_&yr, MBSF &yr collapsed);
%MEND pull_mbsf;

%MACRO pull_all_mbsf;
    %DO yr = &mbsf_start %TO &mbsf_end;
        %pull_mbsf(&yr);
    %END;
%MEND pull_all_mbsf;
%pull_all_mbsf;


/* ============================================================ */
/* 1b. Stack into a single bene-year panel                       */
/* ============================================================ */

%MACRO stack_mbsf;
    DATA PL027710.bene_mbsf_panel;
        SET
        %DO yr = &mbsf_start %TO &mbsf_end;
            WORK.mbsf_collapsed_&yr
        %END;
        ;
    RUN;
%MEND stack_mbsf;
%stack_mbsf;

%row_count(PL027710.bene_mbsf_panel, MBSF stacked panel);


/* ============================================================ */
/* 1c. Diagnostics                                               */
/* ============================================================ */

TITLE "MBSF panel — counts by year and FFS/MA";
PROC SQL;
    SELECT year,
           COUNT(*)                          AS n_benes,
           SUM(is_ffs)                       AS n_ffs,
           SUM(1 - is_ffs)                   AS n_ma,
           MEAN(1 - is_ffs)                  AS pct_ma FORMAT=PERCENT8.1,
           SUM(switched_within_year)         AS n_switched
    FROM PL027710.bene_mbsf_panel
    GROUP BY year
    ORDER BY year;
QUIT;
TITLE;


/* ============================================================ */
/* Clean up WORK                                                 */
/* ============================================================ */

%MACRO cleanup_mbsf;
    %DO yr = &mbsf_start %TO &mbsf_end;
        PROC DELETE DATA=WORK.mbsf_&yr;            RUN;
        PROC DELETE DATA=WORK.mbsf_collapsed_&yr;  RUN;
    %END;
%MEND cleanup_mbsf;
%cleanup_mbsf;
