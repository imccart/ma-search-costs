/* ------------------------------------------------------------ */
/* TITLE:        VRDC config — libnames, year ranges, macros    */
/* PROJECT:      ma-search-costs                                 */
/* AUTHOR:       Ian McCarthy / Emory University                 */
/* PURPOSE:      Global parameters for the structural-model     */
/*               extraction pipeline. Sourced first by every    */
/*               extraction script.                             */
/* ------------------------------------------------------------ */
/* DUA: RSCH-2015-27710. Approved data 2015-2018 MCBS Survey +   */
/* Cost Supplement, 2007-2018 MBSF, plus MCBSXWLK BASE_ID-BENE_ID*/
/* crosswalk and MCBS 2007-2013 legacy (out of scope for v1).    */
/* See background/vrdc-plan.md for the full extraction plan.    */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* Library references                                            */
/* ------------------------------------------------------------ */
/* All libraries — both CMS-side data and the writable project  */
/* library — are auto-mounted by the VRDC seat's startup. User  */
/* code references them directly without LIBNAME statements:    */
/*                                                                */
/*   PL027710.<dataset>           (writable project library)     */
/*   MBSF.MBSF_ABCD_<yr>                                          */
/*   MCBS<yr>.SURVEY_<SEGMENT>_<yr>                               */
/*   MCBS<yr>.COSTUSE_<SEGMENT>_<yr>                              */
/*   MCBSXWLK.MCBSXWLK             (BASE_ID -> BENE_ID xwalk)     */
/*   ENRFPL<yr>.<svc>_BASE_ENC                                    */
/*   ENRFPL<yr>.<svc>_LINE_ENC     (carrier, dme)                 */
/*   ENRFPL<yr>.<svc>_REVENUE_ENC  (op, snf, hha, ip)             */
/*       <svc> in {IP, SNF, HHA, OP, CARRIER, DME}, <yr> 15..18   */
/*   MEDPAR.MEDPAR_<yyyy>           (FFS IP+SNF stays, annual)    */
/*   RIF<yyyy>.HHA_CLAIMS_<MM>     +HHA_REVENUE_<MM>              */
/*   RIF<yyyy>.OUTPATIENT_CLAIMS_<MM> +OUTPATIENT_REVENUE_<MM>    */
/*   RIF<yyyy>.BCARRIER_LINE_<MM>                                 */
/*       MM in 01..12, yyyy in 2015..2018                         */
/*                                                                */
/* Do NOT add any LIBNAME statements. If a library appears       */
/* undefined in the SAS log, that's a seat-config issue — fix    */
/* it on the seat side, not in user code.                        */
/* ============================================================ */


/* ============================================================ */
/* Year ranges                                                   */
/* ============================================================ */

/* Primary MCBS sample: 2015-2018 (post-redesign, single survey  */
/* instrument across years). Legacy 2007-2013 is out of scope    */
/* for v1; see background/vrdc-plan.md §3.                       */
%LET mcbs_start = 2015;
%LET mcbs_end   = 2018;

/* MBSF year range. Pull one year before mcbs_start so we have   */
/* the lagged contract+PBP for the bene-specific incumbent flag. */
%LET mbsf_start = 2014;
%LET mbsf_end   = 2018;


/* ============================================================ */
/* Staging folder for the CSV export from PROC EXPORT            */
/* ------------------------------------------------------------ */
/* This is a filesystem path inside the VRDC seat where SAS      */
/* writes `bene_panel.csv` so the R analysis pipeline can read   */
/* it. It is NOT the off-VRDC clearance-export route (that is    */
/* manual via CMS protocol).                                     */
/*                                                                */
/* PL027710 is the SAS library, not a filesystem path. Code,     */
/* CSV staging, and the SAS library can all live in different    */
/* directories on the seat. Edit the path below to match the     */
/* writable folder you want to stage the CSV in.                 */
/* ============================================================ */
%LET export_dir = /workspace/pl027710/export;


/* ============================================================ */
/* Sample-restriction macros (re-used across scripts)            */
/* ============================================================ */

/* Aged-in (65+) Medicare with full-year Part A and Part B.      */
/* We do NOT require zero HMO months — MA enrollees are the      */
/* inside option of interest.                                    */
%MACRO mbsf_age65_filter;
    AGE_AT_END_REF_YR >= 65
    AND BENE_HI_CVRAGE_TOT_MONS  = 12
    AND BENE_SMI_CVRAGE_TOT_MONS = 12
%MEND mbsf_age65_filter;


/* ============================================================ */
/* Utility — modal contract+PBP across 12 monthly columns        */
/* ------------------------------------------------------------ */
/* The MBSF Base segment carries PTC_CNTRCT_ID_01..12 and        */
/* PTC_PBP_ID_01..12. We need a single annual plan assignment    */
/* per beneficiary. Approach: take the December value if         */
/* non-blank, else the first non-blank month. If all 12 are      */
/* blank/0, the bene is FFS for that year.                       */
/* ============================================================ */

%MACRO modal_partc(in_ds, out_ds);
    DATA &out_ds;
        SET &in_ds;

        ARRAY cntrct[12] $ PTC_CNTRCT_ID_01-PTC_CNTRCT_ID_12;
        ARRAY pbp[12]    $ PTC_PBP_ID_01-PTC_PBP_ID_12;

        LENGTH ann_contract $ 5 ann_pbp $ 3;
        ann_contract = "";
        ann_pbp      = "";

        /* Count months with any MA assignment */
        ma_months = 0;
        DO i = 1 TO 12;
            IF cntrct[i] NOT IN ("", "0", "N") THEN ma_months + 1;
        END;

        /* Take December if non-blank, else first non-blank */
        IF cntrct[12] NOT IN ("", "0", "N") THEN DO;
            ann_contract = cntrct[12];
            ann_pbp      = pbp[12];
        END;
        ELSE DO i = 1 TO 12;
            IF ann_contract = "" AND cntrct[i] NOT IN ("", "0", "N") THEN DO;
                ann_contract = cntrct[i];
                ann_pbp      = pbp[i];
            END;
        END;

        /* FFS flag: zero MA months */
        is_ffs = (ma_months = 0);

        /* Switcher flag: changed contract within the year */
        switched_within_year = 0;
        DO i = 2 TO 12;
            IF cntrct[i] NE "" AND cntrct[i-1] NE "" AND cntrct[i] NE cntrct[i-1] THEN
                switched_within_year = 1;
        END;

        DROP i;
    RUN;
%MEND modal_partc;


/* ============================================================ */
/* Logging — print row counts after each step                    */
/* ============================================================ */
%MACRO row_count(ds, label);
    PROC SQL NOPRINT;
        SELECT COUNT(*) INTO :nrows FROM &ds;
    QUIT;
    %PUT NOTE: &label rows = &nrows;
%MEND row_count;
