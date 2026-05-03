/* ------------------------------------------------------------ */
/* TITLE:        Export bene panel as CSV for R analysis         */
/* PROJECT:      ma-search-costs                                 */
/* INPUT:        PL027710.bene_panel                             */
/* OUTPUT:       &export_dir/bene_panel.csv                      */
/* ------------------------------------------------------------ */
/* Writes the analysis-ready bene panel to CSV for the R-side    */
/* GMM pipeline (code/analysis/vrdc/). One row per MCBS          */
/* respondent-year. CMS clearance is required to remove the      */
/* CSV from the VRDC enclave; this export goes to a project-     */
/* internal staging folder, not directly off-VRDC.               */
/* ------------------------------------------------------------ */


/* ============================================================ */
/* 4a. Apply the standard analysis-sample filter                 */
/* ------------------------------------------------------------ */
/* Apply the model-sample filters here so the exported CSV is   */
/* the actual estimation sample. Document any further filters   */
/* applied on the R side in code/analysis/vrdc/1-load-bene.R.   */
/* ============================================================ */

DATA WORK.export;
    SET PL027710.bene_panel;
    WHERE full_year_partAB = 1
      AND not_esrd          = 1
      AND link_status      IN ("ok",
                               "mismatch_mcbs_ma_mbsf_ffs",
                               "mismatch_mcbs_ffs_mbsf_ma") ;
    /* keep mismatches: they're informative about admin-vs-survey
       discrepancy and we want them visible in R diagnostics.    */
RUN;
%row_count(WORK.export, export sample);


/* ============================================================ */
/* 4b. Write CSV                                                  */
/* ============================================================ */

%LET out_path = &export_dir./bene_panel.csv;

PROC EXPORT
    DATA    = WORK.export
    OUTFILE = "&out_path"
    DBMS    = CSV
    REPLACE ;
RUN;

%PUT NOTE: Wrote &out_path ;


/* ============================================================ */
/* 4c. Companion: variable dictionary                            */
/* ============================================================ */

%LET dict_path = &export_dir./bene_panel_dictionary.csv;

PROC CONTENTS DATA=PL027710.bene_panel
    OUT  = WORK.dict (KEEP=NAME TYPE LENGTH FORMAT LABEL)
    NOPRINT ;
RUN;

PROC EXPORT
    DATA    = WORK.dict
    OUTFILE = "&dict_path"
    DBMS    = CSV
    REPLACE ;
RUN;

%PUT NOTE: Wrote &dict_path ;


/* ============================================================ */
/* Clean up                                                       */
/* ============================================================ */

PROC DELETE DATA=WORK.export; RUN;
PROC DELETE DATA=WORK.dict;   RUN;
