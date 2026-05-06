/* ------------------------------------------------------------ */
/* TITLE:        VRDC data-build master driver (optional)        */
/* PROJECT:      ma-search-costs                                 */
/* PURPOSE:      Run the full extraction pipeline end-to-end.    */
/* ------------------------------------------------------------ */
/* This driver is OPTIONAL. The recommended SAS Enterprise Guide */
/* workflow is to open and run each script individually in       */
/* order: _config.sas first, then 1, 2, 3, 4. Each numbered      */
/* script is standalone and references libraries directly        */
/* (PL027710, MBSF, MCBS<yr>, MCBSXWLK) — no relative paths or   */
/* %INCLUDE chains.                                              */
/*                                                                */
/* Use this driver only if you want a single batch run via       */
/* `sas -SYSIN _build-vrdc.sas` or equivalent. Edit the          */
/* `code_dir` macro below to match the filesystem location       */
/* where you placed the .sas scripts on the seat (NOT the        */
/* PL027710 library — code and library can live in different     */
/* directories).                                                  */
/*                                                                */
/* Pipeline:                                                      */
/*   1. _config.sas                  libnames, year ranges, macros */
/*   2. 1-extract-mbsf.sas           MBSF 2014-2018 panel          */
/*   3. 2-extract-mcbs.sas           MCBS 2015-2018 multi-segment  */
/*   4. 3-build-bene-panel.sas       MCBS x MCBSXWLK x MBSF + lag  */
/*   5. 4-extract-ma-encounters.sas  MA encounter utilization      */
/*   6. 5-extract-ffs-claims.sas     FFS claims utilization + obs C*/
/*                                                                  */
/* CSV export off the seat is done manually via the CMS clearance  */
/* protocol — there is no in-pipeline export script.                */
/* ------------------------------------------------------------ */

/* Edit this to match where you placed the .sas files on the seat.   */
/* This is the filesystem path to the code directory, NOT the SAS    */
/* library PL027710 (which is auto-mounted and lives elsewhere).     */
%LET project_root = /your/seat/path/to/scripts;

%INCLUDE "&project_root/_config.sas";

%INCLUDE "&project_root/1-extract-mbsf.sas";
%INCLUDE "&project_root/2-extract-mcbs.sas";
%INCLUDE "&project_root/3-build-bene-panel.sas";
%INCLUDE "&project_root/4-extract-ma-encounters.sas";
%INCLUDE "&project_root/5-extract-ffs-claims.sas";

%PUT NOTE: VRDC data-build complete ;
