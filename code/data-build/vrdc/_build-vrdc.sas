/* ------------------------------------------------------------ */
/* TITLE:        VRDC data-build master driver                   */
/* PROJECT:      ma-search-costs                                 */
/* PURPOSE:      Run the full extraction pipeline end-to-end.    */
/* ------------------------------------------------------------ */
/* Sourced from /workspace/pl027710/code/data-build/vrdc/.       */
/*                                                                */
/* Pipeline:                                                      */
/*   1. _config.sas              libnames, year ranges, macros   */
/*   2. 1-extract-mbsf.sas       MBSF 2014-2018 panel            */
/*   3. 2-extract-mcbs.sas       MCBS 2015-2018 multi-segment    */
/*   4. 3-build-bene-panel.sas   MCBS x MCBSXWLK x MBSF + lag    */
/*   5. 4-export-bene-panel.sas  CSV for R analysis              */
/* ------------------------------------------------------------ */

%LET project_root = /workspace/pl027710/code/data-build/vrdc;

%INCLUDE "&project_root/_config.sas";

%INCLUDE "&project_root/1-extract-mbsf.sas";
%INCLUDE "&project_root/2-extract-mcbs.sas";
%INCLUDE "&project_root/3-build-bene-panel.sas";
%INCLUDE "&project_root/4-export-bene-panel.sas";

%PUT NOTE: VRDC data-build complete ;
