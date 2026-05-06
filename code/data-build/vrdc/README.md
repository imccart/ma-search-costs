# VRDC data-build (`code/data-build/vrdc/`)

SAS extraction pipeline for the structural-search MCBS analysis. Runs inside the CMS VRDC enclave under DUA RSCH-2015-27710 (Ian McCarthy as direct-access seat holder, Emory University).

## Run order

```
_config.sas
1-extract-mbsf.sas             MBSF 2014-2018 -> PL027710.bene_mbsf_panel
2-extract-mcbs.sas             MCBS 2015-2018 -> PL027710.mcbs_panel
3-build-bene-panel.sas         join via MCBSXWLK -> PL027710.bene_panel
4-extract-ma-encounters.sas    ENRFPL15-18 -> PL027710.ma_util_panel
5-extract-ffs-claims.sas       MEDPAR + RIF15-18 -> PL027710.ffs_util_panel
```

Use `_build-vrdc.sas` as the master driver. CSV export off the seat is done
manually via the CMS clearance protocol — there is no in-pipeline export script.

## VRDC seat conventions

- All libraries — CMS-side data and the writable project library `PL027710` — are auto-mounted by the seat startup.
- User code references them directly (`PL027710.<dataset>`, `MBSF.MBSF_ABCD_<yr>`, `MCBS<yr>.SURVEY_<SEGMENT>_<yr>`, `MCBSXWLK.<dataset>`).
- Do NOT add `LIBNAME` statements anywhere. If a library is reported as undefined in the SAS log, that's a seat-config issue — fix it on the seat side, not in user code.

## Variable name verification

- **MBSF Base segment**: variable names verified against ResDAC documentation as of 2026-05-03 (lab convention: `MBSF.MBSF_ABCD_<yr>`, fields `BENE_ID`, `AGE_AT_END_REF_YR`, `SEX_IDENT_CD`, `BENE_RACE_CD`, `BENE_HI_CVRAGE_TOT_MONS`, `BENE_SMI_CVRAGE_TOT_MONS`, `BENE_HMO_CVRAGE_TOT_MONS`, `STATE_CNTY_FIPS_CD`, `ZIP_CD`, `DUAL_ELGBL_MONS`, `PTC_CNTRCT_ID_01..12`, `PTC_PBP_ID_01..12`).
- **MCBS Survey File**: variable names verified against the local copy of the 2023 MCBS Survey File codebooks at `~/Downloads/mcbs-survey-file-extracted/Codebooks/`. The 2015–2018 schema matches per the CMS user guide; sanity-check the exact spellings on first run by looking at one PROC CONTENTS per segment.
- **MCBSXWLK**: dataset name and per-year vs. single-table structure NOT YET VERIFIED. On first run, do `PROC CONTENTS DATA=MCBSXWLK._ALL_; RUN;` and update the FROM clause in `3-build-bene-panel.sas` accordingly.

## On-disk output

After a successful run, the writable library `PL027710` holds:

```
PL027710.bene_mbsf_panel        MBSF 2014-2018 (one row per BENE_ID-year)
PL027710.mcbs_panel             MCBS 2015-2018 (one row per BASE_ID-year)
PL027710.bene_panel             MCBS x MBSF, lagged plan, incumbent flag
PL027710.ma_util_panel          MA encounter utilization (BENE_ID x year)
PL027710.ma_util_<svc>          per-service stacked panels (ip/snf/hha/op/car/dme)
PL027710.ffs_util_panel         FFS claims utilization + observed cost-share
PL027710.ffs_util_<svc>         per-service stacked panels (ipsnf/hha/op/car)
```

Datasets stay inside the VRDC enclave; CSV exports off-VRDC require CMS
clearance review and are done manually outside this pipeline.

## Approved DUA scope

- **MBSF** (Master Beneficiary Summary File), 2007–2018, rows 29–32 of DUA. We use 2014–2018 only.
- **MCBS Survey File + Cost Supplement** (post-redesign), 2015–2018, row 5.
- **MCBSXWLK** (BASE_ID ↔ BENE_ID crosswalk), 2007–2013 + 2023, rows 1–4.
- **MCBSAC** (Access to Care, legacy), 2007–2013, rows 40–42. Out of scope for v1.
- **MCBSCU** (Cost & Use, legacy), 2007–2010 and 2012–2013 (no 2011), rows 37–39. Out of scope for v1.

If the project ever expands to legacy 2007–2013 era, the August 2026 DUA renewal can patch the missing 2011 MCBSCU year; see `background/vrdc-plan.md` §3.

## Sample restrictions applied

- Community-dwelling MCBS respondents (`INT_TYPE IN ('C','B')`).
- Aged 65+ (`H_AGE >= 65`).
- Full-year Part A and Part B (`PTA_MONS = 12 AND PTB_MONS = 12`).
- Non-ESRD (`H_MEDSTA IN ('10','20')`).
- The export step also drops respondents with `link_status = 'no_xwalk'` or `'no_mbsf'` (no possible MBSF linkage).

## Dependencies / uploaded inputs

The R analysis side (`code/analysis/vrdc/`) requires `structural_panel.csv` (with prominence columns from script 14) to be uploaded from local. See `background/vrdc-upload-bundle.md`.
