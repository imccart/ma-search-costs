# Year-specific lists of CMS Star Ratings component measures, ported from
# research-projects/_future-ideas/ma-product-differentiation/analysis/rating_variables.R.
#
# CMS reshuffled component measures across years (some retired, others added),
# so each year has a tailored list. Used by 3-cluster.R, which intersects each
# year's list with the columns actually present in ma_data_YYYY.txt.

rating_vars <- list(
  `2008` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_cholscreen",
    "diabetes_cholscreen", "glaucoma_test", "pn_vaccine", "primaryaccess",
    "hospital_followup", "depression_followup", "nodelays", "carequickly",
    "overallrating_care", "overallrating_plan", "calltime",
    "doctor_communicate", "osteo_manage", "diabetes_eye", "diabetes_kidney",
    "diabetes_bloodsugar", "diabetes_chol", "antidepressant", "bloodpressure",
    "ra_manage", "copd_test", "betablocker", "appeals_timely", "appeals_review"
  ),

  `2009` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_cholscreen",
    "diabetes_cholscreen", "glaucoma_test", "monitoring", "flu_vaccine",
    "pn_vaccine", "physical_health", "mental_health", "osteo_test",
    "physical_monitor", "primaryaccess", "hospital_followup",
    "depression_followup", "nodelays", "carequickly", "overallrating_care",
    "overallrating_plan", "calltime", "doctor_communicate", "customer_service",
    "osteo_manage", "diabetes_eye", "diabetes_kidney", "diabetes_bloodsugar",
    "diabetes_chol", "antidepressant", "bloodpressure", "ra_manage",
    "copd_test", "betablocker", "bladder", "falling", "appeals_timely",
    "appeals_review"
  ),

  `2010` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_diab_cholscreen",
    "glaucoma_test", "monitoring", "flu_vaccine", "pn_vaccine",
    "physical_health", "mental_health", "osteo_test", "physical_monitor",
    "primaryaccess", "nodelays", "doctor_communicate", "carequickly",
    "customer_service", "overallrating_care", "overallrating_plan",
    "osteo_manage", "diab_healthy", "bloodpressure", "ra_manage", "copd_test",
    "betablocker", "bladder", "falling", "complaints_plan", "appeals_timely",
    "appeals_review", "leave_plan", "audit_problems", "hold_times",
    "info_accuracy", "ttyt_available"
  ),

  `2011` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_cholscreen",
    "diab_cholscreen", "glaucoma_test", "monitoring", "flu_vaccine",
    "pn_vaccine", "physical_health", "mental_health", "osteo_test",
    "physical_monitor", "primaryaccess", "osteo_manage", "diabetes_eye",
    "diabetes_kidney", "diabetes_bloodsugar", "diabetes_chol", "bloodpressure",
    "ra_manage", "copd_test", "bladder", "falling", "nodelays",
    "doctor_communicate", "carequickly", "customer_service",
    "overallrating_care", "overallrating_plan", "complaints_plan",
    "appeals_timely", "appeals_review", "corrective_action", "hold_times",
    "info_accuracy", "ttyt_available"
  ),

  `2012` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_cholscreen",
    "diab_cholscreen", "glaucoma_test", "flu_vaccine", "pn_vaccine",
    "physical_health", "mental_health", "physical_monitor", "primaryaccess",
    "bmi_assess", "older_medication", "older_function", "older_pain",
    "osteo_manage", "diabetes_eye", "diabetes_kidney", "diabetes_bloodsugar",
    "diabetes_chol", "bloodpressure", "ra_manage", "bladder", "falling",
    "readmissions", "nodelays", "carequickly", "customer_service",
    "overallrating_care", "overallrating_plan", "complaints_plan",
    "access_problems", "leave_plan", "appeals_timely", "appeals_review",
    "ttyt_available"
  ),

  `2013` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_cholscreen",
    "diab_cholscreen", "glaucoma_test", "flu_vaccine", "physical_health",
    "mental_health", "physical_monitor", "bmi_assess", "older_medication",
    "older_function", "older_pain", "osteo_manage", "diabetes_eye",
    "diabetes_kidney", "diabetes_bloodsugar", "diabetes_chol", "bloodpressure",
    "ra_manage", "bladder", "falling", "readmissions", "nodelays",
    "carequickly", "customer_service", "overallrating_care",
    "overallrating_plan", "coordination", "complaints_plan", "access_problems",
    "leave_plan", "improve", "appeals_timely", "appeals_review",
    "ttyt_available", "enroll_timely"
  ),

  `2014` = c(
    "breastcancer_screen", "rectalcancer_screen", "cv_cholscreen",
    "diab_cholscreen", "glaucoma_test", "flu_vaccine", "physical_health",
    "mental_health", "physical_monitor", "bmi_assess", "older_medication",
    "older_function", "older_pain", "osteo_manage", "diabetes_eye",
    "diabetes_kidney", "diabetes_bloodsugar", "diabetes_chol", "bloodpressure",
    "ra_manage", "bladder", "falling", "readmissions", "nodelays",
    "carequickly", "customer_service", "overallrating_care",
    "overallrating_plan", "coordination", "complaints_plan", "access_problems",
    "leave_plan", "improve", "appeals_timely", "appeals_review",
    "ttyt_available"
  ),

  `2015` = c(
    "rectalcancer_screen", "cv_cholscreen", "diab_cholscreen", "flu_vaccine",
    "physical_health", "mental_health", "physical_monitor", "bmi_assess",
    "specialneeds_manage", "older_medication", "older_function", "older_pain",
    "osteo_manage", "diabetes_eye", "diabetes_kidney", "diabetes_bloodsugar",
    "diabetes_chol", "bloodpressure", "ra_manage", "bladder", "falling",
    "readmissions", "nodelays", "carequickly", "customer_service",
    "overallrating_care", "overallrating_plan", "coordination",
    "complaints_plan", "leave_plan", "improve", "appeals_timely",
    "appeals_review"
  ),

  `2016` = c(
    "breastcancer_screen", "rectalcancer_screen", "flu_vaccine",
    "physical_health", "mental_health", "physical_monitor", "bmi_assess",
    "specialneeds_manage", "older_medication", "older_function", "older_pain",
    "osteo_manage", "diabetes_eye", "diabetes_kidney", "diabetes_bloodsugar",
    "bloodpressure", "ra_manage", "falling", "readmissions", "nodelays",
    "carequickly", "customer_service", "overallrating_care",
    "overallrating_plan", "coordination", "complaints_plan", "leave_plan",
    "access_problems", "improve", "appeals_timely", "appeals_review",
    "ttyt_available"
  ),

  `2017` = c(
    "breastcancer_screen", "rectalcancer_screen", "flu_vaccine",
    "physical_health", "mental_health", "physical_monitor", "bmi_assess",
    "specialneeds_manage", "older_medication", "older_function", "older_pain",
    "osteo_manage", "diabetes_eye", "diabetes_kidney", "diabetes_bloodsugar",
    "bloodpressure", "ra_manage", "falling", "readmissions", "nodelays",
    "carequickly", "customer_service", "overallrating_care",
    "overallrating_plan", "coordination", "complaints_plan", "leave_plan",
    "access_problems", "improve", "appeals_timely", "appeals_review",
    "ttyt_available"
  ),

  `2018` = c(
    "breastcancer_screen", "rectalcancer_screen", "flu_vaccine",
    "physical_health", "mental_health", "physical_monitor", "bmi_assess",
    "specialneeds_manage", "older_medication", "older_function", "older_pain",
    "osteo_manage", "diabetes_eye", "diabetes_kidney", "diabetes_bloodsugar",
    "bloodpressure", "ra_manage", "falling", "bladder", "medication",
    "readmissions", "nodelays", "carequickly", "customer_service",
    "overallrating_care", "overallrating_plan", "coordination",
    "complaints_plan", "leave_plan", "access_problems", "improve",
    "appeals_timely", "appeals_review", "ttyt_available"
  )
)
