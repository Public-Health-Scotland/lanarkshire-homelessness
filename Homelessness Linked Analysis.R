##########################################################
# Homelessness analysis using SLF files
# Written by: Dylan
# R 4.5.1
# Stats drive folder at:
# /conf/LIST_analytics/Lanarkshire/HSCP/Homelessness/Source Linked Analysis 2026/
##########################################################

# Load packages ---------------------------------------------------------------

pacman::p_load(
  arrow,
  tidyverse,
  openxlsx2,
  tidylog
)


# controls ----------------------------------------------------------------

years <- c(2324)

# source episode data ---------------------------------------------------------
#### note - largely unchanged

## Read in episode files
# Create list of required variables
episode_vars <- c("year", "anon_chi", "age", "gender", "smrtype", "hl1_sending_lca", "record_keydate1")

# read and combine HL1 applications from single year episode files
HL1_all <- map(
  years,
  \(yr){
    filename <- paste0(
      "/conf/hscdiip/01-Source-linkage-files/source-episode-file-",
      yr,
      ".parquet"
    )
    cat("Reading in ", filename, "\n")
    read_parquet(
      filename,
      col_select = episode_vars
    ) %>%
      filter(smrtype == "HL1-Main") %>%
      filter(record_keydate1 >= dmy(paste0("01-04-20", substr(yr, 1, 2))) &
        record_keydate1 <= dmy(paste0("31-03-20", substr(yr, 3, 4))))
  }
) %>%
  list_rbind()

#### filter to LCA
HL1_all <- HL1_all %>%
  mutate(hl1_sending_lca = case_when(
    hl1_sending_lca == "S12000044" ~ "North Lanarkshire",
    hl1_sending_lca == "S12000029" ~ "South Lanarkshire"
  ))

HL1_all <- HL1_all %>%
  filter(hl1_sending_lca %in% c("North Lanarkshire", "South Lanarkshire"))

#### remove duplicates
HL1_all <- distinct(HL1_all)

#### aggregate
HL1_all_agg <- HL1_all %>%
  filter_out(is.na(anon_chi)) %>%
  group_by(year, anon_chi, hl1_sending_lca) %>%
  summarise(
    applications = n()
  ) %>%
  ungroup()


# source individual data -----------------------------------------------------

ind_vars <- c(
  "year", "anon_chi", "age", "gender", "lca", "keep_population", "hscp2019",
  "ae_attendances", "acute_episodes", "acute_inpatient_episodes",
  "acute_inpatient_beddays", "acute_el_inpatient_episodes",
  "acute_el_inpatient_beddays", "acute_non_el_inpatient_episodes",
  "acute_non_el_inpatient_beddays", "acute_daycase_episodes", "mh_episodes",
  "mh_inpatient_episodes", "mh_inpatient_beddays", "mh_el_inpatient_episodes",
  "mh_el_inpatient_beddays", "mh_non_el_inpatient_episodes", "mh_non_el_inpatient_beddays",
  "mat_episodes", "mat_inpatient_episodes", "mat_inpatient_beddays",
  "mat_daycase_episodes", "op_newcons_attendances", "op_newcons_dnas", "gls_episodes",
  "gls_inpatient_episodes", "gls_inpatient_beddays", "gls_el_inpatient_episodes",
  "gls_el_inpatient_beddays", "gls_non_el_inpatient_episodes",
  "gls_non_el_inpatient_beddays", "preventable_admissions",
  "preventable_beddays", "cmh_contacts", "ooh_cases", "ooh_homev", "ooh_advice",
  "ooh_dn", "ooh_nhs24", "ooh_other", "ooh_pcc", "ooh_covid_advice",
  "ooh_covid_assessment", "ooh_covid_other", "ooh_consultation_time",
  "dd_noncode9_episodes", "dd_noncode9_beddays", "dd_code9_episodes",
  "dd_code9_beddays", "dn_episodes", "health_net_cost", "pis_paid_items",
  "pis_cost", "arth", "asthma", "atrialfib", "cancer", "cvd", "liver", "copd",
  "dementia", "diabetes", "epilepsy", "chd", "hefailure", "ms", "parkinsons",
  "refailure", "ch_cis_episodes", "ch_beddays", "ch_cost", "hc_episodes",
  "hc_total_hours", "hc_total_cost", "hc_personal_episodes",
  "hc_personal_hours", "hc_personal_hours_cost", "hc_non_personal_episodes",
  "hc_non_personal_hours", "hc_non_personal_hours_cost",
  "hc_reablement_episodes", "hc_reablement_hours", "hc_reablement_hours_cost",
  "at_alarms", "at_telecare", "sds_option_1", "sds_option_2", "sds_option_3",
  "sds_option_4"
)

# read in
Ind_all <- open_dataset(
  str_glue(
    "/conf/hscdiip/01-Source-linkage-files/source-individual-file-{yr}.parquet",
    yr = years
  )
) %>%
  select(all_of(ind_vars)) %>%
  collect()

# join HL1 data
matched <- Ind_all %>%
  left_join(HL1_all_agg,
    by = c("anon_chi", "year"),
    relationship = "one-to-many"
  )
# can't be one-to-one join as individual might be an applicant to both NL and SL

rm(Ind_all)

# filter to NL/SL residents, plus applicants regardless of recorded residence
matched <- matched %>%
  filter(
    lca == 23 |
      lca == 29 |
      !is.na(hl1_sending_lca)
  )

# Assign lca_group (hl1_sending_lca for hl1 applicants, lca for non-applicants),
# create hl1 cohort indicator, and create general population and maternity
# population weights
matched <- matched %>%
  mutate(
    lca_group = replace_when(
      hl1_sending_lca,
      is.na(hl1_sending_lca) & lca == 29 ~ "South Lanarkshire",
      is.na(hl1_sending_lca) & lca == 23 ~ "North Lanarkshire"
    ),
    cohort = case_when(
      is.na(applications) ~ "non-HL1",
      !is.na(applications) ~ "HL1"
    ),
    pop = case_when(
      cohort == "HL1" ~ 1,
      .default = keep_population
    ),
    mat_pop = case_when(
      age >= 15 & age <= 44 & gender == 1 ~ pop,
      .default = 0
    ),
  ) %>%
  select(-applications)
# population weights (keep pop) applied to non-HL1 cohort only

# calculate LTC numbers
matched <- matched %>%
  mutate(
    LTCCount = arth + asthma + atrialfib + cancer + cvd + liver + copd + dementia +
      diabetes + epilepsy + chd + hefailure + ms + parkinsons + refailure,
    LTC3plus = if_else(LTCCount > 2, 1, 0)
  )

# add pan-lan rows
matched <- matched %>%
  distinct(anon_chi, year, .keep_all = TRUE) %>%
  mutate(lca_group = "Pan Lanarkshire") %>%
  bind_rows(matched)


# rates -------------------------------------------------------------------

rates <- matched %>%
  group_by(year, lca_group, cohort) %>%
  summarise(
    pop = sum(pop),
    mat_pop = sum(mat_pop),
    across(
      c(
        ae_attendances, acute_episodes, acute_inpatient_episodes,
        acute_inpatient_beddays, acute_el_inpatient_episodes,
        acute_el_inpatient_beddays, acute_non_el_inpatient_episodes,
        acute_non_el_inpatient_beddays, acute_daycase_episodes,
        mh_episodes, mh_inpatient_episodes, mh_inpatient_beddays,
        mh_el_inpatient_episodes, mh_el_inpatient_beddays,
        mh_non_el_inpatient_episodes, mh_non_el_inpatient_beddays,
        op_newcons_attendances, op_newcons_dnas, gls_episodes,
        gls_inpatient_episodes, gls_inpatient_beddays,
        gls_el_inpatient_episodes, gls_el_inpatient_beddays,
        gls_non_el_inpatient_episodes, gls_non_el_inpatient_beddays,
        preventable_admissions, preventable_beddays, cmh_contacts, ooh_cases,
        ooh_homev, ooh_advice, ooh_dn, ooh_nhs24, ooh_other, ooh_pcc,
        ooh_covid_advice, ooh_covid_assessment, ooh_covid_other,
        ooh_consultation_time, dd_noncode9_episodes, dd_noncode9_beddays,
        dd_code9_episodes, dd_code9_beddays, dn_episodes, health_net_cost,
        pis_paid_items, pis_cost,
        ch_cis_episodes, ch_beddays, ch_cost, hc_episodes, hc_total_hours,
        hc_personal_episodes, hc_personal_hours, hc_personal_hours_cost,
        hc_non_personal_episodes, hc_non_personal_hours,
        hc_non_personal_hours_cost, hc_reablement_episodes, hc_reablement_hours,
        hc_reablement_hours_cost, at_alarms, at_telecare, sds_option_1,
        sds_option_2, sds_option_3, sds_option_4,
        LTC3plus,
        mat_episodes, mat_inpatient_episodes, mat_inpatient_beddays,
        mat_daycase_episodes
      ),
      sum
    )
  ) %>%
  ungroup()


rates <- rates %>%
  pivot_longer(
    cols = -c(year, lca_group, cohort, pop, mat_pop),
    names_to = "indicator",
    values_to = "numerator"
  )

# apply labels
rates <- rates %>%
  mutate(
    indicator_label = recode_values(
      indicator,
      "ae_attendances" ~ "A&E Attendances",
      "acute_episodes" ~ "Acute Episodes",
      "acute_inpatient_episodes" ~ "Acute Inpatient Episodes",
      "acute_inpatient_beddays" ~ "Acute Inpatient Episode Bed Days",
      "acute_el_inpatient_episodes" ~ "Acute Elective Inpatient Episodes",
      "acute_el_inpatient_beddays" ~ "Acute Elective Inpatient Episode Bed Days",
      "acute_non_el_inpatient_episodes" ~ "Acute Non Elective Inpatient Episodes",
      "acute_non_el_inpatient_beddays" ~ "Acute Non Elective Inpatient Episode Bed Days",
      "acute_daycase_episodes" ~ "Acute Day Case Episodes",
      "mh_episodes" ~ "Mental Health Episodes",
      "mh_inpatient_episodes" ~ "Mental Health Inpatient Episodes",
      "mh_inpatient_beddays" ~ "Mental Health Inpatient Bed Days",
      "mh_el_inpatient_episodes" ~ "Mental Health Elective Inpatient Episodes",
      "mh_el_inpatient_beddays" ~ "Mental Health Elective Inpatient Bed Days",
      "mh_non_el_inpatient_episodes" ~ "Mental Health Non Elective Inpatient Episodes",
      "mh_non_el_inpatient_beddays" ~ "Mental Health Non Elective Inpatient Bed Days",
      "mat_episodes" ~ "Maternity Episodes",
      "mat_inpatient_episodes" ~ "Maternity Inpatient Episodes",
      "mat_inpatient_beddays" ~ "Maternity Inpatient Bed Days",
      "mat_daycase_episodes" ~ "Maternity Day Case Episodes",
      "op_newcons_attendances" ~ "Outpatient New Contacts Attendances",
      "op_newcons_dnas" ~ "Outpatient New Contacts Appointments",
      "gls_episodes" ~ "GLS Episodes",
      "gls_inpatient_episodes" ~ "GLS Inpatient Episodes",
      "gls_inpatient_beddays" ~ "GLS Inpatient Bed Days",
      "gls_el_inpatient_episodes" ~ "GLS Elective Inpatient Episodes",
      "gls_el_inpatient_beddays" ~ "GLS Elective Inpatient Bed Days",
      "gls_non_el_inpatient_episodes" ~ "GLS Non Elective Inpatient Episodes",
      "gls_non_el_inpatient_beddays" ~ "GLS Non Elective Inpatient Bed Days",
      "preventable_admissions" ~ "Preventable Admissions",
      "preventable_beddays" ~ "Preventable Admission Bed Days",
      "cmh_contacts" ~ "Community Mental Health Contacts",
      "ooh_cases" ~ "Out of Hours Cases",
      "ooh_homev" ~ "Out of Hours Home Visits",
      "ooh_advice" ~ "Out of Hours Advice",
      "ooh_dn" ~ "Out of Hours District Nurse Visits",
      "ooh_nhs24" ~ "Out of Hours NHS24 Calls",
      "ooh_other" ~ "Out of Hours - Other",
      "ooh_pcc" ~ "Out of Hours PCC",
      "ooh_covid_advice" ~ "Out of Hours Covid Advice",
      "ooh_covid_assessment" ~ "Out of Hours Covid Assessment",
      "ooh_covid_other" ~ "Out of Hours Covid Other",
      "ooh_consultation_time" ~ "Out of Hours Consultation time",
      "dd_noncode9_episodes" ~ "Delayed Disharges non Code 9",
      "dd_noncode9_beddays" ~ "Delayed Disharges non Code 9 Bed Days",
      "dd_code9_episodes" ~ "Delayed Disharges Code 9",
      "dd_code9_beddays" ~ "Delayed Disharges Code 9 Bed Days",
      "dn_episodes" ~ "District Nursing Episodes",
      "health_net_cost" ~ "Health Net Costs",
      "pis_paid_items" ~ "Prescribed Items Paid",
      "pis_cost" ~ "Prescribed Items Cost",
      "LTC3plus" ~ "Percentage of cohort with 3+ LTCs",
      "ch_cis_episodes" ~ "Care Home Continuous Episodes",
      "ch_beddays" ~ "Care Home Continuous Episode Bed Days",
      "ch_cost" ~ "Care Home Costs",
      "hc_episodes" ~ "Home Care Episodes",
      "hc_total_hours" ~ "Home Care Total Hours",
      "hc_total_cost" ~ "Home Care Total Cost",
      "hc_personal_episodes" ~ "Home Care Personal Episodes",
      "hc_personal_hours" ~ "Home Care Personal Hours",
      "hc_personal_hours_cost" ~ "Home Care Personal Hours Cost",
      "hc_non_personal_episodes" ~ "Home Care Non Personal Episodes",
      "hc_non_personal_hours" ~ "Home Care Non Personal Hours",
      "hc_non_personal_hours_cost" ~ "Home Care Non Personal Hours Cost",
      "hc_reablement_episodes" ~ "Home Care Reablement Episodes",
      "hc_reablement_hours" ~ "Home Care Reablement Hours",
      "hc_reablement_hours_cost" ~ "Home Care Reablement Hours Cost",
      "at_alarms" ~ "Total Alarms Packages",
      "at_telecare" ~ "Total Telecare Packages",
      "sds_option_1" ~ "Total SDS (Option 1) Packages",
      "sds_option_2" ~ "Total SDS (Option 2) Packages",
      "sds_option_3" ~ "Total SDS (Option 3) Packages",
      "sds_option_4" ~ "Total SDS (Option 4) Packages"
    ),
    .before = numerator
  )

# calculate rates
rates <- rates %>%
  mutate(
    denominator = if_else(
      indicator %in% c(
        "mat_episodes",
        "mat_inpatient_episodes",
        "mat_inpatient_beddays",
        "mat_daycase_episodes"
      ),
      mat_pop,
      pop
    ),
    rate = case_when(
      indicator_label %in% c(
        "Out of Hours Consultation time", "Prescribed Items Paid", "Prescribed Items Cost",
        "Care Home Costs", "Home Care Total Hours", "Home Care Total Cost", "Home Care Personal Hours",
        "Home Care Personal Hours Cost", "Home Care Non Personal Hours", "Home Care Non Personal Hours Cost",
        "Home Care Reablement Hours", "Home Care Reablement Hours Cost", "Health Net Costs"
      ) ~ numerator / denominator,
      indicator_label == "Percentage of cohort with 3+ LTCs" ~ (100 / denominator) * numerator,
      .default = (numerator / denominator) * 1000
    )
  )


# save out ----------------------------------------------------------------

rates_output <- rates %>%
  select(
    Year = year,
    Indicator = indicator_label,
    LCA = lca_group,
    Cohort = cohort,
    Numerator = numerator,
    Population = denominator,
    Rate = rate
  ) %>%
  mutate(Cohort = replace_values(
    Cohort,
    "HL1" ~ "HL1 Cohort",
    "non-HL1" ~ "Non HL1 Cohort"
  ))

write_csv(
  rates_output,
  str_glue(
    "/conf/LIST_analytics/Lanarkshire/HSCP/Homelessness/Source Linked Analysis 2026/Data/HL1 Linked Analysis datasheet ({fromto}) - {date}.csv",
    fromto = ifelse(length(years) == 1, years, paste0(min(years), "-", max(years))),
    date = ymd(Sys.Date())
  )
)
