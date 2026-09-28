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

indicator_lookup <- c(
  # hospital use
  "A&E Attendances" = "ae_attendances",
  "Acute Episodes" = "acute_episodes",
  "Acute Inpatient Episodes" = "acute_inpatient_episodes",
  "Acute Inpatient Episode Bed Days" = "acute_inpatient_beddays",
  "Acute Elective Inpatient Episodes" = "acute_el_inpatient_episodes",
  "Acute Elective Inpatient Episode Bed Days" = "acute_el_inpatient_beddays",
  "Acute Non Elective Inpatient Episodes" = "acute_non_el_inpatient_episodes",
  "Acute Non Elective Inpatient Episode Bed Days" = "acute_non_el_inpatient_beddays",
  "Acute Day Case Episodes" = "acute_daycase_episodes",

  # preventable admissions
  "Preventable Admissions" = "preventable_admissions",
  "Preventable Admission Bed Days" = "preventable_beddays",

  # mental health
  "Mental Health Episodes" = "mh_episodes",
  "Mental Health Inpatient Episodes" = "mh_inpatient_episodes",
  "Mental Health Inpatient Bed Days" = "mh_inpatient_beddays",
  "Mental Health Elective Inpatient Episodes" = "mh_el_inpatient_episodes",
  "Mental Health Elective Inpatient Bed Days" = "mh_el_inpatient_beddays",
  "Mental Health Non Elective Inpatient Episodes" = "mh_non_el_inpatient_episodes",
  "Mental Health Non Elective Inpatient Bed Days" = "mh_non_el_inpatient_beddays",

  # maternity
  "Maternity Episodes" = "mat_episodes",
  "Maternity Inpatient Episodes" = "mat_inpatient_episodes",
  "Maternity Inpatient Bed Days" = "mat_inpatient_beddays",
  "Maternity Day Case Episodes" = "mat_daycase_episodes",

  # outpatient attendance
  "Outpatient New Contacts Attendances" = "op_newcons_attendances",
  "Outpatient New Contacts Appointments" = "op_newcons_dnas",

  # out-of-hours
  "Out of Hours Cases" = "ooh_cases",
  "Out of Hours Advice" = "ooh_advice",
  "Out of Hours NHS24 Calls" = "ooh_nhs24",
  "Out of Hours - Other" = "ooh_other",
  "Out of Hours PCC" = "ooh_pcc",
  "Out of Hours Consultation time" = "ooh_consultation_time",

  # delayed discharge
  "Delayed Discharges non Code 9" = "dd_noncode9_episodes",
  "Delayed Discharges non Code 9 Bed Days" = "dd_noncode9_beddays",
  "Delayed Discharges Code 9" = "dd_code9_episodes",
  "Delayed Discharges Code 9 Bed Days" = "dd_code9_beddays",

  # health costs
  "Health Net Costs" = "health_net_cost",
  "Prescribed Items Paid" = "pis_paid_items",
  "Prescribed Items Cost" = "pis_cost",

  # LCAs (derived later)
  "Percentage of cohort with 3+ LTCs" = "LTC3plus"
)

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
  "demographic_cohort", "service_use_cohort",
  unname(indicator_lookup),

  # LTCs
  "arth", "asthma", "atrialfib", "cancer", "cvd", "liver", "copd",
  "dementia", "diabetes", "epilepsy", "chd", "hefailure", "ms",
  "parkinsons", "refailure"
)


# read in
Ind_all <- open_dataset(
  str_glue(
    "/conf/hscdiip/01-Source-linkage-files/source-individual-file-{yr}.parquet",
    yr = years
  )
) %>%
  select(any_of(ind_vars)) %>%
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

# save out linked data
write_parquet(
  matched,
  glue::glue(
    "/conf/LIST_analytics/Lanarkshire/HSCP/Homelessness/Source Linked Analysis 2026/Data/Linked Data/linked_data_{year}_{date}.parquet",
    year = years,
    date = ymd(Sys.Date())
  )
)

# rates -------------------------------------------------------------------

rates <- matched %>%
  group_by(year, lca_group, cohort) %>%
  summarise(
    pop = sum(pop),
    mat_pop = sum(mat_pop),
    across(all_of(unname(indicator_lookup)), sum),
    .groups = "drop"
  )

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
      from = indicator_lookup,
      to = names(indicator_lookup),
      unmatched = "error"
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
        "Out of Hours Consultation time",
        "Prescribed Items Paid",
        "Prescribed Items Cost",
        "Health Net Costs"
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
