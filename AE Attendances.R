## A&E attendance full analysis


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


# read in data ------------------------------------------------------------

matched <- read_parquet("/conf/LIST_analytics/Lanarkshire/HSCP/Homelessness/Source Linked Analysis 2026/Data/Linked Data/linked_data_2324_2026-09-28.parquet")

ae_df <- matched |>
  select(year, age, gender, cohort, lca_group, hscp2019, pop, ae_attendances) |>
  mutate(
    age_group = case_when(
      age <= 15 ~ "0-15",
      between(age, 16, 25) ~ "16-25",
      between(age, 26, 44) ~ "26-44",
      between(age, 45, 64) ~ "45-64",
      age >= 65 ~ "65+"
    ),
    .after = age
  )

rm(matched)


# freq perc ---------------------------------------------------------------

cum_freq <- ae_df |>
  mutate(ae_attendances = factor(
    if_else(ae_attendances >= 10, "10+",
      as.character(ae_attendances)
    ),
    levels = c(0:9, "10+")
  )) |>
  count(year, cohort, lca_group, ae_attendances) |>
  group_by(year, cohort, lca_group) |>
  arrange(fct_rev(ae_attendances)) |>
  mutate(
    cumfreq = cumsum(n),
    total = sum(n),
    perc = n / total,
    cumperc = cumfreq / total
  ) |>
  arrange(year, cohort, lca_group, ae_attendances)

cum_freq |>
  ggplot(aes(x = ae_attendances, y = perc, group = cohort, linetype = cohort)) +
  geom_line() +
  facet_wrap(~lca_group)


# disag rates -------------------------------------------------------------

disag_rate <- ae_df |>
  group_by(year, age_group, gender, cohort, lca_group) |>
  summarise(
    numerator = sum(ae_attendances),
    denominator = n(),
    rate = (numerator / denominator) * 1000
  )


# standardised rates ------------------------------------------------------

library(PHEindicatormethods)

esp <- data.frame(
  age_group = factor(seq(from = 0, to = 90, by = 5)),
  esp = esp2013
)

std_rate_df <- ae_df |>
  mutate(age_group = floor(age / 5) * 5) |>
  mutate(age_group = replace_when(
    age_group,
    age_group > 90 ~ 90,
    age_group <= 0 ~ 0
  ) |>
    factor(levels = esp$age_group))

pop <- std_rate_df |>
  group_by(year, cohort, lca_group, age_group) |>
  summarise(pop = sum(pop)) |>
  ungroup() |>
  complete(year, cohort, lca_group, age_group, fill = list(pop = 0))

std_rate <- std_rate_df |>
  group_by(year, age_group, cohort, lca_group) |>
  count(year, cohort, lca_group, age_group, ae_attendances) %>%
  left_join(
    esp,
    by = "age_group"
  ) |>
  left_join(
    pop,
    by = c("year", "cohort", "lca_group", "age_group")
  ) |>
  group_by(year, cohort, lca_group) %>%
  PHEindicatormethods::calculate_dsr(
    x = n,
    n = pop,
    stdpop = esp,
    independent_events = FALSE,
    eventfreq = ae_attendances,
    ageband = age_group,
    multiplier = 1000
  )


# output ------------------------------------------------------------------

openxlsx2::wb_workbook() |>
  wb_add_worksheet() |>
  wb_add_data(sheet = 1, cum_freq) |>
  wb_add_data(sheet = 1, disag_rate, start_col = ncol(cum_freq) + 2) |>
  wb_add_data(sheet = 1, std_rate, start_col = sum(ncol(cum_freq), ncol(disag_rate), 3)) |>
  wb_save("/conf/LIST_analytics/Lanarkshire/HSCP/Homelessness/Source Linked Analysis 2026/Data/HL1 A&E deep dive.xlsx")
