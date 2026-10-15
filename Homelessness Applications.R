##########################################################
# Homelessness application level analysis using SLF episode files
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


# load eps ----------------------------------------------------------------

episode_vars <- c("year", "anon_chi", "age", "gender", "smrtype", "hl1_sending_lca", "record_keydate1", "hl1_property_type", "hl1_reason_ftm")

# read and combine HL1 applications from single year episode files

HL1_all <- open_dataset(
  str_glue(
    "/conf/hscdiip/01-Source-linkage-files/source-episode-file-{yr}.parquet",
    yr = years
  )
) %>%
  select(all_of(episode_vars)) %>%
  filter(
    smrtype == "HL1-Main",
    hl1_sending_lca %in% c("S12000044", "S12000029")
  ) %>%
  collect()

# Deduplicate

HL1_all <- unique(HL1_all)

# wrangle -----------------------------------------------------------------

#### label LCA, property type, and reasons
HL1_all <- HL1_all %>%
  mutate(
    hl1_sending_lca = case_when(
      hl1_sending_lca == "S12000044" ~ "North Lanarkshire",
      hl1_sending_lca == "S12000029" ~ "South Lanarkshire"
    ),
    hl1_property_type = str_remove(hl1_property_type, ".*\\d - "),
    reason_financial = str_detect(hl1_reason_ftm, "F"),
    reason_physical = str_detect(hl1_reason_ftm, "Ph"),
    reason_mental = str_detect(hl1_reason_ftm, "M"),
    reason_support = str_detect(hl1_reason_ftm, "U"),
    reason_family = str_detect(hl1_reason_ftm, "L"),
    reason_managing = str_detect(hl1_reason_ftm, "O"),
    reason_drugs = str_detect(hl1_reason_ftm, "D"),
    reason_criminal = str_detect(hl1_reason_ftm, "C"),
    reason_other = str_detect(hl1_reason_ftm, "N")
  )

reason_lookup <- c(
  reason_financial = "Financial Difficulties / Debt / Unemployment",
  reason_physical  = "Physical Health Reasons",
  reason_mental    = "Mental Health Reasons",
  reason_support   = "Unmet Need for Support from Housing / Social Work / Health Services",
  reason_family    = "Lack of Support from Friends / Family",
  reason_managing  = "Difficulties Managing on Own",
  reason_drugs     = "Drug / Alcohol Dependency",
  reason_criminal  = "Criminal / Anti-Social Behaviour",
  reason_other     = "Not to do with Applicant Household"
)


# tabs & plots ----------------------------------------------------------------

# applications per person

apps_pp <- HL1_all |>
  filter(!is.na(anon_chi)) |>
  count(anon_chi, hl1_sending_lca) |>
  rename(applications = n) |>
  count(hl1_sending_lca, applications)

ggplot(apps_pp, aes(y = hl1_sending_lca, x = n, fill = factor(applications))) +
  geom_col(position = position_fill(reverse = T)) +
  labs(
    title = "Number of applications per person",
    x = "Proportion of applicants",
    y = NULL
  ) +
  phsstyles::scale_fill_discrete_phs(type = "qual") +
  phsstyles::theme_phs()

# property type

prop_type <- HL1_all |>
  count(hl1_sending_lca, hl1_property_type) |>
  group_by(hl1_sending_lca) |>
  mutate(prop = n / sum(n))

ggplot(prop_type, aes(x = hl1_property_type, y = prop, fill = hl1_sending_lca)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(
    labels = \(x) stringr::str_wrap(x, width = 40)
  ) +
  labs(
    title = "Property type of last settled address",
    y = "Proportion of applications",
    x = NULL
  ) +
  coord_flip() +
  phsstyles::scale_fill_discrete_phs(type = "qual") +
  phsstyles::theme_phs()

# reasons

n_reasons <- HL1_all |>
  rowwise() |>
  mutate(reasons = sum(c_across(reason_financial:reason_other))) |>
  ungroup() |>
  count(hl1_sending_lca, reasons) |>
  mutate(reasons = as.factor(replace_when(
    as.character(reasons),
    reasons > 3 ~ "4+"
  )))

ggplot(n_reasons, aes(y = hl1_sending_lca, x = n, fill = reasons)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  labs(
    x = "Proportion of applications",
    y = NULL,
    title = "Number of reasons recorded"
  ) +
  phsstyles::scale_fill_discrete_phs(type = "qual", palette = "all") +
  phsstyles::theme_phs()


reasons <- HL1_all |>
  group_by(hl1_sending_lca) |>
  summarise(across(starts_with("reason"), mean),
    reason_none = mean(hl1_reason_ftm == "")
  ) |>
  pivot_longer(
    cols = starts_with("reason"),
    names_to = "reason",
    values_to = "prop",
    names_transform = ~ recode_values(.x,
      from = names(reason_lookup),
      to = reason_lookup,
      default = "No reason recorded"
    ) |>
      factor(levels = c(reason_lookup, "No reason recorded"))
  )

ggplot(reasons, aes(x = reason, y = prop, fill = hl1_sending_lca)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(
    labels = \(x) stringr::str_wrap(x, width = 40)
  ) +
  labs(
    title = "Reason for failing to maintain accommodation",
    y = "Proportion of applications",
    x = NULL
  ) +
  coord_flip() +
  phsstyles::scale_fill_discrete_phs(type = "qual") +
  phsstyles::theme_phs()
