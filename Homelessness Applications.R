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

HL1_all <- HL1_all |>
  mutate(
    age_group = case_when(
      age <= 17 ~ "0-17",
      between(age, 18, 44) ~ "18-44",
      between(age, 45, 64) ~ "45-64",
      age >= 65 ~ "65+"
    ),
    gender = recode_values(
      gender,
      1 ~ "Male",
      2 ~ "Female"
    )
  )

# summary function --------------------------------------------------------


age_gender_summary <- function(
  data,
  var,
  other_group = "hl1_sending_lca",
  include_zero_counts = TRUE
) {
  groups <- list(
    character(),
    "gender",
    "age_group",
    c("gender", "age_group")
  )

  out <- groups |>
    purrr::map(\(g) {
      grouping_vars <- c(g, other_group)

      x <- data |>
        count(
          across(all_of(c(grouping_vars, var)))
        )

      if (include_zero_counts) {
        x <- x |>
          tidyr::complete(
            tidyr::nesting(!!!rlang::syms(grouping_vars)),
            !!rlang::sym(var),
            fill = list(n = 0)
          )
      }

      x |>
        group_by(
          across(all_of(grouping_vars))
        ) |>
        mutate(prop = n / sum(n)) |>
        mutate(
          gender = if (!"gender" %in% g) "all" else gender,
          age_group = if (!"age_group" %in% g) "all" else age_group,
          .before = all_of(var)
        )
    }) |>
    bind_rows()

  return(out)
}

# summaries ----------------------------------------------------------------

# applications per person

apps_pp <- HL1_all |>
  filter(!is.na(anon_chi)) |>
  count(anon_chi, hl1_sending_lca, age_group, gender) |>
  rename(applications = n) |>
  age_gender_summary("applications")

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

prop_type <- age_gender_summary(HL1_all, "hl1_property_type")


# reasons

n_reasons <- HL1_all |>
  rowwise() |>
  mutate(reasons = sum(c_across(reason_financial:reason_other))) |>
  ungroup() |>
  mutate(reasons = as.factor(replace_when(
    as.character(reasons),
    reasons > 3 ~ "4+"
  ))) |>
  age_gender_summary("reasons")

reasons <- HL1_all |>
  mutate(reason_none = hl1_reason_ftm == "") |>
  pivot_longer(
    cols = starts_with("reason"),
    names_to = "reason",
    values_to = "has_reason"
  ) |>
  mutate(
    reason = recode_values(
      reason,
      from = names(reason_lookup),
      to = reason_lookup,
      default = "No reason recorded"
    ) |>
      factor(levels = c(reason_lookup, "No reason recorded"))
  ) |>
  age_gender_summary(
    var = "has_reason",
    other_group = c("hl1_sending_lca", "reason")
  ) |>
  filter(has_reason == TRUE) |>
  select(-has_reason)


# exploratory plots -------------------------------------------------------

prop_type |>
  filter(age_group == "all", gender == "all") |>
  ggplot(aes(x = hl1_property_type, y = prop, fill = hl1_sending_lca)) +
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


prop_type |>
  filter(age_group != "all", gender != "all") |>
  ggplot(aes(x = hl1_property_type, y = prop, fill = hl1_sending_lca)) +
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
  facet_grid(gender ~ age_group) +
  phsstyles::scale_fill_discrete_phs(type = "qual") +
  phsstyles::theme_phs()


n_reasons |>
  filter(age_group == "all", gender == "all") |>
  ggplot(aes(y = hl1_sending_lca, x = n, fill = reasons)) +
  geom_col(position = position_fill(reverse = TRUE)) +
  labs(
    x = "Proportion of applications",
    y = NULL,
    title = "Number of reasons recorded"
  ) +
  phsstyles::scale_fill_discrete_phs(type = "qual", palette = "all") +
  phsstyles::theme_phs()

reasons |>
  filter(age_group == "all", gender == "all") |>
  ggplot(aes(x = reason, y = prop, fill = hl1_sending_lca)) +
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

reasons |>
  filter(age_group != "all", gender != "all") |>
  ggplot(aes(x = reason, y = prop, fill = hl1_sending_lca)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(
    labels = \(x) stringr::str_wrap(x, width = 40)
  ) +
  facet_grid(gender ~ age_group) +
  labs(
    title = "Reason for failing to maintain accommodation",
    y = "Proportion of applications",
    x = NULL
  ) +
  coord_flip() +
  phsstyles::scale_fill_discrete_phs(type = "qual") +
  phsstyles::theme_phs()
