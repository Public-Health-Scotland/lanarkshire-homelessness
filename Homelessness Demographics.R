##########################################################
# Homelessness demographics using SLF files
# Written by: Kap
# Minor change by Dylan:
#     - Deduplicate applications
#     - Improve memory efficiency
# Written/run on Posit
# R 4.5.1
##########################################################

#### 1. Set up ----

## Load required packages
pacman::p_load(
  arrow,
  tidyverse,
  tidylog
)

###############################################
## Read in episode files
###############################################
# Create list of required variables
VariablesNeeded <- c(
  "year", "anon_chi", "age", "gender", "smrtype", "locality", "simd2020v2_sc_quintile",
  "hl1_sending_lca", "record_keydate1"
)


# 22-23 episode file
Ep_2223 <- read_parquet("/conf/hscdiip/01-Source-linkage-files/source-episode-file-2223.parquet",
  col_select = VariablesNeeded
) %>%
  as_tibble()

# filter for hl1 applications
Ep_2223 <- Ep_2223 %>%
  #  filter(smrtype %in% c("HL1-Main", "HL1-Other"))
  filter(smrtype == "HL1-Main")
# Select for applications made during financial year
Ep_2223 <- Ep_2223 %>%
  filter(record_keydate1 >= dmy("01-04-2022") &
    record_keydate1 <= dmy("31-03-2023"))

#### 23-24 episode file
Ep_2324 <- read_parquet("/conf/hscdiip/01-Source-linkage-files/source-episode-file-2324.parquet",
  col_select = VariablesNeeded
) %>%
  as_tibble()

Ep_2324 <- Ep_2324 %>%
  filter(smrtype == "HL1-Main")

# Select for applications made during financial year
Ep_2324 <- Ep_2324 %>%
  filter(record_keydate1 >= dmy("01-04-2023") &
    record_keydate1 <= dmy("31-03-2024"))


###### 24-25 episode file
Ep_2425 <- read_parquet("/conf/hscdiip/01-Source-linkage-files/source-episode-file-2425.parquet",
  col_select = VariablesNeeded
) %>%
  as_tibble()

Ep_2425 <- Ep_2425 %>%
  filter(smrtype == "HL1-Main")

# Select for applications made during financial year
Ep_2425 <- Ep_2425 %>%
  filter(record_keydate1 >= dmy("01-04-2024") &
    record_keydate1 <= dmy("31-03-2025"))

#### Bind rows
HL1_all <- bind_rows(Ep_2223, Ep_2324, Ep_2425)

rm(Ep_2223, Ep_2324, Ep_2425)


### Tidy
HL1_all <- HL1_all %>%
  mutate(hl1_sending_lca = case_when(
    hl1_sending_lca == "S12000044" ~ "North Lanarkshire",
    hl1_sending_lca == "S12000029" ~ "South Lanarkshire"
  ))

HL1_all <- HL1_all %>%
  filter(hl1_sending_lca %in% c("North Lanarkshire", "South Lanarkshire"))

#### remove duplicates
HL1_all <- distinct(HL1_all)

HL1_all <- HL1_all %>%
  group_by(year, anon_chi, hl1_sending_lca) %>%
  summarise(
    age = last(age),
    gender = last(gender),
    simd2020v2_sc_quintile = last(simd2020v2_sc_quintile),
    locality = last(locality),
    applications = n()
  ) %>%
  ungroup()

# remove unknown chis
HL1_all <- HL1_all %>%
  filter(!is.na(anon_chi))

HL1_all_check <- HL1_all %>%
  group_by(year, hl1_sending_lca) %>%
  summarise(
    applications = sum(applications),
    applicants = n()
  ) %>%
  ungroup()

#######################
# create pan lan

HL1_all_PanLan <- HL1_all %>%
  mutate(hl1_sending_lca = "Pan Lanarkshire")

HL1_all_PanLan <- HL1_all_PanLan %>%
  group_by(year, anon_chi, hl1_sending_lca) %>%
  summarise(
    age = last(age),
    gender = last(gender),
    simd2020v2_sc_quintile = last(simd2020v2_sc_quintile),
    locality = last(locality),
    applications = sum(applications)
  ) %>%
  ungroup()

HL1_all <- bind_rows(HL1_all, HL1_all_PanLan)

rm(HL1_all_PanLan)

#########################################################
# Population
########################################################

VariablesNeeded2 <- c("year", "anon_chi", "age", "dob", "gender", "locality", "lca", "keep_population", "hscp2019", "simd2020v2_sc_quintile")

Ind_all <- open_dataset(
  c(
    "/conf/hscdiip/01-Source-linkage-files/source-individual-file-2223.parquet",
    "/conf/hscdiip/01-Source-linkage-files/source-individual-file-2324.parquet",
    "/conf/hscdiip/01-Source-linkage-files/source-individual-file-2425.parquet"
  )
) %>%
  select(all_of(VariablesNeeded2)) %>%
  filter(
    lca %in% c("23", "29"),
    keep_population == 1
  ) %>%
  collect()

Ind_all <- Ind_all %>%
  mutate(hl1_sending_lca = case_when(
    hscp2019 == "S37000028" ~ "South Lanarkshire",
    hscp2019 == "S37000035" ~ "North Lanarkshire"
  ))

###############
# create pan lan
Ind_all_PanLan <- Ind_all %>%
  mutate(hl1_sending_lca = "Pan Lanarkshire")

Ind_all <- bind_rows(Ind_all, Ind_all_PanLan)

rm(Ind_all_PanLan)
########################################################
#### AGE BAND
#######################################################

# Recode negative ages to 0 for time being - issue has been raised to source team.
HL1_AgeBand <- HL1_all %>%
  mutate(age = ifelse(age < 0, 0, age))

HL1_AgeBand <- HL1_AgeBand %>%
  mutate(Group = case_when(
    age <= 17 ~ "0-17",
    between(age, 18, 44) ~ "18-44",
    between(age, 45, 64) ~ "45-64",
    age >= 65 ~ "65+"
  ))

HL1_AgeBand <- HL1_AgeBand %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = n()
  ) %>%
  ungroup()

HL1_AgeBandTot <- HL1_AgeBand %>%
  mutate(Group = "Total")

HL1_AgeBandTot <- HL1_AgeBandTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = sum(applicants)
  ) %>%
  ungroup()

HL1_AgeBand <- bind_rows(HL1_AgeBand, HL1_AgeBandTot)

rm(HL1_AgeBandTot)

# pop
Ind_AgeBand <- Ind_all %>%
  mutate(Group = case_when(
    age <= 17 ~ "0-17",
    between(age, 18, 44) ~ "18-44",
    between(age, 45, 64) ~ "45-64",
    age >= 65 ~ "65+"
  ))

Ind_AgeBand <- Ind_AgeBand %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = n()) %>%
  ungroup()

Ind_AgeBandTot <- Ind_AgeBand %>%
  mutate(Group = "Total")

Ind_AgeBandTot <- Ind_AgeBandTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

Ind_AgeBand <- bind_rows(Ind_AgeBand, Ind_AgeBandTot)

rm(Ind_AgeBandTot)

HL1_AgeBand <- HL1_AgeBand %>%
  left_join(Ind_AgeBand,
    by = c("year", "hl1_sending_lca", "Group")
  )

HL1_AgeBand <- HL1_AgeBand %>%
  mutate(Indicator = "AgeBand Demographics")

rm(Ind_AgeBand)

########################################################
#### Gender
#######################################################

HL1_Gender <- HL1_all %>%
  mutate(Group = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female"
  ))

HL1_Gender <- HL1_Gender %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = n()
  ) %>%
  ungroup()

HL1_GenderTot <- HL1_Gender %>%
  mutate(Group = "Total")

HL1_GenderTot <- HL1_GenderTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = sum(applicants)
  ) %>%
  ungroup()

HL1_Gender <- bind_rows(HL1_Gender, HL1_GenderTot)

rm(HL1_GenderTot)

# pop
Ind_Gender <- Ind_all %>%
  mutate(Group = case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female"
  ))

Ind_Gender <- Ind_Gender %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = n()) %>%
  ungroup()

Ind_GenderTot <- Ind_Gender %>%
  mutate(Group = "Total")

Ind_GenderTot <- Ind_GenderTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

Ind_Gender <- bind_rows(Ind_Gender, Ind_GenderTot)

rm(Ind_GenderTot)

HL1_Gender <- HL1_Gender %>%
  left_join(Ind_Gender,
    by = c("year", "hl1_sending_lca", "Group")
  )

HL1_Gender <- HL1_Gender %>%
  mutate(Indicator = "Gender Demographics")

rm(Ind_Gender)


########################################################
#### SIMD
#######################################################

HL1_SIMD <- HL1_all %>%
  mutate(Group = case_when(
    simd2020v2_sc_quintile == 1 ~ "Q1 (Most Deprived)",
    simd2020v2_sc_quintile == 2 ~ "Q2",
    simd2020v2_sc_quintile == 3 ~ "Q3",
    simd2020v2_sc_quintile == 4 ~ "Q4",
    simd2020v2_sc_quintile == 5 ~ "Q5 (Least Deprived)",
    TRUE ~ "Unknown"
  ))


HL1_SIMD <- HL1_SIMD %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = n()
  ) %>%
  ungroup()

HL1_SIMDTot <- HL1_SIMD %>%
  mutate(Group = "Total")

HL1_SIMDTot <- HL1_SIMDTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = sum(applicants)
  ) %>%
  ungroup()

HL1_SIMD <- bind_rows(HL1_SIMD, HL1_SIMDTot)

rm(HL1_SIMDTot)

# pop
Ind_SIMD <- Ind_all %>%
  mutate(Group = case_when(
    simd2020v2_sc_quintile == 1 ~ "Q1 (Most Deprived)",
    simd2020v2_sc_quintile == 2 ~ "Q2",
    simd2020v2_sc_quintile == 3 ~ "Q3",
    simd2020v2_sc_quintile == 4 ~ "Q4",
    simd2020v2_sc_quintile == 5 ~ "Q5 (Least Deprived)",
    TRUE ~ "Unknown"
  ))

Ind_SIMD <- Ind_SIMD %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = n()) %>%
  ungroup()

Ind_SIMDTot <- Ind_SIMD %>%
  mutate(Group = "Total")

Ind_SIMDTot <- Ind_SIMDTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

Ind_SIMD <- bind_rows(Ind_SIMD, Ind_SIMDTot)

rm(Ind_SIMDTot)

HL1_SIMD <- HL1_SIMD %>%
  left_join(Ind_SIMD,
    by = c("year", "hl1_sending_lca", "Group")
  )

HL1_SIMD <- HL1_SIMD %>%
  mutate(Indicator = "SIMD Demographics")

rm(Ind_SIMD)

########################################################
#### Locality
#######################################################

HL1_Locality <- HL1_all %>%
  group_by(year, hl1_sending_lca, locality) %>%
  summarise(
    applications = sum(applications),
    applicants = n()
  ) %>%
  ungroup()

NL_localities <- c(
  "Airdrie", "Bellshill", "Coatbridge", "Motherwell",
  "North Lanarkshire North", "Wishaw"
)

SL_localities <- c(
  "Clydesdale", "East Kilbride", "Hamilton",
  "Rutherglen Cambuslang"
)

HL1_Locality <- HL1_Locality %>%
  mutate(
    Group = case_when(
      # NORTH LANARKSHIRE SENDING
      hl1_sending_lca == "North Lanarkshire" & locality %in% SL_localities ~ "South Lanarkshire",
      hl1_sending_lca == "North Lanarkshire" & locality %in% NL_localities ~ locality,
      hl1_sending_lca == "North Lanarkshire" & !locality %in% c(NL_localities, SL_localities) ~ "Outwith Lanarkshire",

      # SOUTH LANARKSHIRE SENDING
      hl1_sending_lca == "South Lanarkshire" & locality %in% SL_localities ~ locality,
      hl1_sending_lca == "South Lanarkshire" & locality %in% NL_localities ~ "North Lanarkshire",
      hl1_sending_lca == "South Lanarkshire" & !locality %in% c(NL_localities, SL_localities) ~ "Outwith Lanarkshire",

      # PAN LANARKSHIRE SENDING
      hl1_sending_lca == "Pan Lanarkshire" & locality %in% c(NL_localities, SL_localities) ~ "Within Lanarkshire",
      hl1_sending_lca == "Pan Lanarkshire" & !locality %in% c(NL_localities, SL_localities) ~ "Outwith Lanarkshire"
    )
  )

HL1_Locality <- HL1_Locality %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = sum(applicants)
  ) %>%
  ungroup()

HL1_LocalityTot <- HL1_Locality %>%
  mutate(Group = "Total")

HL1_LocalityTot <- HL1_LocalityTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(
    applications = sum(applications),
    applicants = sum(applicants)
  ) %>%
  ungroup()

HL1_Locality <- bind_rows(HL1_Locality, HL1_LocalityTot)

HL1_Locality <- HL1_Locality %>%
  select(year, hl1_sending_lca, Group, applications, applicants)

rm(HL1_LocalityTot)

# pop

Ind_Locality <- Ind_all %>%
  rename(Group = locality)

Ind_Locality <- Ind_Locality %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = n()) %>%
  ungroup()

Ind_Locality <- Ind_Locality %>%
  mutate(Group = case_when(
    hl1_sending_lca == "Pan Lanarkshire" &
      Group %in% c(NL_localities, SL_localities) ~ "Within Lanarkshire",
    hl1_sending_lca == "Pan Lanarkshire" &
      !Group %in% c(NL_localities, SL_localities) ~ "Outwith Lanarkshire",
    TRUE ~ Group
  ))


Ind_Locality <- Ind_Locality %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()


Ind_LocalityTot <- Ind_Locality %>%
  mutate(Group = "Total")

Ind_LocalityTot <- Ind_LocalityTot %>%
  group_by(year, hl1_sending_lca, Group) %>%
  summarise(Population = sum(Population)) %>%
  ungroup()

Ind_Locality <- bind_rows(Ind_Locality, Ind_LocalityTot)

rm(Ind_LocalityTot)

HL1_Locality <- HL1_Locality %>%
  left_join(Ind_Locality,
    by = c("year", "hl1_sending_lca", "Group")
  )

HL1_Locality <- HL1_Locality %>%
  mutate(Indicator = "Locality Demographics")

rm(Ind_Locality)

#####################################
## tidy
#####################################

HL1_Combined <- bind_rows(HL1_AgeBand, HL1_Gender, HL1_SIMD, HL1_Locality)

rm(HL1_all_check)

HL1_Combined <- HL1_Combined %>%
  select(Indicator, year, hl1_sending_lca, Group, applicants, applications, Population)

###########################################
# Caluclate rate
############################################
HL1_Combined <- HL1_Combined %>%
  mutate(ApplicantRate = applicants / Population * 1000)

HL1_Combined <- HL1_Combined %>%
  mutate(ApplicationRate = applications / Population * 1000)


#######################
# Save
#################

HL1_Combined %>%
  write_csv(
    str_glue("/conf/LIST_analytics/Lanarkshire/HSCP/Homelessness/Source Linked Analysis 2026/Data/Demographics datasheet {date}.csv",
      date = ymd(Sys.Date())
    )
  )
