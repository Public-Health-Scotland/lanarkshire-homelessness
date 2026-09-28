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
