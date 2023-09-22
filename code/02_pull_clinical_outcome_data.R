library(knitr)
library(nerdify)
library(rio)
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)
library(table1)
library(lme4)
library(lmerTest)
library(splines)
library(car)
library(sjPlot)
library(ggplot2)
library(see)
library(ggforce)
library(modelr)


# Link carrier member IDs to spring IDs
customer_name <- c("Docusign", "Hearst", "Pepsi", "Wellstar", "Tegna")

df_ids <- list()
for (i in 1:5) {
df_ids[[i]] <- import(paste0(Sys.getenv("claims_folder_path"), "data/", customer_name[i], "/data_to_sba/", customer_name[i], "_eligibility_cls.csv")) %>% 
  select(-c(V1)) %>%
  rename(carrier_member_id = 5) %>%
  mutate(carrier_member_id = as.character(carrier_member_id)) %>%
  select(covered_life_id, carrier_member_id)
}
df_ids <- df_ids %>% bind_rows()
#########################################
# This code gets covered life IDs for individuals in the ROI matching study.

# Get matched individuals
matched_ids <- readRDS(paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/df_matched.Rdata")) %>%
  distinct(carrier_member_id, spring_dummy)

customer_name <- c("Wellstar", "Pepsi", "Hearst", "Docusign", "Tegna")
elig <- list()
for (i in 1:length(customer_name)) {
  elig[[i]] <- import(paste0(Sys.getenv("claims_folder_path"), "data/", customer_name[i], "/data_to_sba/", customer_name[i], "_", "eligibility_cls.csv")) %>%
    rename(any_of(c(
      carrier_member_id = "member_key",
      carrier_member_id = "indv_id",
      carrier_member_id = "individual_id"
    ))) %>%
    mutate(customer_name = .env$customer_name[i]) %>%
    mutate(carrier_member_id = as.character(carrier_member_id))
}

elig2 <- elig %>% 
  bind_rows() %>%
  mutate(carrier_member_id = as.character(carrier_member_id))

matched_cl <- matched_ids %>%
  inner_join(elig2) %>%
  filter(!is.na(covered_life_id)) %>%
  select(carrier_member_id, covered_life_id, customer_name, spring_dummy)

cl2 <- shGetQuery(con_type = "base_layer", 
                  "select member_id, cl_id AS covered_life_id from base_members where covered_life_id IS NOT NULL") %>%
  right_join(matched_cl)
  
  
# Make study dates
study_dates <- df_matched %>%
  ungroup() %>%
  distinct(customer_name, spring_start_date, spring_end_date) %>%
  mutate(customer_name = case_when(
    customer_name == "Pepsi" ~ "PepsiCo",
    customer_name == "Tegna" ~ "TEGNA",
    TRUE ~ customer_name
  ))


# Load functions
function_path <- "~/Library/CloudStorage/GoogleDrive-mhawrilenko@springhealth.com/Shared drives/Data Science/People/Matt/ds_git/ds_research/core_outcomes_model/"
#function_path <- "/Volumes/GoogleDrive-117005732839348332540/Shared drives/Data Science/People/Matt/ds_git/ds_research/core_outcomes_model/"
source(paste0(function_path, "01_pull_data_by_time.R")) # make_outcome_data
source(paste0(function_path, "02_make_clinical_variables.R")) # make_clinical_variables
source(paste0(function_path, "03_clean_demographics.R")) #clean_demographics


# Pull data
outcome_data_list <- make_outcome_data(
  min_time_since_first_session = 0,
  max_time_since_first_session = 9999,
  max_baseline_gap = 9999,
  max_follow_up = 9999,
  min_phq = 0,
  min_gad = 0)

df_outcomes <- outcome_data_list["df_outcomes"] %>% 
  data.frame() %>%
  rename_with(~ gsub("df_outcomes[.]", "", .x)) %>% # names are prefaced by the list name
  inner_join(cl2 %>% select (-c(customer_name))) %>%
  inner_join(study_dates) %>%
  make_clinical_variables() %>%
  clean_demographics() %>%
  ungroup() %>%
  select(-c(customer_name)) %>%
  # align with time periods of the study
  filter(month_year <= spring_end_date) %>%
  group_by(carrier_member_id, month_year) %>%
  arrange(carrier_member_id, date) %>%
  mutate(
    phq = last(na.omit(PHQ9)),
    gad = last(na.omit(GAD7))
  ) %>%
  slice_tail() %>%
  ungroup() %>%
  mutate(
    phq_change = phq9_baseline - phq,
    gad_change = gad7_baseline - gad
  ) %>%
  mutate(
    phq_rc = ifelse(phq_change >= 5, "yes", "no"),
    gad_rc = ifelse(gad_change >= 5, "yes", "no")
  ) %>%
  mutate(
    any_rc = case_when(
      phq_rc == "yes" ~ "yes",
      gad_rc == "yes" ~ "yes",
      is.na(phq_rc) & is.na(gad_rc) ~ NA,
      TRUE ~ "no"
    )
  ) %>%
  select(carrier_member_id, month_year, time_6m, time_6m_square, time_6m_cube, phq9_baseline, gad7_baseline, phq, gad, phq_change, gad_change, phq_rc, gad_rc, any_rc)

saveRDS(df, paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/clinical_outcomes.Rdata"))
