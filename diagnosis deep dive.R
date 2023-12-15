
# Libraries
library(tidyverse)
library(icd)
library(table1)
library(car)
library(sjPlot)
library(ggplot2)


# Load the data
df_matched <- readRDS(paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/df_matched.Rdata"))


# Make the table
df_dx_table <- df_matched %>%
  group_by(customer_name, carrier_member_id, spring_dummy) %>%
  arrange(month_year) %>%
  summarize(
    carrier_mh_dx = first(carrier_primary_mh_dx, na_rm = TRUE),
    spring_mh_dx = first(spring_primary_mh_dx, na_rm = TRUE),
    risk_score_dx = risk_score[months == 1],
    carrier_icd_code = first(carrier_primary_mh_code, na_rm=TRUE),
    spring_icd_code = first(spring_primary_mh_code, na_rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    icd_code = coalesce(spring_icd_code, carrier_icd_code),
    mh_dx = coalesce(spring_mh_dx, carrier_mh_dx)
    ) %>%
  mutate(dx_collapsed = fct_collapse(mh_dx,
                                     mood = "mood",
                                     anxiety = "anxiety",
                                     sud = "sud",
                                     other = c("personality", "psychotic", "intellectual disability", "physical behavioral", "physiological", "developmental", "child onset", "unspecified") 
  )) %>%
  mutate(spring_dummy = factor(spring_dummy, labels = c("Control", "Spring Health"))) 

# Add decimals to data missing decimals
df_dx_table <- df_dx_table %>%
  filter(!is.na(icd_code)) %>%
  mutate(icd_code = ifelse(str_detect(icd_code, "\\."), icd_code, short_to_decimal(icd_code))) %>%
  mutate(icd_dx = explain_table(icd_code)$short_desc) %>%
  filter(!is.na(icd_dx))


# Add labels
label(df_dx_table$mh_dx) <- "Mental health diagnoses, collapsed"
label(df_dx_table$icd_dx) <- "Mental health diagnoses, detailed"



# NOTE: p-value function is broken here so currently dropping
table1( ~ mh_dx  | spring_dummy,
        overall = F,
        footnote = "This table is based on raw values. In cases where 2 matches were not achieved, weights were applied.",
        render.categorical= "PCTnoNA%",
        render.missing = NULL,
        data=df_dx_table)

table1( ~ mh_dx + icd_dx  | spring_dummy,
        overall = F,
        footnote = "This table is based on raw values. In cases where 2 matches were not achieved, weights were applied.",
        render.categorical= "FREQ (PCTnoNA%)",
        render.missing = NULL,
        data=filter(df_dx_table, mh_dx == "mood"))

df_anxiety <- df_dx_table %>%
  filter(!is.na(mh_dx)) %>%
  filter(mh_dx=="anxiety") %>%
  group_by(spring_dummy, mh_dx, icd_dx) %>%
  summarize(n = n()) %>%
  group_by(spring_dummy) %>%
  mutate(pct = 100*n/sum(n)) %>%
  mutate(pct = round(pct, 1)) %>%
  group_by(icd_dx) %>%
  mutate(overall_pct = 100*sum(n)/nrow(filter(df_dx_table, !is.na(mh_dx)))) %>%
  mutate(overall_pct = round(overall_pct, 1)) %>%
  arrange(spring_dummy, mh_dx, -overall_pct) %>%
  pivot_wider(names_from = spring_dummy, values_from = n:pct) 

df_mood <- df_dx_table %>%
  filter(!is.na(mh_dx)) %>%
  filter(mh_dx=="mood") %>%
  group_by(spring_dummy, mh_dx, icd_dx) %>%
  summarize(n = n()) %>%
  group_by(spring_dummy) %>%
  mutate(pct = 100*n/sum(n)) %>%
  mutate(pct = round(pct, 1)) %>%
  group_by(icd_dx) %>%
  mutate(overall_pct = 100*sum(n)/nrow(filter(df_dx_table, !is.na(mh_dx)))) %>%
  mutate(overall_pct = round(overall_pct, 1)) %>%
  arrange(spring_dummy, mh_dx, -overall_pct) %>%
  pivot_wider(names_from = spring_dummy, values_from = n:pct) 

# Drop median, min, max
render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(round(x, 1)), digits=3), c("",
                                                                     "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}

table1( ~ age_at_index + member_sex + dx_imputed + risk_score_dx + months_diagnosed + any_bh + bh_if_engaged | spring_dummy,
        overall = F,
        render.continuous = render.cont,
        footnote = "This table is based on raw values. In cases where 2 matches were not achieved, weights were applied.",
        data=df_match_table)