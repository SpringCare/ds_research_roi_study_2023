
# Load packages
library(MatchIt)
library(mice)

# user inputs
customer_name <- c("Wellstar", "Pepsi", "Hearst", "Docusign", "Tegna", "Western Governors University", "IDEXX")

# Load data
df_list <- list()
for (i in 1:length(customer_name)) {
df_list[[i]] <- readRDS(file = paste0(Sys.getenv("claims_folder_path"), "data/round_2_analysis/", customer_name[i], "/", customer_name[i], "_analyze_all.Rdata")) #roi_at_scale
}

df_analyze_all <- df_list %>%
  bind_rows()


# Make matched data
df_risk <- df_analyze_all %>%
  mutate(cc_hier = case_when(cc_any == "no" ~ "none"
                             , cc_cancer == "yes" ~ "Cancer"
                             , cc_stroke == "yes" ~ "Stroke"
                             , cc_diabetes == "yes" ~ "Diabetes"
                             , cc_hypertension == "yes" ~ "Hypertension"
                             , cc_heart == "yes" ~ "Heart Disease"
                             , cc_kidney == "yes" ~ "Kidney Disease"
                             , cc_asthma_copd == "yes" ~ "Asthma and COPD"
                             , cc_osteo == "yes" ~ "Osteoporosis"
                             , TRUE ~ "none"
                             
  )) %>%
  group_by(customer_name, carrier_member_id, spring_start_date, spring_dummy, index_date, age_at_index, age_factor, member_sex) %>%
  arrange(month_year) %>%
  summarize(
    carrier_mh_dx = first(carrier_primary_mh_dx, na_rm = TRUE),
    spring_mh_dx = first(spring_primary_mh_dx, na_rm = TRUE),
    risk_score_dx = risk_score[months == 1],
    risk_score_log = log(risk_score[months == 1]),
    risk_score_post_log = log(last(risk_score, na_rm = T))
  ) %>%
  ungroup() %>%
  mutate(mh_dx = coalesce(spring_mh_dx, carrier_mh_dx)) %>%
  mutate(dx_collapsed = fct_collapse(mh_dx,
                                     mood = "mood",
                                     anxiety = "anxiety",
                                     sud = "sud",
                                     other = c("personality", "psychotic", "intellectual disability", "physical behavioral", "physiological", "developmental", "child onset", "unspecified")
  )) %>%
  mutate(age_factor = droplevels(as.factor(age_factor))) %>%
  select(-c(carrier_mh_dx, spring_mh_dx)) %>%
  filter(!is.na(risk_score_log)) %>%
  ungroup() 


# Make propensity scores--------------------------------------------------------

## First, we need to impute missing diagnoses for spring group
# To keep it simple, we will 
# use a single imputation since diagnosis is only one of several variables, and
# mostly distributed across 2 groups, and only used to find a reasonable match; 
# the influence in terms of increased error should be trivial.


## save spring ids
spring_ids <- df_risk$carrier_member_id[df_risk$spring_dummy == "Spring"]


## Run imputation model  
set.seed(20220110)
imp <- mice(
  data = df_risk %>%
    filter(spring_dummy == "Spring") %>%
    rename(dx_imputed = dx_collapsed) %>%
    select(-carrier_member_id, -index_date, -spring_start_date, -mh_dx), # Drop variables that break imputation
  m=1,
  maxit = 30)

## Collect imputed data
df_imp <- complete(imp, 1) %>%
  cbind(spring_ids) %>% # add ids back in
  rename(carrier_member_id = spring_ids) %>% # join ids on their original name
  select(carrier_member_id, dx_imputed)

## Bring imputed diagnoses back into original df
df_risk <- df_risk %>%
  left_join(df_imp) %>%
  mutate(dx_imputed = coalesce(dx_collapsed, dx_imputed))


## Matchit only uses a binary tx variable, 
df_post <- df_risk %>%
  filter(index_date >= spring_start_date) %>%
  filter(!is.na(risk_score_log)) %>%
  filter(!is.na(dx_imputed)) # fixes some IDEXX issues


## Nearest neighbor matching
set.seed(20220110)
match.final <-  matchit(spring_dummy ~ customer_name + age_at_index + member_sex + risk_score_log + dx_imputed + index_date, 
                        data = df_post,
                        method = "nearest",
                        caliper = c(risk_score_log = 0.10),
                        exact = c("customer_name", "dx_imputed"),
                        ratio = 2, 
                        distance = "glm")
summary(match.final)

# Add matching info back to df_risk
df_matched <- match.data(match.final, drop.unmatched = TRUE) %>%
  rename(
    weights_nn = weights,
    subclass_nn = subclass
  ) %>%
  full_join(df_risk) %>%
  left_join(df_analyze_all) %>%
  filter(weights_nn > 0)

#---------------
saveRDS(df_analyze_all, paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/new employer update/df_analyze_all_new_employers.Rdata"))
saveRDS(df_matched, paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/new employer update/df_matched_new_employers.Rdata"))


         
         

