cc1 <- lmer(med_pmpm_trunc_ov_mh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc2 <- lmer(med_pmpm_trunc_er_mh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc3 <- lmer(med_pmpm_trunc_hosp_op_mh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc4 <- lmer(med_pmpm_trunc_ip_mh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc5 <- lmer(med_pmpm_trunc_therapy_mh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc6 <- lmer(med_pmpm_trunc_other_mh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

## Non-MH
cc7 <- lmer(med_pmpm_trunc_ov_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc8 <- lmer(med_pmpm_trunc_er_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc9 <- lmer(med_pmpm_trunc_hosp_op_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc10 <- lmer(med_pmpm_trunc_ip_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc11 <- lmer(med_pmpm_trunc_therapy_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              customer_name +
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = filter(df_matched, months >= -5 & months <= 6))

cc12 <- lmer(med_pmpm_trunc_other_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
               customer_name +
               (1 | carrier_member_id) +
               (1 | subclass_nn),
             weights = weights_nn,
             data = filter(df_matched, months >= -5 & months <= 6))

## Spring therapy cost
### For this to work, need to move code for total invoiced up higher. for now, we'll do it the old fashioned way: hard-coding!!
sc_cost_adjustment = 188.27

# Make in table
df_cost_results <- data.frame(
  outcome = rep(c("Office visits", "Emergency room", "Hospital outpatient", "Hospital inpatient", "Therapy", "Other"), 2),
  spend_type = c(rep("Mental healthcare savings", 6), rep("All other healthcare savings", 6)),
  estimate = NA,
  se = NA
) %>%
  mutate(outcome = fct_relevel(as.factor(outcome), "Office visits", "Emergency room", "Hospital outpatient", "Hospital inpatient"))

for (i in 1:12) {
  df_cost_results$estimate[i] = summary(get(paste0("cc", i)))$coefficients["spring_dummySpring:phasepost_tx", "Estimate"]
  df_cost_results$se[i] = summary(get(paste0("cc", i)))$coefficients["spring_dummySpring:phasepost_tx", "Std. Error"]
}

# Adjust therapy costs for Spring fees
df_cost_results <- df_cost_results %>%
  mutate(
    estimate = case_when(
      outcome == "Therapy" & spend_type == "Mental healthcare savings" ~ estimate + sc_cost_adjustment,
      TRUE ~ estimate
    )) %>%
  mutate(
    conf.low = estimate - 1.96*se,
    conf.high = estimate + 1.96*se
  )
