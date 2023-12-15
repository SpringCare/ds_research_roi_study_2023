# load packages
library(knitr)
library(tidyverse)
library(nerdify)
library(janitor)
library(rio)
library(lubridate)
library(table1)
library(lme4)
library(lmerTest)
library(lmeresampler)
library(car)
library(sjPlot)
library(ggplot2)
library(ggrepel)
library(marginaleffects)
library(see)
library(kableExtra)
library(gt)


## Model versus no chronic condition

df_matched_6m <- df_matched %>%
  filter(months >= -5 & months <= 6)

# get physical health only
df_matched_6m <- df_matched_6m %>%
  mutate(
    med_pmpm_trunc_mh = 
      med_pmpm_trunc_therapy_mh + 
      med_pmpm_trunc_ov_mh + 
      med_pmpm_trunc_er_mh + 
      med_pmpm_trunc_ip_mh + 
      med_pmpm_trunc_hosp_op_mh + 
      med_pmpm_trunc_other_mh,
    med_pmpm_trunc_nonmh = 
      med_pmpm_trunc_therapy_nonmh + 
      med_pmpm_trunc_ov_nonmh + 
      med_pmpm_trunc_er_nonmh + 
      med_pmpm_trunc_ip_nonmh + 
      med_pmpm_trunc_hosp_op_nonmh + 
      med_pmpm_trunc_other_nonmh
  )

m1_overall_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched_6m)

m1_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                cc_diabetes + cc_diabetes*(spring_dummy + spring_dummy*phase) +
                customer_name +
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched_6m)

m2_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                cc_hypertension + cc_hypertension*(spring_dummy + spring_dummy*phase) +
                customer_name +
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched_6m)

m3_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                cc_asthma_copd + cc_asthma_copd*(spring_dummy + spring_dummy*phase) +
                customer_name +
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched_6m)

m4_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                excl_pregnancy + excl_pregnancy*(spring_dummy + spring_dummy*phase) +
                customer_name +
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched_6m)

m5_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                cc_cancer + cc_cancer*(spring_dummy + spring_dummy*phase) +
                customer_name +
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched_6m)

m_no_cc_6m <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + 
                     cc_any + cc_any*(spring_dummy + spring_dummy*phase) +
                     customer_name +
                     (1 | carrier_member_id) +
                     (1 | subclass_nn),
                   weights = weights_nn,
                   data = df_matched_6m)

tab_model(m1_6m, m2_6m, m3_6m, m4_6m, m5_6m, 
          dv.labels = c("Diabetes", "Hypertension", "Asthma/COPD", "Pregnancy", "Cancer"),
          #show.ci=T, 
          show.se=T)

### Plot
# Make plotting df
m_list <- list(m1_6m, m2_6m, m3_6m, m4_6m, m5_6m, m_no_cc_6m)
df_cc_results <- data.frame(
  Term = NA, 
  Estimate = NA, 
  se = NA, 
  z = NA, 
  p = NA, 
  lb = NA, 
  ub = NA)

for (i in 1:6) {
  df_cc_results[i,] <- hypotheses(m_list[[i]], hypothesis = "b9 + b12 = 0")
  df_cc_results[6,] <- hypotheses(m_list[[6]], hypothesis = "b9 = 0")
}

# Add cc names
df_cc_results <- df_cc_results %>%
  mutate(
    chronic_condition = c("Diabetes", "Hypertension", "Asthma/COPD", "Pregnancy", "Cancer", "No chronic condition"),
    var_name = c("cc_diabetes", "cc_hypertension", "cc_asthma_copd", "excl_pregnancy", "cc_cancer", "cc_any")
  )

## Make it nice
df_plot_cc <- df_cc_results %>%
  mutate(
    Estimate = -1*Estimate,
    lb = -1*lb,
    ub = -1*ub
  ) %>%
  rename(ub = "lb", lb = "ub") %>%
  relocate(ub, .after = lb) %>%
  mutate(chronic_condition = fct_reorder(as.factor(chronic_condition), Estimate))


# Make dotplot
ggplot(df_plot_cc, aes(x=Estimate, xmin=ub, xmax=lb, y=chronic_condition)) +
  geom_pointrange(position = position_dodge(width=0.5)) +
  geom_text(aes(x=Estimate, y=chronic_condition, label = round(Estimate, 0), fill = NULL, vjust = -1), colour = "#8e8e8e", size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_modern() +
  theme(legend.position = "bottom") +
  labs(
    x = "PMPM savings",
    y = "",
    fill = " "
  )


ph_savings <- df_plot_cc %>%
  select(-c(Term, z, p, var_name)) %>%
  relocate(chronic_condition) %>%
  mutate(across(c(Estimate:ub), ~ round (.x,0))) 

write.csv(ph_savings, file = "physical health savings - chronic conditions.csv")




