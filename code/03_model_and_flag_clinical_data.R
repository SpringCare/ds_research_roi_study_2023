library(lme4)
library(lmerTest)

# currently have all the clinical data for members in the matching study.
# Now, need to model it through study end dates.
# We want 3 values:
  # 1. Numeric change. Last value in month minus baseline value. If they eventually get RC, they are coded as 1.
  # 2. Time-varying values of RC.
  # 3. pre-post numeric values
  # 4. Time-varying numeric values.


# Model overall outcomes
## PHQ
phq_cubic <- lmer(phq ~ time_6m + time_6m_square + time_6m_cube + 
                    (1 + time_6m | carrier_member_id),
                  control = lmerControl(optimizer="bobyqa"),
                  REML = T,
                  data = filter(df_outcomes, phq9_baseline >= 10))


## GAD
gad_cubic <- lmer(gad ~ time_6m + time_6m_square + time_6m_cube + 
                    (1 + time_6m | carrier_member_id),
                  control = lmerControl(optimizer="bobyqa"),
                  REML = T,
                  data = filter(df_outcomes, gad7_baseline >= 10))

tab_model(phq_cubic, gad_cubic, show.ci=F, show.se=T)
# Outcomes on par with what we've seen for other customers

## Plot outcomes
df_plot_outcomes <- df_outcomes %>%
  distinct(time_6m, time_6m_square, time_6m_cube) %>%
  filter(time_6m >=0 & time_6m <=1) %>%
  mutate(months = time_6m*6)

df_plot_phq <- marginaleffects::predictions(phq_cubic, newdata = df_plot_outcomes, re.form = ~0)
df_plot_gad <- marginaleffects::predictions(gad_cubic, newdata = df_plot_outcomes, re.form = ~0)

# Plot PHQ
p1 <- ggplot(data = df_plot_phq, aes(x = months, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + guides(alpha = "none") +
  theme_modern() +
  labs(x = "Months",
       y = "PHQ-9 score",
       title = "Change in depression over time"
  )

# Plot GAD
p2 <- ggplot(data = df_plot_gad, aes(x = months, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) + guides(alpha = "none") +
  theme_modern() +
  labs(x = "Months",
       y = "GAD-7 score",
       title = "Change in anxiety over time"
  )

plot_grid(list(p1,p2))

df_phq_rc <- df_outcomes %>%
  group_by(carrier_member_id) %>%
  arrange(month_year) %>%
  filter(!is.na(phq_rc)) %>%
  slice_tail() %>%
  mutate(phq_baseline_severity = ifelse(phq9_baseline >= 10, "Baseline screen positive", "Baseline screen negative")) %>%
  select(carrier_member_id, phq9_baseline, phq_baseline_severity, phq_change, phq_rc)

df_gad_rc <- df_outcomes %>%
  group_by(carrier_member_id) %>%
  arrange(month_year) %>%
  filter(!is.na(gad_rc)) %>%
  slice_tail() %>%
  mutate(gad_baseline_severity = ifelse(gad7_baseline >= 10, "Baseline screen positive", "Baseline screen negative")) %>%
  select(carrier_member_id, gad7_baseline, gad_baseline_severity, gad_change, gad_rc)


# Make change table
## phq
label(df_phq_rc$phq_rc) <- "Reliable change in depression"
label(df_phq_rc$phq_change) <- "Total change in depression"
label(df_gad_rc$gad_rc) <- "Reliable change in anxiety"
label(df_gad_rc$gad_change) <- "Total change in anxiety"

df_rc <- df_phq_rc %>%
  full_join(df_gad_rc)

table1( ~ phq_rc + phq_change | phq_baseline_severity,
        data = df_phq_rc,
        render.missing=NULL,
        topclass = "Baseline severity",
        render.categorical="FREQ (PCTnoNA%)")

## gad
table1( ~ gad_rc + gad_change | gad_baseline_severity,
        data = df_gad_rc,
        render.missing=NULL,
        render.categorical="FREQ (PCTnoNA%)")



df_long <- df_matched %>%
  left_join(df_outcomes) %>%
  group_by(carrier_member_id) %>%
  arrange(month_year) %>%
  fill(c(phq9_baseline, gad7_baseline), .direction = "down") %>%
  group_by(subclass_nn, month_year) %>%
  fill(c(phq9_baseline, gad7_baseline, phq, gad, phq_change, gad_change, phq_rc, gad_rc, any_rc), .direction="downup") %>%
  group_by(carrier_member_id) %>%
  mutate(pmpm_base = mean(med_pmpm_trunc[months_post==0], na.rm=T)) %>%
  ungroup() %>%
  filter(months_post > 0) %>%
  mutate(pmpm_diff = med_pmpm_trunc - pmpm_base) 

# Main model

# Numeric
m1_phq <- lmer(pmpm_diff ~ 1 + spring_dummy + as.factor(months_post)*spring_dummy + phq*spring_dummy + 
                (1 | carrier_member_id),
              weights = weights_nn,
              data = df_long)

m1_gad <- lmer(pmpm_diff ~ 1 + spring_dummy + as.factor(months_post)*spring_dummy + gad*spring_dummy + 
                 (1 | carrier_member_id),
               weights = weights_nn,
               data = df_long)

# Binary
m2_rc <- lmer(pmpm_diff ~ 1 + spring_dummy + as.factor(months_post)*spring_dummy + any_rc*spring_dummy + 
                (1 | carrier_member_id),
              weights = weights_nn,
              data = df_long)

tab_model(m1_phq, m1_gad, m2_rc, 
          #show.ci=F, 
          #show.se=T,
          show.icc = F,
          show.re.var = F,
          show.r2 = F,
          show.aic = T,
          digits=0,
          drop = c("months_post", "(Intercept)"),
          dv.labels = c("PHQ score (continuous)", "GAD score (continuous)", "Any reliable change (binary)"),
          title = "Relationship between clinical improvement and medical spending (negative coefficients = savings)"
)

# Does it vary by time? We see early savings in month 1 that may be confounding
m3_rc <- lmer(pmpm_diff ~ 1 + spring_dummy + as.factor(months_post)*any_rc*spring_dummy + (1 | carrier_member_id),
            weights = weights_nn,
            data = df_long)

m4_rc <- lmer(pmpm_diff ~ 1 + spring_dummy + as.factor(months_post)*phq*spring_dummy + (1 | carrier_member_id),
              weights = weights_nn,
              data = df_long)

m5_rc <- lmer(pmpm_diff ~ 1 + spring_dummy + as.factor(months_post)*gad*spring_dummy + (1 | carrier_member_id),
              weights = weights_nn,
              data = df_long)



## Plot time-varying effect
df_plot <- list()
for (i in 1:12) {
  df_loop <- data.frame()
  if(i == 1) {df_loop <- hypotheses(m3_rc, "b37=0")}
  if(i > 1) {df_loop <- hypotheses(m3_rc, paste0("b37 + b", i+36, "=0"))}
  df_loop$months <- i
  df_plot[[i]] <- df_loop
}
df_plot <- df_plot %>% bind_rows()

ggplot(df_plot, aes(x=months, y = estimate, ymin=conf.low, ymax=conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_cartesian(ylim = c(-3000, 3000)) +
  theme_modern() +
  labs(
    x = "Months post-diagnosis",
    y = "Triple difference (negative is better)",
    title = "Association between reliable change and Spring's impact on claims spend"
  )


