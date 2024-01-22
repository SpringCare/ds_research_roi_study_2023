
# SPlit to MH and PH
df_matched <- df_matched %>%
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

# Make binary indicator variablers
df_matched <- df_matched %>%
  mutate(
    Hearst = ifelse(customer_name == "Hearst", 1, 0),
    Pepsi = ifelse(customer_name == "Pepsi", 1, 0),
    Tegna = ifelse(customer_name == "Tegna", 1, 0),
    Wellstar = ifelse(customer_name == "Wellstar", 1, 0),
    DocuSign = ifelse(customer_name == "DocuSign", 1, 0)
  )

# Model Mental health vs Physical health
m1_mh <- lmer(med_pmpm_trunc_mh ~ 1 + spring_dummy + spring_dummy*phase + Hearst + DocuSign + Tegna + Wellstar +
                (1 | carrier_member_id) +
                (1 | subclass_nn),
              weights = weights_nn,
              data = df_matched)

m1_nonmh <- lmer(med_pmpm_trunc_nonmh ~ 1 + spring_dummy + spring_dummy*phase + Hearst + DocuSign + Tegna + Wellstar +
                   (1 | carrier_member_id) +
                   (1 | subclass_nn),
                 weights = weights_nn,
                 data = df_matched)

# By category

cc1 <- lmer(med_pmpm_trunc_ov_mh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc2 <- lmer(med_pmpm_trunc_er_mh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc3 <- lmer(med_pmpm_trunc_hosp_op_mh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc4 <- lmer(med_pmpm_trunc_ip_mh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc5 <- lmer(med_pmpm_trunc_therapy_mh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc6 <- lmer(med_pmpm_trunc_other_mh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

## Non-MH
cc7 <- lmer(med_pmpm_trunc_ov_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc8 <- lmer(med_pmpm_trunc_er_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc9 <- lmer(med_pmpm_trunc_hosp_op_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc10 <- lmer(med_pmpm_trunc_ip_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
              Hearst + DocuSign + Tegna + Wellstar + 
              (1 | carrier_member_id) +
              (1 | subclass_nn),
            weights = weights_nn,
            data = df_matched)

cc11 <- lmer(med_pmpm_trunc_therapy_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
               Hearst + DocuSign + Tegna + Wellstar + 
               (1 | carrier_member_id) +
               (1 | subclass_nn),
             weights = weights_nn,
             data = df_matched)

cc12 <- lmer(med_pmpm_trunc_other_nonmh ~ 1 + spring_dummy + spring_dummy*phase +
               Hearst + DocuSign + Tegna + Wellstar + 
               (1 | carrier_member_id) +
               (1 | subclass_nn),
             weights = weights_nn,
             data = df_matched)


spend_cat <- rep(c("Office visits", "Emergency room", "Hospital outpatient", "Hospital inpatient", "Behavioral Health", "Other"), 2)
mh_spend <- c(rep("Mental health", 6), rep("Physical health", 6))
df_cc_list <- list()

for (i in 1:12) {
  df_temp <- predictions(get(paste0("cc", i)), newdata = df_plot, re.form = ~0) %>%
    mutate(
      spend_category = spend_cat[i],
      mh = mh_spend[i]
      )
  df_cc_list[[i]] <- df_temp
}

df_cc_plot <- df_cc_list %>%
  bind_rows() %>%
  filter(phase == "post_tx") %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(spring_dummy == "Spring" & spend_category == "Behavioral Health" & mh == "Mental health", .x + spring_cost_adjustment, .x))) %>% 
  select(spring_dummy, spend_category, mh, estimate, conf.low, conf.high) %>%
  mutate(spring_dummy = factor(spring_dummy, levels = c("Health Plan", "Spring"))) %>%
  mutate(spend_category = factor(spend_category, levels = c("Office visits", "Emergency room", "Hospital outpatient", "Hospital inpatient", "Behavioral Health")))


# Overall scale
y_total <- 650
y_cat <- 275


# Overall MH
df_plot <- df_matched %>%
  ungroup() %>%
  distinct(spring_dummy, phase) %>%
  cbind(cust_recode) %>%
  mutate(
    phase_label = case_when(
      phase == "pre_tx" ~ "Before MH diagnosis",
      phase == "post_tx" ~ "After MH diagnosis"
    )
  ) %>%
  mutate(phase_label = factor(phase_label, levels = c("Before MH diagnosis", "After MH diagnosis")))

# spring_cost_adjustment <- # total fees for study members / total member months for individuals in the study. Not programmed for ROI template yet.

df_plot_mh <- predictions(m1_mh, newdata = df_plot, re.form = ~0) %>%
  mutate(spend_category = "Mental Health - Overall") %>%
  filter(phase == "post_tx") %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(spring_dummy == "Spring" & phase == "post_tx", .x + spring_cost_adjustment, .x))) %>%
  mutate(spring_dummy = ifelse(spring_dummy == "Spring", "Program", "Control"))

p1 <- ggplot(df_plot_mh, aes(x=spring_dummy, y=estimate, ymin = conf.low, ymax = conf.high, fill = spring_dummy, group = spring_dummy)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(width = .1, color = "gray45") +
  scale_fill_manual(values = rev(cc_colors), labels = c("Control", "Program")) +
  geom_text(aes(x=spring_dummy, y=conf.high, label = round(estimate, 0), fill = NULL), colour = "gray35", size = 4, vjust=-0.3) +
  scale_y_continuous(breaks = seq(0, y_total, by = 200), limits = c(0, y_total)) +
  theme_modern() +
  theme(panel.grid.major.y = element_line(color = "gray85")) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=18)) +
  theme(legend.text =  element_text(size=14)) +
  theme(
    legend.position = "none",
    legend.justification = "right",
    strip.text = element_text(size=15)
  ) +
  labs(
    title = "Mental Health Costs - Overall",
    x = "",
    y = "PMPM spend",
    fill = " "
  )
p1  

# Overall PH
df_plot_nonmh <- predictions(m1_nonmh, newdata = df_plot, re.form = ~0) %>%
  filter(phase == "post_tx") %>%
  mutate(spend_category = "Physical Health - Overall") %>%
  mutate(spring_dummy = ifelse(spring_dummy == "Spring", "Program", "Control"))

p2 <- ggplot(df_plot_nonmh, aes(x=spring_dummy, y=estimate, ymin = conf.low, ymax = conf.high, fill = spring_dummy, group = spring_dummy)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(width = .1, color = "gray45") +
  scale_fill_manual(values = rev(cc_colors), labels = c("Control", "Program")) +
  geom_text(aes(x=spring_dummy, y=conf.high, label = round(estimate, 0), fill = NULL), colour = "gray35", size = 4, vjust=-0.3) +
  scale_y_continuous(breaks = seq(0, y_total, by = 200), limits = c(0, y_total)) +
  theme_modern() +
  theme(panel.grid.major.y = element_line(color = "gray85")) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=18)) +
  theme(legend.text =  element_text(size=14)) +
  theme(
    legend.position = "none",
    legend.justification = "right",
    strip.text = element_text(size=15)
  ) +
  labs(
    title = "Physical Health Costs - Overall",
    x = "",
    y = "PMPM spend",
    fill = " "
  )
p2  

# MH breakdown
p3 <- ggplot(filter(df_cc_plot, mh == "Mental health"), aes(x=spend_category, y=estimate, ymin = conf.low, ymax = conf.high, fill = spring_dummy, group = spring_dummy)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge()) +
  geom_errorbar(position = position_dodge(0.6), width = .1, color = "gray45") +
  scale_fill_manual(values = rev(cc_colors), labels = c("Control", "Program")) +
  geom_text(aes(x=spend_category, y=conf.high, label = round(estimate, 0), fill = NULL), colour = "gray35", size = 4, vjust=-0.3, position = position_dodge(width = 0.6)) +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(breaks = seq(0, y_cat, by = 100), limits = c(-20, 275)) +
  theme_modern() +
  theme(panel.grid.major.y = element_line(color = "gray85")) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=18)) +
  theme(legend.text =  element_text(size=14)) +
  theme(
    legend.position = "none",
    legend.justification = "right",
    strip.text = element_text(size=15)
  ) +
  labs(
    title = "Mental Health Costs by Service Category",
    x = "",
    y = "PMPM spend",
    fill = " "
  )
p3

# PH breakdown
p4 <- ggplot(filter(df_cc_plot, mh == "Physical health" & spend_category != "Behavioral Health"), aes(x=spend_category, y=estimate, ymin = conf.low, ymax = conf.high, fill = spring_dummy, group = spring_dummy)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge()) +
  geom_errorbar(position = position_dodge(0.6), width = .1, color = "gray45") +
  scale_fill_manual(values = rev(cc_colors), labels = c("Control", "Program")) +
  geom_text(aes(x=spend_category, y=conf.high, label = round(estimate, 0), fill = NULL), colour = "gray35", size = 4, vjust=-0.3, position = position_dodge(width = 0.6)) +
  scale_x_discrete(labels = scales::label_wrap(10)) +
  scale_y_continuous(breaks = seq(0, y_cat, by = 100), limits = c(-20, 275)) +
  theme_modern() +
  theme(panel.grid.major.y = element_line(color = "gray85")) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=18)) +
  theme(legend.text =  element_text(size=14)) +
  theme(
    legend.position = "none",
    legend.justification = "right",
    strip.text = element_text(size=15)
  ) +
  labs(
    title = "Physical Health Costs by Service Category",
    x = "",
    y = "PMPM spend",
    fill = " "
  )
p4


cowplot::plot_grid(p1, p2, p3, p4, NA, fig_legend, nrow=3, rel_heights = c(1,1,.07))

