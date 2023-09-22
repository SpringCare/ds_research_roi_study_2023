
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

spend_cat <- rep(c("Office visits", "Emergency room", "Hospital outpatient", "Hospital inpatient", "Therapy", "Other (including platform fees)"), 2)
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


# Estimate total BH costs at spring
spring_bh_fees <- df_matched %>% 
  filter(spring_dummy == "Spring") %>% 
  group_by(carrier_member_id) %>% 
  arrange(carrier_member_id, months) %>% 
  slice_tail() %>% 
  select(session_count_total) %>%
  ungroup() %>%
  summarize(total_sessions = sum(session_count_total, na.rm=T)) %>%
  mutate(spring_bh_fees = total_sessions*195) %>%
  pull(spring_bh_fees)

spring_bh_adjustment = spring_bh_fees/member_months
spring_platform_adjustment <- spring_cost_adjustment - spring_bh_adjustment

df_cc_plot <- df_cc_list %>%
  bind_rows() %>%
  filter(phase == "post_tx") %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(spring_dummy == "Spring" & spend_category == "Therapy" & mh == "Mental health", .x + spring_bh_adjustment, .x))) %>% 
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ ifelse(spring_dummy == "Spring" & spend_category == "Other (including platform fees)" & mh == "Mental health", .x + spring_platform_adjustment, .x))) %>% 
  select(spring_dummy, spend_category, mh, estimate, conf.low, conf.high) %>%
  mutate(spring_dummy = factor(spring_dummy, levels = c("Health Plan", "Spring")))



p1 <- ggplot(filter(df_cc_plot, spend_category == "Therapy" & mh == "Mental health"), aes(x=spend_category, y=estimate, ymin = conf.low, ymax = conf.high, fill = spring_dummy, group = spring_dummy)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge()) +
  geom_errorbar(position = position_dodge(0.6), width = .1, color = "gray45") +
  scale_fill_manual(values = rev(cc_colors), labels = c("Control", "Program")) +
  geom_text(aes(x=spend_category, y=conf.high, label = round(estimate, 0), fill = NULL), colour = "gray35", size = 4, vjust=-0.3, position = position_dodge(width = 0.6)) +
  #facet_grid(. ~ mh) +
  theme_modern() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=18)) +
  theme(legend.text =  element_text(size=14)) +
  labs(
    x = "",
    y = "PMPM spend",
    fill = " "
  )
p1
p2 <- ggplot(filter(df_cc_plot, spend_category != "Therapy"), aes(x=spend_category, y=estimate, ymin = conf.low, ymax = conf.high, fill = spring_dummy, group = spring_dummy)) +
  geom_bar(stat = "identity", width = 0.6, position = position_dodge()) +
  geom_errorbar(position = position_dodge(0.6), width = .1, color = "gray45") +
  scale_fill_manual(values = rev(cc_colors), labels = c("Control", "Program")) +
  geom_text(aes(x=spend_category, y=conf.high, label = round(estimate, 0), fill = NULL), colour = "gray35", size = 4, vjust=-0.3, position = position_dodge(width = 0.6)) +
  facet_grid(mh ~ ., scales = "free") +
  theme_modern() +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  theme(axis.title.x = element_text(size=18)) +
  theme(legend.text =  element_text(size=14)) +
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    strip.text = element_text(size=15)
  ) +
  labs(
    x = "",
    y = "PMPM spend",
    fill = " "
  )

plot_legend <- get_legend(p2)
p2_no_legend <- p2 + theme(legend.position = "none")

library(cowplot)

pg1 <- plot_grid(
  NULL, p1, NULL,
  nrow = 3,
  rel_heights = c(1, 3.5, 1)
)


plot_grid(
  pg1, p2_no_legend, NULL, plot_legend, 
  align = 'v',
  ncol=2,
  nrow=2,
  rel_widths = c(1.2,3),
  rel_heights = c(1, .06)
)



