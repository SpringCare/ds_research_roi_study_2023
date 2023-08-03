
# Models
dx_sud <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + spring_dummy*phase + 
                      (1 | carrier_member_id) +
                      (1 | subclass_nn),
                    weights = weights_nn,
                    data = filter(df_matched, dx_imputed == "sud"))

dx_mood <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + spring_dummy*phase +
                  (1 | carrier_member_id) +
                  (1 | subclass_nn),
                weights = weights_nn,
                data = filter(df_matched, dx_imputed == "mood"))

dx_anxiety <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + spring_dummy*phase + 
                     (1 | carrier_member_id) +
                     (1 | subclass_nn),
                   weights = weights_nn,
                   data = filter(df_matched, dx_imputed == "anxiety"))

dx_did <- rbind(fixef(dx_sud), fixef(dx_mood), fixef(dx_anxiety)) %>%
  data.frame() %>%
  mutate(dx = c("SUD", "Mood", "Anxiety"))

# Line plot
df_plot_dx <- df_matched %>%
  ungroup() %>%
  distinct(spring_dummy, phase) %>%
  mutate(
    phase_label = case_when(
      phase == "pre_tx" ~ "Before MH diagnosis",
      phase == "post_tx" ~ "After MH diagnosis"
    )
  ) %>%
  mutate(phase_label = factor(phase_label, levels = c("Before MH diagnosis", "After MH diagnosis")))

df_plot <- predictions(dx_sud, newdata = df_plot_dx, re.form = ~0) %>%
  mutate(dx = "SUD") %>%
  rbind(
    predictions(dx_mood, newdata = df_plot_dx, re.form = ~0) %>%
      mutate(dx = "Mood")
  ) %>%
  rbind(
    predictions(dx_anxiety, newdata = df_plot_dx, re.form = ~0) %>%
      mutate(dx = "Anxiety")
  )

color_scheme <- c('#c7c7c7', "#37837c")

# SUD
plot_sud <- ggplot(filter(df_plot, dx == "SUD"), aes(x=phase_label, y=estimate, group = spring_dummy, color = spring_dummy)) +
  geom_point(size=3) +
  geom_line() +
  scale_color_manual(values = color_scheme, labels = c("Control", "Spring Health")) +
  geom_text_repel(aes(label=round(estimate, 0))) +
  theme_modern() +
  ylim(0.9*min(df_plot$estimate), 1.1*max(df_plot$estimate)) +
  theme(legend.position = "none") +
  labs(
   # title = "Spend trajectories for Spring Health versus control group",
    x = "",
    y = "PMPM allowed amount",
    color = ""
  )

# Mood
plot_mood <- ggplot(filter(df_plot, dx == "Mood"), aes(x=phase_label, y=estimate, group = spring_dummy, color = spring_dummy)) +
  geom_point(size=3) +
  geom_line() +
  scale_color_manual(values = color_scheme, labels = c("Control", "Spring Health")) +
  geom_text_repel(aes(label=round(estimate, 0))) +
  theme_modern() +
  ylim(0.9*min(df_plot$estimate), 1.1*max(df_plot$estimate)) +
  theme(legend.position = "none") +
  labs(
    #title = "Spend trajectories for Spring Health versus control group",
    x = "",
    y = "PMPM allowed amount",
    color = ""
  )

# Anxiety
plot_anxiety <- ggplot(filter(df_plot, dx == "Anxiety"), aes(x=phase_label, y=estimate, group = spring_dummy, color = spring_dummy)) +
  geom_point(size=3) +
  geom_line() +
  scale_color_manual(values = color_scheme, labels = c("Control", "Spring Health")) +
  geom_text_repel(aes(label=round(estimate, 0))) +
  theme_modern() +
  ylim(0.9*min(df_plot$estimate), 1.1*max(df_plot$estimate)) +
  theme(legend.position = "none") +
  labs(
    #title = "Spend trajectories for Spring Health versus control group",
    x = "",
    y = "PMPM allowed amount",
    color = ""
  )

dx_plot_list <- list(plot_sud, plot_mood, plot_anxiety)

## Plot
# Make the table
dx_did %>%
  mutate(plot = NA) %>%
  mutate(
    gross_pct = 6.5*spring_dummySpring.phasepost_tx / (5.5*(X.Intercept.) + 6.5*(X.Intercept. + phasepost_tx)),
    pppy = spring_dummySpring.phasepost_tx*mean_months_treated
  ) %>%
  rename(spring_effect = spring_dummySpring.phasepost_tx) %>%
  select(spring_effect, pppy, gross_pct, plot) %>%
  tibble() %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = `plot`),
    fn = function(x) {
      dx_plot_list %>%
        ggplot_image(height = px(160))
    }
  ) %>%
  tab_options(column_labels.font.weight = "bold") %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_column_labels()
  ) #%>%
  tab_style(
    style = cell_text(weight = "bold"), 
    locations = cells_body(columns = c("Service category", "PMPM savings"))
  )
  
  
  
  
  dx_preg <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + spring_dummy*phase + 
                   (1 | carrier_member_id) +
                   (1 | subclass_nn),
                 weights = weights_nn,
                 data = filter(df_matched, excl_pregnancy == "yes"))
summary(dx_preg)  
