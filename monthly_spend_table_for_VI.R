# All participants stratified by MH spend
df_mh_spend <- df_matched %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_gross_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    med_gross_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  select(spring_dummy, months, member_months, med_gross, med_gross_mh, med_gross_nonmh) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_gross_mh", "med_gross_nonmh"), names_vary = "slowest") 
#write.table(df_mh_spend, file = "code/output/monthly spend table - mh vs non-mh.csv", row.names = F, col.names = T)

# Break MH spend out by service location
df_service_cat_monthly <- df_matched %>%
  group_by(spring_dummy, phase, months) %>%
  summarize(
    ov_mh = sum(med_pmpm_trunc_ov_mh, na.rm=T),
    er_mh = sum(med_pmpm_trunc_er_mh, na.rm=T),
    hosp_out_mh = sum(med_pmpm_trunc_hosp_op_mh, na.rm=T),
    hosp_in_mh = sum(med_pmpm_trunc_ip_mh, na.rm=T),
    bh_mh = sum(med_pmpm_trunc_therapy_mh, na.rm=T),
    other_mh = sum(med_pmpm_trunc_other_mh, na.rm=T),
    ov_nonmh = sum(med_pmpm_trunc_ov_nonmh, na.rm=T),
    er_nonmh = sum(med_pmpm_trunc_er_nonmh, na.rm=T),
    hosp_out_nonmh = sum(med_pmpm_trunc_hosp_op_nonmh, na.rm=T),
    hosp_in_nonmh = sum(med_pmpm_trunc_ip_nonmh, na.rm=T),
    other_nonmh = sum(med_pmpm_trunc_other_nonmh, na.rm=T),
    total_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    total_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  filter(months >= -5, months <= 6) %>%
  mutate(spring_dummy = ifelse(spring_dummy == "Spring", "Spring", "Control"))
#write.table(df_service_cat_monthly, file = "code/output/monthly spend table - service categories.csv", row.names = F, col.names = T)

df_service_cat_summary <-  df_service_cat_monthly %>%
  group_by(spring_dummy, phase) %>%
  summarize(across(ov_mh:total_nonmh, ~ sum(.x) / sum(member_months)),
            sum_member_months = sum(member_months)
            ) %>%
  mutate(bh_mh = ifelse(phase == "post_tx" & spring_dummy == "Spring", bh_mh + 3353699/sum_member_months, bh_mh)) %>% # add in spring total invoiced)
  select(-c(sum_member_months)) %>%
  pivot_longer(-c(spring_dummy, phase), names_to = "service_category", values_to = "pmpm") %>%
  mutate(
    spend_type = case_when(
        str_detect(service_category, "nonmh") ~ "Physical Health",
        TRUE ~ "Mental Health"
        ),
    service_category = case_when(
      str_detect(service_category, "ov_") ~ "Office Visit",
      str_detect(service_category, "other_") ~ "Other",
      str_detect(service_category, "er_") ~ "Emergency Room",
      str_detect(service_category, "hosp_out") ~ "Hospital Outpatient",
      str_detect(service_category, "hosp_in") ~ "Hospital Inpatient",
      str_detect(service_category, "bh") ~ "Behavioral Health",
      str_detect(service_category, "total") ~ "Total",
    )
  ) %>%
  #filter(phase == "post_tx") %>%
  group_by(service_category, spend_type) %>%
  mutate(pmpm_normalized = pmpm / max(pmpm)) %>%
  mutate(spring_dummy = ifelse(spring_dummy == "Spring", "Spring", "Control"))
#write.table(df_service_cat_summary, file = "code/output/summary spend table - service categories.csv", row.names = F, col.names = T)

ggplot(filter(df_service_cat_summary, phase =="post_tx"), (aes(x=service_category, y=pmpm, group=spring_dummy, fill=spring_dummy))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ spend_type)

# cancer
df_cancer <- df_matched %>%
  filter(cc_cancer == "yes") %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_gross_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    med_gross_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  select(spring_dummy, months, member_months, med_gross, med_gross_mh, med_gross_nonmh) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_gross_mh", "med_gross_nonmh"), names_vary = "slowest") 
#write.table(df_cancer, file = "code/output/monthly spend table - cancer.csv", row.names = F, col.names = T)

# hypertension
df_hypertension <- df_matched %>%
  filter(cc_hypertension == "yes") %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_gross_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    med_gross_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  select(spring_dummy, months, member_months, med_gross, med_gross_mh, med_gross_nonmh) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_gross_mh", "med_gross_nonmh"), names_vary = "slowest") 
#write.table(df_hypertension, file = "code/output/monthly spend table - hypertension.csv", row.names = F, col.names = T)


# Pregnancy
df_pregnancy <- df_matched %>%
  filter(excl_pregnancy == "yes") %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_gross_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    med_gross_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  select(spring_dummy, months, member_months, med_gross, med_gross_mh, med_gross_nonmh) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_gross_mh", "med_gross_nonmh"), names_vary = "slowest") 
#write.table(df_pregnancy, file = "code/output/monthly spend table - pregnancy.csv", row.names = F, col.names = T)

# Diabetes
df_diabetes <- df_matched %>%
  filter(cc_diabetes == "yes") %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_gross_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    med_gross_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  select(spring_dummy, months, member_months, med_gross, med_gross_mh, med_gross_nonmh) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_gross_mh", "med_gross_nonmh"), names_vary = "slowest") 
#write.table(df_diabetes, file = "code/output/monthly spend table - diabetes.csv", row.names = F, col.names = T)


# Asthma/COPD
df_asthma_copd <- df_matched %>%
  filter(cc_asthma_copd == "yes") %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_gross_mh = sum(med_pmpm_trunc_mh, na.rm=T),
    med_gross_nonmh = sum(med_pmpm_trunc_nonmh, na.rm=T),
    member_months = n_distinct(carrier_member_id)
  ) %>%
  select(spring_dummy, months, member_months, med_gross, med_gross_mh, med_gross_nonmh) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_gross_mh", "med_gross_nonmh"), names_vary = "slowest") 
#write.table(df_asthma_copd, file = "code/output/monthly spend table - asthma and copd.csv", row.names = F, col.names = T)


# Plot VI 

df_names <- c("asthma_copd", "cancer", "diabetes", "hypertension", "pregnancy", "All participants - MH vs non-MH")
df_vi <- list()
for (i in 1:6) {
df_vi[[i]] <- read_xlsx("code/output/chronic condition spend table.xlsx", sheet = paste0(df_names[i]), range = "A2:F7", col_names = TRUE) %>%
  data.frame() %>%
  mutate(condition = df_names[i])
}
df_vi <- df_vi %>% 
  bind_rows() %>% 
  filter(Time.Period == "Total" | Time.Period == "Proportion MH spend") %>%
    select(Time.Period, DID, condition) %>%
    pivot_wider(names_from = Time.Period, values_from = DID) 

df_vi_long <- df_vi %>%
  mutate(
    mh = `Proportion MH spend`*round(Total,0)*6, # 6 months
    ph = (1 - `Proportion MH spend`)*round(Total,0)*6
) %>%
  select(condition, mh, ph) %>%
  pivot_longer(cols = c(mh, ph), names_to = "spend_type") %>%
  filter(condition != "pregnancy", condition != "asthma_copd") %>%
  mutate(
    condition = case_when(
      condition == "All participants - MH vs non-MH" ~ "Population average",
      condition == "cancer" ~ "Cancer",
      condition == "diabetes" ~ "Diabetes",
      condition == "hypertension" ~ "Hypertension",
      TRUE ~ condition),
    spend_type = case_when(
      spend_type == "mh" ~ "Mental healthcare",
      spend_type == "ph" ~ "Physical healthcare"
      ))%>%
  mutate(condition = factor(condition, levels = c("Population average", "Hypertension", "Diabetes", "Cancer")))
openxlsx::write.xlsx(df_vi_long, file = "chronic conditions table - VI GTM deck.xlsx")


## For barplot, we want overall value labels
df_labels <- df_vi_long %>%
  group_by(condition) %>%
  summarize(total = sum(value)) 


ggplot(df_vi_long, aes(x=value, y=condition, fill = spend_type)) +
  geom_bar(stat = "identity", width = 0.6, position=position_stack(reverse=T)) +
  scale_fill_manual(values = cc_colors) +
  geom_text(aes(x=total, y=condition, label = scales::dollar(total), fill = NULL, hjust = ifelse(total < 0, 1.1, -0.4)), colour = "#8e8e8e", size = 5, data = df_labels) +
  expand_limits(x=c(0, 1.2*max(df_labels$total))) +
  scale_x_continuous(labels = scales::dollar_format()) + 
  theme_modern() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size=14),
    axis.text = element_text(size=14),
    axis.title = element_text(size=18)
    ) +
  labs(
    x = "6 month savings",
    y = "",
    fill = ""
  )


## Add donut chart *gag* 
# Compute percentages
df_donut <- df_vi_long %>%
  filter(condition == "Population average") %>%
  group_by(spend_type) %>%
  summarize(total = sum(value)) %>%
  ungroup() %>%
  mutate(fraction = total / sum(total)) %>%
  mutate(
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n=-1))
  ) %>%
  mutate(labelPosition = (ymax + ymin) /2)


# Make the plot
ggplot(df_donut, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=spend_type)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_modern() +
  theme_void() +
  scale_fill_manual(values = cc_colors) + 
  geom_text(aes(x=3.5, y=labelPosition, label=paste0(100*round(df_donut$fraction, 2), "%")), fontface = "bold", size = 6) +
  labs(fill = "") +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size=14)
    )
