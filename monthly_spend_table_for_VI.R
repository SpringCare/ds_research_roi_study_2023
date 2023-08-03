
# Group by month
df_monthly <- df_matched %>%
  group_by(spring_dummy, months) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_net = sum(med_pmpm_trunc_net, na.rm=T),
    member_months = n_distinct(carrier_member_id),
    sd = sd(med_pmpm_trunc_net, na.rm=T)
  ) %>%
  mutate(
    pmpm_gross = med_gross/member_months,
    pmpm_net = med_net/member_months,
    se = sd / sqrt(member_months)
    ) 

df_vi <- df_monthly %>%
  select(spring_dummy, months, member_months, med_gross, med_net, pmpm_gross, pmpm_net) %>%
  pivot_wider(names_from = "spring_dummy", values_from = c("member_months", "med_gross", "med_net", "pmpm_gross", "pmpm_net"), names_vary = "slowest") 
write.table(df_vi, file = "code/output/monthly spend table.csv", row.names = F, col.names = T)


ggplot(df_monthly, aes(x=months, y=pmpm_gross, ymin = pmpm_gross - 1.96*se, ymax = pmpm_gross + 1.96*se, group = spring_dummy, color = spring_dummy)) +
  #geom_errorbar() +
  geom_line() +
  theme_modern()

ggplot(df_monthly, aes(x=months, y=pmpm_net, ymin = pmpm_net - 1.96*se, ymax = pmpm_net + 1.96*se, group = spring_dummy, color = spring_dummy)) +
  #geom_errorbar() +
  geom_line() +
  theme_modern()



# Now do by follow-up group
df_follow_up_group <- df_matched %>%
  filter(month_year >= index_date) %>%
  filter(months <= 12) %>%
  filter(month_year <= spring_end_date) %>%
  mutate(months_follow_up = interval(floor_date(index_date, unit="months"), floor_date(spring_end_date, unit = "months")) %/% months(1) + 1) %>%
  group_by(spring_dummy, months_follow_up) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    med_net = sum(med_pmpm_trunc_net, na.rm=T),
    n_ids = n_distinct(carrier_member_id),
    member_months = n(),
    sd = sd(med_pmpm_trunc_net, na.rm=T)
  ) %>%
  mutate(
    pmpm_gross = med_gross/member_months,
    pmpm_net = med_net/member_months
  ) 

# Check member months match in both groups
df_monthly %>%
  filter(months >= 1) %>%
  group_by(spring_dummy) %>%
  summarize(n = sum(member_months))

df_follow_up_group %>%
  group_by(spring_dummy) %>%
  summarize(
    n_months = sum(member_months),
    n_ids = sum(n_ids)
    )

ggplot(df_follow_up_group, aes(x=months_follow_up, y=pmpm_gross, group = spring_dummy, color = spring_dummy)) +
  geom_line() +
  theme_modern()


ggplot(df_follow_up_group, aes(x=months_follow_up, y=pmpm_net, group = spring_dummy, color = spring_dummy)) +
  geom_line() +
  theme_modern()



df_matched %>%
  filter(spring_dummy == "Spring") %>%
  filter(month_year == first_spring_tx_date) %>%
  mutate(months_treated = interval(first_spring_tx_date, spring_end_date) %/% months(1) + 1) %>%
  group_by(customer_name, months_treated) %>%
  summarize(n = n()) %>%
  mutate(
    member_months_treated = n*months_treated,
    month_roi_gross = n*months_treated*summary(m1_gross_yes_match)$coefficients["spring_dummySpring:phasepost_tx", 1]
    ) %>%
  group_by(customer_name) %>%
  summarize(amount = sum(month_roi_gross))
