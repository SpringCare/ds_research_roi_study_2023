

df_sud_all <- df_analyze_all %>%
  group_by(carrier_member_id) %>%
  arrange(carrier_member_id, month_year) %>%
  mutate(
    carrier_mh_dx = first(carrier_primary_mh_dx, na_rm = TRUE),
    spring_mh_dx = first(spring_primary_mh_dx, na_rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(mh_dx = coalesce(spring_mh_dx, carrier_mh_dx)) %>%
  filter(mh_dx == "sud") 


df_sud_matched <- df_matched %>%
  group_by(carrier_member_id) %>%
  arrange(carrier_member_id, month_year) %>%
  mutate(
    carrier_mh_dx = first(carrier_primary_mh_dx, na_rm = TRUE),
    spring_mh_dx = first(spring_primary_mh_dx, na_rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(mh_dx = coalesce(spring_mh_dx, carrier_mh_dx)) %>%
  filter(mh_dx == "sud")


sud_summary <- df_sud_all %>%
  filter(months >= -11 | months <= 12) %>%
  group_by(spring_dummy, phase) %>%
  summarize(
  pmpm = round(mean(med_pmpm, na.rm=T), 0)
  )

sud_pmpm <- df_sud_all %>%
  filter(months >= 1 | months <= 12) %>%
  group_by(spring_dummy, months_post) %>%
  summarize(
    pmpm = round(mean(med_pmpm, na.rm=T), 0)
  ) %>%
  group_by(spring_dummy) %>%
  summarize(pmpm = mean(pmpm))

df_sud_matched %>%
  group_by(spring_dummy, phase) %>%
  summarize(
    pmpm = mean(med_pmpm, na.rm=T)
  )
