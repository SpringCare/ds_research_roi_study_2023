df_mh_spend <- df_matched %>%
  filter(months >= -5, months <= 6) %>%
  group_by(customer_name, spring_dummy, phase) %>%
  summarize(
    med_gross = sum(med_pmpm_trunc, na.rm=T),
    member_months = n()
  ) %>%
  select(customer_name, spring_dummy, phase, member_months, med_gross) %>%
  pivot_wider(names_from = c(spring_dummy, phase), values_from = c(member_months, med_gross)) %>%
  mutate(did = (`med_gross_Health Plan_pre_tx`/`member_months_Health Plan_pre_tx` - `med_gross_Health Plan_post_tx`/`member_months_Health Plan_post_tx`) - (`med_gross_Spring_pre_tx`/`member_months_Spring_pre_tx` - `med_gross_Spring_post_tx`/`member_months_Spring_post_tx`))

member_months_12m <- df_matched %>%
  filter(phase == "post_tx", spring_dummy == "Spring") %>%
  group_by(customer_name, spring_dummy) %>%
  summarize(
    member_months_12m = n()
  ) %>%
  select(customer_name, member_months_12m) 

# Convert to 6 month fees
fees_12m <- spring_fees %>%
  mutate(customer_name = ifelse(customer_name == "Docusign", "DocuSign", customer_name)) %>%
  group_by(customer_name) %>%
  summarize(amount = sum(Amount)) 

df_roi <- df_mh_spend %>%
  mutate(customer_name = ifelse(customer_name == "Docusign", "DocuSign", customer_name)) %>%
  left_join(fees_12m) %>%
  left_join(member_months_12m) %>%
  mutate(roi = (did * `member_months_Spring_post_tx`) / (amount * (`member_months_Spring_post_tx` / member_months_12m))) %>%
  select(customer_name, roi) %>%
  mutate(roi = round(roi, 2))


m1_gross <- lmer(med_pmpm_trunc ~ 1 + spring_dummy + spring_dummy*phase +
                 (1 | carrier_member_id) +
                 (1 | subclass_nn),
               weights = weights_nn,
               data = df_matched)
