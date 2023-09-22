# session types for Todd

matched_ids <- df_matched %>%
  filter(spring_dummy == "Spring", months_post >= 1, months_post <= 6) %>%
  select(carrier_member_id, month_year)

elig <- list()
for (i in 1:length(customer_name)) {
  elig[[i]] <- import(paste0(Sys.getenv("claims_folder_path"), "data/", customer_name[i], "/data_to_sba/", customer_name[i], "_", "eligibility_cls.csv")) %>%
    rename(any_of(c(
      carrier_member_id = "member_key",
      carrier_member_id = "indv_id",
      carrier_member_id = "individual_id"
    ))) %>%
    mutate(customer_name = .env$customer_name[i]) %>%
    mutate(carrier_member_id = as.character(carrier_member_id))
}

elig2 <- elig %>% 
  bind_rows() %>%
  mutate(carrier_member_id = as.character(carrier_member_id))

matched_cl <- matched_ids %>%
  inner_join(elig2) %>%
  filter(!is.na(covered_life_id)) %>%
  select(carrier_member_id, covered_life_id, customer_name, month_year)

cl2 <- shGetQuery(con_type = "base_layer", 
                  "select member_id, cl_id AS covered_life_id from base_members where covered_life_id IS NOT NULL") %>%
  right_join(matched_cl)

apps <- shGetQuery(con_type = "base_layer", 
                   "select member_id, appointment_kind, provider_roles_clean, appointment_start_time, appointment_attendance_status
from BASE_APPOINTMENTS
where appointment_attendance_status NOT LIKE 'Cancelled'") 

apps <- apps %>%
  mutate(month_year = floor_date(appointment_start_time, unit = "months"))

cl3 <- cl2 %>%
  left_join(apps)

df_sessions <- cl3 %>%
  filter(!is.na(appointment_kind)) %>%
  group_by(appointment_kind, appointment_attendance_status) %>%
  summarize(n = n()) %>%
  arrange(appointment_attendance_status, appointment_kind)

