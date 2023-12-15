
# This code gets covered life IDs for individuals in the ROI matching study.

# Get matched individuals
matched_ids <- readRDS(paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/df_matched.Rdata")) %>%
  distinct(carrier_member_id, spring_dummy)

customer_name <- c("Wellstar", "Pepsi", "Hearst", "Docusign", "Tegna")

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
  select(carrier_member_id, covered_life_id, customer_name, spring_dummy)


# Save
saveRDS(matched_cl, paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/matched_cl_ids.Rdata"))
write.table(matched_cl, paste0(Sys.getenv("claims_folder_path"), "data/roi_study_2023/matched_cl_ids.csv"), col.names=T, row.names=F)
                

matched_cl %>%
  filter(is.na(covered_life_id)) %>%
  group_by(customer_name) %>%
  summarize(n = n_distinct(carrier_member_id))

unique(df_matched$spring_start_date[df_matched$customer_name == "DocuSign"])
unique(df_matched$spring_start_date)  
