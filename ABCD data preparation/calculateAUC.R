calculate_auc <- function(data) {

# Load necessary libraries
  library("discAUC")
  library("dplyr")
  library("reshape2")
  
  # Define relevant variables
  var <- c("src_subject_id", "ddis_scr_val_indif_point_6h", 
           "ddis_scr_val_indif_pnt_1da", "ddis_scr_val_indif_pnt_1week", 
           "ddis_scr_val_indif_pnt_1mth", "ddis_scr_val_indif_pnt_3mth", 
           "ddis_scr_val_indif_pnt_1yr", "ddis_scr_val_indif_pnt_5yr")
  
  # Subset and reshape data
  long_dd <- data %>%
    select(all_of(var)) %>%
    melt(id.vars = "src_subject_id", 
         variable.name = "condition", 
         value.name = "measurement") %>%
    arrange(src_subject_id)
  
 # Identify and exclude participants with missing values or constant scores
exclude_ids <- long_dd %>%
  group_by(src_subject_id) %>%
  summarize(
    duplicates = sum(duplicated(measurement)),
    any_missing = any(is.na(measurement)),
    .groups = 'drop'
  ) %>%
  filter(duplicates == 6 | any_missing) %>%
  pull(src_subject_id)
  
  long_dd <- long_dd %>% filter(!src_subject_id %in% exclude_ids)
  
  # Map delay conditions to weeks
  delay_map <- c("ddis_scr_val_indif_point_6h" = 6 / 168,
                 "ddis_scr_val_indif_pnt_1da" = 24 / 168,
                 "ddis_scr_val_indif_pnt_1week" = 1,
                 "ddis_scr_val_indif_pnt_1mth" = 4,
                 "ddis_scr_val_indif_pnt_3mth" = 12,
                 "ddis_scr_val_indif_pnt_1yr" = 48,
                 "ddis_scr_val_indif_pnt_5yr" = 240)
  
  long_dd <- long_dd %>%
  mutate(delay_week = delay_map[condition],
         delay_rank = match(condition, names(delay_map))) %>%
  group_by(src_subject_id) %>%
  mutate(inconsistencies = sum(measurement[delay_rank] < lead(measurement[delay_rank], order_by = delay_rank, default = NA), na.rm = TRUE)) %>%
  ungroup()

  
  # Exclude participants with too many inconsistencies
  incon_excl <- long_dd %>%
    filter(inconsistencies >= 3) %>%
    pull(src_subject_id)
  
  long_dd <- long_dd %>% filter(!src_subject_id %in% incon_excl)


  # Turn measurement into numerical
  long_dd$measurement <- as.numeric(gsub(",",".",long_dd$measurement))
  
  # Calculate AUC
  AUC <- discAUC::AUC(dat = long_dd, indiff = "measurement", 
                      x_axis = "delay_week", amount = 1, 
                      grouping = "src_subject_id")
  
  # Return modified data
  data <- data[!(data$src_subject_id %in% unique(c(exclude_ids, incon_excl))), ]
  data$discAUC <- unlist(as.vector(AUC[, 2]))
  
  return(data)
}
