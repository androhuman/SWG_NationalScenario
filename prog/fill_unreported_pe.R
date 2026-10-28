#this file handles primary energy carriers that a model does not report.
#A carrier is set to 0 only when the carriers the model does report add up to its
#reported total primary energy (within `tol` %) in every year of that
#Model-Region-Scenario. In that case the carrier is not part of the model's energy
#balance. Otherwise the data are left unchanged and the carrier stays missing.

PE_PARTS <- paste0("Primary Energy|", c("Coal", "Oil", "Gas", "Biomass", "Nuclear", "Hydro",
                                        "Wind", "Solar", "Geothermal", "Other"))

#carriers that may be filled with 0 ("Other" is only used in the closure check)
PE_FILL_PARTS <- setdiff(PE_PARTS, "Primary Energy|Other")


#this function compares reported total primary energy with the sum of the reported
#carriers for each Model-Region-Scenario and labels the result
check_pe_closure <- function(df, tol = 1, pe_parts = PE_PARTS) {
  
  safe_max <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
  
  pe_long <- df %>%
    filter(Variable %in% c("Primary Energy", pe_parts)) %>%
    select(-Unit) %>%
    pivot_longer(matches("^\\d{4}$"), names_to = "Year", values_to = "Value") %>%
    filter(!is.na(Value))
  
  #per year: total vs sum of reported carriers
  pe_check_year <- full_join(
    pe_long %>% filter(Variable == "Primary Energy") %>%
      select(Model, Region, Scenario, Year, total = Value),
    pe_long %>% filter(Variable != "Primary Energy") %>%
      group_by(Model, Region, Scenario, Year) %>%
      summarise(reported_sum = sum(Value), .groups = "drop"),
    by = c("Model", "Region", "Scenario", "Year")) %>%
    mutate(gap_pct = 100 * (total - reported_sum) / total)
  
  #carriers not reported by each Model-Region-Scenario
  pe_missing <- pe_long %>%
    distinct(Model, Region, Scenario) %>%
    left_join(pe_long %>% filter(Variable != "Primary Energy") %>%
                group_by(Model, Region, Scenario) %>%
                summarise(rep = list(unique(Variable)), .groups = "drop"),
              by = c("Model", "Region", "Scenario")) %>%
    mutate(missing = map_chr(rep, ~ paste(str_remove(setdiff(pe_parts, .x), "Primary Energy\\|"),
                                          collapse = ", "))) %>%
    select(-rep)
  
  pe_check_year %>%
    group_by(Model, Region, Scenario) %>%
    summarise(has_total      = any(!is.na(total)),
              has_components = any(!is.na(reported_sum)),
              min_gap_pct    = safe_min(gap_pct),
              max_gap_pct    = safe_max(gap_pct),
              .groups = "drop") %>%
    mutate(status = case_when(
      !has_total                                ~ "No total reported",
      !has_components                           ~ "No carriers reported",
      is.na(max_gap_pct)                        ~ "No overlapping years",
      max_gap_pct <= tol & min_gap_pct >= -tol  ~ "Matches total",
      min_gap_pct < -tol                        ~ "Carriers exceed total",
      TRUE                                      ~ "Carriers below total")) %>%
    left_join(pe_missing, by = c("Model", "Region", "Scenario")) %>%
    select(Model, Region, Scenario, status, min_gap_pct, max_gap_pct, missing) %>%
    arrange(status, Model, Region, Scenario)
}


#this function adds 0-valued rows for unreported carriers in the Model-Region-Scenario
#cases whose reported carriers match total primary energy. Other cases are not changed.
#Returns a list: data (with added rows), check (closure table), added (rows that were added)
fill_unreported_pe <- function(df, tol = 1, pe_parts = PE_PARTS, fill_parts = PE_FILL_PARTS) {
  
  check <- check_pe_closure(df, tol = tol, pe_parts = pe_parts)
  
  matched <- check %>%
    filter(status == "Matches total") %>%
    distinct(Model, Region, Scenario)
  
  year_cols <- grep("^\\d{4}$", names(df), value = TRUE)
  
  #unit of total primary energy in each case, used for the added rows
  pe_unit <- df %>%
    filter(Variable == "Primary Energy") %>%
    distinct(Model, Region, Scenario, Unit) %>%
    group_by(Model, Region, Scenario) %>%
    slice(1) %>%
    ungroup()
  
  added <- matched %>%
    cross_join(tibble(Variable = fill_parts)) %>%
    anti_join(df %>% distinct(Model, Region, Scenario, Variable),
              by = c("Model", "Region", "Scenario", "Variable")) %>%
    left_join(pe_unit, by = c("Model", "Region", "Scenario"))
  added[year_cols] <- 0
  
  message("fill_unreported_pe(): ", nrow(matched), " Model-Region-Scenario cases match total primary energy (tol = ",
          tol, "%); ", nrow(added), " unreported carrier rows set to 0.")
  
  list(
    data  = bind_rows(df, added),
    check = check,
    added = added
  )
}