#this function interpolates the missing data in 10-year data format and makes it 5-year
#also checks the missing data or NA values.
#Gaps inside a series (up to 3 consecutive 5-year steps) are filled by linear interpolation.
#Missing values at the start or end are only filled (with the nearest reported value) up to
#`max_lead_steps` 5-year steps before the first reported year and `max_trail_steps` steps
#after the last one; series with longer gaps at either end are flagged and excluded.
interpolate_and_flag_missing <- function(df, max_lead_steps = 1, max_trail_steps = 0) {
  # Ensure long format
  df_long <- df %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "Year",
      values_to = "Value",
      names_transform = list(Year = as.numeric)
    ) %>%
    filter(Year %in% seq(2020, 2050, 5)) %>%
    mutate(Value = as.numeric(Value))
  
  # Empty lists to collect flagged cases
  flagged <- list(
    all_na = list(),
    all_zero = list(),
    single_point = list(),
    too_many_na = list(),
    missing_ends = list()
  )
  
  # Split by group (Model, Scenario, Region, Variable)
  df_split <- df_long %>%
    group_by(Model, Scenario, Region, Variable) %>%
    group_split()
  
  # Interpolate each group
  df_interp_list <- map(df_split, function(grp) {
    y_vals <- grp$Value
    x_vals <- grp$Year
    
    # Case 1: all NA
    if (all(is.na(y_vals))) {
      flagged$all_na <<- append(flagged$all_na, list(grp[1, c("Model", "Scenario", "Region", "Variable")]))
      return(grp)
    }
    
    # Case 2: all zero (treat formatted 0.00 etc. as zero)
    if (all(abs(y_vals) < 1e-10, na.rm = TRUE)) {
      flagged$all_zero <<- append(flagged$all_zero, list(grp[1, c("Model", "Scenario", "Region", "Variable")]))
      return(grp)
    }
    
    # Case 3: only one non-NA
    if (sum(!is.na(y_vals)) == 1) {
      flagged$single_point <<- append(flagged$single_point, list(grp[1, c("Model", "Scenario", "Region", "Variable")]))
      return(grp)
    }
    
    # Case 4: more than 3 consecutive NAs
    na_runs <- rle(is.na(y_vals))
    if (any(na_runs$values & na_runs$lengths > 3)) {
      flagged$too_many_na <<- append(flagged$too_many_na, list(grp[1, c("Model", "Scenario", "Region", "Variable")]))
      return(grp)
    }
    
    # Case 5: too many missing steps at the start or end (no extrapolation beyond the limits)
    obs_years <- x_vals[!is.na(y_vals)]
    if (min(obs_years) - min(x_vals) > 5 * max_lead_steps ||
        max(x_vals) - max(obs_years) > 5 * max_trail_steps) {
      flagged$missing_ends <<- append(flagged$missing_ends, list(grp[1, c("Model", "Scenario", "Region", "Variable")]))
      return(grp)
    }
    
    # Case 6: eligible for interpolation
    #rule = 2 holds the nearest value only for the end steps allowed above
    valid_idx <- !is.na(y_vals)
    if (sum(valid_idx) >= 2) {
      grp$Value <- approx(
        x = x_vals[valid_idx],
        y = y_vals[valid_idx],
        xout = x_vals,
        rule = 2
      )$y
    }
    
    return(grp)
  })
  
  # Combine interpolated groups
  df_interp <- bind_rows(df_interp_list)
  
  # Combine flagged groups into a single dataframe
  flagged_all <- bind_rows(
    bind_rows(flagged$all_na) %>% mutate(Flag = "All NA"),
    bind_rows(flagged$all_zero) %>% mutate(Flag = "All Zero"),
    bind_rows(flagged$single_point) %>% mutate(Flag = "Single Data Point"),
    bind_rows(flagged$too_many_na) %>% mutate(Flag = "Too Many Consecutive NAs"),
    bind_rows(flagged$missing_ends) %>% mutate(Flag = "Missing Start/End Years")
  ) %>% distinct()
  
  # Create clean data by excluding flagged Model–Scenario–Region–Variable combos
  if (nrow(flagged_all) > 0) {
    clean_df <- df_interp %>%
      anti_join(flagged_all%>% filter(!Flag %in% "All Zero"), by = c("Model", "Scenario", "Region", "Variable"))
  } else {
    clean_df <- df_interp
  }
  
  return(list(interpolated = df_interp, flagged = flagged_all, clean = clean_df))
}

#this function fixes the units according to `unit_correction_table.R`
fix_unit <- function(df, correction_table){
  df <- df %>%
    pivot_longer(cols = -c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value", names_transform = list(Year = as.numeric)) %>% 
    left_join(correction_table, by = c("Model", "Variable")) %>%
    mutate(Value = if_else(!is.na(Factor), Value * Factor, Value)) %>%
    select(-Factor) %>% 
    pivot_wider(names_from = Year, values_from = Value)
  
  return(df)
}


#this function calculates the secondary variables required for the analysis
var_calc <- function(df){
  calc <- df %>% 
    filter(Variable %in% c("Emissions|CO2|Energy|Demand", 
                           "Emissions|CO2|Energy|Supply",
                           "Emissions|CO2|Energy",
                           "Emissions|CO2",
                           
                           "GDP|MER",
                           "Population",
                           
                           "Primary Energy",
                           "Primary Energy|Coal",
                           "Primary Energy|Gas",
                           "Primary Energy|Oil",
                           "Primary Energy|Biomass",
                           "Primary Energy|Solar",
                           "Primary Energy|Wind",
                           "Primary Energy|Nuclear",
                           "Primary Energy|Hydro",
                           "Primary Energy|Geothermal",
                           
                           "Final Energy",
                           "Final Energy|Electricity",
                           "Final Energy|Hydrogen")) %>% 
    select("Model", "Scenario", "Region", "Variable", "Unit", "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>% 
    pivot_longer(cols=-c(Model, Scenario, Region, Variable, Unit), names_to="Year", values_to="Value", names_transform=as.numeric) %>%  
    pivot_wider(names_from = Variable, values_from = Value, id_cols = c(Model, Region, Scenario, Year)) %>% 
    mutate(`Emissions|CO2|Energy` = ifelse(
      !is.na(`Emissions|CO2|Energy`), 
      `Emissions|CO2|Energy`, 
      `Emissions|CO2|Energy|Demand` + `Emissions|CO2|Energy|Supply`),
      `Emissions Intensity of GDP` = `Emissions|CO2|Energy`/`GDP|MER`,
      `Emission_CO2 Intensity of GDP` = `Emissions|CO2`/`GDP|MER`,
      
      `GDP Per Capita` = `GDP|MER`/Population,
      `Carbon Intensity of TPES` = `Emissions|CO2|Energy`/`Primary Energy`,
      `Energy Intensity of GDP` = `Primary Energy`/`GDP|MER`,
      `Emissions Per Capita` = `Emissions|CO2|Energy`/Population,
      `Energy Consumption Per Capita` = `Final Energy`/Population,
      
      `Final Energy|Electricity|Share`=100*`Final Energy|Electricity`/`Final Energy`,
      `Final Energy|Electricity+Hydrogen|Share`=100*(`Final Energy|Electricity`+`Final Energy|Hydrogen`)/`Final Energy`,
      
      `Primary Energy|Non-fossil` = `Primary Energy|Wind`+`Primary Energy|Solar` + `Primary Energy|Nuclear` + `Primary Energy|Hydro` + `Primary Energy|Geothermal` + `Primary Energy|Biomass`,
      `Primary Energy|Non-fossil|Share` = 100*`Primary Energy|Non-fossil`/`Primary Energy`,
      
      `Primary Energy|Non-fossil|Solar|Share` = 100*`Primary Energy|Solar`/`Primary Energy|Non-fossil`,
      `Primary Energy|Non-fossil|Wind|Share` = 100*`Primary Energy|Wind`/`Primary Energy|Non-fossil`,
      
      `Primary Energy|Fossil` = `Primary Energy|Coal`+`Primary Energy|Oil`+`Primary Energy|Gas`,
      `Primary Energy|Fossil|Share` = 100*(`Primary Energy|Coal`+`Primary Energy|Oil`+`Primary Energy|Gas`)/`Primary Energy`
      
    ) %>% 
    pivot_longer(cols = c(`Emissions|CO2|Energy|Demand`,
                          `Emissions|CO2|Energy|Supply`,
                          `Emissions|CO2|Energy`,
                          `Emissions|CO2`,
                          
                          `GDP|MER`,
                          Population,
                          `GDP|MER`,
                          `Emissions Intensity of GDP`,
                          `Emission_CO2 Intensity of GDP`,
                          `GDP Per Capita`,
                          `Carbon Intensity of TPES`,
                          `Energy Intensity of GDP`,
                          `Emissions Per Capita`,
                          `Energy Consumption Per Capita`,
                          
                          `Primary Energy`,
                          `Primary Energy|Coal`,
                          `Primary Energy|Oil`,
                          `Primary Energy|Gas`,
                          `Primary Energy|Biomass`,
                          `Primary Energy|Solar`,
                          `Primary Energy|Wind`,
                          `Primary Energy|Nuclear`,
                          `Primary Energy|Hydro`,
                          `Primary Energy|Geothermal`,
                          
                          `Primary Energy|Non-fossil`,
                          `Primary Energy|Non-fossil|Share`,
                          
                          `Primary Energy|Fossil`,
                          `Primary Energy|Fossil|Share`,
                          
                          `Primary Energy|Non-fossil|Solar|Share`,
                          `Primary Energy|Non-fossil|Wind|Share`,
                          
                          `Final Energy`,
                          `Final Energy|Electricity`,
                          `Final Energy|Hydrogen`,
                          `Final Energy|Electricity|Share`,
                          `Final Energy|Electricity+Hydrogen|Share`
                          
    ), names_to = "Variable") %>% 
    pivot_wider(names_from = "Year", values_from = "value")
  
  return(calc)
}