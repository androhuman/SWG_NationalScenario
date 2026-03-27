#this function interpolates the missing data in 10-year data format and makes it 5-year
#also checks the missing data or NA values. 
#Interpolation is done for variables having more than 3 values between 2020 and 2050
interpolate_and_flag_missing <- function(df) {
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
    too_many_na = list()
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
    
    # Case 5: eligible for interpolation
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
    bind_rows(flagged$too_many_na) %>% mutate(Flag = "Too Many Consecutive NAs")
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
                           
                           "Secondary Energy|Electricity",
                           "Secondary Energy|Electricity|Wind|Offshore",
                           "Secondary Energy|Electricity|Wind|Onshore",
                           "Secondary Energy|Electricity|Solar|PV",
                           "Secondary Energy|Electricity|Solar|CSP",
                           "Secondary Energy|Electricity|Hydro",
                           "Secondary Energy|Electricity|Nuclear",
                           
                           "Secondary Energy|Electricity|Coal|w/o CCS",
                           "Secondary Energy|Electricity|Coal|w/ CCS",
                           "Secondary Energy|Electricity|Gas|w/o CCS",
                           "Secondary Energy|Electricity|Gas|w/ CCS",
                           "Secondary Energy|Electricity|Oil|w/o CCS",
                           "Secondary Energy|Electricity|Oil|w/ CCS",
                           "Secondary Energy|Electricity|Biomass|w/o CCS",
                           "Secondary Energy|Electricity|Biomass|w/ CCS",
                           
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
      
      
      #`Primary Energy|Low Carbon` = `Primary Energy|Wind`+`Primary Energy|Solar` + `Primary Energy|Nuclear` + `Primary Energy|Hydro` + `Primary Energy|Geothermal`,
      #`Primary Energy|Low Carbon|Share` = 100*`Primary Energy|Low Carbon`/`Primary Energy`,
      `Primary Energy|Non-fossil` = `Primary Energy|Wind`+`Primary Energy|Solar` + `Primary Energy|Nuclear` + `Primary Energy|Hydro` + `Primary Energy|Geothermal` + `Primary Energy|Biomass`,
      `Primary Energy|Non-fossil|Share` = 100*`Primary Energy|Non-fossil`/`Primary Energy`,
      `Primary Energy|Solar|Share` = 100*`Primary Energy|Solar`/`Primary Energy`,
      `Primary Energy|Wind|Share` = 100*`Primary Energy|Wind`/`Primary Energy`,
      
      `Primary Energy|Wind and Solar|Share`=100*(`Primary Energy|Wind`+`Primary Energy|Solar`)/`Primary Energy`,
      `Primary Energy|Coal|Share`=100*`Primary Energy|Coal`/`Primary Energy`,
      `Primary Energy|Gas|Share`=100*`Primary Energy|Gas`/`Primary Energy`,
      `Primary Energy|Biomass|Share`=100*`Primary Energy|Biomass`/`Primary Energy`,
      `Primary Energy|Nuclear|Share`=100*`Primary Energy|Nuclear`/`Primary Energy`,
      `Primary Energy|Hydro|Share`=100*`Primary Energy|Nuclear`/`Primary Energy`,
      
      
      #`Primary Energy|Low Carbon|Solar|Share` = 100*`Primary Energy|Solar`/`Primary Energy|Low Carbon`,
      #`Primary Energy|Low Carbon|Wind|Share` = 100*`Primary Energy|Wind`/`Primary Energy|Low Carbon`,
      `Primary Energy|Non-fossil|Solar|Share` = 100*`Primary Energy|Solar`/`Primary Energy|Non-fossil`,
      `Primary Energy|Non-fossil|Wind|Share` = 100*`Primary Energy|Wind`/`Primary Energy|Non-fossil`,
      
      `Primary Energy|Fossil` = `Primary Energy|Coal`+`Primary Energy|Oil`+`Primary Energy|Gas`,
      `Primary Energy|Fossil|Share` = 100*(`Primary Energy|Coal`+`Primary Energy|Oil`+`Primary Energy|Gas`)/`Primary Energy`,
      
      
      `Secondary Energy|Electricity|Fossil|w/o CCS`=`Secondary Energy|Electricity|Coal|w/o CCS`+`Secondary Energy|Electricity|Gas|w/o CCS`+`Secondary Energy|Electricity|Oil|w/o CCS`,
      `Secondary Energy|Electricity|Fossil|w/o CCS|Share`= 100*`Secondary Energy|Electricity|Fossil|w/o CCS`/`Secondary Energy|Electricity`,
      `Secondary Energy|Electricity|Fossil|w/ CCS`= `Secondary Energy|Electricity|Coal|w/ CCS`+ `Secondary Energy|Electricity|Gas|w/ CCS` + `Secondary Energy|Electricity|Oil|w/ CCS`,
      `Secondary Energy|Electricity|Fossil|w/ CCS|Share` = `Secondary Energy|Electricity|Fossil|w/ CCS`/ `Secondary Energy|Electricity`,
      `Secondary Energy|Electricity|Non-Bio Renewables`=`Secondary Energy|Electricity|Wind|Onshore`+`Secondary Energy|Electricity|Wind|Offshore`+
        `Secondary Energy|Electricity|Solar|CSP`+`Secondary Energy|Electricity|Solar|PV`+`Secondary Energy|Electricity|Hydro`,
      `Secondary Energy|Electricity|Non-Bio Renewables|Share`= 100*`Secondary Energy|Electricity|Non-Bio Renewables`/`Secondary Energy|Electricity`,
      `Secondary Energy|Electricity|Biomass|w/ CCS|Share`= 100*`Secondary Energy|Electricity|Biomass|w/ CCS`/`Secondary Energy|Electricity`,
      `Secondary Energy|Electricity|Nuclear|Share`= 100*`Secondary Energy|Electricity|Nuclear`/`Secondary Energy|Electricity`) %>% 
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
                          `Primary Energy|Wind and Solar|Share`,
                          #`Primary Energy|Low Carbon`,
                          #`Primary Energy|Low Carbon|Share`,
                          `Primary Energy|Non-fossil`,
                          `Primary Energy|Non-fossil|Share`,
                          `Primary Energy|Coal|Share`,
                          `Primary Energy|Gas|Share`,
                          `Primary Energy|Biomass|Share`,
                          `Primary Energy|Nuclear|Share`,
                          `Primary Energy|Hydro|Share`,
                          `Primary Energy|Solar|Share`,
                          `Primary Energy|Wind|Share`,
                          `Primary Energy|Fossil`,
                          `Primary Energy|Fossil|Share`,
                          #`Primary Energy|Low Carbon|Solar|Share`,
                          #`Primary Energy|Low Carbon|Wind|Share`,
                          `Primary Energy|Non-fossil|Solar|Share`,
                          `Primary Energy|Non-fossil|Wind|Share`,
                          
                          `Secondary Energy|Electricity`,
                          `Secondary Energy|Electricity|Coal|w/o CCS`,
                          `Secondary Energy|Electricity|Gas|w/o CCS`,
                          `Secondary Energy|Electricity|Oil|w/o CCS`,
                          `Secondary Energy|Electricity|Coal|w/ CCS`,
                          `Secondary Energy|Electricity|Gas|w/ CCS`,
                          `Secondary Energy|Electricity|Oil|w/ CCS`,
                          `Secondary Energy|Electricity|Fossil|w/o CCS`,
                          `Secondary Energy|Electricity|Non-Bio Renewables`,
                          `Secondary Energy|Electricity|Biomass|w/o CCS`,
                          `Secondary Energy|Electricity|Biomass|w/ CCS`,
                          `Secondary Energy|Electricity|Nuclear`,
                          `Secondary Energy|Electricity|Wind|Onshore`,
                          `Secondary Energy|Electricity|Wind|Offshore`,
                          `Secondary Energy|Electricity|Solar|CSP`,
                          `Secondary Energy|Electricity|Solar|PV`,
                          `Secondary Energy|Electricity|Hydro`,
                          `Secondary Energy|Electricity|Fossil|w/ CCS`,
                          `Secondary Energy|Electricity|Fossil|w/ CCS|Share`,
                          `Secondary Energy|Electricity|Biomass|w/ CCS|Share`,
                          `Secondary Energy|Electricity|Nuclear|Share`,
                          `Secondary Energy|Electricity|Fossil|w/o CCS|Share`,
                          `Secondary Energy|Electricity|Non-Bio Renewables|Share`,
                          
                          `Final Energy`,
                          `Final Energy|Electricity`,
                          `Final Energy|Hydrogen`,
                          `Final Energy|Electricity|Share`,
                          `Final Energy|Electricity+Hydrogen|Share`
                          
    ), names_to = "Variable") %>% 
    pivot_wider(names_from = "Year", values_from = "value")
  
  return(calc)
}

