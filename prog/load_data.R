#this function loads variable list
load_var_list <-function(config){
  basic_var <- read_excel(paste0(config$data$root,"/20240815_IAMC-SWG-National-Scenario_Variable-List_Final_D4V7_Shared.xlsx"), sheet=2) %>% 
    {unique(.$Variable)}  
  return(basic_var)
}

#this function was used to load and save csv from main scenario database
load_scen_data <- function(config, scen_dir, var, national_model) {
  
  scen_file <- file.path(config$data$scen, "scen_data.csv")
  raw_file  <- file.path(config$data$scen, "snapshot_all_regions.csv")
  
  scen_data <- tryCatch({
    
    if (file.exists(scen_file)) {
      message("Loading existing scen_data.csv")
      return(read_csv(scen_file))
    }
    
    if (!file.exists(raw_file)) {
      stop("Raw data file 'snapshot_all_regions.csv' is missing in the scen_data folder.")
    }
    
    message("scen_data.csv not found. Creating from snapshot_all_regions.csv...")
    
    scen_all <- read_csv(raw_file) %>% 
      mutate(Model = str_trim(Model)) %>% 
      filter(!Model %in% "CPAM China 1") %>% 
      filter(Model %in% national_model) %>% 
      filter(Variable %in% var)
    
    # Save for future use
    write_csv(scen_all, scen_file)
    
    scen_all
    
  }, error = function(e) {
    stop("Error in load_scen_data(): ", e$message)
  })
  
  return(scen_data)
}

#this function was used to extract and save IPCC AR6 database
load_IPCC_AR6 <- function(config){
  
  AR6 <- list()
  
  AR6$R5 <- read_csv(paste0(config$data$ref, "/1668008228539-AR6_Scenarios_Database_R5_regions_v1.1.csv/AR6_Scenarios_Database_R5_regions_v1.1.csv")) %>% 
    select("Model", "Scenario", "Region", "Variable", "Unit", "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>% 
    filter(Variable %in% var_list)
  
  AR6$World <- read_csv(paste0(config$data$ref, "/1668008312256-AR6_Scenarios_Database_World_v1.1.csv/AR6_Scenarios_Database_World_v1.1.csv")) %>% 
    select("Model", "Scenario", "Region", "Variable", "Unit", "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>% 
    filter(Variable %in% var_list)
  
  AR6$meta <-  read_excel(paste0(config$data$ref, "/1668008228539-AR6_Scenarios_Database_R5_regions_v1.1.csv/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"), sheet=2) %>% 
    select("Model", "Scenario", "Category")
  
  AR6$R5 <- left_join(AR6$R5, AR6$meta, by=c("Model", "Scenario")) %>% 
    filter(!is.na(Category))
  
  AR6$World <- left_join(AR6$World, AR6$meta, by=c("Model", "Scenario")) %>% 
    filter(!is.na(Category))
  
  write_csv(AR6$World, file = file.path(config$data$ref, "/", "AR6_world.csv"))
  write_csv(AR6$R5 %>% filter(Region %in% 'R5ASIA'), file = file.path(config$data$ref, "/", "AR6_R5Asia.csv"))
  #return(AR6)
}


#this function was used to extract and save effort sharing data
load_effort_sharing <- function(config){
  
  df <- rgdx.param(paste0(config$data$ref, "/Results_2020to2050"), "CB") %>% 
    filter(i %in% region_mapping$ISO) %>% 
    rename(ISO=i) %>% 
    left_join(region_mapping%>% select(ISO, Country), by = "ISO") %>% 
    mutate(CB = CB/1000) %>% 
    rename(Region=Country)
  
  write_csv(df, file = file.path(config$data$ref, "/", "effort_sharing.csv"))
  
  #return(df)
  
}

#this function loads historical emissions data from IEA_EDGAR
load_hist_emi <- function(config){
  hist_emi<- read_excel(paste0(config$data$ref, "/IEA_EDGAR_CO2_1970_2022.xlsx"), sheet=2, skip = 9) %>% 
    filter(str_detect(ipcc_code_2006_for_standard_report, "^1.")) %>% 
    rename(Region=Name, Variable=ipcc_code_2006_for_standard_report_name) %>% 
    select("Region", "Variable", paste0("Y_", 1990:2020)) %>% 
    rename_with(~ gsub("Y_", "", .x), starts_with("Y_")) %>% 
    pivot_longer(cols = -c(Region, Variable), values_to = "Value", names_to = "Year", names_transform=as.numeric) %>% 
    mutate(Value=Value/1000, Year = as.numeric(Year)) %>% 
    mutate(Region=recode(Region, "Korea, Republic of"="South Korea"))
  
  return(hist_emi)
}


#this function loads country-wise income group from World Bank's database
load_WB_Class <- function(config){
  WB_Class <- read_excel(paste0(config$data$ref, "/CLASS_2025_10_07.xlsx"), sheet = 1) %>% 
    select("Economy", "Code", "Region", "Income group") %>% 
    drop_na()
  return(WB_Class)
}