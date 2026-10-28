#this function loads variable list
load_var_list <-function(config){
  basic_var <- read_excel(paste0(config$data$root,"/20240815_IAMC-SWG-National-Scenario_Variable-List_Final_D4V7_Shared.xlsx"), sheet=2) %>% 
    {unique(.$Variable)}  
  return(basic_var)
}

#this function was used to load csv from main scenario database file with name: snapshot_all_regions.csv
load_scen_data <- function(config, var) {
  
  raw_file <- file.path(config$data$scen, "snapshot_all_regions.csv")
  
  if (!file.exists(raw_file)) {
    stop("load_scen_data(): 'snapshot_all_regions.csv' is missing in ", config$data$scen)
  }
  
  message("Loading snapshot_all_regions.csv")
  
  read_csv(raw_file, col_types = cols(
    Model    = col_character(),
    Scenario = col_character(),
    Region   = col_character(),
    Variable = col_character(),
    Unit     = col_character(),
    .default = col_double()      # all year columns numeric
  ),
  show_col_types = FALSE) %>% 
    mutate(Model = str_trim(Model)) %>% 
    filter(Variable %in% var)
  
}


#this function was used to extract and save IPCC AR6 database
load_IPCC_AR6 <- function(config){
  
  AR6 <- list()
  
  AR6$R5 <- read_csv(paste0(config$data$ref, "/AR6_Scenarios_Database_R5_regions_v1.1.csv")) %>% 
    select("Model", "Scenario", "Region", "Variable", "Unit", "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>% 
    filter(Variable %in% var_list)
  
  AR6$World <- read_csv(paste0(config$data$ref, "/AR6_Scenarios_Database_World_v1.1.csv")) %>% 
    select("Model", "Scenario", "Region", "Variable", "Unit", "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>% 
    filter(Variable %in% var_list)
  
  AR6$meta <-  read_excel(paste0(config$data$ref, "/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"), sheet=2) %>% 
    select("Model", "Scenario", "Category")
  
  
  AR6$R5 <- left_join(AR6$R5, AR6$meta, by=c("Model", "Scenario")) %>% 
    filter(!is.na(Category))
  
  AR6$World <- left_join(AR6$World, AR6$meta, by=c("Model", "Scenario")) %>% 
    filter(!is.na(Category))
  
  write_csv(AR6$World, file = file.path(config$data$ref, "/", "AR6_world.csv"))
  write_csv(AR6$R5 %>% filter(Region %in% 'R5ASIA'), file = file.path(config$data$ref, "/", "AR6_R5Asia.csv"))
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
    filter(str_detect(ipcc_code_2006_for_standard_report, "^1\\.")) %>% 
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