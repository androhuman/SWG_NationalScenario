library(here)

config <- list(
  root = here::here(),
  prog = here::here("prog"),
  data = list(
    root = here::here("data"),
    ref = here::here("data", "ref_data"),
    scen = here::here("data", "scen_data")
  ),
  output = list(
    root = here::here("output"),
    fig = here::here("output", "figures"),
    main = here::here("output", "figures", "main"),
    supplementary  = here::here("output", "figures", "supplementary")
  )
)

scen_color <- c('BAU'='#F8766D', 'CPS'='#619CFF', 'NZS'='#00BA38')

region_colors <- country_colors <- c(
  "Thailand"        = "#4E79A7",
  "Japan"           = "#F28E2B",
  "Viet Nam"        = "#9C755F",
  "Nepal"           = "#B07AA1",
  "Indonesia"       = "#78B7B2",
  "Australia"       = "#EDC948",
  "Brazil"          = "#86BC86",
  "China"           = "lightblue3",
  "India"           = "#59A14F",
  "Saudi Arabia"    = "#7F7F7F",
  "United States"   = "#EE6677",
  "Norway"          = "#A0CBE8",
  "Ukraine"          = "#B44E52",
  "Algeria"         = "#8CD17D",
  #"Nigeria"         = "#499894",
  "Ecuador"        = "#499894",
  "Mexico"         = "#7E8B57",
  
  #"Tunisia"         = "#D37295",
  "South Korea"     = "#FABFD2",
  #"Pakistan"        = "#2E8B57",
  "Switzerland"     = "#B6992D",
  #"Turkey"          = "#C44E52",
  "Canada"          = "#2E8B57",
  "Germany"         = "#6B6ECF",
  "Portugal"        = "#C7C7C7",
  "Argentina"       = "#8C564B",
  "Other"           = "white"   
)

#national model 
national_model <- c("AIM-Thailand 1", 
                    "AIM/Enduse India 3.3", 
                    "AIM/Hub-Japan 2.4", 
                    "AIM/Hub-Vietnam 2.2",
                    "AIM/Nepal V1.0", 
                    "AIM/Technology-Japan 2.1", 
                    "ASEAN Energy Outlook AEO8", 
                    "ATEM v1",
                    "BLUES 2.0", 
                    "CPAM-China 1.0.0",
                    "China TIMES 2", 
                    "D-TIMES_3",
                    "DNE21+ V.16_JMIP", 
                    "ELENA v1",
                    "GCAM KAIST 2.0",
                    "GCAM-China-v6",
                    "GCAM-KSA V1.0", 
                    "GCAM-USA-CGS", 
                    "GRACE Nor_v1.0",
                    "GTEM-C v1",
                    "Goblin lite 0.3.4", 
                    "IEEJ-NE_Japan v2023", 
                    "IFE-TIMES-Norway v.2023",
                    "IMED_China V1.0", 
                    "India Energy Policy Simulator 3.1.3",
                    "KLEM-DZA 2", 
                    "KLEM/ENERGYPATHWAYS/RIO-MX 1",
                    "KLEM/LEAP-ARG 1",
                    "KLEM/LEAP-NGA V1.0", 
                    "KLEM/MESSAGE-TUN V1.0", 
                    "MESSAGEix-Canada 1",
                    "MESSAGEix-KOR 1", 
                    "MESSAGEix-Pakistan 1", 
                    "MESSAGEix-Turkey 5", 
                    "PECE-LIU 2024_V1.0",
                    "PROMETHEUS V2 China",
                    "PROMETHEUS V2 India",
                    "PyPSA-BD v1", 
                    "PyPSA-TH v1", 
                    "REMod V1.0", 
                    "SPINE v1", 
                    "Swiss TIMES Energy System Model (STEM) 2", 
                    "TIMES-Japan 3.3", 
                    "TIMES-Japan 3.4",
                    "TIMES-Nigeria V1", 
                    "TIMES-Portugal 9.3",
                    "TIMES-Ukraine v1",
                    "UNICON-K-Power 2"
)
