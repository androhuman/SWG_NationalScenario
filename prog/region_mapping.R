
region_mapping <- tribble(
  ~Country,        ~ISO,  ~Region,
  
  "Japan",         "JPN", "Asia",
  "China",         "CHN", "Asia",
  "India",         "IND", "Asia",
  "South Korea",   "KOR", "Asia",
  "Pakistan",      "PAK", "Asia",
  "Nepal",         "NPL", "Asia",
  "Indonesia",     "IDN", "Asia",
  "Thailand",      "THA", "Asia",
  "Viet Nam",      "VNM", "Asia",
  "Saudi Arabia",  "SAU", "Asia",
  "Turkey",        "TUR", "Asia",
  
  "United States", "USA", "North America",
  "Canada",        "CAN", "North America",
  "Mexico",        "MEX", "North America",
  
  "Argentina",     "ARG", "South America",
  "Brazil",        "BRA", "South America",
  "Ecuador",       "ECU", "South America",
  
  
  "Australia",     "AUS", "Oceania",
  
  "Tunisia",       "TUN", "Africa",
  "Algeria",       "DZA", "Africa",
  "Nigeria",       "NGA", "Africa",
  
  "Ukraine",       "UKR", "Europe",
  "Ireland",       "IRL", "Europe",
  "Germany",       "DEU", "Europe",
  "Portugal",      "PRT", "Europe",
  "Norway",        "NOR", "Europe",
  "Switzerland",   "CHE", "Europe"
)

World <- c(region_mapping$Country)