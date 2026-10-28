
region_mapping <- tribble(
  ~Country,        ~ISO,  ~Region,         ~AR6_R5,
  
  "Japan",         "JPN", "Asia",          "R5OECD",
  "China",         "CHN", "Asia",          "R5ASIA",
  "India",         "IND", "Asia",          "R5ASIA",
  "South Korea",   "KOR", "Asia",          "R5ASIA",
  "Pakistan",      "PAK", "Asia",          "R5ASIA",
  "Nepal",         "NPL", "Asia",          "R5ASIA",
  "Indonesia",     "IDN", "Asia",          "R5ASIA",
  "Thailand",      "THA", "Asia",          "R5ASIA",
  "Viet Nam",      "VNM", "Asia",          "R5ASIA",
  "Saudi Arabia",  "SAU", "Asia",          "R5MAF",
  "Turkey",        "TUR", "Asia",          "R5OECD",
  "Bangladesh",    "BGD", "Asia",          "R5ASIA",
  
  "United States", "USA", "North America", "R5OECD",
  "Canada",        "CAN", "North America", "R5OECD",
  "Mexico",        "MEX", "North America", "R5LAM",
  
  "Argentina",     "ARG", "South America", "R5LAM",
  "Brazil",        "BRA", "South America", "R5LAM",
  "Ecuador",       "ECU", "South America", "R5LAM",
  
  "Australia",     "AUS", "Oceania",       "R5OECD",
  
  "Tunisia",       "TUN", "Africa",        "R5MAF",
  "Algeria",       "DZA", "Africa",        "R5MAF",
  "Nigeria",       "NGA", "Africa",        "R5MAF",
  
  "Ukraine",       "UKR", "Europe",        "R5REF",
  "Ireland",       "IRL", "Europe",        "R5OECD",
  "Germany",       "DEU", "Europe",        "R5OECD",
  "Portugal",      "PRT", "Europe",        "R5OECD",
  "Norway",        "NOR", "Europe",        "R5OECD",
  "Switzerland",   "CHE", "Europe",        "R5OECD"
)

World <- c(region_mapping$Country)

