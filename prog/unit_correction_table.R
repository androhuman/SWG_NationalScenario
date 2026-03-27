#Model name, variable name to fix and required factor to multiply

correction_table <- tribble(
  ~Model,                 ~Variable,                       ~Factor,
  "UNICON-K-Power 2",     "Primary Energy",                1e-3,
  "UNICON-K-Power 2",     "Primary Energy|Coal",           1e-3,
  "UNICON-K-Power 2",     "Primary Energy|Gas",            1e-3,
  "UNICON-K-Power 2",     "Primary Energy|Oil",            1e-3,
  "UNICON-K-Power 2",     "Final Energy",                  1e-3,
  "UNICON-K-Power 2",     "Final Energy|Electricity",      1e-3,
  "UNICON-K-Power 2",     "Final Energy|Gases",            1e-3,
  "UNICON-K-Power 2",     "Final Energy|Hydrogen",         1e-3,
  "UNICON-K-Power 2",     "Final Energy|Liquids",          1e-3,
  "UNICON-K-Power 2",     "Final Energy|Solids",           1e-3,
  "UNICON-K-Power 2",     "Final Energy|Solids|Fossil",    1e-3,
  "UNICON-K-Power 2",     "Secondary Energy|Hydrogen",     1e-3,
  
  "KLEM-DZA 2",           "Population",                    1e-6
)
