#other plots

#basic timeseries charts
countries <- coverage$Region

plot_scenario_region(fixed_data %>% filter(Region %in% countries), World, "World", "Emissions|CO2", "Mt CO2/yr", w=375, h=300)
plot_scenario_region(fixed_data %>% filter(Region %in% countries), World, "World", "Emissions|CO2|Energy", "Mt CO2/yr", w=375, h=300)
plot_scenario_region(fixed_data %>% filter(Region %in% countries), World, "World", "Primary Energy", "EJ/yr", w=375, h=300)
plot_scenario_region(fixed_data %>% filter(Region %in% countries), World, "World", "Final Energy", "EJ/yr", w=375, h=300)
plot_scenario_region(fixed_data %>% filter(Region %in% countries), World, "World", "Secondary Energy|Electricity", "EJ/yr", w=375, h=300)
plot_scenario_region(fixed_data %>% filter(Region %in% countries), World, "World", "GDP|MER", "billion USD_2010/yr", w=375, h=300)


#Supplementary 1

#taking Emissions|CO2 instead of Emissions|CO2|Energy

scen_calc <- var_calc(fixed_data)

#calculate change rate from baseline
reduct_rate <- scen_calc %>% 
  filter(Variable %in% c(#"Emissions|CO2|Energy",
    "Emissions|CO2",
    "Final Energy",
    "GDP Per Capita",
    "Energy Intensity of GDP",
    "Emissions Intensity of GDP",
    "Emission_CO2 Intensity of GDP",
    "Carbon Intensity of TPES"
  )) %>% 
  pivot_longer(cols=-c(Model, Scenario, Region, Variable), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  pivot_wider(names_from = Scenario, values_from = Value) %>% 
  mutate(rate = if_else(!is.na(BAU), 100*(BAU-NZS)/BAU, 100*(CPS-NZS)/CPS)) %>% 
  pivot_longer(cols=-c(Model, Region, Variable, Year, rate), names_to = "Scenario", values_to = "Value") %>% 
  select(Model, Region, Variable, Scenario, Year, rate) %>% 
  pivot_wider(names_from = Year, values_from = rate) %>% 
  mutate(Variable = paste0(Variable, "|Rate|Change")) %>% 
  filter(Scenario %in% 'NZS')

#calculate change from baseline
share_change <- scen_calc %>% 
  filter(Variable %in% c("Primary Energy|Fossil|Share",
                         "Primary Energy|Non-fossil|Share",
                         "Primary Energy|Non-fossil|Solar|Share",
                         "Primary Energy|Non-fossil|Wind|Share",
                         #"Primary Energy|Wind and Solar|Share",
                         #"Primary Energy|Biomass|Share",
                         #"Primary Energy|Nuclear|Share",
                         #"Primary Energy|Solar|Share",
                         #"Primary Energy|Wind|Share",
                         "Final Energy|Electricity|Share",
                         "Final Energy|Electricity+Hydrogen|Share"
                         #"GDP Per Capita",
                         #"Energy Intensity of GDP",
                         #"Emissions Intensity of GDP",
                         #"Carbon Intensity of TPES"
  )) %>% 
  pivot_longer(cols=-c(Model, Scenario, Region, Variable), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  pivot_wider(names_from = Scenario, values_from = Value) %>% 
  mutate(change = if_else(!is.na(BAU), (NZS-BAU), (NZS-CPS))) %>% 
  pivot_longer(cols=-c(Model, Region, Variable, Year, change), names_to = "Scenario", values_to = "Value") %>% 
  select(Model, Region, Scenario, Variable, Year, change) %>% 
  pivot_wider(names_from = Year, values_from = change) %>% 
  mutate(Variable = paste0(Variable, "|Change")) %>% 
  filter(Scenario %in% 'NZS') 


trend_df <- rbind(reduct_rate, share_change) %>% 
  filter(Model %in% national_model) %>% 
  na.omit()


s_1 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Final Energy|Rate|Change", 
                       scen='NZS',
                       p_title="a. Reduction in final energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change from baseline (%)")

s_2 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Final Energy|Electricity|Share|Change", 
                       scen='NZS',
                       p_title="b. Electricity share in final energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

s_3 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Share|Change", 
                       scen='NZS',
                       p_title="c. Non-fossil share in primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

s_4 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Primary Energy|Fossil|Share|Change", 
                       scen='NZS',
                       p_title="d. Fossil share in primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

s_5 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Solar|Share|Change", 
                       scen='NZS',
                       p_title="e. Solar share in Non-fossil primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

s_6 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Wind|Share|Change", 
                       scen='NZS',
                       p_title="f. Wind share in non-fossil primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

s_7 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Carbon Intensity of TPES|Rate|Change", 
                       scen='NZS',
                       p_title="g. Carbon intensity of TPES", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Decrease from baseline (%)")

s_8 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Energy Intensity of GDP|Rate|Change", 
                       scen='NZS',
                       p_title="h. Energy intensity of GDP", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Decrease from baseline (%)")

s_9 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Rate|Change", 
                       y_var="Emission_CO2 Intensity of GDP|Rate|Change", 
                       scen='NZS',
                       p_title="i. Emissions intensity of GDP", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Decrease from baseline (%)")

s_10 <- plot_regression(trend_df, 
                        x_var="Emissions|CO2|Rate|Change", 
                        y_var="GDP Per Capita|Rate|Change", 
                        scen='NZS',
                        p_title="j. GDP per capita", 
                        xlab="Emissions reduction rate from baseline (%)", 
                        ylab="Decrease from baseline (%)")

legend <- get_legend(s_1$plot)

s <- (s_1$plot + s_2$plot + s_3$plot + s_4$plot + s_5$plot + s_6$plot + s_7$plot + s_8$plot + s_9$plot) & 
  theme(legend.position = "none") 

s1 <- s / legend +
  plot_layout(heights = c(1, 0.08))

ggsave(filename=paste0(config$output$supplementary,"/", "1. Regression of Key Variables with Emissions CO2", ".jpg"), plot=s1, width=335, height=280, units='mm', dpi=300)


#save regression statistics 
regression_table <- bind_rows(
  s_1$stats,
  s_2$stats,
  s_3$stats,
  s_4$stats,
  s_5$stats,
  s_6$stats,
  
  s_7$stats,
  s_8$stats,
  s_9$stats,
  s_10$stats
) 

#annotate stars to significance levels
slope_star <- paste0((regression_table$slope),
                     sig_stars(regression_table$p_value)
)

regression_table <- regression_table %>%
  mutate(slope = paste0(round(slope, 3), sig_stars(p_value)))

write_csv(regression_table, file = file.path(config$output$supplementary, "/", "regression_summary_emi_CO2.csv"))


#supplementary 2
#considering 2020 as reference year of comparison rather than baseline
#calculate change rate from 2020
scen_calc <- var_calc(fixed_data)

reduct_rate <- scen_calc %>% 
  filter(Variable %in% c("Emissions|CO2|Energy",
                         "Emissions|CO2",
                         "Final Energy",
                         "GDP Per Capita",
                         "Energy Intensity of GDP",
                         "Emissions Intensity of GDP",
                         "Carbon Intensity of TPES"
  )) %>% 
  pivot_longer(cols=-c(Model, Scenario, Region, Variable), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  pivot_wider(names_from = Year, values_from = Value) %>% 
  filter(Scenario %in% 'NZS') %>% 
  mutate(`2020_r`=100*(`2020`-`2020`)/`2020`, `2025_r`=100*(`2020`-`2025`)/`2020`, `2030_r`=100*(`2020`-`2030`)/`2020`, `2035_r`=100*(`2020`-`2035`)/`2020`, `2040_r`=100*(`2020`-`2040`)/`2020`, `2045_r`=100*(`2020`-`2045`)/`2020`, `2050_r`=100*(`2020`-`2050`)/`2020`) %>% 
  select("Model", "Region",  "Scenario", "Variable", "2020_r", "2025_r", "2030_r", "2035_r", "2040_r", "2045_r", "2050_r") %>% 
  rename(`2020`='2020_r', `2025`='2025_r', `2030`='2030_r', `2035`='2035_r', `2040`='2040_r', `2045`='2045_r', `2050`='2050_r') %>% 
  mutate(Variable = paste0(Variable, "|Rate|Change")) 

#calculate change from 2020
share_change <- scen_calc %>% 
  filter(Variable %in% c("Primary Energy|Fossil|Share",
                         "Primary Energy|Non-fossil|Share",
                         "Primary Energy|Non-fossil|Solar|Share",
                         "Primary Energy|Non-fossil|Wind|Share",
                         "Primary Energy|Wind and Solar|Share",
                         "Primary Energy|Biomass|Share",
                         "Primary Energy|Nuclear|Share",
                         "Primary Energy|Solar|Share",
                         "Primary Energy|Wind|Share",
                         "Final Energy|Electricity|Share",
                         "Final Energy|Electricity+Hydrogen|Share"
                         #"GDP Per Capita",
                         #"Energy Intensity of GDP",
                         #"Emissions Intensity of GDP",
                         #"Carbon Intensity of TPES"
  )) %>% 
  pivot_longer(cols=-c(Model, Scenario, Region, Variable), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  pivot_wider(names_from = Year, values_from = Value) %>% 
  filter(Scenario %in% 'NZS') %>% 
  mutate(`2020_r`=(`2020`-`2020`), `2025_r`= (`2025`-`2020`), `2030_r`= (`2030`-`2020`), `2035_r`= (`2035`-`2020`), `2040_r`= (`2040`-`2020`), `2045_r`= (`2045`-`2020`), `2050_r`= (`2050`-`2020`)) %>% 
  select("Model", "Region",  "Scenario", "Variable", "2020_r", "2025_r", "2030_r", "2035_r", "2040_r", "2045_r", "2050_r") %>% 
  rename(`2020`='2020_r', `2025`='2025_r', `2030`='2030_r', `2035`='2035_r', `2040`='2040_r', `2045`='2045_r', `2050`='2050_r') %>% 
  mutate(Variable = paste0(Variable, "|Change")) 
#filter(Scenario %in% 'NZS')


scen_calc <- rbind(reduct_rate, share_change)

trend_df <- scen_calc %>% 
  filter(Model %in% national_model) %>% 
  filter(Scenario %in% 'NZS')

#plot comparison with 2020 

s_1 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Final Energy|Rate|Change", 
                       scen='NZS',
                       p_title="a. Reduction in final energy", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Change from base year (2020) (%)")

s_2 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Final Energy|Electricity|Share|Change", 
                       scen='NZS',
                       p_title="b. Electricity share in final energy", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Change in share from base year (2020) (%)")

s_3 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Share|Change", 
                       scen='NZS',
                       p_title="c. Non-fossil share in primary energy", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Change in share from base year (2020) (%)")

s_4 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Fossil|Share|Change", 
                       scen='NZS',
                       p_title="d. Fossil share in primary energy", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Change in share from base year (2020) (%)")

s_5 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Solar|Share|Change", 
                       scen='NZS',
                       p_title="e. Solar share in non-fossil primary energy", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Change in share from base year (2020) (%)")

s_6 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Wind|Share|Change", 
                       scen='NZS',
                       p_title="f. Wind share in non-fossil primary energy", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Change in share from base year (2020) (%)")

s_7 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Carbon Intensity of TPES|Rate|Change", 
                       scen='NZS',
                       p_title="g. Carbon intensity of TPES", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Decrease from base year (2020) (%)")

s_8 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Energy Intensity of GDP|Rate|Change", 
                       scen='NZS',
                       p_title="h. Energy intensity of GDP", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Decrease from base year (2020) (%)")

s_9 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Emissions Intensity of GDP|Rate|Change", 
                       scen='NZS',
                       p_title="i. Emissions intensity of GDP", 
                       xlab="Emissions reduction rate from base year (2020) (%)", 
                       ylab="Decrease from base year (2020) (%)")

s_10 <- plot_regression(trend_df, 
                        x_var="Emissions|CO2|Energy|Rate|Change", 
                        y_var="GDP Per Capita|Rate|Change", 
                        scen='NZS',
                        p_title="j. GDP per capita", 
                        xlab="Emissions reduction rate from base year (2020) (%)", 
                        ylab="Decrease from base year (2020) (%)")

legend <- get_legend(s_1$plot)

s <- (s_1$plot + s_2$plot + s_3$plot + s_4$plot + s_5$plot + s_6$plot + s_7$plot + s_8$plot + s_9$plot) & 
  theme(legend.position = "none") 

s2 <- s / legend +
  plot_layout(heights = c(1, 0.08))

ggsave(filename=paste0(config$output$supplementary,"/", "2. Regression of Key Variables with 2020 base year", ".jpg"), plot=s2, width=335, height=280, units='mm', dpi=300)


#save regression statistics 
regression_table <- bind_rows(
  s_1$stats,
  s_2$stats,
  s_3$stats,
  s_4$stats,
  s_5$stats,
  s_6$stats,
  
  s_7$stats,
  s_8$stats,
  s_9$stats,
  s_10$stats
) 

#annotate stars to significance levels
slope_star <- paste0((regression_table$slope),
                     sig_stars(regression_table$p_value)
)

regression_table <- regression_table %>%
  mutate(slope = paste0(round(slope, 3), sig_stars(p_value)))

write_csv(regression_table, file = file.path(config$output$supplementary, "/", "regression_summary_base_2020.csv"))


#Supplementary 3

#countries colored in regression chart 

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Final Energy|Rate|Change", 
                      scen='NZS',
                      p_title="a. Change in Final Energy from Baseline", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Final Energy from Baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Final Energy|Electricity|Share|Change", 
                      scen='NZS',
                      p_title="b. Electricity Share in Final Energy", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Share from Baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Final Energy|Electricity+Hydrogen|Share|Change", 
                      scen='NZS',
                      p_title="c. Electricity+Hydrogen Share in Final Energy", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Share from Baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Primary Energy|Non-fossil|Share|Change", 
                      scen='NZS',
                      p_title="d. Non-fossil Share in Primary Energy", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Share from Baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Primary Energy|Fossil|Share|Change", 
                      scen='NZS',
                      p_title="e. Fossil Share in Primary Energy", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Share from Baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Primary Energy|Non-fossil|Solar|Share|Change", 
                      scen='NZS',
                      p_title="f. Solar Share in Non-fossil Primary Energy", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Share from Baseline (%)")


plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Primary Energy|Non-fossil|Wind|Share|Change", 
                      scen='NZS',
                      p_title="g. Wind Share in Non-fossil Primary Energy", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Change in Share from Baseline (%)")


plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Carbon Intensity of TPES|Rate|Change", 
                      scen='NZS',
                      p_title="h. Carbon Intensity of TPES", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Decrease from baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Energy Intensity of GDP|Rate|Change", 
                      scen='NZS',
                      p_title="i. Energy Intensity of GDP", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Decrease from baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="Emissions Intensity of GDP|Rate|Change", 
                      scen='NZS',
                      p_title="j. Emissions Intensity of GDP", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Decrease from baseline (%)")

plot_regression_color(trend_df, 
                      x_var="Emissions|CO2|Energy|Rate|Change", 
                      y_var="GDP Per Capita|Rate|Change", 
                      scen='NZS',
                      p_title="k. GDP Per Capita", 
                      xlab="Emissions Reduction Rate from Baseline (%)", 
                      ylab="Decrease from baseline (%)")



#Supplementary 4
#national scenario trends comparison with global trends:

count_df <- share_world_asia %>%
  distinct(Model, Region, Scenario, Variable, Category, Type) %>%
  group_by(Type, Variable, Category) %>%
  summarise(n_scen = n(), .groups = "drop")

label_df <- count_df %>%
  pivot_wider(names_from = Category, values_from = n_scen) %>%
  mutate(#Year = Inf,
    #Value = Inf,
    label = paste0(
      "C1: ", C1,
      "\nC2: ", C2,
      "\nC3: ", C3,
      "\nNZS: ", NZS
    )
  )

p <- ggplot(share_world_asia, 
             aes(x = factor(Year), y = Value, fill = Category)) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    color = "black",
    alpha = 0.6,
    linewidth = 0.25
  ) +
  coord_cartesian(ylim = c(0, 100))+
  facet_grid( Type ~ Variable, scales = "free_y") +
  geom_label(
    data = label_df,
    aes(x = -Inf, y = Inf, label = label),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.1,
    size = 2.5
  ) +
  scale_fill_manual(values = fill_colors, name = "") + 
  labs(
    x = "Year",
    y = "Change in share(%) from year 2020"
    #title = "National NZS vs Global and Regional Trends"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())+
  theme(panel.grid = element_blank())+
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5),
        legend.position = "bottom"
  )

ggsave(filename=paste0(config$output$supplementary,"/", "4. National scenario trend comparison with regional and global trend", ".jpg"), plot=p, width=265, height=120, units='mm', dpi=300)


#Supplementary 5

#plot emissions reductions in national and global scenarios

emi_national <- fixed_data %>% 
  filter(Variable %in% "Emissions|CO2|Energy", Scenario %in% "NZS") %>% 
  pivot_longer(cols = -c(Model, Scenario, Region, Variable, Unit), names_to = "Year", values_to = "Value") %>% 
  mutate(Category="NZS", Type="National")

emi_AR6_world <- AR6_world %>% 
  filter(Variable %in% "Emissions|CO2|Energy", Category %in% c("C1", "C2", "C3"))

emi_AR6_world_interpolated <- interpolate_and_flag_missing(emi_AR6_world)

emi_AR6_world_fixed <- emi_AR6_world_interpolated$interpolated %>% 
  mutate(Type="Global")

#emi_AR6_asia <- AR6_R5Asia %>% filter(Variable %in% "Emissions|CO2|Energy", Category %in% c("C1", "C2", "C3"))

#emi_AR6_asia_interpolated <- interpolate_and_flag_missing(emi_AR6_asia)

#emi_AR6_asia_fixed <- emi_AR6_asia_interpolated$interpolated

#national R10 emissions from AR6
emi_AR6_R10 <- read_csv(paste0(config$data$ref, "/AR6_Scenarios_Database_R10_regions_v1.1.csv")) %>% 
  left_join(AR6_meta, by=c("Model", "Scenario")) %>% 
  filter(Region %in% c("R10INDIA+", "R10CHINA+"), Variable %in% "Emissions|CO2|Energy", Category %in% c("C1", "C2", "C3")) %>% 
  pivot_longer(cols = -c(Model, Scenario, Region, Variable, Unit, Category), names_to = "Year", values_to = "Value") %>% 
  filter(Year %in% seq(2020, 2050, by=5)) %>% 
  pivot_wider(names_from = "Year", values_from = "Value")

emi_AR6_R10_interpolated <- interpolate_and_flag_missing(emi_AR6_R10)

emi_AR6_R10_fixed <- emi_AR6_R10_interpolated$interpolated %>% 
  mutate(Type = "R10")

#combine

emi_df <- rbind(emi_AR6_world_fixed, emi_national, emi_AR6_R10_fixed)
#mutate(Type="Global")

emi_rate_df <- emi_df %>% 
  mutate(Year = as.numeric(Year)) %>%
  group_by(Model, Region,Scenario, Category) %>%
  mutate(
    baseline_2020 = Value[Year == 2020][1],
    reduction_rate = (baseline_2020 - Value) / baseline_2020
  ) %>% 
  ungroup()


#plot

fill_colors <- c(
  NZS = "red",
  C1 = "#C7E9C0",
  C2 = "#FED9A6",
  C3 = "#CCE5FF"
)

p1 <- ggplot(emi_rate_df %>% filter(Type %in% c("Global", "National")),
             aes(x = factor(Year),
                 y = reduction_rate,
                 fill = Category)) +
  
  geom_jitter(
    aes(color = Category),
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.8
    ),
    size = 1.2,
    alpha = 0.9
  ) +
  
  geom_boxplot(
    aes(group = interaction(Year, Category)),
    position = position_dodge(width = 0.8),
    width = 0.4,
    fill = NA,
    colour = "black",
    outlier.shape = NA,
    linewidth = 0.4
  )+
  
  scale_colour_manual(values = fill_colors,
                      guide = guide_legend(
                        override.aes = list(size = 4, alpha = 1)
                      ))+
  
  scale_y_continuous(labels = scales::percent)+

  labs(
    x = "Year",
    y = "Reduction rate from 2020",
    fill = "Category",
    color = "Category"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


#for R10 vs national

emi_rate_df2 <- emi_rate_df %>%
  filter(Region %in% c("India","China","R10INDIA+","R10CHINA+")) %>%
  mutate(
    Country = case_when(
      Region %in% c("India","R10INDIA+") ~ "India",
      Region %in% c("China","R10CHINA+") ~ "China"
    ),
    Dataset = case_when(
      Region %in% c("India","China") ~ "National",
      TRUE ~ "R10"
    )
  )


p2 <- ggplot(emi_rate_df2,
             aes(x = factor(Year),
                 y = reduction_rate,
                 fill = Category)) +
  
  geom_jitter(
    aes(color = Category),
    position = position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.8
    ),
    size = 1.2,
    alpha = 0.9
  ) +
  
  geom_boxplot(
    aes(group = interaction(Year, Category)),
    position = position_dodge(width = 0.8),
    width = 0.4,
    fill = NA,
    colour = "black",
    outlier.shape = NA,
    linewidth = 0.4
  )+
  facet_wrap(~Country)+
  scale_colour_manual(values = fill_colors,
                      guide = guide_legend(
                        override.aes = list(size = 4, alpha = 1)
                      ))+
  
  scale_y_continuous(labels = scales::percent)+

  labs(
    x = "Year",
    y = "Reduction rate from 2020",
    fill = "Category",
    color = "Category"
  ) +
  
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_blank()
    
  )

#final plot

legend <- get_legend(
  p1 + theme(legend.position = "right")
)

p1 <- p1 +
  ggtitle("a. Emissions reductions in national scenarios compared with global pathways") +
  
  theme(legend.position = "none",
        plot.title = element_text(
          face = "bold",
          hjust = 0,
          size = 12
        ))

p2 <- p2 +
  ggtitle("b. Comparison of emissions reduction in national and R10 regional pathways") +
  theme(legend.position = "none",
        plot.title = element_text(
          face = "bold",
          hjust = 0,
          size = 12
        ))

top_row <-
  p1 +
  wrap_elements(legend) +
  plot_layout(widths = c(2, 1))

p <-
  top_row /
  p2 +
  plot_layout(heights = c(1, 1))

ggsave(filename=paste0(config$output$supplementary,"/", "5. Emissions reduction in national and global scenarios", ".jpg"), plot=p, width=250, height=150, units='mm', dpi=300)


#Supplementary 6
#effort sharing considering Emissions|CO2 in national scenario

#Coverage of countries for Emissions|CO2
coverage_SI <- fixed_data %>%
  filter(Model %in% national_model,  Variable %in% 'Emissions|CO2') %>% 
  select('Model', 'Region', 'Scenario') %>% 
  group_by(Model, Region) %>%
  filter(any(Scenario == 'NZS')) %>%
  summarise(
    Scenario = paste(unique(Scenario), collapse = ", "),
    .groups = "drop"
  )

emi_cumulative <- fixed_data %>% 
  filter(Variable %in% 'Emissions|CO2') %>% 
  pivot_longer(cols=-c(Model, Scenario, Region, Variable, Unit), names_to = 'Year', values_to = 'Emission', names_transform = as.numeric) %>%
  group_by(Model, Region, Scenario) %>% 
  arrange(Year, .by_group = TRUE) %>% 
  mutate(
    next_Emission = lead(Emission),
    next_Year = lead(Year),
    interval = next_Year - Year,
    avg_Emission = (Emission + next_Emission) / 2,
    interval_cumulative = avg_Emission * interval,
    interval_cumulative = if_else(is.na(interval_cumulative), 0, interval_cumulative),
    cumulative = cumsum(interval_cumulative)) %>% 
  ungroup() %>% 
  mutate(cumulative=cumulative/1000, Unit = "GtCO2") %>% 
  filter(Year %in% '2050') %>% 
  select(Model, Scenario, Region, Variable, Unit,  cumulative)


#effort sharing for combined country chart with nzs budget as 1
emi_cumulative_median <- emi_cumulative %>% 
  filter(Model %in% coverage_SI$Model) %>% 
  filter(Scenario %in% 'NZS') %>% 
  group_by(Region, Scenario) %>%
  summarize(
    emi_median = median(cumulative, na.rm = TRUE),
    emi_lower = quantile(cumulative, 0.25, na.rm = TRUE),
    emi_upper = quantile(cumulative, 0.75, na.rm = TRUE)
  ) %>% ungroup() 

emi_effort_c <- emi_cumulative_median %>% 
  left_join(effort_sharing_C, by='Region')

emi_effort_c_index <- emi_effort_c %>% 
  mutate(emi_median_idx = emi_median/emi_median, 
         emi_lower_idx = emi_lower/emi_median,
         emi_upper_idx = emi_upper/emi_median,
         min_CB_idx = min_CB/emi_median,
         max_CB_idx = max_CB/emi_median
  ) %>% 
  filter(!Category %in% 'C3')

emi_effort_c_index$a <- as.character(emi_effort_c_index$a)

#adding income group
emi_effort_c_index_w_income <- left_join(emi_effort_c_index, region_mapping %>% select(!"Region"), by=c("Region"="Country")) %>% 
  left_join(Income_Class, by =c("ISO"="Code"))


#plots
p3.1 <- plot_income_group(emi_effort_c_index_w_income, group = 'High income')
p3.2 <- plot_income_group(emi_effort_c_index_w_income, group = 'Upper middle income')
p3.3 <- plot_income_group(emi_effort_c_index_w_income, group = 'Lower middle income')
#p3.4 <- plot_income_group(emi_effort_c_index_w_income, group = 'Low income') #no low income countries in this analysis


#design numbers of countries as column for the combined chart
design <- emi_effort_c_index_w_income %>%
  distinct(ISO, `Income group`) %>%
  mutate(`Income group` = factor(`Income group`, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) %>%
  count(`Income group`) %>%
  { paste0(
    mapply(
      function(n, l)
        paste0(strrep(l, n), strrep("#", max(.$n) - n)),
      .$n, LETTERS[1:nrow(.)]
    ),
    collapse = "\n"
  ) }

p3 <- p3.1 / p3.2 / p3.3 +
  plot_layout(design = design, guides = "collect")+
  plot_annotation(
    # title = 'Cumulative Emissions and Effort-Sharing Carbon Budgets by Income Group'
  )+
  labs(
    x = "Effort Sharing Schemes",
    y = "Index (Median Cumulative NZS Emissions = 1)"
  )&
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    axis.title.y = element_text(hjust = -0.5)
  )

ggsave(filename=paste0(config$output$supplementary,"/", "6. Cumulative Emissions and Effort Sharing Budgets by Income Group Emissions CO2", ".jpg"), plot=p3, width=300, height=170, units='mm', dpi=300)


#supplementary 7
#considering Emissions|CO2 for aggregated analysis

#historical cumulative for countries considered in this study
#hist_emi_ene <- load_hist_emi(config)
hist_emi<- read_excel(paste0(config$data$ref, "/IEA_EDGAR_CO2_1970_2022.xlsx"), sheet=3, skip = 9) %>% 
  rename(Region=Name, Variable=Substance) %>% 
  select("Region", "Variable", paste0("Y_", 1990:2020)) %>% 
  rename_with(~ gsub("Y_", "", .x), starts_with("Y_")) %>% 
  pivot_longer(cols = -c(Region, Variable), values_to = "Value", names_to = "Year", names_transform=as.numeric) %>% 
  mutate(Value=Value/1000, Year = as.numeric(Year)) %>% 
  mutate(Region=recode(Region, "Korea, Republic of"="South Korea"))

hist_emi_1990_2020 <- hist_emi %>% 
  group_by(Region) %>% 
  summarize(cum_1990_2020 = sum(Value, na.rm = TRUE)/1000) %>% 
  mutate(Total = sum(cum_1990_2020)) %>% 
  mutate(percentage = 100*cum_1990_2020/Total) %>% 
  filter(Region %in% coverage_SI$Region) %>% 
  select("Region", "percentage")


hist_emi_1990_2020 <- hist_emi_1990_2020 %>% 
  bind_rows(tibble(Region = "Other", 
                   percentage = 100 - sum(hist_emi_1990_2020$percentage))) %>% 
  arrange(desc(percentage))

hist_emi_1990_2020 <- hist_emi_1990_2020 %>%
  mutate(
    cumshare = cumsum(percentage),
    ypos = cumshare - percentage/2,
    label = if_else(row_number() <= 4,
                    sprintf("%.2f%%", percentage), ""),
    Category = "Historical (1990-2020)"
  ) 

#2020 emission

hist_emi_2020 <- hist_emi%>%
  filter(Year %in% "2020") %>% 
  group_by(Region) %>% 
  summarize(emi_2020 = sum(Value, na.rm = TRUE)/1000) %>% 
  mutate(Total = sum(emi_2020)) %>% 
  filter(Region %in% coverage_SI$Region) %>%
  mutate(percentage = 100*emi_2020/Total) %>% 
  select("Region", "percentage")

hist_emi_2020 <- hist_emi_2020 %>% 
  bind_rows(tibble(Region = "Other", 
                   percentage = 100 - sum(hist_emi_2020$percentage))) %>% 
  arrange(desc(percentage))

hist_emi_2020 <- hist_emi_2020 %>%
  mutate(
    cumshare = cumsum(percentage),
    ypos = cumshare - percentage/2,
    label = if_else(row_number() <= 4,
                    sprintf("%.2f%%", percentage), ""),
    Category = "Historical (2020)"
  ) 


emi_coverage <- rbind(hist_emi_1990_2020, hist_emi_2020)

emi_coverage$Region <- factor(emi_coverage$Region,
                              levels = c(setdiff(emi_coverage$Region, "Other"), "Other"))

#chart
p4.1 <-ggplot(emi_coverage, aes(x = 2, y = percentage, fill = Region)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  scale_fill_manual(values = region_colors, name = "Countries", guide = guide_legend(ncol = 1)) +
  coord_polar("y", start = 0, direction = -1) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size=3, angle=60, color = "black") +
  facet_wrap("Category")+
  xlim(0, 2.5) +
  theme_void() +
  labs(title = "a. Emissions coverage") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none",
        legend.direction = "vertical")

#C1, c2 range across effort sharing for the considered countries in this study 
c_range_n <- effort_sharing_C %>% 
  group_by(a, Category) %>% 
  summarise(lower=sum(min_CB), upper=sum(max_CB)) %>% filter(!Category %in% 'C3')


# find max value across all y-like measures
ymin <- min(
  emi_cumulative_median$emi_lower, 
  #c_range_w$lower,
  c_range_n$lower)

ymax <- max(
  emi_cumulative_median$emi_upper,
  #c_range_w$upper,
  c_range_n$upper)

if (ymin > 0) ymin <- 0
ymax <- ymax * 1.05

#emissions bar chart
g_emi_nzs_bar <- emi_cumulative_median %>%
  mutate(Region = fct_reorder(Region, emi_median, .fun = sum, .desc = TRUE)) %>%
  ungroup() %>% 
  ggplot() +
  geom_bar(position=position_stack(reverse = TRUE), stat="identity", aes(x = Scenario, y = emi_median, fill=Region)) +
  scale_fill_manual(values = region_colors, name = "") +
  scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  ylab("GCO2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),strip.text.y = element_text(angle = 90, hjust = 0), 
        strip.background = element_blank(),
        legend.position = "none") 

#IPCC AR6 temperature range (C1, C2) for the considered countries
g_world_C <- ggplot(c_range_n) +
  geom_linerange(aes(x = a, ymin = lower, ymax = upper), color="grey",
                 linewidth = 3, position = position_dodge(width = 0.6)) + 
  labs(x = NULL, y = NULL) +
  scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  facet_wrap(~Category)+
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y.left = element_blank(),
    plot.subtitle = element_text(hjust = 0.5),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    legend.position = "right"
  )

p4.3 <- (g_emi_nzs_bar + g_world_C) +
  plot_layout(ncol = 2, widths = c(0.07, 0.4)) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12))+
  labs(x = 'Effort Sharing Schemes',
       title = "d. Cumulative CO2 emissions")   

#IPCC c1, c2 line range chart

AR6_w <- AR6_world %>% 
  filter(Variable %in% 'Emissions|CO2', Category %in% c('C1', 'C2', 'C3'))

processed_AR6_w <- interpolate_and_flag_missing(AR6_w)
AR6_w_interpolated <- processed_AR6_w$clean

data_summary <- AR6_w_interpolated %>%
  group_by(Region, Category, Year) %>%
  summarize(
    median = median(Value/1000, na.rm = TRUE),
    lower = min(Value/1000, na.rm = TRUE),
    upper = max(Value/1000, na.rm = TRUE)
  )

#for the terminal boxplot
summary_box <- data_summary %>% 
  filter(Year %in% '2050') %>% 
  mutate(
    x_box = max(data_summary$Year) + 1   # controls how close it is to the chart
  )

#cumulative emissions from AR6 data
emi_cumulative_AR6 <- AR6_w_interpolated %>% 
  mutate(Emission=Value) %>% 
  select('Model', 'Scenario', 'Category', 'Year', 'Emission') %>% 
  group_by(Model, Scenario) %>% 
  arrange(Year, .by_group = TRUE) %>% 
  mutate(
    next_Emission = lead(Emission),
    next_Year = lead(Year),
    interval = next_Year - Year,
    avg_Emission = (Emission + next_Emission) / 2,
    interval_cumulative = avg_Emission * interval,
    interval_cumulative = if_else(is.na(interval_cumulative), 0, interval_cumulative),
    cumulative = cumsum(interval_cumulative)) %>% 
  ungroup() %>% 
  mutate(cumulative=cumulative/1000, Unit = "GtCO2") %>% 
  filter(Year %in% '2050') %>% 
  select(Model, Scenario, Unit,  cumulative, Category)

emi_AR6_summary <- emi_cumulative_AR6 %>% 
  group_by(Category) %>% 
  summarize(
    median = median(cumulative, na.rm = TRUE),
    lower = min(cumulative, na.rm = TRUE),
    upper = max(cumulative, na.rm = TRUE)
  )

p4.4 <- ggplot()+
  geom_boxplot(data = emi_AR6_summary, aes(x=Category, y=median, ymin=lower, ymax = upper, lower=lower, upper=upper, middle = median, fill = Category, color=Category), 
               stat = "identity",
               alpha = 0.2)+
  coord_flip()+
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),strip.text.y = element_text(angle = 90, hjust = 0), 
        strip.background = element_blank(),
        legend.position = "none",
        legend.title.align = 0.5,  
        legend.text.align = 0) +  
  labs(y = 'GtCO2',
       title = "c. Cumulative emissions from (b)")  

p4.2 <- ggplot() +
  geom_ribbon(data = data_summary, aes(x = Year, ymin = lower, ymax = upper, fill = Category), alpha = 0.2) +
  geom_line(data = data_summary, aes(x = Year, y = median, color = Category), size = 1) +
  geom_boxplot(data = summary_box,
               aes(
                 x = x_box,
                 y=median,
                 ymin = lower,
                 middle = median,
                 ymax = upper,
                 lower=lower,
                 upper = upper,
                 fill=Category,
                 color=Category),
               outlier.shape = NA,
               stat = "identity",
               width = 1.5,
               alpha = 0.2
  ) +
  geom_text(data=summary_box, aes(x = x_box + c(-0.5, 0, 0.5), y=lower-1, label=Category), hjust = 0, size=2)+
  theme_bw() +
  theme(plot.title = element_text(size = 12, face = "bold"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),strip.text.y = element_text(angle = 90, hjust = 0), 
        strip.background = element_blank(),
        legend.position = "none", 
        legend.title.align = 0.5, 
        legend.text.align = 0) +
  labs(y = 'GtCO2/yr',
       title = "b. Net global CO2 emissions (IPCC AR6)")

#plotting map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  select("name", "continent", "iso_a3", "geometry") %>% 
  mutate(name=recode(name, "United States of America"="United States", "Vietnam"="Viet Nam")) %>% 
  mutate(iso_a3=case_when(name=="Norway"~"NOR",
                          name=="France"~"FRA",
                          TRUE ~ iso_a3
  ))

emi_2020_map <- hist_emi %>%
  filter(Year %in% "2020") %>% 
  group_by(Region) %>% 
  summarize(emi_2020 = sum(Value, na.rm = TRUE)/1000) %>% 
  filter(Region %in% coverage_SI$Region)

#map for selected countries
map_df <- world %>%
  left_join(emi_2020_map, by = c("name" = "Region"))  

p4.5 <- ggplot(map_df) +
  geom_sf(aes(fill = emi_2020), color = "white", linewidth = 0.1) +
  scale_fill_gradient(low = "blanchedalmond", high = "indianred4", na.value = "grey80", name = "CO2 emissions in 2020 (Gt)")+ 
  labs(title = "e. Countries covered in this study")+
  theme_void()+
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.position = "bottom",
        legend.direction = "horizontal")


legend1 <- get_legend(
  p4.1 +
    theme(
      legend.position = "right",
      legend.direction = "vertical"
    )
)

p4.1_noleg <- p4.1 +
  theme(legend.position = "none")

middle_block <- (
  (
    p4.2 /
      p4.4
  ) +
    plot_layout(heights = c(0.8, 0.2))
) |
  p4.3

top_block <- (
  p4.1_noleg /
    middle_block
) +
  plot_layout(heights = c(0.5, 0.5))

top_with_legend <- (
  top_block |
    wrap_elements(full = legend1)
) +
  plot_layout(widths = c(0.85, 0.15))

p4 <- (
  top_with_legend /
    p4.5
) +
  plot_layout(heights = c(0.6, 0.4))


ggsave(filename=paste0(config$output$supplementary,"/", "7. Emissions Coverage and Remaining Budget Emissions CO2", ".jpg"), plot=p4, width=240, height=310, units='mm', dpi=300)

#plot coverage

#full coverage information

coverage_full <- union(coverage, coverage_SI)

#save as csv
write.csv(coverage_full, paste0(config$output$supplementary, "/Model_region_scenario_coverage.csv"), row.names = FALSE)


#Supplementary 8
#effort sharing charts for each country 

lapply(countries, function(countries){
  plot_effort_sharing(
    emi_cumulative,
    effort_sharing_range,
    effort_sharing_C,
    countries
  )
})


#Additional
#Full coverage in a map

country_label <- coverage_full %>%
  group_by(Region) %>%
  summarise(
    label = paste0(
      Region,
      "\n",
      paste(unique(Model), collapse="\n")
    )
  ) %>% unique()

world <- ne_countries(returnclass = "sf")

world$Region <- world$name

# fix names
world$Region[world$Region=="United States of America"] <- "United States"
world$Region[world$Region=="Korea, Rep."] <- "South Korea"
world$Region[world$Region=="Vietnam"] <- "Viet Nam"

countries <- world %>%
  inner_join(country_label)

centers <- st_centroid(countries)

coord <- st_coordinates(centers)

centers$x <- coord[,1]
centers$y <- coord[,2]

label_pos <- tribble(
  ~Region,          ~x_box, ~y_box,
  
  # North
  "Canada",          -90,    70,
  "Norway",          0,    70,
  "Germany",          -15,    55,
  "Switzerland",      55,    70,
  "Ukraine",          55,    55,
  "Portugal",        -30,    40,
  "Algeria",          -20,   10,
  "Turkey",           15,   10,
  
  # West
  "United States",  -150,    40,
  "Mexico",         -150,    15,
  "Ecuador",        -90,   -10,
  "Brazil",         -40,   -10,
  "Argentina",      -40,   -50,
  
  # East
  "Japan",           175,    35,
  "South Korea",     175,    70,
  "China",           175,     0,
  "Nepal",           60,   40,
  
  # South
  "India",            60,   -40,
  "Saudi Arabia",     50,   10,
  "Thailand",        90,   -12,
  "Viet Nam",        120,   5,
  "Indonesia",       120,   -50,
  "Australia",       170,   -40
)

label_df <- centers %>%
  left_join(label_pos, by = "Region")

plot_map <- ggplot() +
  
  geom_sf(data=world,
          fill="grey95",
          color="grey60",
          linewidth=0.2)+
  
  geom_sf(data=countries,
          fill="steelblue")+
  
  geom_segment(
    data=label_df,
    aes(
      x=x,
      y=y,
      xend=x_box,
      yend=y_box
    ),
    linewidth=0.4
  )+
  
  geom_label(
    data=label_df,
    aes(
      x=x_box,
      y=y_box,
      label=label
    ),
    size=2.5,
    label.size=0.3
  )+
  
  coord_sf(
    xlim=c(-180,180),
    ylim=c(-90,90)
  )+
  
  theme_void()

ggsave(filename=paste0(config$output$supplementary,"/", "9. Coverage Map", ".jpg"), plot=plot_map, width=285, height=150, units='mm', dpi=300)



