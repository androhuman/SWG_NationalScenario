library(tidyverse)
library(readr)
library(readxl)
library(RColorBrewer)
library(broom)
library(patchwork)
library(sf)
library(rnaturalearth)


source("prog/config.R")
source("prog/region_mapping.R")
source("prog/load_data.R")
source("prog/process_data.R")
source("prog/unit_correction_table.R")
source("prog/functions.R")

#-----------------------
# Pre processing
#---*---*---*---*---*---

create_dirs(config)

var_list <- load_var_list(config)

scen_data <- load_scen_data(config, scen_dir, var_list, national_model)

processed_data <- interpolate_and_flag_missing(scen_data)

#flagged <- processed_data$flagged

clean_data <- processed_data$clean %>% 
  pivot_wider(names_from = Year, values_from = Value)

fixed_data <- fix_unit(clean_data, correction_table)

AR6_R5Asia <- read_csv(paste0(config$data$ref, "/AR6_R5Asia.csv"))

AR6_world <- read_csv(paste0(config$data$ref, "/AR6_world.csv")) 

AR6_meta <-  read_excel(paste0(config$data$ref, "/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"), sheet=2) %>% 
  select("Model", "Scenario", "Category")

#effort_sharing <- load_effort_sharing(config)

effort_sharing <- read_csv(paste0(config$data$ref, "/effort_sharing.csv"))

Income_Class <- load_WB_Class(config) %>% 
  select("Code", "Income group")

#coverage
coverage <- fixed_data %>%
  filter(Variable %in% 'Emissions|CO2|Energy') %>% 
  select('Model', 'Region', 'Scenario') %>% 
  group_by(Model, Region) %>%
  filter(any(Scenario == 'NZS')) %>%
  summarise(
    Scenario = paste(unique(Scenario), collapse = ", "),
    .groups = "drop"
  )

#calculate new parameters from all scenario
scen_calc <- var_calc(fixed_data) %>% na.omit()

#calculate change rate from baseline
reduct_rate <- scen_calc %>% 
  filter(Variable %in% c("Emissions|CO2|Energy",
                         #"Emissions|CO2",
                         "Final Energy",
                         "GDP Per Capita",
                         "Energy Intensity of GDP",
                         "Emissions Intensity of GDP",
                         #"Emission_CO2 Intensity of GDP",
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


#dataframe for regression analysis
trend_df <- rbind(reduct_rate, share_change) %>% 
  na.omit()


#-----------------------
# Figure 1
#---*---*---*---*---*---

#regression charts for key variables 
p_1 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Final Energy|Rate|Change", 
                       scen='NZS',
                       p_title="a. Reduction in final energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change from baseline (%)")

p_2 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Final Energy|Electricity|Share|Change", 
                       scen='NZS',
                       p_title="b. Electricity share in final energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

p_3 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Share|Change", 
                       scen='NZS',
                       p_title="c. Non-fossil share in primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

p_4 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Fossil|Share|Change", 
                       scen='NZS',
                       p_title="d. Fossil share in primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

p_5 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Solar|Share|Change", 
                       scen='NZS',
                       p_title="e. Solar share in non-fossil primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

p_6 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Primary Energy|Non-fossil|Wind|Share|Change", 
                       scen='NZS',
                       p_title="f. Wind share in non-fossil primary energy", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Change in share from baseline (%)")

p_7 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Carbon Intensity of TPES|Rate|Change", 
                       scen='NZS',
                       p_title="g. Carbon intensity of TPES", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Decrease from baseline (%)")

p_8 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Energy Intensity of GDP|Rate|Change", 
                       scen='NZS',
                       p_title="h. Energy intensity of GDP", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Decrease from baseline (%)")

p_9 <- plot_regression(trend_df, 
                       x_var="Emissions|CO2|Energy|Rate|Change", 
                       y_var="Emissions Intensity of GDP|Rate|Change", 
                       scen='NZS',
                       p_title="i. Emissions intensity of GDP", 
                       xlab="Emissions reduction rate from baseline (%)", 
                       ylab="Decrease from baseline (%)")

#not in the main chart
p_10 <- plot_regression(trend_df, 
                        x_var="Emissions|CO2|Energy|Rate|Change", 
                        y_var="GDP Per Capita|Rate|Change", 
                        scen='NZS',
                        p_title="j. GDP per capita", 
                        xlab="Emissions reduction rate from baseline (%)", 
                        ylab="Decrease from baseline (%)")

legend <- get_legend(p_1$plot) 

p <- (p_1$plot + p_2$plot + p_3$plot + p_4$plot + p_5$plot + p_6$plot + p_7$plot + p_8$plot + p_9$plot) & 
  theme(legend.position = "none") 

p1 <- p / legend +
  plot_layout(heights = c(1, 0.08))

ggsave(filename=paste0(config$output$main,"/", "1. Regression of Key Variables", ".jpg"), plot=p1, width=335, height=300, units='mm', dpi=300)


#save regression statistics 
regression_table <- bind_rows(
  p_1$stats,
  p_2$stats,
  p_3$stats,
  p_4$stats,
  p_5$stats,
  p_6$stats,
  
  p_7$stats,
  p_8$stats,
  p_9$stats,
  p_10$stats
) 

#annotate stars to significance levels
slope_star <- paste0((regression_table$slope),
                     sig_stars(regression_table$p_value)
)

regression_table <- regression_table %>%
  mutate(slope = paste0(round(slope, 3), sig_stars(p_value)))

write_csv(regression_table, file = file.path(config$output$main, "/", "regression_summary.csv"))


#-----------------------
# Figure 2
#---*---*---*---*---*---

#national trends comparison with regional/global trends from IPCC AR6

#calculate new parameters from all scenario
IPCC_R5ASIA_calc <- var_calc(AR6_R5Asia) %>% na.omit()

#add C1, C2, C3 from metadata
IPCC_R5ASIA_calc <- left_join(IPCC_R5ASIA_calc, AR6_meta, by=c("Model", "Scenario")) %>%
  filter(Category %in% c('C1','C2','C3'))


# Define color palette
fill_colors <- c(
  NZS = "red",
  C1 = "#C7E9C0",
  C2 = "#FED9A6",
  C3 = "#CCE5FF"
)

#combine regional and national data
share_R5ASIA <- IPCC_R5ASIA_calc %>% 
  pivot_longer(cols=-c(Model, Region, Scenario, Variable, Category), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  filter(Variable %in% c("Primary Energy|Non-fossil|Share",
                         "Primary Energy|Non-fossil|Solar|Share",
                         "Primary Energy|Non-fossil|Wind|Share",
                         "Final Energy|Electricity|Share"
  ), Year %in% c('2020', '2030', '2040', '2050')) %>% 
  mutate(Category = factor(Category, levels = c("C3", "C2", "C1")))

share_national <- scen_calc %>% 
  pivot_longer(cols=-c(Model, Region, Scenario, Variable), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  semi_join(
    region_mapping %>% filter(Region == "Asia"),
    by = c("Region" = "Country")) %>% 
  filter(Scenario %in% 'NZS', 
         Variable %in% c("Primary Energy|Non-fossil|Share",
                    "Primary Energy|Non-fossil|Solar|Share",
                    "Primary Energy|Non-fossil|Wind|Share",
                    "Final Energy|Electricity|Share"
    ), Year %in% c('2020', '2030', '2040', '2050')) %>% 
  mutate(Category = "NZS")

share_asia <- bind_rows(share_R5ASIA, share_national) %>% 
  mutate(Type = "Asia")

#global
IPCC_World_calc <- var_calc(AR6_world) %>% na.omit()

IPCC_World_calc <- left_join(IPCC_World_calc, AR6_meta, by=c("Model", "Scenario")) %>%
  filter(Category %in% c('C1','C2','C3'))

#combine global and national data 
share_world <- IPCC_World_calc %>% 
  pivot_longer(cols=-c(Model, Region, Scenario, Variable, Category), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  filter(Variable %in% c("Primary Energy|Non-fossil|Share",
                         "Primary Energy|Non-fossil|Solar|Share",
                         "Primary Energy|Non-fossil|Wind|Share",
                         "Final Energy|Electricity|Share"
  ), Year %in% c('2020', '2030', '2040', '2050')) %>% 
  mutate(Category = factor(Category, levels = c("C3", "C2", "C1")))

share_national <- scen_calc %>% 
  pivot_longer(cols=-c(Model, Region, Scenario, Variable), names_to="Year", values_to="Value", names_transform=as.numeric) %>% 
  filter(Scenario %in% 'NZS', Variable %in% c("Primary Energy|Non-fossil|Share",
                                              "Primary Energy|Non-fossil|Solar|Share",
                                              "Primary Energy|Non-fossil|Wind|Share",
                                              "Final Energy|Electricity|Share"
  ), Year %in% c('2020', '2030', '2040', '2050')) %>% 
  mutate(Category = "NZS")

share_world <- bind_rows(share_world, share_national) %>% 
  mutate(Type = "Global")

#combined dataframe for plot
share_world_asia <- bind_rows(share_asia, share_world) %>% 
  mutate(Variable = recode(Variable, "Primary Energy|Non-fossil|Share"="Non-fossil share in primary energy",
                           "Primary Energy|Non-fossil|Solar|Share"="Solar share in non-fossil primary energy",
                           "Primary Energy|Non-fossil|Wind|Share"="Wind share in non-fossil primary energy",
                           "Final Energy|Electricity|Share"="Electricity share in final energy"))

share_world_asia$Type <- factor(share_world_asia$Type, levels = c("Global", "Asia")) %>% na.omit()

#count of scenarios for each variables

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

p2 <- ggplot(share_world_asia, 
             aes(x = factor(Year), y = Value, fill = Category)) +
  geom_boxplot(
    width = 0.6,
    outlier.shape = NA,
    position = position_dodge(width = 0.8),
    color = "black",
    alpha = 0.5
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
    y = "Share(%)"
    #title = "National NZS vs Global and Regional Trends"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom"
  )

ggsave(filename=paste0(config$output$main,"/", "2. National scenario trend comparison with regional and global trend", ".jpg"), plot=p2, width=250, height=150, units='mm', dpi=300)

#-----------------------
# Figure 3
#---*---*---*---*---*---

#cumulative nzs emissions and global emission budget/effort sharing comparison 

emi_cumulative <- fixed_data %>% 
  filter(Variable %in% 'Emissions|CO2|Energy') %>% 
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


effort_sharing_range <- effort_sharing %>% 
  filter(rcb %in% seq(400,1200, by=100)) %>% 
  group_by(a, Region) %>% 
  summarise(
    min_CB = min(CB, na.rm = TRUE),
    max_CB = max(CB, na.rm = TRUE),
    .groups = "drop"
  )

#C1, C2, C3 mapping

effort_sharing_C <- effort_sharing %>% 
  filter(rcb %in% seq(400,1200, by=100)) %>% 
  mutate(rcb = as.numeric(as.character(rcb)),
         Category = case_when(
           rcb >= 400 & rcb <= 700  ~ "C1",
           rcb >= 500 & rcb <= 900  ~ "C2",
           rcb >= 600 & rcb <= 1200 ~ "C3",
           TRUE ~ NA_character_
         )) %>% 
  group_by(Region, a, Category) %>% summarise(
    min_CB = min(CB, na.rm = TRUE),
    max_CB = max(CB, na.rm = TRUE),
    .groups = "drop"
  )

#effort sharing for combined country chart with nzs budget as 1
emi_cumulative_median <- emi_cumulative %>% 
  filter(Scenario %in% 'NZS') %>% 
  group_by(Region, Scenario) %>%
  summarize(
    emi_median = median(cumulative, na.rm = TRUE),
    emi_lower = min(cumulative, na.rm = TRUE),
    emi_upper = max(cumulative, na.rm = TRUE)
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

ggsave(filename=paste0(config$output$main,"/", "3. Cumulative Emissions and Effort Sharing Budgets by Income Group", ".jpg"), plot=p3, width=300, height=170, units='mm', dpi=300)

#-----------------------
# Figure 4
#---*---*---*---*---*---

#figure with summation of NZS emissions

#historical cumulative for countries considered in this study
hist_emi_ene <- load_hist_emi(config)

hist_emi_1990_2020 <- hist_emi_ene %>% 
  group_by(Region) %>% 
  summarize(cum_1990_2020 = sum(Value, na.rm = TRUE)/1000) %>% 
  mutate(Total = sum(cum_1990_2020)) %>% 
  mutate(percentage = 100*cum_1990_2020/Total) %>% 
  filter(Region %in% coverage$Region) %>%  
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

hist_emi_2020 <- hist_emi_ene %>%
  filter(Year %in% "2020") %>% 
  group_by(Region) %>% 
  summarize(emi_2020 = sum(Value, na.rm = TRUE)/1000) %>% 
  mutate(Total = sum(emi_2020)) %>% 
  filter(Region %in% coverage$Region) %>% 
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

emi_2020_2050 <- emi_cumulative_median %>% 
  select("Region", "emi_median") %>% 
  mutate(`C2_median`=639.8031) %>% #aggregate national cumulative median emissions exceed C1 median value 
  mutate(percentage = 100*emi_median/C2_median) %>% 
  select("Region", "percentage")

emi_2020_2050 <- emi_2020_2050 %>% 
  bind_rows(tibble(Region = "Other", 
                   percentage = 100 - sum(emi_2020_2050$percentage))) %>% 
  arrange(desc(percentage))

emi_2020_2050 <- emi_2020_2050 %>%
  mutate(
    cumshare = cumsum(percentage),
    ypos = cumshare - percentage/2,
    label = if_else(row_number() <= 4,
                    sprintf("%.2f%%", percentage), ""),
    Category = "Net Zero (2020-2050)"
  ) 

emi_coverage <- rbind(hist_emi_1990_2020, hist_emi_2020)

emi_coverage$Region <- factor(emi_coverage$Region,
                              levels = c(setdiff(emi_coverage$Region, "Other"), "Other"))

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
  c_range_n$lower)

ymax <- max(
  emi_cumulative_median$emi_upper,
  c_range_n$upper)

if (ymin > 0) ymin <- 0
ymax <- ymax * 1.05

#emissions bar chart, making the same order of countries for legend
emi_cumulative_median$Region <- factor(emi_cumulative_median$Region,levels = c(setdiff(hist_emi_2020$Region, "Other")))

g_emi_nzs_bar <- emi_cumulative_median %>%
  ggplot() +
  geom_bar(position=position_stack(reverse = TRUE), stat="identity", aes(x = Scenario, y = emi_median, fill=Region)) +
  scale_fill_manual(values = region_colors, name = "") +
  scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
  ylab("GtCO2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),strip.text.y = element_text(angle = 90, hjust = 0), 
        strip.background = element_blank(),
        legend.position = "none") 
#labs(x = NULL) 

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
  filter(Variable %in% 'Emissions|CO2|Energy', Category %in% c('C1', 'C2', 'C3'))

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

emi_2020_map <- hist_emi_ene %>%
  filter(Year %in% "2020") %>% 
  group_by(Region) %>% 
  summarize(emi_2020 = sum(Value, na.rm = TRUE)/1000) %>% 
  filter(Region %in% coverage$Region)

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


middle_block <- (p4.2 / p4.4 + plot_layout(heights = c(0.8, 0.2))) | p4.3

top_block <- (p4.1 /
                (middle_block + plot_layout(widths = c(0.5,0.5)))) +
  plot_layout(heights = c(0.5,0.5))

legend1 <- get_legend(
  p4.1 + theme(legend.position = "right",
               legend.direction = "vertical")
)

top_with_legend <- plot_grid(
  top_block,
  legend1,
  ncol = 2,
  rel_widths = c(0.85, 0.15)
) 

p4 <- plot_grid(
  top_with_legend,
  p4.5,
  ncol = 1,
  rel_heights = c(0.6, 0.4)
)

ggsave(filename=paste0(config$output$main,"/", "4. Emissions Coverage and Remaining Budget", ".jpg"), plot=p4, width=240, height=310, units='mm', dpi=300)


#-----------------------
# supplementary
#---*---*---*---*---*---

source("prog/sup_plots.R")
