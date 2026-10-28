#this file runs a leave-one-out diagnostic for the 9 regressions in Figure 2.
#Each model (or country) is removed in turn and every regression is refitted with the
#existing plot_regression() function.

LOO_X_VAR <- "Emissions|CO2|Energy|Rate|Change"
LOO_SCEN  <- "NZS"

#the 9 regressions of Figure 2 (same y variables as p_1 ... p_9 in main.R)
LOO_SPECS <- tribble(
  ~panel,                                          ~y_var,
  "a. Reduction in final energy",                  "Final Energy|Rate|Change",
  "b. Electricity share in final energy",          "Final Energy|Electricity|Share|Change",
  "c. Non-fossil share in primary energy",         "Primary Energy|Non-fossil|Share|Change",
  "d. Fossil share in primary energy",             "Primary Energy|Fossil|Share|Change",
  "e. Solar share in non-fossil primary energy",   "Primary Energy|Non-fossil|Solar|Share|Change",
  "f. Wind share in non-fossil primary energy",    "Primary Energy|Non-fossil|Wind|Share|Change",
  "g. Carbon intensity of TPES",                   "Carbon Intensity of TPES|Rate|Change",
  "h. Energy intensity of GDP",                    "Energy Intensity of GDP|Rate|Change",
  "i. Emissions intensity of GDP",                 "Emissions Intensity of GDP|Rate|Change"
)


#x-y pairs prepared the same way as in plot_regression(); used for the held-out points
loo_xy <- function(df, y_var, x_var = LOO_X_VAR, scen = LOO_SCEN) {
  
  w <- df %>%
    select("Region", "Model", "Scenario", "Variable",
           "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>%
    filter(Scenario %in% scen, Variable %in% c(x_var, y_var)) %>%
    pivot_longer(cols = -c(Model, Scenario, Region, Variable),
                 names_to = "Year", values_to = "Value",
                 names_transform = list(Year = as.numeric)) %>%
    pivot_wider(names_from = "Variable", values_from = "Value")
  
  if (!all(c(x_var, y_var) %in% names(w))) return(tibble(x_val = numeric(), y_val = numeric()))
  
  w %>%
    rename(x_val = !!rlang::sym(x_var), y_val = !!rlang::sym(y_var)) %>%
    filter(!is.na(x_val) & !is.na(y_val))
}


#regression statistics from the existing plot_regression(); NA row if the fit is not possible
loo_fit <- function(df, y_var, x_var = LOO_X_VAR, scen = LOO_SCEN) {
  tryCatch(
    plot_regression(df, x_var = x_var, y_var = y_var, scen = scen,
                    p_title = "", xlab = "", ylab = "")$stats,
    error = function(e) tibble(Variable = y_var, slope = NA_real_, intercept = NA_real_,
                               r_squared = NA_real_, rmse = NA_real_, p_value = NA_real_,
                               n_points = 0L)
  )
}


#removes each value of `group_col` in turn, refits all regressions and compares with the
#full-sample fit. `label` is the name of the output column holding the removed group.
run_loo <- function(df, group_col, label, specs = LOO_SPECS) {
  
  groups <- sort(unique(df[[group_col]]))
  
  #full-sample reference
  full <- map_dfr(seq_len(nrow(specs)), function(i) {
    loo_fit(df, specs$y_var[i]) %>% mutate(panel = specs$panel[i])
  }) %>%
    select(panel, Variable,
           slope_full = slope, r_squared_full = r_squared,
           p_value_full = p_value, n_points_full = n_points)
  
  res <- map_dfr(groups, function(g) {
    train <- df %>% filter(.data[[group_col]] != g)
    test  <- df %>% filter(.data[[group_col]] == g)
    
    map_dfr(seq_len(nrow(specs)), function(i) {
      st <- loo_fit(train, specs$y_var[i])
      xy <- loo_xy(test, specs$y_var[i])
      
      #out-of-sample error: predict the removed group's points with the refitted line
      heldout_rmse <- if (nrow(xy) > 0 && !is.na(st$slope)) {
        sqrt(mean((xy$y_val - (st$intercept + st$slope * xy$x_val))^2))
      } else NA_real_
      
      st %>% mutate(removed = g,
                    panel = specs$panel[i],
                    n_removed_points = nrow(xy),
                    heldout_rmse = round(heldout_rmse, 3))
    })
  })
  
  res %>%
    left_join(full, by = c("panel", "Variable")) %>%
    mutate(significance     = sig_stars(p_value),
           slope_change     = slope - slope_full,
           slope_change_pct = round(100 * slope_change / abs(slope_full), 1),
           r_squared_change = r_squared - r_squared_full,
           sign_flip        = sign(slope) != sign(slope_full),
           signif_change    = (p_value < 0.05) != (p_value_full < 0.05)) %>%
    rename(!!label := removed) %>%
    select(all_of(label), panel, Variable, n_removed_points, n_points,
           slope, intercept, r_squared, rmse, p_value, significance, heldout_rmse,
           slope_full, r_squared_full, p_value_full,
           slope_change, slope_change_pct, r_squared_change, sign_flip, signif_change) %>%
    arrange(panel, .data[[label]])
}


#box plot of the refitted slopes for each of the 9 regressions, one panel per regression.
#Left axis: slope. Right axis: the same values expressed as % change 

plot_loo_box <- function(loo, group_label, file, specs = LOO_SPECS, ncol = 3) {
  
  df <- loo %>% 
    filter(n_removed_points > 0, !is.na(slope)) %>% 
    mutate(panel = factor(panel, levels = specs$panel),
           fit = "Leave-one-out fit")
  
  full <- df %>% 
    distinct(panel, slope_full) %>% 
    mutate(fit = "Full sample")
  
  panel_names <- levels(droplevels(df$panel))
  
  make_panel <- function(i) {
    pn     <- panel_names[i]
    d      <- df   %>% filter(panel == pn)
    f      <- full %>% filter(panel == pn)
    s_full <- f$slope_full[1]
    
    #% change from the full-sample slope; no secondary axis if the full slope is 0 or missing
    sec <- if (!is.na(s_full) && s_full != 0) {
      sec_axis(function(x) 100 * (x - s_full) / abs(s_full),
               name   = if (i %% ncol == 0 || i == length(panel_names)) "Change from full sample (%)" else NULL,
               labels = function(x) paste0(round(x, 1), "%"))
    } else waiver()
    
    ggplot(d, aes(x = "")) +
      geom_boxplot(aes(y = slope), width = 0.45, fill = "grey88", colour = "grey35",
                   outlier.shape = NA, linewidth = 0.4) +
      geom_point(aes(y = slope, colour = fit, shape = fit),
                 position = position_jitter(width = 0.12, height = 0, seed = 1),
                 size = 1.3, alpha = 0.6) +
      geom_point(data = f, aes(y = slope_full, colour = fit, shape = fit), size = 3) +
      scale_y_continuous(sec.axis = sec) +
      scale_colour_manual(values = c("Leave-one-out fit" = "grey30", "Full sample" = "red"),
                          name = NULL, drop = FALSE) +
      scale_shape_manual(values = c("Leave-one-out fit" = 16, "Full sample" = 18),
                         name = NULL, drop = FALSE) +
      labs(x = NULL,
           y = if (i %% ncol == 1) "Slope" else NULL,
           subtitle = str_wrap(pn, 30)) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.subtitle = element_text(face = "bold", size = 9))
  }
  
  p <- wrap_plots(map(seq_along(panel_names), make_panel), ncol = ncol) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title   = paste0("Regression slopes with one ", group_label, " left out at a time"),
      caption = "Slope: change per 1% emissions reduction from baseline. Right axis: % change from the full-sample slope.",
      theme   = theme(plot.title = element_text(face = "bold"))
    ) &
    theme(legend.position = "bottom")
  
  ggsave(filename = file, plot = p, width = 240, height = 230, units = "mm", dpi = 300)
  
  invisible(p)
}