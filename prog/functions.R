#this file contains the plot related functions and operations

mytheme <-theme(panel.background = element_rect(fill = "white", color = NA),
                plot.background = element_rect(fill = "white", color = NA),  
                panel.grid.major = element_line(color = "grey90"),
                panel.grid.minor = element_line(color = "grey90"), 
                legend.position = "right",
                legend.direction = "vertical",
                axis.line = element_line(colour = "black"),
                plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1),
                #plot.title = element_text(size = 20, face = "bold"),
                axis.title = element_text(size = 16),
                axis.text = element_text(size = 12),
                strip.text = element_text(size = 12)
)


create_dirs <- function(config) {
  paths <- unlist(config, use.names = FALSE)
  paths <- paths[file.exists(dirname(paths)) | paths != ""]
  for (p in paths) {
    if (!dir.exists(p)) {
      dir.create(p, recursive = TRUE)
    }
  }
  message("Directory setup complete.")
}

sig_stars <- function(p) {
  dplyr::case_when(
    is.na(p)        ~ "",
    p < 0.001       ~ "***",
    p < 0.01        ~ "**",
    p < 0.05        ~ "*",
    TRUE            ~ ""
  )
}

plot_scenario_region <- function(scen, region, region_name, p_var, y_lab, dir, w, h) {
  filt <- scen %>% 
    filter(Variable %in% p_var, Region %in% region) %>%
    select("Region", "Model", "Scenario", "Variable", "2020", "2025", "2030", "2035", "2040", "2045", "2050") %>%
    pivot_longer(cols = -c(Region, Model, Scenario, Variable), names_to = "Year", values_to = "Value", names_transform = list(Year = as.numeric))

  unique_models <- unique(filt$Model)
  num_models <-length(unique_models)
  linetype_values <- rep(c("solid", "dashed", "dotted"), length.out=num_models) 
  shape_values <- seq_along(unique_models)   # Ensures unique shapes within ggplot limits
  
  p <- ggplot(filt, aes(x = Year, y = Value, color = Scenario, linetype = Model)) +
    geom_line(size = 1, na.rm = TRUE) +  
    geom_point(aes(shape = Model), size = 2, show.legend = c(color = FALSE), na.rm = TRUE) + 
    scale_color_manual(values = scen_color) +  
    scale_linetype_manual(values = linetype_values) + 
    scale_shape_manual(values = shape_values) +
    
    labs(title = p_var,
         x = "Year",
         y = y_lab) +
    facet_wrap(~Region, scales = "free_y") + 
    mytheme+
    theme() +
    guides(
      color = guide_legend(order = 1),
      linetype = guide_legend(ncol = 1), 
      shape = guide_legend(ncol = 1)
    )
  
  s_var <- gsub("[|/]", "-", p_var)
  
  outdir <- paste0(config$output$supplementary, '/timeseries')
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  ggsave(filename=paste0(outdir,"/", s_var, ".jpg"), plot=p, width=w, height=h, units='mm', dpi=300)
}


plot_regression <- function(scen_calc, x_var, y_var, scen, p_title, xlab, ylab) {

  ci_level = 0.95
  pi_level = 0.95
  n_grid = 200

  obj <- scen_calc %>% 
    select("Region", "Model", "Scenario", "Variable",
           "2020","2025","2030","2035","2040","2045","2050") %>% 
    filter(Scenario %in% scen, Variable %in% c(x_var, y_var)) %>% 
    pivot_longer(cols = -c(Model, Scenario, Region, Variable),
                 names_to = "Year", values_to = "Value",
                 names_transform = list(Year = as.numeric)) %>% 
    pivot_wider(names_from = "Variable", values_from = "Value") %>% 
    rename(x_val = !!rlang::sym(x_var), y_val = !!rlang::sym(y_var)) %>%
    filter(!is.na(x_val) & !is.na(y_val))
  
  if (nrow(obj) < 2) stop("Not enough points to fit regression.")
  
  # Fit model
  fit <- lm(y_val ~ x_val, data = obj)
  tidy_fit <- broom::tidy(fit)
  glance_fit <- broom::glance(fit)
  
  rmse <- sqrt(mean(residuals(fit)^2))
  slope <- round(tidy_fit$estimate[tidy_fit$term == "x_val"], 3)
  intercept <- round(tidy_fit$estimate[tidy_fit$term == "(Intercept)"], 3)
  intercept_sign <- ifelse(intercept < 0, " - ", " + ")
  r2 <- round(glance_fit$r.squared, 3)
  pval <- signif(tidy_fit$p.value[tidy_fit$term == "x_val"], 3)
  
  # Create grid of x for smooth ribbons
  x_seq <- seq(min(obj$x_val, na.rm=TRUE), max(obj$x_val, na.rm=TRUE), length.out = n_grid)
  newdata <- data.frame(x_val = x_seq)
  
  # Predict CI and PI
  ci <- predict(fit, newdata = newdata, interval = "confidence", level = ci_level)
  pi <- predict(fit, newdata = newdata, interval = "prediction", level = pi_level)
  
  pred_df <- tibble::tibble(
    x_val = x_seq,
    fit = ci[, "fit"],
    ci_low = ci[, "lwr"],
    ci_high = ci[, "upr"],
    pi_low = pi[, "lwr"],
    pi_high = pi[, "upr"]
  )
  
  eq_label <- paste0("y = ", slope, "x", intercept_sign, abs(intercept),
                     "\nR² = ", r2)
  
  p <- ggplot(obj, aes(x = x_val, y = y_val)) +
    geom_point(aes(x = x_val, y = y_val), color = "grey40",
               size = 2.5, pch = 21, alpha = 0.2, fill = 'black'
               ) +
    geom_ribbon(data = pred_df,
                aes(x = x_val, ymin = pi_low, ymax = pi_high, fill = "95% PI"), #fill = "orange", 
                alpha = 0.15, inherit.aes = FALSE) +
    geom_ribbon(data = pred_df,
                aes(x = x_val, ymin = ci_low, ymax = ci_high, fill = "95% CI"), 
                alpha = 0.18, inherit.aes = FALSE) +
    geom_line(data = pred_df, aes(x = x_val, y = fit, color = "Regression line"),
              size = 0.8, inherit.aes = FALSE) +
    scale_color_manual(
      name = "",
      values = c("Regression line" = "red")) +
    scale_fill_manual(
      name = "",
      values = c(
        "95% CI" = "blue",
        "95% PI" = "orange"
      ))+
    labs(title = p_title, x = xlab, y = ylab) +
    annotate("text", 
             x = mean(obj$x_val, na.rm=TRUE),
             y = max(c(obj$y_val, pred_df$pi_high), na.rm = TRUE),
             label = eq_label, hjust = 0, vjust = 1, size = 3) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"), 
          legend.position = "bottom",
          legend.direction = "horizontal")
  
  stats_tbl <- tibble::tibble(
    Variable     = y_var,
    slope        = slope,
    intercept    = intercept,
    r_squared    = r2,
    rmse       = round(rmse, 3),
    p_value      = pval,
    n_points     = nrow(obj)
  )
  
  return(list(
    plot  = p,
    stats = stats_tbl
  ))
}


plot_regression_color <- function(scen_calc, x_var, y_var, scen, p_title, xlab, ylab){
  
  ci_level = 0.95
  pi_level = 0.95
  n_grid = 200
  
  obj <- scen_calc %>% 
    select("Region", "Model", "Scenario", "Variable",
           "2020","2025","2030","2035","2040","2045","2050") %>% 
    filter(Scenario %in% scen, Variable %in% c(x_var, y_var)) %>% 
    pivot_longer(cols = -c(Model, Scenario, Region, Variable),
                 names_to = "Year", values_to = "Value",
                 names_transform = list(Year = as.numeric)) %>% 
    pivot_wider(names_from = "Variable", values_from = "Value") %>% 
    rename(x_val = !!rlang::sym(x_var), y_val = !!rlang::sym(y_var)) %>%
    filter(!is.na(x_val) & !is.na(y_val))
  
  if (nrow(obj) < 2) stop("Not enough points to fit regression.")
  
  # Fit model
  fit <- lm(y_val ~ x_val, data = obj)
  tidy_fit <- broom::tidy(fit)
  glance_fit <- broom::glance(fit)
  
  #rmse <- sqrt(mean(residuals(fit)^2))
  slope <- round(tidy_fit$estimate[tidy_fit$term == "x_val"], 3)
  intercept <- round(tidy_fit$estimate[tidy_fit$term == "(Intercept)"], 3)
  intercept_sign <- ifelse(intercept < 0, " - ", " + ")
  r2 <- round(glance_fit$r.squared, 3)
  #pval <- signif(tidy_fit$p.value[tidy_fit$term == "x_val"], 3)
  
  # Create grid of x for smooth ribbons
  x_seq <- seq(min(obj$x_val, na.rm=TRUE), max(obj$x_val, na.rm=TRUE), length.out = n_grid)
  newdata <- data.frame(x_val = x_seq)
  
  # Predict CI and PI
  ci <- predict(fit, newdata = newdata, interval = "confidence", level = ci_level)
  pi <- predict(fit, newdata = newdata, interval = "prediction", level = pi_level)
  
  pred_df <- tibble::tibble(
    x_val = x_seq,
    fit = ci[, "fit"],
    ci_low = ci[, "lwr"],
    ci_high = ci[, "upr"],
    pi_low = pi[, "lwr"],
    pi_high = pi[, "upr"]
  )
  
  eq_label <- paste0("y = ", slope, "x", intercept_sign, abs(intercept),
                     "\nR² = ", r2)
  
  n_countries <- length(unique(obj$Region))
  palette <- colorRampPalette(brewer.pal(8, "Set3"))(n_countries)
  
  palette_named <- setNames(
    colorRampPalette(brewer.pal(8, "Set3"))(n_countries),
    sort(unique(obj$Region))
  )
  
  p <- ggplot(obj) +
   
    geom_ribbon(data = pred_df,
                aes(x = x_val, ymin = pi_low, ymax = pi_high, fill = "95% PI"), #fill = "orange", 
                alpha = 0.15) +
    
    geom_point(aes(x = x_val, y = y_val, color= Region),
               size = 2.5, pch = 20, alpha = 1
    ) +
    geom_ribbon(data = pred_df,
                aes(x = x_val, ymin = ci_low, ymax = ci_high, fill = "95% CI"), 
                alpha = 0.18) +
    geom_line(data = pred_df, aes(x = x_val, y = fit, color = "Regression line"),
              size = 0.8) +
    scale_color_manual(
      name = "",
      values = c(palette_named,
                 "Regression line" = "red"),
      breaks = c(sort(unique(obj$Region)), "Regression line")) +
    scale_fill_manual(
      name = "",
      values = c(
        "95% CI" = "blue",
        "95% PI" = "orange"
      ))+
    labs(title = p_title, x = xlab, y = ylab) +
    annotate("text", 
             x = mean(obj$x_val, na.rm=TRUE),
             y = max(c(obj$y_val, pred_df$pi_high), na.rm = TRUE),
             label = eq_label, hjust = 0, vjust = 1, size = 3) +
    theme_bw() +
    theme(plot.title = element_text(face = "bold"), 
          legend.position = "right",
          legend.direction = "vertical")
  
  outdir <- paste0(config$output$supplementary, "/trends")
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  ggsave(filename=paste0(outdir,"/", p_title, ".jpg"), plot=p, width=150, height=150, units='mm', dpi=300)
  
}


plot_effort_sharing <- function(emi_cumulative, effort_sharing_range, effort_sharing_C, country){
  
  
  emi_cumulative_median <- emi_cumulative %>% 
    group_by(Region, Scenario) %>%
    summarize(
      median = median(cumulative, na.rm = TRUE),
      lower = quantile(cumulative, 0.25, na.rm = TRUE),
      upper = quantile(cumulative, 0.75, na.rm = TRUE)
    ) %>% ungroup() 
  
  #filter region
  emi_cumulative_median_R <- emi_cumulative_median %>% filter(Region %in% country)
  effort_sharing_range_R <- effort_sharing_range %>% filter(Region %in% country)
  effort_sharing_C_R <- effort_sharing_C %>% filter(Region %in% country)
  
  # find max value across all y-like measures
  ymin <- min(
    emi_cumulative_median_R$lower, 
    effort_sharing_range_R$min_CB)
  
  ymax <- max(
    emi_cumulative_median_R$upper,
    effort_sharing_range_R$max_CB)
  
  if (ymin > 0) ymin <- 0
  ymax <- ymax * 1.05
  #facet_wrap(~)
  p1 <- ggplot(emi_cumulative_median_R, aes(x = Scenario, y = median, fill = Scenario)) +
    geom_col(position = position_dodge(width = 0.9), width = 0.7) +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position = position_dodge(width = 0.9))+
    #theme_void() +
    scale_fill_manual(values = scen_color, name = "") +
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0))+
    theme_bw() +
    theme(plot.subtitle = element_text(hjust = 0.5),
          panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1), 
          strip.background = element_blank(),
          legend.position='none')+
    labs(x="National Scenarios", y = "GtCO2", subtitle = "Cumulative Emissions")
  
  p2 <- ggplot() +
    
    
    geom_linerange(data = effort_sharing_C_R,
                   aes(x = a, ymin = min_CB, ymax = max_CB, color = Category),
                   linewidth = 3, position = position_dodge(width = 0.6)) +
    
    geom_errorbar(data = effort_sharing_range_R,
                  aes(x = a, ymin = min_CB, ymax = max_CB),
                  width = 0.5, size = 0.8, color = "black") +
    
    scale_y_continuous(limits = c(ymin, ymax), expand = c(0, 0)) +
    scale_color_manual(values = c( C1 = "#A6DDB7",  
                                   C2 = "#FDBE85",  
                                   C3 = "#B3CDE3"  
                                   #C4 = "#FBB4C4"   
                                   )) +
    labs(x = 'Effort Sharing Schemes', y = NULL, 
         subtitle = "Carbon Budget") +
    
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
  
  
  p <- (p1 + p2)+
    plot_layout(ncol = 2, widths = c(0.5, 0.7)) +
    theme(axis.title.y = element_text(size = 12))+
    plot_annotation(
      title = country
    )
  
  outdir <- paste0(config$output$supplementary, "/effort_sharing")
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  ggsave(filename=paste0(outdir,"/", country, ".jpg"), plot=p, width=150, height=100, units='mm', dpi=300)
  
}


plot_income_group <- function(df, group){
  p <- ggplot(df %>% filter(`Income group` %in% group)) +
    geom_linerange(
      aes(x = a, ymin = min_CB_idx, ymax = max_CB_idx, color = Category),
      linewidth = 3
    ) +
    geom_errorbar(
      aes(x = a, ymin = emi_lower_idx, ymax = emi_upper_idx),
      width = 0.2,
      color = "black",
      linewidth = 0.8
    ) +
    geom_point(
      aes(x = a, y = emi_median_idx),
      size = 1,
      color = "black"
    ) +
    scale_color_manual(
      values = c(
        C1 = "#A6DDB7",
        C2 = "#FDBE85",
        C3 = "#B3CDE3"
      ),
      name = "Category"
    ) +
    facet_wrap(~Region, nrow=1, scales= 'free')+
    labs(
      x = NULL,
      y=NULL,
      subtitle = group
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5)+
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      axis.text.x = element_text(angle = 60, hjust = 1)
    )
  
  return(p)
}
