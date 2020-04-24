#' Plot X and Y variables of a single study
#'
#' @param data mtf/us/yrbs
#'
#' @return ggplot2
plot_data <- function(data, variables = c(starts_with("x_"), starts_with("y_"))) {
  variables <- enquo(variables)
  data_sex_age <- data %>%
    pivot_longer(!!variables, names_to = "variable", values_to = "value") %>%
    drop_na(value) %>%
    mutate(variable = str_remove(variable, "[x|y]_")) %>%
    group_by(year, variable, sex, age) %>%
    summarise(value = mean(value, na.rm = TRUE), n = n()) %>%
    ungroup() %>%
    mutate(group = glue("{age} {sex}")) %>%
    mutate(annotation = glue("{sex} {age}y"))
  data_sex <- data_sex_age %>%
    group_by(year, variable, sex) %>%
    summarise(value = mean(value, na.rm = TRUE), n = n()) %>%
    mutate(age = "Average") %>%
    ungroup() %>%
    mutate(annotation = glue("{sex} mean"))
  data_sex %>%
    ggplot(aes(year, value, color = sex, text = annotation)) +
    scale_y_continuous("Mean value") +
    scale_x_continuous(breaks = pretty_breaks()) +
    stat_summary(fun = mean, geom = "line", size = .8) +
    geom_line(data = data_sex_age, aes(group = group), alpha = .15, size = .4) +
    facet_wrap("variable", scales = "free_y", nrow = 3) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

make_fits <- function(
  # Pass grouped df to estimate model for groups
  data,
  yvars,
  xvars,
  model = ~lm(y ~ x, data = .)
) {
  yvars <- enquo(yvars)
  xvars <- enquo(xvars)
  data_really_long <- data %>%
    select(year, sex, age, !!yvars, !!xvars) %>%
    mutate_at(vars(!!xvars, !!yvars), ~scale(.)[,1]) %>%
    pivot_longer(!!yvars, names_to = "outcome", values_to = "y") %>%
    pivot_longer(!!xvars, names_to = "predictor", values_to = "x") %>%
    drop_na(y, x)
  # Regress all yvars on all xvars
  fits <- data_really_long %>%
    # add = TRUE allows passing data that is already grouped on something else
    group_by(outcome, predictor, add = TRUE) %>%
    nest() %>%
    mutate(fit = map(data, model)) %>%
    select(-data)
  fits
}

plot_fits <- function(fits, x) {
  # Evaluate null hypothesis for each model
  plot_data <- fits %>%
    filter(predictor == x) %>%
    mutate(out = map(fit, ~tidy(., conf.int = TRUE))) %>%
    unnest(out) %>%
    filter(term == "x")
  plot_data %>%
    ggplot(aes(year, estimate, fill = age, color = age)) +
    scale_color_viridis_d("") +
    scale_fill_viridis_d("") +
    scale_x_continuous(
      "Year",
      breaks = pretty_breaks(4),
      labels = function(x) paste0("'", str_sub(x, 3, 4))
    ) +
    geom_hline(yintercept = 0) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      alpha = .2, col = NA
    ) +
    geom_line(aes(text = paste("p =", round(p.value, 3)), group = age), show.legend = FALSE) +
    # Define text to show in interactive figures
    geom_point(
      aes(text = paste("p =", p.value))
    ) +
    facet_grid(sex~outcome) +
    labs(
      title = glue("Estimated relationship with {x}"),
      y = "Estimate",
      caption = "Positive estimates indicate greater wellbeing"
    ) +
    theme(legend.position = "bottom")
}
