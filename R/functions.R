#' Plot X and Y variables of a single study
#'
#' @param data `dat` subsetted to desired study
#' @param variables which variables to plot
#'
#' @return ggplot2
plot_data <- function(data, variables) {
  variables <- enquo(variables)
  data %>%
    mutate(year = as.numeric(year), age = as.factor(age)) %>%
    drop_na(year, sex, age) %>%
    select(year, sex, age, !!variables) %>%
    pivot_longer(-c(year, sex, age)) %>%
    drop_na(value) %>%
    mutate(name = str_remove(name, "[x|y]_")) %>%
    group_by(name) %>%
    mutate(value = value / max(.$value)) %>%
    group_by(sex, year, age, name) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ggplot(aes(year, value, col = age, fill = age)) +
    scale_color_viridis_d("") +
    scale_fill_viridis_d("") +
    scale_y_continuous(
      "Mean scaled value",
      breaks = pretty_breaks(4),
    ) +
    scale_x_continuous(
      "Year",
      breaks = pretty_breaks(4),
      labels = function(x) paste0("'", str_sub(x, 3, 4))
    ) +
    geom_line(size = .8) +
    facet_grid(name~sex, scales = "free") +
    labs(caption = "Note: All variables are scaled to the 0-1 interval.\nPositive values indicate greater wellbeing.") +
    theme(legend.position = "bottom")
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
