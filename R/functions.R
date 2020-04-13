plot_data <- function(data, variables) {
  variables <- enquo(variables)
  data %>%
    mutate(year = as.numeric(year), age = as.factor(age)) %>%
    drop_na(year, sex, age) %>%
    select(year, sex, age, !!variables) %>%
    pivot_longer(-c(year, sex, age)) %>%
    drop_na(value) %>%
    group_by(name) %>%
    mutate(value = value / max(.$value)) %>%
    group_by(sex, year, age, name) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    ggplot(aes(year, value, col = age, fill = age)) +
    scale_color_viridis_d("Age", aesthetics = c("color", "fill")) +
    scale_y_continuous(
      "Mean scaled value",
      breaks = pretty_breaks(),
    ) +
    scale_x_continuous(
      "Year",
      breaks = pretty_breaks(),
      labels = function(x) paste0("'", str_sub(x, 3, 4))
    ) +
    geom_line(size = .8) +
    facet_grid(name~sex, scales = "free") +
    theme(legend.position = "bottom")
}

make_fits_1 <- function(
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
  # Regress all yvars on all xvars separately for age, sex, and year
  fits <- data_really_long %>%
    # Fit model to average sex by duplicating dataset
    bind_rows(., mutate(., sex = "Average")) %>%
    group_by(sex, age, year, outcome, predictor) %>%
    nest() %>%
    mutate(fit = map(data, model)) %>%
    select(-data)
  fits
}

plot_fits_1 <- function(fits, x) {
  # Evaluate null hypothesis for each model
  plot_data <- fits %>%
    filter(predictor == x) %>%
    mutate(hypothesis = "x = 0") %>%
    mutate(out = map2(fit, hypothesis, ~try(tidy(confint(glht(.x, .y))), silent = FALSE))) %>%
    unnest(out)
  plot_data %>%
    ggplot(aes(year, estimate, col = age, fill = age)) +
    scale_color_brewer(palette = "Set1", aesthetics = c("color", "fill")) +
    scale_x_continuous(
      breaks = pretty_breaks(),
      labels = function(x) paste0("'", str_sub(x, 3, 4))
    ) +
    geom_hline(yintercept = 0) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      col = NA, alpha = .2
    ) +
    geom_line() +
    # Fill only significant estimates
    geom_point(shape = 21, fill = "white") +
    geom_point(
      data = filter(plot_data, sign(conf.low) == sign(conf.high)),
      shape = 21
    ) +
    facet_grid(sex~outcome) +
    labs(y = glue("Estimated relationship with {x}")) +
    theme(legend.position = "bottom")
}

make_fits_2 <- function(
  data,
  yvars,
  xvars,
  model = ~lm(y ~ x*year, data = .)
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
    # Fit model to average sex by duplicating dataset
    bind_rows(., mutate(., sex = "Average")) %>%
    group_by(sex, age, outcome, predictor) %>%
    nest() %>%
    mutate(fit = map(data, model)) %>%
    select(-data)
  fits
}

plot_fits_2 <- function(fits, x) {
  # Evaluate null hypothesis at each year
  fits <- fits %>%
    filter(predictor == x)
  yrange <- range(fits$fit[[1]]$model$year)
  plot_data <- fits %>%
    expand_grid(year = yrange[1]:yrange[2]) %>%
    mutate(hypothesis = glue("x + x:year*{year} = 0")) %>%
    mutate(out = map2(fit, hypothesis, ~try(tidy(confint(glht(.x, .y)))))) %>%
    unnest(out)
  plot_data %>%
    ggplot(aes(year, estimate, col = age, fill = age)) +
    scale_color_brewer(palette = "Set1", aesthetics = c("fill", "color")) +
    scale_x_continuous(
      breaks = pretty_breaks(),
      labels = function(x) paste0("'", str_sub(x, 3, 4))
    ) +
    geom_hline(yintercept = 0) +
    geom_ribbon(
      aes(ymin = conf.low, ymax = conf.high),
      col = NA, alpha = .2
    ) +
    geom_line() +
    facet_grid(sex~outcome) +
    labs(y = glue("Estimated relationship with {x}")) +
    theme(legend.position = "bottom")
}

tabulate_fits_2 <- function(fits, x) {
  fits %>%
    filter(predictor == x) %>%
    mutate(hypothesis = glue("x:year*10 = 0")) %>%
    mutate(out = map2(fit, hypothesis, ~try(tidy(summary(glht(.x, .y)))))) %>%
    unnest(out) %>%
    mutate(
      res = as.character(glue(
        "b = {format(round(estimate, 2), nsmall = 2)}{ifelse(p.value<0.05, '*', '')} ({format(round(std.error, 2), nsmall = 2)})"
      ))
    ) %>%
    select(predictor, outcome, sex, age, res) %>%
    pivot_wider(names_from = sex, values_from = res) %>%
    arrange(predictor, outcome, age) %>%
    kable(
      digits = 2,
      caption = glue("Estimated {x} by year interactions (in decades)")
      )
}
