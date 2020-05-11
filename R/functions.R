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
    scale_y_continuous("Mean value", breaks = pretty_breaks()) +
    scale_x_continuous("Year", breaks = pretty_breaks()) +
    stat_summary(fun = mean, geom = "line", size = .8) +
    geom_line(data = data_sex_age, aes(group = group), alpha = .15, size = .4) +
    facet_wrap("variable", scales = "free_y", nrow = 3) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
}
