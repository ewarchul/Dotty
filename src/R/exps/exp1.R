library(tidyverse)
library(zeallot)
source(here::here("src/R/util.R"))
source(here::here("src/R/eval-functions.R"))



lambda <- 1000
fn <- sphere_fn
repetitions <- 1000

trunc_m_seq_acc <- list()
p_seq_med_acc <- list()
for (r in seq_len(repetitions)) {
  # P ~ N(0, 1)
  pop <- rnorm(lambda, sd = 1)
  c(fitness_pop, sorted_pop) %<-% sort_population(pop, fn)

  # avg(P)
  midpoint <- mean(sorted_pop)
  fitness_midpoint <- fn(midpoint)

  # m_lambda, m_lambda -1, ..., m_1
  trunc_m_seq <- mk_trunc_midpoint_seq(sorted_pop, lambda)
  trunc_m_seq_flatten <- trunc_m_seq |> purrr::flatten_dbl()
  trunc_m_seq_fitness <- purrr::map_dbl(trunc_m_seq, fn)

  # P_lambda, P_lambda - 1, ..., P_1
  p_seq <- mk_P_seq(sorted_pop, lambda)
  p_seq_med <- purrr::map(p_seq, stats::median)
  p_seq_med_flatten <- p_seq_med |> purrr::flatten_dbl()

  trunc_m_seq_acc[[r]] <- trunc_m_seq_flatten
  p_seq_med_acc[[r]] <- p_seq_med_flatten
}

trunc_m_seq_avg <- avg_nested_list(trunc_m_seq_acc)
p_seq_med_avg <- avg_nested_list(p_seq_med_acc)

median_fitness_plot <- \(fitness_seq, ...) {
  lambda <- length(fitness_seq)
  df <- tibble::tibble(
    k = seq_along(fitness_seq) - 1,
    fn = fitness_seq
  )
  df |> ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = k, y = fn)) +
    ggplot2::theme_classic() +
    ggplot2::ylab("Fitness value of truncated midpoint") +
    ggplot2::xlab("Number of points removed from population") +
    ggplot2::ggtitle(...) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 15, face = "bold"),
      axis.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.title = ggplot2::element_text(size = 15, face = "bold"),
    )
}

median_plot <- \(median_list, ...) {
  df <- tibble::tibble(
    k = seq_along(median_list),
    median = median_list
  )
  df |> ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = k, y = median)) +
    ggplot2::geom_line(aes(x = k, y = median), col = "orange", alpha = 0.3, linewidth = 2) +
    ggplot2::theme_classic() +
    ggplot2::ylab("X^t") +
    ggplot2::xlab("k") +
    ggplot2::ggtitle(...) +
    ggplot2::theme(
      title = ggplot2::element_text(size = 15, face = "bold"),
      axis.title = ggplot2::element_text(size = 15, face = "bold"),
      axis.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.title = ggplot2::element_text(size = 15, face = "bold"),
    )
}
