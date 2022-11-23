mk_trunc_midpoint <- function(sort_pop, len, .avg = stats::median) {
  if (len > length(sort_pop)) {
    warning("len exceeds population size; using whole population")
    .avg(sort_pop[seq_along(sort_pop)])
  } else {
    .avg(sort_pop[1:len])
  }
}

mk_trunc_midpoint_seq <- function(sort_pop, len) {
  lambda <- length(sort_pop)
  if (len > lambda) {
    warning("len exceeds population size; using whole population (lambda - k)")
    len <- lambda
  }
  0:(len) |>
    purrr::map(\(i) {
      mk_trunc_midpoint(sort_pop, lambda - i)
    })
}

mk_P_seq <- function(sort_pop, len) {
  lambda <- length(sort_pop)
  if (len > lambda) {
    warning("len exceeds population size; using whole population (lambda - k)")
    len <- lambda
  }
  1:(len - 1) |> purrr::map(\(i) {
    sort_pop[1:i]
  })
}

mk_pv_seq <- function(trunc_seq, mu) {
  lambda <- length(trunc_seq)
  1:lambda |> purrr::map_dbl(\(i) {
    wilcox.test(trunc_seq[1:i], mu = mu)$p.value
  })
}

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

sort_population <- \(p, fn) {
  fitness <- purrr::map_dbl(p, fn)
  return(list(
    fitness = fitness,
    population = p[order(fitness, decreasing = FALSE)]
  ))
}

avg_nested_list <- \(nested_list) {
  colMeans(do.call(rbind, nested_list))
}
