tidy_naive_bayes <- function(x, ...) {
  UseMethod("tidy_naive_bayes")
}


tidy_naive_bayes.default <- function(x, y) {
  class_column <- data_frame(class = y, id = row_number(class))

  data <- as_data_frame(x) %>%
    mutate(id = row_number()) %>%
    gather(key = feature, value = value, -id) %>%
    inner_join(class_column, by = "id")

  stats <- data %>%
    group_by(class, feature) %>%
    summarize(mu = mean(value),
              sigma = var(value)) %>%
      ungroup()

  priors <- data %>%
    group_by(class) %>%
    summarise(prior = n() / nrow(df))

  structure(
    list(stats = stats, priors = priors),
    class = "tidy_naive_bayes"
  )
}


tidy_naive_bayes.formula <- function(formula, data) {
  data <- model.frame(formula, data = data)
  class_var <- all.vars(formula[[2]])

  x <- dplyr::select(data, -.data[[class_var]])
  y <- dplyr::pull(data, .data[[class_var]])

  tidy_naive_bayes.default(x, y)
}


predict.tidy_naive_bayes <- function(object, newdata = NULL, type = c("class", "prob")) {

  type <- match.arg(type)

  newdata <- newdata %>%
    as_data_frame() %>%
    select(unique(object$stats$feature)) %>%
    mutate(id = row_number()) %>%
    gather(key = feature, value = value, -id)

  likelihoods <-
    newdata %>%
    inner_join(object$stats, by = "feature") %>%
    mutate(p = 1 / (sqrt(2 * pi * sigma)) * exp(-(value - mu)^2 / (2 * sigma)))

  likelihood <-
    likelihoods %>%
    group_by(id, class) %>%
    summarise(likelihood = prod(p)) %>%
    ungroup()

  posteriors <-
    likelihood %>%
    inner_join(object$priors, by = "class") %>%
    mutate(posterior = prior * likelihood)

  # normalize
  posteriors <-
    posteriors %>%
    group_by(id) %>%
    mutate(np = posterior / sum(posterior)) %>%
    ungroup()

  if (type == "class") {
    posteriors %>%
      arrange(desc(posterior)) %>%
      group_by(id) %>%
      summarize(prediction = first(class)) %>%
      arrange(id) %>%
      pull(prediction)
  } else {
    posteriors %>%
      select(id, class, np) %>%
      spread(key = class, value = np) %>%
      select(-id) %>%
      remove_rownames() %>%
      as.matrix()
  }
}
