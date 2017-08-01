
#' Tidy Naive Bayes Classifier
#'
#' @param x A data frame.
#' @param y A vector.
#' @param formula An object of class 'formula' of the form 'class ~ predictors'.
#' @param data A data frame.
#' @seealso [predict.tidy_naive_bayes()]
#' @name tidy_naive_bayes
#' @examples
#' data(iris)
#' model <- tidy_naive_bayes(Species ~ ., iris)
NULL

tidy_naive_bayes <- function(x, ...) {
  UseMethod("tidy_naive_bayes")
}

#' @rdname tidy_naive_bayes
#' @export
tidy_naive_bayes.default <- function(x, y) {
  class_column <- data_frame(class = y, id = row_number(class))

  data <- as_data_frame(x) %>%
    mutate(id = row_number()) %>%
    gather(key = feature, value = value, -id) %>%
    inner_join(class_column, by = "id")

  stats <- data %>%
    group_by(class, feature) %>%
    summarise(mu = mean(value),
              sigma = var(value)) %>%
      ungroup()

  priors <- data %>%
    group_by(class) %>%
    summarise(prior = n() / nrow(data))

  structure(
    list(stats = stats, priors = priors),
    class = "tidy_naive_bayes"
  )
}




#' @rdname tidy_naive_bayes
#' @export
tidy_naive_bayes.formula <- function(formula, data) {
  data <- model.frame(formula, data = data)
  class_var <- all.vars(formula[[2]])

  x <- dplyr::select(data, -.data[[class_var]])
  y <- dplyr::pull(data, .data[[class_var]])

  tidy_naive_bayes.default(x, y)
}


#' Predict method for Naive Bayes classifier models
#'
#' @export
predict.tidy_naive_bayes <- function(model, newdata = NULL, type = c("class", "prob")) {

  type <- match.arg(type)

  newdata <- newdata %>%
    as_data_frame() %>%
    select(unique(model$stats$feature)) %>%
    mutate(id = row_number()) %>%
    gather(key = feature, value = value, -id)

  likelihoods <-
    newdata %>%
    inner_join(model$stats, by = "feature") %>%
    mutate(p = 1 / (sqrt(2 * pi * sigma)) * exp(-(value - mu)^2 / (2 * sigma)))

  likelihood <-
    likelihoods %>%
    group_by(id, class) %>%
    summarise(likelihood = prod(p)) %>%
    ungroup()

  posteriors <-
    likelihood %>%
    inner_join(model$priors, by = "class") %>%
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

#' @rdname tidy_naive_bayes
#' @export
tidy_naive_bayes.tbl_df <- function(.data, class_var) {
  class_var <- enquo(class_var)
  x <- dplyr::select(.data, -!!class_var)
  y <- dplyr::pull(.data, !!class_var)

  tidy_naive_bayes.default(x, y)
}
