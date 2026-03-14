# data.R - Sample data helpers for netmetaviz

#' Load W2I sample data
#'
#' Returns the trial-level data and CINeMA report from the W2I (Waking to
#' Insomnia) NMA as a named list. These are the data originally analysed in
#' Furukawa et al. (2024) comparing CBT-I, Combination (CBT-I + pharmacotherapy),
#' and Pharmacotherapy for insomnia remission.
#'
#' @details
#' The trial data (\code{$trials}) contains one row per treatment arm:
#' \describe{
#'   \item{id}{Study identifier}
#'   \item{t}{Treatment label ("CBT-I", "Combination", "Pharmacotherapy")}
#'   \item{n}{Number of randomised participants}
#'   \item{r}{Number achieving remission at long-term follow-up}
#'   \item{n_dropout}{Number who dropped out at long-term follow-up}
#'   \item{r_pt}{Number achieving remission at post-treatment}
#'   \item{n_dropout_pt}{Number who dropped out at post-treatment}
#'   \item{rob}{Risk of bias ("L" = low, "M" = some concerns, "H" = high)}
#'   \item{indirectness}{Indirectness score (1 = no concerns)}
#' }
#'
#' The CINeMA data (\code{$cinema}) applies to the long-term remission outcome
#' only. It contains one row per pairwise comparison with columns for each
#' CINeMA domain and the overall confidence rating.
#'
#' @return A named list with elements \code{$trials} and \code{$cinema}.
#' @export
#'
#' @examples
#' d <- load_w2i()
#' head(d$trials)
#' d$cinema
load_w2i <- function() {
  trials_path <- system.file("extdata", "w2i_trials.csv",
                              package = "netmetaviz", mustWork = TRUE)
  cinema_path <- system.file("extdata", "w2i_cinema.csv",
                              package = "netmetaviz", mustWork = TRUE)
  list(
    trials = utils::read.csv(trials_path,  stringsAsFactors = FALSE),
    cinema = utils::read.csv(cinema_path,  stringsAsFactors = FALSE,
                              check.names  = FALSE)
  )
}


#' Build a netmeta object from W2I sample data
#'
#' Convenience wrapper that calls \code{\link{load_w2i}()} then runs
#' \code{meta::pairwise()} and \code{netmeta::netmeta()}.
#'
#' Four outcomes are available:
#' \describe{
#'   \item{"remission_lt"}{Insomnia remission at long-term follow-up (OR; higher = better)}
#'   \item{"dropout_lt"}{Treatment dropout at long-term follow-up (OR; lower = better)}
#'   \item{"remission_pt"}{Insomnia remission at post-treatment (OR; higher = better)}
#'   \item{"dropout_pt"}{Treatment dropout at post-treatment (OR; lower = better)}
#' }
#'
#' CINeMA ratings are available only for the \code{"remission_lt"} outcome.
#' Pass \code{load_w2i()$cinema} (or the path returned by
#' \code{system.file("extdata", "w2i_cinema.csv", package = "netmetaviz")})
#' to functions that accept a \code{cinema} argument.
#'
#' @param outcome One of \code{"remission_lt"} (default), \code{"dropout_lt"},
#'   \code{"remission_pt"}, or \code{"dropout_pt"}.
#' @param reference Reference treatment (default \code{"Pharmacotherapy"}).
#' @return A \code{netmeta} object.
#' @export
#'
#' @importFrom meta pairwise
#' @importFrom netmeta netmeta
#'
#' @examples
#' net_lt  <- build_w2i_netmeta("remission_lt")
#' net_dlt <- build_w2i_netmeta("dropout_lt")
#' net_pt  <- build_w2i_netmeta("remission_pt")
#' net_dpt <- build_w2i_netmeta("dropout_pt")
build_w2i_netmeta <- function(outcome   = c("remission_lt", "dropout_lt",
                                             "remission_pt", "dropout_pt"),
                               reference = "Pharmacotherapy") {
  outcome <- match.arg(outcome)

  d <- load_w2i()$trials

  event_col    <- switch(outcome,
    remission_lt = "r",
    dropout_lt   = "n_dropout",
    remission_pt = "r_pt",
    dropout_pt   = "n_dropout_pt"
  )
  small_values <- switch(outcome,
    remission_lt = "undesirable",   # high OR = more remission = good
    dropout_lt   = "desirable",     # high OR = more dropout   = bad
    remission_pt = "undesirable",
    dropout_pt   = "desirable"
  )

  pw <- meta::pairwise(
    treat   = t,
    event   = d[[event_col]],
    n       = n,
    sm      = "OR",
    data    = d,
    studlab = id
  )

  netmeta::netmeta(
    TE       = pw$TE,
    seTE     = pw$seTE,
    treat1   = pw$treat1,
    treat2   = pw$treat2,
    studlab  = pw$studlab,
    data     = pw,
    ref      = reference,
    sm       = "OR",
    common   = FALSE,
    small.values = small_values
  )
}
