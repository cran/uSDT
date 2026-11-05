## -----------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse   = FALSE,
  comment    = "",
  fig.width  = 8,
  fig.height = 5.2,
  fig.align  = "center",
  out.width  = "100%",
  dpi        = 150
)

# The printed output rules the tables with box characters in a UTF-8 locale
# and falls back to ASCII when there is none, so the vignette asks for UTF-8
# to read the same wherever it is built. It only asks when the session is not
# already there, and the last chunk puts the width and the locale back as they
# were.
old_options <- options(width = 80)
old_ctype   <- Sys.getlocale("LC_CTYPE")

if (!isTRUE(l10n_info()[["UTF-8"]])) {
  for (loc in c("en_US.UTF-8", "C.UTF-8", "English_United States.utf8")) {
    suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
    if (isTRUE(l10n_info()[["UTF-8"]])) break
  }
  # Windows accepts locale names it does not actually have, so trust
  # l10n_info() to verify the locale, and undo the attempts when none
  # of the names produced a UTF-8 session.
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype)))
  }
}

## -----------------------------------------------------------------------------
library(uSDT)

## -----------------------------------------------------------------------------
head(vadillo_awareness)

## -----------------------------------------------------------------------------
head(vadillo_cuing)

## -----------------------------------------------------------------------------
data_usdt <- usdt_data_tasks(
  awareness = vadillo_awareness,
  influence = vadillo_cuing,
  subject_col      = "subj",
  condition_col    = "condition",
  condition_levels = c(signal = "old", noise = "new"),
  response_col     = list(awareness = "judged.old", influence = "rt"),
  response_levels  = list(
    awareness = c(signal = 1, noise = 0),
    influence = c(signal = "faster", noise = "slower")
  ),
  dichotomize = list(awareness = FALSE, influence = TRUE)
)

## -----------------------------------------------------------------------------
data_usdt

## -----------------------------------------------------------------------------
fit_uSDT <- hsdt(data_usdt)
summary(fit_uSDT)

## -----------------------------------------------------------------------------
h <- function(term) fit_uSDT$tests[match(term, fit_uSDT$tests$term), ]
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
ci <- function(term) {
  r <- h(term)
  sprintf("[%s, %s]", fmt(r$conf.low), fmt(r$conf.high))
}

## -----------------------------------------------------------------------------
usdt_equivalence(fit_uSDT, c("diff in [-0.2, 0.2]", "intercept < 0.2",
                            "rho in [-0.3, 0.3]"))

## -----------------------------------------------------------------------------
reliability <- usdt_reliability(fit_uSDT)
reliability

## -----------------------------------------------------------------------------
head(reliability$subjects)

## -----------------------------------------------------------------------------
plot(fit_uSDT)

## -----------------------------------------------------------------------------
plot(fit_uSDT, type = "shrinkage")

## -----------------------------------------------------------------------------
plot(fit_uSDT, type = "caterpillar")

## -----------------------------------------------------------------------------
plot(fit_uSDT, type = "roc")

## -----------------------------------------------------------------------------
plot(fit_uSDT, type = "roc", subject_id = 2040)

## -----------------------------------------------------------------------------
# fit_boot <- usdt_boot(fit_uSDT, nsim = 1000, ncores = 2, seed = 2026)
# summary(fit_boot)
# fit_boot$tests

## -----------------------------------------------------------------------------
options(old_options)
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype)))

