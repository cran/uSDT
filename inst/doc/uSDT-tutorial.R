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
  # l10n_info() rather than the return value, and undo the attempts when none
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
  # One data frame per task
  direct   = vadillo_awareness,
  indirect = vadillo_cuing,

  # Shared subject column
  subject_col = "subj",

  # Condition mapping (shared column name and labels here)
  condition_col    = "condition",
  condition_levels = c(signal = "old", noise = "new"),

  # Response columns differ: judgements vs continuous response times
  response_col = list(direct = "judged.old", indirect = "rt"),

  # How responses map to signal vs noise
  response_levels = list(
    direct   = c(signal = 1, noise = 0),
    indirect = c(signal = "faster", noise = "slower")
  ),

  # Dichotomize continuous response times using a median split
  dichotomize = list(direct = FALSE, indirect = TRUE),
  
  # Contrast coding scheme across conditions (deviation by-default)
  coding = "deviation"
)

## -----------------------------------------------------------------------------
data_usdt

## -----------------------------------------------------------------------------
# Fit hierarchical SDT model
fit_uSDT <- hsdt(data_usdt)

# Check model results
summary(fit_uSDT)

## -----------------------------------------------------------------------------
# Small helpers so the prose below quotes the fit rather than repeating it.
h   <- function(term) fit_uSDT$tests[match(term, fit_uSDT$tests$term), ]
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
ci  <- function(term, digits = 2) {
  r <- h(term)
  sprintf("[%s, %s]", fmt(r$conf.low, digits), fmt(r$conf.high, digits))
}

## -----------------------------------------------------------------------------
usdt_reliability(fit_uSDT)

## -----------------------------------------------------------------------------
print(head(usdt_reliability(fit_uSDT)$subjects), digits = 3)

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
# fit_boot <- usdt_boot(fit_uSDT, nsim = 1000, ncores = 4, seed = 2026)
# summary(fit_boot)
# fit_boot$tests

## -----------------------------------------------------------------------------
options(old_options)
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype)))

