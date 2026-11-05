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
d <- usdt_data_tasks(
  awareness = vadillo_awareness,
  influence = vadillo_cuing,
  subject_col      = "subj",
  condition_col    = "condition",
  condition_levels = c(signal = "old", noise = "new"),
  response_col     = list(awareness = "judged.old", influence = "rt"),
  response_levels  = list(awareness = c(signal = 1, noise = 0),
                          influence = c(signal = "faster", noise = "slower")),
  dichotomize      = list(awareness = FALSE, influence = TRUE)
)
pilot <- hsdt(d)

## -----------------------------------------------------------------------------
pop <- usdt_population(from = pilot, intercept = 0.1)
pop

## -----------------------------------------------------------------------------
plot(pop, trials = c(awareness = 64, influence = 384), dichotomize = "influence")

## -----------------------------------------------------------------------------
usdt_power(pop, subjects = 104, trials = c(awareness = 64, influence = 384),
           hypothesis = c("diff < 0", "rho = 0", "intercept > 0"),
           dichotomize = "influence", combine = "all")

## -----------------------------------------------------------------------------
curves <- usdt_power(pop, subjects = seq(50, 800, 25),
                     trials = list(awareness = c(64, 256), influence = 384),
                     hypothesis = c("rho = 0", "intercept > 0"),
                     dichotomize = "influence")
plot(curves)

## -----------------------------------------------------------------------------
plan <- usdt_plan(pop, hypothesis = "intercept > 0", power = 0.8,
                  subjects = c(50, 1000),
                  trials = list(awareness = c(32, 512), influence = c(192, 768)),
                  cost = c(subject = 600, awareness = 5, influence = 2),
                  dichotomize = "influence")
plan

## -----------------------------------------------------------------------------
plan <- usdt_plan(pop, hypothesis = "intercept > 0", power = 0.8,
                  subjects = c(50, 2000),
                  trials = list(awareness = c(32, 128), influence = c(192, 768)),
                  cost = c(subject = 600, awareness = 5, influence = 2),
                  dichotomize = "influence")
plan
plot(plan)

## -----------------------------------------------------------------------------
usdt_plan(pop, hypothesis = "intercept > 0", budget = 30 * 3600,
          subjects = c(50, 2000), trials = c(awareness = 128, influence = 384),
          cost = c(subject = 600, awareness = 5, influence = 2),
          dichotomize = "influence")
usdt_plan(pop, precision = c(intercept = 0.2), subjects = c(50, 2000),
          trials = c(awareness = 128, influence = 384),
          dichotomize = "influence")

## -----------------------------------------------------------------------------
scenarios <- update(pop, intercept = c(0.1, 0.15), cor_dprime = c(0.3, 0.49))
usdt_plan(scenarios, hypothesis = "intercept > 0", power = 0.8,
          subjects = c(50, 3000), trials = c(awareness = 128, influence = 384),
          dichotomize = "influence")

## -----------------------------------------------------------------------------
mdes <- usdt_mdes(pop, subjects = plan$design$subjects,
                  trials = c(awareness = plan$design$trials_awareness,
                             influence = plan$design$trials_influence),
                  hypothesis = c("diff < 0", "rho = 0", "intercept > 0"),
                  dichotomize = "influence")
mdes
plot(mdes)

## -----------------------------------------------------------------------------
# check <- usdt_power(plan, method = "simulation", nsim = 1000, ncores = 2,
#                     seed = 2026)
# check
# summary(check)

## -----------------------------------------------------------------------------
validation <- utils::read.csv(system.file("extdata", "power-validation.csv",
                                          package = "uSDT"))
validation$design <- sprintf("%s subjects, %s + %s trials",
                             validation$subjects, validation$trials_awareness,
                             validation$trials_influence)
validation$design <- factor(validation$design,
                            levels = rev(unique(validation$design)))
validation$population <- factor(validation$population,
                                levels = c("A", "Vadillo"))
validation$hypothesis <- factor(
  validation$hypothesis, levels = c("diff = 0", "rho = 0", "intercept = 0"),
  labels = c("H1: difference = 0", "H2: correlation = 0",
             "H3: intercept = 0"))
long <- rbind(
  data.frame(validation[c("population", "design", "hypothesis")],
             engine = "Closed form", power = validation$closed),
  data.frame(validation[c("population", "design", "hypothesis")],
             engine = "Approximation of usdt_power()",
             power = validation$approximate),
  data.frame(validation[c("population", "design", "hypothesis")],
             engine = "Simulation with hsdt()",
             power = validation$simulation))
long$engine <- factor(long$engine, levels = c("Simulation with hsdt()",
                                              "Approximation of usdt_power()",
                                              "Closed form"))

ggplot2::ggplot(long, ggplot2::aes(x = power, y = design)) +
  ggplot2::geom_errorbar(
    data = validation,
    ggplot2::aes(xmin = simulation_low, xmax = simulation_high, y = design),
    inherit.aes = FALSE, orientation = "y", width = 0.45, colour = "#292D30",
    linewidth = 0.5) +
  ggplot2::geom_point(ggplot2::aes(shape = engine, colour = engine,
                                   fill = engine), size = 2.6, stroke = 0.9) +
  ggplot2::scale_shape_manual(values = c(21, 24, 23), name = NULL) +
  ggplot2::scale_colour_manual(values = c("#292D30", "#116B60", "#B6543A"),
                               name = NULL) +
  ggplot2::scale_fill_manual(values = c("#292D30", "#3FA88E", "white"),
                             name = NULL) +
  ggplot2::scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1),
                              labels = c("0", "0.5", "1")) +
  ggplot2::facet_grid(population ~ hypothesis, scales = "free_y",
                      space = "free_y") +
  ggplot2::labs(
    x = "Power", y = NULL,
    caption = paste0(
      "1000 simulated studies per design. The bars are 95% Wilson intervals ",
      "of the simulated power. Studies whose fit ends on the boundary count ",
      "as failures to reject the correlation and the intercept.")) +
  ggplot2::theme_classic(base_size = 12) +
  ggplot2::theme(
    legend.position = "top",
    panel.spacing.x = grid::unit(14, "pt"),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(colour = "#252A2D"),
    panel.grid.major.x = ggplot2::element_line(colour = "#E4E5E1",
                                               linewidth = 0.4),
    axis.text = ggplot2::element_text(colour = "#3E4347"),
    plot.caption = ggplot2::element_text(colour = "#687076", size = 9.5,
                                         hjust = 0))

## -----------------------------------------------------------------------------
options(old_options)
invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", old_ctype)))

