# Figures 2 and 3: empirical loadings against the averaged-transformer embedding loadings, one panel per factor,
# as in the original Correlations_new_colors.R (facets of the panel's factor in red, all others light blue;
# least-squares line with 95% band).
#
# Run from the Rebuild-EPM folder after 03_fit.R: Rscript 05_figures.R
# Output: Figure 2.png (NEO, macro encoding), Figure 3.png (HEXACO, atomic encoding)
suppressMessages({library(ggplot2); library(gridExtra)})

root <- getwd()
key <- function(s) gsub('[^a-z]', '', tolower(s))

figures <- list(
  NEO = list(file = 'Figure 2.png', encoding = 'sent', bench = 'NEO/benchmark_Johnson2014_Table4_left_panel.csv',
             factors = c(O = 'Openness', C = 'Conscientiousness', E = 'Extraversion', A = 'Agreeableness',
                         N = 'Neuroticism'),
             legend = c(O = 'Openness', C = 'Conscientiousness', E = 'Extraversion', A = 'Agreeableness',
                        N = 'Neuroticism')),
  HEXACO = list(file = 'Figure 3.png', encoding = 'item', bench = 'HEXACO/benchmark_LeeAshton2004_Table4.csv',
                factors = c(H = 'Honesty-Humility', E = 'Emotionality', X = 'Extraversion', A = 'Agreeableness',
                            C = 'Conscientiousness', O = 'Openness'),
                legend = c(H = 'Honesty', E = 'Emotionality', X = 'Extraversion', A = 'Agreeableness',
                           C = 'Conscientiousness', O = 'Openness'))
)

for (study in names(figures)) {
  f <- figures[[study]]
  k <- names(f$factors)
  L <- read.csv(file.path(root, study, 'results', paste0('Results_target_', f$encoding, '_labeled.csv')), row.names = 1)
  L <- L[, paste0(k, '_avg_trans')]  # the printed solution aligns every factor
  b <- read.csv(file.path(root, f$bench))
  rownames(b) <- if (study == 'NEO') key(b$facet) else b$code
  b <- b[if (study == 'NEO') key(rownames(L)) else rownames(L), k]
  per_factor <- nrow(L) / length(k)  # facets are ordered factor by factor
  own <- rep(k, each = per_factor)

  panels <- lapply(k, function(x) {
    d <- data.frame(embedding = L[[paste0(x, '_avg_trans')]], empirical = b[[x]],
                    target = ifelse(own == x, f$legend[[x]], 'Off-target'))
    ggplot(d, aes(x = embedding, y = empirical)) +
      geom_smooth(method = lm, formula = y ~ x, color = 'darkgrey') +
      geom_point(size = 5, alpha = .7, aes(color = target)) +
      labs(title = paste(study, f$factors[[x]], 'loadings'), y = 'Empirical loading', x = 'Pseudo loading',
           color = 'Trait') +
      scale_color_manual(values = setNames(c('#FF0000', '#87CEFA'), c(f$legend[[x]], 'Off-target'))) +
      theme_minimal()
  })
  png(file.path(root, f$file), width = 10, height = 11, units = 'in', res = 300)
  grid.arrange(grobs = panels, ncol = 2, nrow = 3, top = paste(study, 'Trait Loadings'))
  invisible(dev.off())
}
cat('wrote Figure 2.png and Figure 3.png\n')
