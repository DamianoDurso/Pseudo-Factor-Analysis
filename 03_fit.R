# Step 4: target-rotated ML factor analysis of every similarity matrix, and DAAL labelling.
#
# For each study and encoding, 7 solutions are fitted: the 6 encoders and avg_trans (the cell-wise
# mean of the 5 transformer matrices, excluding USE-DAN). Settings as published: esem_efa with
# targetQ rotation, ML extraction, default rotation iterations (non-convergence is recorded, not acted on).
#
# Outputs per study, in <study>/results/:
#   Results_target_{item,item_rev,sent}_labeled.csv  loadings (3 decimals), columns labelled by DAAL,
#                                                    in the layout of the original results files
#   DAAL.csv                                         the DAAL calculation for every solution
#   robustness.csv                                   robustness check (Tables 7 and 8): factors aligned at each
#                                                    perturbation step, at DAAL floors .30 and .35
#   residuals.csv                                    residual diagnostics of the printed solution (Table 1 / 4)
suppressMessages({library(psych); library(GPArotation); library(esemComp)})

# Run from the Rebuild-EPM folder: Rscript 03_fit.R
root <- getwd()

models <- c('distilroberta', 'miniLM', 'mpnet', 't5', 'psych', 'use_dan')
solutions <- c(models, 'avg_trans')

studies <- list(
  NEO = list(
    var_order = c('Imagination', 'Artistic.Interests', 'Emotionality', 'Adventurousness', 'Intellect', 'Liberalism',
                  'Self.Efficacy', 'Orderliness', 'Dutifulness', 'Achievement.Striving', 'Self.Discipline', 'Cautiousness',
                  'Friendliness', 'Gregariousness', 'Assertiveness', 'Activity.Level', 'Excitement.Seeking', 'Cheerfulness',
                  'Trust', 'Morality', 'Altruism', 'Cooperation', 'Modesty', 'Sympathy',
                  'Anxiety', 'Anger', 'Depression', 'Self.Consciousness', 'Immoderation', 'Vulnerability'),
    factors = c('O', 'C', 'E', 'A', 'N'), per_factor = 6, perturb = 'NEOAC', maxit = 10000, printed = 'sent'),
  HEXACO = list(
    var_order = c('Sinc', 'Fair', 'Gree', 'Mode', 'Fear', 'Anxi', 'Depe', 'Sent', 'Expr', 'SocB', 'Soci', 'Live',
                  'Forg', 'Gent', 'Flex', 'Pati', 'Orga', 'Dili', 'Perf', 'Prud', 'AesA', 'Inqu', 'Crea', 'Unco'),
    factors = c('H', 'E', 'X', 'A', 'C', 'O'), per_factor = 4, perturb = 'HEXACO', maxit = 50000, printed = 'item')
)

# Encoding -> (matrix file prefix, results file name, suffix the original gave unlabelled columns).
encodings <- list(
  item     = list(prefix = 'matrix_concatenated_item_',     out = 'Results_target_item_labeled.csv'),
  item_rev = list(prefix = 'matrix_concatenated_item_rev_', out = 'Results_target_item_rev_labeled.csv'),
  sent     = list(prefix = 'matrix_concatenated_',          out = 'Results_target_sent_labeled.csv')
)
column_tag <- function(study, enc) if (study == 'HEXACO' && enc == 'sent') '_sent_' else '_item_'

# Copy the upper triangle from the lower one, then put rows and columns in theoretical order.
symm <- function(m, var) {
  m[upper.tri(m)] <- t(m)[upper.tri(m)]
  rownames(m) <- colnames(m)
  m[match(var, colnames(m)), match(var, colnames(m))]
}

# DAAL: for each theoretical factor, the extracted factor with the highest mean absolute loading
# over that factor's facets. Labels are assigned in factor order; a column won by two factors gets
# both labels concatenated (e.g. "E_t5X_t5"), and a column won by none keeps its original name.
daal <- function(L, groups, mod) {
  labelled <- colnames(L)
  calc <- NULL
  for (k in names(groups)) {
    values <- colMeans(abs(L[groups[[k]], , drop = FALSE]))
    win <- which.max(values)
    labelled[win] <- if (labelled[win] == colnames(L)[win]) paste0(k, mod) else paste0(labelled[win], k, mod)
    calc <- rbind(calc, data.frame(factor = k, winning_column = colnames(L)[win], daal = unname(values[win])))
  }
  calc$aligned <- !(duplicated(calc$winning_column) | duplicated(calc$winning_column, fromLast = TRUE))
  list(labelled = labelled, calc = calc)
}

# Robustness check DAAL, as in the original robustness scripts: a factor is aligned if its dominant average
# absolute loading reaches the floor and no other factor (that reaches the floor) has the same winning column.
daal_floor <- function(L, groups, floor) {
  d <- sapply(groups, function(g) colMeans(abs(L[g, , drop = FALSE])))
  winner <- apply(d, 2, which.max)
  ok <- apply(d, 2, max) >= floor
  winner[!ok] <- NA
  dups <- !is.na(winner) & (duplicated(winner, incomparables = NA) | duplicated(winner, fromLast = TRUE, incomparables = NA))
  as.integer(ok & !dups)
}

fit <- function(M, target, ...) {
  converged <- TRUE
  f <- withCallingHandlers(
    esem_efa(M, nfactors = ncol(target), target = target, targetAlgorithm = 'targetQ', fm = 'ml', ...),
    warning = function(w) {
      if (grepl('convergence not obtained', conditionMessage(w))) converged <<- FALSE
      invokeRestart('muffleWarning')
    })
  list(L = f$loadings[], residual = f$residual, converged = converged)
}

# Residual diagnostics over the unique off-diagonal residuals (similarity minus model-implied similarity).
residual_summary <- function(R) {
  r <- R[lower.tri(R)]
  data.frame(rmsr = sqrt(mean(r^2)), median_abs = median(abs(r)), max_abs = max(abs(r)),
             pct_abs_over_05 = 100 * mean(abs(r) > .05), pct_abs_over_10 = 100 * mean(abs(r) > .10),
             n_residuals = length(r))
}

for (study in names(studies)) {
  s <- studies[[study]]
  k <- length(s$factors)
  groups <- setNames(split(s$var_order, rep(seq_len(k), each = s$per_factor)), s$factors)
  target <- sapply(seq_len(k), function(j) as.numeric(rep(seq_len(k), each = s$per_factor) == j))
  rownames(target) <- s$var_order
  out <- file.path(root, study, 'results')
  dir.create(out, showWarnings = FALSE)
  daal_all <- NULL

  for (enc in names(encodings)) {
    e <- encodings[[enc]]
    mats <- lapply(models, function(m)
      symm(as.matrix(read.csv(file.path(root, study, 'matrices', paste0(e$prefix, m, '.csv')))), s$var_order))
    mats[[7]] <- apply(simplify2array(mats)[, , 1:5], 1:2, mean)

    # Leading columns as in the original results files: an empty column (NEO) or the target matrix (HEXACO).
    if (study == 'NEO') {
      res <- matrix(NA, length(s$var_order), 1, dimnames = list(s$var_order, paste0('targ_load_', enc, '_labeled')))
    } else {
      res <- target
      colnames(res) <- paste0(s$factors, '_emp')
    }

    for (i in seq_along(solutions)) {
      r <- fit(mats[[i]], target)
      if (solutions[i] == 'avg_trans' && enc == s$printed)
        write.csv(cbind(encoding = enc, solution = 'avg_trans', residual_summary(r$residual)),
                  file.path(out, 'residuals.csv'), row.names = FALSE)
      L <- r$L
      colnames(L) <- paste0(colnames(L), column_tag(study, enc), solutions[i])
      d <- daal(L, groups, paste0('_', solutions[i]))
      colnames(L) <- d$labelled
      res <- cbind(res, round(L, 3))
      daal_all <- rbind(daal_all, data.frame(encoding = enc, solution = solutions[i], d$calc,
                                             rotation_converged = r$converged))
    }
    write.csv(res, file.path(out, e$out))
  }
  daal_all$daal <- round(daal_all$daal, 6)
  write.csv(daal_all, file.path(out, 'DAAL.csv'), row.names = FALSE)

  # Robustness check: averaged-transformer matrix at each perturbation step; rotation iteration limit as in the
  # original robustness scripts (NEO 10,000; HEXACO 50,000).
  steps <- c('baseline', paste0('perturb_', substring(s$perturb, 1, seq_len(nchar(s$perturb)))))
  rob <- NULL
  for (step in steps) {
    M <- symm(as.matrix(read.csv(file.path(root, study, 'matrices', paste0(tolower(study), '_', step, '_avg.csv')))),
              s$var_order)
    r <- fit(M, target, maxit = s$maxit)
    for (floor in c(0.30, 0.35)) {
      a <- daal_floor(r$L, groups, floor)
      rob <- rbind(rob, data.frame(step = step, floor = floor, t(setNames(a, s$factors)), total = sum(a),
                                   rotation_converged = r$converged))
    }
  }
  write.csv(rob, file.path(out, 'robustness.csv'), row.names = FALSE)
  cat(study, ': aligned', sum(daal_all$aligned), 'of', nrow(daal_all), '; non-converged rotations:',
      sum(!daal_all$rotation_converged[!duplicated(daal_all[, c('encoding', 'solution')])]), '\n')
}
