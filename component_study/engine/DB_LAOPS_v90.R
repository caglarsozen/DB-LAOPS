# DB-LAOPS v9.0.1 reproducibility release
# SPDX-License-Identifier: MIT
# Code repository: https://github.com/caglarsozen/DB-LAOPS
# Numerical rules match the v9.0 revision. Source this file; run db_main("full").
# DB-LAOPS v9.0 major-revision engine -- 13 November 2026
# Research code. No empirical advantage is assumed.
# The v8.1 component library is retained; accounting, timing, inference and
# evaluation are revised. See README.md and the complete methodological appendix in the manuscript.
# source this file, then db_main("full") or db_main("smoke").
# No automatic execution, environment clearing, or silent synthetic fallback.

min_lookback <- 252        # earliest decision after one trading year
expert_lookback <- 126     # expert estimation window
advice_lookback <- 126     # expert-advice learning window
corr_lookback <- 63
w_max_default <- 0.35      # cap for regular diversified long-only portfolios
crypto_total_cap <- 0.15
crypto_single_cap <- 0.10

# Transaction cost grid for sensitivity. Main model uses 10 bps.
tcost_bps_main <- 10
tcost_grid_bps <- c(0, 5, 10, 25)
run_full_tcost_rerun <- TRUE  # TRUE: rerun algorithms for each cost in tcost_grid_bps

# Online learning hyperparameters
eta_eg <- 8.0
eta_omd <- 0.80
eta_ftrl <- 4.0
eta_expert <- 12.0
eta_context <- 24.0      # sharper context-local expert selection
eta_rob <- 10.0          # learning rate for online-backbone weighting

# Trust adaptation hyperparameters
tau_max <- 0.90
tau_min <- 0.05       # guarded-advice floor; low value is intentional in v3.2
lambda_error <- 6.0
kappa_regime <- 0.30
entropy_gamma <- 2.00    # hard entropy gate; penalizes diffuse/uninformative expert probabilities
fixed_tau <- 0.50

# Turnover control for the proposed DB-LAOPS layer
no_trade_band <- 0.02    # if target turnover is below this, do not trade
daily_turnover_cap <- 0.04 # otherwise move partially toward target

# Growth/protection candidate layer retained from v5 experiments
gsp_no_trade_band <- 0.015
gsp_daily_turnover_cap <- 0.08
gsp_growth_cap <- 0.80
gsp_protection_max <- 0.85
lambda_downside_utility <- 2.0
lambda_tail_utility <- 2.0

# Drawdown-budgeted candidate selection layer for v6 experiments
# Aim: retain growth participation using rolling historical downside-risk screens.
db_lookback <- 126
db_mdd_budget <- 0.30
db_cvar_budget <- 0.026
db_strict_mdd_budget <- 0.25
db_strict_cvar_budget <- 0.021
db_soft_mdd_budget <- 0.35
db_soft_cvar_budget <- 0.034
db_eta <- 28.0
db_dd_penalty <- 0.80
db_cvar_penalty <- 1.50
db_vol_penalty <- 0.05
db_no_trade_band <- 0.015
db_daily_turnover_cap <- 0.06


softmax_stable <- function(x) {
  x <- as.numeric(x)
  x[!is.finite(x)] <- -1e6
  z <- x - max(x, na.rm = TRUE)
  ez <- exp(z)
  if (sum(ez) <= 0 || !is.finite(sum(ez))) return(rep(1 / length(x), length(x)))
  ez / sum(ez)
}




apply_turnover_control <- function(w_target, w_prev, no_trade = 0.02, cap = 0.04) {
  w_target <- as.numeric(w_target)
  w_prev <- as.numeric(w_prev)
  raw_turnover <- sum(abs(w_target - w_prev))
  if (!is.finite(raw_turnover)) raw_turnover <- Inf
  if (raw_turnover <= no_trade) {
    w_out <- w_prev
    zeta <- 0
  } else if (raw_turnover <= cap) {
    w_out <- w_target
    zeta <- 1
  } else {
    zeta <- cap / raw_turnover
    w_out <- (1 - zeta) * w_prev + zeta * w_target
  }
  w_out <- normalize_weights(w_out, cap = 1.0)
  list(w = w_out, zeta = zeta, raw_turnover = raw_turnover)
}

entropy01 <- function(p) {
  p <- as.numeric(p)
  p <- p[p > 0 & is.finite(p)]
  if (length(p) <= 1) return(0)
  -sum(p * log(p)) / log(length(p))
}

make_loss_weighted_probs <- function(expert_ret_hist, expert_names, lookback = 126, eta = 12.0) {
  if (is.null(expert_ret_hist) || nrow(expert_ret_hist) < 20) {
    out <- rep(1 / length(expert_names), length(expert_names)); names(out) <- expert_names; return(out)
  }
  recent_idx <- max(1, nrow(expert_ret_hist) - lookback + 1):nrow(expert_ret_hist)
  recent_ret <- expert_ret_hist[recent_idx, , drop = FALSE]
  recent_loss <- -log(pmax(1 + recent_ret, 1e-8))
  avg_loss <- colMeans(recent_loss, na.rm = TRUE)
  out <- softmax_stable(-eta * avg_loss)
  names(out) <- expert_names
  out
}

make_contextual_ml_probs <- function(expert_ret_hist, expert_context_hist, x_now, expert_names,
                                     lookback = 126, eta = 12.0, bandwidth_quantile = 0.50) {
  # Nonparametric context-local expert selector.
  # It learns which expert has performed well in contexts similar to the current one.
  if (is.null(expert_ret_hist) || is.null(expert_context_hist) || nrow(expert_ret_hist) < 40) {
    return(make_loss_weighted_probs(expert_ret_hist, expert_names, lookback, eta))
  }

  n_hist <- min(nrow(expert_ret_hist), nrow(expert_context_hist))
  recent_idx <- max(1, n_hist - lookback + 1):n_hist

  Xh <- as.matrix(expert_context_hist[recent_idx, , drop = FALSE])
  Rh <- as.matrix(expert_ret_hist[recent_idx, , drop = FALSE])
  x0 <- as.numeric(x_now)

  common_ok <- complete.cases(Xh) & complete.cases(Rh)
  if (sum(common_ok) < 30 || length(x0) != ncol(Xh) || any(!is.finite(x0))) {
    return(make_loss_weighted_probs(expert_ret_hist, expert_names, lookback, eta))
  }
  Xh <- Xh[common_ok, , drop = FALSE]
  Rh <- Rh[common_ok, , drop = FALSE]

  cm <- colMeans(Xh, na.rm = TRUE)
  cs <- apply(Xh, 2, sd, na.rm = TRUE)
  cs[!is.finite(cs) | cs <= 1e-8] <- 1
  Xs <- scale(Xh, center = cm, scale = cs)
  xz <- (x0 - cm) / cs
  xz[!is.finite(xz)] <- 0

  dist_vec <- sqrt(rowSums((sweep(Xs, 2, xz, "-"))^2))
  h <- as.numeric(quantile(dist_vec, probs = bandwidth_quantile, na.rm = TRUE))
  if (!is.finite(h) || h <= 1e-8) h <- median(dist_vec, na.rm = TRUE)
  if (!is.finite(h) || h <= 1e-8) h <- 1

  kw <- exp(-0.5 * (dist_vec / h)^2)
  kw[!is.finite(kw)] <- 0
  if (sum(kw) <= 0) kw <- rep(1 / length(kw), length(kw))
  kw <- kw / sum(kw)

  L <- -log(pmax(1 + Rh, 1e-8))
  avg_loss <- as.numeric(t(kw) %*% L)
  out <- softmax_stable(-eta * avg_loss)
  names(out) <- colnames(Rh)

  # Align exactly to expert_names.
  full <- rep(0, length(expert_names)); names(full) <- expert_names
  common_names <- intersect(names(out), expert_names)
  full[common_names] <- out[common_names]
  if (sum(full) <= 0 || any(!is.finite(full))) full[] <- 1 / length(full) else full <- full / sum(full)
  full
}


w_equal <- function(asset_names) rep(1 / length(asset_names), length(asset_names))

w_spy_bh <- function(asset_names) {
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  if ("SPY" %in% asset_names) w["SPY"] <- 1 else w[] <- 1 / length(asset_names)
  as.numeric(w)
}


w_qqq_bh <- function(asset_names) {
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  if ("QQQ" %in% asset_names) w["QQQ"] <- 1 else w[] <- 1 / length(asset_names)
  as.numeric(w)
}

w_spy_qqq_5050 <- function(asset_names) {
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  if (all(c("SPY", "QQQ") %in% asset_names)) {
    w["SPY"] <- 0.50; w["QQQ"] <- 0.50
  } else {
    w[] <- 1 / length(asset_names)
  }
  as.numeric(w)
}

add_weight <- function(w, sym, val) {
  if (sym %in% names(w)) w[sym] <- val
  w
}

portfolio_vol_ann <- function(R_hist, w) {
  R_hist <- as.matrix(R_hist)
  if (nrow(R_hist) < 20) return(NA_real_)
  pr <- as.numeric(R_hist %*% as.numeric(w))
  sd(pr, na.rm = TRUE) * sqrt(252)
}

vol_target_to_shy <- function(w, R_hist, asset_names, target_vol = 0.12) {
  names(w) <- asset_names
  pvol <- portfolio_vol_ann(tail(as.matrix(R_hist), min(126, nrow(as.matrix(R_hist)))), w)
  if (!is.finite(pvol) || pvol <= 1e-8) return(as.numeric(w))
  scale <- min(1, target_vol / pvol)
  if (scale >= 0.999 || !("SHY" %in% asset_names)) return(as.numeric(w))
  w <- w * scale
  w["SHY"] <- w["SHY"] + (1 - sum(w))
  normalize_weights(w, cap = 1.0)
}

w_risk_on_growth <- function(R_hist, asset_names, context_row) {
  # Rule-based growth sleeve with explicit stress de-risking.
  # This is a benchmark/expert, not tuned with future data.
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  vix_now <- if ("log_VIX" %in% colnames(context_row)) as.numeric(exp(context_row[, "log_VIX"])) else 20
  spy_mom <- if ("SPY_mom_63" %in% colnames(context_row)) as.numeric(context_row[, "SPY_mom_63"]) else 0
  if (!is.finite(vix_now)) vix_now <- 20
  if (!is.finite(spy_mom)) spy_mom <- 0

  if (vix_now < 20 && spy_mom > 0) {
    w <- add_weight(w, "QQQ", 0.42)
    w <- add_weight(w, "SPY", 0.25)
    w <- add_weight(w, "IWM", 0.08)
    w <- add_weight(w, "BTC", 0.10)
    w <- add_weight(w, "GLD", 0.10)
    w <- add_weight(w, "SHY", 0.05)
  } else if (vix_now < 25 && spy_mom > -0.03) {
    w <- add_weight(w, "QQQ", 0.30)
    w <- add_weight(w, "SPY", 0.25)
    w <- add_weight(w, "EFA", 0.10)
    w <- add_weight(w, "GLD", 0.15)
    w <- add_weight(w, "TLT", 0.10)
    w <- add_weight(w, "SHY", 0.10)
  } else {
    w <- add_weight(w, "SPY", 0.10)
    w <- add_weight(w, "GLD", 0.30)
    w <- add_weight(w, "SHY", 0.30)
    w <- add_weight(w, "UUP", 0.20)
    w <- add_weight(w, "TLT", 0.10)
  }
  if (sum(w) <= 0) w[] <- 1 / length(asset_names)
  w <- apply_crypto_cap(w, asset_names, crypto_total_cap, crypto_single_cap)
  target_vol <- ifelse(vix_now < 15, 0.16, ifelse(vix_now <= 25, 0.12, 0.08))
  vol_target_to_shy(w, R_hist, asset_names, target_vol = target_vol)
}

w_dual_momentum <- function(R_hist, asset_names, cap = w_max_default, lookback = 126, top_n = 3) {
  R_hist <- as.matrix(R_hist)
  n <- nrow(R_hist)
  lb <- min(lookback, n)
  mom <- colSums(log(1 + pmax(R_hist[(n - lb + 1):n, , drop = FALSE], -0.99)), na.rm = TRUE)
  names(mom) <- asset_names
  positive <- mom[mom > 0 & is.finite(mom)]
  if (length(positive) == 0) {
    w <- rep(0, length(asset_names)); names(w) <- asset_names
    if ("SHY" %in% asset_names) w["SHY"] <- 1 else w[] <- 1 / length(asset_names)
    return(as.numeric(w))
  }
  sel <- names(sort(positive, decreasing = TRUE))[seq_len(min(top_n, length(positive)))]
  score <- rep(0, length(asset_names)); names(score) <- asset_names
  score[sel] <- positive[sel]
  w <- normalize_weights(score, cap = cap)
  apply_crypto_cap(w, asset_names, crypto_total_cap, crypto_single_cap)
}

w_trend_vol_target <- function(R_hist, asset_names, context_row, cap = w_max_default) {
  # Momentum selection with inverse-volatility sizing and volatility targeting.
  R_hist <- as.matrix(R_hist)
  n <- nrow(R_hist)
  lb <- min(126, n)
  mom <- colSums(log(1 + pmax(R_hist[(n - lb + 1):n, , drop = FALSE], -0.99)), na.rm = TRUE)
  vol <- apply(tail(R_hist, min(63, n)), 2, sd, na.rm = TRUE) * sqrt(252)
  names(mom) <- names(vol) <- asset_names
  score <- pmax(mom, 0) / pmax(vol, 1e-6)
  # Prefer risk assets in calm/normal markets; allow defensive assets in stress.
  vix_now <- if ("log_VIX" %in% colnames(context_row)) as.numeric(exp(context_row[, "log_VIX"])) else 20
  if (!is.finite(vix_now)) vix_now <- 20
  if (vix_now > 25) {
    allowed <- intersect(c("GLD", "TLT", "SHY", "UUP", "SPY"), asset_names)
  } else {
    allowed <- asset_names
  }
  score[setdiff(asset_names, allowed)] <- 0
  if (sum(score, na.rm = TRUE) <= 0) {
    score[] <- 0
    if ("SHY" %in% asset_names) score["SHY"] <- 1 else score[] <- 1
  }
  w <- normalize_weights(score, cap = cap)
  w <- apply_crypto_cap(w, asset_names, crypto_total_cap, crypto_single_cap)
  target_vol <- ifelse(vix_now < 15, 0.14, ifelse(vix_now <= 25, 0.11, 0.075))
  vol_target_to_shy(w, R_hist, asset_names, target_vol = target_vol)
}


get_vix_now <- function(context_row) {
  v <- if ("log_VIX" %in% colnames(context_row)) as.numeric(exp(context_row[, "log_VIX"])) else 20
  if (!is.finite(v)) v <- 20
  v
}

get_spy_mom <- function(context_row) {
  x <- if ("SPY_mom_63" %in% colnames(context_row)) as.numeric(context_row[, "SPY_mom_63"]) else 0
  if (!is.finite(x)) x <- 0
  x
}

calc_asset_drawdown <- function(R_hist, asset_names, asset = "QQQ", lookback = 126) {
  if (!(asset %in% asset_names)) return(NA_real_)
  R_hist <- as.matrix(R_hist)
  j <- which(asset_names == asset)
  rr <- tail(R_hist[, j], min(lookback, nrow(R_hist)))
  wealth <- cumprod(1 + pmax(rr, -0.99))
  dd <- tail(wealth, 1) / max(wealth, na.rm = TRUE) - 1
  as.numeric(dd)
}

calc_asset_mom <- function(R_hist, asset_names, asset = "QQQ", lookback = 126) {
  if (!(asset %in% asset_names)) return(NA_real_)
  R_hist <- as.matrix(R_hist)
  j <- which(asset_names == asset)
  rr <- tail(R_hist[, j], min(lookback, nrow(R_hist)))
  as.numeric(sum(log(1 + pmax(rr, -0.99)), na.rm = TRUE))
}

w_qqq_trend_protected <- function(R_hist, asset_names, context_row) {
  # Captures QQQ/growth regimes but de-risks under trend breaks or high VIX.
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  vix_now <- get_vix_now(context_row)
  qqq_mom <- calc_asset_mom(R_hist, asset_names, "QQQ", 126)
  spy_mom <- get_spy_mom(context_row)
  if (!is.finite(qqq_mom)) qqq_mom <- 0
  if (vix_now < 22 && qqq_mom > 0 && spy_mom > -0.02) {
    w <- add_weight(w, "QQQ", 0.65)
    w <- add_weight(w, "SPY", 0.20)
    w <- add_weight(w, "BTC", 0.05)
    w <- add_weight(w, "GLD", 0.05)
    w <- add_weight(w, "SHY", 0.05)
  } else if (vix_now < 28 && qqq_mom > -0.08) {
    w <- add_weight(w, "QQQ", 0.35)
    w <- add_weight(w, "SPY", 0.25)
    w <- add_weight(w, "TLT", 0.15)
    w <- add_weight(w, "GLD", 0.15)
    w <- add_weight(w, "SHY", 0.10)
  } else {
    w <- add_weight(w, "SPY", 0.15)
    w <- add_weight(w, "TLT", 0.25)
    w <- add_weight(w, "GLD", 0.30)
    w <- add_weight(w, "SHY", 0.20)
    w <- add_weight(w, "UUP", 0.10)
  }
  if (sum(w) <= 0) w[] <- 1 / length(asset_names)
  apply_crypto_cap(as.numeric(w), asset_names, crypto_total_cap, crypto_single_cap)
}

w_drawdown_controlled_qqq <- function(R_hist, asset_names, context_row) {
  # QQQ sleeve protected by QQQ drawdown and VIX filters.
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  vix_now <- get_vix_now(context_row)
  qdd <- calc_asset_drawdown(R_hist, asset_names, "QQQ", 126)
  qmom <- calc_asset_mom(R_hist, asset_names, "QQQ", 126)
  if (!is.finite(qdd)) qdd <- 0
  if (!is.finite(qmom)) qmom <- 0
  if (vix_now < 25 && qdd > -0.12 && qmom > 0) {
    w <- add_weight(w, "QQQ", 0.75)
    w <- add_weight(w, "SPY", 0.15)
    w <- add_weight(w, "GLD", 0.05)
    w <- add_weight(w, "SHY", 0.05)
  } else if (vix_now < 30 && qdd > -0.20) {
    w <- add_weight(w, "QQQ", 0.35)
    w <- add_weight(w, "SPY", 0.25)
    w <- add_weight(w, "TLT", 0.20)
    w <- add_weight(w, "GLD", 0.10)
    w <- add_weight(w, "SHY", 0.10)
  } else {
    w <- add_weight(w, "TLT", 0.30)
    w <- add_weight(w, "GLD", 0.30)
    w <- add_weight(w, "SHY", 0.25)
    w <- add_weight(w, "UUP", 0.15)
  }
  if (sum(w) <= 0) w[] <- 1 / length(asset_names)
  apply_crypto_cap(as.numeric(w), asset_names, crypto_total_cap, crypto_single_cap)
}

compute_stress_intensity <- function(context_row) {
  vix_now <- get_vix_now(context_row)
  spy_mom <- get_spy_mom(context_row)
  S_t <- if ("regime_shift_score" %in% colnames(context_row)) as.numeric(context_row[, "regime_shift_score"]) else 0
  spy_dd <- if ("SPY_drawdown_252" %in% colnames(context_row)) as.numeric(context_row[, "SPY_drawdown_252"]) else 0
  if (!is.finite(S_t)) S_t <- 0
  if (!is.finite(spy_dd)) spy_dd <- 0
  raw <- 0
  raw <- raw + pmax(vix_now - 18, 0) / 18
  raw <- raw + pmax(-spy_mom - 0.03, 0) * 4
  raw <- raw + pmax(-spy_dd - 0.08, 0) * 2.5
  raw <- raw + pmax(S_t - 2, 0) / 8
  min(max(raw, 0), gsp_protection_max)
}

make_regime_top2_weights <- function(expert_ret_hist, expert_regime_hist, expert_names, current_regime, W_expert_mat) {
  if (is.null(expert_ret_hist) || nrow(expert_ret_hist) < 40 || length(expert_regime_hist) < 40) {
    # Growth-biased fallback: QQQ trend and 50/50 are allowed to dominate in favorable periods.
    fallback <- rep(0, length(expert_names)); names(fallback) <- expert_names
    for (nm in intersect(c("QQQTrendProtected", "DrawdownControlledQQQ", "SPY_QQQ_5050", "FTRL_Online"), expert_names)) fallback[nm] <- 1
    if (sum(fallback) <= 0) fallback[] <- 1
    probs <- fallback / sum(fallback)
  } else {
    n_hist <- min(nrow(expert_ret_hist), length(expert_regime_hist))
    idx <- seq_len(n_hist)
    # Prefer same-regime history, but fall back to recent all-regime data.
    same <- idx[expert_regime_hist[idx] == current_regime]
    if (length(same) >= 30) idx_use <- tail(same, min(advice_lookback, length(same))) else idx_use <- tail(idx, min(advice_lookback, length(idx)))
    R <- expert_ret_hist[idx_use, , drop = FALSE]
    L <- -log(pmax(1 + R, 1e-8))
    avg_loss <- colMeans(L, na.rm = TRUE)
    ord <- names(sort(avg_loss, decreasing = FALSE))[seq_len(min(2, length(avg_loss)))]
    probs <- rep(0, length(expert_names)); names(probs) <- expert_names
    probs[ord] <- softmax_stable(-eta_context * avg_loss[ord])
    if (sum(probs) <= 0) probs[] <- 1 / length(probs) else probs <- probs / sum(probs)
  }
  w <- as.numeric(W_expert_mat %*% probs[expert_names])
  normalize_weights(w, cap = gsp_growth_cap)
}

make_utility_selector_weights <- function(expert_ret_hist, expert_names, W_expert_mat) {
  if (is.null(expert_ret_hist) || nrow(expert_ret_hist) < 40) {
    probs <- rep(1 / length(expert_names), length(expert_names)); names(probs) <- expert_names
  } else {
    idx <- max(1, nrow(expert_ret_hist) - advice_lookback + 1):nrow(expert_ret_hist)
    R <- expert_ret_hist[idx, , drop = FALSE]
    mean_r <- colMeans(R, na.rm = TRUE)
    down_sd <- apply(pmin(R, 0), 2, sd, na.rm = TRUE)
    tail_loss <- apply(R, 2, safe_cvar_loss, alpha = 0.95)
    util <- mean_r - lambda_downside_utility * down_sd - lambda_tail_utility * tail_loss
    probs <- softmax_stable(eta_context * util)
    names(probs) <- expert_names
  }
  w <- as.numeric(W_expert_mat %*% probs[expert_names])
  normalize_weights(w, cap = gsp_growth_cap)
}


candidate_history_stats <- function(candidate_ret_hist, lookback = db_lookback) {
  if (is.null(candidate_ret_hist) || nrow(candidate_ret_hist) < 20) return(NULL)
  idx <- max(1, nrow(candidate_ret_hist) - lookback + 1):nrow(candidate_ret_hist)
  R <- candidate_ret_hist[idx, , drop = FALSE]
  stats <- fast_frame(
    candidate = colnames(R),
    mean_log = NA_real_,
    ann_return = NA_real_,
    ann_vol = NA_real_,
    mdd = NA_real_,
    cvar = NA_real_,
    sharpe = NA_real_,
    stringsAsFactors = FALSE
  )
  for (j in seq_along(colnames(R))) {
    r <- as.numeric(R[, j])
    r <- r[is.finite(r)]
    if (length(r) < 20) next
    stats$mean_log[j] <- mean(log(pmax(1 + r, 1e-8)), na.rm = TRUE)
    stats$ann_return[j] <- prod(1 + r, na.rm = TRUE)^(252 / length(r)) - 1
    stats$ann_vol[j] <- sd(r, na.rm = TRUE) * sqrt(252)
    stats$mdd[j] <- max_drawdown_numeric(r)
    stats$cvar[j] <- safe_cvar_loss(r, 0.95)
    stats$sharpe[j] <- mean(r, na.rm = TRUE) / sd(r, na.rm = TRUE) * sqrt(252)
  }
  stats
}


w_6040 <- function(asset_names) {
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  if (all(c("SPY", "TLT") %in% asset_names)) {
    w["SPY"] <- 0.60; w["TLT"] <- 0.40
  } else {
    w[] <- 1 / length(asset_names)
  }
  as.numeric(w)
}



w_momentum <- function(R_hist, asset_names, cap = w_max_default, lookback = 63) {
  R_hist <- as.matrix(R_hist)
  n <- nrow(R_hist)
  lb <- min(lookback, n)
  score <- colSums(log(1 + pmax(R_hist[(n - lb + 1):n, , drop = FALSE], -0.99)), na.rm = TRUE)
  score <- pmax(score, 0)
  if (sum(score) <= 0) score <- rep(1, length(asset_names))
  w <- normalize_weights(score, cap = cap)
  apply_crypto_cap(w, asset_names, crypto_total_cap, crypto_single_cap)
}

w_mean_reversion <- function(R_hist, asset_names, cap = w_max_default, lookback = 5) {
  R_hist <- as.matrix(R_hist)
  n <- nrow(R_hist)
  lb <- min(lookback, n)
  short_ret <- colSums(R_hist[(n - lb + 1):n, , drop = FALSE], na.rm = TRUE)
  score <- pmax(-short_ret, 0)
  if (sum(score) <= 0) score <- rep(1, length(asset_names))
  w <- normalize_weights(score, cap = cap)
  apply_crypto_cap(w, asset_names, crypto_total_cap, crypto_single_cap)
}

w_safe_haven <- function(asset_names, vix_now) {
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  add <- function(sym, val) { if (sym %in% asset_names) w[sym] <<- val }

  if (!is.finite(vix_now)) vix_now <- 20

  if (vix_now > 25) {
    add("GLD", 0.30); add("TLT", 0.35); add("SHY", 0.20); add("UUP", 0.15)
  } else if (vix_now >= 15) {
    add("SPY", 0.20); add("QQQ", 0.10); add("IWM", 0.05); add("EFA", 0.05); add("EEM", 0.05)
    add("GLD", 0.20); add("TLT", 0.20); add("SHY", 0.10); add("UUP", 0.05)
  } else {
    add("SPY", 0.25); add("QQQ", 0.20); add("IWM", 0.10); add("EFA", 0.10); add("EEM", 0.10)
    add("GLD", 0.10); add("TLT", 0.10); add("SHY", 0.03); add("UUP", 0.02)
  }
  if (sum(w) <= 0) w[] <- 1 / length(asset_names)
  normalize_weights(w, cap = 1.0)
}

w_crypto_control <- function(R_hist, asset_names, context_row, cap = w_max_default) {
  # Momentum backbone with strict crypto cap and stress filters.
  w <- w_momentum(R_hist, asset_names, cap = cap, lookback = 63)
  names(w) <- asset_names
  crypto_names <- intersect(c("BTC", "ETH"), asset_names)
  if (length(crypto_names) == 0) return(as.numeric(w))

  vix_now <- if ("log_VIX" %in% colnames(context_row)) as.numeric(exp(context_row[, "log_VIX"])) else 20
  regime_score <- if ("regime_shift_score" %in% colnames(context_row)) as.numeric(context_row[, "regime_shift_score"]) else 0

  # If stress is high, sharply reduce crypto.
  if (is.finite(vix_now) && vix_now > 25) w[crypto_names] <- 0
  if (is.finite(regime_score) && regime_score > 4) w[crypto_names] <- 0

  # If BTC drawdown is severe, reduce crypto.
  if ("BTC_drawdown_252" %in% colnames(context_row)) {
    btc_dd <- as.numeric(context_row[, "BTC_drawdown_252"])
    if (is.finite(btc_dd) && btc_dd < -0.50) w[intersect("BTC", crypto_names)] <- 0
  }

  apply_crypto_cap(as.numeric(w), asset_names, crypto_total_cap, crypto_single_cap)
}


w_qqq_soft_guard <- function(R_hist, asset_names, context_row) {
  # More permissive QQQ participation than QQQTrendProtected.
  # It exits only under severe trend, drawdown, or VIX stress.
  w <- rep(0, length(asset_names)); names(w) <- asset_names
  vix_now <- get_vix_now(context_row)
  qqq_ret_63 <- 0
  qqq_dd <- 0
  if ("QQQ" %in% colnames(R_hist)) {
    q <- as.numeric(R_hist[, "QQQ"])
    lb <- min(63, length(q))
    qqq_ret_63 <- prod(1 + tail(q, lb), na.rm = TRUE) - 1
    wealth <- cumprod(1 + q)
    qqq_dd <- tail(wealth, 1) / max(wealth, na.rm = TRUE) - 1
  }
  severe_stress <- is.finite(vix_now) && vix_now > 35
  severe_trend <- is.finite(qqq_ret_63) && qqq_ret_63 < -0.15
  severe_dd <- is.finite(qqq_dd) && qqq_dd < -0.25
  add <- function(sym, val) { if (sym %in% asset_names) w[sym] <<- val }
  if (!severe_stress && !severe_trend && !severe_dd) {
    add("QQQ", 0.65); add("SPY", 0.25); add("TLT", 0.05); add("GLD", 0.05)
  } else {
    add("SPY", 0.35); add("TLT", 0.30); add("GLD", 0.20); add("SHY", 0.10); add("UUP", 0.05)
  }
  if (sum(w) <= 0) w[] <- 1 / length(asset_names)
  normalize_weights(w, cap = 0.75)
}

make_expert_weights <- function(R_hist, asset_names, context_row) {
  vix_now <- if ("log_VIX" %in% colnames(context_row)) as.numeric(exp(context_row[, "log_VIX"])) else 20
  list(
    EqualWeight = w_equal(asset_names),
    SPY_BuyHold = w_spy_bh(asset_names),
    QQQ_BuyHold = w_qqq_bh(asset_names),
    SPY_QQQ_5050 = w_spy_qqq_5050(asset_names),
    QQQTrendProtected = w_qqq_trend_protected(R_hist, asset_names, context_row),
    DrawdownControlledQQQ = w_drawdown_controlled_qqq(R_hist, asset_names, context_row),
    QQQSoftGuard = w_qqq_soft_guard(R_hist, asset_names, context_row),
    SPY_TLT_6040 = w_6040(asset_names),
    InverseVol = w_inverse_vol(R_hist, asset_names),
    MinVariance = w_minvar(R_hist, asset_names),
    Momentum = w_momentum(R_hist, asset_names),
    DualMomentum = w_dual_momentum(R_hist, asset_names),
    TrendVolTarget = w_trend_vol_target(R_hist, asset_names, context_row),
    RiskOnGrowth = w_risk_on_growth(R_hist, asset_names, context_row),
    MeanReversion = w_mean_reversion(R_hist, asset_names),
    SafeHaven = w_safe_haven(asset_names, vix_now),
    CryptoControl = w_crypto_control(R_hist, asset_names, context_row)
  )
}


run_ta_laops_backtest <- function(R_simple, context_features, regime_xts,
                                  label = "main", tcost_bps = tcost_bps_main,
                                  keep_weights = TRUE, verbose = TRUE, config = list()) {
  R_simple <- na.omit(R_simple)
  asset_names <- colnames(R_simple)
  p <- length(asset_names)

  # Retain raw return history for expert estimation; do not discard it at context warm-up.
  X <- context_features[index(R_simple)]
  Reg <- regime_xts[index(R_simple)]

  n <- nrow(R_simple)
  if (n < min_lookback + 50) stop("Insufficient observations for backtest: ", label)

  # Earliest index must have enough historical returns and complete context features.
  start_i <- max(min_lookback + SIGNAL_LAG,
                 which(complete.cases(X))[1] + SIGNAL_LAG,
                 which(as.Date(index(R_simple)) >= SIM_START)[1])
  end_i <- n - 1

  # Expert and model names.
  expert_names <- names(make_expert_weights(as.matrix(R_simple[1:min_lookback, ]), asset_names, X[min_lookback, ]))
  db_candidate_names <- c(
    "QQQ_BuyHold", "QQQSoftGuard", "SPY_QQQ_5050", "SPY_BuyHold", "EqualWeight",
    "FTRL_Online", "TA_LAOPS", "ROB_Backbone", "GSP_NoStressProtection",
    "GrowthFTRLHybrid", "SafeHaven", "MinVariance", "InverseVol", "GSP_ProtectionSleeve"
  )
  db_model_names <- c("DB_LAOPS", "DB_StrictBudget", "DB_NoRiskBudget", "DB_GrowthGuard", "DB_TailBudget")
  model_names <- c(
    expert_names,
    "StaticExpertEnsemble",
    "EG_Online",
    "OMD_Online",
    "FTRL_Online",
    "ROB_EqualBackbone",
    "ROB_Backbone",
    "LossWeightedAdvice",
    "ContextualML_Advice",
    "GrowthFTRLHybrid",
    "RegimeTop2Switch",
    "UtilityExpertSelector",
    "GSP_GrowthSleeve",
    "GSP_ProtectionSleeve",
    "GSP_LAOPS",
    "GSP_NoStressProtection",
    "GSP_NoTurnoverControl",
    db_model_names,
    "TA_LAOPS",
    "TA_FixedTau",
    "TA_NoRegimePenalty",
    "TA_NoEntropyGate",
    "TA_NoTurnoverControl"
  )

  ext_names <- c("Hedge_Experts", "OGD_Experts", "TC_MeanVariance", "Mean_CVaR", "Mean_CDaR",
                 "QQQ_SHY_V10", "SPY_SHY_V10", "Balanced_SHY_V10", "Equal_SHY_V10",
                 "QQQ_SHY_MatchDB", "SPY_SHY_MatchDB", "Balanced_SHY_MatchDB", "Equal_SHY_MatchDB",
                 "DB_UniformCandidates", "DB_BudgetEqual", "DB_NoTurnover", "DB_NoStress", "DB_NoDrawdown", "DB_NoCVaR")
  model_names <- c(model_names, ext_names)
  ext_state <- list(q = rep(1 / length(expert_names), length(expert_names)),
                    cumulative = rep(0, length(expert_names)))
  candidate_weights_out <- vector("list",n-1L)
  db_actual_hist <- numeric()
  db_targets <- matrix(NA_real_, nrow=n-1, ncol=p, dimnames=list(NULL,asset_names))
  diag_out <- list()
  # Previous weights are pre-trade drifted holdings, not the last targets.
  prev_w <- list()
  for (m in model_names) prev_w[[m]] <- w_equal(asset_names)

  # Online algorithm states.
  w_EG <- w_equal(asset_names)
  w_OMD <- w_equal(asset_names)
  cum_score_FTRL <- rep(0, p)

  online_names <- c("EG_Online", "OMD_Online", "FTRL_Online")
  online_ret_hist <- matrix(nrow = 0, ncol = length(online_names))
  colnames(online_ret_hist) <- online_names

  db_ret_hist <- matrix(nrow = 0, ncol = length(db_candidate_names))
  colnames(db_ret_hist) <- db_candidate_names

  # Historical expert net returns and corresponding contexts for advice learning.
  expert_ret_hist <- matrix(nrow = 0, ncol = length(expert_names))
  colnames(expert_ret_hist) <- expert_names
  expert_context_hist <- matrix(nrow = 0, ncol = ncol(X))
  colnames(expert_context_hist) <- colnames(X)
  expert_context_dates <- as.Date(character(0))
  expert_regime_hist <- character(0)

  # Advice and robust-backbone histories for trust error.
  advice_net_hist <- numeric(0)
  robust_net_hist <- numeric(0)

  returns_out <- list()
  weights_out <- list()
  tau_out <- list()
  prob_out <- list()

  for (i in start_i:end_i) {
    d0 <- index(R_simple)[i]
    d1 <- index(R_simple)[i + 1]

    signal_i <- i - SIGNAL_LAG
    R_hist <- as.matrix(R_simple[1:signal_i, ])
    R_est <- tail(R_hist, expert_lookback)
    context_row <- X[signal_i, ]
    next_ret <- as.numeric(R_simple[i + 1, ])
    names(next_ret) <- asset_names

    # Histories ending at signal_i only. Each appended row is a realized next return.
    nh <- max(0L, i - start_i - SIGNAL_LAG)
    eh <- if (nh) expert_ret_hist[seq_len(nh),,drop=FALSE] else expert_ret_hist[integer(0),,drop=FALSE]
    xh <- if (nh) expert_context_hist[seq_len(nh),,drop=FALSE] else expert_context_hist[integer(0),,drop=FALSE]
    oh <- if (nh) online_ret_hist[seq_len(nh),,drop=FALSE] else online_ret_hist[integer(0),,drop=FALSE]
    dh <- if (nh) db_ret_hist[seq_len(nh),,drop=FALSE] else db_ret_hist[integer(0),,drop=FALSE]
    ah <- head(advice_net_hist, nh); rh <- head(robust_net_hist, nh)
    regh <- head(expert_regime_hist, nh)
    # 1) Expert weights.
    experts <- make_expert_weights(R_est, asset_names, context_row)
    W_expert_mat <- do.call(cbind, experts)
    colnames(W_expert_mat) <- expert_names

    # 2) Online robust benchmark weights using latest observed return R_i.
    r_today <- as.numeric(R_simple[signal_i, ])
    r_today[!is.finite(r_today)] <- 0

    w_EG <- normalize_weights(w_EG * exp(eta_eg * r_today), cap = w_max_default)
    w_EG <- apply_crypto_cap(w_EG, asset_names, crypto_total_cap, crypto_single_cap)

    w_OMD <- project_box_simplex(w_OMD + eta_omd * r_today, cap = w_max_default)
    w_OMD <- apply_crypto_cap(w_OMD, asset_names, crypto_total_cap, crypto_single_cap)

    cum_score_FTRL <- 0.995 * cum_score_FTRL + r_today
    w_FTRL <- softmax_stable(eta_ftrl * cum_score_FTRL)
    w_FTRL <- normalize_weights(w_FTRL, cap = w_max_default)
    w_FTRL <- apply_crypto_cap(w_FTRL, asset_names, crypto_total_cap, crypto_single_cap)

    # Robust backbone for learning-augmented allocation.
    # v3 uses a loss-weighted combination of online algorithms rather than a fixed average.
    w_ROB_equal <- normalize_weights((w_EG + w_OMD + w_FTRL) / 3, cap = w_max_default)
    w_ROB_equal <- apply_crypto_cap(w_ROB_equal, asset_names, crypto_total_cap, crypto_single_cap)

    prob_rob <- make_loss_weighted_probs(
      expert_ret_hist = oh,
      expert_names = online_names,
      lookback = advice_lookback,
      eta = eta_rob
    )
    W_online_mat <- cbind(EG_Online = w_EG, OMD_Online = w_OMD, FTRL_Online = w_FTRL)
    w_ROB <- as.numeric(W_online_mat %*% prob_rob[online_names])
    w_ROB <- normalize_weights(w_ROB, cap = w_max_default)
    w_ROB <- apply_crypto_cap(w_ROB, asset_names, crypto_total_cap, crypto_single_cap)

    # 3) Learning-augmented expert advice probabilities.
    # LossWeightedAdvice ignores current context except through past realized expert losses.
    prob_loss <- make_loss_weighted_probs(
      expert_ret_hist = eh,
      expert_names = expert_names,
      lookback = advice_lookback,
      eta = eta_expert
    )

    # ContextualML_Advice uses local similarity in the market-context feature space.
    prob_ctx <- make_contextual_ml_probs(
      expert_ret_hist = eh,
      expert_context_hist = xh,
      x_now = as.numeric(context_row),
      expert_names = expert_names,
      lookback = advice_lookback,
      eta = eta_context,
      bandwidth_quantile = 0.35
    )

    if (NO_CONTEXT) prob_ctx[] <- 1 / length(prob_ctx)
    w_LOSSADV <- as.numeric(W_expert_mat %*% prob_loss)
    w_LOSSADV <- normalize_weights(w_LOSSADV, cap = w_max_default)
    w_LOSSADV <- apply_crypto_cap(w_LOSSADV, asset_names, crypto_total_cap, crypto_single_cap)

    w_CTXADV <- as.numeric(W_expert_mat %*% prob_ctx)
    w_CTXADV <- normalize_weights(w_CTXADV, cap = w_max_default)
    w_CTXADV <- apply_crypto_cap(w_CTXADV, asset_names, crypto_total_cap, crypto_single_cap)


    # 3b) Growth/protection candidate weights retained for v6 candidate selection.
    current_regime <- as.character(Reg[signal_i, 1])
    stress_intensity <- compute_stress_intensity(context_row)

    w_QQQTP <- experts[["QQQTrendProtected"]]
    w_DDQQQ <- experts[["DrawdownControlledQQQ"]]
    w_SPYQQQ <- experts[["SPY_QQQ_5050"]]
    w_SAFE <- experts[["SafeHaven"]]
    w_MINV <- experts[["MinVariance"]]

    # Growth sleeve: deliberately captures QQQ/growth regimes while retaining FTRL adaptivity.
    w_GROWTH <- normalize_weights(
      0.35 * w_QQQTP + 0.25 * w_DDQQQ + 0.20 * w_SPYQQQ + 0.20 * w_FTRL,
      cap = gsp_growth_cap
    )
    w_GROWTH <- apply_crypto_cap(w_GROWTH, asset_names, crypto_total_cap, crypto_single_cap)

    # Protection sleeve: explicit downside/risk-control allocation.
    w_PROTECT <- normalize_weights(
      0.35 * w_SAFE + 0.25 * w_MINV + 0.25 * w_ROB + 0.15 * experts[["SPY_TLT_6040"]],
      cap = w_max_default
    )
    w_PROTECT <- apply_crypto_cap(w_PROTECT, asset_names, crypto_total_cap, crypto_single_cap)

    w_GrowthFTRLHybrid <- normalize_weights(
      (1 - stress_intensity) * normalize_weights(0.55 * w_GROWTH + 0.45 * w_FTRL, cap = gsp_growth_cap) +
        stress_intensity * normalize_weights(0.55 * w_PROTECT + 0.45 * w_FTRL, cap = w_max_default),
      cap = gsp_growth_cap
    )
    w_GrowthFTRLHybrid <- apply_crypto_cap(w_GrowthFTRLHybrid, asset_names, crypto_total_cap, crypto_single_cap)

    w_RegimeTop2 <- make_regime_top2_weights(eh, regh, expert_names, current_regime, W_expert_mat)
    w_RegimeTop2 <- apply_crypto_cap(w_RegimeTop2, asset_names, crypto_total_cap, crypto_single_cap)

    w_Utility <- make_utility_selector_weights(eh, expert_names, W_expert_mat)
    w_Utility <- apply_crypto_cap(w_Utility, asset_names, crypto_total_cap, crypto_single_cap)

    # Retained GSP target used as an additional benchmark candidate.
    w_GSP_target <- normalize_weights(
      (1 - stress_intensity) * normalize_weights(0.55 * w_GROWTH + 0.20 * w_GrowthFTRLHybrid + 0.15 * w_RegimeTop2 + 0.10 * w_Utility, cap = gsp_growth_cap) +
        stress_intensity * normalize_weights(0.55 * w_PROTECT + 0.25 * w_GrowthFTRLHybrid + 0.20 * w_ROB, cap = w_max_default),
      cap = gsp_growth_cap
    )
    w_GSP_target <- apply_crypto_cap(w_GSP_target, asset_names, crypto_total_cap, crypto_single_cap)

    tc_GSP <- apply_turnover_control(w_GSP_target, prev_w[["GSP_LAOPS"]], gsp_no_trade_band, gsp_daily_turnover_cap)
    w_GSP <- tc_GSP$w

    w_GSP_no_stress <- normalize_weights(0.60 * w_GROWTH + 0.25 * w_GrowthFTRLHybrid + 0.15 * w_RegimeTop2, cap = gsp_growth_cap)
    w_GSP_no_stress <- apply_crypto_cap(w_GSP_no_stress, asset_names, crypto_total_cap, crypto_single_cap)
    w_GSP_no_stress <- apply_turnover_control(w_GSP_no_stress, prev_w[["GSP_NoStressProtection"]], gsp_no_trade_band, gsp_daily_turnover_cap)$w

    w_GSP_no_tc <- w_GSP_target

    # 4) Trust score tau_t.
    # E_t: recent underperformance of ContextualML_Advice relative to ROB_Backbone.
    if (length(ah) < 20 || length(rh) < 20) {
      E_t <- 0
    } else {
      idx_recent <- max(1, length(ah) - advice_lookback + 1):length(ah)
      loss_adv <- -log(pmax(1 + ah[idx_recent], 1e-8))
      loss_rob <- -log(pmax(1 + rh[idx_recent], 1e-8))
      E_t <- mean(loss_adv - loss_rob, na.rm = TRUE)
      E_t <- max(E_t, 0)
    }

    S_t <- if ("regime_shift_score" %in% colnames(context_row)) as.numeric(context_row[, "regime_shift_score"]) else 0
    if (!is.finite(S_t)) S_t <- 0

    # Entropy-gated confidence: if expert probabilities are nearly uniform, advice is unreliable.
    H_ctx <- entropy01(prob_ctx)
    confidence_gate <- max(0, 1 - H_ctx)^entropy_gamma

    tau_t <- tau_min + (tau_max - tau_min) *
      exp(-lambda_error * E_t) * exp(-kappa_regime * max(S_t, 0)) * confidence_gate
    tau_t <- min(max(tau_t, tau_min), tau_max)

    tau_no_regime <- tau_min + (tau_max - tau_min) *
      exp(-lambda_error * E_t) * confidence_gate
    tau_no_regime <- min(max(tau_no_regime, tau_min), tau_max)

    tau_no_entropy <- tau_min + (tau_max - tau_min) *
      exp(-lambda_error * E_t) * exp(-kappa_regime * max(S_t, 0))
    tau_no_entropy <- min(max(tau_no_entropy, tau_min), tau_max)

    # 5) TA-LAOPS, GSP, and DB-LAOPS ablations.
    # The full model uses ContextualML_Advice as the learning-augmented advice leg.
    w_TA_target <- normalize_weights((1 - tau_t) * w_ROB + tau_t * w_CTXADV, cap = w_max_default)
    w_TA_target <- apply_crypto_cap(w_TA_target, asset_names, crypto_total_cap, crypto_single_cap)
    tc_TA <- apply_turnover_control(w_TA_target, prev_w[["TA_LAOPS"]], no_trade_band, daily_turnover_cap)
    w_TA <- tc_TA$w

    w_TA_no_tc <- w_TA_target

    w_TA_fixed_target <- normalize_weights((1 - fixed_tau) * w_ROB + fixed_tau * w_CTXADV, cap = w_max_default)
    w_TA_fixed_target <- apply_crypto_cap(w_TA_fixed_target, asset_names, crypto_total_cap, crypto_single_cap)
    w_TA_fixed <- apply_turnover_control(w_TA_fixed_target, prev_w[["TA_FixedTau"]], no_trade_band, daily_turnover_cap)$w

    w_TA_no_regime_target <- normalize_weights((1 - tau_no_regime) * w_ROB + tau_no_regime * w_CTXADV, cap = w_max_default)
    w_TA_no_regime_target <- apply_crypto_cap(w_TA_no_regime_target, asset_names, crypto_total_cap, crypto_single_cap)
    w_TA_no_regime <- apply_turnover_control(w_TA_no_regime_target, prev_w[["TA_NoRegimePenalty"]], no_trade_band, daily_turnover_cap)$w

    w_TA_no_entropy_target <- normalize_weights((1 - tau_no_entropy) * w_ROB + tau_no_entropy * w_CTXADV, cap = w_max_default)
    w_TA_no_entropy_target <- apply_crypto_cap(w_TA_no_entropy_target, asset_names, crypto_total_cap, crypto_single_cap)
    w_TA_no_entropy <- apply_turnover_control(w_TA_no_entropy_target, prev_w[["TA_NoEntropyGate"]], no_trade_band, daily_turnover_cap)$w

    # 5b) Drawdown-budgeted candidate-selection layer for v6.
    db_candidates <- list(
      QQQ_BuyHold = experts[["QQQ_BuyHold"]],
      QQQSoftGuard = experts[["QQQSoftGuard"]],
      SPY_QQQ_5050 = experts[["SPY_QQQ_5050"]],
      SPY_BuyHold = experts[["SPY_BuyHold"]],
      EqualWeight = experts[["EqualWeight"]],
      FTRL_Online = w_FTRL,
      TA_LAOPS = w_TA,
      ROB_Backbone = w_ROB,
      GSP_NoStressProtection = w_GSP_no_stress,
      GrowthFTRLHybrid = w_GrowthFTRLHybrid,
      SafeHaven = w_SAFE,
      MinVariance = w_MINV,
      InverseVol = experts[["InverseVol"]],
      GSP_ProtectionSleeve = w_PROTECT
    )
    db_candidates <- db_candidates[db_candidate_names]
    candidate_weights_out[[i]] <- db_candidates

    db_main <- make_db_selector_weights(dh, db_candidates, asset_names, prev_w[["DB_LAOPS"]], context_row, mode = "main")
    w_DB <- apply_turnover_control(db_main$w, prev_w[["DB_LAOPS"]], db_no_trade_band, db_daily_turnover_cap)$w

    db_strict <- make_db_selector_weights(dh, db_candidates, asset_names, prev_w[["DB_StrictBudget"]], context_row, mode = "strict", mdd_budget = db_strict_mdd_budget, cvar_budget = db_strict_cvar_budget)
    w_DB_strict <- apply_turnover_control(db_strict$w, prev_w[["DB_StrictBudget"]], db_no_trade_band, db_daily_turnover_cap)$w

    db_norisk <- make_db_selector_weights(dh, db_candidates, asset_names, prev_w[["DB_NoRiskBudget"]], context_row, mode = "norisks")
    w_DB_norisk <- apply_turnover_control(db_norisk$w, prev_w[["DB_NoRiskBudget"]], db_no_trade_band, db_daily_turnover_cap)$w

    db_growth <- make_db_selector_weights(dh, db_candidates, asset_names, prev_w[["DB_GrowthGuard"]], context_row, mode = "growth", mdd_budget = db_soft_mdd_budget, cvar_budget = db_soft_cvar_budget)
    w_DB_growth <- apply_turnover_control(db_growth$w, prev_w[["DB_GrowthGuard"]], db_no_trade_band, db_daily_turnover_cap)$w

    db_tail <- make_db_selector_weights(dh, db_candidates, asset_names, prev_w[["DB_TailBudget"]], context_row, mode = "tail", mdd_budget = db_mdd_budget, cvar_budget = db_strict_cvar_budget)
    w_DB_tail <- apply_turnover_control(db_tail$w, prev_w[["DB_TailBudget"]], db_no_trade_band, db_daily_turnover_cap)$w

    db_targets[i, ] <- db_main$w
    ext_state$db_hist <- head(db_actual_hist, nh)
    ext <- external_weights(R_est, experts, db_candidates, eh, dh, prev_w,
                            ext_state, i-start_i+1L, tcost_bps/10000, context_row)
    ext_state <- ext$state
    diag_out[[length(diag_out)+1L]] <- fast_frame(date=as.Date(d1),
      signal_date=as.Date(index(R_simple)[signal_i]),
      db_fallback=ifelse(is.null(db_main$fallback), TRUE, db_main$fallback),
      db_mdd_budget=ifelse(is.null(db_main$budget_dd),NA,db_main$budget_dd),
      db_cvar_budget=ifelse(is.null(db_main$budget_cvar),NA,db_main$budget_cvar),
      match_target=ext$target_vol, match_qqq_predicted=ext$matched_vol,
      match_qqq_attainable=ext$attainable,
      cvar_solver=ext$cvar_status, cdar_solver=ext$cdar_status)
    # 6) Full model weight list.
    static_ens <- normalize_weights(rowMeans(W_expert_mat), cap = w_max_default)
    static_ens <- apply_crypto_cap(static_ens, asset_names, crypto_total_cap, crypto_single_cap)

    W <- c(
      experts,
      list(
        StaticExpertEnsemble = static_ens,
        EG_Online = w_EG,
        OMD_Online = w_OMD,
        FTRL_Online = w_FTRL,
        ROB_EqualBackbone = w_ROB_equal,
        ROB_Backbone = w_ROB,
        LossWeightedAdvice = w_LOSSADV,
        ContextualML_Advice = w_CTXADV,
        GrowthFTRLHybrid = w_GrowthFTRLHybrid,
        RegimeTop2Switch = w_RegimeTop2,
        UtilityExpertSelector = w_Utility,
        GSP_GrowthSleeve = w_GROWTH,
        GSP_ProtectionSleeve = w_PROTECT,
        GSP_LAOPS = w_GSP,
        GSP_NoStressProtection = w_GSP_no_stress,
        GSP_NoTurnoverControl = w_GSP_no_tc,
        DB_LAOPS = w_DB,
        DB_StrictBudget = w_DB_strict,
        DB_NoRiskBudget = w_DB_norisk,
        DB_GrowthGuard = w_DB_growth,
        DB_TailBudget = w_DB_tail,
        TA_LAOPS = w_TA,
        TA_FixedTau = w_TA_fixed,
        TA_NoRegimePenalty = w_TA_no_regime,
        TA_NoEntropyGate = w_TA_no_entropy,
        TA_NoTurnoverControl = w_TA_no_tc
      )
    )

    W <- c(W, ext$weights)

    # 7) Evaluate one-step-ahead returns and turnover.
    gross_model_return <- numeric(length(W)); names(gross_model_return) <- names(W)
    net_model_return <- numeric(length(W)); names(net_model_return) <- names(W)
    turnover_model <- numeric(length(W)); names(turnover_model) <- names(W)

    for (m in names(W)) {
      ww <- as.numeric(W[[m]])
      ww[!is.finite(ww)] <- 0
      ww <- normalize_weights(ww, cap = 1.0)
      # Target constraints have priority over the no-trade band after drift.
      constrained <- grepl("^(DB_|TA_|ROB_|EG_|OMD_|FTRL_|Hedge_|OGD_|TC_)",m) ||
        m %in% c("ContextualML_Advice","LossWeightedAdvice","StaticExpertEnsemble","InverseVol","MinVariance")
      if(constrained) ww <- project_assets(ww,asset_names,if(m=="DB_GrowthGuard") .8 else .35)

      trade <- execute_trade(ww, prev_w[[m]], next_ret, tcost_bps/10000)
      turnover <- trade$turnover
      cost_turnover <- turnover
      gross_ret <- trade$gross
      net_ret <- trade$net

      gross_model_return[m] <- gross_ret
      net_model_return[m] <- net_ret
      turnover_model[m] <- turnover

      returns_out[[length(returns_out) + 1]] <- fast_frame(
        date = as.Date(d1),
        decision_date = as.Date(d0),
        signal_date = as.Date(index(R_simple)[signal_i]),
        sample = label,
        model = m,
        gross_return = gross_ret,
        net_return = net_ret,
        cost_fraction = trade$cost,
        turnover = turnover,
        cost_turnover = cost_turnover,
        tcost_bps = tcost_bps,
        tau = ifelse(m %in% c("TA_LAOPS", "TA_FixedTau", "TA_NoRegimePenalty", "TA_NoEntropyGate", "TA_NoTurnoverControl"),
                     ifelse(m == "TA_LAOPS", tau_t,
                            ifelse(m == "TA_FixedTau", fixed_tau,
                                   ifelse(m == "TA_NoRegimePenalty", tau_no_regime,
                                          ifelse(m == "TA_NoEntropyGate", tau_no_entropy, tau_t)))), NA_real_),
        regime_shift_score = S_t,
        advice_error = E_t,
        confidence_gate = confidence_gate,
        tau_no_entropy = tau_no_entropy,
        ta_raw_turnover = tc_TA$raw_turnover,
        ta_zeta = tc_TA$zeta,
      gsp_stress_intensity = stress_intensity,
      gsp_raw_turnover = tc_GSP$raw_turnover,
      gsp_zeta = tc_GSP$zeta,
        db_selected = db_main$selected,
        db_pass_count = db_main$pass_count,
        db_score = ifelse(is.null(db_main$score), NA_real_, db_main$score),
        prob_entropy_lossweighted = entropy01(prob_loss),
        prob_entropy_contextual = entropy01(prob_ctx),
        regime = as.character(Reg[signal_i, 1])
      )

      if (keep_weights) {
        weights_out[[length(weights_out) + 1]] <- fast_frame(
          date = as.Date(d0),
          sample = label,
          model = m,
          asset = asset_names,
          weight = ww,
          tcost_bps = tcost_bps
        )
      }

      prev_w[[m]] <- trade$drift
    }

    db_actual_hist <- c(db_actual_hist,net_model_return["DB_LAOPS"])
    # 8) Append expert, online, context, advice and robust-backbone histories for future decisions.
    online_net_vec <- c(
      EG_Online = unname(net_model_return["EG_Online"]),
      OMD_Online = unname(net_model_return["OMD_Online"]),
      FTRL_Online = unname(net_model_return["FTRL_Online"])
    )
    online_ret_hist <- rbind(online_ret_hist, online_net_vec)

    db_net_vec <- sapply(db_candidate_names, function(k) net_model_return[k])
    db_ret_hist <- rbind(db_ret_hist, db_net_vec)

    exp_net_vec <- sapply(expert_names, function(k) net_model_return[k])
    expert_ret_hist <- rbind(expert_ret_hist, exp_net_vec)
    expert_context_hist <- rbind(expert_context_hist, as.numeric(context_row))
    colnames(expert_context_hist) <- colnames(X)
    expert_context_dates <- c(expert_context_dates, as.Date(d0))
    expert_regime_hist <- c(expert_regime_hist, current_regime)

    advice_net_hist <- c(advice_net_hist, net_model_return["ContextualML_Advice"])
    robust_net_hist <- c(robust_net_hist, net_model_return["ROB_Backbone"])

    tau_out[[length(tau_out) + 1]] <- fast_frame(
      date = as.Date(d0),
      sample = label,
      tcost_bps = tcost_bps,
      tau = tau_t,
      tau_no_regime = tau_no_regime,
      tau_no_entropy = tau_no_entropy,
      fixed_tau = fixed_tau,
      advice_error = E_t,
      regime_shift_score = S_t,
      confidence_gate = confidence_gate,
      ta_raw_turnover = tc_TA$raw_turnover,
      ta_zeta = tc_TA$zeta,
      gsp_stress_intensity = stress_intensity,
      gsp_raw_turnover = tc_GSP$raw_turnover,
      gsp_zeta = tc_GSP$zeta,
      db_selected = db_main$selected,
      db_pass_count = db_main$pass_count,
      db_score = ifelse(is.null(db_main$score), NA_real_, db_main$score),
      prob_entropy_lossweighted = entropy01(prob_loss),
      prob_entropy_contextual = entropy01(prob_ctx),
      rob_prob_eg = as.numeric(prob_rob["EG_Online"]),
      rob_prob_omd = as.numeric(prob_rob["OMD_Online"]),
      rob_prob_ftrl = as.numeric(prob_rob["FTRL_Online"]),
      regime = as.character(Reg[signal_i, 1])
    )

    prob_out[[length(prob_out) + 1]] <- fast_frame(
      date = as.Date(d0),
      sample = label,
      tcost_bps = tcost_bps,
      expert = expert_names,
      prob_lossweighted = as.numeric(prob_loss[expert_names]),
      prob_contextual = as.numeric(prob_ctx[expert_names])
    )

    if (verbose && ((i - start_i + 1) %% 250 == 0)) {
      message(label, " | ", tcost_bps, " bps: completed ", i - start_i + 1, " / ", end_i - start_i + 1, " decisions")
    }
  }

  returns_df <- bind_rows(returns_out)
  weights_df <- if (keep_weights) bind_rows(weights_out) else fast_frame()
  tau_df <- bind_rows(tau_out)
  prob_df <- bind_rows(prob_out)

  list(returns = returns_df, weights = weights_df, tau = tau_df, probabilities = prob_df,
       candidate_weights = candidate_weights_out[start_i:end_i],
       targets = db_targets[start_i:end_i,,drop=FALSE],
       decision_dates = as.Date(index(R_simple))[start_i:end_i],
       diagnostics = bind_rows(diag_out))
}
# Corrected mathematics, numerical optimization, and external comparators.
SIGNAL_LAG <- 1L
SIM_START <- as.Date("2016-01-01")
NO_CONTEXT <- FALSE
FAST_TEST <- FALSE
RISK_PAR <- list(dd=.30, cvar=.026, lookback=126L)
ASSET_NAMES <- character()
fast_frame <- function(..., stringsAsFactors=FALSE) {
  x<-list(...)
  if(!length(x)) return(structure(list(),class="data.frame",row.names=integer()))
  n<-max(lengths(x))
  if(any(!lengths(x)%in%c(1L,n))) stop("Invalid internal frame dimensions")
  for(j in seq_along(x)) if(length(x[[j]])==1L && n>1L) x[[j]]<-rep(x[[j]],n)
  structure(x,class="data.frame",row.names=.set_row_names(n))
}
bind_rows <- function(...) {
  x <- list(...); if(length(x)==1L && is.list(x[[1]]) && !is.data.frame(x[[1]])) x <- x[[1]]
  x <- Filter(function(z) NROW(z)>0L, x)
  if(!length(x)) return(data.frame())
  # Column-wise concatenation avoids quadratic row-name work for long backtests.
  if(all(vapply(x,function(z) identical(names(z),names(x[[1]])),logical(1)))) {
    ans<-lapply(seq_along(x[[1]]),function(j) {
      v<-unlist(lapply(x,function(z) z[[j]]),use.names=FALSE)
      if(inherits(x[[1]][[j]],"Date")) v<-as.Date(v,origin="1970-01-01")
      v
    })
    names(ans)<-names(x[[1]])
    return(structure(ans,class="data.frame",row.names=.set_row_names(sum(vapply(x,nrow,integer(1))))))
  }
  ans <- do.call(rbind, x); rownames(ans)<-NULL; ans
}
normalize_weights <- function(w, cap=1, eps=1e-12, max_iter=100) {
  # Normalize scores, then perform a true Euclidean box-simplex projection.
  w <- as.numeric(w)
  if(any(!is.finite(w))) stop("Nonfinite allocation, not silently replaced")
  w <- pmax(w,0); if(sum(w)<=eps) w[]<-1/length(w) else w<-w/sum(w)
  project_box(w, rep(cap,length(w)))
}
project_box <- function(v, upper=rep(1,length(v))) {
  if(length(upper)==1L) upper<-rep(upper,length(v))
  stopifnot(all(is.finite(v)),all(upper>=0),sum(upper)>=1-1e-10)
  if(all(v>=0)&&all(v<=upper)&&abs(sum(v)-1)<1e-12) return(as.numeric(v))
  lo<-min(v-upper)-1; hi<-max(v)+1
  for(k in 1:65) {mid<-(lo+hi)/2; if(sum(pmin(upper,pmax(v-mid,0)))>1) lo<-mid else hi<-mid}
  w<-pmin(upper,pmax(v-(lo+hi)/2,0))
  if(abs(sum(w)-1)>1e-9) stop("Projection failed")
  w
}
project_box_simplex <- function(v,cap=1) project_box(v,rep(cap,length(v)))
asset_upper <- function(names,cap=.35) {
  u<-rep(cap,length(names)); u[names %in% c("BTC","ETH")]<-.10; u
}
project_assets <- function(w, asset_names=ASSET_NAMES,cap=.35) {
  u<-asset_upper(asset_names,cap); ans<-project_box(w,u)
  ci<-which(asset_names %in% c("BTC","ETH"))
  if(length(ci)>1 && sum(ans[ci])>.15+1e-10) {
    # General two-crypto projection with the additional aggregate halfspace.
    p<-length(w); A<-cbind(rep(1,p),diag(p),-diag(p),-as.numeric(seq_len(p)%in%ci))
    ans<-quadprog::solve.QP(diag(p),as.numeric(w),A,c(1,rep(0,p),-u,-.15),meq=1)$solution
  }
  ans
}
apply_crypto_cap <- function(w,asset_names,total_cap=.15,single_cap=.10) {
  # No-crypto sample has exactly the same general constraint semantics.
  u<-rep(1,length(w)); u[asset_names %in% c("BTC","ETH")]<-single_cap
  out<-project_box(as.numeric(w),u)
  ci<-which(asset_names %in% c("BTC","ETH"))
  if(length(ci)>1 && sum(out[ci])>total_cap) {
    p<-length(w); A<-cbind(rep(1,p),diag(p),-diag(p),-as.numeric(seq_len(p)%in%ci))
    out<-quadprog::solve.QP(diag(p),as.numeric(w),A,c(1,rep(0,p),-u,-total_cap),meq=1)$solution
  }
  as.numeric(out)
}
max_drawdown_numeric <- function(r) {
  stopifnot(all(is.finite(r)), all(r>-1))
  w<-cumprod(c(1,1+r)); min(w/cummax(w)-1)
}
es_loss <- function(loss,alpha=.95) {
  # Exact empirical integral of the upper quantile: fractional tail mass.
  stopifnot(all(is.finite(loss)),length(loss)>0,alpha>0,alpha<1)
  z<-sort(as.numeric(loss),decreasing=TRUE); a<-(1-alpha)*length(z)
  k<-floor(a+1e-12); f<-a-k
  (if(k) sum(z[seq_len(k)]) else 0) / a + if(f>1e-10) f*z[k+1L]/a else 0
}
safe_cvar_loss <- function(r,alpha=.95) es_loss(-r,alpha)
execute_trade <- function(target,pre,asset_return,cost) {
  # Exact self-financing proportional cost. q is wealth remaining after trades.
  stopifnot(abs(sum(target)-1)<1e-8,abs(sum(pre)-1)<1e-8,
            all(target>=-1e-10),all(pre>=-1e-10),all(asset_return>-1),cost>=0,cost<.1)
  q<-1
  for(k in 1:50) {qn<-1-cost*sum(abs(q*target-pre)); if(abs(qn-q)<1e-14) {q<-qn;break}; q<-qn}
  turnover<-sum(abs(q*target-pre)); gross<-sum(target*asset_return)
  list(gross=gross,net=q*(1+gross)-1,cost=1-q,turnover=turnover,
       drift=target*(1+asset_return)/(1+gross))
}
w_inverse_vol <- function(R_hist,asset_names,cap=w_max_default) {
  s<-apply(R_hist,2,sd); s<-pmax(s,1e-6)
  project_assets((1/s)/sum(1/s),asset_names,cap)
}
w_minvar <- function(R_hist,asset_names,cap=w_max_default) {
  S<-cov(R_hist)+diag(1e-6,ncol(R_hist));p<-ncol(R_hist);u<-asset_upper(asset_names,cap)
  w<-quadprog::solve.QP(2*S,rep(0,p),cbind(rep(1,p),diag(p),-diag(p)),c(1,rep(0,p),-u),meq=1)$solution
  as.numeric(w)
}
make_db_selector_weights <- function(candidate_ret_hist,candidate_weights,asset_names,prev_weight,
                                     context_row,mode="main",mdd_budget=RISK_PAR$dd,
                                     cvar_budget=RISK_PAR$cvar,lookback=RISK_PAR$lookback) {
  W<-do.call(cbind,candidate_weights); nm<-names(candidate_weights)
  st<-candidate_history_stats(candidate_ret_hist,lookback)
  stress<-compute_stress_intensity(context_row)
  if(mode=="nostress") stress<-0
  bd<-max(.18,mdd_budget-.04*stress);bc<-max(.014,cvar_budget-.004*stress)
  if(mode=="strict") {bd<-.25;bc<-.021}
  if(mode=="tail") {bd<-mdd_budget;bc<-.021}
  if(mode=="growth") {bd<-.35;bc<-.034}
  if(mode=="norisks") {bd<-Inf;bc<-Inf}
  if(mode=="nodd") bd<-Inf
  if(mode=="nocvar") bc<-Inf
  if(is.null(st)) {
    fb<-intersect(c("SPY_QQQ_5050","QQQSoftGuard","FTRL_Online","TA_LAOPS"),nm)
    q<-setNames(rep(0,length(nm)),nm);q[fb]<-1/length(fb)
    return(list(w=project_assets(as.numeric(W%*%q),asset_names,.35),selected=paste(fb,collapse="+"),
                pass_count=NA_integer_,score=NA_real_,fallback=TRUE,budget_dd=bd,budget_cvar=bc))
  }
  stopifnot(identical(st$candidate,nm),all(complete.cases(st)))
  ddterm<-if(is.finite(bd)) pmax(-st$mdd-bd,0)/bd else 0
  cvterm<-if(is.finite(bc)) pmax(st$cvar-bc,0)/bc else 0
  st$score<-st$mean_log-.8*ddterm/252-1.5*cvterm/252-
    .05*pmax(st$ann_vol/max(median(st$ann_vol),1e-8)-1.5,0)/252-
    ifelse(nm %in% c("QQQ_BuyHold","SPY_QQQ_5050","QQQSoftGuard","DrawdownControlledQQQ"),stress*.00035,0)
  if(mode=="norisks") st$score<-st$mean_log
  if(mode=="growth" && stress<.35) st$score<-st$score+ifelse(nm%in%c("QQQ_BuyHold","SPY_QQQ_5050","QQQSoftGuard","FTRL_Online"),.00020,0)
  pass<-which(-st$mdd<=bd & st$cvar<=bc)
  fallback<-length(pass)<2L
  pool<-if(fallback||mode=="norisks") seq_along(nm) else pass
  top<-head(pool[order(-st$score[pool],pool)],3)
  q<-rep(0,length(nm));q[top]<-softmax_stable(db_eta*st$score[top])
  if(mode=="uniform") q[]<-1/length(q)
  if(mode=="budgetequal") {q[]<-0;q[pool]<-1/length(pool)}
  list(w=project_assets(as.numeric(W%*%q),asset_names,if(mode=="growth") .8 else .35),
       selected=paste(nm[q>0],collapse="+"),pass_count=length(pass),score=max(st$score[pool]),
       fallback=fallback,budget_dd=bd,budget_cvar=bc)
}
# Exact convex mean-variance with L1 cost-to-trade penalty (amortization horizon 21 days).
meanvar_cost <- function(R,pre,cost,gamma=10,horizon=21) {
  p<-ncol(R);S<-.9*cov(R)+.1*diag(diag(cov(R)))+diag(1e-7,p); mu<-colMeans(R)
  D<-diag(1e-9,2*p);D[1:p,1:p]<-gamma*S
  a<-c(mu,rep(-cost/horizon,p));Z<-matrix(0,p,p)
  A<-rbind(c(rep(1,p),rep(0,p)),cbind(diag(p),Z),cbind(-diag(p),Z),
           cbind(-diag(p),diag(p)),cbind(diag(p),diag(p)),cbind(Z,diag(p)))
  b<-c(1,rep(0,p),-asset_upper(colnames(R)), -pre,pre,rep(0,p))
  as.numeric(quadprog::solve.QP(D,a,t(A),b,meq=1)$solution[1:p])
}
# Rockafellar--Uryasev empirical mean-CVaR LP; additive CDaR LP for a second risk dimension.
risk_lp <- function(R,pre,cost,kind=c("cvar","cdar"),lambda=5,alpha=.95,horizon=21) {
  kind<-match.arg(kind);N<-nrow(R);p<-ncol(R);mu<-colMeans(R);u<-asset_upper(colnames(R))
  # Variables: weights, abs-trades, z_plus, z_minus, tail slacks, [running highs].
  iw<-1:p;iy<-p+1:p;izp<-2*p+1;izm<-2*p+2;it<-2*p+2+1:N
  ih<-if(kind=="cdar") 2*p+2+N+1:N else integer();nv<-2*p+2+N+length(ih)
  obj<-rep(0,nv);obj[iw]<- -mu;obj[iy]<-cost/horizon
  obj[izp]<-lambda;obj[izm]<- -lambda;obj[it]<-lambda/((1-alpha)*N)
  A<-list();di<-character();rhs<-numeric()
  add<-function(row,dir,b){A[[length(A)+1L]]<<-row;di<<-c(di,dir);rhs<<-c(rhs,b)}
  row<-numeric(nv);row[iw]<-1;add(row,"=",1)
  for(j in 1:p) {
    row<-numeric(nv);row[iw[j]]<-1;add(row,"<=",u[j])
    row<-numeric(nv);row[iy[j]]<-1;row[iw[j]]<- -1;add(row,">=",-pre[j])
    row<-numeric(nv);row[iy[j]]<-1;row[iw[j]]<-1;add(row,">=",pre[j])
  }
  if(kind=="cdar") {row<-numeric(nv);row[izm]<-1;add(row,"=",0);CR<-apply(R,2,cumsum)}
  for(t in 1:N) {
    row<-numeric(nv);row[it[t]]<-1;row[izp]<-1;row[izm]<- -1
    if(kind=="cvar") {row[iw]<-R[t,];add(row,">=",0)} else {
      row[iw]<-CR[t,];row[ih[t]]<- -1;add(row,">=",0)
      row<-numeric(nv);row[ih[t]]<-1;row[iw]<- -CR[t,];add(row,">=",0)
      if(t>1) {row<-numeric(nv);row[ih[t]]<-1;row[ih[t-1]]<- -1;add(row,">=",0)}
    }
  }
  ans<-lpSolve::lp("min",obj,do.call(rbind,A),di,rhs)
  if(ans$status!=0) stop(kind," LP failed with status ",ans$status,"; no silent substitute")
  list(w=project_assets(ans$solution[iw],colnames(R)),status=ans$status)
}
vol_match <- function(base,R,target) {
  safe<-rep(0,ncol(R));safe[match("SHY",colnames(R))]<-1
  S<-cov(R)*252; d<-base-safe
  a<-as.numeric(t(d)%*%S%*%d);b<-2*as.numeric(t(safe)%*%S%*%d);cc<-as.numeric(t(safe)%*%S%*%safe)
  roots<-numeric()
  if(a>1e-14) {disc<-b*b-4*a*(cc-target^2);if(disc>=0) roots<-(-b+c(-1,1)*sqrt(disc))/(2*a)}
  roots<-roots[roots>=0&roots<=1]
  if(length(roots)) z<-max(roots) else {
    cand<-unique(c(0,1,if(a>0) min(1,max(0,-b/(2*a))) else 0))
    z<-cand[which.min(abs(sqrt(pmax(a*cand^2+b*cand+cc,0))-target))]
  }
  w<-z*base+(1-z)*safe;v<-sqrt(max(as.numeric(t(w)%*%S%*%w),0))
  list(w=w,vol=v,attainable=abs(v-target)<1e-6,exposure=z)
}
external_weights <- function(R,experts,candidates,eh,dh,pre,state,day,cost,context) {
  W<-do.call(cbind,experts);K<-ncol(W);ns<-colnames(R)
  # Reconstruct scores from admissible history, never use the realized test-day return.
  # Cumulative Hedge uses net stand-alone expert histories for scoring only.
  if(nrow(eh)) {
    qh<-softmax_stable(colSums(log1p(eh)))
    if(day>1 && nrow(eh)>(state$seen %||% 0L)) {
      # Update each newly admissible observation once.
      seen<-state$seen %||% 0L
      for(k in seq.int(seen+1L,nrow(eh))) {
        x<-1+eh[k,];grad<- -x/sum(state$q*x)
        state$q<-project_box(state$q-.1/sqrt(k)*grad)
      }
      state$seen<-nrow(eh)
    }
  } else qh<-rep(1/K,K)
  ew<-list(Hedge_Experts=project_assets(as.numeric(W%*%qh),ns),
           OGD_Experts=project_assets(as.numeric(W%*%state$q),ns),
           TC_MeanVariance=meanvar_cost(R,pre$TC_MeanVariance,cost))
  # LPs are recomputed every 5 sessions and held without trading between updates.
  # The same schedule is pre-specified for both risk-optimization comparators.
  if(day==1 || (day-1)%%5==0) {
    cv<-risk_lp(R,pre$Mean_CVaR,cost,"cvar",lambda=5)
    cd<-risk_lp(R,pre$Mean_CDaR,cost,"cdar",lambda=1)
    ew$Mean_CVaR<-cv$w;ew$Mean_CDaR<-cd$w
    cvs<-cv$status;cds<-cd$status
  } else {ew$Mean_CVaR<-pre$Mean_CVaR;ew$Mean_CDaR<-pre$Mean_CDaR;cvs<-cds<-NA_integer_}
  target<-.10
  # Match the DB candidate portfolio's trailing virtual net return, stored separately below.
  if(!is.null(state$db_hist) && length(state$db_hist)>=63) target<-sd(tail(state$db_hist,126))*sqrt(252)
  bases<-list(QQQ=w_qqq_bh(ns),SPY=w_spy_bh(ns),Balanced=w_6040(ns),Equal=w_equal(ns))
  for(bn in names(bases)) {
    a<-vol_match(bases[[bn]],R,.10);b<-vol_match(bases[[bn]],R,target)
    ew[[paste0(bn,"_SHY_V10")]]<-a$w;ew[[paste0(bn,"_SHY_MatchDB")]]<-b$w
    if(bn=="QQQ") {mv<-b$vol;att<-b$attainable}
  }
  modes<-c(DB_UniformCandidates="uniform",DB_BudgetEqual="budgetequal",DB_NoTurnover="main",
           DB_NoStress="nostress",DB_NoDrawdown="nodd",DB_NoCVaR="nocvar")
  for(nm in names(modes)) {
    z<-make_db_selector_weights(dh,candidates,ns,pre[[nm]],context,mode=modes[[nm]])$w
    ew[[nm]]<-if(nm=="DB_NoTurnover") z else apply_turnover_control(z,pre[[nm]],db_no_trade_band,db_daily_turnover_cap)$w
  }
  list(weights=ew,state=state,target_vol=target,matched_vol=mv,attainable=att,cvar_status=cvs,cdar_status=cds)
}
`%||%` <- function(x,y) if(is.null(x)) y else x
# Reuse identical rolling candidate statistics within a decision; cache keys contain
# every observation in the window, so this optimization cannot cross data changes.
candidate_history_stats_uncached <- candidate_history_stats
.db_stats_cache <- new.env(parent=emptyenv())
candidate_history_stats <- function(candidate_ret_hist,lookback=db_lookback) {
  if(is.null(candidate_ret_hist)||nrow(candidate_ret_hist)<20) return(NULL)
  z<-tail(candidate_ret_hist,lookback)
  if(exists("returns",.db_stats_cache,inherits=FALSE)&&identical(z,.db_stats_cache$returns)) return(.db_stats_cache$stats)
  ans<-candidate_history_stats_uncached(z,lookback)
  .db_stats_cache$returns<-z;.db_stats_cache$stats<-ans;ans
}
# Data are immutable after the first successful download. No winsorization.
DB_DEFAULTS <- list(raw_start="2014-01-01",raw_end="2025-12-31",sim_start="2016-01-01",
                    eval_start="2020-01-01",eval_end="2025-12-31",signal_lag=1L,cost_bps=c(0,5,10,25),
                    main_cost=10,bootstrap_B=4999L,block_lengths=c(5L,20L,60L),seed=20261113L,
                    holdout_note="Retrospective walk-forward; 2016-2025 was already used for design.")
check_packages <- function(install=FALSE) {
  pk<-c("quantmod","xts","zoo","quadprog","lpSolve")
  missing<-pk[!vapply(pk,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing) && install) install.packages(missing,repos="https://cloud.r-project.org")
  missing<-pk[!vapply(pk,requireNamespace,logical(1),quietly=TRUE)]
  if(length(missing)) stop("Install required packages: ",paste(missing,collapse=", "))
  suppressPackageStartupMessages({library(xts);library(zoo)})
}
roll <- function(x,n,fun) as.numeric(zoo::rollapplyr(as.numeric(x),n,fun,fill=NA))
lag_vec <- function(x,k=1L) c(rep(NA,k),head(x,-k))
ffill <- function(x) zoo::na.locf(x,na.rm=FALSE)
rolling_z_past <- function(x,n=252L) {
  mu<-roll(x,n,mean);v<-roll(x,n,sd);(x-mu)/pmax(v,1e-8)
}
read_market <- function(data_dir,download=TRUE,settings=DB_DEFAULTS) {
  dir.create(data_dir,recursive=TRUE,showWarnings=FALSE)
  asset_map<-c(SPY="SPY",QQQ="QQQ",IWM="IWM",EFA="EFA",EEM="EEM",GLD="GLD",TLT="TLT",SHY="SHY",UUP="UUP",BTC="BTC-USD",VIX="^VIX")
  dl_log<-list()
  for(nm in names(asset_map)) {
    fp<-file.path(data_dir,paste0(nm,".csv"))
    if(file.exists(fp)) next
    if(!download) stop("Missing immutable data cache: ",fp)
    message("Downloading ",asset_map[[nm]])
    z<-tryCatch(quantmod::getSymbols(asset_map[[nm]],src="yahoo",from=settings$raw_start,
                to=as.Date(settings$raw_end)+1,auto.assign=FALSE),error=function(e) stop("Yahoo download failed for ",nm,": ",conditionMessage(e),". No synthetic fallback; retry later or supply cached CSV."))
    # ETFs/Bitcoin require Adjusted; VIX uses its index Close. No silent field substitution.
    px<-if(nm=="VIX") quantmod::Cl(z) else quantmod::Ad(z)
    if(!NROW(px)||all(!is.finite(px))) stop("No valid adjusted price for ",nm)
    write.csv(data.frame(date=as.Date(index(px)),price=as.numeric(px)),fp,row.names=FALSE)
    dl_log[[nm]]<-data.frame(series=nm,source="Yahoo Finance",field=if(nm=="VIX") "Close" else "Adjusted",retrieved_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
  }
  for(nm in c("DGS3MO","DGS10")) {
    fp<-file.path(data_dir,paste0(nm,".csv"));if(file.exists(fp)) next
    if(!download) stop("Missing risk-free/context cache: ",fp)
    z<-tryCatch(quantmod::getSymbols(nm,src="FRED",auto.assign=FALSE),error=function(e) stop("FRED download failed: ",conditionMessage(e)))
    z<-z[paste0(settings$raw_start,"/",settings$raw_end)]
    write.csv(data.frame(date=as.Date(index(z)),price=as.numeric(z)),fp,row.names=FALSE)
    dl_log[[nm]]<-data.frame(series=nm,source="Federal Reserve H.15 via FRED",field="annual percent investment-basis yield",retrieved_utc=format(Sys.time(),tz="UTC",usetz=TRUE))
  }
  if(length(dl_log)) write.csv(bind_rows(dl_log),file.path(data_dir,"download_manifest.csv"),row.names=FALSE)
  read_one<-function(nm) {
    a<-read.csv(file.path(data_dir,paste0(nm,".csv")),check.names=FALSE)
    if(!all(c("date","price")%in%names(a))) stop(nm," CSV must have date,price")
    a$date<-as.Date(a$date);a<-a[order(a$date),]
    stopifnot(!anyDuplicated(a$date),!anyNA(a$date))
    a[a$date>=as.Date(settings$raw_start)&a$date<=as.Date(settings$raw_end),]
  }
  sp<-read_one("SPY");dates<-sp$date
  P<-sapply(names(asset_map),function(nm){a<-read_one(nm);a$price[match(dates,a$date)]})
  if(any(!is.finite(P[,setdiff(colnames(P),"BTC"),drop=FALSE]))) stop("Missing equity/VIX observation; inspect cache, do not silently compress calendar")
  if(any(P[is.finite(P)]<=0)) stop("Nonpositive prices")
  rf<-read_one("DGS3MO");y10<-read_one("DGS10")
  align_yield<-function(a) {
    ix<-findInterval(dates,a$date);v<-rep(NA,length(dates));v[ix>0]<-ffill(a$price)[ix[ix>0]];v
  }
  y3<-align_yield(rf);y10<-align_yield(y10)
  if(anyNA(y3)||anyNA(y10)) stop("Treasury history must cover raw start; supply earlier quote if needed")
  make_market(P,dates,y3,y10)
}
make_market <- function(P,dates,y3,y10) {
  assets<-setdiff(colnames(P),"VIX");n<-nrow(P)
  R<-P[-1,assets,drop=FALSE]/P[-n,assets,drop=FALSE]-1;d<-dates[-1]
  # Yield at the preceding close is conservatively delayed one additional trading session.
  # Simple annual investment-basis quote, ACT/365 carry approximation, not a traded bill return.
  rf<-lag_vec(y3,1)[-n]/100*as.numeric(diff(dates))/365
  r<-c(NA,diff(log(P[,"SPY"])));vix<-P[,"VIX"]
  dd<-roll(c(0,r[-1]),252,function(z){w<-cumprod(c(1,exp(z)));tail(w,1)/max(w)-1})
  rv22<-roll(r,22,sd)*sqrt(252);dv<-c(NA,diff(log(vix)))
  S<-pmax(rolling_z_past(log(vix)),0)+pmax(rolling_z_past(abs(dv)),0)+
     pmax(rolling_z_past(rv22),0)+pmax(rolling_z_past(-dd),0)
  X<-cbind(log_VIX=log(vix),d_log_VIX=dv,Treasury10Y_pct=y10,d_Treasury10Y_pct=c(NA,diff(y10)),
           SPY_mom_5=roll(r,5,sum),SPY_mom_22=roll(r,22,sum),SPY_mom_63=roll(r,63,sum),
           SPY_rv_5=roll(r,5,sd)*sqrt(252),SPY_rv_22=rv22,SPY_drawdown_252=dd,regime_shift_score=S)
  eq<-c("SPY","QQQ","IWM","EFA","EEM");eR<-rbind(NA,log(P[-1,eq,drop=FALSE]/P[-n,eq,drop=FALSE]))
  X<-cbind(X,cross_disp_22=roll(apply(eR,1,sd),22,mean))
  for(nm in c("TLT","GLD","UUP")) {
    rr<-c(NA,diff(log(P[,nm])));z<-rep(NA,n)
    for(i in 63:n) z[i]<-cor(r[(i-62):i],rr[(i-62):i])
    X<-cbind(X,z);colnames(X)[ncol(X)]<-paste0("corr_SPY_",nm,"_63")
  }
  rg<-ifelse(vix<15,"Calm",ifelse(vix<=25,"Normal","Stress"))
  list(R=xts::xts(R,order.by=d),X=xts::xts(X[-1,,drop=FALSE],order.by=d),
       regime=xts::xts(rg[-1],order.by=d),rf=data.frame(date=d,rf=rf),
       prices=data.frame(date=dates,P),yield=data.frame(date=dates,DGS3MO=y3,DGS10=y10))
}
synthetic_market <- function(n=900,seed=998) {
  # Labeled mechanical validation fixture; NEVER used as empirical financial evidence.
  set.seed(seed);d<-seq(as.Date("2014-01-01"),by="day",length.out=ceiling(n*1.6))
  d<-d[as.integer(format(d,"%u"))<=5][seq_len(n)]
  nm<-c("SPY","QQQ","IWM","EFA","EEM","GLD","TLT","SHY","UUP","BTC")
  common<-rnorm(n,0,.006);sig<-c(.007,.009,.010,.008,.010,.007,.008,.001,.003,.025)
  RR<-sapply(seq_along(nm),function(j) .00015+(.8-(j>5)*.6)*common+rnorm(n,0,sig[j]))
  if(n>=620) RR[seq(620,min(n,630)),1:5]<-RR[seq(620,min(n,630)),1:5]-.015
  P<-100*apply(1+RR,2,cumprod);colnames(P)<-nm
  P<-cbind(P,VIX=18+50*abs(common)+3*sin(seq_len(n)/25))
  make_market(P,d,rep(2,n),rep(3,n))
}
performance <- function(r,rf,turnover=NULL) {
  stopifnot(length(r)==length(rf),all(is.finite(r)),all(is.finite(rf)),all(r>-1))
  e<-r-rf;n<-length(r);w<-cumprod(c(1,1+r));dd<-1-w/cummax(w);sg<-sd(e)
  ann<-expm1(252*mean(log1p(r)));md<-max(dd);down<-sqrt(mean(pmin(e,0)^2))
  c(n=n,ann_return=ann,ann_vol=sd(r)*sqrt(252),sharpe=if(sg>1e-12) sqrt(252)*mean(e)/sg else NA_real_,
    sortino=if(down>1e-12) sqrt(252)*mean(e)/down else NA_real_,mdd=md,
    cvar95=es_loss(-r,.95),cvar99=es_loss(-r,.99),cdar95=es_loss(dd[-1],.95),
    ulcer=sqrt(mean(dd[-1]^2)),calmar=if(md>0) ann/md else NA_real_,
    log_growth=252*mean(log1p(r)),ce2=252*(mean(e)-var(e)),ce5=252*(mean(e)-2.5*var(e)),
    ce10=252*(mean(e)-5*var(e)),final_wealth=tail(w,1),
    avg_turnover=if(is.null(turnover)) NA_real_ else mean(turnover))
}
perf_table <- function(df,rf,start,end) {
  df<-df[df$date>=as.Date(start)&df$date<=as.Date(end),]
  groups<-split(df,interaction(df$sample,df$model,drop=TRUE));out<-list()
  for(z in groups) {
    z<-z[order(z$date),];fr<-rf$rf[match(z$date,rf$date)]
    if(anyNA(fr)) stop("Missing risk-free observations in evaluation")
    out[[length(out)+1L]]<-cbind(data.frame(sample=z$sample[1],model=z$model[1],start=min(z$date),end=max(z$date)),
                               as.data.frame(as.list(performance(z$net_return,fr,z$turnover))))
  }
  bind_rows(out)
}
hac_se <- function(x,lag=NULL) {
  n<-length(x);if(is.null(lag)) lag<-floor(4*(n/100)^(2/9));lag<-min(lag,n-2L)
  z<-x-mean(x);v<-sum(z*z)/n
  if(lag>0) for(k in seq_len(lag)) v<-v+2*(1-k/(lag+1))*sum(z[(k+1):n]*z[1:(n-k)])/n
  sqrt(max(v,0)/n)
}
sharpe_diff_se <- function(a,b) {
  # Delta-method influence functions; arithmetic excess-return Sharpe, annualized.
  inf<-function(x) {mu<-mean(x);s<-sqrt(mean((x-mu)^2));if(s<1e-10)return(rep(NA,length(x)))
    sqrt(252)*((x-mu)/s-mu*((x-mu)^2-s^2)/(2*s^3))}
  hac_se(inf(a)-inf(b))
}
circular_indices <- function(n,L) {
  starts<-sample.int(n,ceiling(n/L),replace=TRUE)
  head(as.vector(t(outer(starts-1,0:(L-1),"+")%%n+1L)),n)
}
paired_inference <- function(a,b,rf,B=4999L,L=20L,seed=123L) {
  keep<-c("sharpe","log_growth","ce5","cvar95","mdd","cdar95")
  fast_metric<-function(r,rf) {
    e<-r-rf;v<-var(e);w<-cumprod(c(1,1+r));dd<-(1-w/cummax(w))[-1]
    c(sharpe=sqrt(252)*mean(e)/sqrt(v),log_growth=252*mean(log1p(r)),
      ce5=252*(mean(e)-2.5*v),cvar95=es_loss(-r),mdd=max(dd),cdar95=es_loss(dd))
  }
  metric<-function(a,b,rf) fast_metric(a,rf)-fast_metric(b,rf)
  hat<-metric(a,b,rf);n<-length(a);boot<-matrix(NA_real_,B,length(keep),dimnames=list(NULL,keep))
  ts<-numeric(B);se<-sharpe_diff_se(a-rf,b-rf);set.seed(seed)
  for(j in seq_len(B)) {
    ix<-circular_indices(n,L);boot[j,]<-metric(a[ix],b[ix],rf[ix])
    s<-sharpe_diff_se(a[ix]-rf[ix],b[ix]-rf[ix])
    ts[j]<-if(is.finite(s)&&s>1e-12) (boot[j,"sharpe"]-hat["sharpe"])/s else NA_real_
  }
  out<-data.frame(metric=keep,difference=as.numeric(hat),lower=NA_real_,upper=NA_real_,p_value=NA_real_,B=B,block_length=L)
  for(k in seq_along(keep)) {
    err<-boot[,k]-hat[k];qs<-quantile(err,c(.025,.975),na.rm=TRUE)
    out$lower[k]<-hat[k]-qs[2];out$upper[k]<-hat[k]-qs[1]
    out$p_value[k]<-(1+sum(abs(err)>=abs(hat[k])))/(B+1)
  }
  if(is.finite(se)&&se>1e-12&&sum(is.finite(ts))>.95*B) {
    q<-quantile(ts,c(.025,.975),na.rm=TRUE);i<-match("sharpe",keep)
    out$lower[i]<-hat[i]-q[2]*se;out$upper[i]<-hat[i]-q[1]*se
    out$p_value[i]<-(1+sum(abs(ts)>=abs(hat[i]/se),na.rm=TRUE))/(1+sum(is.finite(ts)))
  }
  out$method<-ifelse(out$metric=="sharpe","studentized circular block bootstrap with HAC influence-function SE",
                    "centered circular block bootstrap; approximate fixed-rule inference")
  out$interpretation<-ifelse(out$metric %in% c("mdd","cdar95"),"exploratory path-statistic uncertainty; not a future drawdown guarantee","conditional on fixed backtest rules; not selection-adjusted")
  out
}
# Reproduction workflow: fixed-rule historical test, risk matching, retrospective walk-forward.
run_engine <- function(m,sample="main",cost_bps=10,no_context=FALSE,lag=1L,keep_weights=FALSE,sim_start=as.Date("2016-01-01")) {
  env<-environment(run_ta_laops_backtest)
  assign("NO_CONTEXT",no_context,env);assign("SIGNAL_LAG",as.integer(lag),env);assign("SIM_START",as.Date(sim_start),env)
  R<-m$R
  if(sample=="no_crypto") R<-R[,!colnames(R)%in%c("BTC","ETH"),drop=FALSE]
  R<-na.omit(R);assign("ASSET_NAMES",colnames(R),env)
  bt<-run_ta_laops_backtest(R,m$X,m$regime,label=sample,tcost_bps=cost_bps,keep_weights=keep_weights)
  bt$settings<-list(sample=sample,cost_bps=cost_bps,no_context=no_context,lag=lag,sim_start=sim_start)
  bt
}
replay_targets <- function(targets,dates,R,cost_bps=10,model="DB_WalkForward",sample="main",control=TRUE) {
  p<-ncol(R);pre<-rep(1/p,p);out<-vector("list",nrow(targets));we<-matrix(NA_real_,nrow(targets),p)
  for(t in seq_len(nrow(targets))) {
    i<-match(dates[t],as.Date(index(R)));stopifnot(!is.na(i),i<nrow(R))
    raw<-targets[t,];w<-if(control) apply_turnover_control(raw,pre,db_no_trade_band,db_daily_turnover_cap)$w else raw
    w<-project_assets(w,colnames(R));tr<-execute_trade(w,pre,as.numeric(R[i+1,]),cost_bps/10000)
    out[[t]]<-data.frame(date=as.Date(index(R)[i+1]),decision_date=dates[t],model=model,sample=sample,
                        gross_return=tr$gross,net_return=tr$net,turnover=tr$turnover,cost_fraction=tr$cost)
    we[t,]<-w;pre<-tr$drift
  }
  list(returns=bind_rows(out),weights=we)
}
walkforward <- function(bt,m,outdir,start="2020-01-01",end="2025-12-31") {
  ns<-names(bt$candidate_weights[[1]]);N<-length(bt$candidate_weights);dates<-bt$decision_dates
  asset_names<-colnames(bt$targets);R<-na.omit(m$R[,asset_names,drop=FALSE])
  hist<-sapply(ns,function(nm) bt$returns$net_return[bt$returns$model==nm])
  stopifnot(nrow(hist)==N)
  # Original center retained; grid fixed before inspecting revised OOS results.
  grid<-expand.grid(dd=c(.25,.30,.35),cvar=c(.021,.026,.034))
  grid$id<-sprintf("D%03d_C%04d",round(100*grid$dd),round(10000*grid$cvar))
  paths<-list();targets<-list();base<-bt$returns[bt$returns$model=="DB_LAOPS",]
  for(g in seq_len(nrow(grid))) {
    T<-matrix(NA_real_,N,length(asset_names));colnames(T)<-asset_names
    for(t in seq_len(N)) {
      nh<-max(0L,t-1L-bt$settings$lag)
      H<-hist[seq_len(nh),,drop=FALSE]
      i<-match(dates[t],as.Date(index(R)));sdate<-index(R)[i-bt$settings$lag]
      X<-m$X[sdate]
      T[t,]<-make_db_selector_weights(H,bt$candidate_weights[[t]],asset_names,rep(1/length(asset_names),length(asset_names)),X,
                       mode="main",mdd_budget=grid$dd[g],cvar_budget=grid$cvar[g],lookback=126)$w
    }
    z<-replay_targets(T,dates,R,bt$settings$cost_bps,grid$id[g],bt$settings$sample)
    paths[[g]]<-z$returns;targets[[g]]<-T
  }
  center<-which(grid$dd==.30&grid$cvar==.026)
  delta<-max(abs(paths[[center]]$net_return-base$net_return))
  if(delta>1e-8) stop("Replay accounting disagrees with baseline: ",delta)
  chosen<-rep(center,N);audit<-list()
  # At each January, select using previous two calendar years' CE5 only.
  years<-seq(as.integer(format(as.Date(start),"%Y")),as.integer(format(as.Date(end),"%Y")))
  for(y in years) {
    cutoff<-as.Date(paste0(y,"-01-01"));lo<-as.Date(paste0(y-2,"-01-01"))
    # Lag applies to selection as well as daily allocation signals.
    first_target<-which(base$date>=cutoff)[1]
    if(is.na(first_target)) next
    si<-match(dates[first_target],as.Date(index(R)))-bt$settings$lag
    last_known<-as.Date(index(R)[si]);hi<-min(cutoff-1,last_known)
    ix<-which(base$date>=lo & base$date<=hi)
    if(length(ix)<252) stop("Insufficient inner-validation history for ",y)
    fr<-m$rf$rf[match(base$date[ix],m$rf$date)]
    scores<-vapply(paths,function(z) unname(performance(z$net_return[ix],fr)["ce5"]),numeric(1))
    # Deterministic tie break: closest to original center, then fixed grid order.
    best<-which(scores>=max(scores)-1e-12);best<-best[order(abs(grid$dd[best]-.30)+abs(grid$cvar[best]-.026),best)][1]
    use<-which(base$date>=cutoff & base$date<as.Date(paste0(y+1,"-01-01")))
    chosen[use]<-best
    audit[[length(audit)+1L]]<-data.frame(test_year=y,validation_start=min(base$date[ix]),
       validation_end=max(base$date[ix]),last_known=last_known,n_validation=length(ix),selected_id=grid$id[best],
       dd=grid$dd[best],cvar=grid$cvar[best],inner_ce5=scores[best],criterion="CE5; no test-year optimization")
  }
  T<-bt$targets
  for(t in seq_len(N)) T[t,]<-targets[[chosen[t]]][t,]
  wf<-replay_targets(T,dates,R,bt$settings$cost_bps,"DB_WalkForward",bt$settings$sample)
  all<-bind_rows(paths);write.csv(grid,file.path(outdir,"budget_grid.csv"),row.names=FALSE)
  write.csv(bind_rows(audit),file.path(outdir,paste0("walkforward_choices_",bt$settings$sample,".csv")),row.names=FALSE)
  write.csv(perf_table(all,m$rf,start,end),file.path(outdir,paste0("budget_sensitivity_",bt$settings$sample,".csv")),row.names=FALSE)
  write.csv(data.frame(sample=bt$settings$sample,max_replay_error=delta),file.path(outdir,paste0("replay_check_",bt$settings$sample,".csv")),row.names=FALSE)
  list(returns=wf$returns,choices=bind_rows(audit),grid=grid)
}
regime_table <- function(df,rf,start,end) {
  df<-df[df$date>=as.Date(start)&df$date<=as.Date(end),];out<-list()
  for(z in split(df,interaction(df$sample,df$model,df$regime,drop=TRUE))) {
    e<-z$net_return-rf$rf[match(z$date,rf$date)]
    # No concatenated regime-specific wealth or drawdown paths.
    out[[length(out)+1L]]<-data.frame(sample=z$sample[1],model=z$model[1],regime=z$regime[1],n=nrow(z),
      mean_daily_excess=mean(e),sd_daily_excess=sd(e),daily_sharpe=mean(e)/sd(e),cvar95=es_loss(-z$net_return))
  }
  bind_rows(out)
}
write_figures <- function(df,rf,outdir,start,end) {
  models<-c("DB_LAOPS","DB_WalkForward","QQQ_SHY_MatchDB","TC_MeanVariance","Hedge_Experts","Mean_CVaR","Mean_CDaR")
  for(sample in unique(df$sample)) {
    z<-df[df$sample==sample & df$date>=as.Date(start)&df$date<=as.Date(end)&df$model%in%models,]
    sets<-split(z,z$model);if(!length(sets))next
    f<-file.path(outdir,paste0("wealth_drawdown_",sample,".pdf"));grDevices::cairo_pdf(f,10,7);par(mfrow=c(2,1),mar=c(3,4,2,1))
    cols<-c("#173F35","#237B9A","#BF5B17","#545B66","#8B4A8B","#6C8318","#B34D67")[seq_along(sets)];mx<-max(vapply(sets,function(x) max(cumprod(1+x$net_return)),numeric(1)))
    plot(range(z$date),c(.5,mx),type="n",xlab="",ylab="Net wealth",main=if(sample=="main") "Bitcoin-inclusive evaluation" else "ETF-only evaluation")
    for(k in seq_along(sets)) lines(sets[[k]]$date,cumprod(1+sets[[k]]$net_return),col=cols[k],lwd=1.7)
    labs<-c(DB_LAOPS="DB-LAOPS",DB_WalkForward="Annual walk-forward",QQQ_SHY_MatchDB="QQQ/SHY matched",TC_MeanVariance="Cost-aware mean-variance",Hedge_Experts="Hedge experts",Mean_CVaR="Mean-CVaR",Mean_CDaR="Mean-CDaR")
    legend("topleft",labs[names(sets)],col=cols,lty=1,cex=.65,bty="n",ncol=2)
    plot(range(z$date),c(-.3,0),type="n",xlab="",ylab="Drawdown")
    for(k in seq_along(sets)){v<-cumprod(c(1,1+sets[[k]]$net_return));lines(sets[[k]]$date,(v/cummax(v)-1)[-1],col=cols[k],lwd=1.7)}
    dev.off()
  }
}
latex_table <- function(df,file,caption) {
  # Numeric outputs use fixed column names; escape all user-visible labels.
  esc<-function(x) gsub("_"," ",as.character(x),fixed=TRUE)
  x<-df
  for(j in seq_along(x)) x[[j]]<-if(is.numeric(x[[j]])) formatC(x[[j]],format="f",digits=4) else esc(x[[j]])
  rows<-apply(as.matrix(x),1,paste,collapse=" & ")
  writeLines(c("\\begin{table}[htbp]","\\centering\\scriptsize",paste0("\\caption{",caption,"}"),
    "\\begin{adjustbox}{max width=\\linewidth}",paste0("\\begin{tabular}{",paste(rep("l",ncol(df)),collapse=""),"}"),"\\hline",
    paste0(paste(esc(names(df)),collapse=" & ")," \\\\"),"\\hline",paste0(rows," \\\\"),"\\hline","\\end{tabular}","\\end{adjustbox}","\\end{table}"),file)
}
primary_benchmarks <- c("QQQ_SHY_MatchDB","SPY_SHY_MatchDB","Balanced_SHY_MatchDB","Equal_SHY_MatchDB",
                        "Hedge_Experts","OGD_Experts","TC_MeanVariance","Mean_CVaR","Mean_CDaR")
db_main <- function(mode=c("full","review","smoke"),root=getwd(),download=TRUE,install=FALSE,settings=DB_DEFAULTS) {
  mode<-match.arg(mode);check_packages(install)
  root<-normalizePath(root,mustWork=TRUE);outdir<-file.path(root,paste0("outputs_",mode));dir.create(outdir,recursive=TRUE,showWarnings=FALSE)
  for(d in c("tables","figures","diagnostics","checkpoints","daily")) dir.create(file.path(outdir,d),showWarnings=FALSE)
  if(mode=="smoke") {settings$bootstrap_B<-99L;settings$block_lengths<-20L;settings$cost_bps<-10
    settings$eval_start<-"2016-06-01";settings$eval_end<-"2017-12-31";m<-synthetic_market(820)
  } else m<-read_market(file.path(root,"data","source_cache"),download,settings)
  if(mode=="review") {settings$bootstrap_B<-999L;settings$block_lengths<-20L;settings$cost_bps<-10}
  set.seed(settings$seed)
  dput(settings,file=file.path(outdir,"protocol_locked.R"));capture.output(sessionInfo(),file=file.path(outdir,"sessionInfo.txt"))
  saveRDS(m,file.path(outdir,"market_snapshot.rds"))
  write.csv(m$rf,file.path(outdir,"daily","risk_free_carry.csv"),row.names=FALSE)
  input_files<-list.files(file.path(root,"data","source_cache"),full.names=TRUE)
  if(length(input_files)) write.csv(data.frame(file=basename(input_files),md5=unname(tools::md5sum(input_files))),file.path(outdir,"input_checksums.csv"),row.names=FALSE)
  # Cache is guarded by settings, complete inputs and source checksum.
  code_file<-file.path(root,"DB_LAOPS_v90_MAJOR_REVISION.R")
  signature<-list(settings=settings,inputs=if(length(input_files)) tools::md5sum(input_files) else "synthetic",
                  code=if(file.exists(code_file)) unname(tools::md5sum(code_file)) else "development")
  cache_run<-function(sample,cost,noctx=FALSE,lag=settings$signal_lag) {
    tag<-paste(sample,cost,if(noctx)"noctx" else "ctx",lag,sep="_");fp<-file.path(outdir,"checkpoints",paste0(tag,".rds"))
    if(file.exists(fp)) {zz<-readRDS(fp);if(identical(zz$signature,signature)) {message("Resuming ",tag);return(zz$bt)}}
    message("Running ",tag)
    z<-run_engine(m,sample,cost,noctx,lag,keep_weights=cost==settings$main_cost&&!noctx&&lag==settings$signal_lag,
                  sim_start=as.Date(settings$sim_start))
    saveRDS(list(signature=signature,bt=z),fp,compress=TRUE);z
  }
  all<-list();tests<-list();secondary<-list();allcost<-list();yearly<-list();noctxout<-list();factorial<-list()
  samples<-if(mode=="smoke") "main" else c("main","no_crypto")
  for(s in samples) {
    bt<-cache_run(s,settings$main_cost);df<-bt$returns
    stopifnot(all(df$signal_date<df$decision_date),all(df$decision_date<df$date))
    write.csv(df,file.path(outdir,"daily",paste0("returns_",s,".csv")),row.names=FALSE)
    write.csv(perf_table(df,m$rf,min(df$date),max(df$date)),file.path(outdir,"tables",paste0("simulation_performance_",s,".csv")),row.names=FALSE)
    write.csv(bt$weights,file.path(outdir,"daily",paste0("weights_",s,".csv")),row.names=FALSE)
    write.csv(bt$diagnostics,file.path(outdir,"diagnostics",paste0("decisions_",s,".csv")),row.names=FALSE)
    write.csv(bt$tau,file.path(outdir,"diagnostics",paste0("trust_",s,".csv")),row.names=FALSE)
    write.csv(bt$probabilities,file.path(outdir,"diagnostics",paste0("probabilities_",s,".csv")),row.names=FALSE)
    if(mode!="smoke") {
      wf<-walkforward(bt,m,file.path(outdir,"tables"),settings$eval_start,settings$eval_end)
      # Use only shared columns when adding the replay path to summary metrics.
      cols<-intersect(names(df),names(wf$returns));summarydf<-rbind(df[,cols],wf$returns[,cols])
      nctx<-cache_run(s,settings$main_cost,TRUE);zz<-nctx$returns[nctx$returns$model=="DB_LAOPS",cols];zz$model<-"DB_UniformContext"
      summarydf<-rbind(summarydf,zz)
      write.csv(wf$returns,file.path(outdir,"daily",paste0("walkforward_returns_",s,".csv")),row.names=FALSE)
      write.csv(zz,file.path(outdir,"daily",paste0("uniform_context_returns_",s,".csv")),row.names=FALSE)
      write.csv(matching_diagnostics(bt,m),file.path(outdir,"diagnostics",paste0("risk_matching_",s,".csv")),row.names=FALSE)
      write.csv(budget_activation(bt,m),file.path(outdir,"diagnostics",paste0("budget_activation_",s,".csv")),row.names=FALSE)
      write.csv(mechanism_diagnostics(bt,nctx,settings$eval_start,settings$eval_end),file.path(outdir,"diagnostics",paste0("mechanism_identity_",s,".csv")),row.names=FALSE)
      da<-df[df$model=="DB_LAOPS"&df$date>=as.Date(settings$eval_start)&df$date<=as.Date(settings$eval_end),]
      frr<-m$rf$rf[match(da$date,m$rf$date)]
      for(bm in c("DB_NoRiskBudget","DB_NoDrawdown","DB_NoCVaR","DB_UniformCandidates","DB_BudgetEqual","DB_UniformContext")) {
        db<-if(bm=="DB_UniformContext")nctx$returns[nctx$returns$model=="DB_LAOPS"&nctx$returns$date>=min(da$date)&nctx$returns$date<=max(da$date),] else df[df$model==bm&df$date>=min(da$date)&df$date<=max(da$date),]
        stopifnot(identical(da$date,db$date))
        si<-paired_inference(da$net_return,db$net_return,frr,settings$bootstrap_B,20L,settings$seed)
        secondary[[length(secondary)+1L]]<-cbind(data.frame(sample=s,target="DB_LAOPS",benchmark=bm),si)
      }
      # 2x2 mechanism ablation: contextual vs uniform advice; budget layer on/off.
      for(cx in c("context","uniform")) for(risk in c("on","off")) {
        src<-if(cx=="context")df else nctx$returns
        mod<-if(risk=="on")"DB_LAOPS" else "DB_NoRiskBudget"
        z<-src[src$model==mod,cols];z$model<-paste("Factor",cx,risk,sep="_")
        factorial[[length(factorial)+1L]]<-z
      }
    } else summarydf<-df
    all[[s]]<-summarydf
    eval<-df[df$date>=as.Date(settings$eval_start)&df$date<=as.Date(settings$eval_end),]
    td<-eval[eval$model=="DB_LAOPS",];fr<-m$rf$rf[match(td$date,m$rf$date)]
    refs<-if(mode=="smoke") head(primary_benchmarks,1) else primary_benchmarks
    for(bm in refs) {
      b<-eval[eval$model==bm,];stopifnot(identical(td$date,b$date))
      for(L in settings$block_lengths) {
        message("Inference ",s," / ",bm," / L=",L)
        ti<-paired_inference(td$net_return,b$net_return,fr,settings$bootstrap_B,L,settings$seed)
        tests[[length(tests)+1L]]<-cbind(data.frame(sample=s,target="DB_LAOPS",benchmark=bm),ti)
      }
    }
    for(cost in settings$cost_bps) {
      z<-if(cost==settings$main_cost)bt else cache_run(s,cost)
      a<-perf_table(z$returns,m$rf,settings$eval_start,settings$eval_end);a$tcost_bps<-cost
      allcost[[length(allcost)+1L]]<-a
    }
    write.csv(regime_table(df,m$rf,settings$eval_start,settings$eval_end),file.path(outdir,"tables",paste0("regime_conditional_",s,".csv")),row.names=FALSE)
    for(y in unique(format(summarydf$date,"%Y"))) {
      if(y<substr(settings$eval_start,1,4))next
      a<-perf_table(summarydf,m$rf,paste0(y,"-01-01"),paste0(y,"-12-31"));a$year<-y;yearly[[length(yearly)+1L]]<-a
    }
    if(mode=="full") {
      lag2<-cache_run(s,settings$main_cost,lag=2L)
      write.csv(perf_table(lag2$returns,m$rf,settings$eval_start,settings$eval_end),file.path(outdir,"tables",paste0("lag2_sensitivity_",s,".csv")),row.names=FALSE)
    }
  }
  all<-bind_rows(all);pt<-perf_table(all,m$rf,settings$eval_start,settings$eval_end);ti<-bind_rows(tests)
  # Holm family is all pre-specified sample x benchmark x metric tests at the primary L=20.
  ti$p_holm_primary<-NA_real_;ix<-which(ti$block_length==20 & ti$metric%in%c("sharpe","log_growth","ce5","cvar95"))
  ti$p_holm_primary[ix]<-p.adjust(ti$p_value[ix],method="holm")
  ti$fee_equivalent_bps<-ifelse(ti$metric=="ce5",1e4*ti$difference,NA_real_)
  write.csv(pt,file.path(outdir,"tables","performance.csv"),row.names=FALSE)
  write.csv(ti,file.path(outdir,"tables","paired_inference.csv"),row.names=FALSE)
  if(length(secondary)) {
    si<-bind_rows(secondary);si$p_holm_secondary<-NA_real_
    ix<-which(si$metric%in%c("sharpe","log_growth","ce5","cvar95"))
    si$p_holm_secondary[ix]<-p.adjust(si$p_value[ix],method="holm")
    write.csv(si,file.path(outdir,"tables","mechanism_inference.csv"),row.names=FALSE)
  }
  write.csv(bind_rows(allcost),file.path(outdir,"tables","cost_sensitivity.csv"),row.names=FALSE)
  write.csv(bind_rows(yearly),file.path(outdir,"tables","yearly_performance.csv"),row.names=FALSE)
  if(length(factorial)) write.csv(perf_table(bind_rows(factorial),m$rf,settings$eval_start,settings$eval_end),file.path(outdir,"tables","factorial_mechanisms.csv"),row.names=FALSE)
  write_figures(all,m$rf,file.path(outdir,"figures"),settings$eval_start,settings$eval_end)
  main<-pt[pt$model %in% c("DB_LAOPS","DB_WalkForward",primary_benchmarks),c("sample","model","ann_return","sharpe","mdd","cvar95","ce5")]
  latex_table(main,file.path(outdir,"tables","performance.tex"),"Recomputed net performance; retrospective evaluation.")
  writeLines(c(paste("COMPLETED MODE",mode),paste("Bootstrap replications",settings$bootstrap_B),
               paste("Costs",paste(settings$cost_bps,collapse=",")),paste("Block lengths",paste(settings$block_lengths,collapse=",")),
               if(mode=="smoke")"SYNTHETIC VALIDATION ONLY; NO EMPIRICAL FINDING" else settings$holdout_note),file.path(outdir,"RUN_STATUS.txt"))
  message("Finished: ",outdir)
  invisible(list(performance=pt,inference=ti,output=outdir))
}
# Independent diagnostic reconstruction from saved allocations.
matching_diagnostics <- function(bt,m) {
  nm<-c("QQQ","SPY","Balanced","Equal");out<-list();R<-na.omit(m$R[,colnames(bt$targets),drop=FALSE])
  for(bn in nm) {
    model<-paste0(bn,"_SHY_MatchDB")
    for(t in seq_along(bt$decision_dates)) {
      i<-match(bt$decision_dates[t],as.Date(index(R)))-bt$settings$lag
      hist<-as.matrix(R[max(1,i-125):i,]);target<-bt$diagnostics$match_target[t]
      base<-switch(bn,QQQ=w_qqq_bh(colnames(R)),SPY=w_spy_bh(colnames(R)),Balanced=w_6040(colnames(R)),Equal=w_equal(colnames(R)))
      z<-vol_match(base,hist,target)
      out[[length(out)+1L]]<-fast_frame(sample=bt$settings$sample,date=bt$diagnostics$date[t],model=model,
               target_vol=target,predicted_vol=z$vol,attainable=z$attainable,base_exposure=z$exposure)
    }
  }
  bind_rows(out)
}
mechanism_diagnostics <- function(bt,nctx,start,end) {
  d<-bt$returns;d<-d[d$date>=as.Date(start)&d$date<=as.Date(end),]
  a<-d[d$model=="DB_LAOPS",];out<-list()
  for(nm in c("DB_NoDrawdown","DB_NoCVaR","DB_NoRiskBudget","DB_UniformCandidates","DB_BudgetEqual","DB_UniformContext")) {
    b<-if(nm=="DB_UniformContext") nctx$returns[nctx$returns$model=="DB_LAOPS"&nctx$returns$date>=as.Date(start)&nctx$returns$date<=as.Date(end),] else d[d$model==nm,]
    stopifnot(identical(a$date,b$date))
    out[[nm]]<-fast_frame(sample=bt$settings$sample,ablation=nm,n=nrow(a),
         n_different_returns=sum(abs(a$net_return-b$net_return)>1e-12),
         max_absolute_daily_difference=max(abs(a$net_return-b$net_return)))
  }
  bind_rows(out)
}
# Sufficient-condition audit: DD screening is redundant when every CVaR-passing
# candidate also passes DD and at least two CVaR candidates exist. This is a
# diagnostic identity, not a novel portfolio performance theorem.
budget_activation <- function(bt,m,dd=.30,cvar=.026) {
  nm<-names(bt$candidate_weights[[1]]);N<-length(bt$candidate_weights)
  H<-sapply(nm,function(k)bt$returns$net_return[bt$returns$model==k]);out<-list()
  R<-na.omit(m$R[,colnames(bt$targets),drop=FALSE])
  for(t in seq_len(N)) {
    n<-max(0L,t-1L-bt$settings$lag);if(n<20)next
    st<-candidate_history_stats(H[seq_len(n),,drop=FALSE],126)
    i<-match(bt$decision_dates[t],as.Date(index(R)))-bt$settings$lag
    xi<-compute_stress_intensity(m$X[index(R)[i]])
    bd<-max(.18,dd-.04*xi);bc<-max(.014,cvar-.004*xi)
    passc<-st$cvar<=bc;passd<- -st$mdd<=bd
    out[[length(out)+1L]]<-fast_frame(sample=bt$settings$sample,date=bt$diagnostics$date[t],
       budget_dd=bd,budget_cvar=bc,cvar_pass=sum(passc),dd_pass=sum(passd),joint_pass=sum(passc&passd),
       dd_only_exclusions=sum(passc & !passd),cvar_only_exclusions=sum(passd & !passc),
       dd_redundancy_sufficient=sum(passc)>=2&&all(passd[passc]),
       cvar_redundancy_sufficient=sum(passd)>=2&&all(passc[passd]))
  }
  bind_rows(out)
}
