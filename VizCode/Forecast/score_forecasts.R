# =============================================================================
# score_forecasts.R
#
# Scores the frozen 2026 Senate (Team 5) and House (solo) forecasts against the
# election results, and benchmarks both against Kalshi and a fundamentals-only
# baseline built from the models' own priors.
#
# Run from the project root (open Website.Rproj in RStudio, then source this
# file). It is NOT run during render: forecast.qmd and forecast-scorecard.qmd
# only read the scores.rds it writes. Re-run it whenever you want fresh results,
# then re-render.
#
#   1. FETCH   - race calls from Kalshi (settled markets), vote shares from
#                Wikipedia's MediaWiki API. Each successful fetch is cached in
#                VizCode/Forecast/cache/; a failed fetch keeps the last cache.
#   2. COMBINE - one results table per chamber, by source priority:
#                  winner: overrides > Kalshi > Wikipedia > seed
#                  margin: overrides > Wikipedia > seed
#                and a checks table for any disagreement between sources.
#   3. SCORE   - Brier, Brier skill vs Kalshi, log loss, call accuracy, Murphy
#                reliability / resolution, margin MAE / RMSE / bias, interval
#                coverage, seat-distribution CRPS and PIT. Writes scores.rds.
#
# Inputs you may edit by hand (normally you never need to):
#   inputs/results_overrides.csv  one row per correction; wins over every source
#
# Packages: jsonlite, xml2 (both on CRAN).
# =============================================================================

# ---- settings ----------------------------------------------------------------
REFRESH       <- TRUE    # FALSE = skip the network and rescore from the caches
MIN_COVERAGE  <- 0.90    # share of scorable races that must be resolved before
                         # race-level scores are shown as final on the page
COMPETITIVE   <- 0.05    # a race is "competitive" if ANY forecaster gave the
                         # trailing side at least this probability
P_CLIP        <- 1e-4    # log loss clips probabilities to [P_CLIP, 1 - P_CLIP]
N_BINS        <- 5       # equal-width probability bins for calibration (few races, so few bins)
KALSHI_PAUSE  <- 0.15    # seconds between Kalshi requests
USER_AGENT    <- "MarkGScott-forecast-scoring/1.0 (https://github.com/MarkGScott960/2026_Midterm_Forecast)"
KALSHI_URL    <- "https://api.elections.kalshi.com/trade-api/v2/markets/"
WIKI_API      <- "https://en.wikipedia.org/w/api.php"
WIKI_PAGES    <- c(senate = "2026 United States Senate elections",
                   house  = "2026 United States House of Representatives elections")

suppressPackageStartupMessages({
  for (p in c("jsonlite", "xml2")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package '", p, "' is needed: install.packages('", p, "')")
  }
  library(jsonlite)
})

# ---- paths (relative to the project root) ------------------------------------
if (!file.exists("_quarto.yml"))
  stop("Run this from the website project root (open Website.Rproj first).")
DIR      <- "VizCode/Forecast"
IN_DIR   <- file.path(DIR, "inputs")
CACHE    <- file.path(DIR, "cache")
RES_DIR  <- file.path(DIR, "results")
for (d in c(CACHE, RES_DIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

now_et <- function() format(Sys.time(), "%Y-%m-%d %H:%M %Z", tz = "America/New_York")
LOG <- data.frame(time = character(), step = character(), status = character(),
                  detail = character(), stringsAsFactors = FALSE)
log_it <- function(step, status, detail = "") {
  LOG[nrow(LOG) + 1, ] <<- list(now_et(), step, status, detail)
  message(sprintf("[%s] %-10s %-6s %s", format(Sys.time(), "%H:%M:%S"), step, status, detail))
}

read_csv_chr <- function(f) {
  if (!file.exists(f)) return(NULL)
  read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"), check.names = FALSE,
           colClasses = "character", encoding = "UTF-8")
}

http_text <- function(url) {
  tf <- tempfile()
  on.exit(unlink(tf))
  utils::download.file(url, tf, quiet = TRUE, method = "libcurl",
                       headers = c("User-Agent" = USER_AGENT, "Accept" = "application/json"))
  paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# =============================================================================
# 1. FORECASTS (read straight from the frozen snapshot in forecast/)
# =============================================================================
sen_cfg   <- read_json("forecast/senate/data/config_snapshot.json")
sen_head  <- read_json("forecast/senate/data/headline.json")
hou_head  <- read_json("forecast/house/data/headline.json")

sen_races <- read.csv("forecast/senate/data/races.csv", stringsAsFactors = FALSE)
sen_prob  <- read.csv("forecast/senate/data/race_probabilities.csv", stringsAsFactors = FALSE)
hou_races <- read.csv("forecast/house/data/races.csv", stringsAsFactors = FALSE)

# Prior SD = the SD the model gave a race that had nothing but fundamentals
# (share_prior == 1). Read from the data, not typed in.
prior_sd <- function(sd, share_prior) stats::median(sd[abs(share_prior - 1) < 1e-9], na.rm = TRUE)
SEN_PRIOR_SD <- prior_sd(sen_prob$post_sd, sen_prob$share_prior)
HOU_PRIOR_SD <- prior_sd(hou_races$sd, hou_races$share_prior)

sen_fc <- merge(
  sen_races[, c("state", "p_dem", "margin", "sd", "kalshi_p", "n_polls")],
  setNames(sen_prob[, c("State", "prior_mean", "kalshi_margin")], c("state", "prior_mean", "kalshi_margin")),
  by = "state", all.x = TRUE)
names(sen_fc)[names(sen_fc) == "state"] <- "race_id"
sen_fc$chamber   <- "senate"
sen_fc$race_type <- "DR"
sen_fc$label     <- sen_fc$race_id

hou_fc <- hou_races[, c("district", "p_dem", "margin", "sd", "kalshi_p", "n_polls",
                        "prior_mean", "kalshi_margin", "race_type", "dem_candidate", "rep_candidate")]
names(hou_fc)[1] <- "race_id"
hou_fc$chamber <- "house"
hou_fc$label   <- hou_fc$race_id

# Fundamentals-only baseline: P(D) implied by the prior mean and prior SD.
sen_fc$p_fund <- stats::pnorm(sen_fc$prior_mean / SEN_PRIOR_SD)
hou_fc$p_fund <- stats::pnorm(hou_fc$prior_mean / HOU_PRIOR_SD)
sen_fc$sd_fund <- SEN_PRIOR_SD
hou_fc$sd_fund <- HOU_PRIOR_SD

# Scorable = a real two-sided contest. Same-party (DD, RR) and single-party
# (D_only) House seats were certain in the model and are left out of race scores.
SCORABLE_TYPES <- c("DR", "IND")
FORECASTERS <- c(model = "p_dem", kalshi = "kalshi_p", fundamentals = "p_fund")
FORECASTER_LABEL <- c(model = "Model", kalshi = "Kalshi", fundamentals = "Fundamentals only")

# =============================================================================
# 2. FETCH
# =============================================================================
markets <- read_csv_chr(file.path(IN_DIR, "kalshi_markets.csv"))

fetch_kalshi <- function() {
  out <- vector("list", nrow(markets))
  for (i in seq_len(nrow(markets))) {
    m <- markets[i, ]
    res <- tryCatch({
      j <- fromJSON(http_text(paste0(KALSHI_URL, m$market_ticker)))$market
      if (is.null(j$ticker) || j$ticker != m$market_ticker)
        stop("response is not for ", m$market_ticker)
      data.frame(chamber = m$chamber, race_id = m$race_id, market_ticker = m$market_ticker,
                 market_side = m$market_side, status = as.character(j$status),
                 result = as.character(j$result %||% ""),
                 settlement_ts = as.character(j$settlement_ts %||% ""),
                 fetched = now_et(), stringsAsFactors = FALSE)
    }, error = function(e) {
      log_it("kalshi", "error", paste(m$market_ticker, conditionMessage(e)))
      NULL
    })
    out[[i]] <- res
    Sys.sleep(KALSHI_PAUSE)
  }
  do.call(rbind, out)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

STATE_ABB <- setNames(c(state.abb, "DC"), c(state.name, "District of Columbia"))

# Parse one Wikipedia results table. Wikipedia's election tables put the race in
# the first cell, a results note ("Incumbent re-elected.", "New member elected.
# Democratic gain.") in the second-to-last cell, and the candidates with vote
# shares, e.g. "Jane Doe (Democratic) 55.2%", in the last cell.
parse_wiki_rows <- function(html, chamber) {
  doc  <- xml2::read_html(html)
  rows <- xml2::xml_find_all(doc, "//table[contains(@class,'wikitable')]//tr")
  st_pat <- paste0("^(", paste(names(STATE_ABB), collapse = "|"), ")\\b")
  out <- list()
  for (tr in rows) {
    cells <- xml2::xml_find_all(tr, "./th|./td")
    if (length(cells) < 3) next
    key_txt <- trimws(gsub("\\s+", " ", xml2::xml_text(cells[[1]])))
    st <- regmatches(key_txt, regexpr(st_pat, key_txt))
    if (length(st) == 0) next
    if (chamber == "house") {
      num <- regmatches(key_txt, regexpr("(at-large|At-large|AL|[0-9]{1,2})\\s*$", key_txt))
      if (length(num) == 0) next
      num <- if (grepl("^[0-9]+$", num)) sprintf("%02d", as.integer(num)) else "AL"
      race_id <- paste0(STATE_ABB[[st]], "-", num)
    } else {
      if (grepl("[0-9]\\s*$|at-large", key_txt, ignore.case = TRUE)) next   # a House row
      race_id <- st
    }
    cand_cell <- cells[[length(cells)]]
    items <- xml2::xml_text(xml2::xml_find_all(cand_cell, ".//li"))
    if (length(items) == 0) items <- strsplit(xml2::xml_text(cand_cell), "\n")[[1]]
    items <- trimws(gsub("\\[[^]]*\\]", "", items))       # drop footnote marks
    pat <- "^(.*?)\\s*\\(([^()]+)\\)\\s*([0-9]+(?:\\.[0-9]+)?)\\s*%"
    hit <- grepl(pat, items, perl = TRUE)
    if (!any(hit)) next
    mm  <- regmatches(items[hit], regexec(pat, items[hit], perl = TRUE))
    cand <- data.frame(name  = trimws(sapply(mm, `[`, 2)),
                       party = trimws(sapply(mm, `[`, 3)),
                       pct   = as.numeric(sapply(mm, `[`, 4)), stringsAsFactors = FALSE)
    is_r <- grepl("^Republican", cand$party)
    note <- xml2::xml_text(cells[[length(cells) - 1]])
    called <- grepl("elected|re-elected|gain|hold|retained|defeated", note, ignore.case = TRUE) &&
              !grepl("runoff", note, ignore.case = TRUE)
    top <- cand[which.max(cand$pct), ]
    out[[length(out) + 1]] <- data.frame(
      chamber = chamber, race_id = race_id,
      winner_side = if (called) ifelse(grepl("^Republican", top$party), "R", "D") else NA,
      d_margin = if (any(is_r) && any(!is_r)) max(cand$pct[!is_r]) - max(cand$pct[is_r]) else NA,
      called = called, n_candidates = nrow(cand), note = trimws(gsub("\\s+", " ", note)),
      stringsAsFactors = FALSE)
  }
  if (length(out) == 0) return(NULL)
  res <- do.call(rbind, out)
  # If a race appears in more than one table, keep the row that has a call and
  # the most candidates (the results table rather than, say, a preview table).
  res <- res[order(res$race_id, !res$called, -res$n_candidates), ]
  res[!duplicated(res[, c("chamber", "race_id")]), ]
}

fetch_wikipedia <- function() {
  out <- list()
  for (ch in names(WIKI_PAGES)) {
    url <- paste0(WIKI_API, "?action=parse&prop=text&format=json&formatversion=2&redirects=1&page=",
                  utils::URLencode(WIKI_PAGES[[ch]], reserved = TRUE))
    res <- tryCatch({
      html <- fromJSON(http_text(url))$parse$text
      parse_wiki_rows(html, ch)
    }, error = function(e) { log_it("wikipedia", "error", paste(ch, conditionMessage(e))); NULL })
    # Sanity floor: a page with results should give most races. If not, the page
    # layout has probably changed - keep the last good cache instead.
    floor_n <- if (ch == "senate") 20 else 200
    n_pct <- if (is.null(res)) 0 else sum(!is.na(res$d_margin))
    if (n_pct < floor_n) {
      log_it("wikipedia", "skip", sprintf("%s: only %d races with vote shares parsed (need %d); cache kept",
                                          ch, n_pct, floor_n))
      next
    }
    log_it("wikipedia", "ok", sprintf("%s: %d races parsed, %d called", ch, nrow(res), sum(res$called)))
    out[[ch]] <- res
  }
  out
}

# Replace only the chambers that fetched cleanly; keep the rest of the cache.
update_cache <- function(new, file, chambers) {
  if (is.null(new) || nrow(new) == 0) return(invisible())
  new[] <- lapply(new, as.character)
  old <- read_csv_chr(file)
  if (!is.null(old) && identical(sort(names(old)), sort(names(new)))) {
    old <- old[!old$chamber %in% chambers, names(new), drop = FALSE]
    new <- rbind(old, new)
  }
  write.csv(new, file, row.names = FALSE, na = "")
}

if (REFRESH) {
  log_it("kalshi", "start", sprintf("%d markets", nrow(markets)))
  k <- fetch_kalshi()
  if (!is.null(k) && nrow(k) > 0) {
    write.csv(k, file.path(CACHE, "kalshi_calls.csv"), row.names = FALSE, na = "")
    log_it("kalshi", "ok", sprintf("%d of %d markets read, %d settled", nrow(k), nrow(markets),
                                   sum(k$result %in% c("yes", "no"))))
  }
  w <- fetch_wikipedia()
  if (length(w) > 0) {
    wdf <- do.call(rbind, w)
    update_cache(wdf, file.path(CACHE, "wikipedia_results.csv"), names(w))
  }
} else {
  log_it("fetch", "skip", "REFRESH = FALSE; using caches")
}

# =============================================================================
# 3. COMBINE
# =============================================================================
seed  <- read_csv_chr(file.path(IN_DIR, "results_seed.csv"))
over  <- read_csv_chr(file.path(IN_DIR, "results_overrides.csv"))
kal   <- read_csv_chr(file.path(CACHE, "kalshi_calls.csv"))
wiki  <- read_csv_chr(file.path(CACHE, "wikipedia_results.csv"))
ccall <- read_csv_chr(file.path(IN_DIR, "chamber_calls_seed.csv"))

settled <- function(df) !is.null(df) && nrow(df) > 0
kal_side <- function(df) {      # D/R winner implied by a settled market, else NA
  ifelse(!df$result %in% c("yes", "no"), NA,
         ifelse(df$result == "yes", df$market_side, ifelse(df$market_side == "R", "D", "R")))
}

CHECKS <- data.frame(chamber = character(), race_id = character(), check = character(),
                     detail = character(), stringsAsFactors = FALSE)
flag <- function(ch, id, what, detail) CHECKS[nrow(CHECKS) + 1, ] <<- list(ch, id, what, detail)

combine <- function(ch, fc) {
  ids <- fc$race_id
  pick <- function(df, col, cond = TRUE) {
    if (!settled(df)) return(setNames(rep(NA_character_, length(ids)), ids))
    df <- df[df$chamber == ch & cond, , drop = FALSE]
    v <- setNames(df[[col]], df$race_id)
    unname(v[ids])
  }
  k_rows <- if (settled(kal)) kal[kal$chamber == ch & kal$race_id != "CONTROL", , drop = FALSE] else NULL
  k_win <- if (!is.null(k_rows) && nrow(k_rows)) setNames(kal_side(k_rows), k_rows$race_id)[ids] else rep(NA, length(ids))
  o_win <- pick(over, "winner_side"); o_mar <- pick(over, "d_margin")
  w_win <- pick(wiki, "winner_side"); w_mar <- pick(wiki, "d_margin")
  s_win <- pick(seed, "winner_side"); s_mar <- pick(seed, "d_margin")
  s_src <- pick(seed, "source");      o_src <- pick(over, "source")

  first <- function(...) { x <- cbind(...); apply(x, 1, function(r) r[!is.na(r)][1]) }
  win <- first(o_win, k_win, w_win, s_win)
  win_src <- ifelse(!is.na(o_win), paste("override:", o_src),
             ifelse(!is.na(k_win), "Kalshi settlement",
             ifelse(!is.na(w_win), "Wikipedia results table",
             ifelse(!is.na(s_win), s_src, NA))))
  mar <- suppressWarnings(as.numeric(first(o_mar, w_mar, s_mar)))
  mar_src <- ifelse(!is.na(o_mar), "override", ifelse(!is.na(w_mar), "Wikipedia (provisional until certified)",
             ifelse(!is.na(s_mar), "seed", NA)))

  # Cross-source checks
  for (i in seq_along(ids)) {
    calls <- c(kalshi = k_win[i], wikipedia = w_win[i], seed = s_win[i])
    calls <- calls[!is.na(calls)]
    if (length(unique(calls)) > 1)
      flag(ch, ids[i], "winner disagreement",
           paste(names(calls), calls, sep = "=", collapse = ", "))
    if (!is.na(mar[i]) && !is.na(win[i]) && mar[i] != 0 && (mar[i] > 0) != (win[i] == "D")) {
      flag(ch, ids[i], "margin sign vs winner",
           sprintf("winner %s but D margin %+.1f; margin dropped", win[i], mar[i]))
      mar[i] <- NA; mar_src[i] <- NA
    }
  }
  data.frame(chamber = ch, race_id = ids, winner_side = win, winner_source = win_src,
             d_margin = mar, margin_source = mar_src, stringsAsFactors = FALSE)
}

sen_res <- combine("senate", sen_fc)
hou_res <- combine("house", hou_fc)

write.csv(sen_res, file.path(RES_DIR, "senate_results.csv"), row.names = FALSE, na = "")
write.csv(hou_res, file.path(RES_DIR, "house_results.csv"), row.names = FALSE, na = "")

# Chamber control: Kalshi control market if settled, else the seed call.
control_call <- function(ch) {
  if (settled(kal)) {
    r <- kal[kal$chamber == ch & kal$race_id == "CONTROL", , drop = FALSE]
    if (nrow(r) && r$result %in% c("yes", "no"))
      return(list(side = kal_side(r), source = "Kalshi control market settlement"))
  }
  r <- ccall[ccall$chamber == ch & ccall$status == "called", , drop = FALSE]
  if (nrow(r)) return(list(side = r$control_side[1], source = r$source[1]))
  list(side = NA, source = NA)
}

# =============================================================================
# 4. SCORE
# =============================================================================
clip  <- function(p) pmin(pmax(p, P_CLIP), 1 - P_CLIP)
brier <- function(p, o) mean((p - o)^2)
lloss <- function(p, o) -mean(o * log(clip(p)) + (1 - o) * log(1 - clip(p)))

# Murphy (1973) decomposition with equal-width bins:
#   BS = reliability - resolution + uncertainty (+ a within-bin term, reported)
murphy <- function(p, o, nb = N_BINS) {
  b <- cut(p, seq(0, 1, length.out = nb + 1), include.lowest = TRUE)
  ob <- mean(o)
  g <- split(data.frame(p, o), b, drop = TRUE)
  n <- sapply(g, nrow); pk <- sapply(g, function(d) mean(d$p)); ok <- sapply(g, function(d) mean(d$o))
  rel <- sum(n * (pk - ok)^2) / length(p)
  res <- sum(n * (ok - ob)^2) / length(p)
  unc <- ob * (1 - ob)
  list(reliability = rel, resolution = res, uncertainty = unc,
       within_bin = brier(p, o) - (rel - res + unc))
}

calib_bins <- function(p, o, nb = N_BINS) {
  b <- cut(p, seq(0, 1, length.out = nb + 1), include.lowest = TRUE)
  g <- split(data.frame(p, o), b, drop = TRUE)
  data.frame(bin = names(g), n = sapply(g, nrow),
             mean_p = sapply(g, function(d) mean(d$p)),
             obs = sapply(g, function(d) mean(d$o)), row.names = NULL)
}

score_chamber <- function(ch, fc, res, seats_file, head, p_control_model, holdover_d = 0) {
  d <- merge(fc, res[, c("race_id", "winner_side", "winner_source", "d_margin", "margin_source")],
             by = "race_id", all.x = TRUE)
  d$scorable <- d$race_type %in% SCORABLE_TYPES
  d$resolved <- d$scorable & !is.na(d$winner_side)
  d$o <- ifelse(d$winner_side == "D", 1, ifelse(d$winner_side == "R", 0, NA))
  # competitive: any forecaster gave the trailing side >= COMPETITIVE
  pm <- as.matrix(d[, FORECASTERS])
  d$competitive <- apply(pm, 1, function(r) any(!is.na(r) & pmin(r, 1 - r) >= COMPETITIVE))

  n_scorable <- sum(d$scorable); n_resolved <- sum(d$resolved)
  n_margin   <- sum(d$resolved & !is.na(d$d_margin))
  cov_calls  <- n_resolved / n_scorable
  cov_margin <- n_margin / n_scorable

  r <- d[d$resolved, ]
  # per-race scores for each forecaster
  for (f in names(FORECASTERS)) {
    p <- r[[FORECASTERS[[f]]]]
    r[[paste0("bs_", f)]] <- (p - r$o)^2
    r[[paste0("hit_", f)]] <- ifelse(is.na(p), NA, (p > 0.5) == (r$o == 1))
  }

  board <- list()
  for (sub in c("all", "competitive")) {
    rs <- if (sub == "all") r else r[r$competitive, ]
    ks <- rs[!is.na(rs$kalshi_p), ]
    bs_k <- if (nrow(ks)) brier(ks$kalshi_p, ks$o) else NA
    for (f in names(FORECASTERS)) {
      col <- FORECASTERS[[f]]
      x <- rs[!is.na(rs[[col]]), ]
      if (!nrow(x)) next
      xk <- ks[!is.na(ks[[col]]), ]
      mp <- murphy(x[[col]], x$o)
      board[[length(board) + 1]] <- data.frame(
        chamber = ch, subset = sub, forecaster = f, label = FORECASTER_LABEL[[f]],
        n = nrow(x), brier = brier(x[[col]], x$o), log_loss = lloss(x[[col]], x$o),
        accuracy = mean((x[[col]] > 0.5) == (x$o == 1)),
        n_kalshi_overlap = nrow(xk),
        brier_overlap = if (nrow(xk)) brier(xk[[col]], xk$o) else NA,
        bss_vs_kalshi = if (nrow(xk) && !is.na(bs_k) && bs_k > 0) 1 - brier(xk[[col]], xk$o) / bs_k else NA,
        reliability = mp$reliability, resolution = mp$resolution, uncertainty = mp$uncertainty,
        stringsAsFactors = FALSE)
    }
  }
  board <- do.call(rbind, board)
  # Rank on the races every forecaster covered (the ones with a Kalshi market),
  # so a forecaster is never ranked on an easier set of races than the others.
  board$rank <- ave(board$brier_overlap, board$subset, FUN = function(b) rank(b, ties.method = "min"))

  # head to head, model vs Kalshi, race by race (competitive races with a market)
  hh <- r[r$competitive & !is.na(r$kalshi_p), ]
  h2h <- list(n = nrow(hh),
              model_closer = sum(hh$bs_model < hh$bs_kalshi),
              kalshi_closer = sum(hh$bs_kalshi < hh$bs_model),
              mean_diff = if (nrow(hh)) mean(hh$bs_model - hh$bs_kalshi) else NA)

  # margins
  rm_ <- r[!is.na(r$d_margin), ]
  margin_tab <- NULL
  if (nrow(rm_)) {
    mrow <- function(lab, fcst, sd) {
      ok <- !is.na(fcst); e <- fcst[ok] - rm_$d_margin[ok]
      z <- if (is.null(sd)) NULL else abs(e) / sd[ok]
      data.frame(chamber = ch, forecaster = lab, n = sum(ok), mae = mean(abs(e)),
                 rmse = sqrt(mean(e^2)), bias = mean(e),
                 cover80 = if (is.null(z)) NA else mean(z <= stats::qnorm(0.90)),
                 cover95 = if (is.null(z)) NA else mean(z <= stats::qnorm(0.975)),
                 stringsAsFactors = FALSE)
    }
    margin_tab <- rbind(mrow("model", rm_$margin, rm_$sd),
                        mrow("fundamentals", rm_$prior_mean, rm_$sd_fund),
                        mrow("kalshi", rm_$kalshi_margin, NULL))
    margin_tab$label <- FORECASTER_LABEL[margin_tab$forecaster]
    rm_$z <- (rm_$d_margin - rm_$margin) / rm_$sd
    rm_$pit <- stats::pnorm(rm_$z)
  }

  # calibration bins (all resolved races)
  cal <- do.call(rbind, lapply(names(FORECASTERS), function(f) {
    col <- FORECASTERS[[f]]; x <- r[!is.na(r[[col]]), ]
    if (!nrow(x)) return(NULL)
    cbind(forecaster = f, label = FORECASTER_LABEL[[f]], calib_bins(x[[col]], x$o))
  }))

  # chamber-level: seat distribution vs actual
  seats <- read.csv(seats_file)
  full <- data.frame(dem_seats = seq(min(seats$dem_seats), max(seats$dem_seats)))
  full$share <- seats$share[match(full$dem_seats, seats$dem_seats)]
  full$share[is.na(full$share)] <- 0
  full$share <- full$share / sum(full$share)
  full$cdf <- cumsum(full$share)

  certain_d <- sum(d$race_type %in% c("DD", "D_only"))
  d_won   <- holdover_d + certain_d + sum(d$o[d$resolved] == 1)
  pending <- sum(d$scorable & !d$resolved)
  seat_lo <- d_won; seat_hi <- d_won + pending
  final_seats <- if (pending == 0) d_won else NA
  crps <- if (!is.na(final_seats)) sum((full$cdf - (full$dem_seats >= final_seats))^2) else NA
  med  <- full$dem_seats[which(full$cdf >= 0.5)[1]]
  p_in_range <- sum(full$share[full$dem_seats >= seat_lo & full$dem_seats <= seat_hi])

  ctrl <- control_call(ch)
  o_ctrl <- if (is.na(ctrl$side)) NA else as.numeric(ctrl$side == "D")
  chamber <- list(
    chamber = ch, n_scorable = n_scorable, n_resolved = n_resolved, n_margin = n_margin,
    coverage_calls = cov_calls, coverage_margin = cov_margin,
    ready_calls = cov_calls >= MIN_COVERAGE, ready_margin = cov_margin >= MIN_COVERAGE,
    control_side = ctrl$side, control_source = ctrl$source,
    p_control_model = p_control_model, p_control_kalshi = head$kalshi_p_control,
    brier_control_model = if (is.na(o_ctrl)) NA else (p_control_model - o_ctrl)^2,
    brier_control_kalshi = if (is.na(o_ctrl)) NA else (head$kalshi_p_control - o_ctrl)^2,
    seats_won_d = d_won, seats_pending = pending, seat_lo = seat_lo, seat_hi = seat_hi,
    seats_final = final_seats, seats_median = med, seats_p10 = head$p10, seats_p90 = head$p90,
    seats_mean = head$exp_d_seats, p_seat_range = p_in_range,
    seat_error = if (is.na(final_seats)) NA else final_seats - med,
    in_80 = if (is.na(final_seats)) NA else final_seats >= head$p10 & final_seats <= head$p90,
    pit_seats = if (is.na(final_seats)) NA else sum(full$share[full$dem_seats <= final_seats]),
    p_exact = if (is.na(final_seats)) NA else full$share[full$dem_seats == final_seats] %||% 0,
    crps = crps, kalshi_expected_seats = head$kalshi_expected_seats)

  list(races = d, resolved = r, board = board, h2h = h2h, margins = margin_tab,
       margin_races = if (nrow(rm_)) rm_ else NULL, calibration = cal,
       seat_dist = full, chamber = chamber)
}

SEN <- score_chamber("senate", sen_fc, sen_res, "forecast/senate/data/seats.csv", sen_head,
                     sen_head$p_d_51, holdover_d = sen_cfg$HOLDOVER_D)
HOU <- score_chamber("house", hou_fc, hou_res, "forecast/house/data/seats.csv", hou_head,
                     hou_head$p_d_218, holdover_d = 0)

# Largest errors: highest model Brier among competitive resolved races
misses <- function(S, n = 8) {
  r <- S$resolved[S$resolved$competitive, ]
  r <- r[order(-r$bs_model), ]
  head(r[, intersect(c("label", "p_dem", "kalshi_p", "p_fund", "winner_side", "margin", "d_margin",
                       "bs_model", "bs_kalshi"), names(r))], n)
}

scores <- list(
  meta = list(scored_at = now_et(), min_coverage = MIN_COVERAGE, competitive = COMPETITIVE,
              p_clip = P_CLIP, n_bins = N_BINS, sen_prior_sd = SEN_PRIOR_SD, hou_prior_sd = HOU_PRIOR_SD,
              sources = list(kalshi = KALSHI_URL, wikipedia = unname(WIKI_PAGES))),
  senate = SEN, house = HOU,
  misses = list(senate = misses(SEN), house = misses(HOU)),
  checks = CHECKS, log = LOG)

saveRDS(scores, file.path(DIR, "scores.rds"))
write.csv(CHECKS, file.path(RES_DIR, "checks.csv"), row.names = FALSE)
write.csv(LOG,    file.path(RES_DIR, "fetch_log.csv"), row.names = FALSE)

cat(sprintf("\nScored at %s\n", scores$meta$scored_at))
for (S in list(SEN, HOU)) {
  c_ <- S$chamber
  cat(sprintf("%-6s resolved %d/%d races (%.0f%%), margins %d; D seats so far %d (+%d pending); control: %s\n",
              c_$chamber, c_$n_resolved, c_$n_scorable, 100 * c_$coverage_calls, c_$n_margin,
              c_$seats_won_d, c_$seats_pending, c_$control_side))
}
if (nrow(CHECKS)) { cat("\nSource checks:\n"); print(CHECKS) }
