# ---- Setup ----
suppressPackageStartupMessages({
  library(tidyverse)  # ggplot2, dplyr, tidyr, readr, forcats, stringr
})

# Ensure output folder exists
if (!dir.exists("results/fig")) dir.create("results/fig", recursive = TRUE)

# ---- Load data ----
bg   <- readr::read_csv("data/background-clean.csv")   # has rsrch (TRUE/FALSE)
intr <- readr::read_csv("data/interest-clean.csv")     # has response_id, lang, ...


lang_df <- intr %>%
  distinct(response_id, .keep_all = TRUE) %>%
  mutate(
    lang = case_when(
      is.na(lang) ~ "No preference",
      stringr::str_detect(lang, regex("^\\s*R\\s*$", ignore_case = TRUE)) ~ "R",
      stringr::str_detect(lang, regex("python", ignore_case = TRUE)) ~ "Python",
      stringr::str_detect(lang, regex("no preference", ignore_case = TRUE)) ~ "No preference",
      TRUE ~ lang
    ),
    lang = forcats::fct_relevel(factor(lang), "R", "Python", "No preference")
  )

df <- lang_df %>%
  left_join(bg %>% select(response_id, rsrch), by = "response_id") %>%
  mutate(
    rsrch = case_when(
      is.na(rsrch) ~ NA_character_,
      rsrch        ~ "Research exp: Yes",
      !rsrch       ~ "Research exp: No"
    ),
    rsrch = factor(rsrch, levels = c("Research exp: No", "Research exp: Yes"))
  )

pal <- c("R" = "#0072B2", "Python" = "#E69F00", "No preference" = "#999999")

# ===============================================================
# Graph 1 — Overall language preference
# ===============================================================
g1_data <- lang_df %>%
  count(lang, name = "n") %>%
  mutate(pct = n / sum(n))

g1 <- ggplot(g1_data, aes(x = lang, y = pct, fill = lang)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            vjust = -0.5, size = 4.2, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, .08))) +
  scale_fill_manual(values = pal) +
  labs(title = "Overall language preference",
       x = NULL, y = "Share of respondents") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank())

ggsave("results/fig/q2_lang_overall.png", g1,
       width = 7, height = 4.6, dpi = 320, bg = "white")

# ===============================================================
# Graph 2 — Language preference by research experience 
# ===============================================================


grp_n <- df %>%
  filter(!is.na(rsrch)) %>%
  distinct(response_id, rsrch) %>%
  count(rsrch, name = "group_n")


g2_data <- df %>%
  filter(!is.na(rsrch), !is.na(lang)) %>%
  count(rsrch, lang, name = "n") %>%
  group_by(rsrch) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  left_join(grp_n, by = "rsrch") %>%
  mutate(
    rsrch_lab = sprintf("%s (n=%d)", rsrch, group_n),
    # keep the order "No" then "Yes"
    rsrch_lab = factor(
      rsrch_lab,
      levels = sprintf(
        "%s (n=%d)",
        levels(df$rsrch),
        grp_n$group_n[match(levels(df$rsrch), grp_n$rsrch)]
      )
    )
  )

g2 <- ggplot(g2_data, aes(x = rsrch_lab, y = pct, fill = lang)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = ifelse(pct >= 0.07,
                               scales::percent(pct, accuracy = 1), "")),
            position = position_stack(vjust = 0.5),
            size = 3.6, color = "white", fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = pal) +
  labs(title = "Language preference by research experience",
       x = NULL, y = "Percent within research-exp group", fill = "Language") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10))

ggsave("results/fig/q2_lang_by_research.png", g2,
       width = 8, height = 5, dpi = 320, bg = "white")

message("Saved figures to: results/fig/q2_lang_overall.png and q2_lang_by_research.png")

# ===============================================================
# advanced analysis
# ===============================================================

df_full <- lang_df %>%
  left_join(bg %>% select(response_id, rsrch, updv.num, prog.comf), by = "response_id") %>%
  mutate(
    updv.num = factor(updv.num, levels = c("0-2","3-5","6-8","9+")),
    updv.mid = case_when(
      updv.num == "0-2" ~ 1.0,
      updv.num == "3-5" ~ 4.0,
      updv.num == "6-8" ~ 7.0,
      updv.num == "9+"  ~ 9.5,
      TRUE ~ NA_real_
    ),
    rsrch_lab = if_else(rsrch, "Yes", "No", missing = NA_character_)
  )

# -------------------------------
# 1) t-test: confidence by language (Python vs R only)
# -------------------------------
df_RP <- df_full %>%
  filter(lang %in% c("R","Python"), !is.na(prog.comf))

ttest_R_vs_P <- t.test(prog.comf ~ lang, data = df_RP, var.equal = FALSE)
cat("\n\n--- Welch t-test (prog.comf: Python vs R) ---\n")
print(ttest_R_vs_P)

# -------------------------------
# 2) One-way ANOVA across 3 language groups
# -------------------------------
df_AOV <- df_full %>%
  filter(!is.na(prog.comf), !is.na(lang))

aov_fit <- aov(prog.comf ~ lang, data = df_AOV)
cat("\n\n--- One-way ANOVA: prog.comf ~ lang ---\n")
print(summary(aov_fit))

kw_fit <- kruskal.test(prog.comf ~ lang, data = df_AOV)
cat("\n\n--- Kruskal–Wallis (nonparametric): prog.comf ~ lang ---\n")
print(kw_fit)

# -------------------------------
# 3) Multinomial logistic regression: lang ~ confidence + coursework + research
# -------------------------------
suppressPackageStartupMessages(library(nnet))  # for multinom()

mod_dat <- df_full %>%
  select(lang, prog.comf, updv.mid, rsrch_lab) %>%
  filter(!is.na(lang), !is.na(prog.comf), !is.na(updv.mid), !is.na(rsrch_lab)) %>%
  mutate(
    lang = fct_drop(lang),                      # 3 classes
    rsrch_lab = factor(rsrch_lab, levels = c("No","Yes"))
  )
mnl <- nnet::multinom(lang ~ prog.comf + updv.mid + rsrch_lab, data = mod_dat, trace = FALSE)

cat("\n\n--- Multinomial logistic regression ---\n")
print(summary(mnl))

coefs   <- summary(mnl)$coefficients
ses     <- summary(mnl)$standard.errors
OR      <- exp(coefs)
lowerCI <- exp(coefs - 1.96*ses)
upperCI <- exp(coefs + 1.96*ses)

OR_tbl <- purrr::map2_dfr(asplit(OR, 1), rownames(OR), ~{
  tibble(
    outcome = .y,                # e.g., Python vs R
    term    = colnames(OR),
    OR      = as.numeric(.x),
    LCL     = as.numeric(lowerCI[.y, ]),
    UCL     = as.numeric(upperCI[.y, ])
  )
})

if (!dir.exists("results/tables")) dir.create("results/tables", recursive = TRUE)
readr::write_csv(OR_tbl, "results/tables/q2_multinom_odds_ratios.csv")

cat("\nOdds ratios saved to results/tables/q2_multinom_odds_ratios.csv\n")

# -------------------------------
# 4a) Scatter/smooth: confidence vs coursework, colored by language
# -------------------------------
scat <- df_full %>%
  filter(!is.na(prog.comf), !is.na(updv.mid), !is.na(lang))

g_scatter <- ggplot(scat, aes(x = updv.mid, y = prog.comf, color = lang)) +
  geom_jitter(width = 0.15, height = 0.08, alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_x_continuous(breaks = c(1,4,7,9.5),
                     labels = c("0–2","3–5","6–8","9+"),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_color_manual(values = c("R"="#0072B2","Python"="#E69F00","No preference"="#999999")) +
  labs(title = "Confidence vs coursework, by language",
       x = "Upper-division coursework (bins)", y = "Programming confidence") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_blank())

ggsave("results/fig/q2_conf_vs_coursework_by_lang.png", g_scatter,
       width = 8, height = 5, dpi = 320, bg = "white")

# -------------------------------
# 4b) Predicted probabilities from multinomial model
# -------------------------------
newdat <- tidyr::expand_grid(
  prog.comf = seq(2, 5, by = 0.1),
  rsrch_lab = factor(c("No","Yes"), levels = c("No","Yes")),
  updv.mid  = 7.0
)

pred <- as_tibble(predict(mnl, newdata = newdat, type = "probs"))
pred$prog.comf <- newdat$prog.comf
pred$rsrch_lab <- newdat$rsrch_lab

pred_long <- pred %>%
  rename(`R` = `R`, `Python` = `Python`, `No preference` = `No preference`) %>%
  pivot_longer(cols = c("R","Python","No preference"),
               names_to = "lang", values_to = "prob")

g_pred <- ggplot(pred_long, aes(x = prog.comf, y = prob, color = lang)) +
  geom_line(size = 1) +
  facet_wrap(~ rsrch_lab, nrow = 1, labeller = label_both) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
  scale_color_manual(values = c("R"="#0072B2","Python"="#E69F00","No preference"="#999999")) +
  labs(title = "Predicted probability of language preference",
       x = "Programming confidence", y = "Predicted probability", color = "Language") +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")

ggsave("results/fig/q2_multinom_pred_probs.png", g_pred,
       width = 9, height = 4.8, dpi = 320, bg = "white")

message("Advanced analysis complete. Figures saved:",
        "\n - results/fig/q2_conf_vs_coursework_by_lang.png",
        "\n - results/fig/q2_multinom_pred_probs.png",
        "\nOdds ratios table: results/tables/q2_multinom_odds_ratios.csv")
