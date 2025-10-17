library(tidyverse)
library(janitor)

bg_path <- "data/background-clean.csv"
res_dir <- "results"
if (!dir.exists(res_dir)) dir.create(res_dir, recursive = TRUE)

bg_raw <- readr::read_csv(bg_path, show_col_types = FALSE) |> clean_names()

prof_cols <- c("prog_prof","math_prof","stat_prof")
comf_cols <- c("prog_comf","math_comf","stat_comf")

cs_cols     <- names(bg_raw)[str_detect(names(bg_raw), "(?i)^cs\\d{1,3}$")]
pstat_cols  <- names(bg_raw)[str_detect(names(bg_raw), "(?i)^pstat\\d{1,3}$")]
econ_cols   <- names(bg_raw)[str_detect(names(bg_raw), "(?i)^econ\\d{1,3}$")]
course_cols <- names(bg_raw)[str_detect(names(bg_raw), "(?i)^(cs|pstat|econ)\\d{1,3}$")]

bg <- bg_raw |>
  mutate(across(all_of(course_cols), ~ as.numeric(as.character(.x)))) |>
  mutate(
    cs_count      = rowSums(across(all_of(cs_cols)), na.rm = TRUE),
    pstat_count   = rowSums(across(all_of(pstat_cols)), na.rm = TRUE),
    econ_count    = rowSums(across(all_of(econ_cols)), na.rm = TRUE),
    total_courses = rowSums(across(all_of(course_cols)), na.rm = TRUE)
  )

bin3 <- function(x) cut(x, breaks = c(-Inf, 0, 1, Inf), labels = c("0","1","2+"), right = TRUE, ordered_result = TRUE)

bg <- bg |>
  mutate(
    cs_bin    = bin3(cs_count),
    pstat_bin = bin3(pstat_count)
  )

lvl_prof <- c("beg","int","adv")

bg <- bg |>
  mutate(
    across(all_of(prof_cols), ~ factor(.x, levels = lvl_prof, ordered = TRUE)),
    across(all_of(comf_cols), ~ suppressWarnings(as.numeric(.x)))
  )

tabA <- bg |>
  group_by(cs_bin) |>
  summarise(
    n = n(),
    mean_prog_comf = mean(prog_comf, na.rm = TRUE),
    mean_math_comf = mean(math_comf, na.rm = TRUE),
    mean_stat_comf = mean(stat_comf, na.rm = TRUE),
    .groups = "drop"
  )

tabB <- bg |>
  group_by(pstat_bin) |>
  summarise(
    n = n(),
    mean_stat_comf = mean(stat_comf, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(tabA, file.path(res_dir, "q1_tableA_mean_comfort_by_csbin.csv"))
readr::write_csv(tabB, file.path(res_dir, "q1_tableB_mean_stat_comfort_by_pstatbin.csv"))

p1 <- bg |>
  ggplot(aes(x = cs_bin, fill = prog_prof)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  labs(title = "Programming proficiency skews higher with more prior CS courses",
       x = "CS courses taken", y = "Share of respondents", fill = "Prog proficiency") +
  theme_minimal(base_size = 12)
ggsave(file.path(res_dir, "q1_fig1_prog_prof_by_csbin.png"), p1, width = 7, height = 5, dpi = 300)

p2 <- bg |>
  ggplot(aes(x = cs_bin, y = prog_comf)) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1) +
  geom_boxplot(outlier.alpha = 0, width = 0.5) +
  coord_cartesian(ylim = c(1,5)) +
  labs(title = "Students with more CS coursework report higher programming comfort",
       x = "CS courses taken", y = "Programming comfort (1–5)") +
  theme_minimal(base_size = 12)
ggsave(file.path(res_dir, "q1_fig2_prog_comf_by_csbin.png"), p2, width = 7, height = 5, dpi = 300)

p3 <- bg |>
  ggplot(aes(x = pstat_bin, fill = stat_prof)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Greens", direction = 1) +
  labs(title = "Statistics proficiency increases with prior PSTAT coursework",
       x = "PSTAT courses taken", y = "Share of respondents", fill = "Stat proficiency") +
  theme_minimal(base_size = 12)
ggsave(file.path(res_dir, "q1_fig3_stat_prof_by_pstatbin.png"), p3, width = 7, height = 5, dpi = 300)

p4 <- bg |>
  ggplot(aes(x = total_courses, y = prog_comf)) +
  geom_jitter(height = 0.1, alpha = 0.35, size = 1) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Heavier prior coursework is associated with higher programming comfort",
       x = "Total prior courses (CS/PSTAT/ECON/etc.)", y = "Programming comfort (1–5)") +
  theme_minimal(base_size = 12)
ggsave(file.path(res_dir, "q1_fig4_prog_comf_vs_total_courses.png"), p4, width = 7, height = 5, dpi = 300)

suppressWarnings({
  cor_prog_cs    <- suppressWarnings(cor(bg$prog_comf, bg$cs_count, use = "pairwise.complete.obs", method = "spearman"))
  cor_stat_pstat <- suppressWarnings(cor(bg$stat_comf, bg$pstat_count, use = "pairwise.complete.obs", method = "spearman"))
})

cat(sprintf("\n[INFO] Spearman rho (prog comfort ~ CS count): %.3f\n", ifelse(is.finite(cor_prog_cs), cor_prog_cs, NA_real_)))
cat(sprintf("[INFO] Spearman rho (stat comfort ~ PSTAT count): %.3f\n", ifelse(is.finite(cor_stat_pstat), cor_stat_pstat, NA_real_)))

summ_lines <- c(
  sprintf("Spearman rho (prog comfort ~ CS count): %.3f", cor_prog_cs),
  sprintf("Spearman rho (stat comfort ~ PSTAT count): %.3f", cor_stat_pstat)
)
writeLines(summ_lines, file.path(res_dir, "q1_assoc_summary.txt"))
