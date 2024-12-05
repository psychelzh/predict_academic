library(targets)

tar_option_set(
  packages = c("tidyverse", "lmerTest"),
  controller = crew::crew_controller_local(workers = 8)
)

extract_metrics <- function(y, x, id_col = "user_id",
                            min_finished = 0.8, min_sample = 100) {
  x_matched <- x[x[[id_col]] %in% y[[id_col]], ]
  if (nrow(x_matched) < min_sample) {
    return()
  }
  prop_finished <- x_matched |>
    summarise(across(!any_of(id_col), \(x) mean(!is.na(x))))
  dat <- x_matched |>
    select(
      all_of(id_col),
      names(prop_finished)[prop_finished > min_finished]
    ) |>
    inner_join(y, by = id_col) |>
    drop_na() |>
    distinct(pick(all_of(id_col)), .keep_all = TRUE) |>
    column_to_rownames(id_col) |>
    filter(!performance::check_outliers(pick(everything()), method = "mcd"))
  if (nrow(dat) < min_sample) {
    return()
  }
  fit <- lm(sprintf("%s ~ .", names(y)[[2]]), dat)
  cor(dat[, -ncol(dat)], dat[, ncol(dat)])^2 |>
    as_tibble(
      rownames = "variable",
      .name_repair = ~ "raw_r2"
    ) |>
    full_join(
      olsrr::ols_step_both_r2(fit)$metrics |>
        mutate(
          variable = str_remove_all(variable, "`"),
          r2 = r2 - lag(r2, default = 0)
        ) |>
        select(variable, r2),
      by = "variable"
    ) |>
    full_join(
      relaimpo::calc.relimp(fit)@lmg |>
        enframe("variable", "lmg"),
      by = "variable"
    )
}

list(
  tarchetypes::tar_file_read(
    config_subjects,
    "config/subjects.csv",
    read = read_csv(!!.x, show_col_types = FALSE)
  ),
  tar_target(subjects, config_subjects$name[1:9]),
  tarchetypes::tar_files_input(
    files_academy,
    fs::dir_ls("data", regexp = "academy_.*\\.tsv$")
  ),
  tar_target(
    scores_grade,
    read_tsv(files_academy, col_types = cols(user_id = "I")) |>
      select(user_id, school, any_of(subjects)) |>
      mutate(across(any_of(subjects), as.numeric)),
    pattern = map(files_academy)
  ),
  tarchetypes::tar_files_input(
    files_cognition,
    fs::dir_ls("data", regexp = "cognition_.*\\.tsv$")
  ),
  tar_target(
    scores_cognition,
    read_tsv(files_cognition, col_types = cols(user_id = "I")) |>
      mutate(across(!user_id:class_name, ~ if_else(.x < -100, NA, .x))),
    pattern = map(files_cognition)
  ),
  tar_target(
    results,
    scores_grade |>
      select(user_id, school, all_of(subjects)) |>
      nest(.by = school) |>
      mutate(
        subject = subjects,
        metrics = data |>
          map(
            extract_metrics,
            select(scores_cognition, !user_name:class_name)
          ),
        .keep = "unused"
      ) |>
      unnest(metrics),
    pattern = map(head(subjects, 3))
  ),
  tar_target(
    prop_finished,
    scores_cognition |>
      summarise(across(!user_id:class_name, \(x) mean(!is.na(x)))) |>
      unlist()
  ),
  tar_target(
    data_combined,
    scores_grade |>
      inner_join(
        select(scores_cognition, !user_name:class_name),
        by = "user_id"
      )
  ),
  tar_target(
    fits, {
      terms <- str_c(
        "`", names(which(prop_finished > 0.3)), "`",
        collapse = " + "
      )
      lmer(
        str_glue("{subjects} ~ {terms} + ({terms} | school)"),
        data_combined
      )
    },
    pattern = map(head(subjects, 3)),
    iteration = "list"
  )
)
