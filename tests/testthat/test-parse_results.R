library(mockery)

test_that("get_results_from_local returns data from local storage", {
  # arrange
  m1 <- mock("json_data")
  m2 <- mock("parsed_data")

  stub(get_results_from_local, "jsonlite::read_json", m1)
  stub(get_results_from_local, "parse_results", m2)

  # act
  actual <- get_results_from_local("file")

  # assert
  expect_called(m1, 1)
  expect_called(m2, 1)

  expect_args(m1, 1, "file", simplifyVector = FALSE)
  expect_args(m2, 1, "json_data")

  expect_equal(actual, "parsed_data")
})

test_that("parse_results converts results correctly", {
  m <- mock("patched_results")
  stub(parse_results, "patch_results", m)

  data <- list(
    population_variants = list("a", "b"),
    results = list(
      "a" = list(
        list(
          x = 1,
          model_runs = list(1, 2, 3)
        ),
        list(
          x = 2,
          model_runs = list(2, 3, 4)
        )
      ),
      b = list(
        list(
          x = 3,
          model_runs = list(3, 4, 5)
        ),
        list(
          x = 4,
          model_runs = list(4, 5, 6)
        )
      )
    )
  )

  expected <- list(
    population_variants = c("a", "b"),
    results = list(
      "a" = tibble::tibble(
        x = 1:2,
        model_runs = c(list(1:3), list(2:4))
      ),
      "b" = tibble::tibble(
        x = 3:4,
        model_runs = c(list(3:5), list(4:6))
      )
    )
  )

  # act
  actual <- parse_results(data)

  # assert
  expect_called(m, 1)
  expect_args(m, 1, expected)

  expect_equal(actual, "patched_results")
})

test_that("patch_results returns correct values", {
  # "1-7 days", "8-14 days", "15-21 days", "22+ days"
  # arrange
  r <- list(
    results = list(
      "tretspef_raw" = tibble::tribble(
        ~measure, ~pod, ~tretspef_raw, ~sitetret, ~baseline, ~principal, ~time_profiles, ~lwr_ci, ~median, ~upr_ci,
        "a", "op", "100", "s1", 1, 2, c(1, 2), 3, 4, 5
      ),
      "tretspef_raw+los_group" = tibble::tribble(
        ~measure, ~pod, ~tretspef_raw, ~sitetret, ~baseline, ~principal, ~time_profiles, ~lwr_ci, ~median, ~upr_ci, ~los_group, # nolint
        "a", "ip", "100", "s1", 1, 2, c(1, 2), 3, 4, 5, "0-day",
        "b", "ip", "100", "s1", 2, 3, c(3, 4), 4, 5, 6, "1-7 days",
        "a", "ip", "100", "s1", 3, 4, c(5, 6), 5, 6, 7, "8-14 days",
        "b", "ip", "100", "s1", 4, 5, c(7, 8), 6, 7, 8, "15-21 days",
        "a", "ip", "200", "s1", 2, 1, c(9, 0), 4, 5, 3, "22+ days",
        "b", "ip", "200", "s1", 3, 2, c(1, 3), 5, 6, 4, "0-day",
        "a", "ip", "200", "s1", 4, 3, c(2, 5), 6, 7, 5, "1-7 days",
        "b", "ip", "200", "s1", 5, 4, c(3, 7), 7, 8, 6, "8-14 days",
        "a", "ip", "100", "s2", 5, 4, c(4, 9), 4, 5, 3, "15-21 days",
        "b", "ip", "100", "s2", 4, 3, c(5, 0), 5, 6, 4, "22+ days",
        "a", "ip", "100", "s2", 3, 2, c(6, 2), 6, 7, 5, "0-day",
        "b", "ip", "100", "s2", 2, 1, c(7, 4), 7, 8, 6, "1-7 days",
        "a", "ip", "200", "s2", 4, 5, c(8, 6), 5, 6, 1, "8-14 days",
        "b", "ip", "200", "s2", 3, 4, c(9, 8), 6, 7, 2, "15-21 days",
        "a", "ip", "200", "s2", 2, 3, c(0, 0), 7, 8, 3, "22+ days",
        "b", "ip", "200", "s2", 1, 2, c(1, 9), 8, 9, 4, "0-day"
      )
    )
  )

  expected <- list(
    results = list(
      "tretspef_raw" = tibble::tribble(
        ~measure, ~pod, ~tretspef_raw, ~sitetret, ~baseline, ~principal, ~time_profiles, ~lwr_ci, ~median, ~upr_ci,
        "a", "op", "100", "s1", 1, 2, c(1, 2), 3, 4, 5,
        "a", "ip", "100", "s1", 4, 6, c(6, 8), 8, 10, 12,
        "b", "ip", "100", "s1", 6, 8, c(10, 12), 10, 12, 14,
        "a", "ip", "200", "s1", 6, 4, c(11, 5), 10, 12, 8,
        "b", "ip", "200", "s1", 8, 6, c(4, 10), 12, 14, 10,
        "a", "ip", "100", "s2", 8, 6, c(10, 11), 10, 12, 8,
        "b", "ip", "100", "s2", 6, 4, c(12, 4), 12, 14, 10,
        "a", "ip", "200", "s2", 6, 8, c(8, 6), 12, 14, 4,
        "b", "ip", "200", "s2", 4, 6, c(10, 17), 14, 16, 6
      ),
      "tretspef_raw+los_group" = r$results[["tretspef_raw+los_group"]] |>
        dplyr::mutate(
          dplyr::across(
            "los_group",
            \(.x) {
              forcats::fct_relevel(
                .x,
                c("0-day", "1-7 days", "8-14 days", "15-21 days", "22+ days")
              )
            }
          )
        ) |>
        dplyr::arrange(.data$pod, .data$measure, .data$sitetret, .data$los_group)
    )
  )

  # act
  actual <- patch_results(r)

  # assert
  expect_equal(actual, expected)
})

test_that("get_trust_sites returns the list of trust sites", {
  r <- list(
    results = list(
      default = tibble::tibble(sitetret = c("a", "a", "b", "b", "c", "c"))
    )
  )
  actual <- get_trust_sites(r)

  expect_equal(actual, c("a", "b", "c"))
})

test_that("get_available_aggregations gets the list of available aggregations", {
  r <- list(
    results = list(
      a = tibble::tibble(pod = c("ip_1", "ip_2", "op_1", "op_2")),
      b = tibble::tibble(pod = c("ip_1", "ip_2")),
      c = tibble::tibble(x = 1)
    )
  )

  actual <- get_available_aggregations(r)

  expect_equal(actual, list(ip = c("a", "b"), op = c("a")))
})

test_that("get_model_run_years gets the model run years", {
  r <- list(
    params = list(start_year = 1, end_year = 2)
  )

  actual <- get_model_run_years(r)

  expect_equal(actual, list(start_year = 1, end_year = 2))
})

test_that("get_variants gets the results", {
  r <- list(
    population_variants = c("a", "b", "c")
  )

  actual <- get_variants(r)

  expect_equal(actual, tibble::tibble(model_run = 1:2, variant = c("b", "c")))
})
