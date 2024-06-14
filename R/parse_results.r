#' Get results from local
#' @param filename Filename for model results JSON
#' @export
#' @examples
#' filename <- system.file("sample_results.json", package = "nhp.results")
#' results <- get_results_from_local(filename)
get_results_from_local <- function(filename) {
  jsonlite::read_json(filename, simplifyVector = FALSE) |>
    parse_results()
}

parse_results <- function(r) {
  r$population_variants <- as.character(r$population_variants)

  r$results <- purrr::map(
    r$results,
    purrr::map_dfr,
    purrr::modify_at,
    c("model_runs", "time_profiles"),
    purrr::compose(list, as.numeric)
  )

  patch_results(r)
}

#' Patch results
#' @param r The results list
#' Should be of length 3 with elements: `params`, `population_variants` and `results`
#' @importFrom rlang .data
patch_results <- function(r) {
  r$results[["tretspef_raw"]] <- dplyr::bind_rows(
    r$results[["tretspef_raw"]],
    r$results[["tretspef_raw+los_group"]] |>
      dplyr::summarise(
        .by = c("measure", "pod", "tretspef_raw", "sitetret"),
        dplyr::across(
          c("baseline", "principal", "lwr_ci", "median", "upr_ci"),
          sum
        ),
        dplyr::across("time_profiles", \(.x) list(purrr::reduce(.x, `+`)))
      )
  )

  r$results[["tretspef_raw+los_group"]] <- r$results[["tretspef_raw+los_group"]] |>
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

  r
}

#' Get trust sites
#' @inheritParams patch_results
#' @returns A vector of site names
#' @export
#' @examples
#' filename <- system.file("sample_results.json", package = "nhp.results")
#' results <- get_results_from_local(filename)
#' get_trust_sites(results)
get_trust_sites <- function(r) {
  r$results$default$sitetret |>
    sort() |>
    unique()
}

#' Get available aggregations
#' @inheritParams patch_results
#' @returns A list of the possible aggregations
#' @export
#' @examples
#' filename <- system.file("sample_results.json", package = "nhp.results")
#' results <- get_results_from_local(filename)
#' get_trust_sites(results)
get_available_aggregations <- function(r) {
  r$results |>
    purrr::keep(\(.x) "pod" %in% colnames(.x)) |>
    purrr::map(
      \(.x) {
        .x |>
          dplyr::pull("pod") |>
          stringr::str_extract("^[a-z]*") |>
          unique()
      }
    ) |>
    tibble::enframe() |>
    tidyr::unnest("value") |>
    dplyr::group_by(.data$value) |>
    dplyr::summarise(dplyr::across("name", list)) |>
    tibble::deframe()
}

#' Get model_run_years
#' @inheritParams patch_results
#' @returns A list of 2, containing `start_year` and `end_year`
#' @export
#' @examples
#' filename <- system.file("sample_results.json", package = "nhp.results")
#' results <- get_results_from_local(filename)
#' get_model_run_years(results)
get_model_run_years <- function(r) {
  r$params[c("start_year", "end_year")]
}

#' Get variants
#' Get the population variants
#' @inheritParams patch_results
#' @returns A tibble with two columns, `model_run` & `variant`
#' @export
#' @examples
#' filename <- system.file("sample_results.json", package = "nhp.results")
#' results <- get_results_from_local(filename)
#' get_variants(results)
get_variants <- function(r) {
  r$population_variants |>
    utils::tail(-1) |>
    tibble::enframe("model_run", "variant")
}
