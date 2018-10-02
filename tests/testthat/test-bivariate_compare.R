context("bivariate_compare")

test_that("displays mean (sd) for normal vars", {
  test_display <- paste0(
    round(mean(mtcars$hp[mtcars$cyl == 4]), 2), " (",
    round(sd(mtcars$hp[mtcars$cyl == 4]), 2), ")"
  )

  test_compare <- bivariate_compare(df = mtcars, compare = "cyl", normal_vars = "hp") %>%
    pull(`4 (n = 11)`)

  expect_equal(test_display, test_compare)
})


test_that("displays median (iqr) for non-normal vars", {
  test_display <- paste0(
    round(median(mtcars$hp[mtcars$cyl == 4]), 2), " (",
    round(IQR(mtcars$hp[mtcars$cyl == 4]), 2), ")"
  )

  test_compare <- bivariate_compare(df = mtcars, compare = "cyl", non_normal_vars = "hp") %>%
    pull(`4 (n = 11)`)

  expect_equal(test_display, test_compare)
})

test_that("displays n (%) for categorical vars", {
  test_display <- paste0(
    length(mtcars$am[mtcars$cyl == 4 & mtcars$am == 0]), " (",
    round(
      length(mtcars$am[mtcars$cyl == 4 & mtcars$am == 0]) /
        length(mtcars$am[mtcars$cyl == 4]) * 100,
      0
    ), "%)"
  )

  test_compare <- suppressWarnings(
    bivariate_compare(df = mtcars, compare = "cyl", cat_vars = "am")
    ) %>%
    slice(1) %>%
    pull(`4 (n = 11)`)

  expect_equal(test_display, test_compare)
})


test_that("displays aov p value for normal vars", {
  test_display_exact <- paste0(
    round(summary(aov(mtcars$disp ~ as.factor(mtcars$carb)))[[1]][["Pr(>F)"]][[1]], 4)
  )

  test_compare_exact <- bivariate_compare(df = mtcars, compare = "carb", normal_vars = "disp") %>%
    pull(p.value)

  expect_equal(test_display_exact, test_compare_exact)

  test_display_small <- case_when(
    summary(aov(mtcars$disp ~ as.factor(mtcars$cyl)))[[1]][["Pr(>F)"]][[1]] < 1e-4 ~
      "< 0.0001",
    TRUE ~ paste0(
      summary(aov(mtcars$disp ~ as.factor(mtcars$cyl)))[[1]][["Pr(>F)"]][[1]]
    )
  )

  test_compare_small <- bivariate_compare(df = mtcars, compare = "cyl", normal_vars = "disp") %>%
    pull(p.value)

  expect_equal(test_display_small, test_compare_small)
})


test_that("displays kruskall p value for non-normal vars", {
  test_display_exact <- paste0(
    round(kruskal.test(mtcars$disp ~ as.factor(mtcars$carb))$p.value, 4),
    "^a^"
  )

  test_compare_exact <- bivariate_compare(df = mtcars, compare = "carb", non_normal_vars = "disp") %>%
    pull(p.value)

  expect_equal(test_display_exact, test_compare_exact)

  test_display_small <- case_when(
    kruskal.test(mtcars$disp ~ as.factor(mtcars$cyl))$p.value < 1e-4 ~
      "< 0.0001^a^",
    TRUE ~ paste0(
      round(kruskal.test(mtcars$disp ~ as.factor(mtcars$cyl))$p.value, 4),
      "^a^"
    )
  )

  test_compare_small <- bivariate_compare(df = mtcars, compare = "cyl", non_normal_vars = "disp") %>%
    pull(p.value)

  expect_equal(test_display_small, test_compare_small)
})


test_that("displays chisq p value for cat vars", {
  test_display_exact <- paste0(
    suppressWarnings(
      round(chisq.test(mtcars$disp, as.factor(mtcars$carb))$p.value, 4)
    )
  )


  test_compare_exact <- suppressWarnings(
    bivariate_compare(df = mtcars, compare = "carb", cat_vars = "disp") %>%
      slice(1) %>%
      pull(p.value)
  )

  expect_equal(test_display_exact, test_compare_exact)

  test_display_small <- case_when(
    suppressWarnings(
     chisq.test(mtcars$vs, as.factor(mtcars$cyl))$p.value
    ) < 1e-4 ~
      "< 0.0001",
    TRUE ~ paste0(
      suppressWarnings(
        round(chisq.test(mtcars$vs, as.factor(mtcars$cyl))$p.value, 4)
      )
    )
  )

  test_compare_small <- suppressWarnings(
    bivariate_compare(df = mtcars, compare = "cyl", cat_vars = "vs") %>%
      slice(1) %>%
      pull(p.value)
  )

  expect_equal(test_display_small, test_compare_small)
})

test_that("displays fisher p value for cat vars if requested", {
  test_display_exact <- paste0(
    suppressWarnings(
      round(fisher.test(mtcars$disp, as.factor(mtcars$gear))$p.value, 4)
    )
  )


  test_compare_exact <- suppressWarnings(
    bivariate_compare(df = mtcars, compare = "gear", cat_vars = "disp",
                      fisher = TRUE) %>%
      slice(1) %>%
      pull(p.value)
  )

  expect_equal(test_display_exact, test_compare_exact)

  test_display_small <- case_when(
    suppressWarnings(
      fisher.test(mtcars$vs, as.factor(mtcars$cyl))$p.value
    ) < 1e-4 ~
      "< 0.0001",
    TRUE ~ paste0(
      suppressWarnings(
        round(fisher.test(mtcars$vs, as.factor(mtcars$cyl))$p.value, 4)
      )
    )
  )

  test_compare_small <- suppressWarnings(
    bivariate_compare(df = mtcars, compare = "cyl", cat_vars = "vs",
                      fisher = TRUE) %>%
      slice(1) %>%
      pull(p.value)
  )

  expect_equal(test_display_small, test_compare_small)
})
