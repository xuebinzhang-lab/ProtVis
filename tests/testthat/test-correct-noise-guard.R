test_that("correct_values is safe for matrices without replicate columns", {
  no_replicates <- data.frame(
    ID = c("P1", "P2"), SampleA = c(1, 0), SampleB = c(2, 3),
    check.names = FALSE
  )
  expect_identical(correct_values(no_replicates), no_replicates)

  id_only <- data.frame(ID = c("P1", "P2"), check.names = FALSE)
  expect_identical(correct_values(id_only), id_only)
})

test_that("correct_values still corrects zero replicate values", {
  input <- data.frame(
    ID = "P1", Group_1 = 10, Group_2 = 0, Group_3 = 20,
    check.names = FALSE
  )
  result <- correct_values(input)
  expect_equal(result$Group_2, 15)

  prefix_input <- data.frame(
    ID = "P1", `1_Group` = 10, `2_Group` = 0, `3_Group` = 20,
    check.names = FALSE
  )
  prefix_result <- correct_values(prefix_input)
  expect_equal(prefix_result$`2_Group`, 15)
})
