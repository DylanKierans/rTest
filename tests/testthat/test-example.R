testthat::test_that("Example", {
    expect_equal(2,2)
    expect_error(x -> 1)
})

testthat::test_that("Valid structs", {
    expect_equal(test__struct_size(), 0)
})