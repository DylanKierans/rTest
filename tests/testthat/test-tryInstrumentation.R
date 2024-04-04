

testthat::test_that("Incorrect order of function calls", {
    expect_error({
        ## Throw error due to instrumentation_init not called
        instrument_all_functions()
    })

    expect_error({
        ## Throw error due to instrumentation_init not called
        instrumentation_finalize()
    })

    expect_error({
        ## Throw error due to double init call
        instrumentation_init()
        instrumentation_init()
    })
})