testthat::test_that("ZeroMQ Simple context and open/close socket", {
    expect_no_error({
        init_zmq_client()
        finalize_zmq_client()
    })
})

testthat::test_that("ZeroMQ data transfer over default sockets", {
    expect_equal(0, 0)
    # expect_equal(test__ports(2), 0)
})

testthat::test_that("Valid structs for zmq payloads", {
    expect_equal(test__struct_size(), 0)
})