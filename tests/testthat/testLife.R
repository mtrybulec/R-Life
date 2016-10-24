library(life)

test_that("shift.left shifts a matrix left by 1", {
    m <- matrix(1:12, 4, 3)

    current <- shift.left(m)
    target <- matrix(c(5:12, rep(0, 4)), 4, 3)

    expect_equal(target, current)
})

test_that("shift.right shifts a matrix right by 1", {
    m <- matrix(1:12, 4, 3)

    current <- shift.right(m)
    target <- matrix(c(rep(0, 4), 1:8), 4, 3)

    expect_equal(target, current)
})

test_that("shift.up shifts a matrix up by 1", {
    m <- matrix(1:12, 4, 3)

    current <- shift.up(m)
    target <- matrix(c(2:4, 0, 6:8, 0, 10:12, 0), 4, 3)

    expect_equal(target, current)
})

test_that("shift.down shifts a matrix down by 1", {
    m <- matrix(1:12, 4, 3)

    current <- shift.down(m)
    target <- matrix(c(0, 1:3, 0, 5:7, 0, 9:11), 4, 3)

    expect_equal(target, current)
})

test_that("calculate.neighbors calculates neighbor counts for all cells", {
    m <- matrix(c(1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0), 4, 3)

    current <- calculate.neighbors(m)
    target <- matrix(c(1, 2, 2, 1, 4, 3, 3, 0, 2, 2, 3, 1), 4, 3)

    expect_equal(target, current)
})

test_that("calculate.generation calculates the next generation correctly", {
    m <- matrix(c(
        rep(0, 5),
        0, 0, 1, 0, 0,
        0, 0, 0, 1, 0,
        0, 1, 1, 1, 0,
        0, 0, 0, 0, 0 ,
        rep(0, 5)),
        5, 6)

    current <- calculate.generation(m)

    target <-matrix(c(
        rep(0, 5),
        0, 0, 0, 0, 0,
        0, 1, 0, 1, 0,
        0, 0, 1, 1, 0,
        0, 0, 1, 0, 0,
        rep(0, 5)),
        5, 6)

    expect_equal(target, current)

    current <- calculate.generation(current)

    target <-matrix(c(
        rep(0, 5),
        0, 0, 0, 0, 0,
        0, 0, 0, 1, 0,
        0, 1, 0, 1, 0,
        0, 0, 1, 1, 0 ,
        rep(0, 5)),
        5, 6)

    expect_equal(target, current)
})
