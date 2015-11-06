source("life.R")

test.shift.left <- function()
{
    m <- matrix(1:12, 4, 3)
    
    current <- shift.left(m)
    target <- matrix(c(5:12, rep(0, 4)), 4, 3)
    
    checkEquals(target, current)
}

test.shift.right <- function()
{
    m <- matrix(1:12, 4, 3)
    
    current <- shift.right(m)
    target <- matrix(c(rep(0, 4), 1:8), 4, 3)
    
    checkEquals(target, current)
}

test.shift.up <- function()
{
    m <- matrix(1:12, 4, 3)
    
    current <- shift.up(m)
    target <- matrix(c(2:4, 0, 6:8, 0, 10:12, 0), 4, 3)
    
    checkEquals(target, current)
}

test.shift.down <- function()
{
    m <- matrix(1:12, 4, 3)
    
    current <- shift.down(m)
    target <- matrix(c(0, 1:3, 0, 5:7, 0, 9:11), 4, 3)
    
    checkEquals(target, current)
}
