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

test.zero.borders <- function()
{
    m <- matrix(1:12, 4, 3)
    
    current <- zero.borders(m)
    target <- matrix((c(rep(0, 4), 0, 6, 7, 0, rep(0, 4))), 4, 3)

    checkEquals(target, current)
}

test.calculate.neighbors <- function()
{
    m <- matrix(c(1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0), 4, 3)

    current <- calculate.neighbors(m)
    target <- matrix(c(1, 2, 2, 1, 4, 3, 3, 0, 2, 2, 3, 1), 4, 3)

    checkEquals(target, current)
}

test.calculate.generation <- function()
{
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

    checkEquals(target, current)

    current <- calculate.generation(current)
    
    target <-matrix(c(
        rep(0, 5), 
        0, 0, 0, 0, 0,
        0, 0, 0, 1, 0,
        0, 1, 0, 1, 0, 
        0, 0, 1, 1, 0 ,
        rep(0, 5)),
        5, 6)

    checkEquals(target, current)
}