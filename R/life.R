# Matrix helper functions:

shift.left <- function(m)
{
    m <- m[, -1]
    cbind(m, rep(0, nrow(m)))
}

shift.right <- function(m)
{
    m <- m[, -ncol(m)]
    cbind(rep(0, nrow(m)), m)
}

shift.up <- function(m)
{
    m <- m[-1, ]
    rbind(m, rep(0, ncol(m)))
}

shift.down <- function(m)
{
    m <- m[-nrow(m), ]
    rbind(rep(0, ncol(m)), m)
}

# Life functions:

calculate.neighbors <- function(board)
{
    shifted.up <- shift.up(board)
    shifted.down <- shift.down(board)

    neighbors <-
        shift.left(shifted.up) + shifted.up + shift.right(shifted.up) +
        shift.left(board) + shift.right(board) +
        shift.left(shifted.down) + shifted.down + shift.right(shifted.down)
}

calculate.generation <- function(board)
{
    neighbors <- calculate.neighbors(board)

    remaining <- neighbors == 2
    creating <- neighbors == 3

    (board * remaining) + creating
}

plot.board <- function(board, gen)
{
    row.indexes <- board * row(board)
    col.indexes <- board * col(board)

    row.indexes <- as.list(row.indexes)[row.indexes > 0]
    col.indexes <- as.list(col.indexes)[col.indexes > 0]

    smoothScatter(
        col.indexes,
        row.indexes,
        xlim = c(1, nrow(board)),
        ylim = c(1, ncol(board)),
        xlab = "",
        ylab = "",
        pch = 20,
        main = paste(c("Generation", gen)),
        bandwidth = 2,
        nrpoints = Inf)

    # Forces a refresh of the plot:
    Sys.sleep(1)
}

# Main function:

#' Run Conway's Game of Life.
#'
#' \code{life} runs Conway's Game of Life, using standard rules.
#'
#' @param size (numeric) the size of the board (the length of its sides).
#' @param ngen (numeric) the number of generations to run.
#' @param update.freq (numeric) specifies the number of generations
#'    between plot updates (since plot updates are by far the slowest here,
#'    plotting results too often can become quite slow).
#'
#' @export
life <- function(size, ngen = 1000, update.freq = 10)
{
    if (missing(size)) {
        stop("size argument missing - specify the size of the board (the length of its sides).")
    }

    board <- matrix(data = rbinom(size * size, 1, 0.5), nrow = size, ncol = size)

    for(gen in 1:ngen)
    {
        # Plotting the grid is quite slow; to speed things up,
        # the plot is refreshed every update.freq generations:
        if(gen %% update.freq == 0)
        {
            plot.board(board, gen)
        }

        board <- calculate.generation(board)
    }
}
