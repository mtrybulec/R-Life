# Helper matrix functions:
shift.left <- function(m)
{
    m <- m[, 2:ncol(m)]
    cbind(m, rep(0, nrow(m)))
}

shift.right <- function(m)
{
    m <- m[, 1:ncol(m) - 1]
    cbind(rep(0, nrow(m)), m)
}

shift.up <- function(m)
{
    m <- m[2:nrow(m), ]
    rbind(m, rep(0, ncol(m)))
}

shift.down <- function(m)
{
    m <- m[1:nrow(m) - 1, ]
    rbind(rep(0, ncol(m)), m)
}

zero.borders <- function(m)
{
    m[1, ] <- 0
    m[, 1] <- 0
    m[nrow(m), ] <- 0
    m[, ncol(m)] <- 0

    m
}

calculate.neighbors <- function(m)
{
    shifted.up <- shift.up(m)
    shifted.down <- shift.down(m)
    
    neighbors <- 
        shift.left(shifted.up) + shifted.up + shift.right(shifted.up) +
        shift.left(m) + shift.right(m) +
        shift.left(shifted.down) + shifted.down + shift.right(shifted.down)

    neighbors    
}

calculate.generation <- function(cur.gen)
{
    neighbors <- calculate.neighbors(cur.gen)
    
    remaining <- neighbors == 2
    creating <- neighbors == 3
    
    next.gen <- (cur.gen * remaining) + creating
    zero.borders(next.gen)
}

# Main function:
life <- function(size, ngen = 1000, update.freq = 10)
{
    # Two additional rows and columns will be used to 'zero' the grid at its borders:
    rows <- size + 2
    cols <- size + 2
    
    board <- matrix(data = rbinom(rows * cols, 1, 0.5), nrow = rows, ncol = cols)
    board <- zero.borders(board)

    for(gen in 1:ngen)
    {
        # Plotting the grid is quite slow; to speed things up, 
        # the plot is refreshed every update.freq generations:
        if(gen %% update.freq == 0)
        {
            rowindexes <- board * row(board)
            colindexes <- board * col(board)
            
            rowindexes <- as.list(rowindexes)[rowindexes > 0]
            colindexes <- as.list(colindexes)[colindexes > 0]
            
            plot(colindexes, 
                 rowindexes, 
                 xlim = c(2, rows - 1), 
                 ylim = c(2, cols - 1), 
                 pch = 20, 
                 main = paste(c("Generation", gen)))
            Sys.sleep(1)
        }
        
        board <- calculate.generation(board)
    }
}