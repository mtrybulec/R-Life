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
}

# Main function:
life <- function(size, ngen = 1000, update.freq = 10)
{
    board <- matrix(data = rbinom(size * size, 1, 0.5), nrow = size, ncol = size)

    for(gen in 1:ngen)
    {
        # Plotting the grid is quite slow; to speed things up, 
        # the plot is refreshed every update.freq generations:
        if(gen %% update.freq == 0)
        {
            row.indexes <- board * row(board)
            col.indexes <- board * col(board)
            
            row.indexes <- as.list(row.indexes)[row.indexes > 0]
            col.indexes <- as.list(col.indexes)[col.indexes > 0]
            
            plot(col.indexes, 
                 row.indexes, 
                 xlim = c(1, nrow(board)), 
                 ylim = c(1, ncol(board)), 
                 pch = 20, 
                 main = paste(c("Generation", gen)))
            Sys.sleep(1)
        }
        
        board <- calculate.generation(board)
    }
}