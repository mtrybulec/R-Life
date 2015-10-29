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

# Main function:
life <- function(size, ngen = 1000, update.freq = 10)
{
    # Two additional rows and columns will be used to 'zero' the grid at its borders:
    rows <- size + 2
    cols <- size + 2
    
    board <- matrix(data = rbinom(rows * cols, 1, 0.5), nrow = rows, ncol = cols)

    for(gen in 1:ngen)
    {
        # 'Zero' the borders of the grid:
        board[1, ] <- 0
        board[, 1] <- 0
        board[rows, ] <- 0
        board[, cols] <- 0
        
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
        
        # Calculate neighbor counts:
        shifted.left <- shift.left(board)
        shifted.right <- shift.right(board)
        
        neighbors <- 
            shift.up(shifted.left) + shift.up(board) + shift.up(shifted.right) +
            shifted.left + shifted.right +
            shift.down(shifted.left) + shift.down(board) + shift.down(shifted.right)
        
        remaining <- neighbors == 2
        creating <- neighbors == 3

        # Calculate new generation:
        board <- (board * remaining) + creating
    }
}