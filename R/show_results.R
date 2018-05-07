#' Show simplex results
#' Function to show simplex results. Sampled data, embedding dimensions, prediction
#' decays and nonlinearity

#' @param samp_input Output from run_simplex
#' @param nrun Iteration to sample

#' @export

show_results <- function(samp_input, nrun){
  #---------------------------------------
  #define multiplot funcion
  multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    library(grid)  

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)  

    numPlots = length(plots)  

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                      ncol = cols, nrow = ceiling(numPlots/cols))
    }  

   if (numPlots==1) {
      print(plots[[1]])  

    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))  

      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))  

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }  

  #---------------------------------------
  #Plot sampled time series
  d1 <- samp_input$samples %>% melt(id.vars = c('iter', 'pars', 'time'))
  d1$iter <- as.numeric(d1$iter)
  d1 <- d1 %>% filter(iter == nrun)
  p1 <- ggplot(d1) + geom_line(aes(x = time, y = value, colour = variable,
  	group = variable)) + ggtitle("Sampled data")

  #Simplex embedding dimension
  d2 <- samp_input$simplex_df
  d2 <- d2 %>% filter(iter == nrun)
  p2 <- ggplot(d2) + geom_line(aes(x = E, y = rho, colour = variable)) +
  	geom_hline(yintercept = 0, lty = 2) + ggtitle("Simplex embedding dimension")

  #Prediction decay
  d3 <- samp_input$pred_decay
  d3 <- d3 %>% filter(iter == nrun)
  p3 <- ggplot(d3) + geom_line(aes(x = tp, y = rho, colour = variable,
  	group = variable)) + geom_hline(yintercept = 0 , lty = 2) + 
    ggtitle("Prediction decay")

  #Nonlinearity
  d4 <- samp_input$nonlinear
  d4 <- d4 %>% filter(iter == nrun)
  p4 <- ggplot(d4) + geom_line(aes(x = theta, y = rho, colour = variable)) +
    geom_hline(yintercept = 0 , lty = 2) +
    ggtitle("Nonlinearity")
# browser()	
  multiplot(p1, p2, p3, p4)
}
