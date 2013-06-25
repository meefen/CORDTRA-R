library(reshape2)
library(ggplot2)

## Read settings
settings <- read.table("setting.txt", header = FALSE, stringsAsFactors=FALSE)
dataFile <- as.character(settings[1, 1])
# start <- as.numeric(settings[2, 1])
# end <- as.numeric(settings[3, 1])

## Read data
df <- read.csv(dataFile, stringsAsFactors=FALSE)
# sort
# df <- df[order(strptime(df$time, "%m/%d/%Y %H:%M:%S")), ]
df$sortedid <- 1:nrow(df)
# clean data
df$WoC <- factor(df$WoC, levels=c("Q", "T", "E", "S", "D", "R"), 
                 labels=c("Questioning", "Theorizing", "Evidence", 
                          "Synthesizing", "Discussion", "Reflection"))
df$PROBLEMS <- factor(df$PROBLEMS, levels=c("E", "F"), 
                      labels=c("Explanatory", "Factual"))
df$COMP <- factor(df$COMP, levels=1:4, 
                  labels=c("COMP-1", "COMP-2", "COMP-3", "COMP-4"))
df$SCI <- factor(df$SCI, levels=1:4, 
                  labels=c("SCI-1", "SCI-2", "SCI-3", "SCI-4"))
# melt data
df.sub <- subset(df, select=c(sortedid, WoC, PROBLEMS, COMP, SCI))
df.melt <- melt(df.sub, id.vars=c("sortedid"))
names(df.melt)[2] <- "schemes"; names(df.melt)[3] <- "categories"
df.melt <- na.omit(df.melt)
alllevels <- c(levels(df$WoC), levels(df$PROBLEMS), levels(df$COMP), levels(df$SCI))
df.melt$categories <- factor(df.melt$categories, levels=alllevels)

# Returns a logical vector of which values in `x` are 
# within the min and max values of `range`.
in_range <- function(x, range) {
  x >= min(range) & x <= max(range)
}

shinyServer(function(input, output) {
  
  # View count
  viewCount <- as.numeric(read.table("viewFile.txt", header = FALSE)[1, 1]) + 1
  write(viewCount, file = "viewFile.txt")
  
  limit_data_range <- function() {
    # ------------------------------------------------------------------
    # Because we're using reactiveUI for x_range and y_range, they start
    # out as null, then get resolved after the client and server talk a bit.
    # If they are not yet set, there will be some errors in this function, so
    # do nothing for now (this function will be run again).
    if (is.null(input$x_range)) {
      return(NULL)
    }
    
    # ------------------------------------------------------------------
    # Limit range of data
    # 1. Take a subset of the data, respecting the limited range
    df.melt.sub <- df.melt[in_range(df.melt$sortedid, input$x_range), ]
    # 2. Filter categories
    df.melt.sub <- df.melt.sub[df.melt.sub$categories %in% input$categories, ]
    # df.melt.sub$categories <- df.melt.sub$categories[1:length(df.melt.sub$categories), drop=TRUE]
    
    df.melt.sub
  }

  ## Main plot
  output$main_plot <- renderPlot({
    
    df.melt.sub <- limit_data_range()
    if (is.null(df.melt.sub))
      return()
    
    p <- ggplot(df.melt.sub) + 
      geom_point(aes(x=sortedid, y=categories, shape=categories, colour=schemes)) + 
      scale_shape_manual(values=1:length(alllevels)) + 
      xlab("Discourse Units") + ylab("Coding Categories")
    print(p)
  })
  
  # ------------------------------------------------------------------
  # Create renderUI sliders for x and y range, because their limits
  # depend on the selected x and y variables.
  
  output$x_range_slider <- renderUI({
    xmin <- floor(min(df$sortedid))
    xmax <- ceiling(max(df$sortedid))
    
    sliderInput(inputId = "x_range",
                label = "",
                min = xmin, max = xmax, value = c(xmin, xmax))
  })
  
  output$choose_categories <- renderUI({
    if (is.null(df.melt))
      return()
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("categories", "", 
                       choices  = alllevels,
                       selected = alllevels)
  })

})
