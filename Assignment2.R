# Mostapha A
# CST8233
# Assignment 2

# function that prints the first menu and validates input
menuOne <- function(){
  # set variable for loop
  menuOption <- "3"
  
  # print menu
  writeLines("MENU\n 1. Exponential Fit\n 2. Quit\n")
  # take input
  menuOption <- readline(prompt = "Please enter your choice: ")
  # if it is not 1 or 2 ask again
  while (menuOption != "1" && menuOption != "2"){
    writeLines("You must enter \"1\" or \"2\", try again.")
    menuOption <- readline(prompt = "Please enter your choice: ")
  }
  
  # return validated input
  return(menuOption)
}

# function that prints the second menu and validates input
menuTwo <- function(){
  # set variable for loop
  menuOption <- "3"
  
  # print menu
  writeLines("MENU\n 1. Extrapolation\n 2. Main Menu\n")
  # take input
  menuOption <- readline(prompt = "Please enter your choice: ")
  # if it is not 1 or 2 ask again
  while (menuOption != "1" && menuOption != "2"){
    writeLines("You must enter \"1\" or \"2\", try again.")
    menuOption <- readline(prompt = "Please enter your choice: ")
  }
  
  # return validated input
  return(menuOption)
}

# function that find the formula
formula <- function(xVals, yVals, numN){
  # variables for calculation
  i <- 1
  xSum <- 0
  ySum <- 0
  xySum <- 0
  xSqSum <- 0
  
  # transform y values as necessary (ln(y))
  zVals <- log(yVals, base = exp(1))
  xyVals <- xVals * zVals
  xSqVals <- xVals * xVals
  
  # calculate sums for a0 and a1
  xSum <- sum(xVals)
  ySum <- sum(zVals)
  xySum <- sum(xyVals)
  xSqSum <- sum(xSqVals)
  
  # calculate constants a0 and a1
  a1 <- ( (numN * xySum) - (xSum * ySum) ) / ( (numN * xSqSum) - (xSum * xSum) )
  a0 <- (ySum / numN) - ( a1 * (xSum / numN) )
  
  # calculate a and b
  a <- exp(a0) 
  b <- a1
  
  # print the equation
  print("The best fit is of the form:", quote = FALSE)
  print(paste("TV =", a, "x e ^ (", b, "x t )"), quote = FALSE)
  
  # calculated fitted values
  yFit <- vector(mode = "numeric", length = numN)
  i <- 1
  while (i <= numN){
    yFit[i] <- (a * exp(b * xVals[i]))
    i <- i + 1
  }
  
  # create dataframe to return
  ab <- vector(length = numN)
  ab[1] <- a
  ab[2] <- b
  valsCalcDF<- data.frame("a.b.date" = c(ab),"x" = c(xVals),"Z" = c(zVals), "y.observed" = c(yVals), "y.fitted" = c(yFit))
  
  return(valsCalcDF)
}

# function that finds the extrapolation
extrapolation <- function(a, b, startDate){
  startDate <- strptime(as.character(startDate), "%Y-%m-%d")
  startDate <- format(startDate, "%d/%m/%Y")
  startDate <- as.character(startDate)
  startDate <- as.Date(startDate, format = "%d/%m/%Y")
  
  # ask for date
  extrapolateDate <- readline(prompt = "Please enter the date to extrapolate to (dd/mm/yyyy): ")
  extrapolateDate <- as.Date(extrapolateDate, format = "%d/%m/%Y")
  
  formattedDate <- extrapolateDate
  formattedDate <- strptime(as.character(formattedDate), "%Y-%m-%d")
  formattedDate <- format(formattedDate, "%d/%m/%Y")
  formattedDate <- as.character(formattedDate)
  
  # find x based on start date  
  x <- as.Date(extrapolateDate, format = "%d/%m/%Y") - as.Date(startDate, format = "%d/%m/%Y")
  x <- as.double(x)
  b <- as.double(b)
  a <- as.double(a)
  
  # calculate
  y <- (a * exp(b * x))
  print(paste("y = ",a,"x e ^ (",b,"x",x, ") =", y), quote = FALSE)
  
  # print the calculated total vaccinations
  print(paste("Total vaccinations on", formattedDate, "is", y), quote = FALSE)
}

# function that finds the exponential fit
exponentialFit <- function(){
  filename <- "file.file"
  startDate <- "00/00/0000"
  endDate <- "00/00/0000"
  
  # ask for filename
  filename <- readline(prompt = "Please enter the name of the file to open: ")
  
  # check if it exists
  while (!file.exists(filename)){
    # print error message, try again
    print(paste("File", filename, "does not exist in", getwd(), "try again."), quote = FALSE)
    # ask for filename
    filename <- readline(prompt = "Please enter the name of the file to open: ")
  }
  
  # read file
  COVID19_data <- read_excel(filename)
  
  # rewrite date in proper date format and count data
  i <- nrow(COVID19_data)
  COVID19_data$date <- strptime(as.character(COVID19_data$date), "%Y-%m-%d")
  COVID19_data$date <- format(COVID19_data$date, "%d/%m/%Y")
  
  # store the last date
  finalDate <- COVID19_data$date[i]
  formattedDate <- finalDate
  
  # ask for start date
  startDate <- readline(prompt = "Please enter the start date (dd/mm/yyyy): ")
  startDate <- as.Date(startDate, format = "%d/%m/%Y")
  # validate start date
  while (as.Date(startDate, format = "%d/%m/%Y") < as.Date("01/02/2021", format = "%d/%m/%Y") || as.Date(startDate, format = "%d/%m/%Y") > as.Date("01/05/2021", format = "%d/%m/%Y")){
    # print error and try again
    writeLines("Start date must be between 01/02/2021 to 01/05/2021. Try again.")
    startDate <- readline(prompt = "Please enter the start date (dd/mm/yyyy): ")
    startDate <- as.Date(startDate, format = "%d/%m/%Y")
  }
  
  # ask for end date
  endDate <- readline(prompt = "Please enter the end date (dd/mm/yyyy): ")
  endDate <- as.Date(endDate, format = "%d/%m/%Y")
  # validate end date
  while (as.Date(endDate, format = "%d/%m/%Y") < (as.Date(startDate, format = "%d/%m/%Y") + 60) || as.Date(endDate, format = "%d/%m/%Y") > as.Date(finalDate, format = "%d/%m/%Y")){
    # print error and try again
    print(paste("End date must be at least 60 days after start date and before", formattedDate, ". Try again."), quote = FALSE )
    endDate <- readline(prompt = "Please enter the end date (dd/mm/yyyy): ")
    endDate <- as.Date(endDate, format = "%d/%m/%Y")
  }
  
  # find exponential fit and print function
  # store the x and y vals in the specified date
  xVals <- vector(mode = "numeric", length = i)
  yVals <- vector(mode = "numeric", length = i)
  i <- 1
  j <- 1
  for (date in COVID19_data$date){
    # if date is between the range store values
    if (as.Date(date, format = "%d/%m/%Y") >= as.Date(startDate, format="%d/%m/%Y") && as.Date(date, format="%d/%m/%Y") <= as.Date(endDate, format = "%d/%m/%Y")){
      # store total cases and days
      xVals[j] <- as.Date(COVID19_data$date[i], format = "%d/%m/%Y") - as.Date(startDate, format = "%d/%m/%Y")
      yVals[j] <- COVID19_data$total_vaccinations[i]
      # increment j, counter for values in range
      j <- j + 1
    }
    # increment i, counter for total values
    i <- i + 1
  }
  j <- j - 1
  
  # cut the vectors to the stored values
  xVals <- xVals[1:j]
  yVals <- yVals[1:j]
  
  # call function for formula, store calculated information
  valsCalcDF <- formula(xVals, yVals, j)
  # store start and end date
  valsCalcDF$a.b.date[3] <- as.character(startDate)
  valsCalcDF$a.b.date[4] <- as.character(endDate)
  
  # return info
  return(valsCalcDF)
}

# function that plots the graph and exports it
plotGraph <- function(valsCalcDF){
  # plot graph in r studio
  plot(valsCalcDF$x, valsCalcDF$y.observed , type="l", lty=2, col="black", main="Total Vaccinations vs. Days", sub="Observed in black dashed line vs. Fitted in solid blue",
       xlab="Days", ylab="Total Vaccinations")
  lines(valsCalcDF$x, valsCalcDF$y.fitted, col="blue")
  
  # create a file for graph
  pdf(file = "total_vacc.pdf", width = 8.5, height = 11)
  
  # plot graph
  plot(valsCalcDF$x, valsCalcDF$y.observed , type="l", lty=2, col="black", main="Total Vaccinations vs. Days", sub="Observed in black dashed line vs. Fitted in solid blue",
       xlab="Days", ylab="Total Vaccinations")
  lines(valsCalcDF$x, valsCalcDF$y.fitted, col="blue")
  
  # finish creating file, print it was saved
  dev.off()
  print(paste("Graph was saved in your working directory", getwd(), "as total_vacc.pdf"), quote = FALSE)
}

# function that loops through main menu and calls necessary functions
bestFitFun <- function(){
  # required library
  # install.packages("readxl")
  require("readxl")
  # variables
  filename <- "assignment2.xlsx"
  menuOption <- "3"
  # create dataframe with all information for verification
  valsCalcDF<- data.frame("a.b.date" = c(NA,NA),"x" = c(NA,NA),"Z" = c(NA,NA), "y.observed" = c(NA,NA), "y.fitted" = c(NA,NA))
  
  # loop through main menu
  while(menuOption != "2"){
    # get first menu option, exponential or quit
    menuOption <- menuOne()
    
    # if they select one for Exponential Fit
    if (menuOption == "1"){
      # call function and store information
      valsCalcDF <- exponentialFit()
      
      # plot graph
      plotGraph(valsCalcDF)
      
      # loop through second menu for extrapolation
      while(menuOption != "2"){
        # print second menu extrapolate or main menu
        menuOption <- menuTwo()
        # return to main menu if 2 otherwise do the extrapolation
        if (menuOption == "1") {
          # call function for extrapolation
          extrapolation(valsCalcDF$a.b.date[1], valsCalcDF$a.b.date[2], valsCalcDF$a.b.date[3])
        }
      } # extrapolation menu
      # reset main menu variable to loop again
      menuOption <- "3"
    }
  } # main menu loop
  print("Good Bye.", quote = FALSE)
}

# call function
bestFitFun()

