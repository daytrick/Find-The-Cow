########## Find the Invisible Cow ##########
#Jan 18, 2022

#Game concept inspired by https://findtheinvisiblecow.com/ 

#Got started with Peter Prevos's LucidManager series on writing games in R: 
#   https://lucidmanager.org/data-science/games-in-r/
#
#Especially these games:
#   Gravedigger: https://github.com/pprevos/RGames/blob/master/gravedigger.R 
#   TicTacToe: https://github.com/pprevos/RGames/blob/master/tic-tac-toe/TicTacToe.R 
#   Pong: https://github.com/pprevos/RGames/blob/master/pong.R 



########## SET UP ##########
# @players you need to change this to wherever you've stored the R project
#setwd("<working directory>")



########## DEPENDENCIES ##########
#Beepr to play the moos: https://cran.r-project.org/web/packages/beepr/index.html
#   Funky documentation: https://cran.r-project.org/web/packages/beepr/beepr.pdf
library(beepr)
#PNG to display the cow: https://cran.r-project.org/web/packages/png/index.html 
library(png)



########## CREATE GAME WINDOW ##########
#Variables
width <- 7
height <- 7
bg_col <- rgb(0.5, 0.8, 0.5)
game_name <- "Find the Invisible Cow"

x11(width=width, height=height, bg=bg_col, title=game_name)
par("plt")
par(mar=c(0, 0, 0, 0))
plot.new()
#How to get plot to fill window from: https://www.stat.auckland.ac.nz/~ihaka/courses/787/lectures-r-graphics.pdf
plot.window(xlim=c(0,1), ylim=c(0,1), xaxs="i", yaxs="i")



########## SET UP ##########
#Place the cow
cowCoords <- list(x=runif(1, 0, 1), y=runif(1, 0, 1))


#Function to calculate distance from click to cow
#   a = sqrt(b^2 + c^2)
calcDistance <- function(click) {
  
  x2 <- (cowCoords$x - click$x) ^ 2
  y2 <- (cowCoords$y - click$y) ^ 2
  
  return(sqrt(x2 + y2))
  
}


#Function to get the furthest distance you can be from the cow
calcMaxDistance <- function() {
  
  bottomLeft <- calcDistance(list(x=0, y=0))
  topLeft <- calcDistance(list(x=0, y=1))
  bottomRight <- calcDistance(list(x=1, y=0))
  topRight <- calcDistance(list(x=1, y=1))
  
  distances <- c(bottomLeft, topLeft, bottomRight, topRight)
  
  return(max(distances))
  
}


#Calculate the max distance
maxDistance <- calcMaxDistance()
steppingDistance <- maxDistance / 6



########## DISPLAY ##########
#Function to display cow
#Adapted from: https://www.rdocumentation.org/packages/png/versions/0.1-7/topics/readPNG
#Cow image from: https://commons.wikimedia.org/wiki/File:201409_cow.png
showCow <- function() {
  
  print("Showing cow!")
  
  cowImage <- readPNG("cow.png")
  
  # if your R supports it, we'll plot it
  if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
    plot(1:2, type='n')
    
    if (names(dev.cur()) == "windows") {
      
      print("Windows")
      
      # windows device doesn't support semi-transparency so we'll need to flatten the image
      transparent <- cowImage[,,4] == 0
      cowImage <- as.raster(cowImage[,,1:3])
      cowImage[transparent] <- NA
      
      # interpolate must be FALSE on Windows, otherwise R will
      # try to interpolate transparency and fail
      rasterImage(cowImage, 1.3, 1.3, 1.7, 1.7, interpolate=FALSE)
      
    } 
    else {
      
      print("Other")
      # any reasonable device will be fine using alpha
      rasterImage(cowImage, 1.3, 1.3, 1.7, 1.7)
      
    }
  }
  
}


#Function to moo at correct volume
#Moo from: https://www.freesoundslibrary.com/cow-moo-sound/
moo <- function(click) {
  
  found <- FALSE
  
  #Figure out how far away the cow is
  distance <- calcDistance(click)
  print(c("Distance: ", distance))
  
  #Beep the correct moo
  if (distance < 0.05) {
    
    showCow()
    beep(sound="Moos\\moo +25.wav")
    print("+25")
    
    found <- TRUE
    
  }
  else if (distance < (steppingDistance / 2)) {
    beep(sound="Moos\\moo +20.wav")
  }
  else if (distance < steppingDistance) {
    beep(sound="Moos\\moo +15.wav")
  }
  else if (distance < (steppingDistance * 1.5)) {
    beep(sound="Moos\\moo +10.wav")
  }
  else if (distance < (steppingDistance * 2)) {
    beep(sound="Moos\\moo +5.wav")
  }
  else if (distance < (steppingDistance * 2.5)) {
    beep(sound="Moos\\moo.wav")
  }
  else if (distance < (steppingDistance * 3.5)) {
    beep(sound="Moos\\moo -5.wav")
  }
  else if (distance < (steppingDistance * 3.5)) {
    beep(sound="Moos\\moo -10.wav")
  }
  else if (distance < (steppingDistance* 4)) {
    beep(sound="Moos\\moo -15.wav")
  }
  else if (distance < (steppingDistance * 5)) {
    beep(sound="Moos\\moo -20.wav")
  }
  else {
    beep(sound="Moos\\moo -25.wav")
  }
  
  return(found)
  
}



########## PLAY ##########
found <- FALSE

while (!found) {
  
  #Get player to click
  click <- locator(n=1)
  #print(click)
  
  #Play the correct moo
  found <- moo(click)
  
}

text(1.5, 1.5, "YOU FOUND THE COW!", cex=2, col = "white")