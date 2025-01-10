

png("mygraphic.png")     #Create a png file
x <- mtcars$wt
y <- mtcars$mpg
# Plot with main and axis titles
# Change point shape (pch = 19) and remove frame.
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 9, cex  = 1, frame = FALSE)
print("done")


dev.off()
browseURL("mygraphic.png")    #R uses the terminal to tell the 
                              #OS to open mygraphic.png