library("tidyverse")
participants_data <- read.csv("participants_data.csv", stringsAsFactors=FALSE, header=TRUE, check.names = FALSE)
participants_data<-as.matrix(participants_data)
save(file="participants_data.Rdata")

# Barplot ####

participants_barplot <- table(participants_data$academic_parents)
barplot(participants_barplot)


# Names and Email ####

ggplot(data = participants_data, 
       aes(x = letters_in_first_name, 
           y = days_to_email_response,
           color=
             academic_parents,
           size =
             working_hours_per_day)) +
  geom_point()


#Scatterplots ####

# Create a scatterplot of iris petal length (y) 
# as a function of sepal length (x) 
# with colors representing iris species (color) 
# and petal width as bubble sizes (size).
ggplot(data = iris, 
       aes(x = Sepal.Length, 
           y = Sepal.Width, 
           color = Species, 
           size = Petal.Width))+ 
  geom_point()


# Diamonds price####

# Create a plot with the diamonds data 
# of the carat (x) and the price (y)
plot1 <- ggplot(data = diamonds, 
                aes(x = carat, y = price, 
                    alpha = 0.2)) +
  geom_point()

plot1

# Create a plot with the diamonds data 
# of the log of carat (x) 
# and the log of price (y)
plot2 <- ggplot(data = diamonds,
       aes(x = log(carat),
           y = log(price),
           alpha = 0.2)) +
  geom_point()

plot2


# Colors and Shapes ####

# Create a smaller diamonds data set (top 100 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the color of the diamond as the color of the points. 
dsmall1 <- top_n(diamonds, n = 100)
ggplot(data = dsmall1, aes(x = carat, 
                          y = price, 
                          color = color)) + 
  geom_point()

# Create a smaller diamonds data set (top 40 rows), 
# create a scatterplot with carat on the x-axis 
# and price on the y-xis and 
# with the cut of the diamond as the shapes for the points. 
dsmall2 <- top_n(diamonds, n = 40)
ggplot( data = dsmall2, 
        aes(x = carat, 
            y = price, 
            shape = cut)) + 
  geom_point()


# Set parameters ####

# Create a plot of the diamonds data 
# with carat on the x-axis, price on the y-axis. 
# Use the inhibit function to set the alpha to 0.1 
# and color to blue.
ggplot(data = diamonds, 
       aes(x = carat, 
           y = price, 
           alpha = I(0.1), 
           color = I("blue"))) + 
  geom_point()


# Geom options ####

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
dsmall3 <- top_n(diamonds,n = 50)

ggplot(data = dsmall3, 
       aes(x = carat, 
           y = price))+
  geom_point()+
  geom_smooth()


# Smooth function ####

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
# Use 'glm' as the option for the smoothing
dsmall4 <- top_n(diamonds, n = 50)

ggplot(data = dsmall4, 
       aes(x = carat, 
           y = price))+ 
  geom_point()+ 
  geom_smooth(method = 'glm')


# Boxplots ####

# Change the boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()


# Jitter points ####

# Change the jittered boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat)) + 
  geom_boxplot()+ 
  geom_jitter()

# Change the alpha to 0.4 to make 
# the scatter less transparent
ggplot(data = diamonds, 
       aes(x = cut, 
           y = price/carat, 
           alpha = I(0.4))) + 
  geom_boxplot()+ 
  geom_jitter()


# Geom Histogram ####

# Change the density plot so that the x-axis is carat 
# the color is the diamond color
# and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds, 
       aes(x = carat, 
           color = color, 
           alpha = I(0.3))) +
  geom_density()


# subset ####

# Create a plot of the mpg data with 
# manufacturer as the color and a linear model 'lm'
# as the smooth method
ggplot(data = mpg, 
       aes(x = displ, 
           y = hwy,  
           color = manufacturer)) + 
  geom_point() +
  geom_smooth(method = "lm")

# Titles and axis ####

# Change the title and labels as you see fit
ggplot(mtcars, 
       aes(mpg, 
           y = hp, 
           col = gear)) +
  geom_point() +
  ggtitle("Car usage in years sorted by horsepower") +
  labs(x = "years of usage",
       y = "horsepower",
       col = "number of gears")

# Geom Tile ####

# subset the data to numeric only with select_if
part_data <- select_if(participants_data, 
                       is.numeric)
# use 'cor' to perform pearson correlation
# use 'round' to reduce correlation 
# results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
# use 'as.data.frame.table' to build a table
# with correlation values
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()


# Export figures ####
# 
# png(file = "cortile_1.png", width = 7, height = 6, units = "in", res = 400)
# ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# dev.off()
# 
# png(file = "cortile_2.png", width = 7, height = 6, units = "in", res = 400)
# ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# dev.off()
# 
# png(file = "cortile_3.png", width = 7, height = 6, units = "in", res = 400)
# ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# dev.off()
# 
# png(file = "cortile_4.png", width = 7, height = 6, units = "in", res = 400)
# ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + geom_point() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# dev.off()
