# input data

system("wget raw.githubusercontent.com/swcarpentry/r-novice-gapminder/master/data/gapminder_data.csv")
gapminder_data <- data.table::fread("gapminder_data.csv", data.table = FALSE)
dim(gapminder_data)
head(gapminder_data)

# let's take a basic task (that we want to do many times)

mean(gapminder_data[gapminder_data$continent == "Asia" & gapminder_data$year == "2002",]$lifeExp)

# now you want to do it again for each continent: why not copy-paste (do not do this)

mean(gapminder_data[gapminder_data$continent == "Asia" & gapminder_data$year == "2002",]$lifeExp)
mean(gapminder_data[gapminder_data$continent == "Africa" & gapminder_data$year == "2002",]$lifeExp)
mean(gapminder_data[gapminder_data$continent == "Europe" & gapminder_data$year == "2002",]$lifeExp)
mean(gapminder_data[gapminder_data$continent == "Australasia" & gapminder_data$year == "2002",]$lifeExp)
mean(gapminder_data[gapminder_data$continent == "Americas" & gapminder_data$year == "2002",]$lifeExp)

table(gapminder_data$continent)

# doing simple operations (vectorised functions)

#scalar operation
1^2
2^2
3^2
#vector operation
c(1:3)^2
c(1:10)^2

# doing something exactly the same for many arguments: FOR Loop

print(paste0(1, "^2 = ", 1^2))
for(ii in 1:4){
  print(paste0(ii, "^2 = ", ii^2))
}

for(continent in unique(gapminder_data$continent)){
  print(
  mean(gapminder_data[gapminder_data$continent == continent & gapminder_data$year == "2002",]$lifeExp)
  )
}

mean_by_cont <- rep(NA, length(unique(gapminder_data$continent)))
for(continent in unique(gapminder_data$continent)){
  ii <- match(continent, unique(gapminder_data$continent))
  mean_by_cont[ii] <- mean(gapminder_data[gapminder_data$continent == continent & gapminder_data$year == "2002",]$lifeExp)
}
names(mean_by_cont) <- unique(gapminder_data$continent)
mean_by_cont 

# doing something for different arguments: nested FOR Loop (can get messy)

mean_by_cont <- matrix(NA, length(unique(gapminder_data$year)), length(unique(gapminder_data$continent)))
for(continent in unique(gapminder_data$continent)){
  jj <- match(continent, unique(gapminder_data$continent))
  for(year in unique(gapminder_data$year)){
    ii <- match(year, unique(gapminder_data$year))
    mean_by_cont[ii, jj] <- mean(gapminder_data[gapminder_data$continent == continent & gapminder_data$year == year,]$lifeExp)
  }
}
colnames(mean_by_cont) <- unique(gapminder_data$continent)
rownames(mean_by_cont) <- unique(gapminder_data$year)
mean_by_cont 

# repeating tasks with functions: if you do something more than once, write a function

continent_mean <- function(continent){
  mean(gapminder_data[gapminder_data$continent == continent & gapminder_data$year == "2002",]$lifeExp)
}
continent_mean("Africa")
continent_mean()

continent_mean <- function(continent = "Europe"){
  mean(gapminder_data[gapminder_data$continent == continent & gapminder_data$year == "2002",]$lifeExp)
}
continent_mean("Africa")
continent_mean() #Europe is default

continent_mean <- function(data, continent = "Europe"){
  mean(data[data$continent == continent & data$year == "2002",]$lifeExp)
}
continent_mean(gapminder_data, "Africa") # compatible with other datasets (not "harcoded")
continent_mean(gapminder_data) #Europe is default for "continent

continent_mean <- function(data, continent = "Europe", year = 2002){
  mean(data[data$continent == continent & data$year == year,]$lifeExp)
}
continent_mean(gapminder_data) #Europe 2002 is default
continent_mean(gapminder_data, year = "1972", continent = "Asia") #Europe 2002 is default

#Now we can run our function on different inputs instead of copying the code
continent_mean(gapminder_data, year = "1972", continent = "Asia") #Europe 2002 is default
continent_mean(gapminder_data, year = "1972", continent = "Africa") #Europe 2002 is default
continent_mean(gapminder_data, year = "1972", continent = "Americas") #Europe 2002 is default


#Let's do our LOOP again with this function

continent_mean(gapminder_data, year = "1972", continent = "Asia") #Europe 2002 is default

mean_by_cont <- matrix(NA, length(unique(gapminder_data$year)), length(unique(gapminder_data$continent)))
for(continent in unique(gapminder_data$continent)){
  jj <- match(continent, unique(gapminder_data$continent))
  for(year in unique(gapminder_data$year)){
    ii <- match(year, unique(gapminder_data$year))
    mean_by_cont[ii, jj] <- continent_mean(gapminder_data, continent, year)
  }
}
colnames(mean_by_cont) <- unique(gapminder_data$continent)
rownames(mean_by_cont) <- unique(gapminder_data$year)
mean_by_cont 

# we can also "apply" functions to a list or matrix

#apply to a list
numbers <- list(1:10, c(0,4,6,2,8))
numbers
max(numbers[[1]])
lapply(numbers, max)
sapply(numbers, max)
#apply to a matrix/data.frame (e.g., for every country)
gdp <- apply(gapminder_data, 1, function(x) as.numeric(x[[6]]) * as.numeric(x[[3]]))
cbind(gapminder_data[,1:4], gdp)
#apply to a matrix columns
apply(gapminder_data[grep("2002", gapminder_data$year),c(3, 5, 6)], 2, sum, na.rm = TRUE)
apply(gapminder_data[grep("2002", gapminder_data$year),c(3, 5, 6)], 2, function(x) sum(x, na.rm = TRUE))

lapply(unique(gapminder_data$continent), function(continent){
  continent_mean(gapminder_data, continent, year = "2002")
  })

sapply(unique(gapminder_data$continent), function(continent){
  continent_mean(gapminder_data, continent, year = "2002")
})

# we do not need to wait for each result to compute the next one: parallel computing

lapply(1:10, function(ii) ii^2)

library("snow") # simple network of workstations
cl <- makeCluster(3)
cl
parLapply(cl, 1:10, function(ii) ii^2)

# note these operations do not occur in order (but results are returned in order of inputs)

system("rm outs.txt")
system("touch outs.txt")
library("snow") 
cl <- makeCluster(3)
cl
parLapply(cl, 1:10, function(ii){
  print(ii)
  write.table(ii^2, "outs.txt", append = TRUE)
  return(ii^2)
  })
system("cat outs.txt")

# note more cores is not always faster (Amdahl's law) due to set up time and core-to-core communication

system.time({
  library("snow") # simple network of workstations
  cl <- makeCluster(1)
  cl
  parLapply(cl, 1:1000, function(ii) ii^2)
})


system.time({
library("snow") # simple network of workstations
cl <- makeCluster(3)
cl
parLapply(cl, 1:1000, function(ii) ii^2)
})

system.time({
  library("snow") # simple network of workstations
  cl <- makeCluster(10) # more than on machine
  cl
  parLapply(cl, 1:1000, function(ii) ii^2)
})
stopCluster(cl)

# setting up parallel computing is better for more complex tasks

system.time({
lapply(unique(gapminder_data$continent), function(continent){
  continent_mean(gapminder_data, continent, year = "2002")
})
})


system.time({
  library("snow") # simple network of workstations
  cl <- makeCluster(3) # more than on machine
  cl
  clusterExport(cl, list("gapminder_data", "continent_mean")) # export data to other cores (can be "ls()")
  parLapply(cl, unique(gapminder_data$continent), function(continent){
    continent_mean(gapminder_data, continent, year = "2002")
  })
})
#slower due to sending data to each core

system.time({
  lapply(unique(gapminder_data$continent), function(continent){
    Sys.sleep(0.5)
    continent_mean(gapminder_data, continent, year = "2002")
  })
})


system.time({
  library("snow") # simple network of workstations
  cl <- makeCluster(3) # more than on machine
  cl
  clusterExport(cl, list("gapminder_data", "continent_mean"))
  parLapply(cl, unique(gapminder_data$continent), function(continent){
    Sys.sleep(0.5) # computational intensice step runs separately on each core
    continent_mean(gapminder_data, continent, year = "2002")
  })
})