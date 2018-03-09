#################
## R_CLASS_III ##
#################

#In R III we're going to look at some of R's simple tools that allow you to make your own data sets
#To begin we'll start by pulling data from the web, restructure and clean the data in preparation for analysis
#We'll also explore the basics of loops, a pre-requisite for pulling data from multiple webpages
#And finally we'll look at how to retrive data using from APIs with R

#In this class we'll look at:
#1. Scraping with RStudio
#2. Structuring the data 
#3. Cleaning the data
#4. Scraping and loops
#5. Making API calls

#############################
##1. Scraping with RStudio ##
#############################

#While R may not be as adept at dealing with the many issues webpages present as other coding languages 
#It does have a range of nifty packages for scraping data and working with APIs 
#When combined with R's other restructuring, cleaning and analysis tools it can serve as an 

#We're going to look at this page http://espn.go.com/nfl/superbowl/history/winners
#Any ideas how the data is structured?

#At the start of the script we install and load the packages needed for the project
#I've commented the lines below as these packages are already installed on the NICAR laptops
#You'll need to uncomment them, by removing the #, if you want to run this on your own machine
#install.packages("rvest")
#install.packages("tidyverse")

#Rvest is used for scraping data from the web and includes commands html and html_node
#Tidyverse contains a number of packages. We'll be using functions from stringr, tidyr and dplyr.
#Stringr is useful for working with strings, including matching, subsetting and extracting data
#Tidyr and dplyr are common package and is used for transforming and manipulating data

#Once the packages have been installed, let's load them using the library command
library(rvest)
library(tidyverse)

#First assign the web address to url
url <- 'http://espn.go.com/nfl/superbowl/history/winners'

#Pass it to the command 'read_html'
#read_html is part of the rvest package and pulls the underlying html from a webpage
superbowl <- read_html(url)

#Let's read superbowl to see what what html we have pulled from the webpage
superbowl

############################
##2. Structuring the data ##
############################

#Now let's strucutre the data into a dataframe

#To do this we use the rvest functions html_nodes and html_table 
#We'll extract the HTML table and convert it to a data frame
superbowl_table <- html_nodes(superbowl, 'table')
#We're taking out the first table element
sb <- html_table(superbowl_table)[[1]]
#What does our dataframe now look like
head(sb)
View(sb)

#What should we remove from the table?
sbowl <- slice(sb,3:54)

#Now let's set new column names or variables so the content is more clear
names(sbowl) <- c("number", "date", "site", "result")
head(sbowl)

#The result column should be split so we can analyse the data
#We'll divide it into 4 new columns
#Let's start splitting with 'separate' by the comma delimiter
sbowl <- separate(sbowl, result, c('winner', 'loser'), sep=', ', remove=TRUE)
head(sbowl)

#Now we'll split out the scores from the winner and loser columns 
#We'll do this by substring pattern matches, which is based on regex
#Let's look at the table and figure what we want...

#We'll first assign the regex we're going to use
#We're assigning it to an object called 'scorepattern'
#What is going on in the regex below?
scorepattern <- " \\d+$"

#Next we overwrite the contents of the columns based on pattern matching
#Before we do, let's write out what the content of the column would be overwritten with
#This is a good precautionary step so you don't overwrite things with the wrong data
winner_score <- as.numeric(str_extract(sbowl$winner, scorepattern))
winner <- gsub(scorepattern, "", sbowl$winner)

#If that all looks good, we're ok to overwrite the contents in the data frame 
sbowl$winner_score <- as.numeric(str_extract(sbowl$winner, scorepattern))
sbowl$loser_score <- as.numeric(str_extract(sbowl$loser, scorepattern))
sbowl$winner <- gsub(scorepattern, "", sbowl$winner)
sbowl$loser <- gsub(scorepattern, "", sbowl$loser)
head(sbowl)

#########################
##3. Cleaning the data ##
#########################

#Now we've strucured the data into something more useful for analysis
#Before we start analysis we'll need to clean up some of the data

#We'll replace the roman numerals with numeric values
#In order to do this we're using the basics of sequencing
#This is a straight over-writing command
sbowl$number <- 1:52

#We will also convert the date to a standard format using the as.Date command
sbowl$date <- as.Date(sbowl$date, "%B. %d, %Y")
head(sbowl)

#The data is now cleaned and ready for analysis
#Let's write the result out to a csv
write.csv(sbowl, 'superbowl.csv', row.names=F)

##########################
##4. Scraping and loops ##
##########################

#We scraped and cleaned one page of data 
#Now let's look at pulling data from a series of pages with the same basic structure
#To do this we need to know how to run a basic loop

#A loop carries out the same process on a number of different webpages
#To write a loop we need to define what it is we want the loop to carry out
#Then we need to direct our instruction to what it is we want the function to be carried out on

#To begin with we write a loop by defining how many times we want to run the command

#Let's take a look at what is happening in this for loop
for (i in 1:3){
  print(paste("newlink"))
}

#What do we need to define here...
for (i in xxxx){
  print(paste("The date is March", i, "2018" ))
}

#In this case we want our loop to run on a set of webpages
#The theory of the above will be just the same but we will apply it to a different set of problems

#We're going to look at UK's Health and Safety Executive website
#http://www.hse.gov.uk/prosecutions/default.asp
#Let's talk about the webpage...

#Let's build the command first

url <- "http://www.hse.gov.uk/prosecutions/case/case_list.asp?PN=1&ST=C&EO=LIKE&SN=F&x=35&SF=DN&SV=&y=6&SO=ADN"
page=read_html(url)
table <- page %>% html_table()
tab <- table[[4]]
cleantab <- tab[-c(10), ] 
alllinks <- page %>% html_nodes("td:nth-child(2) a") %>% html_attr("href")
links <- alllinks[1:10]
cleantab$Links= links
file <- as.character(paste("test_results_page.csv", sep=""))
cleantab$File= file
cleantab$Original= url
write.csv(cleantab, file)

#Let's break down how we would loop through the pages to repeat this command across multiple pages
#We need the loop to iterate through 44 pages 
#The link to each page will change in every iteration

#for (i in 1:44){
#  url <- print(paste("http://www.hse.gov.uk/prosecutions/case/case_list.asp?PN="
#                     ,i,"&ST=C&EO=LIKE&SN=F&x=26&SF=DN&SV=&y=10&SO=ADN",sep=""))
#  ***ALL OUR ABOVE CODE***
#  Sys.sleep(2)
#}

#Now let's bringing the command and a the loop together
for (i in 1:11){
  url <- print(paste("http://www.hse.gov.uk/prosecutions/case/case_list.asp?PN="
                     ,i,"&ST=C&EO=LIKE&SN=F&x=26&SF=DN&SV=&y=10&SO=ADN",sep=""))
  page=read_html(url)
  table <- page %>% html_table()
  tab <- table[[4]]
  cleantab <- tab[-c(10), ] 
  alllinks <- page %>% html_nodes("td:nth-child(2) a") %>% html_attr("href")
  links <- alllinks[1:10]
  cleantab$Links= links
  file <- as.character(paste("case_results_page", i, ".csv", sep=""))
  cleantab$File= file
  cleantab$Original= url
  write.csv(cleantab, file)
  Sys.sleep(2)
}

#Combine all files into a list based on their pattern
allfiles <- list.files(pattern="[[:digit:]].*\\.csv") 

#Make an empty container to bind rows into 
allcases <- NULL

#Combine all files into one dataframe and write to csv
for (f in allfiles) {
  dat <- read.csv(f, header=T, sep=",", na.strings="", colClasses="character")
  allcases <- rbind(allcases, dat)
}

#The we write it out to a .csv file
write.csv(allcases, "allcases.csv")
View(allcases)

#Now we have gathered the data you can see it's far from ready for analysis
#The next steps would be to clean and harmonise the data in preparation for analysis

########################
##5. Making API calls ##
########################

#Install and load RCurl package to call API
#install.packages('RCurl')
library(RCurl)

#Let's also install and load the XML package to allow us to parse XML
#The results of the API call will (usually? always?) be in XML ####
#install.packages('XML')
library(XML)

#Register for xml key with Zillow and assign it to a 'key'
key <- 'X1-ZWz19d402sfmrv_43hp8'

reply = getForm("http://www.zillow.com/webservice/GetRegionChildren.htm",
                'zws-id' = "X1-ZWz19d402sfmrv_43hp8",
                state = "il",
                city = "chicago",
                childtype = "neighborhood")

#Let's parse the XML into a document
zillowdoc = xmlTreeParse(reply, asText = TRUE, useInternal = TRUE)

#By parsing we'll disgard the useless data and keep the elements we need
#Write result to XML file
saveXML(zillowdoc, file='chicagozillow.xml')

########################
## What to do next... ##
########################

#There are many resources out there to help you develop your R skills
#There are plenty of built in datasets to play with in R
#They can be really useful for practicing and building up confidence
#To find built in datasets in R try this...
data()

#RStudio has a range of great tipsheets based on specific packages and areas of code
# www.rstudio.com/resources/cheatsheets/

#Check out r-bloggers for tutorials, hints and tips
#Although new packages and capabilities are being added to R all the time, the syntax and basics remain the same
#The O'Reilly R Cookbook & R for Data Science are great resources

#Find me on Twitter @caelainnbarr
#Or on email caelainn.barr@theguardian.com
#I have a range of other R lessons on my github page - github.com/Caelainn

###################
## Caelainn Barr ##
## The Guardian  ##
## @caelainnbarr ##
###################
