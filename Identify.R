library(stringi)
library(base)
library(plyr)
library(htmltidy)
library(xml2)
library(compiler)

library(randomForest)
library(e1071)

tagNames=c('img', 'video', 'audio', 'script', 'style',
           'article', 'canvas', 'blockquote', 'span',
           'input', 'form', 'object', 'meta', 'link',
           #'table', 'h1', 'h2', 'h3', 'li', 'ul', 'tr', 
           'hr', 'a', 'b', 'p', 'i')

wordCountRegexes=c("password|login|username|użytkownik|zaloguj|hasło",
                   "forum|post|awatat|avatar|quote",
                   #"vid|wideo|film",#
                   #"audio|music|muzyka",#
                   "zdjęcie|zdjęcia|foto|photo",
                   "pic|picture|image|obraz",
                   "artykuł|article",
#                   "tekst|text",
                   "portal",
                   "blog",
#                   "galeria|galery",
                   "news|wiadomości|informacje",
                   "kontakt|contact",
#                   "buy|kup|cena|koszyk|price",
#                   "place|miejsce|ulica|lokalizacja",
#                   "share|udostępnij|prześlij",
                   "email|e-mail|poczta|sendto",
                   "bank|ubezpieczen|kredyt|praw",
                   "bizn|praw|pieni|money|inwest"#,
                   #"ad|reklama",
                   #"zdrowie|medycyna|health",
                   #"uroda|kosmetyki|beauty",
                   #"play|odtwórz"
                   )



extractFeatures<-function(filename)
{
  con <- file(filename, "r")
  descrLines <- readLines(con,n=2)
  
  close(con)
  category   <- strsplit(descrLines[1]," ")[[1]][1]
  url        <- descrLines[2]
  #Features are maked by "###" at the end of line for convinience
  
  x <- readChar(filename, file.info(filename)$size)
  
  # remove first two lines containing metadata

  x<-stri_replace_first_regex(x,"^.*?\n.*?\n", "" )
  dirtyPageSize<-stri_length(x)                            ###
  #html might be malformed
  x<-tidy_html(x)
  cleanPageSize<-stri_length(x)                            ###
  #get tag statistics
  x_html <- read_html(x)
  allNodes <- xml_find_all(x_html,"//*")
  totalTagCount <- length(allNodes)                        ###
  
  childrenCountSum <-0
  leaveCount <- 0 #childless nodes                         ###
  for(i in 1:totalTagCount[1]) {
    childCount <- length(xml_children(allNodes[i][1]))
    if (childCount != 0) {
       childrenCountSum <- childrenCountSum + 1;
    } else {
      leaveCount <-leaveCount +1;
    }
  }
  meanChildCount <- childrenCountSum/totalTagCount         ###
  
  tagCounts = laply(tagNames,
                    function(tagName) {
                      length(xml_find_all(x_html, paste0("//", tagName)))
                    }
                   )
  
  totalTaglessSize <- stri_length(xml_text(x_html))        ###
  cssNodes <- xml_find_all(x_html, "//style")
  jsNodes <- xml_find_all(x_html, "//script")
  headNodes <- xml_find_all(x_html, "//head")
  
  
  inlineCssSize <- sum(stri_length(xml_text(cssNodes)))    ###
  inlineJsSize <- sum(stri_length(xml_text(jsNodes)))      ###
  taglessHeadSize <- sum(stri_length(xml_text(headNodes))) ###
  
  #remove not displayed directly nodes to continue processing
  if(length(jsNodes))
     xml_remove(jsNodes)
  if(length(cssNodes))
     xml_remove(cssNodes)
  if(length(headNodes))
    xml_remove(headNodes)
    
  displayedText <- xml_text(x_html)
  displayedTextSize <- stri_length(displayedText)              ###
  
  displToTotalRatio <- displayedTextSize / cleanPageSize       ###
  headToTotalRatio <- taglessHeadSize / cleanPageSize          ###

  wordCounts<-stri_count_regex(displayedText, wordCountRegexes)###

  #ostatecznie formujemy wektor cech                       
  c(dirtyPageSize,
    cleanPageSize,
    totalTaglessSize,
    inlineCssSize,
    inlineJsSize,
    taglessHeadSize,
    displayedTextSize,
    
    headToTotalRatio,
    displToTotalRatio,
    
    #tagCounts,
    tagCounts/totalTagCount, # relative tag counts
    totalTagCount,
    meanChildCount,
    leaveCount,
    
    wordCounts,
    category);
}
extractFeatures_ <- cmpfun(extractFeatures)

#read list of files

filenames <- paste0("train/",list.files("train/"))

#extact feature vectors from raw data
data = ldply(.data =  filenames, .fun = extractFeatures_)

#Split data to learning and test sets
dataSize <- nrow(data)
vectSize <- ncol(data)

testIdx  <- sample(dataSize,dataSize * 0.3)
test <- data[testIdx, ]
learn <- data[-testIdx, ]


#dane
x <- learn[, 1:vectSize-1]
y <- as.factor(learn[, vectSize])

xtest <- test [, 1:vectSize-1]
ytest <- as.factor(test [, vectSize])

x_matrix     <- data.matrix(x)
xtest_matrix <- data.matrix(xtest)


#svm_model <- svm(x=x_matrix, y=y, gamma = 0.01)
#ytest_pred    <- predict(svm_model, xtest_matrix)
#y_pred   <- predict(svm_model, x_matrix)

#sum(rep(1,length(y_pred)) [y_pred == y])/length(y_pred)
#sum(rep(1,length(ytest_pred)) [ytest_pred == ytest])/length(ytest_pred)

#Teach forest
forest = randomForest(x, y, xtest, ytest, ntree=2000, keep.forest = TRUE)
forest 

#jakie zmienne były istotne
forest$importance
failed = filenames [data[, vectSize] == forest$predicted[filenames] ] 
