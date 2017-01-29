library(stringi)
library(base)
library(plyr)
library(htmltidy)
library(xml2)
library(compiler)

library(randomForest)
library(e1071)
library(class)

readCategory<-function(filename) {
  con <- file(filename, "r")
  descrLines <- readLines(con,n=2)
  
  close(con)
  category   <- strsplit(descrLines[1]," ")[[1]][1]
  #url        <- descrLines[2]
  return(category)
}

#read files and tidy up data
readDataFile<-function(filename) {
  x <- readChar(filename, file.info(filename)$size)
  
  # remove first two lines containing metadata
  x<-stri_replace_first_regex(x,"^.*?\n.*?\n", "" )
  x<-tidy_html(x)
  return(x)
}

tagNames=c(#'video', 'audio', 'object','canvas','blockquote',
           'article', 'span',
           'input', 'form', 'link',
          # 'table', 'h1', 'h2', 'h3', 'li', 'ul', 'tr', 'hr',
          'a' )

wordCountRegexes=c("forum", "blog","video")

extractFeatures<-function(fileContent) {
  #dirtyPageSize<-stri_length(fileContent)                 ###
  #names(dirtyPageSize)="dirtyPageSize"
  #html might be malformed

  cleanPageSize<-stri_length(fileContent)                  ###
  names(cleanPageSize)="cleanPageSize"

  #get tag statistics
  x_html <- read_html(fileContent)
  allNodes <- xml_find_all(x_html,"//*")
  totalTagCount <- length(allNodes)                        ###
  names(totalTagCount)="totalTagCount"

  titleNodes= xml_find_all(x_html,"//title")
  titleText =  paste(xml_text(titleNodes))
  
  metaOgType="NO_OG_TYPE";
  metaNodes <- xml_find_all(x_html,"//meta")
  for(i in 1:length(metaNodes) ){
    nodeAttrs <- unlist(xml_attrs(metaNodes[i]))

    if (!is.na(nodeAttrs["property"])) {
      if(as.character( nodeAttrs["property"])=="og:type") {
        if(!is.na(nodeAttrs["content"])) {
           metaOgType <- nodeAttrs["content"];
        }
        break;
      }
    }
  }
  names(metaOgType)="metaOgType" ###

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
  names(leaveCount) <- "leaveCount"
  meanChildCount <- childrenCountSum/totalTagCount         ###
  names(meanChildCount) <- "meanChildCount"

  tagCounts <- laply(tagNames,
                     function(tagName) {
                       length(xml_find_all(x_html, paste0("//", tagName)))
                     }
                    )
  names(tagCounts)<-stri_paste(tagNames, "TagCount", collapse = NULL)

  relTagCounts <- tagCounts/totalTagCount
  names(relTagCounts)<-stri_paste(tagNames, "RelTagCount", collapse = NULL)

  totalTaglessSize <- stri_length(xml_text(x_html))        ###
  names(totalTaglessSize)<-"totalTaglessSize"

  cssNodes <- xml_find_all(x_html, "//style")
  jsNodes <- xml_find_all(x_html, "//script")
  headNodes <- xml_find_all(x_html, "//head")

  inlineCssSize <- sum(stri_length(xml_text(cssNodes)))    ###
  names(inlineCssSize)<-"inlineCssSize"
  inlineJsSize <- sum(stri_length(xml_text(jsNodes)))      ###
  names(inlineJsSize)<-"inlineJsSize"
  taglessHeadSize <- sum(stri_length(xml_text(headNodes))) ###
  names(taglessHeadSize)<-"taglessHeadSize"
  imgNodes <- xml_find_all(x_html, "//img")
  imgTagCount = length(imgNodes)                           ###
  names(imgTagCount)<-"imgTagCount"   #potential trouble

  imgPaths <- xml_path(imgNodes)
  imgPaths <- stri_replace_all_regex(str = imgPaths, replacement ="", pattern = "\\[[0-9]*?\\]")

  mostCommonImgPathCount <- table (imgPaths)[1]            ###
  if(is.na(mostCommonImgPathCount))
    mostCommonImgPathCount=0

  names(mostCommonImgPathCount) <- "mostCommonImgPathCount"

  #remove not dismostCommonImgPathCountplayed directly nodes to continue processing
  if(length(jsNodes))
     xml_remove(jsNodes)
  if(length(cssNodes))
     xml_remove(cssNodes)
  if(length(headNodes))
    xml_remove(headNodes)

  displayedText <- xml_text(x_html)
  displayedTextSize <- stri_length(displayedText)              ###
  names(displayedTextSize) <- "displayedTextSize"

  displToTotalRatio <- displayedTextSize / cleanPageSize       ###
  names(displToTotalRatio) <- "displToTotalRatio"
  headToTotalRatio <- taglessHeadSize / cleanPageSize          ###
  names(headToTotalRatio) <- "headToTotalRatio"

  wordCounts<-stri_count_regex(displayedText, wordCountRegexes)###
  names(wordCounts)<-paste0("wordCount_", (1:length(wordCounts))) # cause troubles
  #finally we formulate feature vector
  featureVector=
    c(#dirtyPageSize,
    cleanPageSize,
    totalTaglessSize,
    inlineCssSize,
    inlineJsSize,
    taglessHeadSize,
    mostCommonImgPathCount,
    imgTagCount,
    displayedTextSize,

    headToTotalRatio,
    displToTotalRatio,

    tagCounts,
#    relTagCounts, # relative tag counts
    totalTagCount,
    meanChildCount,
    leaveCount,
    wordCounts,
    metaOgType);
}

#compile function to speed up execution
extractFeatures_ <- cmpfun(extractFeatures)

#read list of files
filenames <- paste0("train/",list.files("train/"))

#read files content
categories <- ldply( .data =  filenames, .fun = readCategory )
categories <- as.factor(unlist(categories))

filesContent <- ldply( .data =  filenames, .fun = readDataFile )

#extact feature vectors from file content
featureData <- ldply(.data =  unlist (filesContent), .fun = extractFeatures_)
featureData$.id <- NULL
featureData["metaOgType"] <- as.factor(unlist(featureData["metaOgType"]))

#Data size
dataSize <- nrow(featureData)
vectSize <- ncol(featureData)

#Split data to learning and test sets
testIdx  <- sample(dataSize,dataSize * 0.1)

xlearn <- featureData[-testIdx,]
xtest  <- featureData[ testIdx,]

ylearn <- categories[-testIdx]
ytest  <- categories[ testIdx]

#Some models needs to be feed by matrix
xlearn_matrix <- data.matrix(xlearn)
xtest_matrix  <- data.matrix(xtest )

# try SVM model
svm_model  <- svm(x=xlearn_matrix, y=ylearn)
ytest_pred  <- predict(svm_model, xtest_matrix)
ylearn_pred <- predict(svm_model, xlearn_matrix)
# compare result of prediction for learn and test data
sum(rep(1,length(ylearn_pred)) [ylearn_pred == ylearn])/length(ylearn_pred)
sum(rep(1,length(ytest_pred))  [ytest_pred  == ytest] )/length(ytest_pred)

#try naive bayes model
bayes_model <- naiveBayes(x=xlearn_matrix, y=ylearn)

ytest_pred  <- predict(bayes_model, xtest_matrix)
ylearn_pred <- predict(bayes_model, xlearn_matrix)
# compare result of prediction for learn and test data
sum(rep(1,length(ylearn_pred)) [ylearn_pred == ylearn])/length(ylearn_pred)
sum(rep(1,length(ytest_pred))  [ytest_pred  == ytest] )/length(ytest_pred)

#try random forest # (still the best we have)
forest = randomForest(xlearn, ylearn, xtest, ytest, keep.forest = TRUE)
forest 

#which variables were revelant
forest$importance
