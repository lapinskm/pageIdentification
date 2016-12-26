library(stringi)
library(base)
library(plyr)
library(randomForest)

extractFeatures<-function(filename)
{
  con <- file(filename, "r")
  descrLines <- readLines(con,n=2)
  close(con)
  category   <- strsplit(descrLines[1]," ")[[1]][1]
  url        <- descrLines[2]
  #Features are maked by "###" at the end of line
  
  urlSize <- stri_length(url)                               ###
  x <- readChar(filename, file.info(filename)$size)
  x <- tolower(x)
  
  pageSize=stri_length(x)                                   ###
  
  title<-stri_subset_regex(x, "<title></title>")
  titleBlog    <-grepl(pattern = "blog",           x = title, ignore.case = T )  ###
  #titleForum   <-grepl(pattern = "forum",          x = title, ignore.case = T )  ###
  #titleGallery <-grepl(pattern = "gallery|galeria",x = title, ignore.case = T )  ###
  titlePage    <-grepl(pattern = "page|strona",    x = title, ignore.case = T )  ###
  titlePortal  <-grepl(pattern = "portal",         x = title, ignore.case = T )  ###


  imgTagCount<-stri_count_fixed(x, "<img")               ###
  vidTagCount<-stri_count_fixed(x, "<video")             ###
  audTagCount<-stri_count_fixed(x, "<audio")             ###
  artTagCount<-stri_count_fixed(x, "<article")           ###
  canTagCount<-stri_count_fixed(x, "<canvas")            ###
  quoTagCount<-stri_count_fixed(x, "<blockquote")        ###
  spaTagCount<-stri_count_fixed(x, "<span")              ###
  scrTagCount<-stri_count_fixed(x, "<script")            ###
  cssTagCount<-stri_count_fixed(x, "<style")             ###
  inpTagCount<-stri_count_fixed(x, "<input")             ###
  forTagCount<-stri_count_fixed(x, "<form")              ###
  objTagCount<-stri_count_fixed(x, "<object")            ###
  divTagCount<-stri_count_fixed(x, "<div")               ###
  metTagCount<-stri_count_fixed(x, "<meta")              ###

  #some should be releative to size of site - it is the reason for dividing by pageSize 
  imgTagRelCount<-imgTagCount/pageSize       ###
  vidTagRelCount<-vidTagCount/pageSize       ###
  audTagRelCount<-audTagCount/pageSize       ###
#  artTagRelCount<-artTagCount/pageSize       ###
#  canTagRelCount<-canTagCount/pageSize       ###
#  quoTagRelCount<-quoTagCount/pageSize       ###
#  spaTagRelCount<-spaTagCount/pageSize       ###
#  scrTagRelCount<-scrTagCount/pageSize       ###
#  cssTagRelCount<-cssTagCount/pageSize       ###
#  inpTagRelCount<-inpTagCount/pageSize       ###
#  forTagRelCount<-forTagCount/pageSize       ### 
  objTagRelCount<-objTagCount/pageSize       ### 
  divTagRelCount<-divTagCount/pageSize       ###


  h1TagCount<-stri_count_fixed(x, "<h1") / pageSize         ###
  h2TagCount<-stri_count_fixed(x, "<h2") / pageSize         ###
  h3TagCount<-stri_count_fixed(x, "<h3") / pageSize         ###
  liTagCount<-stri_count_fixed(x, "<li") / pageSize         ###
  trTagCount<-stri_count_fixed(x, "<tr") / pageSize         ###

  aTagCount<-stri_count_fixed(x, "<a")/pageSize             ###
  bTagCount<-stri_count_fixed(x, "<b")/pageSize             ###
  pTagCount<-stri_count_fixed(x, "<p")/pageSize             ###
  iTagCount<-stri_count_fixed(x, "<i")/pageSize             ###

  tc=gsub("<.*?>", "",x); # tagless content
  rm(x) # not needed anymore

  taglessSize <- stri_length(tc)                    ###
  taglessToTotalRatio= taglessSize / pageSize       ###

  loginWrdCount<-stri_count_regex(tc,"password|login|username|użytkownik|zaloguj|hasło")    ###
  forumWrdCount<-stri_count_regex(tc,"forum|post|awatat|avatar|quote")                      ###
  videoWrdCount<-stri_count_regex(tc,"video|wideo|film")                                    ###
  audioWrdCount<-stri_count_regex(tc,"audio|music|muzyka")                                  ###
  articWrdCount<-stri_count_regex(tc,"artykuł|article")                                     ###
  textsWrdCount<-stri_count_regex(tc,"tekst|text")                                          ###
  portlWrdCount<-stri_count_regex(tc,"portal")                                              ###
  newsyWrdCount<-stri_count_regex(tc,"news|wiadomości|informacje")                          ###
  sportWrdCount<-stri_count_regex(tc,"sport|piłka")                                         ###
  fontyWrdCount<-stri_count_regex(tc,"font")                                                ###
  #facebWrdCount<-stri_count_regex(tc,"facebook")                                           ###
  #googlWrdCount<-stri_count_regex(tc,"goole")                                              ###
  #youtuWrdCount<-stri_count_regex(tc,"youtube")                                            ###
  #twittWrdCount<-stri_count_regex(tc,"twitter")                                            ###
  contaWrdCount<-stri_count_regex(tc,"kontakt|contact")                                     ###
  shopsWrdConut<-stri_count_regex(tc,"kup|cena|koszyk|price")                               ###
  placeWrdCount<-stri_count_regex(tc,"place|miejsce|ulica|lokalizacja")                     ###
  shareWrdCount<-stri_count_regex(tc,"share|udostępnij|prześlij")                           ###
  mailsWrdCount<-stri_count_regex(tc,"email|e-mail|poczta|sendto")                          ###
  banksWrdCount<-stri_count_regex(tc,"bank|ubezpieczen|kredyt|praw")                        ###
  biznsWrdCount<-stri_count_regex(tc,"bizn|praw|pieni|money|inwest")                        ###
  adverWrdCount<-stri_count_regex(tc,"ad|reklama")                                          ###
  

  #ostatecznie formujemy wektor cech                       
  c(taglessSize,
    pageSize,
    urlSize,
    taglessToTotalRatio, 

    titleBlog,
    #titleForum,
    #titleGallery,
    #titlePage,
    titlePortal,

    imgTagCount,
    vidTagCount,
    audTagCount,
    artTagCount,
    canTagCount,
    quoTagCount,
    spaTagCount,
    scrTagCount,
    cssTagCount,
    inpTagCount,
    forTagCount,
    #objTagCount,
    #divTagCount,
    metTagCount,
    
    imgTagRelCount,
    vidTagRelCount,
    audTagRelCount,
    #artTagRelCount,
    #canTagRelCount,
    #quoTagRelCount,
    #spaTagRelCount,
    #scrTagRelCount,
    #cssTagRelCount,
    #inpTagRelCount,
    #forTagRelCount,
    objTagRelCount,
    divTagRelCount,

    h1TagCount,
    h2TagCount,
    h3TagCount,
    liTagCount,
    trTagCount,
    aTagCount,
    bTagCount,
    pTagCount,
    iTagCount,
    loginWrdCount,
    forumWrdCount,
    videoWrdCount,
    audioWrdCount,
    articWrdCount,
    portlWrdCount,
    newsyWrdCount,
    fontyWrdCount,
    #facebWrdCount,
    #googlWrdCount,
    #youtuWrdCount,
    #twittWrdCount,
    contaWrdCount,
    shopsWrdConut,
    placeWrdCount,
    shareWrdCount,
    mailsWrdCount,
    banksWrdCount,
    biznsWrdCount,
    adverWrdCount,
    category);
}

#read list of files

filenames <- paste0("train/",list.files("train/"))

#extact feature vectors from raw data
data = ldply(.data =  filenames, .fun = extractFeatures)

#Split data to learning and test sets
dataSize <- nrow(data)
vectSize <- ncol(data)

testIdx  <- sample(dataSize,dataSize*0.3)
test <- data[testIdx, ]
learn <- data[-testIdx, ]

#Teach forest
forest = randomForest(x = learn[, 1:vectSize-1]   ,y=as.factor(learn[, vectSize]),
             xtest = test[, 1:vectSize-1], ytest<-as.factor(test[,vectSize]), ntree=2000, keep.forest = TRUE  )
forest

#jakie zmienne były istotne
forest$importance
