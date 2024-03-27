library(stringr)
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(Rwordseg)
library(tm)
library(tmcn)
library(RColorBrewer)
library(tidyr)
library(grid)
library(RTextTools)
library(pipeR)

setwd("E:\\work\\12.1zhongliu")
##onco <- read.csv("fukeweixin.csv", header = T, stringsAsFactors = F)
#oncodata <- fread("zhongliu.csv")
oncodata <- read_csv(file.choose()) #Save as utf-8 csv
onco_temp <- oncodata %>% data.table() %>%
  .[Public account %in% c('medlive-cancer','oncolatdxy','yxj-zl',
                          'oncology_news','XHWCWK-201209',
                          'oncologymf','CMToncology','CA_3DMed',
                          'oncofrontier','ioncology')] %>%
  
  # name = c('Yimaitong Oncology Department','Tumor Time','Medical Cancer Channel',
  # 'Tumor Information','Gastrointestinal Oncology Surgery',
  # 'Oncology Medical Forum', 'China Medical Tribune Today's Cancer', 'Suddhi',
  # 'Tumor Frontier', 'Tumor Outlook')
  #Remove useless columns
  #onco_temp <- oncodata[, c("identity", "original address", "reading status", "cate") := NULL]
  .[, c("logo", "article content", "real reading number", "appreciation number", "reading status", 'article comments') := NULL] %>%
  .[, reading count := as.numeric(reading count)] %>%
  .[, headlines := as.character(headlines)] %>%
  
  #lubridate
  .[, release time := as.Date(release time)] %>%
  # onco_temp[, release time := ymd_hms(release time)]
  # onco_temp[, date := floor_date(release time, "day")]
  .[, wkday := wday(release time)] #Day of the week, display the day of the week
#onco_temp[, hours := hour(release time)] #hours
#Filter time
#onco.real2 <- onco.real[Release time>= ymd_hms("2015-05-01 00:00:00")][Release time<= ymd_hms("2016-05-02 00:00:00") ]
#setorder(zz3, release time) #release time sorting

#headlinesandnon-headlines
onco_temp <- onco_temp[str_detect(headline, '1'), result := "topic"][is.na(result), result := "nontop"] %>%
  
  
  #Title length
  .[,len := str_length(article title)] %>%
  
  setnames(c('public account', 'article title', 'number of readings', 'number of likes', 'release time', 'wkday', 'headlines', 'original address'),
           c('acc', 'title', 'view', 'like', 'date', 'weekday', 'headline', 'url'))

dt <- onco_temp
# Name matching
dt2 <- data.table( acc = c('medlive-cancer','oncolatdxy','yxj-zl','gh_44783b2de2bd','zhongliuneike',
                           'oncology_news','XHWCWK-201209','cmatumor','oncologymf','wx_zlpl','CMToncology',
                           'CA_3DMed','oncology_doconline','oncofrontier','ioncology'),
                   name = c('Yimaitong Oncology Department','Tumor Time','Medical Cancer Channel','Chinese Oncology Clinic',
                            'Medical Oncology', 'Oncology Information', 'Gastrointestinal Oncology Surgery', 'Oncology Space',
                            'Oncology Medical Forum', 'Oncology Review', 'China Medical Tribune Today's Cancer',
                                     'Studio', 'China Cancer News', 'Tumor Frontier', 'Tumor Outlook'))

# Note that after the change is the data frame
dt3 <- left_join(dt, dt2, 'acc')
dt3 <- as.data.table(dt3)

# Remove unmatched and empty values in the name column
dt4 <- dt3[!is.na(name)]

# txt_combind------------------------------------------------ -------------
#trainisYimaitong Oncology Department
path <- "E:\\work\\12.1zhongliu\\20176.13zhongliuoncology\\all" ##File directory
fileNames <- dir(path) ##Get the file name under the path
filePath <- sapply(fileNames, function(x){
             paste(path,x,sep='/')})

data = lapply(filePath, readLines)
data<-gsub("\n","",data,fixed=TRUE)
data <- str_replace(' ', '', data)
data_all <- data.frame(fileNames, data) %>% setnames( 'fileNames', 'title') %>>%
             as.data.table() %>%
             .[,title := str_replace(title, '.txt', '')] %>%
             .[, data := as.character(data)] %>%
             setkey(title) %>%
             unique(by = 'title')

#Title content alignment------------------------------------------------- -------------------
dt <- read.csv("E:\\work\\12.1zhongliu\\20176.13zhongliuoncology\\onc619.csv") %>%
             as.data.table() %>%
             .[, c('X.U.FEFF.identification', 'original address') := NULL] %>%
             setnames('X', 'ID') %>%
             setnames('result', 'topic')
# filter
dt1 <- dt[ name %in% c('Medical Cancer Channel', 'Yimaitong Oncology Department')] %>%
             .[,title := str_replace(title, '.txt', '')] %>%
             .[, title := as.character(title)] %>%
             left_join(data_all, by = 'title') %>%
             as.data.table()

DT_2var <- dt1[,.(ID,title, data)][,title := NULL] %>%
             setnames('data', 'title')

DT_2var_train_amend <- read.csv("E:\\work\\12.1zhongliu\\20176.13zhongliuoncology\\dt1train_real.csv") %>% # 9.5train-revised-2.csv
             as.data.table() %>%
             setkey(title) %>%
             unique(by = 'title')

setkey(DT_2var, ID) #dt1 all data
setkey(DT_2var_train_amend, ID) #dt2 manual handled dataset
DT_2var_test <- DT_2var[!DT_2var_train_amend, on = "ID"]

setwd('E:\\R\\wechat_process')

listDict()
Dir_self <- readLines("myself.11.3-fuke.csv")
insertWords(Dir_self)

Dir_class <- fread("E:\\R\\wechat_process\\11.3sdfss.csv")
swds1 <- Dir_class[, Medical Humanities]
swds2 <- Dir_class[, meeting minutes]
swds3 <- Dir_class[, case clinical]
swds4 <- Dir_class[, review of works]
swds5 <- Dir_class[, medical science]
swds6 <- Dir_class[, message]
swds7 <- Dir_class[, online continuing education]
swds8 <- Dir_class[, other]


for (x in swds1) DT_2var_test[str_detect(title, x), result := "Medical Humanities"]
for (x in swds2) DT_2var_test[str_detect(title, x), result := "Meeting Minutes"]
for (x in swds3) DT_2var_test[str_detect(title, x), result := "Case Clinical"]
for (x in swds4) DT_2var_test[str_detect(title, x), result := "Summary of thesis"]
for (x in swds5) DT_2var_test[str_detect(title, x), result := "Medical Science Popularization"]
for (x in swds6) DT_2var_test[str_detect(title, x), result := "Message"]
for (x in swds7) DT_2var_test[str_detect(title, x), result := "Online Continuing Education"]
for (x in swds8) DT_2var_test[str_detect(title, x), result := "Other"]
DT_2var_test[is.na(result), result := "MM"]

DT_2var_amend <- bind_rows(DT_2var_train_amend, DT_2var_test)
DT_2var_amend_fr <- as.data.frame(DT_2var_amend)
DT_2var_amend_fr$title <- gsub("[0-9 0 1 2 3 4 5 6 7 8 9 <>~]","",DT_2var_amend_fr$title)


stopwords <- unlist(readLines("E:\\R\\wechat_process\\stopworddd(bone).txt", encoding = "UTF-8"))
f_seg <- function(x,stopwords){
             x<-gsub('[\ra-z]|--','',x)
             seg<-segmentCN(x)
             seg<-seg[!seg %in% stopwords]
             #seg<-gsub('[a-zA-Z0-9]|\\.',"",seg) #remove letters
             seg<-seg[nchar(seg)>1]
             word<-paste(seg,collapse=' ')
             word
}

title_seg <- DT_2var_amend_fr %>% as.data.table() %>%
             .[, title := enc2utf8(title)] %>%
             .[Encoding(title) =='unknown', title := 'unknown']
title_seg <- sapply(title_seg$title, f_seg, stopwords)
names(title_seg) <- NULL

DT_2var_amend_fr_wordseg <- data.frame(DT_2var_amend_fr, word = title_seg, stringsAsFactors = F)

row.names(DT_2var_amend_fr_wordseg) <- NULL

# You have to replace all empty words with values.
DT_2var_amend_fr_wordseg <- data.table(DT_2var_amend_fr_wordseg)


#content c lowercase!!!!
myReader <- readTabular(mapping =list(content = "word"))
corpus <- Corpus(DataframeSource(DT_2var_amend_fr_wordseg),
                  readerControl = list(reader = myReader, language = "zh_cn"))
mat <- TermDocumentMatrix(corpus, control = list(wordLengths = c(2,Inf)))
mat <- weightTfIdf(mat,normalize = TRUE)
mat <- t(removeSparseTerms(mat, sparse = 1-0.0003))

#Use the function create_container to create a container, and the classification label is manual classification.
#The first 250 (about 77.4%) behavioral training set, and the rest is the test set.
# All data in the training set must have corresponding codes.
# all label must have a value, <dt[is.na(result), result := "MM"]>
tratin_len <- length(DT_2var_train_amend$title)
test_len_star <- length(DT_2var_train_amend$title) + 1
test_len_end <- length(DT_2var_amend$title)

container <- create_container(mat, DT_2var_amend_fr_wordseg$result,
                               trainSize = 1:tratin_len, testSize = test_len_star:test_len_end, virgin=FALSE)
SVM <- train_model(container,"SVM")
save(SVM,file="625svmcancer.rda")

SVM_CLASSIFY <- classify_model(container, SVM)

DT_2var_train_amend_svm <- DT_2var_train_amend[, ':=' (SVM_LABEL = result,SVM_PROB = 1)]
#DT_2var_test_fr <- as.data.frame(DT_2var_test)
#SVM_CLASSIFY_fr <- as.data.frame(SVM_CLASSIFY)
DT_2var_test_svm <- cbind(as.data.frame(DT_2var_test), as.data.frame(SVM_CLASSIFY))
DT_all <- rbindlist(list(DT_2var_train_amend_svm, as.data.table(DT_2var_test_svm)))
write.csv(DT_all, "DT_all_128oncology.csv")

# #Merge all data------------------------------------------------ -------------------
#All data
DT <- dt1#"E:\R\7.20fuchankefenxi\2017.2.13\leftjoined.csv"
#Contains only classification results, title and ID
DT_all2 <- DT_all[,.(ID, title, SVM_LABEL)] %>%
             setnames('title', 'content')

setkey(DT, ID)
setkey(DT_all2, ID)
Result <- merge(DT,DT_all2, all.x=TRUE)
result_ulti <- Result[ , content := NULL]
setnames(result_ulti,"data","title")

xxx <- result_ulti[,.(ID, title,SVM_LABEL)] %>>%
             left_join(dt1, ., by = 'ID') %>%
             as.data.table() %>%
             .[, title.x := NULL] %>%
             setnames('title.y', 'title')

#analy------------------------------------------------ -------------------

dt <- data.table(dt3)
dt[, date := as.Date(date)]
df <- data.frame(acc = dt$acc, view = dt$view)
p1 <- dt[,.(view_avg = mean(view)), by = .(name)] %>% ggplot(aes(reorder(name, -view_avg),view_avg)) +
             geom_bar(stat = 'identity', width=0.6) +
             geom_hline(aes(yintercept = mean(view_avg)), linetype = 5 , col = 'red') +
             theme(axis.text.x = element_text(size = 10,angle = 30, vjust = .8)) +
             annotate('text', x = 'Tumor Information', y = mean(df$view) + 3, label = paste('mean', round(mean(df$view),2))) +
             xlab('public account')
p1
save.image('oncology.RData')
# Total trend of total reading volume on date
p6 <- dt[,.(view_sum = sum(view)), by = .(date)] %>% ggplot(aes(date, view_sum)) +
           geom_line(size=.2) + scale_y_log10() + geom_smooth()
p6

#boxplot
library(scales)
p3 <- ggplot(dt, aes(name, view)) + geom_point(alpha = 0.5, position = "jitter") +
             geom_boxplot(outlier.size = 0, alpha = 0.2, color = 'red') +
             xlab("level") + ggtitle("Posting status") +
             geom_hline(yintercept= 2539, color = "skyblue", size=1) +
             theme(axis.text.x = element_text(size = 10,angle = 30, vjust = .8))

p3

#Number of articles
p4 <- dt[,. (num = length(title)), by =.(name)] %>% ggplot(aes(x = reorder(name, -num), y = num)) +
             geom_bar(stat = 'identity') +
             geom_hline(aes(yintercept = mean(num)), linetype = 5 , col = 'red') +
             theme(axis.text.x = element_text(size = 10,angle = 30, vjust = .8))
p4

# point4 Number of articles vs. number of reads

p8 <- dt[,.(num = length(title), sum_view = sum(view)), by = .(date, acc)] %>%
             ggplot(aes(num, sum_view, group = num)) +
             geom_point(alpha = 0.7,position = "jitter") +
             geom_boxplot(outlier.size = 0, alpha = 0.2, color = 'red') +
             # facet_wrap(~ level, ncol =3) +
             xlab("Total number of articles in a single day") + ylab("Total number of reads in a single day")
p8

# p9 <- dt[,.(num = length(title), sum_view = sum(view)), by = .(level, date, acc)] %>%
# ggplot(aes(num, sum_view, group = num)) +
# geom_point(alpha = 0.7,position = "jitter") +
# geom_smooth() +
# # facet_wrap(~ level, ncol =2)
#p9

options(digits = 2)

# p5 word count
p37 <- dt[,.(view_avg = mean(view)), by = .(len)] %>%
             ggplot(aes(len, view_avg)) + geom_bar(stat = 'identity', position = 'identity') +
             geom_text(aes(label = str_sub(view_avg, 1, 5), vjust=1)) +
             #geom_vline(xintercept= 12, color = "red", size=1) +
             #annotate("text", x=12, y=-8, label="12", color = 'red') +
             xlab("number of words in the title") + ylab("average number of reads")
p37

# Total number of posts on working days
p24_2 <- dt[,.(num_sum = length(title)), by = .(weekday)] %>% ggplot(aes(weekday, num_sum)) + geom_bar(stat = 'identity', width=0.6) +
             xlab("weekday") + ylab("number of articles") +
             scale_x_discrete(limits=c('Monday','Tuesday','Wednesday','Thursday',
                                       'Friday','Saturday','Sunday'))
p24_2

#dodge
# p24_3 <- dt[,.(num_sum = length(title)), by = .(name, weekday)] %>% ggplot(aes(weekday, num_sum, fill = name)) + geom_bar(stat = 'identity' , position = 'dodge') +
# xlab("weekday") + ylab("number of articles") +
# scale_x_discrete(limits=c('Monday','Tuesday','Wednesday','Thursday',
# 'Friday','Saturday','Sunday'))
# p24_3

#dodge
p24_4 <- dt[,.(num_sum = length(title)), by = .(name, weekday)] %>% ggplot(aes(weekday, num_sum, fill = name)) + geom_bar(stat = 'identity', position = 'dodge') +
             xlab("weekday") + ylab("number of articles") +
             scale_x_discrete(limits=c('Monday','Tuesday','Wednesday','Thursday',
                                       'Friday','Saturday','Sunday')) +
             facet_wrap(~ name, ncol =3)
p24_4

# p25 Weekday average reading curve
p25 <- dt[,.(view_avg = mean(view)), by = .(weekday)] %>% ggplot(aes(weekday, view_avg, group = 1)) +
             geom_bar(stat = 'identity') +
            ylab("Pageviews(log10)") +
             scale_x_discrete(limits=c('Monday','Tuesday','Wednesday','Thursday',
                                       'Friday','Saturday','Sunday'))
p25

# Reading volume for each number
p26 <- dt[,. (view_sum = sum(view)), by = .(name)] %>% ggplot(aes(reorder(name, -view_sum), view_sum)) + geom_bar(stat = 'identity') +
             theme(axis.text.x = element_text(size = 10,angle = 30, vjust = .8))
p26