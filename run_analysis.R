X_train<-read.table(file = "./train/X_train.txt", quote = "", 
                    comment.char = "", nrows = 100)
classes <- sapply(X_train, class)
X_train<-read.table(file = "./train/X_train.txt", quote = "", 
                    comment.char = "", colClasses = classes)
X_test<-read.table(file = "./test/X_test.txt", quote = "", 
                    comment.char = "", nrows = 100)
classes <- sapply(X_test, class)
X_test<-read.table(file = "./test/X_test.txt", quote = "", 
                    comment.char = "", colClasses = classes)
subject_train<-read.table(file = "./train/subject_train.txt", quote = "", 
                          comment.char = "", colClasses = "numeric")
subject_test<-read.table(file = "./test/subject_test.txt", quote = "", 
                         colClasses = "numeric", comment.char = "")
X_train$SubjectID<-subject_train[,1]
X_test$SubjectID<-subject_test[,1]
y_train<-read.table(file = "./train/y_train.txt", quote = "", 
                    comment.char = "", colClasses = "numeric")
y_test<-read.table(file = "./test/y_test.txt", quote = "", 
                   comment.char = "", colClasses = "numeric")
X_train$Activity<-y_train[,1]
X_test$Activity<-y_test[,1]
MergeData<-merge(x = X_train, y = X_test, all = T)
features<-read.table(file = "./features.txt", quote = "", 
                     comment.char = "", 
                     colClasses = c("numeric", "character"))
colnames(MergeData)<-c(features[,2], "SubjectID", "Activity")
FilterData1<-MergeData[,grepl(pattern = "mean()", x = colnames(MergeData),
                              fixed = T)]
FilterData2<-MergeData[,grepl(pattern = "std()", x = colnames(MergeData), 
                              fixed = T)]
FilterData1$id<-1:nrow(FilterData1)
FilterData2$id<-1:nrow(FilterData2)
FilterData<-merge(x = FilterData1, y = FilterData2, by = "id")
FilterData$ActNum<-MergeData$Activity
FilterData$SubjectID<-MergeData$SubjectID

for(i in seq_along(FilterData$ActNum)) {
       if(FilterData$ActNum[i]==1) {FilterData$ActNum[i]<-"WALKING"}
       else if(FilterData$ActNum[i]==2) {FilterData$ActNum[i]<-"WALKING_UPSTAIRS"}
       else if(FilterData$ActNum[i]==3) {FilterData$ActNum[i]<-"WALKING_DOWNSTAIRS"}
       else if(FilterData$ActNum[i]==4) {FilterData$ActNum[i]<-"SITTING"}
       else if(FilterData$ActNum[i]==5) {FilterData$ActNum[i]<-"STANDING"}
       else if(FilterData$ActNum[i]==6) {FilterData$ActNum[i]<-"LAYING"}
}
attributes(FilterData)$names[68]<-"Activities"
splits<-split(x = FilterData, 
              f = list(FilterData$Activities, FilterData$SubjectID), 
              drop = T)
TidyData<-lapply(splits, function(x) {colMeans(x[, 2:67])})
write.table(x = unlist(TidyData), file = "./TidyData.txt", 
            col.names = "average values")
