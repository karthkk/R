
splitdf <- function(dataframe) {
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/2))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
}


aupr <- function(predictions, labels) {
  step <- ( max(predictions)-min(predictions))/100
  auc <- 0
  last.rec <- 1
  for(threshold in seq(min(predictions),max(predictions), by=step)) {
    tp <- sum(predictions>threshold & labels==1)
    fp <- sum(predictions>threshold & labels==-1)
    fn <- sum(predictions<threshold & labels==1)
    if(tp!=0) {
      prec <- tp/(tp+fp)
      rec <- tp/(tp+fn)
      auc <- auc + abs(last.rec-rec)*prec
      last.rec <- rec
    }
  }
  auc
}

rename.columns <- function(data.frame) {
  column.names <- c("pages.viewed", "products.viewed", "time.to.cart", "duration", "carted", "items.in.cart", "cart.page.views", "total.price.of.items.in.cart", "hour.of.day", "day.of.week", "month.of.year", "visitor.from.search", "number.of.search.terms", "search.term.match.with.product.in.cart", "is.direct.visitor", "num.prev.visits", "num.prev.purchases", "y")
   names(data.frame) <- column.names
  data.frame
}

extract.merchant.name <- function(data.file) {
  sub(".txt","",gsub(".*/","",data.file))
}

report.accuracy <- function(data.file) {
  f <- read.csv(data.file,head=FALSE)
  f <- rename.columns(f)
 # f$cart.page.views = f$cart.page.views - (f$y > 0)
#  f$y=factor(f$y)
  s <- splitdf(f)
#  m=randomForest(y~.,data=s$trainset)
  m=lm(y~.,data=s$trainset)
  pred <- predict(m, s$testset)
  system("growlnotify -a R -s -m \"Completed Building Model\"")
  td = table(pred, s$testset$y)
  ts = s$testset$y
  count = nrow(s$testset)
  merchant.name <- extract.merchant.name(data.file)
  accuracy = (td[1,1]+td[2,2])/count
  paste(merchant.name,count,sum(ts==-1),sum(ts==1),accuracy,td[2,2],td[1,1],td[2,1],td[1,2],sep=" & ")
}

build.tree <- function(data.file) {
  f <- read.csv(data.file,head=FALSE)
  f <- rename.columns(f)
#  f$cart.page.views = f$cart.page.views - (f$y > 0)
  f$y <- factor(f$y)
  
  tree <- rpart(y ~ .,method="class",data=f)
  ps.file <- sub(".txt",".ps",data.file)
  merchant.name <- extract.merchant.name(data.file)
  post(tree, file=ps.file, title=paste("Classification Tree For ", merchant.name))
  system("growlnotify -a R -s -m \"Completed Building TREE\"")
}

plot.prec.rec <- function(data.file) {
   f <- read.csv(data.file,head=FALSE)
   f <- rename.columns(f)
   s=splitdf(f)
   m=glm(as.factor(y)~.,data=s$trainset, family=binomial(link="logit"))
   predictions = predict(m,s$testset)
   pred=prediction(predictions,s$testset$y)
   perf <- performance(pred,"prec","rec")
   plot(perf)
   merchant.name <-  extract.merchant.name(data.file)
   dev.copy2pdf(file=paste("/tmp/linear_model_results/",merchant.name,".pdf",sep=""))
}


write.summary <- function(data.file) {
  f <- read.csv(data.file,head=FALSE)
  f <- rename.columns(f)
 # f$cart.page.views = f$cart.page.views - (f$y > 0)
  f$y <- factor(f$y)
  par(mfrow=c(3,2))
  hist(f$duration/1000)
  hist(log10(f$duration/1000))
  hist(f$time.to.cart/1000)
  hist(log10(f$time.to.cart/1000))
  hist(f$pages.viewed)
  hist(log10(f$pages.viewed))
  merchant.name = extract.merchant.name(data.file)
  dev.copy2pdf(file=paste("/tmp/histograms/",merchant.name,".pdf",sep=""))
}


find.statistics <- function(data.file) {
  f <- read.csv(data.file,head=FALSE)
  f <- rename.columns(f)
  f$cart.page.views = f$cart.page.views - (f$y > 0)
  f$y <- factor(f$y)
  merchant.name = extract.merchant.name(data.file)
  fd = f$duration/1000
  fct = f$time.to.cart/1000
  fpv = f$pages.viewed
  paste(
  paste("Duration on Site", mean(fd), median(fd), min(fd), quantile(fd,0.10),quantile(fd,0.25),quantile(fd,0.50),quantile(fd,0.75),quantile(fd,0.90),max(fd),sep=" & "),
  paste("Time to Cart", mean(fct), median(fct), min(fct), quantile(fct,0.10),quantile(fct,0.25),quantile(fct,0.50),quantile(fct,0.75),quantile(fct,0.90),max(fct),sep=" & "),
paste("Total Pages Viewed", mean(fpv), median(fpv), min(fpv), quantile(fpv,0.10),quantile(fpv,0.25),quantile(fpv,0.50),quantile(fpv,0.75),quantile(fpv,0.90),max(fpv),sep=" & "),
        sep="\\")

}


do.all <- function(folder) {
  library(ROCR)
  library(randomForest)
  dirs <- list.files(folder, full.names=TRUE)
  for(dir in dirs) {
    dirname = extract.merchant.name(dir)
    files = list.files(dir,pattern="*.txt",full.names=TRUE)
    for (file in files) {
          merchant.name = extract.merchant.name(file)
          graph.file.name <- paste(dirname,"_",merchant.name,".pdf",sep="")
          files.existing = list.files("/tmp/graphs",pattern="*.pdf")
          if(!(length(which(files.existing == graph.file.name)) > 0))  {
            rm(f)
            rm(s)
            rm(train.data)
            gc()
            print(paste("Processing ", graph.file.name))
            f <- read.csv(file,head=FALSE)
            f <- rename.columns(f)
            s=splitdf(f)

            train.data.size = min(120000,nrow(s$trainset))
            train.data=s$trainset[1:train.data.size,]

            m.lr=glm(as.factor(y)~.,data=train.data, family=binomial(link="logit"))
            predictions.lr = predict(m.lr,s$testset)
            pred.lr=prediction(predictions.lr,s$testset$y)
            perf.lr <- performance(pred.lr,"prec","rec")
            
            au.lr = aupr(predictions.lr,s$testset$y)
            
            m.rf=randomForest(y~.,data=train.data)
            predictions.rf = predict(m.rf,s$testset)
            pred.rf = prediction(predictions.rf,s$testset$y)
            perf.rf <- performance(pred.rf,"prec","rec")          
            
            au.rf = aupr(predictions.rf,s$testset$y)
            
            predictions.rn = runif(nrow(s$testset),-1,1)
            pred.rn = prediction(predictions.rn,s$testset$y)
            perf.rn <- performance(pred.rn,"prec","rec")
            au.rn = aupr(predictions.rn,s$testset$y)
            
            write.table(data.frame(merchant.name=merchant.name, dir.name=dirname, au.rf=au.rf, au.lr=au.lr, au.rn=au.rn), file="/tmp/graphs/auc",sep="\t", append=TRUE,col.names=FALSE)
            
            pdf(paste("/tmp/graphs/",dirname,"_",merchant.name,".pdf",sep=""))
            plot(perf.lr,col="red",main=paste("Precision Recall Graph for ",merchant.name,sep=" "))
            plot(perf.rf,col="blue", add=TRUE)
            plot(perf.rn,col="green",add=TRUE)
            dev.off()
          }
         }
    
  }

}
