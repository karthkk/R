
splitdf <- function(dataframe) {
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/2))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset=trainset,testset=testset)
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
   library("randomForest")
   library("ROCR")
   f <- read.csv(data.file,head=FALSE)
   f <- rename.columns(f)
   s=splitdf(f)
   m=randomForest(y~.,data=s$trainset)
   #m=glm(as.factor(y)~.,data=s$trainset, family=binomial(link="logit"))
   predictions = predict(m,s$testset)
   pred=prediction(predictions,s$testset$y)
   perf <- performance(pred,"prec","rec")
   merchant.name <-  extract.merchant.name(data.file)
   pdf(paste("/tmp/rf/",merchant.name,".pdf",sep=""))
   plot(perf)
   dev.off()
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

load.file <- function (path) {
  f <- read.csv(path)
  s <- splitdf(f)
  s
}

is.significant <- function(model, var) {
  w = regTermTest(model,var)
  res = w$p[1,1]<0.05
  res
}

get.coefs <- function(model) {
  rx = c()
  for (n in 1:length(model$coefficients)) {
    name = names(m$coefficients[n])
    
    if ((name == "(Intercept)") || is.significant(model,name)) {
      rx = c(rx,m$coefficients[n])
    } else {
      rx = c(rx,0)
    }
  }
#  names(rx)=names(model$coefficients)
  rx
}

do.test <- function(file) {
  s <- load.file(file)
  m <- glm(as.factor(y)~.,data=s$trainset, family=binomial(link="logit"))
  coefs <- get.coefs(m)
  coefs
}
