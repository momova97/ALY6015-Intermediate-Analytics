#significant value
alpha <- 0.05

#sign test
#section 13-2
#question 6
#claim
median <- 3000
#data
game <- c(16210,	3150,	2700,	3012,	4875,
          3540,	6127,	2581,	2642,	2573,
          2792,	2800,	2500,	3700,	6030,
          5437,	2758,	3490,	2851,	2720)
diff <- game - median

#determin positives
pos <- length(diff[diff>0])

#determin negatives
neg <- length(diff[diff<0])

#run the test 
results <- binom.test(x =  c(pos,neg),alternative = "two.sided")
results
#view p-value
p<-results$p.value
#determine reject of accept
ifelse(p >alpha , 'fail to reject','reject null' )

#question 10
#claim
median <- 200
#data
#determin positives
pos <- 40-15

#determin negatives
neg <- 15

#run the test 
results <- binom.test(x =  c(pos,neg),alternative = "less")
results
#view p-value
p<-results$p.value
#determine reject of accept
ifelse(p >alpha , 'fail to reject','reject null' )

#section 13-3
#question 4
males <- c(8,	12,	6	,14	,22,27,	3,2	,2,	2,4,6 ,	19,	15,	13)
females <-c(	7,	5	,2,	3,	21,	26,	3,	9,	4	,	17,	23,	12,	11,	16)
results <- wilcox.test(x = males,y = females ,
                       alternative = 'two.sided',correct = F,exact = F)
results
#view p-value
p<-results$p.value
#determine reject of accept
ifelse(p >alpha , 'fail to reject','reject null' )
#question 8
nl <- c(	89,	9,	8	,101,	90,	91,	9	,96,	108,	100,	9 ,6,	8	,	2	,	5)
al <- c(108,	8	,9,	97	,100,	102,	9	,104,	95,	89,	8,	101 ,	6,	1	,5,8)
results <- wilcox.test(x = nl,y = al ,
                       alternative = 'two.sided',correct = F,exact = F)
results
#view p-value
p<-results$p.value
#determine reject of accept
ifelse(p >alpha , 'fail to reject','reject null' )

#13.5
wh <-data.frame(ndata = c(527, 406, 474, 381 ,411),countries = 'wh')
eu <-data.frame(ndata = c(520 ,510, 513 ,548, 496),countries = 'eu')
ea <-data.frame(ndata = c(523, 547 ,547, 391, 549),countries = 'ea')
data <- rbind(wh,eu,ea)
results <- kruskal.test(ndata ~ countries,data = data )
results
#view p-value
p<-results$p.value
#determine reject of accept
ifelse(p >alpha , 'fail to reject','reject null' )
#13.6
citys <- c(1,	2	,3,	4,	5	,6)
Subway <- c(845,	494,	425,	313,	108	,41)
Rail <- c(39,	291,	142	,103,	33,	38)

data<- data.frame(city = citys,Subway = Subway , Rail = Rail )
results <- cor.test(data$Subway,data$Rail,method = 'spearman')
results
#view p-value
p<-results$p.value
#determine reject of accept
ifelse(p >alpha , 'fail to reject','reject null' )
#section 14-3
#question 16
set.seed(96)
y <-c() 
x <- c()
z <- c()
z<-replicate(40,{
  repeat{
    y <- sample(1:4,1)
    
    if (any(x==4)&any(x==3)&any(x==2)&any(x==1)){
      z <- append(z,length(x),1)
      print(x)
      x <- c()
      break
    }
    x <- append(x,y,1)
    
  }
  
  ;z})

mean(z)
#question 18
set.seed(96)
y <-c() 
x <- c()
z <- c()
z<-replicate(30,{
  repeat{
    y <- sample(c('b','i','g'),1, prob = c(0.6,0.3,0.1))
    
    if (any(x=='b')&any(x=='i')&any(x=='g')){
      z <- append(z,length(x),1)
      print(x)
      x <- c()
      break
    }
    x <- append(x,y,1)
    
  }
  
  ;z})

mean(z)

           

