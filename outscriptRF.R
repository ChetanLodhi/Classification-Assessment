source(RandomForestSolution.R)

Test <- read.table("Classification1Test.txt", sep="\t", stringsAsFactors = FALSE, header = TRUE)

T1 <- subset(Test, select = c(-1))
T2 <- subset(Test, select = c(1))

T1$F15 <- as.numeric(as.Date(T1$F15, format='%m/%d/%Y'))
T1$F16 <- as.numeric(as.Date(T1$F16, format='%m/%d/%Y'))
T1$F17 <- as.factor(T1$F17)
T1$F18 <- as.factor(T1$F18)

output.results <- predict(modelRandom,newdata=subset(T1),type='response')
output <- data.frame(output.results)

write.table(cbind(T2,output), 
            file="output.txt",row.names=F,col.names=F, sep = "\t")
