#PRASHANT MURALI - ID:29625564 - FIT3152 Assignment 1

install.packages("readr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("gridExtra")
install.packages("directlabels")
install.packages("ggcorrplot")
install.packages(c("igraph",	"igraphdata"))

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(directlabels)
library(ggcorrplot)
library(igraph)
library(igraphdata)


#Create dataframe
rm(list = ls())
set.seed(29625564) # XXXXXXXX = your student ID
webforum <- read.csv("webforum.csv")
webforum <- webforum [sample(nrow(webforum), 20000), ] # 20000 rows

#Have a look at the data, and its data types
summary(webforum)
str(webforum)

#Change the date datatype, and remove anonynous authors and posts with no words
webforum$Date <- as.Date(webforum$Date)
webforum <- subset(webforum, AuthorID != -1)
webforum <- subset(webforum, WC != 0)
str(webforum)


#PART A PART 1

#Add a Year-Month column to the webforum dataframe
webforum$MonthYear <- format(webforum$Date, "%Y-%m")
#Summarize the data
summary(webforum)

#Count number of messages by date
part_a_df<-as.data.frame(table(webforum$Date))
names(part_a_df)[names(part_a_df)=="Var1"]<-"Date"        #Change column names
names(part_a_df)[names(part_a_df)=="Freq"]<-"Frequency"   #Change column names
part_a_df$Date <- as.Date(part_a_df$Date)     #Convert to date data type again
#Add Year and Julian Date to the data frame
part_a_df$Year <- format(part_a_df$Date, "%Y")
part_a_df$JulianDate <- yday(part_a_df$Date)
part_a_df

#Plot a time series showing the total number of messages over time
ggplot(data=part_a_df,aes(x=JulianDate, y=Frequency),group="Frequency")+
  facet_wrap(~Year, ncol=3)+geom_line(color="orange")+
  xlab("Julian Date")+ylab("Total Number of Posts")+
  stat_smooth(color="blue")

#Found that total number of forum posts peak right before 2006, so we look closer into the 2nd half of 2005

#Closer look at the second half of 2005
second_half_twothousandfive<-part_a_df[part_a_df$Date<="2006-01-01",]
second_half_twothousandfive<-second_half_twothousandfive[second_half_twothousandfive$Date>="2005-06-06",]
second_half_twothousandfive
#Plot it to have a closer look
ggplot(data=second_half_twothousandfive,aes(x=Date, y=Frequency),group="Frequency")+geom_line(color="orange")+
  xlab("Date in 2005")+ylab("Total Number of Messages")

#Find the dates that had the top most activity
sort_highest_freq <- second_half_twothousandfive[order(second_half_twothousandfive$Frequency),]
#Take the top six
six_highest<-tail(sort_highest_freq)
names(six_highest)[names(six_highest)=="Frequency"]<-"Number of posts"        #Change column names
six_highest <- subset(six_highest,select=c("Date", "Number of posts"))
#Display in a table
grid.table(six_highest)

#The bottom two dates have the highest amount of messages in the forum, so we will look into which threads contributed
# to this spike in messages

#Select the top two
top_two<-tail(sort_highest_freq, n=2)
#Get the highest date
highest_totalpost_date<-tail(top_two$Date, n=1)
highest_totalpost_date
#Get the second highest date
second_highest_totalpost_date<-head(top_two$Date, n=1)
second_highest_totalpost_date

#Look for the date in the webforum data frame
highest_totalpost_date_data <- webforum[webforum$Date %in% highest_totalpost_date,]
highest_totalpost_date_data
#Count number of posts in each thread for that day
sort_threads<-as.data.frame(table(highest_totalpost_date_data$ThreadID))
#Sort the threads
sorted_threads <- sort_threads[order(sort_threads$Freq),]
names(sorted_threads)[names(sorted_threads)=="Var1"]<-"ThreadID"        #Change column names
names(sorted_threads)[names(sorted_threads)=="Freq"]<-"Number of posts"        #Change column names
sorted_threads
#Display a table to show which threads contributed to make that date have the highest amount of thread posts.
grid.table(sorted_threads)

#Repeat the same as above for the date with second highset number of posts
#Look for the date in the webforum data frame
second_highest_totalpost_date_data <- webforum[webforum$Date %in% second_highest_totalpost_date,]
second_highest_totalpost_date_data
#Count number of posts in each thread for that day
sort_threads_2<-as.data.frame(table(second_highest_totalpost_date_data$ThreadID))
sorted_threads_2 <- sort_threads_2[order(sort_threads_2$Freq),]
names(sorted_threads_2)[names(sorted_threads_2)=="Var1"]<-"ThreadID"        #Change column names
names(sorted_threads_2)[names(sorted_threads_2)=="Freq"]<-"Number of posts"        #Change column names
sorted_threads_2
#Display a table to show which threads contributed to make that date have the second highest amount of thread posts.
grid.table(sorted_threads_2)

#Lastly, pLot a decomposed graph
detach(package:igraph)
new_df<-as.data.frame(table(webforum$MonthYear))
decompose_df<-ts(new_df$Freq,frequency = 12, start = c(2002,1))
decomposed_graph<-decompose(decompose_df)
plot(decomposed_graph)



#PART A PART 2

head(webforum)
#Aggregate by month and year, and get the mean values
part_a_df4<-aggregate(webforum, by=list(webforum$MonthYear), mean)

#First graph is for the first 5 variables - for the overall active time period.
part_a_df4_graph<-subset(part_a_df4,select=c(Group.1, WC, Analytic, Clout, Authentic, Tone))
part_a_df4_graph_melted<-melt(part_a_df4_graph,id="Group.1")
ggplot(part_a_df4_graph_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
  geom_line()+
  facet_wrap(~variable) +
  theme(axis.text.x = element_blank())+
  xlab("Date") + ylab("Average")+
  stat_smooth(color="black")

#Second graph is for the remaining linguistic variables
part_a_df4_graph2<-subset(part_a_df4,select=-c(ThreadID,AuthorID,Time, Date, WC, Analytic, Clout, Authentic, 
  Tone, MonthYear))
part_a_df4_graph2_melted<-melt(part_a_df4_graph2,id="Group.1")
ggplot(part_a_df4_graph2_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
  geom_line()+
  facet_wrap(~variable) +
  theme(axis.text.x = element_blank())+
  xlab("Date") + ylab("Average")+
  stat_smooth(color="black")

#NOW DO FOR DAYS IN A WEEK
head(webforum)
#Specify day and hours in the dataframe
webforum$Day <- weekdays(webforum$Date)
webforum$Hour <- substr(webforum$Time, 1, 2)
webforum
#Aggregate by the Day
part_a_df2 <- aggregate(webforum, by=list(webforum$Day), mean)
#Order them in days
part_a_df2$Group.1 <- factor(part_a_df2$Group.1, levels=c("Monday", "Tuesday", "Wednesday", "Thursday",
  "Friday", "Saturday", "Sunday"))
part_a_df2 <- part_a_df2[order(part_a_df2$Group.1), ]
#Select the WC and first 4 linguistic variables
part_a_df2_graph <- subset(part_a_df2,select=c(Group.1, WC, Analytic, Clout, Authentic, Tone))
#Melt and then plot
part_a_df2_graph_melted <- melt(part_a_df2_graph,id="Group.1")
#Plot
ggplot(part_a_df2_graph_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
  geom_line() + xlab("Day")+ylab("Average")+
  geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))

#Plot another for the rest of the linguistic variables
part_a_df2_graph2 <- subset(part_a_df2,select=-c(ThreadID, AuthorID, Date, Time, WC, Analytic, Clout,
  Authentic, Tone, MonthYear, Day, Hour))
part_a_df2_graph2_melted <- melt(part_a_df2_graph2,id="Group.1")
ggplot(part_a_df2_graph2_melted, aes(x=Group.1, y=value, colour=variable, group=variable))+
  geom_line() + xlab("Day")+ylab("Average")+
  scale_x_discrete(expand=c(0,1))+
  geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))
  
#DO FOR HOURS OF THE DAY
part_a_df3 <- aggregate(webforum, by=list(webforum$Hour), mean)
#For WC and first 4 linguistic variables
part_a_df3_graph <- subset(part_a_df3,select=c(Group.1, WC, Analytic, Clout, Authentic, Tone))
part_a_df3_graph_melted <- melt(part_a_df3_graph,id="Group.1")
ggplot(part_a_df3_graph_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
  geom_line() + xlab("Hour of the day")+ylab("Average")+
  geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))
#For the rest of the linguistic variables
part_a_df3_graph2 <- subset(part_a_df3,select=-c(ThreadID, AuthorID, Date, Time, WC, Analytic, Clout,
  Authentic, Tone, MonthYear, Day, Hour))
part_a_df3_graph2_melted <- melt(part_a_df3_graph2,id="Group.1")
ggplot(part_a_df3_graph2_melted, aes(x=Group.1, y=value, colour=variable, group=variable))+
  geom_line() + xlab("Hour of the day")+ylab("Average")+
  scale_x_discrete(expand=c(0,1))+
  geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))

#Do ppl think more analytically or emotionally over time?
analytical_tone_df<-subset(webforum,select=c(MonthYear,Analytic,Tone))
#Get the mean aggregated by MonthYear
analytical_tone_df<-aggregate(analytical_tone_df, by=list(analytical_tone_df$MonthYear), mean)
analytical_tone_df <- subset(analytical_tone_df,select=c(Group.1,Analytic,Tone))
analytical_tone_df_melted <- melt(analytical_tone_df,id="Group.1")
analytical_tone_df_melted
#Plot
ggplot(analytical_tone_df_melted, aes(x=Group.1, y=value,color=variable,group=variable))+
  geom_line() + xlab("Date")+ylab("Average")+ggtitle("Analytic and Tone over time ")+
  theme(axis.text.x = element_blank())+
  stat_smooth(color="blue")

#FURTHER TESTING
#Get a subset of the data with WC and all the linguistic variables
correlation_test<-subset(webforum,select=-c(ThreadID,AuthorID,Time,Date, MonthYear,Day,Hour))
#View the correlation details
correlation_data<-round(cor(correlation_test),2)
correlation_data
#Melt the data for plotting
melted_correlation_data<-melt(correlation_data, na.rm = TRUE)
melted_correlation_data
#Plot a heatmap for the correlation details
ggplot(melted_correlation_data, aes(Var1, Var2, fill=value))+
  geom_tile(color="white")+
  geom_text(aes(Var1,Var2,label=value), color="black", size=4)+
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0)+
  xlab("Linguistic Variables")+ylab("Linguistic Variables")

#Do further hypothesis testing for variables with high correlation valeues
#FOR posemo and affect
cor.test(webforum$posemo,webforum$affect)

#FOR anger and negemo
cor.test(webforum$anger,webforum$negemo)

#FOR social and Clout
cor.test(webforum$social,webforum$Clout)

#FOR i and ppron
cor.test(webforum$i,webforum$ppron)

#FOR posemo and Tone
cor.test(webforum$posemo,webforum$Tone)

#FOR ppron and Analytic
cor.test(webforum$ppron,webforum$Analytic)

#FOR i and Clout
cor.test(webforum$i,webforum$Clout)



#PART B PART 1

str(webforum)
#There are 600 threads.

#Below essentially counts how many messages are in each thread.
partb_df<-as.data.frame(table(webforum$ThreadID))
names(partb_df)[names(partb_df)=="Var1"]<-"Thread"
str(partb_df)

#There is too much data to plot, so the graph of number of threads is used
# for analysing at a high level.
ggplot(partb_df, aes(x=Thread, y=Freq))+
  geom_bar(stat="identity",color="blue")+
  theme(axis.text.x = element_blank())+
  xlab("Threads") + 
  ylab("Number of posts")

#It can be seen from the graph before that there are 6 threads that stands
# out the most (i.e. highest amount of activity).
#So now we attempt to analyse these six top threads specifically. These top
# six threads will be used for further analysis with linguistic variables.

#Getting the top 6 threads
sort_threads <- partb_df[order(partb_df$Freq),]
top_six_threads<-tail(sort_threads,n=6)
names(top_six_threads)[names(top_six_threads)=="Freq"]<-"Number of posts"        #Change column name
#Display in a table. Note: The thread that contributed to day with the most posts is also the thread with
#with one of the most posts.
grid.table(top_six_threads)




#PART B PART 2
#Look at all the threads at a highlevel
partb_df2<-aggregate(webforum, by=list(webforum$ThreadID), mean)
partb_df2

#Plot a graph to see the first four linguistic variables used in all the threads.
graph_b1<-subset(partb_df2,select=c(Group.1, Analytic, Clout, Authentic, Tone))
graph_b1
melted_graph_b1<-melt(graph_b1,id="Group.1")
melted_graph_b1
ggplot(melted_graph_b1, aes(x = seq(1:length(melted_graph_b1$value)), y = melted_graph_b1$value, fill = melted_graph_b1$variable)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_discrete(name = "Linguistic Variables") +
  xlab("Threads") + 
  ylab("Average")+
  ggtitle("Top four average linguistic variables used in all threads")



#FIND OUT THE DIFFERENCES/SIMILARITY IN LANGUAGE USED BETWEEN THE TOP AND BOTTOM 10 THREADS

#FIND THE TOP 10 AND BOTTOM 10 THREADS
top_ten_threads<-tail(sort_threads,n=10)
top_ten_threads
bottom_ten_threads<-head(sort_threads,n=10)
bottom_ten_threads
#Get all the posts for those 10 threads
top_ten_thread_posts <- webforum[webforum$ThreadID %in% top_ten_threads$Thread,]
top_ten_thread_posts
bottom_ten_thread_posts <- webforum[webforum$ThreadID %in% bottom_ten_threads$Thread,]
bottom_ten_thread_posts

#Get the mean for the top 10 threads, for selected variables.
top_ten_mean<-summarise(top_ten_thread_posts, WC_mean=mean(WC),Analytic_mean=mean(Analytic),Clout_mean=mean(Clout),
                        Authentic_mean=mean(Authentic),Tone_mean=mean(Tone),ppron_mean=mean(ppron), 
                        affect_mean=mean(affect), posemo_mean=mean(posemo), negemo_mean=mean(negemo),
                        anx_mean=mean(anx),anger_mean=mean(anger), social_mean=mean(social))

#Get the mean for the bottom 10 threads, for selected variables.
bottom_ten_mean<-summarise(bottom_ten_thread_posts, WC_mean=mean(WC),Analytic_mean=mean(Analytic),Clout_mean=mean(Clout),
                           Authentic_mean=mean(Authentic),Tone_mean=mean(Tone),ppron_mean=mean(ppron), 
                           affect_mean=mean(affect),posemo_mean=mean(posemo), negemo_mean=mean(negemo),
                           anx_mean=mean(anx),anger_mean=mean(anger), social_mean=mean(social))

#Melt the top ten data
melted_top_ten_mean<-melt(top_ten_mean)
#Plot a bar chart of the average for the top 10
ggplot(melted_top_ten_mean, aes(x = variable, y = value, fill=variable)) + 
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle=90)) +
  xlab("Linguistic Variables") + 
  ylab("Top ten average")+
  ggtitle("Linguistic variables for top ten thread average")


#Melt the bottom ten data
melted_bottom_ten_mean<-melt(bottom_ten_mean)
#Plot a bar chart of the average for the bottom 10
ggplot(melted_bottom_ten_mean, aes(x = variable, y = value, fill=variable)) + 
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle=90)) +
  xlab("Linguistic Variables") + 
  ylab("Bottom ten average") +
  ggtitle("Linguistic variables for bottom ten thread average")



#Perform a t.test to see if they are similar

#Create a table to store the p-values for each variable later
p_value_table<-matrix(ncol=1, nrow=11)
colnames(p_value_table)<-"p-value"
rownames(p_value_table)<-c("Analytic","Clout","Authentic","Tone","ppron","affect","posemo","negemo","anx","anger","social")
p_value_table

#T-test for analytic
test1<-t.test(top_ten_thread_posts$Analytic,bottom_ten_thread_posts$Analytic, conf.level=0.95)
test1
#Store the p-value into the table
p_value_table[1]<-test1$p.value


#T-test for Clout
test2<-t.test(top_ten_thread_posts$Clout,bottom_ten_thread_posts$Clout, conf.level=0.95)
test2
#Store the p-value into the table
p_value_table[2]<-test2$p.value


#T-test for Authentic
test3<-t.test(top_ten_thread_posts$Authentic,bottom_ten_thread_posts$Authentic, conf.level=0.95)
test3
#Store the p-value into the table
p_value_table[3]<-test3$p.value


#T-test for Tone
test4<-t.test(top_ten_thread_posts$Tone,bottom_ten_thread_posts$Tone, conf.level=0.95)
test4
#Store the p-value into the table
p_value_table[4]<-test4$p.value


#T-test for ppron
test5<-t.test(top_ten_thread_posts$ppron,bottom_ten_thread_posts$ppron, conf.level=0.95)
test5
#Store the p-value into the table
p_value_table[5]<-test5$p.value


#T-test for affect
test6<-t.test(top_ten_thread_posts$affect,bottom_ten_thread_posts$affect, conf.level=0.95)
test6
#Store the p-value into the table
p_value_table[6]<-test6$p.value


#T-test for posemo
test7<-t.test(top_ten_thread_posts$posemo,bottom_ten_thread_posts$posemo, conf.level=0.95)
test7
#Store the p-value into the table
p_value_table[7]<-test7$p.value


#T-test for negemo
test8<-t.test(top_ten_thread_posts$negemo,bottom_ten_thread_posts$negemo, conf.level=0.95)
test8
#Store the p-value into the table
p_value_table[8]<-test8$p.value


#T-test for anx
test9<-t.test(top_ten_thread_posts$anx,bottom_ten_thread_posts$anx, conf.level=0.95)
test9
#Store the p-value into the table
p_value_table[9]<-test9$p.value


#T-test for anger
test10<-t.test(top_ten_thread_posts$anger,bottom_ten_thread_posts$anger, conf.level=0.95)
test10
#Store the p-value into the table
p_value_table[10]<-test10$p.value


#T-test for social
test11<-t.test(top_ten_thread_posts$social,bottom_ten_thread_posts$social, conf.level=0.95)
test11
#Store the p-value into the table
p_value_table[11]<-test11$p.value

p_value_table<-round(p_value_table,6)
grid.table(p_value_table)




#NEXT, WE WILL LOOK INTO THE ANALYSIS OF THE TOP 6 THREADS INDIVIDUALLY

#So coming back to to the top 6 threads, we will look into its linguistic variables.
top_six_thread_posts <- webforum[webforum$ThreadID %in% top_six_threads$Thread,]
top_six_thread_mean <- aggregate(top_six_thread_posts, list(top_six_thread_posts$ThreadID), mean)
top_six_thread_mean

#View the first 4 linguistic variables in the top 6 threads
graph_b4<-subset(top_six_thread_mean,select=c(Group.1,Analytic, Clout, Authentic, Tone))
melted_graph_b4<-melt(graph_b4, id="Group.1")
melted_graph_b4
#Plot the data
ggplot(melted_graph_b4, aes(x = seq(1:length(value)), y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge',width=3) +
  geom_text(aes(label=variable), angle =90, hjust=1.8) +
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_discrete(name = "Linguistic Variables") +
  xlab("Linguistic Variables") + 
  ylab("Average")+
  facet_wrap(~Group.1)+
  ggtitle("Top 6 most active threads")


#View the reamining linguistic variables for the top 6 threads
graph_b5<-subset(top_six_thread_mean,select=-c(ThreadID,AuthorID,Time, Date, WC, Analytic, Clout, Authentic, Tone, MonthYear, Day, Hour))
melted_graph_b5<-melt(graph_b5, id="Group.1")
#Plot the data
ggplot(melted_graph_b5, aes(x = seq(1:length(value)), y = value, fill = variable)) + 
  geom_bar(stat = 'identity', position = 'dodge', width=4) + 
  theme(axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
  scale_fill_discrete(name = "Linguistic Variables") +
  xlab("Linguistic Variables") + 
  ylab("Average")+
  facet_wrap(~Group.1)+
  ggtitle("Top 6 most active threads")



#Perform a t.test to see if the variables are similar

#Create a two tables to store the p-values for each variable for the two different thread comparisons
p_value_table_1<-matrix(ncol=1, nrow=4)
colnames(p_value_table_1)<-"p-value"
rownames(p_value_table_1)<-c("Analytic","Clout","Authentic","Tone")
p_value_table_1

p_value_table_2<-matrix(ncol=1, nrow=4)
colnames(p_value_table_2)<-"p-value"
rownames(p_value_table_2)<-c("Analytic","Clout","Authentic","Tone")
p_value_table_2

#Get the data for the top 3 threads individually
thread_252620<-top_six_thread_posts[top_six_thread_posts$ThreadID %in% 252620,]
thread_127115<-top_six_thread_posts[top_six_thread_posts$ThreadID %in% 127115,]
thread_145223<-top_six_thread_posts[top_six_thread_posts$ThreadID %in% 145223,]


#TESTING FOR THREAD 252620 AND 127115

#T-test for Analytic
analytic_test_1<-t.test(thread_252620$Analytic,thread_127115$Analytic, conf.level=0.95)
analytic_test_1
#Store the p-value into the table
p_value_table_1[1]<-analytic_test_1$p.value

#T-test for Clout
clout_test_1<-t.test(thread_252620$Clout,thread_127115$Clout, conf.level=0.95)
clout_test_1
#Store the p-value into the table
p_value_table_1[2]<-clout_test_1$p.value

#T-test for Authentic
authentic_test_1<-t.test(thread_252620$Authentic,thread_127115$Authentic, conf.level=0.95)
authentic_test_1
#Store the p-value into the table
p_value_table_1[3]<-authentic_test_1$p.value

#T-test for Tone
tone_test_1<-t.test(thread_252620$Tone,thread_127115$Tone, conf.level=0.95)
tone_test_1
#Store the p-value into the table
p_value_table_1[4]<-tone_test_1$p.value

#Display the table for the testing between thread 252620 and 127115
p_value_table_1<-(p_value_table_1)
grid.table(p_value_table_1)



#TESTING FOR THREAD 252620 AND 145223

#T-test for Analytic
analytic_test_2<-t.test(thread_252620$Analytic,thread_145223$Analytic, conf.level=0.95)
analytic_test_2
#Store the p-value into the table
p_value_table_2[1]<-analytic_test_2$p.value

#T-test for Clout
clout_test_2<-t.test(thread_252620$Clout,thread_145223$Clout, conf.level=0.95)
clout_test_2
#Store the p-value into the table
p_value_table_2[2]<-clout_test_2$p.value

#T-test for Authentic
authentic_test_2<-t.test(thread_252620$Authentic,thread_145223$Authentic, conf.level=0.95)
authentic_test_2
#Store the p-value into the table
p_value_table_2[3]<-authentic_test_2$p.value

#T-test for Tone
tone_test_2<-t.test(thread_252620$Tone,thread_145223$Tone, conf.level=0.95)
tone_test_2
#Store the p-value into the table
p_value_table_2[4]<-tone_test_2$p.value

#Display the table for the testing between thread 252620 and 127115
p_value_table_2<-(p_value_table_2)
grid.table(p_value_table_2)




#PART B PART 3

#We will look into the top 3 threads and observe their linguistic
# variables, but this time against time.
#Below gets the top 6 thread ID's individually.
third_highest<-top_six_threads[4,]
second_highest<-top_six_threads[5,]
first_highest<-top_six_threads[6,]

#Get all the data for each thread individually.
first <- webforum[webforum$ThreadID %in% first_highest$Thread,]
second <- webforum[webforum$ThreadID %in% second_highest$Thread,]
third <- webforum[webforum$ThreadID %in% third_highest$Thread,]

#The function below is defined to plot a timeseries for the averages of the linguistic variables.
top_six_linguistics_graph<-function(data){
  data <- aggregate(data, list(data$Date), mean)
  graph_data<-subset(data,select=-c(ThreadID, AuthorID, Date, Time, WC, MonthYear, Day, Hour, money, swear, family, leisure))
  melted_graph<-melt(graph_data,id="Group.1")
  graph<-ggplot(melted_graph, aes(x=Group.1, y=value, colour=variable)) + 
    geom_line(size=0.2) + xlab("Date") + ylab("Average")+
    ggtitle("Average linguistic variables used over time")+
    facet_wrap(~variable)+
    theme(axis.text.x = element_text(angle=90))
  return(graph)
}

#Graph for thread with most posts
first_graph<-top_six_linguistics_graph(first)
first_graph

#Graph for thread with second most posts
second_graph<-top_six_linguistics_graph(second)+
  stat_smooth(color="black")
second_graph

#Graph for thread with third most posts
third_graph<-top_six_linguistics_graph(third)+
  stat_smooth(color="black")
third_graph

#Closer look into some of the graphs to observe the trends of linguistic variables over time

plot_graph<-function(data){
  graph_data<-subset(data,select=-c(ThreadID, AuthorID, Date, Time, WC, MonthYear,Day, Hour, money, swear, family, leisure))
  melted_graph_data<-melt(graph_data,id="Group.1")
  graph<-ggplot(melted_graph_data, aes(x=Group.1, y=value, colour=variable)) + 
    geom_line(size=0.2) + xlab("Date") + ylab("Average")+
    facet_wrap(~variable)+
    theme(axis.text.x = element_text(angle=90))+
    stat_smooth(color="black")
  return(graph)
}

#Looking at the data for the thread with most posts, there is a large gap, so we remove it
# by looking at the dates before "2006-01-05".
detailed_first_graph <- aggregate(first, list(first$Date), mean)
detailed_first_graph<-detailed_first_graph[detailed_first_graph$Date<="2006-01-05",]
plot_graph(detailed_first_graph)
  
#Looking at the data for the thread with the second most posts, there is a spike on one of
# of the days in the year 2009, so we'll look closer into that.
detailed_second_graph <- aggregate(second, list(second$Date), mean)
detailed_second_graph<-detailed_second_graph[detailed_second_graph$Date<="2010-01-01",]
detailed_second_graph<-detailed_second_graph[detailed_second_graph$Date>="2009-01-01",]
plot_graph(detailed_second_graph)


#ANALYSIS EXPLAINED IN REPORT.

#Next, We will look into the daily and hourly trends of linguistic for the top first and second threads
#DAILY TREND for word count and top 4 linguistic variables
days_graph<-function(data){
  df <- aggregate(data, by=list(data$Day), mean)
  #Order them in days
  df$Group.1 <- factor(df$Group.1, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Group.1), ]
  #Select the WC and first 4 linguistic
  graph_data <- subset(df,select=c(Group.1, WC, Analytic, Clout, Authentic, Tone))
  #Melt and then plot
  graph_data_melted <- melt(graph_data,id="Group.1")
  #Plot
  graph<-ggplot(graph_data_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
    geom_line() + xlab("Day")+ylab("Average")+
    geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))+
    ggtitle(paste("Thread ID" ,toString(head(data$ThreadID,n=1)),sep=" "))
  
  return(graph)
}

#DAILY TREND for remaining linguistic variables
days_graph2<-function(data){
  df <- aggregate(data, by=list(data$Day), mean)
  #Order them in days
  df$Group.1 <- factor(df$Group.1, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- df[order(df$Group.1), ]
  #Select remaining linguistic variables
  graph_data <- subset(df,select=-c(ThreadID, AuthorID, Date, Time, WC, Analytic, Clout, Authentic, Tone, MonthYear, Day, Hour))
  #Melt and then plot
  graph_data_melted <- melt(graph_data,id="Group.1")
  #Plot
  graph<-ggplot(graph_data_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
    geom_line() + xlab("Day")+ylab("Average")+
    geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))+
    ggtitle(paste("Thread ID" ,toString(head(data$ThreadID,n=1)),sep=" "))
  
  return(graph)
}


#DO FOR DAYS IN A WEEK FOR FIRST FOUR LINGUISTIC VARIABLES
days_graph(first)
days_graph(second)

#DO FOR DAYS IN A WEEK BUT FOR REMAINING LINGUISTIC VARIABLES
days_graph2(first)
days_graph2(second)


#HOURLY TREND
hours_graph<-function(data){
  df <- aggregate(data, by=list(data$Hour), mean)
  #For WC and first 4 linguistic variables
  graph_data <- subset(df,select=c(Group.1, WC, Analytic, Clout, Authentic, Tone))
  graph_data_melted <- melt(graph_data,id="Group.1")
  graph<-ggplot(graph_data_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
    geom_line() + xlab("Hours of the day")+ylab("Average")+
    geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))+
    ggtitle(paste("Thread ID" ,toString(head(data$ThreadID,n=1)),sep=" "))
  
  return(graph)
}

hours_graph2<-function(data){
  df <- aggregate(data, by=list(data$Hour), mean)
  #For WC and first 4 linguistic variables
  graph_data <- subset(df,select=-c(ThreadID, AuthorID, Date, Time, WC, Analytic, Clout, Authentic, Tone, MonthYear, Day, Hour))
  graph_data_melted <- melt(graph_data,id="Group.1")
  graph<-ggplot(graph_data_melted, aes(x=Group.1, y=value, colour=variable, group=variable)) + 
    geom_line() + xlab("Hours of the day")+ylab("Average")+
    geom_dl(aes(label=variable),method=list(dl.trans(x=x-0.2),"first.points",cex=0.8))+
    ggtitle(paste("Thread ID" ,toString(head(data$ThreadID,n=1)),sep=" "))
  return(graph)
}

#FOR HOURS IN A DAY FOR FIRST FOUR LINGUISTIC VARIABLES
hours_graph(first)
hours_graph(second)

#FOR HOURS IN A DAY FOR REMAINING LINGUISTIC VARIABLES
hours_graph2(first)
hours_graph2(second)




# PART C

#We will look into the author that made the most posts. It is likely
# that this author would have posted in different threads, and so will use
# that to develop an extended social network.

#Count how many posts each author made
partc_df<-as.data.frame(table(webforum$AuthorID))
#Sort them
sort_author <- partc_df[order(partc_df$Freq),]
#View the top few authors
display_top_authors<-tail(sort_author)
names(display_top_authors)[names(display_top_authors)=="Var1"]<-"AuthorID"        #Change column name
names(display_top_authors)[names(display_top_authors)=="Freq"]<-"Total number of posts"        #Change column name
grid.table(display_top_authors)
#Get the ID of the top author
top_author<-tail(sort_author, n=1)
top_author
top_author<-top_author$Var1
top_author

#Search for all the posts made by this author
author_posts <- webforum[webforum$AuthorID %in% top_author,]


author_posts

#Find the thread that the author posted in the most
most_post_thread<-as.data.frame(table(author_posts$ThreadID))
most_post_thread
most_post_thread<-most_post_thread[order(most_post_thread$Freq),]
#Find the top thread that the author posted in
tail(most_post_thread)
names(most_post_thread)[names(most_post_thread)=="Var1"]<-"ThreadID"        #Change column name
names(most_post_thread)[names(most_post_thread)=="Freq"]<-"Num of posts that top author contributed"        #Change column name
grid.table(tail(most_post_thread))
#Get the ID with the most posts
most_post_thread_ID<-tail(most_post_thread$ThreadID, n=1)
most_post_thread_ID

#Get all the posts in that thread
thread_posts <- webforum[webforum$ThreadID %in% most_post_thread_ID,]
thread_posts

#Have a look at the number of posts over time
partc_graph_data<-as.data.frame(table(thread_posts$Date))
partc_graph_data
names(partc_graph_data)[names(partc_graph_data)=="Var1"]<-"Date"        #Change column names
names(partc_graph_data)[names(partc_graph_data)=="Freq"]<-"Frequency"   #Change column names
partc_graph_data$Date <- as.Date(partc_graph_data$Date)     #Convert to date data type again
ggplot(partc_graph_data, aes(x=Date, y=Frequency),group="Frequency") + 
  geom_line(color="red") + xlab("Date")+ylab(paste("Number of posts in Thread",toString(head(thread_posts$ThreadID,n=1)),sep=" "))+
  ggtitle("Number of posts over time for ThreadID 145223")

#Looking at the graph and data, it can be seen that there's a large amount of acitivty in the month of August in 2004,
#so we will use that month for the first social network diagram.

#All data in August 2004
aug_2004_posts<-author_posts[author_posts$Date<="2004-08-31",]
aug_2004_posts<-aug_2004_posts[aug_2004_posts$Date>="2004-08-01",]
aug_2004_posts

#View the threads that the author has contributed to in August 2004
threads_in_aug_2004<-as.data.frame(table(aug_2004_posts$ThreadID))
threads_in_aug_2004

#Sort it
sorted_threads_in_aug_2004<-threads_in_aug_2004[order(threads_in_aug_2004$Freq),]
sorted_threads_in_aug_2004
#Display
names(sorted_threads_in_aug_2004)[names(sorted_threads_in_aug_2004)=="Var1"]<-"ThreadID"        #Change column name
names(sorted_threads_in_aug_2004)[names(sorted_threads_in_aug_2004)=="Freq"]<-"Num of posts top author posted in Aug 2004"        #Change column name
grid.table(sorted_threads_in_aug_2004)

#Get the top two threads
Thread1<-tail(sorted_threads_in_aug_2004$ThreadID, n=1)
Thread2<-head(tail(sorted_threads_in_aug_2004$ThreadID, n=2),n=1)

#This function filters the data and creates a list of strings with the user ID's within the thread
get_data<-function(ID, start_date, end_date){
  thread_data <- webforum[webforum$ThreadID %in% ID,]
  thread_data
  #Filter for a particular month
  filtered_thread_data<-thread_data[thread_data$Date<=end_date,]
  filtered_thread_data<-filtered_thread_data[filtered_thread_data$Date>=start_date,]
  filtered_thread_data
  #Get the ID's
  author_IDs<-as.data.frame(table(filtered_thread_data$AuthorID))
  #Turn them into a list of strings seperated by commas
  author_IDs<-author_IDs$Var1
  id_string<-toString(author_IDs)
  author_ID_strings<-unlist(strsplit(id_string, ","))
  return(author_ID_strings)
}

#Re-attach the library
library(igraph)

#FOR FIRST THREAD
author_ID_strings_1<-get_data(Thread1,"2004-08-01","2004-08-31")

#Adjacency matrix for the graph
adj_matrix_1<-matrix(1, nrow=(length(author_ID_strings_1)),ncol=(length(author_ID_strings_1)),dimnames = list(author_ID_strings_1,author_ID_strings_1))
#Note: Diagonal will be zeroed out by the graph.adjacency... function below
adj_matrix_1

#Plot this matrix as a network
gg_1<-graph.adjacency(adj_matrix_1, mode	=	"undirected",	weighted	=	NULL, diag=FALSE)
plot(gg_1)



#FOR SECOND THREAD
author_ID_strings_2<-get_data(Thread2,"2004-08-01","2004-08-31")

#Adjacency matrix for the graph
adj_matrix_2<-matrix(1, nrow=(length(author_ID_strings_2)),ncol=(length(author_ID_strings_2)),dimnames = list(author_ID_strings_2,author_ID_strings_2))
#Note: Diagonal will be zeroed out by the graph.adjacency... function below
adj_matrix_2

#Plot this matrix as a network
gg_2<-graph.adjacency(adj_matrix_2, mode	=	"undirected",	weighted	=	NULL, diag=FALSE)
plot(gg_2)


#JOIN FIRST AND SECOND THREADS
joined_network	=	(gg_1	%u%	gg_2)
plot(joined_network)


#ANALYSIS OF THIS NETWORK
hist(degree(joined_network),	breaks	=	15,	col	=	"grey")

degree<-as.table(degree(joined_network))

betweenness<-as.table(betweenness(joined_network))

closeness<-as.table(closeness(joined_network))

eigenvector <- as.table(evcent(joined_network)$vector)

#Combine everything
part_c_df2<-as.data.frame(rbind(degree, betweenness, closeness, eigenvector))
#Display as a table
grid.table(t(part_c_df2))



#REPEAT EVERYTHING ABOVE FOR THE FOLLOWING MONTH (sEPTEMBER)
#FOR FIRST THREAD
author_ID_strings_4<-get_data(Thread1,"2004-09-01","2004-09-30")

#Adjacency matrix for the graph
adj_matrix_4<-matrix(1, nrow=(length(author_ID_strings_4)),ncol=(length(author_ID_strings_4)),dimnames = list(author_ID_strings_4,author_ID_strings_4))
#Note: Diagonal will be zeroed out by the graph.adjacency... function below
adj_matrix_4

#Plot this matrix as a network
gg_4<-graph.adjacency(adj_matrix_4, mode	=	"undirected",	weighted	=	NULL, diag=FALSE)
plot(gg_4)



#FOR SECOND THREAD, NOTE: NO USER ACTIVITY FOUND IN THIS MONTH FOR THIS THREAD, SO NETWORK SHOULD BE EMPTY
author_ID_strings_5<-get_data(Thread2,"2004-09-01","2004-09-30")

#Adjacency matrix for the graph
adj_matrix_5<-matrix(1, nrow=(length(author_ID_strings_5)),ncol=(length(author_ID_strings_5)),dimnames = list(author_ID_strings_5,author_ID_strings_5))
#Note: Diagonal will be zeroed out by the graph.adjacency... function below
adj_matrix_5

#Plot this matrix as a network (SHOULD BE EMPTY)
gg_5<-graph.adjacency(adj_matrix_5, mode	=	"undirected",	weighted	=	NULL, diag=FALSE)
plot(gg_5)


#JOIN FIRST AND SECOND THREADS (No change because 2nd graph is empty)
joined_network_2	=	(gg_4	%u%	gg_5)
plot(joined_network_2)



#OCTOBER HAD NO ACTIVITY IN THE THREAD, SO THERE IS NO GRAPH

#END OF ASSIGNMENT CODE