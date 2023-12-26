library(tidyr)
library(dplyr)
library(stargazer)

#Loading the data in RStudio
ab = read.csv("~/Downloads/Academics/AMforBus/Midterm Project/Abandoned.csv", header = TRUE, na.strings = "")
View(ab)
rs = read.csv("~/Downloads/Academics/AMforBus/Midterm Project/Reservation.csv", header = TRUE, na.strings = "")
View(rs)

#1. Retargeting customers is beneficial

#2.
#Number of Control and Test group in abandoned.csv
nrow(ab[ab$Test_Control=="control",])
nrow(ab[ab$Test_Control=="test",])

#Number of Control and Test group in reservation.csv
nrow(rs[rs$Test_Control=="control",])
nrow(rs[rs$Test_Control=="test",])

#Number of Control and Test group in both in table form, easier to compare
table(ab$Test_Control)
table(rs$Test_Control)

#3.
#Calculating summary statistics for the test variable, segmenting by available State data
table(ab$Address, ab$Test_Control)
table(rs$Address, rs$Test_Control)

#4. Potential Keys: Email, Incoming_Phone and Contact_Phone
match_email = ab$Email[complete.cases(ab$Email)] %in% rs$Email[complete.cases(rs$Email)]
match_incoming = ab$Incoming_Phone[complete.cases(ab$Incoming_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]
match_contact = ab$Contact_Phone[complete.cases(ab$Contact_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]
match_incoming_contact = ab$Incoming_Phone[complete.cases(ab$Incoming_Phone)] %in% rs$Contact_Phone[complete.cases(rs$Contact_Phone)]
match_contact_incoming = ab$Contact_Phone[complete.cases(ab$Contact_Phone)] %in% rs$Incoming_Phone[complete.cases(rs$Incoming_Phone)]

ab$match_email = 0
ab$match_email[complete.cases(ab$Email)] = 1*match_email

ab$match_incoming = 0
ab$match_incoming[complete.cases(ab$Incoming_Phone)] = 1*match_incoming

ab$match_contact = 0
ab$match_contact[complete.cases(ab$Contact_Phone)] = 1*match_contact

ab$match_incoming_contact = 0
ab$match_incoming_contact[complete.cases(ab$Incoming_Phone)] = 1*match_incoming_contact

ab$match_contact_incoming = 0
ab$match_contact_incoming[complete.cases(ab$Contact_Phone)] = 1*match_contact_incoming

#5.
# *1 is used so that we get 0s and 1s instead of FALSEs and TRUEs, it's just like using as.numeric()
ab$pur = 1*(ab$match_email | ab$match_incoming | ab$match_contact | ab$match_incoming_contact | ab$match_contact_incoming)

#Treatment group who purchased
ab[ab$Test_Control=='test' & ab$pur==0,]
#Treatment group who didn't purchase
ab[ab$Test_Control=='test' & ab$pur==1,]
#Control group who purchased
ab[ab$Test_Control=='control' & ab$pur==0,]
#Control group who didn't purchase
ab[ab$Test_Control=='control' & ab$pur==1,]

#Consolidated cross tabulation for count of Test/Control who purchased/not purchased
table(ab$pur,ab$Test_Control)

#6.
############################# We won't use this code if we only have to check for duplicates ####################################
ab = ab %>%
  mutate(unm = ifelse(
    !(Email %in% rs$Email |
        Incoming_Phone %in% rs$Incoming_Phone |
        Contact_Phone %in% rs$Contact_Phone |
        Incoming_Phone %in% rs$Contact_Phone |
        Contact_Phone %in% rs$Incoming_Phone), 1, 0))

#No. of unmatched columns
nrow(ab[ab$unm==1,])

#Removing unmatched columns
ab = ab[ab$unm==0,]
#################################################################################################################################

############################################### Code to check and remove duplicates #############################################

###Checking duplicates
#For Emails
is_duplicated <- duplicated(ab$Email) | duplicated(ab$Email)

#Getting duplicates excluding NA values
remove = ab$Email[is_duplicated & !is.na(ab$Email)]

#For Incoming_Phone
is_duplicated <- duplicated(ab$Incoming_Phone) | duplicated(ab$Incoming_Phone)

#Getting duplicates excluding NA values
remove = ab$Incoming_Phone[is_duplicated & !is.na(ab$Incoming_Phone)]

#For Contact_Phone
is_duplicated <- duplicated(ab$Contact_Phone) | duplicated(ab$Contact_Phone)

#Getting duplicates excluding NA values
remove = ab$Contact_Phone[is_duplicated & !is.na(ab$Contact_Phone)]


###Removing duplicate Emails
ab = ab[!duplicated(ab$Email, fromLast = TRUE) | is.na(ab$Email), ]

#Removing duplicate Incoming_Phones
ab = ab[!duplicated(ab$Incoming_Phone, fromLast = TRUE) | is.na(ab$Incoming_Phone), ]

#Removing duplicate Contact_Phones
ab = ab[!duplicated(ab$Contact_Phone, fromLast = TRUE) | is.na(ab$Contact_Phone), ]
#################################################################################################################################

#7.
table(ab$pur,ab$Test_Control)

#Customers who made reservations after the targeting campaign can be found as
ab_conv_cust = ab[ab$pur==1,]

#Whereas the customers that didn't make the reservations can be found as
ab_unconv_cust = ab[ab$pur==0,]

#8.
#unique is used so that we don't have duplicates in our random selection
states = unique(ab$Address[!is.na(ab$Address)])
set.seed(9996)  # Setting a seed value for reproducibility
random_states = sample(states, 5)

#Adding to create final clean dataset, ab$Treatment is used in this step
ab$HasEmail = 1*complete.cases(ab$Email)
ab$HasState = 1*complete.cases(ab$Address)
ab$Treatment = ifelse(ab$Test_Control == "test", 1, 0)

state_cross_tabs = list()

for (state in random_states) {
  i_data = ab[ab$Address == state, ]
  state_cross_tab = table(i_data$pur, i_data$Treatment)
  dimnames(state_cross_tab) = list(c("Not Purchased","Purchased"), c("Control Group", "Treatment Group"))
  state_cross_tabs[[state]] = state_cross_tab
}
print(state_cross_tabs)


#9. Cleaning the dataset
clean_ab = data.frame (
    "Caller_ID" = ab$Caller_ID,
    "Test_Group" = ab$Treatment,
    "Outcome" = ab$pur,
    "State_Available" = ab$HasState,
    "Email_Available" = ab$HasEmail
  )

clean_ab

#Exporting the clean data set as a csv
write.csv(clean_ab, file = "~/Downloads/Academics/AMforBus/Midterm Project/clean_ab.csv", row.names = FALSE)

#10. LMs
pred1 = lm(Outcome ~ Test_Group, data = clean_ab)
summary(pred1)

pred2 = lm(Outcome ~ Test_Group+State_Available, data = clean_ab)
summary(pred2)

pred3 = lm(Outcome ~ Test_Group+State_Available+Email_Available, data = clean_ab)
summary(pred3)

stargazer(pred1, pred2, pred3, type="text")

#11.
data = clean_ab[2:5]
aov_data = gather(data, key = "Treatment", value = "score", 1:4)

anova_result <- aov(score~Treatment,data = aov_data)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result, conf.level = 0.95)
print(tukey_result)
summary(tukey_result)
plot(tukey_result)

#13. Interactions
interaction_model1 <- lm(Outcome ~ Test_Group*State_Available, data = clean_ab)
interaction_model2 <- lm(Outcome ~ Test_Group*Email_Available, data = clean_ab)
interaction_model3 <- lm(Outcome ~ Test_Group*(State_Available + Email_Available), data = clean_ab)
summary(interaction_model1)
summary(interaction_model2)
summary(interaction_model3)

stargazer(interaction_model1, interaction_model2, interaction_model3, type="text")