# VENDORS COOKBOOX
# this is the cookbook for the vendors analysis

library(tidyverse)
library(readxl)

#call in additional fonts
library(extrafont)
windowsFonts(Times=windowsFont("Times New Roman"))
windowsFonts(Century=windowsFont("Century"))


vendor_list <- read_excel("active_vendors_dec_17.xlsx")

#rename cols of monthly table to match vendor list table
colnames(vendor_list)[1] <- "VendorId"
colnames(vendor_list)[2] <- "Name"

#turn the id col to numeric which trims the leading zeroes
vendor_list$VendorId <- as.numeric(vendor_list$VendorId)

# read in FY tables
fy13 <- read_excel("all_fy_13.xlsx")
fy14 <- read_excel("all_fy_14.xlsx")
fy15 <- read_excel("all_fy_15.xlsx")
fy16 <- read_excel("all_fy_16.xlsx")
fy17 <- read_excel("all_fy_17.xlsx")

# add a FY field
fy13$FiscalYear <- "2012-2013"
fy14$FiscalYear <- "2013-2014"
fy15$FiscalYear <- "2014-2015"
fy16$FiscalYear <- "2015-2016"
fy17$FiscalYear <- "2016-2017"

#bind the objects by rows
all_purchases <- rbind(fy13, fy14, fy15, fy16, fy17)

#use a left outer join to match all the rows from left and matches from right.
all_vendors <- merge(all_purchases, vendor_list, all.x=T)

#set penalty for scientific notation
options(scipen = 999)

#use complete.cases to remove those with an NA in the city and/or state cols. nearly 280K rows, so down to 1.2 million
all_vendors <- all_vendors[complete.cases(all_vendors[, 30:31]),]

# group and sum for count and total spending per Description. This is for reference only.
by_desc <- group_by(all_vendors, Descr)
sumUnfiltered <- summarize(by_desc,
count = n(),
spending = sum(ExpendedAmount))

#filtering out grants and everything clearly and obviously passing money around in the state.
vendors_final_filter <- filter(all_vendors, Descr != "Grants To Public Schools-Univ" & Descr != "Grants To Local Governments"&
Descr != "Grants -Higher Ed Institution" & Descr != "Grants to Native Amer Indians"&
Descr != "ISD Services" & Descr != "DOIT HCM Assessment Fees" & Descr != "Other Financing Uses" &
Descr != "Debt Service-Principal" & Descr != "Other Services - Interagency" &
Descr != "Employee Liability Ins Premium" & Descr != "Grants to Other Agencies" &
Descr != "Temporary Positions F/T & P/T" & Descr != "GSD Work Comp Insur Premium" &
Descr != "Receipts Held In Suspense" & Descr != "Unemployment Comp Premium" &
Descr != "Employee I/S Meals & Lodging" & Descr != "Employee O/S Mileage & Fares" &
Descr != "Revenue Bonds Payable" & Descr != "Grants To Individuals" &
Descr != "Workers' Comp Assessment Fee" & Descr != "Other Receivables" & Descr != "F I C A" &
Descr != "Professional Svcs - Interagenc" & Descr != "Transp - Transp Insurance" &
Descr != "Interest On Investments" & Descr != "Employee I/S Mileage & Fares" &
Descr != "Brd & Comm O/S Mileage & Fares" & Descr != "Brd & Comm O/S Meals & Lodging" &
Descr != "Other Employee Benefits" & Descr != "Insurance Assessments" &
Descr != "Bond Premiums" & Descr != "Group Insurance Premium" &
Descr != "Undistributed Int. Earnings" & Descr != "Deposits Held for Others" &
Descr != "Miscellaneous Revenue" & Descr != "DOIT Telecommunications" &
Descr != "Other Services - CU" & Descr != "GCD Radio Communications Svcs" &
Descr != "State Transp Pool Charges"
)

# group and sum for count and total spending, post-filter.
by_desc_filtered <- group_by(vendors_final_filter, Descr)
sumFiltered <- summarize(by_desc_filtered,
count = n(),
spending = sum(ExpendedAmount))


#exploration filtering out almost everything local/state.
onlyNM <- filter(vendors_final_filter, STATE == "NM")
notNM <- filter(vendors_final_filter, STATE != "NM")
sum(onlyNM$ExpendedAmount)
sum(notNM$ExpendedAmount)



#
# sumFiltered gives the total. use onlyNM and notNM with same operation to get top descrs
#
#
by_desc_onlyNM <- group_by(onlyNM, Descr)
sumDesc_onlyNM <- summarize(by_desc_onlyNM,
count = n(),
spending = sum(ExpendedAmount))

by_desc_notNM <- group_by(notNM, Descr)
sumDesc_notNM <- summarize(by_desc_notNM,
count = n(),
spending = sum(ExpendedAmount))

#
#
#
#plot comparing IS/OOS by spending description
sumDesc_onlyNM$desc <- as.factor("Inside NM")
sumDesc_onlyNM_top <- top_n(sumDesc_onlyNM, 7, spending)
sumDesc_notNM$desc <- as.factor("Outside NM")
sumDesc_notNM_top <- top_n(sumDesc_notNM, 7, spending)
spending_desc <- rbind(sumDesc_onlyNM_top, sumDesc_notNM_top)

#divide and format spending for use in internal labels
spending_desc$spending <- spending_desc$spending / 1000000 #get down to 100s (subhed will explain in millions)
spending_desc$spending <- format(spending_desc$spending, digits = 3) #format sets number of sig digits
spending_desc <- filter(spending_desc, Descr != "Rent Of Land & Buildings" & Descr != "Supplies-Drugs" 
& Descr != "Supplies-Field Supplies" & Descr != "Buildings & Structures")

#factor the descr
spending_desc$Descr <- as.factor(spending_desc$Descr)
spending_desc$Descr <- factor(spending_desc$Descr, levels = c("Other Services", "Medical Services", "Professional Services",
"IT Services", "Miscellaneous Expense"))
spending_desc$Descr <- filter(spending_desc$Descr, Descr != "NA")

#plot as bars
ggplot(spending_desc, aes(x=Descr, y=spending), group = desc)+
geom_bar(aes(fill = desc), stat = "identity", position = "dodge", color = "black")+
geom_text(aes(label=paste0("$", format(spending))), position=position_dodge(width=0.9), vjust=-0.25, size = 4, color = "grey30")+
scale_fill_manual(values = c("indianred3", "turquoise4"), labels = c("Total to in-state vendors", "Total to out-of-state vendors"))+
#scale_x_discrete(labels=c("2013", "2014", "2015", "2016", "2017")) +
#coord_cartesian(ylim=c(0, 2000000000)) +
#scale_y_continuous(breaks = c(0, 500000000, 1000000000, 1500000000, 2000000000), labels = c("0", " ", "1", " ", "2"))+
theme(axis.text = element_text(family = "Times", size=10), axis.title = element_text(family="Times", size=12),
legend.text=element_text(family="Times", size=10),
plot.title = element_text(family = "Century", size=14, face = "bold"), panel.grid.major = element_line(color="white"),
panel.grid.minor = element_line(color="white"),
plot.subtitle = element_text(family = "Century", size=12),
legend.position = "bottom",
legend.title = element_blank(), #removes legend title
axis.text.y = element_blank(),  #removes y axis
panel.background=element_rect(fill="white"),
axis.text.x = element_text(color = "black"),
legend.key = element_blank(), #remove grey boxes in legend
axis.ticks = element_blank(), #remove axis ticks
plot.caption = element_text(family = "Times", size = 10))+
labs(title="New Mexico's annual payments to vendors by location", subtitle = "in millions of dollars per fiscal year ending", x=" ", y=" ",
caption = "Source: Searchlight New Mexico analysis of DFA data
John R. Roby / Searchlight New Mexico")



#how many unique vendors? 20,615
length(unique(vendors_final_filter$VendorId))

#how many unique POs? 214957
length(unique(vendors_final_filter$PoId))

#how many transactions? 1,174,826, down from 1,513,177 (due to filtering descs and no addys

#number and amt of transactions by year
by_year_filtered <- group_by(vendors_final_filter, FiscalYear)
sumYearFiltered <- summarize(by_year_filtered,
count = n(),
spending = sum(ExpendedAmount))


#number and amt and origin of transactions by year
by_year_InNM <- group_by(onlyNM, FiscalYear)
sumYear_InNM <- summarize(by_year_InNM,
count = n(),
spending = sum(ExpendedAmount))

by_year_NotNM <- group_by(notNM, FiscalYear)
sumYear_NotNM <- summarize(by_year_NotNM,
count = n(),
spending = sum(ExpendedAmount))

#plot comparing spending in and out of state
#first factor and combine the two tables
sumYear_InNM$dest <- as.factor("Inside NM")
sumYear_NotNM$dest <- as.factor("Outside NM")
spending_dest <- rbind(sumYear_InNM, sumYear_NotNM)


#divide and format spending for use in internal labels
spending_dest$spending_dec <- spending_dest$spending / 1000000 #get down to 100s (subhed will explain in millions)
spending_dest$spending_dec <- format(spending_dest$spending_dec, digits = 3) #format sets number of sig digits

#plot as bars
ggplot(spending_dest, aes(x=FiscalYear, y=spending_dec, group = dest))+
geom_bar(aes(fill = dest), stat = "identity", position = "dodge", color = "black")+
geom_text(aes(label=paste0("$", format(spending_dec))), position=position_dodge(width=0.9), vjust=-0.25, size = 4, color = "grey30")+
scale_fill_manual(values = c("indianred3", "turquoise4"), labels = c("Total to in-state vendors", "Total to out-of-state vendors"))+
scale_x_discrete(labels=c("2013", "2014", "2015", "2016", "2017")) +
#coord_cartesian(ylim=c(0, 2000000000)) +
#scale_y_continuous(breaks = c(0, 500000000, 1000000000, 1500000000, 2000000000), labels = c("0", " ", "1", " ", "2"))+
theme(axis.text = element_text(family = "Times", size=10), axis.title = element_text(family="Times", size=12),
legend.text=element_text(family="Times", size=10),
plot.title = element_text(family = "Century", size=14, face = "bold"), panel.grid.major = element_line(color="white"),
panel.grid.minor = element_line(color="white"),
plot.subtitle = element_text(family = "Century", size=12),
legend.position = "bottom",
legend.title = element_blank(), #removes legend title
axis.text.y = element_blank(),  #removes y axis
panel.background=element_rect(fill="white"),
axis.text.x = element_text(color = "black"),
legend.key = element_blank(), #remove grey boxes in legend
axis.ticks = element_blank(), #remove axis ticks
plot.caption = element_text(family = "Times", size = 10))+
labs(title="New Mexico's annual payments to vendors by location", subtitle = "in millions of dollars per fiscal year ending", x=" ", y=" ",
caption = "Source: Searchlight New Mexico analysis of DFA data
John R. Roby / Searchlight New Mexico")



### group and sum by biz unit -- which govt agencies are spending most IS and OOS -- use the InNM/NotNM tables
#number and amt and origin of transactions by year
by_unit_InNM <- group_by(onlyNM, BusinessUnitId)
sumUnit_InNM <- summarize(by_unit_InNM,
count = n(),
spending = sum(ExpendedAmount))

by_unit_NotNM <- group_by(notNM, BusinessUnitId)
sumUnit_NotNM <- summarize(by_unit_NotNM,
count = n(),
spending = sum(ExpendedAmount))

#plot comparing spending in and out of state
#limited utility as can only do so many at once. 
#first factor and combine the two tables
sumUnit_InNM$dest <- as.factor("Inside NM")
sumUnit_NotNM$dest <- as.factor("Outside NM")
unit_dest <- rbind(sumUnit_InNM, sumUnit_NotNM)



###
###
#find ratio of IS/OOS spending by dept
#remove the count category
unit_dest <- subset(unit_dest, select = -c(count))
#spread the cols and then rename
unit_dest <- spread(unit_dest, dest, spending)
colnames(unit_dest)[2] <- "InsideNM"
colnames(unit_dest)[3] <- "OutsideNM"
#figure the ratio. anything below 1 is a deficit. 20 depts total
unit_dest$the_ratio <- with(unit_dest, InsideNM / OutsideNM)

#marry with the biz unit id table
biz_unit_codes <- read_excel("biz_unit_key.xlsx")
unit_dest <- merge(unit_dest, biz_unit_codes, by = "BusinessUnitId", all.x=T)

#use complete.cases to remove those with an NA in the BusinessUnitname col.
unit_dest <- unit_dest[complete.cases(unit_dest[, 5]),]
test1 <- subset(unit_dest, the_ratio <= 1)
#of 111 departments, agencies, offices and boards, 27 spent more with OOS vendors than IS vendors

#select top/bottom 20 using top_n wrapper function from dplyr
top20 <- top_n(unit_dest, 20, the_ratio)
bottom20 <- top_n(unit_dest, -20, the_ratio)
#gather back together and restore dest column, preserving the in and out spending
bottom20 <- gather(bottom20, key = dest, value = spending, InsideNM, OutsideNM)
top20 <- gather(top20, key = dest, value = spending, InsideNM, OutsideNM)


#plot bottom 20, using log10 scale because range. requires scales pkg
library(scales)
ggplot(bottom20, aes(x=reorder(BusinessUnitName, spending), y=spending, group = dest))+
geom_point(aes(color = dest), size = 4, pch = 18, alpha = 0.8)+
scale_color_manual(values = c("indianred3", "turquoise4"),
labels = c("Total to in-state vendors", "Total to out-of-state vendors"))+
coord_flip()+
scale_y_log10(labels = dollar, breaks = 10^(0:9))+ #log10 scale transformaton
theme(axis.text = element_text(family = "Times", size=10), axis.title = element_text(family="Times", size=12),
legend.text=element_text(family="Times", size=10),
plot.title = element_text(family = "Century", size=14, face = "bold"), panel.grid.major = element_line(color="grey90"),
panel.grid.minor = element_line(color="white"),
legend.position = "bottom",
legend.title = element_blank(), #removes legend title
panel.background=element_rect(fill="white"),
legend.key = element_blank(), #remove grey boxes in legend
axis.ticks = element_blank(), #remove axis ticks
axis.text.x = element_text(color = "black", hjust = 0.8), #hjust nudges the axis text to the left a bit
axis.text.y = element_text(color = "black"),
plot.subtitle = element_text(family = "Century", size=12, hjust = 0.5),
plot.caption = element_text(family = "Times", size = 10))+
labs(title="Agencies paying more to out-of-state vendors, 2013-2017",
y = " ", x=" ",
caption = "Source: Searchlight New Mexico analysis of DFA data
John R. Roby / Searchlight New Mexico")




#plot top 20, using log10 scale because range. requires scales pkg
library(scales)
#filter out the nas
top20_filtered <- filter(top20, BusinessUnitName != "NA")
#plot
ggplot(top20_filtered, aes(x=reorder(BusinessUnitName, spending), y=spending, group = dest))+
geom_point(aes(color = dest), size = 4, pch = 18, alpha = 0.9)+
scale_color_manual(values = c("indianred3", "turquoise4"),
labels = c("Total to in-state vendors", "Total to out-of-state vendors"))+
coord_flip()+
scale_y_log10(labels = dollar, breaks = 10^(0:9))+ #log10 scale transformaton
theme(axis.text = element_text(family = "Times", size=10), axis.title = element_text(family="Times", size=12),
legend.text=element_text(family="Times", size=10),
plot.title = element_text(family = "Century", size=14, face = "bold"), panel.grid.major = element_line(color="grey90"),
panel.grid.minor = element_line(color="white"),
legend.position = "bottom",
legend.title = element_blank(), #removes legend title
panel.background=element_rect(fill="white"),
axis.text.x = element_text(color = "black"),
axis.text.y = element_text(color = "black"),
legend.key = element_blank(), #remove grey boxes in legend
axis.ticks = element_blank(), #remove axis ticks
plot.subtitle = element_text(family = "Century", size=12, hjust = 0.5),
plot.caption = element_text(family = "Times", size = 10))+
labs(title="Agencies paying the most to in-state vendors, 2013-2017",
y = " ", x=" ",
#y = expression(paste(log[2]," (dollars)")), #how to insert a log subset in labels
caption = "Source: Searchlight New Mexico analysis of DFA data
John R. Roby / Searchlight New Mexico")





### group and sum by vendor ID -- top IS and top OOS -- use the INNM/NotNM tables
### which IS and OOS vendors are getting the most 
by_vendor_InNM <- group_by(onlyNM, VendorId, Name, ADDRESS1, CITY, STATE, POSTAL)
sumVendor_InNM <- summarize(by_vendor_InNM,
count = n(),
spending = sum(ExpendedAmount))

by_vendor_NotNM <- group_by(notNM, VendorId, Name, ADDRESS1, CITY, STATE, POSTAL)
sumVendor_NotNM <- summarize(by_vendor_NotNM,
count = n(),
spending = sum(ExpendedAmount))

#plot of top 20 OOS vendors
#select top 20 from each using top_n wrapper function from dplyr. must ungroup first for top_n to work
sumVendor_InNM_ungrouped <- ungroup(sumVendor_InNM)
sumVendor_NotNM_ungrouped <- ungroup(sumVendor_NotNM)
vendors_InNM_top20 <- top_n(sumVendor_InNM_ungrouped, 21, spending) #21 because need to merge presby health w and w/o comma below
vendors_NotNM_top20 <- top_n(sumVendor_NotNM_ungrouped, 20, spending)
#divide spending for scaling bars
vendors_NotNM_top20$spending_div <- vendors_NotNM_top20$spending / 1000000
vendors_NotNM_top20$spending_div <- format(vendors_NotNM_top20$spending_div, digits = 3) #format sets number of sig digits
vendors_NotNM_top20$spending_div <- as.numeric(vendors_NotNM_top20$spending_div)
#plot
ggplot(vendors_NotNM_top20, aes(x=reorder(Name, spending_div), y=spending_div))+
geom_bar(stat = "identity", color = "black", fill = "salmon", width = 1)+
coord_flip()+
geom_text(aes(label=Name), hjust = -0.1, size = 3, inherit.aes=T)+
scale_fill_manual(values = "salmon")+
#scale_x_discrete(labels=c("2013", "2014", "2015", "2016", "2017")) +
#coord_cartesian(ylim=c(0, 600)) +
scale_y_continuous(breaks = c(0, 150, 300, 450), labels = c("$0", "$150", "$300", "$450"), limits = c(0,800))+
theme(axis.text = element_text(family = "Times", size=10), axis.title = element_text(family="Times", size=12),
legend.text=element_text(family="Times", size=10),
plot.title = element_text(family = "Century", size=14, face = "bold", hjust = 0.1), panel.grid.major.x = element_line(color="grey90"),
panel.grid.minor = element_line(color="white"),
plot.subtitle = element_text(family = "Century", size=12, hjust = 0.06),
legend.position = "bottom",
legend.title = element_blank(), #removes legend title
axis.text.y = element_blank(),  #removes y axis
panel.background=element_rect(fill="white"),
legend.key = element_blank(), #remove grey boxes in legend
axis.ticks = element_blank(), #remove axis ticks
plot.caption = element_text(family = "Times", size = 10))+
labs(title="New Mexico's top-paid out-of-state vendors, 2013-2017", subtitle = "total, in millions of dollars", x=" ", y=" ",
caption = "Source: Searchlight New Mexico analysis of DFA data
John R. Roby / Searchlight New Mexico")


#need to combine presbyterian health plan w and w/out the comma
vendors_InNM_top20[12,8] <- vendors_InNM_top20[12,8] + vendors_InNM_top20[19,8]
vendors_InNM_top20 <- vendors_InNM_top20[rownames(vendors_InNM_top20) != 19,]

#divide spending for scaling bars
vendors_InNM_top20$spending_div <- vendors_InNM_top20$spending / 1000000
vendors_InNM_top20$spending_div <- format(vendors_InNM_top20$spending_div, digits = 3) #format sets number of sig digits
vendors_InNM_top20$spending_div <- as.numeric(vendors_InNM_top20$spending_div)

#plot
ggplot(vendors_InNM_top20, aes(x=reorder(Name, spending_div), y=spending_div))+
geom_bar(stat = "identity", color = "black", fill = "lightblue3", width = 1)+
coord_flip()+
geom_text(aes(label=Name), hjust = -0.1, size = 3, inherit.aes=T)+
#scale_x_discrete(labels=c("2013", "2014", "2015", "2016", "2017")) +
#coord_cartesian(ylim=c(0, 600)) +
scale_y_continuous(breaks = c(0, 150, 300, 450), labels = c("$0", "$150", "$300", "$450"), limits = c(0,800))+
theme(axis.text = element_text(family = "Times", size=10), axis.title = element_text(family="Times", size=12),
legend.text=element_text(family="Times", size=8),
plot.title = element_text(family = "Century", size=14, face = "bold", hjust = 0.1), panel.grid.major.x = element_line(color="grey90"),
panel.grid.minor = element_line(color="white"),
plot.subtitle = element_text(family = "Century", size=12, hjust = 0.06),
legend.position = "bottom",
legend.title = element_blank(), #removes legend title
axis.text.y = element_blank(),  #removes y axis
panel.background=element_rect(fill="white"),
legend.key = element_blank(), #remove grey boxes in legend
axis.ticks = element_blank(), #remove axis ticks
plot.caption = element_text(family = "Times", size = 10))+
labs(title="New Mexico's top-paid in-state vendors, 2013-2017", subtitle = "total, in millions of dollars", x=" ", y=" ",
caption = "Source: Searchlight New Mexico analysis of DFA data
John R. Roby / Searchlight New Mexico")



####
