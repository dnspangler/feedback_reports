## Setup ----------------

library(tidyverse)
library(lubridate)
library(rmarkdown)
library(readxl)
library(writexl)
# note: RDCOMClient can a bit tricky to install - You'll need Rtools installed, and the CRAN version is a bit out of date and has issues with the latest versions of R, so use devtools to cmpile from the source unsing
# devtools::install_github("omegahat/RDCOMClient")
library(RDCOMClient)

options(scipen = 999)

## Set report date to the first day of the month of the report to be generated
report_date = ymd("2023-10-01")

#Number of years of historical data to show in the figures
report_years = 1

queue_time_cutoff <- 3600 # Exclude queue times over...secs
ready_time_cutoff <- 3600 # Exclude ready times over...secs
call_time_cutoff <- 7200 # Exclude call times over...secs
referral_delay_cutoff <- 3600 # Consider calls waiting at a referral priority for over ... secs to be a new contact if dispatched

## Map numeric priority values to names

prios <- c("1"="1A","2"="1B","3"="2A","4"="2B","5"="3","7"="Referral")

## Function to convert seconds to minuutes:seconds

sec_to_time <- function(x){
  t <- seconds_to_period(x)
  ifelse(!is.na(x),
         ifelse(x<3600,
                sprintf('%02.f:%02.f', round(minute(t)), round(second(t))),
                sprintf('%02.f:%02.f:%02.f', round(hour(t)), round(minute(t)), round(second(t)))),
         NA)
}

# Function to remove swedish letters from file names
as_filename <- function(name){
  out <- gsub(" ","_",name) %>%
    gsub("Ä|Å","A",.) %>%
    gsub("å|ä","a",.) %>%
    gsub("ö","o",.) %>%
    gsub("Ö","O",.) %>%
    gsub("é|è","e",.)
  return(out)
}

# Load data ----------

# Variable descriptions:

# name               Identifier of user taking the call
# month              month of call
# caseid             unique identifier of call
# user_org           Organization of user taking the call
# call_org           Orgnization where the call originated
# Priority           Priority of the call (1/2/none)
# queue_time         Seconds from incoming call to answering call
# call_time          Seconds from answering call to ending call
# pout               Priority set by dispatcher (1-7)
# recpout            Priority recommended by clinical decision support system (1-7)
# cats               Clinical call type of incident
# age                Age of patient
# care_need          Indicator of incident involving a patient with care need (1/0) 
# patient_care       Indicator of incident with a single patient assessed using the CDSS (1/0)
# main_complete      Indicator of whether life threat questions were completed (1/0)
# pnr_lookup         Indicator of whether a valid personal identification number was captured (1/0)
# recontact_3day_txp Indicator of whether a patient with a PID number recontacted the EMD center and was transported by an ambulance within 72 hours (1/0)
# has_cat            Indicator of whether a clinical call type was selected for the patient
# mbs_complete       Indicator of whether an incident had a complete journal
# referral_delay     Seconds between documentation of a patient referral to dispatch of an ambulance (to capture cases where a referral is left as an open incident, the patient calls back, and an ambulance is dispatched)
# disp_ready_time    Seconds between answering a call and assigning a patient for dispatch of an ambulance
# any_prio1in        Indicator of whether a patient was transported priority 1 (lights & Sirens) to the hospital (1/0/NA = no transport))


report_data_fn <- "report_data.csv"

if(file.exists(report_data_fn)){
  
  report_data <- read_csv(report_data_fn)
  
}else{
  
  print("no data found, making some up")
  
  n = 10000
  
  #Note that this is completely fantastical data, but in the correct format. It's expected for instance that there should be lots of missing and correlated values - this is just so that you can test the functionality of the script.
  
  report_data <- data.frame(
    "month" = floor_date(seq(report_date+months(1)-years(1),report_date+months(1),by = 365/(n))[1:n],unit = "month"), 
    "caseid" = seq(1,n,1), 
    "CallId" = seq(1,n,1), 
    
    "queue_time" = round(rlnorm(n,meanlog = 0.8),2), 
    "call_time" = round(rlnorm(n,meanlog = 1.2)*60), 
    "referral_delay" = round(rlnorm(n,meanlog = 1.5)*60),
    "disp_ready_time" = round(rlnorm(n,meanlog = 1)*10),
    "age" = round(round(runif(n,1,100))),
    
    "has_close" = as.logical(rbinom(n,1,0.95)), 
    "care_need" = as.logical(rbinom(n,1,0.73)), 
    "patient_care" = as.logical(rbinom(n,1,0.64)), 
    "main_complete" = as.logical(rbinom(n,1,0.95)), 
    "pnr_lookup" = as.logical(rbinom(n,1,0.95)), 
    "has_cat" = as.logical(rbinom(n,1,0.9)), 
    "mbs_complete" = as.logical(rbinom(n,1,0.8)),   
    "any_prio1in" = as.logical(rbinom(n,1,0.15)), 
    "recontact_3day_txp" = as.logical(rbinom(n,1,0.014)), 
    
    "name" = sample(c("testop1","testop2","testop3","testop4","testop5","testop6"),size = n,replace = T), 
    "Priority" = sample(c(1,2,NA),size = n,replace = T), 
    "pout" = sample(c(1,2,3,4,5,7),size = n,replace = T), 
    "recpout" = sample(c(1,2,3,4,5,7),size = n,replace = T), 
    "cats" = sample(c("Tummy ache", 
                      "Big boo boo", 
                      "Fallen and cant get up", 
                      "Just can't even"),size = n,replace = T))
  report_data$user_org = ifelse(report_data$name %in% c("testop1","testop2","testop3"),"testregion1","testregion2")
  report_data$call_org = ifelse(report_data$name %in% c("testop1","testop2","testop3"),"testregion1","testregion2")
  
  write_csv(report_data, report_data_fn)
}


#load("Intervention/svlc_report_data.rda")

generate_measures <- function(d){
  
  # Note: In our data priorities are as follows:
  # 1=1A
  # 2=1B
  # 3=2A
  # 4=2B
  # 5=3
  # 7=Referral
  # there is some logic below which depends on this ordering!
  
  summarise(d,
         n_total = n(),
         n_matched_call = sum(!is.na(CallId)),
         n_inorg = sum(user_org == call_org),
         n_prio_1 = sum(Priority == 1,na.rm = T),
         n_has_close = sum(has_close),
         n_p1a = sum(pout == 1),
         n_p1b = sum(pout == 2),
         n_p1 = sum(pout <= 2),
         n_p2 = sum(pout %in% c(3,4)),
         n_hanv = sum(pout == 7),
         n_cn = sum(care_need),
         n_cn_dba = sum(care_need & main_complete),
         n_cn_referral = sum(care_need & pout == 7),
         n_cn_p1 = sum(care_need & pout <= 2),
         n_pc = sum(patient_care),
         n_pc_referral = sum(patient_care & pout == 7),
         n_pc_p1 = sum(patient_care & pout <= 2),
         n_pc_pnr = sum(!is.na(pnr_lookup) & patient_care),
         n_pc_dba = sum(ifelse(patient_care,main_complete,NA),na.rm = T),
         n_pc_cat = sum(patient_care & has_cat),
         n_pc_complete = sum(patient_care & mbs_complete),
         n_pc_ref_delay = sum(patient_care & 
                                referral_delay > referral_delay_cutoff & 
                                disp_ready_time > referral_delay_cutoff & 
                                pout <= 4, na.rm = T),
         n_pc_ref_delay_txp = sum(patient_care & 
                                    referral_delay > referral_delay_cutoff & 
                                    disp_ready_time > referral_delay_cutoff & 
                                    pout <= 4 & 
                                    !is.na(any_prio1in), na.rm = T),
         n_pc_referral = sum(patient_care & pout == 7) + n_pc_ref_delay,
         n_pc_referral_72txp = sum(patient_care & pout == 7 & recontact_3day_txp == 1, na.rm = T),
         n_pc_ref_txp_tot = sum((patient_care & pout == 7 & recontact_3day_txp == 1) | 
                                  (patient_care & 
                                     referral_delay > referral_delay_cutoff & 
                                     disp_ready_time > referral_delay_cutoff & 
                                     pout <= 4 & 
                                     !is.na(any_prio1in)),na.rm = T),
         n_pc_manual_prio = sum(patient_care & 
                                  pout != recpout,
                                na.rm = T),
         n_pc_mbs_up = sum(patient_care & pout < recpout, na.rm = T),
         n_pc_mbs_down = sum(patient_care & pout > recpout, na.rm = T),
         mean_queue_time = mean(queue_time,na.rm = T),
         mean_call_time = mean(call_time,na.rm = T),
         median_queue_time = median(queue_time,na.rm = T),
         median_inorg_queue_time = median(ifelse(call_org == user_org,queue_time,NA),na.rm = T),
         median_call_time = median(call_time,na.rm = T),
         median_ready_time = median(disp_ready_time,na.rm = T),
         median_p1_ready_time = median(ifelse(pout <= 2,disp_ready_time,NA),na.rm = T),
         median_p1a_ready_time = median(ifelse(pout == 1,disp_ready_time,NA),na.rm = T),
         median_p1a_ready_time = median(ifelse(pout == 1,disp_ready_time,NA),na.rm = T),
         median_p1b_ready_time = median(ifelse(pout == 2,disp_ready_time,NA),na.rm = T),
         pct_cn_referral = n_cn_referral/n_cn*100,
         pct_cn_p1 = n_cn_p1/n_cn*100,
         pct_pc_pnr = n_pc_pnr/n_pc*100,
         pct_pc_cat = n_pc_cat/n_pc*100,
         pct_pc_dba = n_pc_dba/n_pc*100,
         pct_pc_complete = n_pc_complete/n_pc*100,
         pct_pc_manual_prio = n_pc_manual_prio/n_pc*100,
         pct_pc_mbs_up = n_pc_mbs_up/n_pc*100,
         pct_pc_mbs_down = n_pc_mbs_down/n_pc*100,
         pct_p2out_p1in = mean(any_prio1in[pout %in% c(3,4)],na.rm = T)*100,
         pct_p1out_vpp = mean(is.na(any_prio1in)[pout %in% c(1,2) & patient_care & 
                                                   !(grepl("Hjärtstopp|Allergisk|Blodsocker lågt",cats) | 
                                                       (grepl("Andningsbesvär",cats) & age <= 5))],
                              na.rm=T)*100,
         pct_pc_referral_72txp = n_pc_ref_txp_tot/n_pc_referral*100
  )
}

# generate user-level report data
r_user <- report_data %>%
  filter(month <= report_date) %>%
  group_by(month,user_org,name) %>%
   generate_measures() %>%
  ungroup()

# generate org-level report data
r_org <- report_data %>%
  filter(month <= report_date) %>%
  group_by(month,user_org) %>%
  generate_measures() %>%
  ungroup()

# r_ind <- report_data %>%
#   filter(month <= report_date) %>%
#   rowwise() %>%
#   generate_measures() %>%
#   ungroup() %>%
#   mutate(across(everything(),function(x)ifelse(is.nan(x),NA,x)))

# Generate reports -----------

folder = paste0(year(report_date),sprintf('%02d',month(report_date)))

outnames <- r_user$name[r_user$month == report_date & r_user$n_cn >= 50]

#yearly report
# outnames <- r_user$name[r_user$n_cn >= 200]
# folder = paste0("svlc_rapportering/",year(report_date))

# Add full year evals if desired
#outnames_yr <- r_user %>%
#  group_by(name) %>%
#  summarise(n = sum(n_cn)) %>%
#  filter(n>500) %>%
#  pull(name)

# outnames <- unique(c(outnames,outnames_yr))

display_data <- report_data %>%
  filter(care_need) %>%
  transmute(name,month,
            "Case number" = caseid,
            "Chosen priority" = plyr::revalue(as.character(pout),prios),
            "CDSS priority" = plyr::revalue(as.character(recpout),prios),
            "Call type" = ifelse(is.na(cats),"Sökorsak saknas",cats),
            "Life threats complete" = ifelse(patient_care,ifelse(main_complete == 1,"Yes","No"),NA),
            "CDSS complete" = ifelse(patient_care,ifelse(mbs_complete == 1,"Yes","No"),NA),
            "Ambulane priority in" = ifelse(is.na(any_prio1in),NA,ifelse(any_prio1in == 1,"Prio 1","Prio 2")),
            "Transport within 3 days" = ifelse(care_need,
                                             ifelse(referral_delay > referral_delay_cutoff & 
                                                    disp_ready_time > referral_delay_cutoff & 
                                                    pout <= 4 &
                                                    !is.na(any_prio1in),
                                                    "Transport after referral",
                                                    ifelse(is.na(recontact_3day_txp),
                                                           "Missing ID",
                                                           ifelse(recontact_3day_txp == 1,"Yes","No"))),
                                             NA),
            "Queue time" = sec_to_time(round(queue_time)),
            "Call time" = sec_to_time(round(call_time)),
            "Time to dispatch ready" = sec_to_time(round(disp_ready_time)))




dir.create(folder, showWarnings = FALSE, recursive = TRUE)

for(i in unique(r_user$user_org)) {
  dir.create(paste0(folder,"/",i), showWarnings = FALSE, recursive = TRUE)
}

for(i in outnames){
  
  org <- unique(r_user$user_org[which(r_user$name == i)])
  
  r_ind <- display_data %>%
    filter(name == i,
           month == report_date,
           `Chosen priority` %in% prios) %>%
    select(-name,-month)
  
  fn = paste0(getwd(),"/",folder,"/",org,"/",year(report_date),sprintf('%02d',month(report_date)),"_",as_filename(i))
  
  render("report.Rmd",
         output_file = fn,
         params = list(name = i, month = report_date, data = r_user,data_raw = r_ind),
         clean = T)
}

# render("svlc_rapportering/rapport_verksamhet.Rmd",
#        output_file = paste0(getwd(),"/",year(report_date),sprintf('%02d',month(report_date)),"/",year(report_date),sprintf('%02d',month(report_date)),"_verksamheter"),
#        params = list(month = report_date, data = r_user),
#        clean = T)

# Generate emails ---------------

user_info_fn <- "user_info.csv"

if(file.exists(user_info_fn)){
  
  user_info <- read_csv(user_info_fn)
  
}else{
  
  print("no user info found, making some up")
  
  user_info <- data.frame("name" = unique(report_data$name),
                          "email" = paste0(unique(report_data$name),"@fakedomain.com")) %>%
    left_join(distinct(select(report_data,name,user_org)))
  
}  

emailnames <-  r_user$name[r_user$month == report_date & r_user$n_cn >= 50]

emails <- filter(user_info, name %in% emailnames)

# Open Outlook
Outlook <- COMCreate("Outlook.Application")
subject <- paste0("SvLC feedback report for ",month(report_date,label = T,abbr = F))
body <- paste0("
Hello!<p>

Find attached your personal feedback report. You have recieved this mail because you handled 50 or more patient calls during the month of",report_month,". This is an automatically generated mail, but if you have questions or comments about the report you cn reply to this mail. If you find errors in the data, please provide the case ID number found in the table in the end of the report.<p>

Cheers,<br>
Douglas
")

for(i in 1:nrow(user_info)){
  # Create a new message
  Email = Outlook$CreateItem(0)

  # Set the recipient, subject, and body
  Email[["to"]] = emails$email[i]
  Email[["cc"]] = ""
  Email[["bcc"]] = ""
  Email[["subject"]] = subject
  #Email[["bodyformat"]] <- 2
  Email[["htmlbody"]] = body
  Email[["attachments"]]$Add(paste0(getwd(),"/",folder,"/",emails$user_org[i],"/",year(report_date),sprintf('%02d',month(report_date)),"_",as_filename(emails$name[i]),".html"))


  # This will generate mails in outlook, but our permission settings don't allow automatically sending messages via the RDCom API, so have to press send manually.
  Email$Display()
  
}
