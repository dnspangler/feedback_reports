# Feedback reports

Scripts enabling the replication of the (forthcoming) study "Continuous Individual Feedback to Nurses at Emergency Medical Dispatch Centers: A Stepped-wedge, Interrupted Time Series Analysis".

Expects a data file named `report_data.csv` in the base directory. If none is found, a file containing junk values will be genereated to demonstrate the functionality of the script.

Sourcing `generate_reports.r` will parse `report_data.csv` and generate a folder for the report month, a subfolder for each region, and a report for each user. It will then generate an email in outlook associating each user with an email adress defined in `user_info.csv`, which will also be faked if no file is found.
