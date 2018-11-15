library(googleVis)

nig_logging_file = "DATA/TIPTOPHHSBaselineNigeria_Logging_2018-11-15_1226.csv"
mad_logging_file = "DATA/TIPTOPHHSBaselineMadagascarMAL_Logging_2018-11-15_1550.csv"

nig_data_manager = "aokoro"
mad_data_manager = "rramananjato"

nig_logs = read.csv(nig_logging_file)
mad_logs = read.csv(mad_logging_file)

column_names = c("datetime", "user", "action", "description")
colnames(nig_logs) = column_names
colnames(mad_logs) = column_names

nig_logs_from_field = subset(nig_logs, user == nig_data_manager)
mad_logs_from_field = subset(mad_logs, user == mad_data_manager)

nig_logs_from_field_tablets = nig_logs_from_field[grep("(API)", nig_logs_from_field$action), ]
mad_logs_from_field_tablets = mad_logs_from_field[grep("(API)", mad_logs_from_field$action), ]

nig_creation_logs = nig_logs_from_field_tablets[grep("Created Record", 
                                                     nig_logs_from_field_tablets$action), ]
mad_creation_logs = mad_logs_from_field_tablets[grep("Enregistrement créé", 
                                                     mad_logs_from_field_tablets$action), ]

nig_creation_logs$date = substr(nig_creation_logs$datetime, 0, 10)
mad_creation_logs$date = substr(mad_creation_logs$datetime, 0, 10)

nig_records_transferred_by_day = as.data.frame(table(nig_creation_logs$date))
mad_records_transferred_by_day = as.data.frame(table(mad_creation_logs$date))

column_names = c("date", "records")
colnames(nig_records_transferred_by_day) = column_names
colnames(mad_records_transferred_by_day) = column_names

nig_records_transferred_by_day$date = as.Date(nig_records_transferred_by_day$date)
mad_records_transferred_by_day$date = as.Date(mad_records_transferred_by_day$date)

plot(
  gvisCalendar(nig_records_transferred_by_day, 
               datevar = "date", 
               numvar = "records",
               options = list(
                 title    = "Records transferred from Nigeria",
                 calendar = "{cellSize: 25}",
                 noDataPattern = "{backgroundColor: '#ffffff'}",
                 width    = 1500,
                 height   = 320
               )
  )
)

plot(
  gvisCalendar(mad_records_transferred_by_day, 
               datevar = "date", 
               numvar = "records",
               options = list(
                 title    = "Records transferred from Madagascar",
                 calendar = "{cellSize: 25}",
                 noDataPattern = "{backgroundColor: '#ffffff'}",
                 width    = 1500,
                 height   = 320
               )
  )
)
