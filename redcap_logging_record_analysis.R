library(googleVis)

logging_file = "DATA/TIPTOPHHSBaselineNigeria_Logging_2018-11-15_1226.csv"
data_manager = "aokoro"

logs = read.csv(logging_file)

logs_from_field = subset(logs, Username == data_manager)
logs_from_field_tablets = logs_from_field[grep("(API)", logs_from_field$Action), ]

creation_logs = logs_from_field_tablets[grep("Created Record", logs_from_field_tablets$Action), ]
creation_logs$Date = substr(creation_logs$Time...Date, 0, 10)

records_transferred_by_day = as.data.frame(table(creation_logs$Date))
colnames(records_transferred_by_day) = col.names = c("date", "records")
records_transferred_by_day$date = as.Date(records_transferred_by_day$date)

plot(
  gvisCalendar(records_transferred_by_day, 
               datevar = "date", 
               numvar = "records",
               options = list(
                 title    = "Records transferred from field",
                 calendar = "{cellSize: 25}",
                 noDataPattern = "{backgroundColor: '#ffffff'}",
                 width    = 1500,
                 height   = 320
               )
  )
)
