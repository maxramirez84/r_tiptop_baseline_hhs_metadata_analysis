library(googleVis)

nig_logging_file = "DATA/TIPTOPHHSBaselineNigeria_Logging_2018-11-15_1226.csv"
mad_logging_file = "DATA/TIPTOPHHSBaselineMadagascarMAL_Logging_2018-11-15_1550.csv"
drc_logging_file = "DATA/TIPTOPHHSBaselineDRC_Logging_2018-11-16_0850.csv"
moz_logging_file = "DATA/TIPTOPHHSBaselineMozambique_Logging_2018-11-16_0913.csv"

nig_data_manager = "aokoro"
mad_data_manager = "rramananjato"
drc_data_manager = "dndombe"
moz_data_manager = "ejamisse"

records_transferred_by_day_calendar = function(logging_file, lang = "EN", data_manager, title) {
  #browser()
  api_mark = "(API)"
  created_record_en_mark = "Created Record"
  created_record_fr_mark = "Enregistrement créé"
  interview_date_mark = "interview_date"
  
  logs = read.csv(logging_file)
  
  colnames(logs) = c("log_datetime", "user", "action", "description")
  
  logs_from_field = subset(logs, user == data_manager)
  logs_from_field_tablets = logs_from_field[grep(api_mark, logs_from_field$action), ]
  
  if(lang == "FR")
    created_record_mark = created_record_fr_mark
  else
    created_record_mark = created_record_en_mark
  
  creation_logs = logs_from_field_tablets[grep(created_record_mark, 
                                               logs_from_field_tablets$action), ]
  
  # Remove logs of records without interview_date
  creation_logs = creation_logs[regexpr(interview_date_mark, creation_logs$description) >= 0, ]
  
  creation_logs$transfer_date = substr(creation_logs$log_datetime, 0, 10)
  creation_logs$record_date = substr(creation_logs$description, 
                                     regexpr(interview_date_mark, creation_logs$description) + 18, 
                                     regexpr(interview_date_mark, creation_logs$description) + 18 + 9)
  
  # Indicator: Average of time that a new record remains in the Tablet before being transferred, in
  # days and disaggregated by area.
  creation_logs$date_diff = as.Date(creation_logs$transfer_date) - as.Date(creation_logs$record_date)
  print(
    paste0(
      "The average of time that a record remains in the Tablet in distric 1 (", 
      title,
      ") is ", 
      mean(creation_logs$date_diff[regexpr("district = '1'", creation_logs$description) >= 0]),
      " days (",
      nrow(creation_logs[regexpr("district = '1'", creation_logs$description) >= 0, ]),
      " records)."
    )
  )
  print(
    paste0(
      "The average of time that a record remains in the Tablet in distric 2 (", 
      title,
      ") is ", 
      mean(creation_logs$date_diff[regexpr("district = '2'", creation_logs$description) >= 0]),
      " days (",
      nrow(creation_logs[regexpr("district = '2'", creation_logs$description) >= 0, ]),
      " records)."
    )
  )
  
  records_transferred_by_day = as.data.frame(table(creation_logs$transfer_date))
  
  colnames(records_transferred_by_day) = c("transfer_date", "records")
  records_transferred_by_day$transfer_date = as.Date(records_transferred_by_day$transfer_date)
  
  transfers_calendar = gvisCalendar(
    records_transferred_by_day, 
    datevar = "transfer_date", 
    numvar = "records",
    options = list(
      title    = paste0("Transferred ", nrow(creation_logs), " records from ", title),
      titleTextStyle = "{color: 'red', fontSize: 12}",
      calendar = "{cellSize: 25}",
      # noDataPattern = "{backgroundCasxolor: '#ffffff'}",
      colorAxis = "{colors:['#ECF2FF','#17357A']}",
      width    = 1500,
      height   = 320
    )
  )
  
  records_collected_by_day = as.data.frame(table(creation_logs$record_date))
  
  colnames(records_collected_by_day) = c("record_date", "records")
  records_collected_by_day$record_date = as.Date(records_collected_by_day$record_date)
  
  collected_calendar = gvisCalendar(
    records_collected_by_day, 
    datevar = "record_date", 
    numvar = "records",
    options = list(
      title    = paste0("Captured ", nrow(creation_logs), " records in ", title),
      titleTextStyle = "{color: 'red', fontSize: 12}",
      calendar = "{cellSize: 25}",
      # noDataPattern = "{backgroundCasxolor: '#ffffff'}",
      colorAxis = "{colors:['#E2AAAA','#8B2D2D']}",
      width    = 1500,
      height   = 320
    )
  )
  
  return(gvisMerge(collected_calendar, transfers_calendar))
}

nig_calendar = records_transferred_by_day_calendar(nig_logging_file, "EN", nig_data_manager, "NIG")
mad_calendar = records_transferred_by_day_calendar(mad_logging_file, "FR", mad_data_manager, "MAD")
drc_calendar = records_transferred_by_day_calendar(drc_logging_file, "FR", drc_data_manager, "DRC")
moz_calendar = records_transferred_by_day_calendar(moz_logging_file, "EN", moz_data_manager, "MOZ")

calendar = gvisMerge(gvisMerge(gvisMerge(nig_calendar, mad_calendar), drc_calendar), moz_calendar)
plot(calendar)
