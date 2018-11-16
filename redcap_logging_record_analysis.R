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
  api_mark = "(API)"
  created_record_en_mark = "Created Record"
  created_record_fr_mark = "Enregistrement créé"
  
  logs = read.csv(logging_file)
  
  colnames(logs) = c("datetime", "user", "action", "description")
  
  logs_from_field = subset(logs, user == data_manager)
  logs_from_field_tablets = logs_from_field[grep(api_mark, logs_from_field$action), ]
  
  if(lang == "FR")
    created_record_mark = created_record_fr_mark
  else
    created_record_mark = created_record_en_mark
  
  creation_logs = logs_from_field_tablets[grep(created_record_mark, 
                                               logs_from_field_tablets$action), ]
  creation_logs$date = substr(creation_logs$datetime, 0, 10)
  
  records_transferred_by_day = as.data.frame(table(creation_logs$date))
  
  colnames(records_transferred_by_day) = c("date", "records")
  records_transferred_by_day$date = as.Date(records_transferred_by_day$date)
  
  calendar = gvisCalendar(
    records_transferred_by_day, 
    datevar = "date", 
    numvar = "records",
    options = list(
      title    = paste0(title, " (", nrow(creation_logs), " records sent)"),
      titleTextStyle = "{color: 'red', fontSize: 12}",
      calendar = "{cellSize: 25}",
      # noDataPattern = "{backgroundCasxolor: '#ffffff'}",
      colorAxis = "{colors:['#ECF2FF','#17357A']}",
      width    = 1500,
      height   = 320
    )
  )
  
  return(calendar)
}

nig_calendar = records_transferred_by_day_calendar(nig_logging_file, "EN", nig_data_manager,
                                                   "NIG: Ohaukwu & Akure South")
mad_calendar = records_transferred_by_day_calendar(mad_logging_file, "FR", mad_data_manager,
                                                   "MAD: Mananjary & Toliary II")
drc_calendar = records_transferred_by_day_calendar(drc_logging_file, "FR", drc_data_manager,
                                                   "DRC: Kenge & Bulungu")
moz_calendar = records_transferred_by_day_calendar(moz_logging_file, "EN", moz_data_manager,
                                                   "MOZ: Nhamatanda & Meconta")

calendar = gvisMerge(gvisMerge(gvisMerge(nig_calendar, mad_calendar), drc_calendar), moz_calendar)
plot(calendar)
