#' @export
add_to_excel <- function(fil, sheet_navn, data_frame, figur=NULL, overwrite = FALSE){
  
  if(file.exists(fil)){
    wb <- xlsx::loadWorkbook(file = fil)
  } else {
    
    wb<-xlsx::createWorkbook()
  }
  
  # Define some cell styles
  #++++++++++++++++++++++++++++++++++++
  # Title and sub title styles
  
  TABLE_COLNAMES_STYLE <- xlsx::CellStyle(wb) + xlsx::Font(wb, isBold=TRUE) +
    xlsx::Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    xlsx::Border(color="black", position=c("TOP", "BOTTOM"), 
                 pen=c("BORDER_THIN", "BORDER_THICK")) 
  
  # Create a new sheet in the workbook
  #++++++++++++++++++++++++++++++++++++
  #+
  if(overwrite & any(names(xlsx::getSheets(wb))==sheet_navn)){
    xlsx::removeSheet(wb, sheet_navn)
    xlsx::saveWorkbook(wb, fil)
  }
  
  sheet <- xlsx::createSheet(wb, sheetName = sheet_navn)
  #++++++++++++++++++++++++
  # Helper function to add titles
  #++++++++++++++++++++++++
  # - sheet : sheet object to contain the title
  # - rowIndex : numeric value indicating the row to 
  #contain the title
  #++++++++++++++++++++++++++++++++++++
  
  xlsx::addDataFrame(data_frame, sheet, startRow=1, startColumn=1, 
                     colnamesStyle = TABLE_COLNAMES_STYLE)
  # Change column width
  # setColumnWidth(sheet, colIndex=c(1:ncol(data_frame)), colWidth=11)
  # Add a plot into a worksheet
  #++++++++++++++++++++++++++++++++++++
  # create a png plot
  if(!is.null(figur)){
    grDevices::png("plot_til_figur.png", height=101.6, width=130.4, units = "mm", res = 250, pointsize = 8 )
    plot(figur)
    grDevices::dev.off()
    
    
    if(nrow(data_frame)>50) {
      
      fig_start_col <- ncol(data_frame)+3
      fig_start_row <- 3
      
    } else {
      
      fig_start_col <- 3
      fig_start_row <- nrow(data_frame)+3
      
    }
    
    
    # Add the plot created previously
    xlsx::addPicture("plot_til_figur.png", sheet, scale = 1, startRow = fig_start_row,
                     startColumn = fig_start_col)
    # remove the plot from the disk
    res<-file.remove("plot_til_figur.png")
  }
  # Save the workbook to a file...
  #++++++++++++++++++++++++++++++++++++
  xlsx::saveWorkbook(wb, fil)
  
}
