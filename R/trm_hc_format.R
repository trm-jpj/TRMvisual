#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
trm_hc_format <- function(input, titel, download_fil_navn="download", note = NULL){
  highcharter::hc_chart(input, spacingTop = 40,
                        backgroundColor = "#FFFFFF",
                        plotBackgroundColor = "#FFFFFF",
                        style = list(fontFamily= 'Georgia')) |> 
    highcharter::hc_title(text =titel,
                          margin = 20,
                          align = "left",
                          x = 25,               
                          y = -10,
                          style = list(useHTML = TRUE)) |>
    highcharter::hc_xAxis(lineWidth = 0.5,
                          lineColor = "#000000",
                          gridLineWidth = 0,
                          tickWidth = 0) |>
    highcharter::hc_yAxis(gridLineWidth = 0,
                          lineWidth = 0,
                          tickWidth = 0) |>
    highcharter::hc_exporting(
      enabled = TRUE, # always enabled
      filename = paste0(download_fil_navn)) |> 
    highcharter::hc_subtitle(point = list(x = 0.5, y = 0.0, xAxis = 0, yAxis = 0), # Position relative to chart
                             text = note,
                             align = "bottom",
                             verticalAlign = "bottom",  # Place at the bottom of the chart area
                             y = 10,  # Adjust this value to fine-tune the position below the legend
                             style = list(fontSize = "12px", fontStyle = "italic")
    )
}