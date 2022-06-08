# Requires bookdown install with
# Install with install.packages("bookdown") if not yet installed
library(bookdown)

# Book was initialized with create_bs4_book(path = "./bookdown/")
# For Bookdown (and possibly R Markdown to work) Pandoc is required.
# For me it was not found. Setting this environment variable fixed it.
Sys.setenv(RSTUDIO_PANDOC="C:/Users/Simon/AppData/Local/Pandoc")
serve_book("./bookdown/")

reload <- function() {
  servr::daemon_stop(1)
  serve_book("./bookdown/")
}


makePlot <- function(program, type, heading = "", js = T) {
  reformname <- program
  caption <- heading
  wtp_or_cost <- switch(type, "wtp" = "willingness_to_pay", "cost" = "government_net_costs")
  result <- sprintf(r"(<div class="barChartCaption">%s</div><div class="figure" style="text-align: center">
  <div id="%s-chart-%s"></div>
  <script>
    DataLoadedIn.then(() => {
      drawBarChart("%s", "%s", "%s-chart-%s", "%s-assumptions");
    })
  </script>
  </div>)", caption, reformname, wtp_or_cost, wtp_or_cost, reformname, reformname, wtp_or_cost, reformname)
  return(htmltools::HTML(result))
}
