#' @title SankeyDiagram
#' 
#' @description
#' Class to handle the Sankey diagram of TreatmentPatterns.
#' 
#' @export
SankeyDiagram <- R6::R6Class(
  classname = "SankeyDiagram",
  inherit = InteracitvePlot,
  
  private = list(
    renderPlot = function() {
      shiny::tagList(
        shiny::htmlOutput(shiny::NS(private$.namespace, "sankey"))
      )
    },
    
    plot = function(input, output, inputHandler) {
      shiny::observeEvent(
        eventExpr = list(
          input$dbSelector,
          input[[private$.ageOption]],
          input[[private$.sexOption]],
          input[[private$.indexYearOption]],
          input[[private$.noneOption]]
        ),
        handlerExpr = {
          if (!is.null(inputHandler$reactiveValues$treatmentPathways)) {
            if (nrow(inputHandler$reactiveValues$treatmentPathways) > 0) {
              data <- private$formatData(
                inputHandler = inputHandler,
                input = input
              )
              sankeyList <- lapply(input$dbSelector, function(name) {
                try({
                  shiny::tagList(
                    shiny::h3(name),
                    TreatmentPatterns::createSankeyDiagram(
                      treatmentPathways = data$treatmentPathways %>%
                        dplyr::filter(.data$db == name)
                    )
                  )
                })
              })
              output$sankey <- shiny::renderUI(shiny::tagList(sankeyList))
            }
          }
        })
    }
  )
)
