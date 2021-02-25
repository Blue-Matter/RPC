


Help_UI <- function(id, label="Help1") {

  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("Intro")),
    checkboxInput(ns("test"),"test")
  )

}

Help_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Help and info"
      })

    }
  )
}

