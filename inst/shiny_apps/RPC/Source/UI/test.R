

Test_UI <- function(id, label="Test1") {

  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("Intro")),
    checkboxInput(ns("test"),"test")
  )

}

Test_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Test stuff"
      })

    }
  )
}

