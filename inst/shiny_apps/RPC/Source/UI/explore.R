


Explore_UI <- function(id, label="Explore") {

  ns <- NS(id)
  tagList(

  )

}

Explore_Server <- function(id) {
  moduleServer(id,
    function(input, output, session) {

      output$Intro <- renderText({
        "Explore operating models here"
      })

    }
  )
}

