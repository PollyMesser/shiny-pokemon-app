library(pacman)
p_load(shiny, shinyjs, dplyr, readr, tidyr, bslib, purrr, DT)


load("./pokemon_final.RData")

# UI
ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      /* Custom CSS for nav-pills */
      .nav-pills > li > a {
        background-color: #6baed6;
        color: white;
      }
      
      .nav-pills {
        margin-bottom: 15px;
      }
      
      .nav-pills > li.active > a {
        background-color: #1c73b0;
        color: white;
      }

      .nav-pills > li.active > a:hover {
        background-color: #0f4a73;
      }

      .nav-pills > li > a:hover {
        background-color: #559bb7;
      }

      /* Custom CSS for table row hover effect */
      table.dataTable tbody tr:hover {
        background-color: #f0e68c; /* Farbe beim Hover */
        color: black;
      }
      
      .table tbody tr.odd {
        background-color: #acdfd8; !important /* Farbe beim Hover */
        color: black;
      }
      
      .compact-text {
        margin-bottom: 5px; !important /* Verringerter Abstand zwischen den Texten */
        line-height: 1.2; !important /* Engerer Zeilenabstand */
      }
    "))
  ),
  
  # Card container for the app
  card(

    full_screen = TRUE,
    # Sidebar layout with select input and action buttons
    layout_sidebar(
      sidebar = sidebar(
        # Pokemon Type selection (dynamic choices populated in server)
        selectInput("poke_type", "Choose a Pokemon Type:", choices = NULL),
        
        # Start and Stop buttons
        actionButton("stop_button", "Stop"),
        actionButton("start_button", "Start"),
        
        # Dynamic UI for total and percentage texts
        # Textausgabe
        uiOutput("total_text", class = "compact-text"),
        uiOutput("percentage_text", class = "compact-text")
      ),
      card_header = "Pokemon Info Dropper",
      # Main panel with two navigation tabs: Drops and Table
      card_body(
          navset_card_pill(
            # Drops tab: Display sprite box with specified styles
            nav_panel("Drops",
                      tags$div(id = "sprite_box", 
                               style = "width: 400px; height: 400px; border: 10px dashed steelblue; 
                              border-bottom: 10px solid steelblue; 
                              background-color: lightblue; opacity: 0.9; 
                              position: relative; overflow: hidden;")
            ),
            
            # Table tab: DataTable output for displaying the Pokemon table
            nav_panel("Table",
                      tags$div(class="table",
                               dataTableOutput("pokemon_table"))
            )
          ) # End of navset_card
        )# End of card_body
        
    ), # End of layout_sidebar
    useShinyjs(),
  ) # End of card
) # End of fluidPage



# -------------------------------------------------


# Server
server <- function(input, output, session) {
  
  # Load and prepare Pokémon data
  pokemon_data <- pokemon_final %>%
    select(pokemon_name, type, sprite_url, rarity, charged_moves, fast_moves, max_cp) %>%
    unnest(type)  # Separate combined types
  
  # Update dropdown with types (initially on startup)
  updateSelectInput(session, "poke_type", 
                    choices = pokemon_data %>%
                      distinct(type))
  
  # Reactive value to control animation state
  animation_state <- reactiveVal("stopped")
  
  # Stop button event
  observeEvent(input$stop_button, {
    animation_state("stopped")
    shinyjs::runjs("stopAnimation = true; $('.sprite').stop(true, false);")  # Stop animations, keep current position
  })
  
  # Start button event
  observeEvent(input$start_button, {
    animation_state("running")
    shinyjs::runjs("
      stopAnimation = false;
      $('.sprite').each(function() {
        var $this = $(this);
        var currentTop = $this.position().top;
        if (currentTop < 350) {
          $this.animate({top: '350px'}, {
            duration: 2000 * (350 - currentTop) / 350,
            step: function(now) {
              if (stopAnimation) {
                $(this).stop();
              }
            },
            complete: function() {
              if (!stopAnimation) {
                $(this).animate({top: '-=20px'}, 200).animate({top: '+=20px'}, 200);
              }
            }
          });
        }
      });
    ")
  })
  
  # Pokemon type selection event
  observeEvent(input$poke_type, {
    selected_pokemon <- subset(pokemon_data, type == input$poke_type)
    
    # Shuffle the selected Pokemon to ensure random order
    selected_pokemon <- selected_pokemon[sample(nrow(selected_pokemon)), ]
    
    # Calculate the percentage of Pokémon of the selected type
    total_pokemon <- nrow(pokemon_data)
    selected_type_count <- nrow(selected_pokemon)
    percentage <- round((selected_type_count / total_pokemon) * 100, 2)
    
    # Update the text output using Bootstrap 5 classes
    # Update the text output with the percentage using renderUI without extra spacing
    output$total_text <- renderUI({
      HTML(sprintf(
        "There are <span style='font-size: 24px; font-weight: bold; color: orange;'>%s</span> Pokemon Type <span style='font-size: 24px; font-weight: bold; color: orange;'>%s</span>.",
        selected_type_count, input$poke_type))
    })
    
    # Update the text output with the percentage using renderUI without extra spacing
    output$percentage_text <- renderUI({
      HTML(sprintf(
        "<span style='font-size: 24px; font-weight: bold; color: orange;'>%s%%</span> of all Pokemon are Type <span style='font-size: 24px; font-weight: bold; color: orange;'>%s</span>.",
        percentage, input$poke_type))
    })
    
    # Prepare sprites for display
    pokemon_sprites <- selected_pokemon %>%
      mutate(
        # Wenn charged_moves eine Liste ist, werden die Elemente mit einem Komma getrennt
        charged_moves_str = map_chr(charged_moves, function(x) paste(x, collapse = ", ")),
        fast_moves_str = map_chr(fast_moves, function(x) paste(x, collapse = ", ")),
        sprite_url = paste0("<img src=\"",
                            sprite_url,
                            "\" height=\"50\" data-toggle=\"tooltip\" data-placement=\"right\" title=\"Charged Moves: ",
                            charged_moves_str,
                            "\"></img>")
      ) %>%
      rename(
        Name = pokemon_name,
        Sprite = sprite_url,
        Rarity = rarity,
        "Charged Moves" = charged_moves_str,
        "Fast Moves" = fast_moves_str,
        "Max CP" = max_cp
      ) %>%
      select(-type, -charged_moves, -fast_moves)
    
    # Bereich der Werte von 15 bis 10000 in Intervallen von 100
    brks <- seq(15, 10000, length.out = 100)
    
    # Farbpalette von weiß zu blau
    clrs <- colorRampPalette(c("white", "#E03FD8"))(length(brks) + 1)
    
    # Datatable mit formatStyle erstellen und in renderDataTable einbinden
    output$pokemon_table <- renderDataTable({
      datatable(pokemon_sprites,
                selection = "single",
                options = list(scrollX = TRUE,
                               order = list(list(5, 'desc'))),
                escape = FALSE) %>%
        formatStyle("Max CP",
                    backgroundColor = styleInterval(brks, clrs))  # Korrektur: backgroundColor statt background
    })
    
    # Clear sprite box
    shinyjs::runjs("$('#sprite_box').empty();")
    
    # Display each sprite only once
    for (i in seq_len(nrow(selected_pokemon))) {
      shinyjs::runjs(sprintf("
        setTimeout(function() {
          if (stopAnimation) return;
          var sprite = $('<img src=\"%s\" class=\"sprite\" style=\"position: absolute; top: -100px; left: %dpx;\">');
          $('#sprite_box').append(sprite);
          sprite.animate({
            top: '280px'
          }, {
            duration: 2000,
            step: function(now) {
              if (stopAnimation) {
                $(this).stop();
              }
            },
            complete: function() {
              if (!stopAnimation) {
                $(this).animate({top: '-=20px'}, 200).animate({top: '+=20px'}, 200);
              }
            }
          });
        }, %d);
      ", selected_pokemon$sprite_url[i], sample(-20:300, 1), (i-1) * 300))
    }
  })
}


shinyApp(ui, server)
