library(shiny)
library(DT)
library(dplyr)
library(googlesheets4)
library(uuid)
library(purrr)
library(tibble)

### -- Data logic ----

ss <-"1SYo6kXx2y4RzXaEuOfD70ZnrLbnWDFMV4zjXsKVPMdM"

load_recipes_only <- function(){
  read_sheet(ss, sheet = "recipes")
}

load_ingredients_only <- function(){
  read_sheet(ss, sheet = "ingredients")
}

load_recipes <- function(recOnly = NULL,
                         ingOnly = NULL) {
  if(is.null(recOnly))
    recipes <- load_recipes_only()
  else
    recipies <- recOnly
  if(is.null(ingOnly))
    ingredients <- load_ingredients_only()
  else
    ingredients <- ingOnly

  # Join back into nested list for rendering
  split_recipes <- lapply(seq_len(nrow(recipes)), function(i) {
    rec <- recipes[i, ]
    rec_ingredients <- ingredients[ingredients$recipe_id == rec$id, , drop = FALSE]
    list(
      id = rec$id,
      title = rec$title,
      instructions = rec$instructions,
      source = rec$source,
      ingredients = rec_ingredients[, c("item", "quantity", "unit", "store"), drop = FALSE]
    )
  })

  return(split_recipes)
}

save_ingredients <- function(df) {
  sheet_write(df, ss = ss, sheet = "ingredients")
}

save_recipes_only <- function(df) {
  sheet_write(df, ss = ss, sheet = "recipes")
}

save_ingredients_only <- function(df) {
  sheet_write(df, ss = ss, sheet = "ingredients")
}

flatten_recipes <- function(recipe_list){
  # 1. Flatten top-level recipe metadata into a dataframe
  recipe_df <- purrr::map_dfr(recipe_list, ~{
    tibble::tibble(
      id = .x$id,
      title = .x$title,
      instructions = .x$instructions,
      source = .x$source
    )
  })

  # 2. Flatten all ingredients and include parent recipe_id
  ingredients_df <- purrr::map_dfr(recipe_list, ~{
    if (is.null(.x$ingredients)) return(NULL)
    dplyr::mutate(.x$ingredients, recipe_id = .x$id)
  })

  return(list(recipe_df,ingredients_df))
}

save_recipes <- function(df) {
  if(inherits(df,"list")){

  }else{
    stop("Malformed data supplied to save_recipes().")
  }

  sheet_write(df, ss = ss, sheet = "recipes")
}

expected_shopping_items <- function() {
  tibble(
    item_id = character(),
    shopping_list_id = character(),
    recipe_id = character(),
    item = character(),
    unit = character(),
    store = character(),
    quantity = numeric(),
    have = logical(),
    timestamp = as.POSIXct(character()),
    recipe_title = character(),
    multiplier = integer()
  )
}

sanitize_id <- function(x) {
  gsub("[^a-zA-Z0-9]", "_", x)
}

# ---- Save shopping lists to Google Sheets ----
save_shopping_items <- function(shopping_lists) {
  sheet_id <- ss

  if (is.null(shopping_lists) || nrow(shopping_lists) == 0) {
    # Save empty skeleton to clear sheet
    empty_df <- expected_shopping_items()
    sheet_write(
      data = empty_df,
      ss = as_sheets_id(sheet_id),
      sheet = "shopping_items"
    )
    message("Cleared shopping_items sheet (empty save).")
    return(invisible(NULL))
  }

  # Otherwise save the real data
  sheet_write(
    data = shopping_lists,
    ss = as_sheets_id(sheet_id),
    sheet = "shopping_items"
  )
  message("Saved shopping_items sheet with", nrow(shopping_lists), "rows.")
}



# save_recipes <- function(rec_list) {
#   if (length(rec_list) == 0) {
#     saveRDS(data.frame(), "recipes_flat.rds")
#     saveRDS(data.frame(), "ingredients_flat.rds")
#     return()
#   }
#
#   recipes_df <- do.call(rbind, lapply(rec_list, function(r) {
#     data.frame(
#       id = r$id,
#       title = r$title,
#       instructions = r$instructions,
#       source = r$source %||% NA_character_,
#       stringsAsFactors = FALSE
#     )
#   }))
#
#   ingredients_df <- do.call(rbind, lapply(rec_list, function(r) {
#     if (is.null(r$ingredients) || nrow(r$ingredients) == 0) return(NULL)
#     cbind(recipe_id = r$id, r$ingredients)
#   }))
#
#   saveRDS(recipes_df, "recipes_flat.rds")
#   saveRDS(ingredients_df, "ingredients_flat.rds")
# }

load_shopping_items <- function() {
  sheet_data <- tryCatch(
    read_sheet(ss, sheet = "shopping_items"),
    error = function(e) NULL
  )

  if (is.null(sheet_data) || nrow(sheet_data) == 0) {
    return(expected_shopping_items())
  }

  # Ensure correct column types and order, fallback if missing columns
  sheet_df <- as_tibble(sheet_data)
  expected_df <- expected_shopping_items()

  missing_cols <- setdiff(names(expected_df), names(sheet_df))
  if (length(missing_cols) > 0) {
    # Add missing columns with appropriate empty vectors
    for (col in missing_cols) {
      sheet_df[[col]] <- expected_df[[col]][0]
    }
  }

  # Reorder columns to expected schema
  sheet_df <- sheet_df[, names(expected_df)]

  return(sheet_df)
}



# ---- Flatten shopping lists into a flat tibble for GS writing ----
flatten_shopping_lists <- function(shopping_lists) {
  if (length(shopping_lists) == 0) {
    return(tibble(
      shopping_list_id = character(),
      recipe_id = character(),
      item = character(),
      unit = character(),
      store = character(),
      quantity = numeric(),
      have = logical()
    ))
  }

  flat_items <- lapply(shopping_lists, function(sl) {
    shopping_list_id <- sl$id
    recipes <- sl$recipes
    items <- sl$items

    # Defensive: handle empty recipes or items
    if (length(recipes) == 0 || nrow(items) == 0) {
      return(tibble())
    }

    # For each recipe, replicate items and add recipe_id, shopping_list_id
    df_list <- lapply(recipes, function(r) {
      recipe_id <- r$id
      items %>%
        mutate(
          shopping_list_id = shopping_list_id,
          recipe_id = recipe_id,
          have = as.logical(have)
        ) %>%
        select(shopping_list_id, recipe_id, item, unit, store, quantity, have)
    })

    bind_rows(df_list)
  })

  bind_rows(flat_items)
}



`%||%` <- function(a, b) if (!is.null(a)) a else b

### -- UI ----
ui <- fluidPage(
  titlePanel("Recipe Manager"),
  tabsetPanel(id = "main_tabs",  # <-- Add ID so we can switch tabs
              tabPanel("Recipe Book",
                       sidebarLayout(
                         sidebarPanel(
                           width = 7,
                           textInput("title", "Recipe Title"),
                           textAreaInput("instructions", "Instructions", rows = 5),
                           textInput("source", "Source (website, book, etc.)", value = ""),
                           h4("Ingredients"),
                           uiOutput("ingredient_ui"),
                           actionButton("add_ing", "Add Ingredient"),
                           hr(),
                           textOutput("save_status"),
                           actionButton("save_recipe", "Save Recipe"),
                           conditionalPanel(
                             condition = "output.is_editing_rec === true",
                             actionButton("cancel_edit", "Cancel Edit", icon = icon("times"), style = "margin-top: 10px;")
                           )
                         ),
                         mainPanel(
                           width = 5,
                           h3("Saved Recipes"),
                           DTOutput("recipe_table"),
                           uiOutput("recipe_viewer")
                         )
                       )
              ),
              tabPanel("Shopping List",
                       h3("Select Recipes"),
                       DTOutput("shopping_table"),
                       uiOutput("multiplier_inputs"),  # New: multipliers per recipe
                       h3("Generated Shopping List"),
                       uiOutput("shopping_list")       # Replace tableOutput with UI for checkboxes + header
              ),
              tabPanel("View Recipe",
                       uiOutput("recipe_viewer"),
                       value = "View Recipe"

              ),
              tabPanel("Saved Lists",
                       h3("Previously Saved Shopping Lists"),
                       DTOutput("saved_shopping_lists_table"),
                       uiOutput("saved_list_details")
              ),
              tabPanel("Ingredients",
                       h3("Canonical Ingredients"),
                       DTOutput("canonical_table"),
                       uiOutput("ingredient_edit_ui")
              )
  )

)

# -- Server ----
server <- function(input, output, session) {

  gs4_auth(path = "gs.json")

  ## ----- Reactive Vals ----------
  recipes_rv <- reactiveVal(load_recipes())
  ingredients <- reactiveVal(
    data.frame(item = "", quantity = NA_real_, unit = "cup", store = "Any",stringsAsFactors = FALSE)
  )
  editing_recipe_id <- reactiveVal(NULL)
  saved_shopping_lists_rv <- reactiveVal(list())
  recipe_to_delete <- reactiveVal(NULL)
  have_items <- reactiveVal(list())
  editing_shopping_list_id <- reactiveVal(NULL)
  selected_indices_rv <- reactiveVal(NULL)
  pending_canonical_decision <- reactiveVal(NULL)


  output$is_editing_shop <- reactive({
    !is.null(editing_shopping_list_id()) && nzchar(editing_shopping_list_id())
  })
  outputOptions(output, "is_editing_shop", suspendWhenHidden = FALSE)

  output$is_editing_rec <- reactive({
    !is.null(editing_recipe_id()) && nzchar(editing_recipe_id())
  })
  outputOptions(output, "is_editing_rec", suspendWhenHidden = FALSE)

  proxy <- dataTableProxy("shopping_table")

  ingredient_file <- "canonical_ingredients.rds"

  canonical_ingredients <- if (file.exists(ingredient_file)) {
    readRDS(ingredient_file)
  } else {
    character()
  }
  canonical_ingredients_rv <- reactiveVal(canonical_ingredients)

  # ------- Functions --------
  canonicalReplacementModal <- function(new_item, row_index) {
    modalDialog(
      title = paste("New Ingredient Detected:", shQuote(new_item)),
      p("This item isn’t in the canonical list."),
      radioButtons("canonical_decision", "What would you like to do?",
                   choices = setNames(
                     c("add", "replace"),
                     c(
                       paste0("Add '", new_item, "' to canonical list"),
                       "Replace with existing canonical ingredient"
                     )
                   )
      ),
      conditionalPanel(
        condition = "input.canonical_decision === 'replace'",
        selectInput("canonical_choice", "Choose Canonical Match:",
                    choices = isolate(canonical_ingredients_rv()))
      ),
      footer = tagList(
        actionButton("confirm_canonical_action", "Confirm"),
        modalButton("Cancel")
      ),
      easyClose = FALSE
    )
  }

  # Load saved shopping lists once at startup
  shopping_file <- load_shopping_items()
  if (!is.null(shopping_file)) {
    saved_shopping_lists_rv(shopping_file)
  } else {
    saved_shopping_lists_rv(list())
  }


  ## ------ Render UI: Add ingredients ---------
  output$ingredient_ui <- renderUI({
    df <- ingredients()
    n <- nrow(df)
    all_recipes <- load_recipes()
    ingredient_pool <- attr(df, "ingredient_pool")
    if (is.null(ingredient_pool)) {
      ingredient_pool <- unique(unlist(lapply(all_recipes, function(r) r$ingredients$item)))
    }
    ingredient_pool <- sort(na.omit(ingredient_pool))

    # Define known_terms here
    known_terms <- canonical_ingredients_rv()

    new_items <- trimws(tolower(df$item))
    unknowns <- setdiff(new_items, known_terms)
    unknowns <- unknowns[unknowns != ""]  # remove blanks


    lapply(1:n, function(i) {
      fluidRow(
        column(4, selectizeInput(
          inputId = paste0("item_", i),
          label = "Item",
          choices = c("", ingredient_pool),
          selected = df$item[i] %||% "",
          options = list(create = TRUE, maxOptions = 100)
        )),
        column(2, numericInput(paste0("qty_", i), "Qty", value = df$quantity[i])),
        column(2, selectInput(paste0("unit_", i), "Unit",
                              choices = c("tsp", "tbsp", "cup", "oz", "lb", "ml", "l", "g", "kg", "pcs", "package"),
                              selected = df$item[i] %||% ""
        )),
        column(3, textInput(paste0("store_", i), "Buy At:", value = df$store[i]))
      )
    })
  })


  ### ------ Observer: Add ingredients ---------
  observeEvent(input$add_ing, {
    df <- ingredients()
    n <- nrow(df)
    new_items <- character(n)

    for (i in 1:n) {
      new_items[i] <- input[[paste0("item_", i)]] %||% ""
      df$item[i] <- new_items[i]
      df$quantity[i] <- input[[paste0("qty_", i)]] %||% NA_real_
      df$unit[i] <- input[[paste0("unit_", i)]] %||% "cup"
      df$store[i] <- input[[paste0("store_", i)]] %||% "Any"
    }

    df <- rbind(df, data.frame(item = "", quantity = NA_real_, unit = "cup", store = "Any", stringsAsFactors = FALSE))

    # Append new values to ingredient pool so they persist in choices
    all_recipes <- load_recipes()
    prev_pool <- unique(unlist(lapply(all_recipes, function(r) r$ingredients$item)))
    new_pool <- unique(c(prev_pool, new_items))
    attr(df, "ingredient_pool") <- new_pool  # stash it temporarily

    ingredients(df)
  })


  ### ------ Observer: Save recipe  ---------
  observeEvent(input$save_recipe, {
    df <- ingredients()
    n <- nrow(df)
    if (input$title == "" || input$instructions == "") {
      output$save_status <- renderText("Title and Instructions are required.")
      return()
    }
    for (i in 1:n) {
      df$item[i] <- input[[paste0("item_", i)]]
      df$quantity[i] <- input[[paste0("qty_", i)]]
      df$unit[i] <- input[[paste0("unit_", i)]]
      df$store[i] <- input[[paste0("store_", i)]]
    }

    #df$item <- canonicalize(df$item, alias_table_rv())

    df <- df[trimws(df$item) != "", , drop = FALSE]
    if (nrow(df) == 0) {
      output$save_status <- renderText("At least one valid ingredient is required.")
      return()
    }

    # After collecting df$item
    new_items <- unique(df$item)
    existing_items <- canonical_ingredients_rv()
    all_items <- sort(unique(c(existing_items, new_items)))

    # Update if changed
    if (!identical(all_items, existing_items)) {
      canonical_ingredients_rv(all_items)
      saveRDS(all_items, ingredient_file)
    }

    all_recipes <- load_recipes()
    id_edit <- editing_recipe_id()

    if (is.null(id_edit)) {
      new_recipe <- list(
        id = UUIDgenerate(),
        title = input$title,
        instructions = input$instructions,
        source = input$source,
        ingredients = df
      )
      all_recipes <- c(all_recipes, list(new_recipe))
      output$save_status <- renderText("Recipe saved!")
    } else {
      idx <- which(sapply(all_recipes, `[[`, "id") == id_edit)
      if (length(idx) == 1) {
        all_recipes[[idx]]$title <- input$title
        all_recipes[[idx]]$instructions <- input$instructions
        all_recipes[[idx]]$source <- input$source
        all_recipes[[idx]]$ingredients <- df

        output$save_status <- renderText("Recipe updated!")
      }
      editing_recipe_id(NULL)
    }

    flat_recipes <- flatten_recipes(all_recipes)

    save_recipes_only(flat_recipes[[1]])
    save_ingredients_only(flat_recipes[[2]])
    #save_recipes(all_recipes)
    recipes_rv(all_recipes)
    ingredients(data.frame(item = "", quantity = NA_real_, unit = "cup", stringsAsFactors = FALSE))
    updateTextInput(session, "title", value = "")
    updateTextAreaInput(session, "instructions", value = "")
    updateTextInput(session, "source", value = "")
    editing_recipe_id(NULL)
  })


  ###----Observer: Direct Replacement
  observe({
    df <- ingredients()
    items <- tolower(trimws(df$item))
    known <- canonical_ingredients_rv()
    unknowns <- which(!(items %in% known) & items != "")

    if (length(unknowns) > 0 && is.null(pending_canonical_decision())) {
      pending_canonical_decision(list(
        row = unknowns[1],
        name = df$item[unknowns[1]]
      ))
      showModal(canonicalReplacementModal(df$item[unknowns[1]], unknowns[1]))
    }
  })

  observeEvent(input$confirm_canonical_action, {
    decision <- isolate(input$canonical_decision)
    row <- pending_canonical_decision()$row
    name <- tolower(trimws(pending_canonical_decision()$name))
    df <- ingredients()

    if (decision == "add") {
      canon <- canonical_ingredients_rv()
      canon <- sort(unique(c(canon, name)))
      canonical_ingredients_rv(canon)
      saveRDS(canon, ingredient_file)
    } else if (decision == "replace") {
      selected <- isolate(input$canonical_choice)
      df$item[row] <- selected
      ingredients(df)
    }

    pending_canonical_decision(NULL)
    removeModal()
    showNotification("Canonical decision saved.", type = "message")
  })

  ## --------- Render: View Canonical Ingredients-------
  output$canonical_table <- renderDT({
    data.frame(Ingredient = canonical_ingredients_rv(), stringsAsFactors = FALSE) %>%
      mutate(
        Edit = sprintf('<button class="edit_ingredient" id="edit_ing_%s">Edit</button>', Ingredient),
        Delete = sprintf('<button class="delete_ingredient" id="del_ing_%s">Delete</button>', Ingredient)
      ) %>%
      datatable(
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
          paging = TRUE,
          searching = TRUE,
          columnDefs = list(list(targets = c(1, 2), orderable = FALSE))
        ),
        callback = JS("
        table.on('click', 'button.edit_ingredient', function() {
          var id = $(this).attr('id');
          Shiny.setInputValue('edit_ingredient', id, {priority: 'event'});
        });
        table.on('click', 'button.delete_ingredient', function() {
          var id = $(this).attr('id');
          Shiny.setInputValue('delete_ingredient', id, {priority: 'event'});
        });
      ")
      )
  })

  ### --- Observer: Edit canonical ingredients  --------
  observeEvent(input$edit_ingredient, {
    ingredient <- sub("edit_ing_", "", input$edit_ingredient)
    showModal(modalDialog(
      title = paste("Edit Ingredient:", ingredient),
      textInput("new_ingredient_name", "New Name", value = ingredient),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_edit_ingredient", "Save")
      )
    ))
    selected_indices_rv(ingredient)
  })

  ### --- Observer: Confirm edit canonical ingredients  --------
  observeEvent(input$confirm_edit_ingredient, {
    old <- selected_indices_rv()
    new <- trimws(tolower(input$new_ingredient_name))
    canon <- canonical_ingredients_rv()

    if (new == "" || new %in% canon) {
      showNotification("Invalid or duplicate name.", type = "error")
      return()
    }

    canon <- setdiff(canon, old)
    canon <- sort(unique(c(canon, new)))
    canonical_ingredients_rv(canon)
    saveRDS(canon, ingredient_file)

    selected_indices_rv(NULL)
    removeModal()
    showNotification("Ingredient renamed.", type = "message")
  })

  ### ------ Observer: Delete canonical ingredients-----
  observeEvent(input$delete_ingredient, {
    ingredient <- sub("del_ing_", "", input$delete_ingredient)
    showModal(modalDialog(
      title = paste("Delete Ingredient:", ingredient),
      "Are you sure you want to remove this ingredient from the canonical list? It won't affect existing recipes.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_ingredient", "Delete", class = "btn-danger")
      )
    ))
    selected_indices_rv(ingredient)
  })

  ### ------ Observer: Confirm delete canonical ingredients ------
  observeEvent(input$confirm_delete_ingredient, {
    canon <- canonical_ingredients_rv()
    canon <- setdiff(canon, selected_indices_rv())
    canonical_ingredients_rv(canon)
    saveRDS(canon, ingredient_file)
    selected_indices_rv(NULL)
    removeModal()
    showNotification("Ingredient deleted.", type = "message")
  })



  ## ------ Render DT: Recipe table  ---------
  output$recipe_table <- renderDT({
    recs <- recipes_rv()
    if (length(recs) == 0) return(data.frame())
    df <- data.frame(
      Title = sapply(recs, `[[`, "title"),
      Ingredients = sapply(recs, function(r) paste(r$ingredients$item, collapse = ", ")),
      ID = sapply(recs, `[[`, "id"),
      stringsAsFactors = FALSE
    )
    df$View <- sprintf('<button class="view_btn" id="view_%s">View</button>', df$ID)
    df$Edit <- sprintf('<button class="edit_btn" id="edit_%s">Edit</button>', df$ID)
    df$Delete <- sprintf('<button class="delete_btn" id="del_%s">Delete</button>', df$ID)
    datatable(
      df[, c("Title", "Ingredients", "View","Edit", "Delete")],
      escape = FALSE,
      selection = "single",
      rownames = FALSE,
      options = list(
        paging = TRUE,
        searching = TRUE,
        columnDefs = list(list(targets = c(2, 3), orderable = FALSE))  # Edit/Delete columns
      ),
      callback = JS("
     table.on('click', 'button.view_btn', function() {
      var id = $(this).attr('id');
      Shiny.setInputValue('view_recipe', id, {priority: 'event'});
    });
    table.on('click', 'button.edit_btn', function() {
      var id = $(this).attr('id');
      Shiny.setInputValue('edit_recipe', id, {priority: 'event'});
    });
    table.on('click', 'button.delete_btn', function() {
      var id = $(this).attr('id');
      Shiny.setInputValue('delete_recipe', id, {priority: 'event'});
    });
  ")
    )

  })

  ### ------- Observer: View recipe ------
  observeEvent(input$view_recipe, {
    id <- sub("view_", "", input$view_recipe)
    recs <- recipes_rv()
    recipe <- Filter(function(r) r$id == id, recs)[[1]]
    if (!is.null(recipe)) {
      selected_recipe(recipe)
      updateTabsetPanel(session, "main_tabs", selected = "View Recipe")
    }
  })


  ### ------ Observer: Edit recipe ---------
  observeEvent(input$edit_recipe, {
    id <- sub("edit_", "", input$edit_recipe)
    recs <- load_recipes()
    recipe <- Filter(function(r) r$id == id, recs)[[1]]
    if (!is.null(recipe)) {
      updateTextInput(session, "title", value = recipe$title)
      updateTextAreaInput(session, "instructions", value = recipe$instructions)
      updateTextInput(session, "source", value = recipe$source %||% "")
      ingredients(recipe$ingredients)
      editing_recipe_id(id)
      output$save_status <- renderText("Editing recipe...")
      output$is_editing_rec <- reactive({
        !is.null(editing_recipe_id())
      })
      outputOptions(output, "is_editing_rec", suspendWhenHidden = FALSE)

    }
  })

  ### ------ Observer: Cancel edit recipe ---------
  observeEvent(input$cancel_edit, {
    editing_recipe_id(NULL)
    ingredients(data.frame(item = "", quantity = NA_real_, unit = "cup", stringsAsFactors = FALSE))
    updateTextInput(session, "title", value = "")
    updateTextAreaInput(session, "instructions", value = "")
    updateTextAreaInput(session, "source", value = "")
    output$save_status <- renderText("Edit cancelled.")
  })


  ### ------ Observer: Delete recipe ---------
  observeEvent(input$delete_recipe, {
    id <- sub("del_", "", input$delete_recipe)

    if (is.null(id) || id == "" || !id %in% sapply(recipes_rv(), `[[`, "id")) {
      showNotification("Invalid recipe selected for deletion.", type = "error")
      return()
    }

    recipe_to_delete(id)
    showModal(modalDialog(
      title = "Confirm Delete",
      "Are you sure you want to delete this recipe?",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class = "btn-danger")
      ),
      easyClose = TRUE,
      fade = TRUE
    ))
  })


  ### ------ Observer: Confirm delete ---------
  observeEvent(input$confirm_delete, {
    req(recipe_to_delete())

    recOnly <- load_recipes_only()
    ingOnly <- load_ingredients_only()

    recOnly <- recOnly[recOnly$id != recipe_to_delete(),]
    ingOnly <- ingOnly[ingOnly$recipe_id != recipe_to_delete(),]

    save_recipes_only(recOnly)
    save_ingredients_only(ingOnly)

    recs <- load_recipes(recOnly,ingOnly)
    recipes_rv(recs)
    output$save_status <- renderText("Recipe deleted.")
    recipe_to_delete(NULL)
    removeModal()

    ingredients(data.frame(item = "", quantity = NA_real_, unit = "cup", stringsAsFactors = FALSE))
    updateTextInput(session, "title", value = "")
    updateTextAreaInput(session, "instructions", value = "")
    editing_recipe_id(NULL)
  })

  selected_recipe <- reactiveVal(NULL)

  ### ------ Observer: Recipe table rows selected ---------
  observeEvent(input$confirm_delete, {
    req(recipe_to_delete())
    recs <- recipes_rv()
    if (length(recs) == 0) {
      showNotification("No recipes to delete.", type = "error")
      return()
    }

    recs <- Filter(function(r) r$id != recipe_to_delete(), recs)
    recipes_rv(recs)
    save_recipes(recs)

    output$save_status <- renderText("Recipe deleted.")
    recipe_to_delete(NULL)
    removeModal()

    # Reset inputs
    ingredients(data.frame(item = "", quantity = NA_real_, unit = "cup", stringsAsFactors = FALSE))
    updateTextInput(session, "title", value = "")
    updateTextAreaInput(session, "instructions", value = "")
    editing_recipe_id(NULL)
  })

  ## ------ Render UI: Recipe viewer ---------
  output$recipe_viewer <- renderUI({
    recipe <- selected_recipe()
    if (is.null(recipe)) return(NULL)
    tagList(
      h2(recipe$title),
      tags$div(
        style = "white-space: pre-wrap; word-wrap: break-word;",
        recipe$instructions
      ),
      if (!is.null(recipe$source) && nzchar(recipe$source)) {
        tagList(
          h4("Source"),
          p(recipe$source)
        )
      },
      h4("Ingredients"),
      tableOutput("ingredients_table"),
      actionButton("back_to_book", "Back to Recipe Book")
    )
  })

  ### ------- Observer: Back to recipe book --------
  observeEvent(input$back_to_book, {
    updateTabsetPanel(session, "main_tabs", selected = "Recipe Book")
  })


  ## ------ Render Table: Ingredients table ---------
  output$ingredients_table <- renderTable({
    recipe <- selected_recipe()
    if (is.null(recipe)) return(NULL)
    recipe$ingredients
  }, rownames = FALSE)

  # ---- Render DT: Shopping table ----
  output$shopping_table <- renderDT({
    recs <- recipes_rv()
    df <- data.frame(
      Title = sapply(recs, `[[`, "title"),
      stringsAsFactors = FALSE
    )
    datatable(df, selection = "multiple", rownames = FALSE)
  }, server = TRUE)  # <-- here


  ###----Observer: Editing Shopping List ID
  observeEvent(editing_shopping_list_id(), {
    req(editing_shopping_list_id())
    saved_shopping_lists <- saved_shopping_lists_rv()
    edshopid <- editing_shopping_list_id()

    sel <- saved_shopping_lists[saved_shopping_lists$shopping_list_id==edshopid,]
    if(length(sel) == 1) {
      saved_recipe_ids <- sapply(sel[[1]]$recipes, `[[`, "id")
      all_recs <- recipes_rv()
      selected_indices <- which(sapply(all_recs, function(r) r$id) %in% saved_recipe_ids)

      selectRows(proxy, selected_indices)
    }
  })


  # Reactive to store multipliers keyed by recipe ID
  recipe_multipliers <- reactiveVal(list())

  # UI: render numericInputs for selected recipes
  output$multiplier_inputs <- renderUI({
    req(input$shopping_table_rows_selected)
    recs <- recipes_rv()[input$shopping_table_rows_selected]

    lapply(recs, function(r) {
      numericInput(
        inputId = paste0("multiplier_", r$id),
        label = paste0("Multiplier for: ", r$title),
        value = recipe_multipliers()[[r$id]] %||% 1,
        min = 1, step = 1
      )
    })
  })

  ## ------ Render UI: Shopping list  ---------
  output$shopping_list <- renderUI({
    req(input$shopping_table_rows_selected)
    recs <- recipes_rv()[input$shopping_table_rows_selected]

    mults <- recipe_multipliers()

    all_ingredients <- do.call(rbind, lapply(recs, function(r) {
      multiplier <- mults[[r$id]] %||% 1
      df <- r$ingredients
      df$quantity <- df$quantity * multiplier
      df
    }))
    # Group and sum quantities
    all_ingredients <- all_ingredients %>%
      group_by(item, unit, store) %>%
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")

    state <- have_items()
    all_ingredients$have <- sapply(all_ingredients$item, function(x) isTRUE(state[[x]]))
    all_ingredients <- all_ingredients %>% arrange(have, item)

    rows <- lapply(1:nrow(all_ingredients), function(i) {
      row <- all_ingredients[i, ]
      checkbox_id <- paste0("have_", sanitize_id(row$item))

      fluidRow(
        style = if (row$have) "color: #aaa; text-decoration: line-through;" else "",
        column(1, checkboxInput(
          inputId = paste0("have_", sanitize_id(row$item)),
          value = row$have,
          label = NULL
        )),
        column(5, row$item),
        column(2, row$quantity),
        column(2, row$unit),
        column(2, row$store)
      )
    })

    # Add header
    tagList(
      fluidRow(
        column(1, strong("I have it")),
        column(5, strong("Ingredient")),
        column(2, strong("Quantity")),
        column(2, strong("Unit")),
        column(2, strong("Buy At:"))
      ),
      tags$hr(),
      rows,
      conditionalPanel(
        condition = "output.is_editing_shop === false",
        tagList(
          textOutput("complete_status"),
          actionButton("complete_list", "Save Shopping List"),
        )
      ),
      conditionalPanel(
        condition = "output.is_editing_shop === true",
        tagList(
          textOutput("complete_status"),
          actionButton("save_edited_list", "Save Edited Shopping List"),
          actionButton("cancel_edit_list", "Cancel Edit")
        )
      )

    )

  })

  ### ---- Observer: Shopping List Have Items ---------
  observe({
    req(input$shopping_table_rows_selected)
    recs <- recipes_rv()[input$shopping_table_rows_selected]
    all_ingredients <- do.call(rbind, lapply(recs, function(r) r$ingredients))
    all_ingredients <- all_ingredients %>%
      group_by(item, unit) %>%
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")

    current_state <- have_items()

    # Update state from inputs
    for (item in all_ingredients$item) {
      id <- paste0("have_", gsub("\\s+", "_", item))
      if (!is.null(input[[id]])) {
        current_state[[item]] <- input[[id]]
      }
    }

    have_items(current_state)
  })

  ###----- Observer: Multiplier in shopping list -------
  # Observe multiplier changes and store in reactiveVal
  observe({
    req(input$shopping_table_rows_selected)
    recs <- recipes_rv()[input$shopping_table_rows_selected]
    current <- recipe_multipliers()

    for (r in recs) {
      id <- paste0("multiplier_", r$id)
      val <- input[[id]]
      if (!is.null(val) && is.numeric(val) && val > 0) {
        current[[r$id]] <- val
      } else {
        current[[r$id]] <- 1
      }
    }

    recipe_multipliers(current)
  })

  ### ---- Observer: Save shopping list (complete list) ----
  observeEvent(input$complete_list, {
    req(input$shopping_table_rows_selected)

    recs <- recipes_rv()[input$shopping_table_rows_selected]
    mults <- recipe_multipliers()


    all_ingredients <- do.call(rbind, lapply(recs, function(r) {
      multiplier <- mults[[r$id]] %||% 1
      df <- r$ingredients
      df$quantity <- df$quantity * multiplier
      df$recipe_id <- r$id
      df$recipe_title <- r$title
      df$multiplier <- multiplier
      df
    }))

    # Group and sum quantities by item, unit, store, recipe (optional)
    all_ingredients <- all_ingredients %>%
      group_by(recipe_id, recipe_title, multiplier, item, unit, store) %>%
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")

    # Add shopping_list_id and timestamp for this batch
    shopping_list_id <- UUIDgenerate()
    timestamp <- Sys.time()

    # Assuming 'shopping_items' is your data frame containing the shopping list items
    have_statuses <- sapply(all_ingredients$item, function(id) {
      input[[paste0("have_", sanitize_id(id))]]
    })

    flat_shopping_list <- all_ingredients %>%
      mutate(
        shopping_list_id = shopping_list_id,
        timestamp = timestamp)

    flat_shopping_list$have <- have_statuses[flat_shopping_list$item]
    print(flat_shopping_list)
    flat_shopping_list <- flat_shopping_list %>%
      select(shopping_list_id, timestamp, recipe_id, recipe_title, multiplier, item, unit, store, quantity, have)

    # Append flat_shopping_list to your reactive or saved data
    current_lists <- saved_shopping_lists_rv()
    new_lists <- bind_rows(current_lists, flat_shopping_list)
    saved_shopping_lists_rv(new_lists)

    # Save directly to Google Sheets or RDS as one flat table
    save_shopping_items(new_lists)

    output$complete_status <- renderText(paste0("Shopping list saved at ", format(timestamp, "%Y-%m-%d %H:%M:%S")))
  })

  ###-----Observer: Save edited list ------
  observeEvent(input$save_edited_list, {
    req(editing_shopping_list_id())
    edshoplisid <- editing_shopping_list_id()
    saved_shopping_lists <- saved_shopping_lists_rv()

    # Remove old rows associated with this list
    saved_shopping_lists <- saved_shopping_lists %>%
      filter(shopping_list_id != edshoplisid)

    recs <- recipes_rv()[input$shopping_table_rows_selected]
    mults <- recipe_multipliers()

    # Generate new shopping list entries
    new_rows <- bind_rows(lapply(recs, function(r) {
      multiplier <- mults[[r$id]] %||% 1
      df <- r$ingredients
      df$quantity <- df$quantity * multiplier
      df$shopping_list_id <- edshoplisid
      df$recipe_id <- r$id
      df$recipe_title <- r$title
      df$multiplier <- multiplier
      df$have <- r$have
      df$timestamp <- Sys.time()
      df$item_id <- NA
      df
    }))

    # Combine updated lists
    updated_df <- bind_rows(saved_shopping_lists, new_rows)

    save_shopping_items(updated_df)
    saved_shopping_lists_rv(updated_df)

    showNotification("Shopping list updated successfully.", type = "message")
  })


  ## ------ Render DT: Saved Shopping LIsts -----------
  output$saved_shopping_lists_table <- renderDT({
    shopping_lists <- saved_shopping_lists_rv()

    if (nrow(shopping_lists) == 0) {
      df_empty <- data.frame(
        Time = character(0),
        Recipes = character(0),
        View = character(0),
        Edit = character(0),
        Delete = character(0),
        stringsAsFactors = FALSE
      )

      return(datatable(
        df_empty,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
          paging = TRUE,
          searching = FALSE,
          columnDefs = list(list(targets = c(2, 3, 4), orderable = FALSE))
        )
      ))
    }

    # Group by shopping_list_id to extract timestamp and recipes
    display_df <- shopping_lists %>%
      group_by(shopping_list_id, timestamp) %>%
      summarise(
        Recipes = paste(unique(recipe_title), collapse = ", "),
        .groups = "drop"
      ) %>%
      mutate(
        View = sprintf('<button class="view_saved_list" id="view_%s">View</button>', shopping_list_id),
        Edit = sprintf('<button class="edit_saved_list" id="edit_%s">Edit</button>', shopping_list_id),
        Delete = sprintf('<button class="delete_saved_list" id="delete_%s">Delete</button>', shopping_list_id),
        Time = format(timestamp, "%Y-%m-%d %H:%M")
      ) %>%
      select(Time, Recipes, View, Edit, Delete)

    datatable(
      display_df,
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        paging = TRUE,
        searching = FALSE,
        columnDefs = list(list(targets = c(2, 3, 4), orderable = FALSE))
      ),
      callback = JS("
      table.on('click', 'button.edit_saved_list', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('edit_saved_list', id, {priority: 'event'});
      });
      table.on('click', 'button.view_saved_list', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('view_saved_list', id, {priority: 'event'});
      });
      table.on('click', 'button.delete_saved_list', function() {
        var id = $(this).attr('id');
        Shiny.setInputValue('delete_saved_list', id, {priority: 'event'});
      });
    ")
    )
  })

  ### ------ Observer: View Saved Shopping Lists -----------
  observeEvent(input$view_saved_list, {
    id <- sub("view_", "", input$view_saved_list)
    saved_shopping_lists <- saved_shopping_lists_rv()

    # Extract rows matching the shopping_list_id
    list_rows <- saved_shopping_lists %>% filter(shopping_list_id == id)

    if (nrow(list_rows) > 0) {
      # Extract timestamp (assume all rows have the same one)
      timestamp <- list_rows$timestamp[1]

      # Reconstruct list of recipes with multipliers
      recipes <- list_rows %>%
        select(recipe_id, recipe_title, multiplier) %>%
        distinct() %>%
        mutate(
          title = recipe_title
        ) %>%
        select(id = recipe_id, title, multiplier) %>%
        split(seq_len(nrow(.)))  # turn into list of lists

      # Prepare items table
      items <- list_rows %>%
        select(item, unit, store, quantity, have)

      output$saved_list_details <- renderUI({
        tagList(
          h4("Saved List Details"),
          p(strong("Saved on: "), format(timestamp, "%Y-%m-%d %H:%M:%S")),
          h5("Recipes:"),
          tags$ul(lapply(recipes, function(r) tags$li(paste0(r$title, " (x", r$multiplier, ")")))),
          h5("Items:"),
          tableOutput("saved_items_table")
        )
      })

      output$saved_items_table <- renderTable({
        items
      }, rownames = FALSE)
    }
  })

  ### ------- Observer: Delete Saved Shopping List -----
  observeEvent(input$delete_saved_list, {
    id <- sub("delete_", "", input$delete_saved_list)
    cat("Deleting list id:", id, "\n")

    #saved_shopping_lists <- load_shopping_items()
    saved_shopping_lists <- saved_shopping_lists_rv()
  #  print(saved_shopping_lists)   # Check data loaded from Sheets

    updated_lists <- saved_shopping_lists %>% filter(shopping_list_id != id)
 #   print(updated_lists) # Check filtered result before saving

    save_shopping_items(updated_lists)

    saved_shopping_lists_rv(updated_lists)  # refresh reactive with updated sheet data

    # if (is.null(saved_shopping_lists) || nrow(saved_shopping_lists) == 0) {
    #   saved_shopping_lists <- tibble(
    #     item_id = character(),
    #     shopping_list_id = character(),
    #     recipe_id = character(),
    #     item = character(),
    #     unit = character(),
    #     store = character(),
    #     quantity = numeric(),
    #     have = logical(),
    #     timestamp = as.POSIXct(character()),
    #     recipe_title = character(),
    #     multiplier = integer()
    #   )
    # }

    # Clear any details showing
    output$saved_list_details <- renderUI(NULL)

    # Optional: show a message
    output$complete_status <- renderText("Saved shopping list deleted.")

    # Refresh DT
    if (nrow(updated_lists) == 0) {
      df <- data.frame(
        Time = character(0),
        Recipes = character(0),
        View = character(0),
        Delete = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      summary_df <- updated_lists %>%
        group_by(shopping_list_id, timestamp) %>%
        summarise(
          Recipes = paste(unique(recipe_title), collapse = ", "),
          .groups = "drop"
        ) %>%
        mutate(
          View = sprintf('<button class="view_saved_list" id="view_%s">View</button>', shopping_list_id),
          Delete = sprintf('<button class="delete_saved_list" id="delete_%s">Delete</button>', shopping_list_id)
        ) %>%
        rename(Time = timestamp)

      df <- summary_df[, c("Time", "Recipes", "View", "Delete")]
    }

    output$saved_shopping_lists_table <- renderDT({
      datatable(
        df,
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
          paging = TRUE,
          searching = FALSE,
          columnDefs = list(list(targets = c(2, 3), orderable = FALSE))
        ),
        callback = JS("
        table.on('click', 'button.view_saved_list', function() {
          var id = $(this).attr('id');
          Shiny.setInputValue('view_saved_list', id, {priority: 'event'});
        });
        table.on('click', 'button.delete_saved_list', function() {
          var id = $(this).attr('id');
          Shiny.setInputValue('delete_saved_list', id, {priority: 'event'});
        });
      ")
      )
    })
  })

  ###------Observer: Edit Saved Shopping List ---------
  # ReactiveVal to hold the indices you want selected
  selected_indices_rv <- reactiveVal(NULL)

  observeEvent(input$edit_saved_list, {
    list_id <- sub("^edit_", "", input$edit_saved_list)

    # Load flat tibble
    saved_shopping_lists <- saved_shopping_lists_rv()

    if (is.null(saved_shopping_lists)) {
      saved_shopping_lists <- tibble(
        item_id = character(),
        shopping_list_id = character(),
        recipe_id = character(),
        item = character(),
        unit = character(),
        store = character(),
        quantity = numeric(),
        have = logical(),
        timestamp = as.POSIXct(character()),
        recipe_title = character(),
        multiplier = integer()
      )
    }

    this_list <- saved_shopping_lists[saved_shopping_lists$shopping_list_id==list_id,]

    if (nrow(this_list) > 0) {
      editing_shopping_list_id(list_id)

      # Get unique recipe IDs and their multipliers
      saved_recipe_ids <- unique(this_list$recipe_id)
      selected_indices <- which(sapply(recipes_rv(), function(r) r$id) %in% saved_recipe_ids)

      selected_indices_rv(selected_indices)

      # Reconstruct multipliers from the flat tibble
      new_multipliers <- this_list %>%
        dplyr::select(recipe_id, multiplier) %>%
        dplyr::distinct() %>%
        tibble::deframe()  # named vector

      recipe_multipliers(as.list(new_multipliers))

      rehydrated_have <- setNames(this_list$have, this_list$item_id)
      have_items(as.list(rehydrated_have))


      updateTabsetPanel(session, "main_tabs", selected = "Shopping List")
    }
  })

  ###----- Observer: Wait until rows are ready   ------
  # Observer to select rows once DT is ready (input$shopping_table_rows_all exists)
  observe({
    req(selected_indices_rv())
    req(input$shopping_table_rows_all)  # DT fully loaded rows

    isolate({
      proxy %>% selectRows(selected_indices_rv())
      selected_indices_rv(NULL)  # clear trigger so it doesn't keep reselecting
    })
  })

  ###-----Observer: Cancel edit shopping list --------
  observeEvent(input$cancel_edit_list, {
    editing_shopping_list_id(NULL)      # Exit editing mode
    recipe_multipliers(list()) # Reset multipliers (or reload original if needed)
    have_items(list())         # Reset have items if needed

    # Clear table selection
    selectRows(proxy, NULL)

    # Clear status message
    output$complete_status <- renderText("Edit cancelled.")

    # Optionally, switch back to non-editing mode in UI
  })


}

shinyApp(ui, server)
