# This script will create a cocktail-descriptions.yaml file if it doesn't already exist.
# This is a yaml file that contains all the drink info scraped from wikipedia
# (from iba-cocktails-wiki.json) plus NULL fields for all the things we want to 
# fill in, like the image, fun facts, etc. This script will then start filling in all 
# the nulls. As the ChatGPT "hangs" regularly, I needed to rerun this script many times
# until cocktail-descriptions.yaml was completely filled in.
#
# If a generated description or images is wonky, just delete it and rerun this script
# and it will be regenerated.

library(tidyverse)
library(yaml)
library(jsonlite)
library(httr)
library(glue)

openai_api_key <- Sys.getenv("OPENAI_API_KEY")

if(openai_api_key == "") {
  stop("OPENAI_API_KEY is not set.")
}

api_error_message <- function(response) {
  parsed <- tryCatch(
    content(response, as = "parsed", type = "application/json", encoding = "UTF-8"),
    error = function(e) NULL
  )
  if(!is.null(parsed) && !is.null(parsed$error) && !is.null(parsed$error$message)) {
    return(parsed$error$message)
  }
  paste("HTTP", status_code(response))
}

assert_openai_success <- function(response, endpoint_name) {
  if(status_code(response) >= 400) {
    stop(
      glue("OpenAI {endpoint_name} failed ({status_code(response)}): {api_error_message(response)}"),
      call. = FALSE
    )
  }
}

# Create the cocktail-descriptions.yaml if it doesn't already exist
cocktails <- read_json("iba-cocktails-wiki.json")
if(!file.exists("cocktail-descriptions.yaml")) {
  placeholder_cocktail_descriptions <- map(cocktails, \(cocktail) {
    base_fname = fname <- cocktail$name |>
        tolower() |> 
        str_replace_all("[^a-z0-9]", "_")
    image_path <- file.path("cocktail-images",  paste0(base_fname, ".jpeg"))
    c(cocktail, list(
      base_fname = base_fname,
      image_path = image_path,
      hook = NULL,
      page_body = NULL,
      extended_method = NULL,
      tips = NULL,
      fun_facts = NULL,
      alcohol_free_alternative = NULL
    ))
  }) 
  write_yaml(placeholder_cocktail_descriptions, "cocktail-descriptions.yaml")  
}
cocktail_descriptions <- read_yaml("cocktail-descriptions.yaml")
dir.create("cocktail-images", showWarnings = FALSE)


# Calls the OpenAI text generation API with the given prompt and returns the answer.
generate_text <- function(prompt) {
  response <- RETRY(
    "POST",
    times = 3,
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", openai_api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-5.4",
      messages = list(list(
        role = "user", 
        content = prompt
      ))
    ),
    timeout(seconds = 90)
  )
  assert_openai_success(response, "chat completion request")
  str_trim(content(response)$choices[[1]]$message$content)
}

# Calls the OpenAI Images API and returns raw image bytes.
generate_image <- function(prompt) {
  response <- RETRY(
    "POST",
    times = 3,
    url = "https://api.openai.com/v1/images/generations",
    add_headers(Authorization = paste("Bearer", openai_api_key)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-image-1.5",
      prompt = prompt,
      size = "1024x1024",
      quality = "medium",
      output_format = "jpeg"
    ),
    timeout(seconds = 120)
  )
  assert_openai_success(response, "image generation request")
  parsed <- content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
  image_b64 <- parsed$data[[1]]$b64_json
  if(is.null(image_b64)) {
    stop("Image generation failed: response did not contain b64_json.")
  }
  jsonlite::base64_dec(image_b64)
}


generate_and_save_image <- function(prompt, path) {
  image_raw <- generate_image(prompt)
  writeBin(image_raw, path)
  path
}

# A function that takes a prompt template and generates another function which 
# takes a cocktail, as from cocktail_descriptions, completes the template and returns
# a ChatGPT answer.
gen_text_func <- function(...) {
  function(cocktail) {
    prompt <- glue_data(cocktail, ..., .sep = "\n")
    cat(glue("Generating text for: {prompt}"), "\n")
    generate_text(prompt)
  }
}

# A list of generator functions that can generate text/images for the corresponding
# field, if missing.
generators <- list( 
  image_path = \(cocktail) {
    path = file.path("cocktail-images",  paste0(cocktail$base_fname, ".jpeg"))
    prompt = glue_data(cocktail,
      "Photorealistic editorial cocktail photo of a {name} served in a {standard_drinkware} on a bartop.",
      "Professional food photography, centered composition, natural lighting, shallow depth of field.",
      "No people, no text, no logos, no watermark.",
      .sep = " "
    )
    cat(glue("Generating image for: {prompt}"), "\n")
    generate_and_save_image(prompt, path)
    path
  },
  hook = gen_text_func(
    "Write a two-sentence hook for the cocktail {name}.",
    "Keep it punchy, concrete, and interesting.",
    "Do not use markdown headings or bullet points."
  ),
  page_body = gen_text_func(
    "Write markdown content for the lower half of a cocktail page about {name}.",
    "Cocktail metadata:",
    "- Category: {category}",
    "- Type: {type}",
    "- Served: {served}",
    "- Standard drinkware: {standard_drinkware}",
    "- Ingredients:\n{as.yaml(ingredients)}",
    "- Method summary: {method}",
    "{ifelse(is.null(cocktail$notes), '', paste0('- Notes: ', cocktail$notes))}",
    "Requirements:",
    "- Start directly with content (no front matter and no repeated title).",
    "- Begin with an extended preparation section right after the image.",
    "- In that first section, include a markdown numbered list with 4 to 8 clear steps for making the drink.",
    "- Use 3 to 5 short sections with markdown headings you choose yourself (including the first preparation section).",
    "- Vary structure and heading wording across cocktails.",
    "- Headings must be concrete and cocktail-specific; avoid generic labels like 'In the glass' or 'Serving notes'.",
    "- Do not repeat the exact ingredient list text from earlier on the page.",
    "- The numbered steps should be more detailed than the short method summary shown earlier.",
    "- Include one section with history (when relevant and known) or fun facts/trivia (when such facts exist).",
    "- If origin details are uncertain, say so briefly and share the most credible context available.",
    "- Include practical serving insight, flavor/profile context, and one non-alcoholic riff or adaptation.",
    "- Keep total length around 220 to 380 words.",
    "- Use plain markdown only."
  )
)

# Looping through the cocktail description and filling in all the blanks.
# As both the text and image generating APIs errors out now and then
# this loop saves every single response directly to cocktail-descriptions.yaml
required_generated_fields <- names(generators)
for(cocktail_i in seq_along(cocktail_descriptions)) {
  cocktail <- cocktail_descriptions[[cocktail_i]]
  for(field in required_generated_fields) {
    if(!(field %in% names(cocktail))) {
      cocktail[field] <- list(NULL)
    }
  }
  if(! is.null(cocktail$image_path) && ! file.exists(cocktail$image_path)) {
    cocktail["image_path"] <- list(NULL)
  }
  items_to_fill <- cocktail[required_generated_fields] |> keep(is.null) |> names()
  for(item in items_to_fill) {
    cocktail[[item]] <- generators[[item]](cocktail)
    cocktail_descriptions[[cocktail_i]] <- cocktail
    write_yaml(cocktail_descriptions, "cocktail-descriptions.yaml") 
  }
}
