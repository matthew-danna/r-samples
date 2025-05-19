# https://ellmer.tidyverse.org/

# github tokens
# https://usethis.r-lib.org/articles/git-credentials.html
# set API keys here
usethis::edit_r_environ()
gitcreds::gitcreds_set()
usethis::gh_token_help()

# install.packages('ellmer')
library(ellmer)

chat.gemini <- chat_gemini(
  system_prompt = "You are a data scientist that uses statistics to make decisions and has played the mobile game Marvel Strike Force since it's global launch",
  turns = NULL,
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = Sys.getenv('GOOGLE_API_KEY'),
  model = "gemini-2.0-flash",
  api_args = list(),
  echo = "all"
)

live_console(chat.gemini)

chat.groq <- chat_groq(
  system_prompt = "You are a data scientist that uses statistics to make decisions and has played the mobile game Marvel Strike Force since it's global launch",
  turns = NULL,
  base_url = "https://api.groq.com/openai/v1",
  api_key = Sys.getenv('GROQ_API_KEY'),
  model = "llama3-8b-8192",
  seed = NULL,
  api_args = list(),
  echo = "all"
)

live_console(chat.groq)

chat.git <- chat_github(
  system_prompt = "You are a data scientist that uses statistics to make decisions and has played the mobile game Marvel Strike Force since it's global launch",
  base_url = "https://models.inference.ai.azure.com/",
  api_key = Sys.getenv('GITHUB_PAT'),
  model = "gpt-4o",
  seed = NULL,
  api_args = list(),
  echo = "all"
)

live_console(chat.git)

#### not working yet
chat.openai <- chat_openai(
  system_prompt = "You are a data scientist that uses statistics to make decisions and has played the mobile game Marvel Strike Force since it's global launch",
  base_url = "https://api.openai.com/v1",
  api_key = Sys.getenv('OPENAI_API_KEY'),
  model = NULL,
#  model = "gpt-4o",
  seed = NULL,
  api_args = list(),
  echo = "all"
)

live_console(chat.openai)

models_openai(base_url = "https://api.openai.com/v1", api_key = openai_key())

