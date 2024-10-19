# Downloading GPT-2 weights
library(hfhub)
library(tok)
model_id <- "meta-llama/Meta-Llama-Guard-2-8B"
path <- 'C:/Users/colmk/Meta-Llama-Guard-2-8B'
library(tensorflow)
library(safetensors)
transformers <- reticulate::import('transformers')
torch <- reticulate::import('torch')
accelerate <- reticulate::import('accelerate')


pipeline <- transformers$pipeline("text-generation", model=path)

pipeline("Hey how are you doing today?")

library(chattr)

chattr_use('gpt35')

chattr::chattr_app()


