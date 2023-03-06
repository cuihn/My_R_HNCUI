install.packages("reticulate")
library(reticulate)
use_python("/Users/hainingcui/opt/anaconda3/myenv/bin/python")
py_install("openai")
openai <- import("openai")
openai$api_key <- "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"

