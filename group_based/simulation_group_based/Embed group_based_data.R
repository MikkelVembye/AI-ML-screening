#--------------------------------------------------------------------------
# Embed the group-based data (before the AI screening)
#--------------------------------------------------------------------------
source("simulation/Simulation function.R")

group_based_data <- readRDS("group_based/data/group_based_data.rds")

python_dir <- "C:/Users/B375477/AppData/Local/miniconda3/envs/positron-python/python.exe"
embedding_dir <- "group_based/data/embeddings"

#--------------------------------------------------------------------------
# Embeddings (saved as group_based_data_<model>.rds in the embedding folder)
#--------------------------------------------------------------------------
tictoc::tic()
embed_corpus(group_based_data, "tfidf", python_dir = python_dir, dir = embedding_dir)
embed_corpus(group_based_data, "all-MiniLM-L6-v2", python_dir = python_dir, dir = embedding_dir)
embed_corpus(group_based_data, "BAAI/bge-m3", python_dir = python_dir, dir = embedding_dir)
embed_corpus(group_based_data, "Alibaba-NLP/gte-base-en-v1.5", python_dir = python_dir, dir = embedding_dir)
embed_corpus(group_based_data, "Alibaba-NLP/gte-large-en-v1.5", python_dir = python_dir, dir = embedding_dir)
tictoc::toc()

# Check: one row per record, named by eppi_id
emb <- readRDS(file.path(embedding_dir, "group_based_data_tfidf.rds"))
dim(emb)
stopifnot(nrow(emb) == nrow(group_based_data), identical(rownames(emb), group_based_data$eppi_id))

