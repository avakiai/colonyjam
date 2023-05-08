# download data attached to repo
require(magrittr)
require(piggyback)


data <- pb_list(repo = "avakiai/colonyjam", 
        tag = "v0.1")

# 1. download raw data
pb_download(repo = "avakiai/colonyjam", 
            tag = "v0.1",
            dest = "./R/1-data/input",
            file = data$file_name[grep(data$file_name,pattern="_data.RDS")])

# 2. download processed data cache
pb_download(repo = "avakiai/colonyjam", 
            tag = "v0.1",
            dest = "./R/1-data/output",
            file = data$file_name[grep(data$file_name,pattern="prc")])
