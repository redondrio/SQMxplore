read_ref <- function(path) {
    lines <- read_lines(paste0(path, "id_list.ref"))
    section <- NULL
    ref_ids <- list() #initialise reference list
    for (line in lines){
        line <- trimws(line, "both", "[ \t]")
        if (substr(line, 1, 1) == "!") { #detect section
            section <- trimws(line, "left", "[!]")
            ref_ids[[section]] <- list() #initialise section list
            next
        }
        if (section == "FUNCTIONS") {
            if (substr(line, 1, 3) == "DB:") { #detect DB
                db <- trimws(strsplit(line, ":")[[1]][2], "both", "[ ]")
                ref_ids[[section]][[db]] <- c() #initialise DB list
            } else {
                ref_ids[[section]][[db]] <- c(
                    ref_ids[[section]][[db]], line) #add identifier
            }
        }
    }
    return(ref_ids)
}