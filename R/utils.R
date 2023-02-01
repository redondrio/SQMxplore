# Read a predefined list of identifiers
# Takes the path to the file as input
# The file is expected to be named id_list.ref
# and must have the format:
# !functions
#     DB:KEGG
#         K01524
#         K03561
#     DB:CAZy
#         GH32
#         GH1
# Returns a list with the structure:
# $functions
#     $KEGG
#         "K01524" "K03561"
#     $CAZy
#         "GH32" "GH1"

read_ref <- function(file_path) {
    lines <- read_lines(file_path)
    section <- NULL
    ref_ids <- list() #initialise reference list
    for (line in lines){
        line <- trimws(line, "both", "[ \t]")
        if (substr(line, 1, 1) == "!") { #detect section
            section <- trimws(line, "left", "[!]")
            ref_ids[[section]] <- list() #initialise section list
            next
        }
        if (section == "functions") {
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

# Takes the path to a list of identifiers
# and an SQM object to compare against
# Returns a list equivalent to that of read_ref
# where the identifiers now have the full name
# if they exist in the SQM object, otherwise 
# they are NULL

check_ref <- function(SQM, file_path) {
    ref_ids <- read_ref(file_path) #read reference identifiers
    check_ids <- list() #initialise checker list
    for (section in names(ref_ids)){
        check_ids[[section]] <- list() #initialise section list
        for (db in names(ref_ids[[section]])){
            check_ids[[section]][[db]] <- c() #initialise db list
            uniques <- unique(rownames(
                SQM[[section]][[db]][["abund"]])) #get existing annotations
            for (ref in ref_ids[[section]][[db]]) {
                full_ref <- grep(ref, uniques, value = TRUE) #get full name
                if (length(full_ref) == 0) {
                    check_ids[[section]][[db]] <- c(
                        check_ids[[section]][[db]], NULL) #id not found
                } else {
                    check_ids[[section]][[db]] <- c(
                        check_ids[[section]][[db]], full_ref) #add identifier
                }
            }
        }
    }
    return(check_ids)
}

# This function will parse the stat file into separate files for the
# main programme to read. Path is the path to the stats files, and creator
# is a character string indicating the type of creator [SQM_longreads, 
# SQM_reads, SqueezeMeta]

parse_stats <- function(path, creator) {
    # Modify the pattern to change what is considered a stat file
    # Admits 21 and 22 for compatibility with SQM versions
    # where stats were 22
    main_stats <- readLines(file(
        paste0(path, "/", dir(path, pattern = "^[21,22]*.stats")[1])))
    print(main_stats)
    sections <- switch(creator,
        "SQM_longreads" = c("header", "reads", "hits", "functions"),
        "SQM_reads"     = c("header", "reads", "hits", "functions"),
        "SqueezeMeta"   = c("header", "reads", "contigs", "taxa",
            "orfs", "bins")
    )
    section_i <- 1
    for (line in main_stats) {
        # For each header, add 1 to section and set new output file
        if (substr(line, 1, 2) == "#-" ||
            substr(line, 1, 18) == "Most abundant taxa") {
            section_i <- section_i + 1
            out_file <- switch(sections[section_i],
                "reads"     = "22.reads.tsv",
                "contigs"   = "22.contigs.tsv",
                "taxa"      = "22.taxa.tsv",
                "orfs"      = "22.orfs.tsv",
                "bins"      = "22.bins.tsv",
                "hits"      = "22.hits.tsv",
                "functions" = "22.functions.tsv",
            )
        }
        if (substr(line, 1, 2) == "#-") {
            next
        } #skip header line
        if (section_i > 1) {
            print(line)
            write(line, file = paste0(path, "/", out_file), append = TRUE)
        } #only write after header
    }
}