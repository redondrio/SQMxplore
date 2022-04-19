#!/usr/bin/env python
# coding: utf-8
# This parser separates the 22.stats file by SQM into 5 tables
# that can be loaded in R to use in the shiny app
import sys
file = sys.argv[1]
with open(file,"r") as stats,    open("./temp_dir/22.reads.tsv","w") as reads_stats,    open("./temp_dir/22.contigs.tsv","w") as contigs_stats,    open("./temp_dir/22.taxa.tsv","w") as taxa_stats,    open("./temp_dir/22.orfs.tsv","w") as orfs_stats,    open("./temp_dir/22.bins.tsv","w") as bins_stats:  
    section = ["header","reads","contigs","taxa","orfs","bins"]
    section_i = 0
    for line in stats:
        if line[0:2]=="#-": section_i += 1
        if line[0:18]=="Most abundant taxa": section_i += 1
        if line[0:2]=="#-": continue #skip comment line
        if section[section_i]=="reads":
            reads_stats.write(line)
        if section[section_i]=="contigs":
            contigs_stats.write(line)
        if section[section_i]=="taxa":
            taxa_stats.write(line)
        if section[section_i]=="orfs":
            orfs_stats.write(line)
        if section[section_i]=="bins":
            bins_stats.write(line)

