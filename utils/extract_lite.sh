#!/bin/sh

# Programme to extract the needed files from a project
# Extract the minimum tables for a SQMlite object
# Extract the 22.stats file and parse it into 5 tables with 22parser.py
# Compress all files

# The 22_parser.py file must be in the same directory

# set the variables
proj_path=$1

if ! [ -e $proj_path]
then
	echo "Invalid project path"
	exit
fi

if [ -e temp_dir]
then
	echo "Temporary directory (temp_dir) already exists"
	exit
fi

mkdir temp_dir

# get the minimum tables and move them to the temporary folder
ls $proj_path/results/tables/ | grep -vE "orf|bin|contig" | xargs -I {} cp $proj_path/results/tables/{} temp_dir
echo "Tables copied"
# get the 22.stats file and move it into the temporary folder
cp $proj_path/results/22* temp_dir

# parse the tables
python 22_parser.py temp_dir/22*
echo "Statistics parsed"

# compress the folder
echo "Compressing"
tar -czvf $proj_path.tar.gz temp_dir

# finish
rm -r temp_dir
echo "Finished"