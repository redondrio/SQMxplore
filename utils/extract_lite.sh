#!/bin/sh

# Programme to extract the needed files from an SQM project
# Extract the minimum tables to load an SQMlite object
# Extract the 22.stats file and parse it into 5 tables with 22parser.py
# Compress all files

# The 22_parser.py file must be in the same directory

# set the variables
proj_path=$1

if ! [ -e $proj_path ]
then
	echo "Invalid project path"
	exit
fi

if [ -e temp_dir ]
then
	echo "Auxiliary directory (temp_dir) already exists"
	exit
fi

if [ -e min_$proj_path ]
then
	echo "Output directory (min_project) already exists"
	exit
fi

mkdir temp_dir
mkdir min_$proj_path
mkdir min_$proj_path/results
mkdir min_$proj_path/results/tables

# get the minimum tables and move them to the new project
ls $proj_path/results/tables/ | grep -vE "orf|bin|contig" | xargs -I {} cp $proj_path/results/tables/{} min_$proj_path/results/tables
echo "Tables copied"
# get the 22.stats file and move it into the temporary directory
cp $proj_path/results/*stats temp_dir

# parse the tables and move them into the new project
python3 22_parser.py temp_dir/*stats
cp temp_dir/* min_$proj_path/results/
echo "Statistics parsed"

# compress the folder
echo "Compressing"
tar -czvf min_$proj_path.tar.gz min_$proj_path

# finish
rm -r temp_dir
echo "Finished"
