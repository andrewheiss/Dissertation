#!/usr/bin/env python3

# Escape unescaped quotes in ICEWS data
#
# The raw ICEWS files do not escape quotes, which makes R's readr::read_tsv
# choke and skip lines. This script opens every .tab file in a folder (both
# compressed and uncompressed) and escapes double quotes (i.e. " -> \")
#
# Usage: fix_icews.py [-h] input_folder clean_folder
#
# positional arguments:
#   input_folder  location of zipped files
#   clean_folder  location for cleaned files

# Load libraries
import argparse
import fnmatch
import io
import time
import os
import zipfile
from collections import namedtuple

# Get command line arguments
# Check if folder actually exists and return expanded full path
# via https://gist.github.com/brantfaircloth/1443543
class FullPaths(argparse.Action):
    """Expand user- and relative-paths"""
    def __call__(self, parser, namespace, values, option_string=None):
        setattr(namespace, self.dest, os.path.abspath(os.path.expanduser(values)))

def is_dir(dirname):
    """Checks if a path is an actual directory"""
    if not os.path.isdir(dirname):
        msg = "{0} is not a directory".format(dirname)
        raise argparse.ArgumentTypeError(msg)
    else:
        return dirname

parser = argparse.ArgumentParser(description='Escape unescaped quotes in ICEWS data')
parser.add_argument('input_folder', action=FullPaths, type=is_dir,
                    help='location of zipped files')
parser.add_argument('clean_folder', action=FullPaths, type=is_dir,
                    help='location for cleaned files')
args = parser.parse_args()


# Access parts of the filename as a nice named tuple
fn = namedtuple('fn', ['zipped', 'tab'])

def parse_filename(x):
    return(fn(os.path.basename(x),
              os.path.basename(x).replace('.zip', '')))

# Loop through each file in the input folder. If it's a zipped file, read it,
# escaped quotes, and write to new file. If it's a tabular file, do the same
# thing and create a new zipped file for it.
for x in os.listdir(args.input_folder):
    if fnmatch.fnmatch(x, '*.zip'):
        print("Fixing {0}".format(x))
        current_file = parse_filename(x)
        with zipfile.ZipFile(os.path.join(args.input_folder,
                                          current_file.zipped), 'r') as z:
            with z.open(current_file.tab, 'rU') as f, \
                 zipfile.ZipFile(os.path.join(args.clean_folder,
                                              current_file.zipped), 'w') as out_zip:
                # Read file inside zipped file and escape quotes
                data_raw = io.TextIOWrapper(f, newline=None)
                data = ''.join([line.replace('"', '\\"') for line in data_raw])

                # Add fixed file to new zipped file
                new_name = zipfile.ZipInfo(current_file.tab)
                new_name.compress_type = zipfile.ZIP_DEFLATED
                new_name.date_time = time.localtime(time.time())[:6]
                out_zip.writestr(new_name, data)
    elif fnmatch.fnmatch(x, '*.tab'):
        print("Fixing {0}".format(x))
        with open(os.path.join(args.input_folder, x), 'r') as f:
            with zipfile.ZipFile(os.path.join(args.clean_folder, x +
                                              '.zip'), 'w') as out_zip:
                data = str(f.read()).replace('"', '\\"')
                new_name = zipfile.ZipInfo(x)
                new_name.compress_type = zipfile.ZIP_DEFLATED
                new_name.date_time = time.localtime(time.time())[:6]
                out_zip.writestr(new_name, data)
