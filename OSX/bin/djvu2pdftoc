#!/usr/bin/env python3
# Convert DJVU to PDF with table of contents, if available.
# Modified from https://github.com/kcroker/dpsprep
# License: GNU GPL v3

import sexpdata
import argparse
import os
import pipes
import subprocess
import re

# Recursively walks the sexpr tree and outputs a metadata format understandable by pdftk
def walk_bmarks(bmarks, level):
    output = ''
    wroteTitle = False
    for j in bmarks:
        if isinstance(j, list):
            output = output + walk_bmarks(j, level + 1)
        elif isinstance(j, str):
            if not wroteTitle:
                output = output + "BookmarkBegin\nBookmarkTitle: %s\nBookmarkLevel: %d\n" % (j, level)
                wroteTitle = True
            else:
                output = output + "BookmarkPageNumber: %s\n" % j.split('#')[1]
                wroteTitle = False
        else:
            pass
    return output

workpath = os.getcwd()

# From Python docs, nice and slick command line arguments
parser = argparse.ArgumentParser(description='Convert DJVU format to PDF format preserving OCRd text and metadata.  Very useful for Sony Digital Paper system')
parser.add_argument('src', metavar='djvufile', type=str,
                    help='the source DJVU file')
parser.add_argument('dest', metavar='pdffile', type=str,
                    help='the destination PDF file')
parser.add_argument('-q, --quality', dest='quality', type=int, default=80,
                    help='specify JPEG lossy compression quality (50-150).  See man ddjvu for more information.')

args = parser.parse_args()

# Reescape the filenames because we will just be sending them to commands via system
# and we don't otherwise work directly with the DJVU and PDF files.
# Also, stash the temp pdf in the clean spot
args.src = pipes.quote(args.src)
finaldest = pipes.quote(args.dest)
args.dest = workpath + '/dumpd.pdf'

# Check for a file presently being processed
if os.path.isfile(workpath + '/inprocess'):
    fname = open(workpath + '/inprocess', 'r').read()
    if not fname == args.src:
        print("ERROR: Attempting to process %s before %s is completed. Aborting." % (args.src, fname))
        exit(3)
    else:
        print("NOTE: Continuing to process %s..." % args.src)
else:
    # Record the file we are about to process
    open(workpath + '/inprocess', 'w').write(args.src)

# Make the PDF, compressing with JPG so they are not ridiculous in size
# (cwd)
if not os.path.isfile(workpath + '/dumpd.pdf'):
    retval = os.system("ddjvu -v --format=pdf %s %s/dumpd.pdf" % (args.src, workpath))
    if retval > 0:
        print("\nNOTE: There was a problem on ddjvu to convert to pdf.")
        exit(retval)
else:
    print("PDF (without toc) already found, use it.")

# Extract the bookmark data from the DJVU document
retval = 0
retval = retval | os.system("djvused %s -u -e 'print-outline' > %s/bmarks.out" % (args.src, workpath))
print("Bookmarks extracted.")

# Check for zero-length outline
if os.stat("%s/bmarks.out" % workpath).st_size > 0:

    # Extract the metadata from the PDF document
    retval = retval | os.system("pdftk %s dump_data_utf8 > %s/pdfmetadata.out" % (args.dest, workpath))
    print("Original PDF metadata extracted.")

    # Parse the sexpr
    pdfbmarks = walk_bmarks(sexpdata.load(open(workpath + '/bmarks.out')), 0)

    # Integrate the parsed bookmarks into the PDF metadata
    p = re.compile('NumberOfPages: [0-9]+')
    metadata = open("%s/pdfmetadata.out" % workpath, 'r').read()

    for m in p.finditer(metadata):
        loc = int(m.end())

        newoutput = metadata[:loc] + "\n" + pdfbmarks[:-1] + metadata[loc:]

        # Update the PDF metadata
        open("%s/pdfmetadata.in" % workpath, 'w').write(newoutput)
        retval = retval | os.system("pdftk %s update_info_utf8 %s output %s" % (args.dest, workpath + '/pdfmetadata.in', finaldest))

else:
    retval = retval | os.system("mv %s %s" % (args.dest, finaldest))
    print("No bookmarks were present!")

# If retval is shit, don't delete temp files
if retval == 0:
    os.system("rm %s/inprocess %s" % (workpath, args.dest))
    print("SUCCESS. Temporary files cleared.")
    exit(0)
else:
    print("There were errors in the metadata step.  Check the errors.")
    exit(retval)
