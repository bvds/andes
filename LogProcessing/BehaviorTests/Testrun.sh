# Run the log tests via a shell command dedicated to generating the
# desired data for Kurt.  I am using a shell command to avoid the problems
# of memory maxxing that I have encountered previously using the 
# lisp commands directly.
#
# This code will make several calls to the Allegro code that I have written
# iteratively testing each directory and Storing the results in a specific 
# target file before Exiting Lisp.
#
# The sum purpose of this exercize is to hack my way around the fact that 
# Allegro does not free resources properly on Windows and will crash unless
# We go about clearing out the memory.  This seemed to be an efficient solution.

LSP="c:/\"Program Files\"/acl61/alisp.exe" 
LOAD="c:/Andes2/HelpDriver/Headers.cl" 
OUTPATH="c:/Andes2/Helpdriver/out"

# ------------------------------------------------------------------
# Generation
# Iterate over the stored logfiles generating the relavent testdata

#eval "$LSP -d $OUTPATH/USNA-Fall1999.drib -L $LOAD -e '(generate-USNA-Fall1999)' -kill"
#eval "$LSP -d $OUTPATH/USNA-Fall2000.drib -L $LOAD -e '(generate-USNA-Fall2000)' -kill"
#eval "$LSP -d $OUTPATH/USNA-Fall2001.drib -L $LOAD -e '(generate-USNA-Fall2001)' -kill"
#eval "$LSP -d $OUTPATH/USNA-Fall2002.drib -L $LOAD -e '(generate-USNA-Fall2002)' -kill"

#eval "$LSP -d $OUTPATH/Pitt-Fall1999.drib -L $LOAD -e '(generate-Pitt-Fall1999)' -kill"
#eval "$LSP -d $OUTPATH/Pitt-Fall2000.drib -L $LOAD -e '(generate-Pitt-Fall2000)' -kill"
#eval "$LSP -d $OUTPATH/Pitt-Fall2001.drib -L $LOAD -e '(generate-Pitt-Fall2001)' -kill"


# --------------------------------------------------------------------------
# Post-Process
# Iterate over each dataset post-processing the result values for later use.

#eval "$LSP -d $OUTPATH/USNA-Fall1999-File.drib -L $LOAD -e '(post-process-USNA-Fall1999)' -kill"
#eval "$LSP -d $OUTPATH/USNA-Fall2000-File.drib -L $LOAD -e '(post-process-USNA-Fall2000)' -kill"
#eval "$LSP -d $OUTPATH/USNA-Fall2001-File.drib -L $LOAD -e '(post-process-USNA-Fall2001)' -kill"
#eval "$LSP -d $OUTPATH/USNA-Fall2002-File.drib -L $LOAD -e '(post-process-USNA-Fall2002)' -kill"

#eval "$LSP -d $OUTPATH/Pitt-Fall1999-File.drib -L $LOAD -e '(post-process-Pitt-Fall1999)' -kill"
#eval "$LSP -d $OUTPATH/Pitt-Fall2000-File.drib -L $LOAD -e '(post-process-Pitt-Fall2000)' -kill"
#eval "$LSP -d $OUTPATH/Pitt-Fall2001-File.drib -L $LOAD -e '(post-process-Pitt-Fall2001)' -kill"





# --------------------------------------------------------------------------
# Testing
# Uncomment the lines here (and comment those above) in order to 
# generate testing databases that can then ban analized as necessary.

eval "$LSP -d $OUTPATH/USNA-Fall2002.drib -L $LOAD -e '(generate-USNA-Fall2002-comp)' -kill"
eval "$LSP -d $OUTPATH/Pitt-Fall1999.drib -L $LOAD -e '(generate-Pitt-Fall1999-comp)' -kill"







# Carry out the binning.
#eval "$LSP -d $OUTPATH/bin-USNA-Fall1999.drib -L $LOAD -e '(classic-bin-USNA-Fall1999)' -kill"
#eval "$LSP -d $OUTPATH/bin-USNA-Fall2000.drib -L $LOAD -e '(classic-bin-USNA-Fall2000)' -kill"
#eval "$LSP -d $OUTPATH/bin-USNA-Fall2001.drib -L $LOAD -e '(classic-bin-USNA-Fall2001)' -kill"
eval "$LSP -d $OUTPATH/bin-USNA-Fall2002.drib -L $LOAD -e '(classic-bin-USNA-Fall2002)' -kill"

eval "$LSP -d $OUTPATH/bin-Pitt-Fall1999.drib -L $LOAD -e '(classic-bin-Pitt-Fall1999)' -kill"
#eval "$LSP -d $OUTPATH/bin-Pitt-Fall2000.drib -L $LOAD -e '(classic-bin-Pitt-Fall2000)' -kill"
#eval "$LSP -d $OUTPATH/bin-Pitt-Fall2001.drib -L $LOAD -e '(classic-bin-Pitt-Fall2001)' -kill"



