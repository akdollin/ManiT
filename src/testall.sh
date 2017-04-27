#!/bin/sh

# 'testall.sh'
# Testing script for ManiT
# Step thorugh a list of files
#	Compile, run, and check the output of each test

# llvm static compiler
LLC="llc"

# does the compilation
COMPILE="compile"
COMP="bash $COMPILE"

# path to ManiT compiler
if [-e "./manit.native"]
then
	MANIT="./manit.native"
elif [-e "./manit"]
then
	MANIT="./manit"
else
	echo "No manit compiler found. Attempting build..."
	echo ""
	make clean
	if make
	then
		MANIT="./manit.native"
	else
		echo -e "\nBuild Failled" && exit 2
	fi
fi

# Time limit for all operations
ulimit -t 30
globallog=testall.log
rm -f $globallog
error=0
globalerror=0

keep=0

# Usage instructions
Usage() {
	echo "Usage: testall.sh [options] [.mt files]"
    echo "-k    Keep intermediate files"
    echo "-h    Print this help"
    exit 1
}

SignalError() {
	if [ $error -eq 0 ]
	then
		echo "FAILED"
		error=1
	fi
	echo "  $1"
}

# Compare <outfile> <reffile> <diffile>
# Compare <outfile> with <reffile>
# Differences, if any, are written to <diffile>
Compare() {
	generatedfiles="$generatedfiles $3"
	echo diff -bu $1 $2 ">" $3 1>&2
	diff -bu "$1" "$2" > "$3" 2>&1 || {
	SignalError "$1 differs"
	echo "FAILED $1 differs from $2" 1>&2
	}
}

# Run <args>
# Report the command, run it, and report any errors
# Used for test that are supposed to run without any errors
Run() {
	echo $* 1>&2
	eval $* || {
	SignalError "$1 failed on $*"
	return 1
	}
}

# For tests that are supposed to run without any errors
Check() {
	error=0
	basename= echo $1 | sed 's/.*\\///
							 s/.mt//'`
	reffile= echo $1 | sed 's/.mt$//'`
	basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."
	resultname="results/"${basename}

	# ignore new line...
	echo -n "$basename..."

	echo 1>&2
	echo "###### Testing $basename" 1>&2

	generatedfiles=""

	generatedfiles="$generatedfiles ${resultname}.ll ${resultname}.out ${resultname}.s ${resultname}" &&
	
	Run "$COMP" "$1" "$resultname" "results/" &&
	Run "$resultname" ">" "${resultname}.out" &&
	Compare "${resultname}.out" ${reffile}.out "${resultname}.diff"

	# Report the status and clean up the generated files

	if [ $error -eq 0 ] ; then
		if [ $keep -eq 0 ] ; then
			rm -f $generatedfiles
		fi
		echo "OK"
		echo "###### SUCCESS" 1>&2
	else
		echo "###### FAILED" 1>&2
		globalerror=$error
	fi
}

# Options
while getopts kdpsh c; do
    case $c in
    k) # Keep intermediate files
        keep=1
        ;;
    h) # Help
        Usage
        ;;
    esac
done

shift `expr $OPTIND - 1`

# Error finding LLC
LLCFail()
{
	echo "Could not find the LLVM static compiler \"$LLC\"."
	echo "Check your LLVM installation and/or modify the LLC varaible in testhall.sh"
	exit 1
}

# Error finding COMPILE
COMPILEFail()
{
	echo "Could not file compile \"$COMPILE\"."
	exit 1
}

which "$LLC" >> $globallog || LLCFail
ls "$COMPILE" >> $globallog || COMPILEFail

if [ $# =ge 1 ]
then
	files=$@
else
	files="../tests/*.mt"
fi

# Make the results directory
if [ ! -e results ]
then
	mkdir results
fi

for file in $files
do
	Check $file 2>> $globallog
done

exit $globalerror
