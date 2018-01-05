#!/bin/sh
# Author: Martin Vassor
# Description: Script to run the tests of the pi-lambda project
# Creation date: 05-01-2018
# Last modified: sam. 06 janv. 2018 00:09:41 CET
# Known bugs: 

print_help() {
	echo "Usage: $0"
	echo "./test.sh"
}

TEST_TABLE="test_table"
NUMBER_FAILED=0

if [ ! -e "$TEST_TABLE" ]; then
	echo "The file test_table is not found";
	exit -1
fi

while read p; do
	TEST_FOLDER="$(echo $p | cut -d ' ' -f1 -)"
	EXPECTED_RETURN="$(echo $p | cut -d ' ' -f2 -)"
	for i in $TEST_FOLDER/*; do
		../inter --input "$i" 1>/dev/null 2>/dev/null
		RETURNED="$?"
		if [ $RETURNED -ne $EXPECTED_RETURN ]; then 
			echo "In file: $i, expected return value $EXPECTED_RETURN but got $RETURNED"
			NUMBER_FAILED="$( echo "$NUMBER_FAILED 1 +p" | dc)"
		fi
	done;
done < "$TEST_TABLE"

echo "Number of failed tests: $NUMBER_FAILED"
if [ "0" = "$NUMBER_FAILED" ]; then
	exit 0
else 
	exit 1
fi

: <<=cut

=pod

=head1 NAME

./test.sh

=head1 SYNOPSIS

./test.sh

Given a test_table file, execute all tests and prints the number of errors.

A test_table is a file containing two columns: in the first column is the name of a test subfolder and in the second column is the returned value expected for all test cases in the subfolder.

=head1 AUTHOR

Martin Vassor
=cut
