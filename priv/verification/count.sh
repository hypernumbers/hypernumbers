#!/bin/bash
FILE=errors.28_Oct_11_12_11_02.terms
echo "Invalid tables (type 1)"
grep "Invalid tables (type 1)" $FILE | wc -l
echo "Invalid tables (type 2)"
grep "Invalid tables (type 2)" $FILE | wc -l
echo "Invalid tables (type 3)"
grep "Invalid tables (type 3)" $FILE | wc -l
echo "Invalid relations (type 1)"
grep "Invalid relations (type 1)" $FILE | wc -l
echo "Invalid relations (type 2)"
grep "Invalid relations (type 2)" $FILE | wc -l
echo "Invalid relations (type 3)"
grep "Invalid relations (type 3)" $FILE | wc -l
echo "Invalid include (type 1)"
grep "Invalid include (type 1)" $FILE | wc -l
echo "Invalid include (type 2)"
grep "Invalid include (type 2)" $FILE | wc -l
echo "Invalid timer"
grep "Invalid timer" $FILE | wc -l
echo "Invalid form"
grep "Invalid form" $FILE | wc -l
echo "Invalid formula (type 1)"
grep "Invalid formula (type 1)" $FILE | wc -l
echo "Invalid formula (type 2)"
grep "Invalid formula (type 2)" $FILE | wc -l
echo "Invalid Object (cell) (type 1)"
grep "Invalid Object (cell) (type 1)" $FILE | wc -l
echo "Invalid Object (cell) (type 2)"
grep "Invalid Object (cell) (type 2)" $FILE | wc -l
echo "Invalid Object (row) (type 3)"
grep "Invalid Object (row) (type 3)" $FILE | wc -l
echo "Invalid Object (column) (type 4)"
grep "Invalid Object (column) (type 4)" $FILE | wc -l
echo "Invalid Object (page) (type 5)"
grep "Invalid Object (page) (type 5)" $FILE | wc -l
echo "Invalid Object (type 6)"
grep "Invalid Object (type 6)" $FILE | wc -l
echo "Invalid types"
grep "Invalid types" $FILE | wc -l
echo "Invalid grid (type 1)"
grep "Invalid grid (type 1)" $FILE | wc -l
echo "Invalid grid (type 2)"
grep "Invalid grid (type 2)" $FILE | wc -l
echo "Invalid grid (type 3)"
grep "Invalid grid (type 3)" $FILE | wc -l
echo "Invalid reverse index"
grep "Invalid reverse index" $FILE | wc -l
echo "Invalid zinf"
grep "Invalid zinf" $FILE | wc -l
echo "Mutiple objects"
grep "multiple local objs" $FILE