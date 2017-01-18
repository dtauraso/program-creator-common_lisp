# program-creator-common_lisp
What I learned while doing this project:
recursion, Common Lisp, tree data structure, Common Lisp plist data structure

Adding any extra spec lines is dangerous and may result in a crash
changing the names of the funcitons is ok as long as you change the names of the function_name.lisp files

The file to run the program is start.lisp.
Type clisp start.lisp to run program.

I could have had a list of replacement text items to convert the number words("five", "seven").  I didn't becuase I was trying to make the process general for all input.

To make the abstract group names none of the elements in the specs are supposed to be treated as special as using a hardcoded text replacement list.  The function_names.lisp is a filter for remaining possible function names because I needed some way for the program to "discover the correct name".

When it knows what the funciton names are, it is supposed to group the input according to the position of the funciton names and parameters relative to their positions in the specs.

The purpose of "if 2 and 4" in and_examples.txt is to calculate possible code for what the design could be.  It is attempting to decipher vaugue input.

I made this program because I want the computer to understand me.

The code works for python 3.

Each line in and_examples.txt represents specs.

Each of the specs represents 1 program.

#
Adding any extra spec lines is dangerous and may result in a crash.
#
Changing the names of the funcitons is ok as long as you change the names of the function_name.lisp files.
#
 The file to run the program is start.lisp.
Type clisp start.lisp to run the program.
#
The Python code created by this program currently works on Python 2.7.10 and Python 3.4.2.
