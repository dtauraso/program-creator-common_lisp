# program-creator-common_lisp
What I learned while doing this project:
recursion, Common Lisp, tree data structure, Common Lisp plist data structure, lisp loops

Adding any extra spec lines is dangerous and may result in a crash
changing the names of the funcitons is ok as long as you change the names of the function_name.lisp files

The file to run the program is start.lisp.
Type clisp start.lisp to run program.

It translates some english to python and then runs the python code.  It also prints out a structure representing the abstract form of an if statement: if condition then statement else statement.  It does this with the "and" and "or" operators as well.

The purpose of "if 2 and 4" in and_examples.txt is to calculate possible code for what the design could be.  It is attempting to decipher vaugue input.

I made this program because I want the computer to translate parts of english phrases.

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
