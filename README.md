This is the accompaying repository to the [paper](https://arxiv.org/abs/2209.01179) published at CCS 22 which includes the implementation and the tests used in the paper.

This is an extension of [Spectector](https://github.com/spectector/spectector) which extends the analysis to Spectre V4 and Spectre V5. Furthermore, it implements the framework for the combined approach presented in the paper. 
Thus, it can analyse the Spectre combinations V1+V4, V4+V5, v1+V5 and V1+V4+V5.

More details are in the paper.
# Installation Guide
- Follow the installation guide of [Spectector](https://github.com/spectector/spectector)
- After installation, replace the spectector folder with this whole repository.
- Replace x86_to_muasm.pl in muasm_translator/src with the provided version here as well.
- Afterwards, build the tool new by doing ciao build in the command line.

The changes to x86_to_muasm.pl are two lines that add callstart and retstart before every call and return. This is done so that we can differentiate between call isntructions and a normal push and jmp instruction and similarly to 
identify return instructions.
# Tests
There are 3 sub-folders specv4_tests, specv5_tests and combinations containing the examples used in the paper.
Each sub-folder provides its own script to execute all the tests inside it. They are also a pointer on how to use the tool.
Some sub-folders have additional README files with more information.
To compile the programs yourself, use the provided makefiles in each of the subfolders. Please note additional instructions that are contained in the README of the sub-folder

$ Versions
The different versions can be selected using the --version flag.
The different versions are:
- 1  : V1 (original implementation of Spectector)
- 4  : Spectre V4
- 5  : Spectre V5
- 14 : Spectre V1 + Spectre V5
- 45 : V4 + V5
- 15 : V1 + V5
- 145: V1 + V4 + V5
