This is an extension of  [Spectector](https://github.com/spectector/spectector) which extends the analysis to multiple different Spectre versions and their combinations


# Installation Guide
- Follow the installation guide of [Spectector](https://github.com/spectector/spectector)
- After installation, replace the spectector folder with this whole repository.
- Replace muasm_translator/ with the provided version here as well.
- Afterwards, build the tool new by doing ciao build in the command line.

# Tests
There are sub-folders specv4_tests, specv5_tests, v2_tests, sls_tests and combinations containing examples.
Each sub-folder provides its own script to execute all the tests inside it. They are also a pointer on how to use the tool.
Some sub-folders have additional README files with more information.

$ Versions
The different versions can be selected using the --version flag.
The different versions are:
- 1  : V1 (original implementation of Spectector)
- 2  : Spectre V2
- 4  : Spectre V4
- 5  : Spectre V5
- 6  : Spectre SLS

and any combination of those versions that are allowed. Versions that are not allowed are combinations of 5 and 6, because they speculate on the same instructions. More details in the paper.
For example: 
- 14 : Spectre V1 + Spectre V5
- 45 : V4 + V5
- 15 : V1 + V5
- 145: V1 + V4 + V5
- 1245 : V1 + V2 + V4 + V5

See the provided scripts to see examples for the usage.

A previous version of this repositoty is the accompaying repository to the [paper](https://arxiv.org/abs/2209.01179) published at CCS 22 which includes the implementation and the tests used in the paper.
See the release Release CCS22 for hte corresponding commit.
