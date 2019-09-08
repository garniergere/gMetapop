# gMetapop
A flexible forward simulation program for modelling the evolution of genes and phenotypes in subdivided populations of species with complex demographic features.

## Overview ##
gMetapop is an individual-based program, individuals being characterized by their genotype at many loci and optionally by their phenotype at a given trait coded by quantitative trait loci. Different types of genes (neutral or under selection) can be simulated, in populations connected by gene flow, either via zygote migration (e.g. seeds) or male gamete migration (e.g. pollen flow). gMetapop also permits demographic models with overlapping generations (for example, perennial species such as trees or many animals with an age class structure), and complex mating systems including selfing and clonal reproduction. One can apply selection within and among populations with different intensities, either on a phenotypic trait determined by theoretical allele additive values or directly by assigning fitness values to each genotype. The total number of loci, individuals, populations and/or generations (or reproductive events) is only limited by the users’ hardware capacities. 

One main advantage of this modelling tool is its user-friendly and flexible Graphical User Interface (GUI) available under Windows and Linux OS. We developped the GUI to replace the cumbersome process of building the large configuration parameter files needed for elaborate evolutionary scenarios. The users can therefore focus on their biological model. The GUI makes also the program a great learning tool for basic evolutionary processes in population or quantitative genetics practical courses. Additionally, we designed several contrasted and detailed examples of simulation scenarios that give a more intuitive and efficient “getting started” with the software (Chapter 5 of the User Manual).  

The software is thus constituted by two main components: 

•	gMetapop_GUI, the interface program which guides the user for designing simulations, launching them, and getting results graphical overview. Technically, the GUI creates an .xml parameter configuration file (param hereafter) with all initial settings and files needed for a simulation run.

•	gMetapop_CORE, the program that takes these files as input to perform the simulations. gMetapop_CORE can be launched directly from the GUI or using command lines under Linux or Windows DOS command prompt. gMetapop_CORE initially stemmed from previous programs that included separate and complementary features (Le Corre et al., 1997; Austerlitz et al. 2000; Machon et al., 2003; Austerlitz & Garnier-Géré 2003; Le Corre and Kremer, 2003). However in this first release, 88% of the whole program code is new (70%) or has been largely modified or extended (18%). 

## Help ##
Various sources of Help are available 

•	Default parameter values proposed in the GUI and automatically loaded in the GUI when making a new param file. We chose them so that they are plausible in scenarios corresponding to non-extreme biological conditions. Therefore, users can keep those values as starting points for preliminary tests that focus on particular parameters, provided that they carefully checked if those are compatible with the organisms that they have in mind.

•	Tooltips appear on screen that describe tabs and parameters within the GUI, when moving the “mouse” on the each sub-window/group-box part of the GUI in a Tab. Tooltips are small texts providing definitions and main use of the corresponding GUI options, for a rapid and intuitive appraisal of particular aspects of the modelled processes (see more explanations in Chapter 3 User Manual for each GUI option). 

•	Tutorials to reproduce simulation tests 1 to 13 are given in Chapter 5 of the User Manual with examples .xml param configuration files provided online at for loading into the GUI. Some of these param files might be better starting points than default parameters in some cases. All configuration files for launching simulations, results files and post-treatment examples are also provided under each test subfolder at <A HREF=""></A>  

•	Finally, more explanations can be found in the User Manual on different GUI features and processes modelled in gMetapop_CORE (Chapter 3), on all input and output files (Chapter 4), and on detailed step by step procedure for building param configuration files (Chapter 5). 

## How to cite gMetapop? ##

Raspail F, Austerlitz F, Mariette S, Le Corre V, Machon N, Baradat D, Gouyon P-H, Godelle B, Kremer A, Garnier-Géré P. gMetapop, a software for simulating the evolution of genes and traits in subdivided populations of species with complex demographic features. in preparation 

## License ##

The content of this repository is licensed under <A HREF="https://choosealicense.com/licenses/gpl-3.0/">(GNU GPLv3)</A> 


