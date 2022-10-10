INSTALLATION INSTRUCTIONS

Download the compressed files corresponding to your Operating System (Windows 64 bits=Win64, Linux 64 bits=Linux64, or Windows 32 bits=Win32), and extract them into a folder of your choice:

* Under Win64, click on the gMetapop_GUI_Win64.exe to launch gMetapop_GUI, from which gMetapop_CORE_Win64.exe can be launched.

* Under Linux64, unzip the linux version of the program then check that both GUI (i.e. gMetapop_GUI_Lin64.exe) and CORE (i.e. gMetapop_CORE_Lin64.exe), and associated library (i.e. lib*) files have executable rights (If needed, type chmod +x filename to allow for executable permissions). 
gMetapop_GUI is developed in C++ with open-source Qt libraries (https://www.qt.io). These are provided in the current distribution, but additional libraries might be needed if they are not already available from your OS, so please type first: 
> sudo apt-get install libxcb-xinerama0 >
Then in the folder where the files have been unzipped, type in command line:
> sh ./run_gM_GUI_Linux64.sh >
 or simply 
> ./ run_gM_GUI_Linux64.sh >
which should launch the GUI application. 

In case of installation problems, in particular for gMetapop_GUI under some Linux OS, please contact Frédéric Raspail (frederic.raspail@inrae.fr) or Pauline Garnier-Géré (pauline.garnier-gere@inrae.fr). If needed, we can also provide a *.exe* files for a 32-bit Windows OS. 
