Buildong in MSVC: http://www.mathworks.com/access/helpdesk/help/techdoc/matlab_external/f24338.html
Debugging in MSVC: http://www.mathworks.com/access/helpdesk/help/techdoc/matlab_external/f32489.html#f18756



The FAR manager, version 1.70 (build 2087)
Copyright (C) 1996-2000 Eugene Roshal, Copyright (C) 2000-2006 FAR Group
Evaluation copy, please register.
C:\cvm\mex\vestnik>mex

Select a compiler:
[1] Microsoft Visual C++ 2008 SP1 in C:\Program Files (x86)\Microsoft Visual Studio 9.0
[2] Microsoft Visual C++ 2005 SP1 in C:\Program Files (x86)\Microsoft Visual Studio 8

[0] None

Compiler: 1

***************************************************************************
  Warning: MEX-files generated using Microsoft Visual C++ 2008 require
           that Microsoft Visual Studio 2008 run-time libraries be
           available on the computer they are run on.
           If you plan to redistribute your MEX-files to other MATLAB
           users, be sure that they have the run-time libraries.
***************************************************************************

Trying to update options file: C:\Documents and Settings\snikolaev\Application Data\MathWorks\MATLAB\R2009b\mexopts.bat
From template:              C:\PROGRA~1\MATLAB\R2009b\bin\win64\mexopts\msvc90opts.bat

Done . . .

**************************************************************************
  Warning: The MATLAB C and Fortran API has changed to support MATLAB
           variables with more than 2^32-1 elements.  In the near future
           you will be required to update your code to utilize the new
           API. You can find more information about this at:
           http://www.mathworks.com/support/solutions/data/1-5C27B9.html?solution=1-5C27B9
           Building with the -largeArrayDims option enables the new API.
**************************************************************************

    Usage:
        MEX [option1 ... optionN] sourcefile1 [... sourcefileN]
            [objectfile1 ... objectfileN] [libraryfile1 ... libraryfileN]

      or (to build an Ada S-function):
        MEX [-v] [-g] -ada <sfcn>.ads

    Use the -help option for more information, or consult the MATLAB API Guide.


  C:\PROGRA~1\MATLAB\R2009B\BIN\MEX.PL: Error: No file names given.


C:\cvm\mex\vestnik>







In this example, the MATLAB command prompt >> is shown in front of MATLAB commands, and linux> represents a Linux[1] prompt; your system may show a different prompt. The debugger prompt is <gdb>.

To compile the source MEX-file, type:
linux> mex -g timestwo.F


On a Linux 32–bit platform, this command creates the executable file timestwo.mexglx.

At the Linux prompt, start the gdb debugger using the matlab -D option:
linux> matlab -Dgdb



Start MATLAB without the Java Virtual Machine (JVM) by using the -nojvm startup flag:
<gdb> run -nojvm


In MATLAB, enable debugging with the dbmex function and run your binary MEX-file:
>> dbmex on
>> y = timestwo(4)


At this point, you are ready to start debugging.

It is often convenient to set a breakpoint at mexFunction so you stop at the beginning of the gateway routine.

Note    The function name may be slightly altered by the compiler (e.g., it may have an underscore appended). To determine how this symbol appears in a given MEX-file, use the Linux command nm. For example:
linux> nm timestwo.mexglx | grep -i mexfunction


The operating system responds with something like:
0000091c T mexfunction_


Use mexfunction_ in the breakpoint statement. Be sure to use the correct case.
<gdb> break mexfunction_
<gdb> continue


Once you hit one of your breakpoints, you can make full use of any commands the debugger provides to examine variables, display memory, or inspect registers.

To proceed from a breakpoint, type continue:
<gdb> continue


After stopping at the last breakpoint, type:
<gdb> continue


timestwo finishes and MATLAB displays:
y =

     8


From the MATLAB prompt you can return control to the debugger by typing:
>> dbmex stop


Or, if you are finished running MATLAB, type:
>> quit


When you are finished with the debugger, type:
<gdb> quit


You return to the Linux prompt.

Refer to the documentation provided with your debugger for more information on its use
