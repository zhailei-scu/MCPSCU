@echo off
rem In cygwin, the linker is VC's link.exe, so the c compiler should also be cl.exe(based on our test, the gcc's compiler result can
rem not compatible with link.exe), to use the cl.exe in cygwin, we should include some search path such as  C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\INCLUDE and 
rem C:\Program Files (x86)\Windows Kits\10\include\10.0.14393.0\ucrt are necessary, to got these path, we should call the batch file "%VS140COMNTOOLS%vsvars32.bat" to generate some 
rem environment vars such as %INCLUDE%.
rem However, the dos's generated environment vars cannot be inheritted in cygwin shell if we call the batch file "%VS140COMNTOOLS%vsvars32.bat" 
rem in cygwin shell. So, we need some strategies to get these environment vars in cygwin shell. There are two ways:
rem (method 1) Based on our test, dos generates environments cannot be inheritted by cygwin shell during shell is running, but if we called dos batch file before cygwin shell is created, 
rem            it can be inheritted by cygwin shell.
rem            we create an dos .bat file and write:
rem                          call "%VS140COMNTOOLS%vsvars32.bat"
rem                          C:\PROGRA~1\PGICE\win64\18.4\pgi.bat
rem            in the .bat file and execute .bat in windows environment. This way would call "%VS140COMNTOOLS%vsvars32.bat" in dos, and a new PGI CYGWIN is called by dos,
rem            so the pgi.bat can inhert the vsvars32.bat generated environment vars. So the new created pgiCE environment inhert the environment vars %INCLUDE%, so the cl.exe
rem            can find it's head file succuessfully.
rem (method 2) currently, we use method 2, we creat an dos .bat file GetWinEnv.dat :
rem                          @echo off
rem                          call "%VS140COMNTOOLS%vsvars32.bat"
rem                          set winEnvFile=WinEnv.txt
rem                          if exist %winEnvFile% (
rem                             del %winEnvFile%
rem                           )
rem                          echo Include dirs are got   %INCLUDE%
rem                          echo %INCLUDE% > WinEnv.txt
rem           This simple dos bat file first call "%VS140COMNTOOLS%vsvars32.bat", then in dos environment get the environment %INCLUDE% and output to an txt file WinEnv.txt.
rem           So, although the dos batch file generated environments cannot be inheritted by cygwin if we call dos batch file in cygwin shell, 
rem           but we writted them to file and the cygwin shell would read them from the file. 

call "%VS140COMNTOOLS%vsvars32.bat"
set winEnvFile=WinEnv.txt
if exist %winEnvFile% (
	del %winEnvFile%
)

echo Include dirs are got   %INCLUDE%

echo %INCLUDE% > WinEnv.txt
