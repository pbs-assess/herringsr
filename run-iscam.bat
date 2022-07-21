@echo off

setlocal
:PROMPT
SET /P CONTINUE=Do you want to run the SCA model (Y/N)?
IF /I "%CONTINUE%" NEQ "Y" GOTO END

set /A num_mcmc = 20000
set /A nth_save = 10

@echo on

cd models/HG
if exist mcmc rd /s /q mcmc
if exist retrospectives rd /s /q retrospectives
if exist temp rd /s /q temp
md temp
mv *.ctl temp
mv *.dat temp
mv *.pfc temp
del /q *.*
mv temp/*.ctl .
mv temp/*.dat .
mv temp/*.pfc .
rd /s /q temp
iscam
iscam -mcmc %num_mcmc% -mcsave %nth_save%
iscam -mceval
md mcmc
mv *.csv mcmc

cd ../PRD
if exist mcmc rd /s /q mcmc
if exist retrospectives rd /s /q retrospectives
if exist temp rd /s /q temp
md temp
mv *.ctl temp
mv *.dat temp
mv *.pfc temp
del /q *.*
mv temp/*.ctl .
mv temp/*.dat .
mv temp/*.pfc .
rd /s /q temp
iscam
iscam -mcmc %num_mcmc% -mcsave %nth_save%
iscam -mceval
md mcmc
mv *.csv mcmc

cd ../CC
if exist mcmc rd /s /q mcmc
if exist retrospectives rd /s /q retrospectives
if exist temp rd /s /q temp
md temp
mv *.ctl temp
mv *.dat temp
mv *.pfc temp
del /q *.*
mv temp/*.ctl .
mv temp/*.dat .
mv temp/*.pfc .
rd /s /q temp
iscam
iscam -mcmc %num_mcmc% -mcsave %nth_save%
iscam -mceval
md mcmc
mv *.csv mcmc

cd ../SoG
if exist mcmc rd /s /q mcmc
if exist retrospectives rd /s /q retrospectives
if exist temp rd /s /q temp
md temp
mv *.ctl temp
mv *.dat temp
mv *.pfc temp
del /q *.*
mv temp/*.ctl .
mv temp/*.dat .
mv temp/*.pfc .
rd /s /q temp
iscam
iscam -mcmc %num_mcmc% -mcsave %nth_save%
iscam -mceval
md mcmc
mv *.csv mcmc

cd ../WCVI
if exist mcmc rd /s /q mcmc
if exist retrospectives rd /s /q retrospectives
if exist temp rd /s /q temp
md temp
mv *.ctl temp
mv *.dat temp
mv *.pfc temp
del /q *.*
mv temp/*.ctl .
mv temp/*.dat .
mv temp/*.pfc .
rd /s /q temp
iscam
iscam -mcmc %num_mcmc% -mcsave %nth_save%
iscam -mceval
md mcmc
mv *.csv mcmc

:END
endlocal