@echo off

setlocal
:PROMPT
SET /P CONTINUE=Do you want to run the SCA model (Y/N)?
IF /I "%CONTINUE%" NEQ "Y" GOTO END

@echo on

cd models/HG
iscam
iscam -mcmc 5000000 -mcsave 1000
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc

cd ../PRD
iscam
iscam -mcmc 5000000 -mcsave 1000
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc

cd ../CC
iscam
iscam -mcmc 5000000 -mcsave 1000
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc

cd ../SoG
iscam
iscam -mcmc 5000000 -mcsave 1000
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc

cd ../WCVI
iscam
iscam -mcmc 5000000 -mcsave 1000
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc

:END
endlocal