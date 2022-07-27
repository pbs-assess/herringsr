@echo off

setlocal
:PROMPT
SET /P CONTINUE=Do you want to run the SCA model (Y/N)?
IF /I "%CONTINUE%" NEQ "Y" GOTO END

set /A num_mcmc = 20000
set /A nth_save = 10
set "list=HG PRD CC SoG WCVI"

cd models

for /D %%d in (%list%) do (
  cd %%d
  echo Running SCA model in %cd%\%%d
  if exist mcmc rd /s /q mcmc
  if exist retrospectives rd /s /q retrospectives
  if exist temp rd /s /q temp
  md temp
  mv *.ctl temp
  mv *.dat temp
  mv *.pfc temp
  mv iscam.exe temp
  del /q *.*
  mv temp/*.ctl .
  mv temp/*.dat .
  mv temp/*.pfc .
  mv temp/iscam.exe .
  rd /s /q temp
  iscam
  iscam -mcmc %num_mcmc% -mcsave %nth_save%
  iscam -mceval
  md mcmc
  mv *.csv mcmc
  cd ..
)

:END
endlocal