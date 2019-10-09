cd models/CC/AM2
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc
cd ../../HG/AM2
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc
cd ../../PRD/AM2
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc
cd ../../SoG/AM2
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc
cd ../../WCVI/AM2
if exist mcmc rd /s /q mcmc
md mcmc
iscam -mceval
mv *.csv mcmc
