IF EXIST C:\R\R-2.14.1\bin\x64\Rcmd.exe C:\R\R-2.14.1\bin\x64\Rcmd.exe build ReadGenea
IF EXIST C:\R2141\bin\x64\Rcmd.exe C:\R2141\bin\x64\Rcmd.exe build ReadGenea
pause
IF EXIST C:\R\R-2.14.1\bin\x64\Rcmd.exe C:\R\R-2.14.1\bin\x64\Rcmd.exe INSTALL ReadGenea
IF EXIST C:\R2141\bin\x64\Rcmd.exe C:\R2141\bin\x64\Rcmd.exe INSTALL ReadGenea
pause
IF EXIST C:\R\R-2.14.1\bin\x64\Rcmd.exe C:\R\R-2.14.1\bin\x64\Rcmd.exe INSTALL --build ReadGenea
IF EXIST C:\R2141\bin\x64\Rcmd.exe C:\R2141\bin\x64\Rcmd.exe INSTALL --build ReadGenea
pause
