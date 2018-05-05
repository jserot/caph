rem Usage : setup_projects <caph_root> <examples_dir> <dotviewer> <pgmviewer>
@echo off
echo "Updating projects"
cd %2
for /d /r  %%i in (*) do (
  echo %%i
  cd %%i
  %1\bin\mkproject -platform win32 -caph %1 -dotviewer %3 -pgmviewer %4 %%~ni.proj
  cd ..\..\..
  )
echo.
