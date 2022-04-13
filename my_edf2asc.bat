@echo off
cd D:\MATLAB\PupilSize\testEDF\
for %%c in (*.edf) do "D:\Program Files (x86)\SR Research\EyeLink\bin\64\edf2asc64.exe" -p D:\MATLAB\PupilSize\testASC\ -sp -sh -res -vel  %%c 