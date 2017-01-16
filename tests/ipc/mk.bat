rem mk.bat --
rem Quick and dirty: compile and link ipc-mmap

if /%1 == /MSVC goto :msvc
if /%1 == /msvc goto :msvc

echo Using gfortran and gcc ...
gcc -c ..\..\src\ipc\ipc_mmap_c.c -DMINGW -DFTN_UNDERSCORE
gfortran -c ..\..\src\ipc\ipc_mmap.f90
gfortran -o test_ipc_mmap test_ipc_mmap.f90 ipc_mmap.o ipc_mmap_c.o

goto end

echo Using MSVC and CVF ...
:msvc
cl /c ..\..\src\ipc\ipc_mmap_c.c -DFTN_ALLCAPS -DINBETWEEN
df /c ..\..\src\ipc\ipc_mmap.f90
df /c test_ipc_mmap.f90
link /out:test_ipc_mmap.exe test_ipc_mmap.obj ipc_mmap.obj ipc_mmap_c.obj

:end
