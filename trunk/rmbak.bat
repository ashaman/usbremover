@echo off

rem Маски для временных файлов
set TFILES=*.~* *.dcu *.ppu *.obj *.dsk *.bk? *.mps ^
*.rst *.s *.a *.bak *.ppw *.ppl *.o *.or *.so *.dll ^
*.res *.lrs *.pdb *.exe *.ncb *.ilk *.manifest ^
*.compiled *.idb *.dbg *.dep *.user *.exp *.lib ^
BuildLog.htm

set curdir=%CD%
call :CleanRecursive %1
cd %curdir% > nul

exit /b

:CleanRecursive

rem Меняем каталог на каталог, в котором
rem находится командный файл
cd %~dp0 || exit /b 1

echo Current directory is %CD%

rem Рекурсивный поиск всех папок и файлов,
rem подходящих по маске и их удаление
for /R %%i in (.) do (
	echo %%i
	set old=%CD%
	cd %%i > nul

	rem Удаление всех директорий backup
	if exist backup (
		echo		Deleting directory %%i\backup
		rmdir backup /s /q
	)

	if exist "Debug" (
		echo		Deleting directory %%i\debug
		rmdir "Debug" /s /q
	)

	if exist "Release" (
		echo		Deleting directory %%i\release
		rmdir "Release" /s /q
	)

	rem Удаление всех файлов по маске
	for %%i in (%TFILES%) do (
		if exist %%i (
			echo		Deleting file: %%i
			del %%i
		)	
	)
	cd %old% > nul
)

exit /b 0