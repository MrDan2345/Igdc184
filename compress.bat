@echo off
pushd %~dp0

for %%i in (*.exe) do (
	if not %%~ni == upx call upx.exe --best --ultra-brute %%i
)

popd