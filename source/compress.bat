@echo off
pushd %~dp0

for %%i in (../release/*.exe) do (
	if not %%~ni == upx call upx.exe --best --ultra-brute ../release/%%i
)

popd