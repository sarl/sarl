param ([String[]] $mavenargs)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Run_Mvn {
	param ([String] $path, [String] $label, [String[]] $additionalargs)
	Write-Host "[INFO]"
	Write-Host "[INFO] ================================================================================="
	Write-Host "[INFO] Compiling $label"
	Write-Host "[INFO] ================================================================================="
	Write-Host "[INFO]"
	Set-Location "$path"
	mvn clean install $additionalargs
	if ($LastExitCode -ne 0) {
		exit $LastExitCode
	}
}

$CDIR = Get-Location

# Show version
mvn "--version"

Run_Mvn "$CDIR\api" "API and SDK for BSPL in SARL" $mavenargs

Run_Mvn "$CDIR\lang" "BSPL language tools" $mavenargs

Run_Mvn "$CDIR\ui" "BSPL editor" $mavenargs

