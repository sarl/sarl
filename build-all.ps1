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

# Build BOMs
Run_Mvn "$CDIR\sarl-bom" "build of materials" $mavenargs

# Build SARL core modules
Run_Mvn "$CDIR\sarl-baseutils" "base utilities" $mavenargs
Run_Mvn "$CDIR\sarl-lang" "SARL language tools" $mavenargs
Run_Mvn "$CDIR\sarl-sdk" "SARL Standard Development Kit - SDK" $mavenargs
Run_Mvn "$CDIR\sarl-apputils" "SARL application utilities" $mavenargs
Run_Mvn "$CDIR\sarl-sre" "SARL Runtime Environment - SRE" $mavenargs
Run_Mvn "$CDIR\sarl-docs" "documentation tools and doclets" $mavenargs
Run_Mvn "$CDIR\sarl-cli" "shell command-line tools" $mavenargs

# Build Eclipse tools
Run_Mvn "$CDIR\sarl-eclipse" "Eclipse-based tools and development environment" $mavenargs

# Build SARL Official Documentation
Run_Mvn "$CDIR\sarl-officialdoc" "SARL official documentation (Markdown)" $mavenargs

