$CDIR = Get-Location

# Build BOMs
Set-Location $CDIR\sarl-bom
mvn clean install

# Build SARL core modules
Set-Location $CDIR\sarl-baseutils
mvn clean install
Set-Location $CDIR\sarl-lang
mvn clean install
Set-Location $CDIR\sarl-sdk
mvn clean install
Set-Location $CDIR\sarl-apputils
mvn clean install
Set-Location $CDIR\sarl-sre
mvn clean install
Set-Location $CDIR\sarl-docs
mvn clean install
Set-Location $CDIR\sarl-cli
mvn clean install

# Build Eclipse tools
Set-Location $CDIR\sarl-eclipse
mvn clean install

# Build SARL Official Documentation
Set-Location $CDIR\sarl-officialdoc
mvn clean install

