#!/bin/bash

exec mvn -Dmaven.test.skip=true -Dcheckstyle.skip=true clean deploy -PuploadP2Repo
