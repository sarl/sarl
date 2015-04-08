
RELEASE SARL
============

The steps for releasing SARL are:

A) PHASE 1: RELEASE VERSION

A.1) Remove "-SNAPSHOT" in all the poms.

A.2) Update the versions in the Eclipse configurations:
   a) Remove ".qualifier" in the MANIFEST.MF files  (in Bundle-Version).
   b) Remove ".qualifier" in the feature.xml files (in root tag).
   c) Remove ".qualifier" in the *.product files (in root tag and feature tag).
   d) Remove ".qualifier" in the category.xml files (in feature tags, url and version).

A.3) Update the graphical resources (splash screen, icons...)

A.4) Compiling locally without error.

    $> rm -rf $HOME/.m2/repository
    $> mvn clean install

    You may compile the documentation is a different process if Maven complains to be out of memory.

    $> cd docs/io.sarl.docs.suite
    $> mvn clean install

A.5) Prepare the bundles for Maven Central:

    $> ./scripts/prepare-bundles-for-central

A.6) Tag the Git with the version number.

    $> git tag "vX.Y.Z"

A.7) Commit and push to Github:

    $> git commit
    $> git push --all

A.8) On Hudson, launch a build for updating the maven repositories and the Eclipse update sites.
   If failing, revert 6, fix the problem, and go back to 4.

A.9) Updload the Maven Bundle on Maven Central with [http://oss.sonatype.org](http://oss.sonatype.org)

A.10) Create the "Changes" page for the website, and add a link to the "Changes" page of the previous website inside.
    In this way, it will be possible to following the change history from the ealier to older changes.

A.11) Synchronize the [Awesome SARL project](https://github.com/sarl/awesome-sarl) with the "Community" page of the website.

A.12) Update the SARL website:

    $> cd path/to/sarl-site
    $> rake build_full
    $> rake transfer

A.13) Commit and push the website Gits.

A.14) Move all the remaining issues on Github to the following version.

A.15) Close the released milestone on Github.

A.16) Add release notes on Github (from the Changes page on the website), attached to the release tag.

B) PHASE 2: NEW SNAPSHOT VERSION

B.1) Revert steps 1 to 3; and change the following:
    * Version ranges in the Required-Bundles of MANIFEST.MF.
    * Versions in the requirements of feature.xml.

B.2) Compiling locally without error.

    $> rm -rf $HOME/.m2/repository
    $> mvn clean install

    You may compile the documentation is a different process if Maven complains to be out of memory.

    $> cd docs/io.sarl.docs.suite
    $> mvn clean install

B.3) Commit and push to Github:

    $> git commit
    $> git push --all

B.4) On Hudson, launch a build for updating the maven repositories and the Eclipse update sites.

C) PHASE 3: DISSEMINATION

C.1) Announce the new version of SARL on the mailing lists.

