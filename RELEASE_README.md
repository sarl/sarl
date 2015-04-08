
RELEASE SARL
============

The steps for releasing SARL are:

A) PHASE 1: RELEASE CANDIDATE VERSION

A.1) Upgrade all the versions in the pom files and the Eclipse platform.

A.2) Compiling locally without error.

     $> rm -rf $HOME/.m2/repository
     $> mvn clean install

     You may compile the documentation is a different process if Maven
     complains to be out of memory.

     $> cd docs/io.sarl.docs.suite
     $> mvn clean install

A.3) Commit all the changes.

A.4) Tag the version with "vX.Y.Z-rcN" (where "N" is the release candidate
     number). And push the tag on Github.

A.3) Do manual tests.

A.5) If a manual test is failing: fix the problem, and go to step A.2.

B) PHASE 2: RELEASE VERSION

B.1) Remove "-SNAPSHOT" in all the poms.

B.2) Update the versions in the Eclipse configurations:
     a) Remove ".qualifier" in the MANIFEST.MF files
        (in Bundle-Version).
     b) Remove ".qualifier" in the feature.xml files
        (in root tag).
     c) Remove ".qualifier" in the *.product files
        (in root tag and feature tag).
     d) Remove ".qualifier" in the category.xml files
        (in feature tags, url and version).

B.3) Update the graphical resources (splash screen, icons...)

B.4) Compiling locally without error.

     $> rm -rf $HOME/.m2/repository
     $> mvn clean install

     You may compile the documentation in a different process if Maven
     complains to be out of memory.

     $> cd docs/io.sarl.docs.suite
     $> mvn clean install

B.5) Prepare the bundles for Maven Central:

     $> ./scripts/prepare-bundles-for-central

B.6) Commit and push to Github:

     $> git commit
     $> git push --all

B.7) Tag the Git with the version number.

     $> git tag "vX.Y.Z"
     $> git push --tags

B.8) On Hudson, launch a build for updating the maven repositories and
     the Eclipse update sites.
     If failing, revert B.7, fix the problem, and go back to B.4.

C) PHASE 3: DISSEMINATION OF THE RELEASE VERSION

C.1) Updload the Maven Bundle on Maven Central with
     [http://oss.sonatype.org](http://oss.sonatype.org)

C.2) Create the "Changes" page for the website, and add a link to the
     "Changes" page of the previous website inside.
     In this way, it will be possible to following the change history
     from the ealier to older changes.

C.3) Synchronize the
     [Awesome SARL project](https://github.com/sarl/awesome-sarl)
     with the "Community" page of the website.

C.4) Add a "News" in the SARL website.

C.5) Update the SARL website:

     $> cd path/to/sarl-site
     $> rake build_full
     $> rake transfer

C.6) Commit and push the website Gits.

C.7) Move all the remaining issues on Github to the following version.

C.8) Close the released milestone on Github.

C.9) Add release notes on Github (from the Changes page on the website),
     attached to the release tag.

C.10) Announce the new version of SARL on the mailing lists.

D) PHASE 4: DEVELOPMENT VERSION

D.1) Revert steps B.1 to B.3; and change the following:
     * Version ranges in the Required-Bundles of MANIFEST.MF.
     * Versions in the requirements of feature.xml.

D.2) Compiling locally without error.

     $> rm -rf $HOME/.m2/repository
     $> mvn clean install

     You may compile the documentation in a different process if Maven
     complains to be out of memory.

     $> cd docs/io.sarl.docs.suite
     $> mvn clean install

D.3) Commit and push to Github:

     $> git commit
     $> git push --all

D.4) On Hudson, launch a build for updating the maven repositories and
     the Eclipse update sites.


