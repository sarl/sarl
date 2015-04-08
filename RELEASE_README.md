
RELEASE SARL
============

The steps for releasing SARL are:

1) Remove "-SNAPSHOT" in all the poms.

2) Update the versions in the Eclipse configurations:
   a) Remove ".qualifier" in the MANIFEST.MF files  (in Bundle-Version).
   b) Remove ".qualifier" in the feature.xml files (in root tag).
   c) Remove ".qualifier" in the *.product files (in root tag and feature tag).
   d) Remove ".qualifier" in the category.xml files (in feature tags, url and version).

3) Update the graphical resources (splash screen, icons...)

4) Compiling locally without error.

    $> mvn clean install

    You may compile the documentation is a different process if Maven complains to be out of memory.

    $> cd io.sarl.docs/io.sarl.docs.suite
    $> mvn clean install

5) Prepare the bundles for Maven Central:

    $> ./scripts/prepare-bundles-for-central

6) Tag the Git with the version number.

    $> git tag "vX.Y.Z"

7) Commit and push to Github:

    $> git commit
    $> git push --all

8) On Hudson, launch a build for updating the maven repositories and the Eclipse update sites.
   If failing, revert 6, fix the problem, and go back to 4.

9) Updload the Maven Bundle on Maven Central with [http://oss.sonatype.org](http://oss.sonatype.org)

10) Create the "Changes" page for the webiste.

11) Update the SARL website:

    $> cd path/to/sarl-site
    $> rake build_full
    $> rake transfer

12) Commit and push the website Gits.

13) Close the milestone on Github.

14) Add release notes on Github (from the Changes page on the website), attached to the release tag.

15) Revert steps 1 to 3; and change the following:
    * Version ranges in the Required-Bundles of MANIFEST.MF.
    * Versions in the requirements of feature.xml.

16) Compiling locally without error.

    $> mvn clean install

    You may compile the documentation is a different process if Maven complains to be out of memory.

    $> cd io.sarl.docs/io.sarl.docs.suite
    $> mvn clean install

17) Commit and push to Github:

    $> git commit
    $> git push --all

18) On Hudson, launch a build for updating the maven repositories and the Eclipse update sites.

19) Announce the new version on the mailing lists.

