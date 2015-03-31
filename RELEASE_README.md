
RELEASE SARL
============

The steps for releasing SARL are:

1) Remove "-SNAPSHOT" in all the poms.

2) Update the versions in the Eclipse configurations:
   a) Remove ".qualifier" in the MANIFEST.MF files  (in Bundle-Version).
   b) Remove ".qualifier" in the feature.xml files (in root tag).
   c) Remove ".qualifier" in the *.product files (in root tag and feature tag).
   d) Remove ".qualifier" in the category.xml files (in feature tags, url and version).

3) Compiling locally without error.

    $> mvn clean install

4) Prepare the bundles for Maven Central:

    $> ./scripts/prepare-bundles-for-central

5) Tag the Git with the version number.

    $> git tag "vX.Y.Z"

6) Commit and push to Github:

    $> git commit
    $> git push --all

7) On Hudson, launch a build for updating the maven repositories and the Eclipse update sites.
   If failing, revert 5, fix the problem, and go back to 3.

8) Updload the Maven Bundle on Maven Central with [http://oss.sonatype.org](http://oss.sonatype.org)

9) Create the "Changes" page for the webiste.

10) Update the SARL website:

    $> cd path/to/sarl-site
    $> rake build_full
    $> rake transfer

11) Commit and push the website Gits.

12) Close the milestone on Github.

13) Add release notes on Github (from the Changes page on the website), attached to the release tag.

14) Revert steps 1 and 2; and change the following:
    * Version ranges in the Required-Bundles of MANIFEST.MF.
    * Versions in the requirements of feature.xml.

15) Compiling locally without error.

    $> mvn clean install

16) Commit and push to Github:

    $> git commit
    $> git push --all

17) On Hudson, launch a build for updating the maven repositories and the Eclipse update sites.

18) Announce the new version on the mailing lists.

