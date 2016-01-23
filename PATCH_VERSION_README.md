
PATH A SARL RELEASE
===================

Let consider version x.y.z of SARL contains a bug that cannot wait the
next release version for being fixed.

Let assume to create patched version x.y.(z+1).

The steps for releasing SARL with the patch are:

A) PHASE 1: CREATE THE PATCH

A.1) Create a specific branch for the patch

     $> git create branch x.y.(z+1)
     $> git checkout x.y.(z+1)

A.2) Go back in history.

     $> git reset --hard x.y.z

A.3) Delete the commit after the version to patch.

     $> git clean -df
     $> git checkout -- .

A.4) Change the version number to x.y.(z+1).

A.5) Apply the patch

A.6) Compiling locally without error.

     $> rm -rf $HOME/.m2/repository
     $> mvn clean install

     You may compile the documentation in a different process if Maven
     complains to be out of memory.

     $> cd docs/io.sarl.docs.suite
     $> mvn clean install

A.7) Do manual testing. If a manual test is failing: fix the problem, and go to step A.5.

A.8) Test the generation of the  bundles for Maven Central:

     $> ./scripts/prepare-bundles-for-central

A.9) Commit and push to Github:

     $> git commit
     $> git push --all

A.10) Tag the Git with the version number.

     $> git tag "vX.Y.(Z+1)"
     $> git push --tags

A.11) On Hudson, launch a build for updating the maven repositories and
     the Eclipse update sites.
     If failing, fix the problem, and go back to A.5.

B) PHASE 2: DISSEMINATION OF THE RELEASE VERSION

B.1) Updload the Maven Bundle on Maven Central with
     [http://oss.sonatype.org](http://oss.sonatype.org)

B.2) Create the "Changes" page for the website, and add a link to the
     "Changes" page of the previous website inside.
     In this way, it will be possible to following the change history
     from the ealier to older changes.

B.3) Synchronize the
     [Awesome SARL project](https://github.com/sarl/awesome-sarl)
     with the "Community" page of the website.

B.4) Add a "News" in the SARL website.

B.5) Update the SARL website:

     $> cd path/to/sarl-site
     $> rake build_full
     $> rake transfer

B.6) Commit and push the website Gits.

B.7) Move all the remaining issues on Github to the following version.

B.8) Close the released milestone on Github.

B.9) Add release notes on Github (from the Changes page on the website),
     attached to the release tag.

D) PHASE 3: MERGE WITH MASTER

D.1) Merge the patch branch into the master.

D.2) Delete the branch.


