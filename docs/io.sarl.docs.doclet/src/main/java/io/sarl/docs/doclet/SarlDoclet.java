/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.docs.doclet;

import java.io.IOException;
import java.util.Arrays;

import com.sun.javadoc.AnnotationTypeDoc;
import com.sun.javadoc.ClassDoc;
import com.sun.javadoc.DocErrorReporter;
import com.sun.javadoc.LanguageVersion;
import com.sun.javadoc.PackageDoc;
import com.sun.javadoc.RootDoc;
import com.sun.tools.doclets.formats.html.AllClassesFrameWriter;
import com.sun.tools.doclets.formats.html.ClassUseWriter;
import com.sun.tools.doclets.formats.html.ConfigurationImpl;
import com.sun.tools.doclets.formats.html.DeprecatedListWriter;
import com.sun.tools.doclets.formats.html.FrameOutputWriter;
import com.sun.tools.doclets.formats.html.HelpWriter;
import com.sun.tools.doclets.formats.html.PackageFrameWriter;
import com.sun.tools.doclets.formats.html.PackageIndexFrameWriter;
import com.sun.tools.doclets.formats.html.PackageIndexWriter;
import com.sun.tools.doclets.formats.html.PackageTreeWriter;
import com.sun.tools.doclets.formats.html.ProfileIndexFrameWriter;
import com.sun.tools.doclets.formats.html.ProfilePackageFrameWriter;
import com.sun.tools.doclets.formats.html.ProfilePackageIndexFrameWriter;
import com.sun.tools.doclets.formats.html.SingleIndexWriter;
import com.sun.tools.doclets.formats.html.SourceToHTMLConverter;
import com.sun.tools.doclets.formats.html.SplitIndexWriter;
import com.sun.tools.doclets.formats.html.TreeWriter;
import com.sun.tools.doclets.internal.toolkit.AbstractDoclet;
import com.sun.tools.doclets.internal.toolkit.builders.AbstractBuilder;
import com.sun.tools.doclets.internal.toolkit.util.ClassTree;
import com.sun.tools.doclets.internal.toolkit.util.DocFile;
import com.sun.tools.doclets.internal.toolkit.util.DocPaths;
import com.sun.tools.doclets.internal.toolkit.util.DocletAbortException;
import com.sun.tools.doclets.internal.toolkit.util.IndexBuilder;
import com.sun.tools.doclets.internal.toolkit.util.Util;
import com.sun.tools.javac.jvm.Profile;

import io.sarl.docs.doclet.proxy.ProxyInstaller;
import io.sarl.docs.doclet.utils.Reflect;
import io.sarl.docs.doclet.utils.Utils;

/** SARL Doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public class SarlDoclet extends AbstractDoclet {

	private static final SarlDoclet SARL_DOCLET = new SarlDoclet();

	/** Constructor.
	 */
	public SarlDoclet() {
		final ConfigurationImpl config = new SarlConfiguration();
		this.configuration = config;
	}

	@Override
	public SarlConfiguration configuration() {
		return (SarlConfiguration) this.configuration;
	}

	/** Replies the number of arguments for the given option.
	 *
	 * @param option the option to test.
	 * @return number of arguments to option. Zero return means option not known. Negative value means
	 *     error occurred.
	 */
	public static int optionLength(String option) {
		return SARL_DOCLET.configuration.optionLength(option);
	}

	/** Validate the given options.
	 *
	 * @param options the options to validate, which their parameters.
	 * @param reporter the receiver of errors.
	 * @return the validation status.
	 */
	public static boolean validOptions(String[][] options, DocErrorReporter reporter) {
		return SARL_DOCLET.configuration.validOptions(options, reporter);
	}

	/** Start the SARL documentation generation based on the given root document.
	 *
	 * @param root the root document.
	 * @return the status.
	 */
	public static boolean start(RootDoc root) {
		return SARL_DOCLET.start(SARL_DOCLET, root);
	}

	/** Replies the language version supported by the Sarl documentation style.
	 *
	 * @return the supported version.
	 */
	public static LanguageVersion languageVersion() {
		return AbstractDoclet.languageVersion();
	}

	/** Replies if the doclet is valid.
	 *
	 * @param doclet the doclet to test.
	 * @return validity status.
	 */
	@SuppressWarnings("static-method")
	protected boolean isValidDoclet(AbstractDoclet doclet) {
		return doclet == SARL_DOCLET;
	}

	@SuppressWarnings("checkstyle:all")
	@Override
	public boolean start(AbstractDoclet doclet, RootDoc root) {
		configuration().root = configuration().getProxyInstaller().installProxies(root);
		if (!isValidDoclet(doclet)) {
			return false;
		}
		try {
			Reflect.callProc(this, AbstractDoclet.class, "startGeneration", //$NON-NLS-1$
					new Class[] { RootDoc.class },
					configuration().root);
		} catch (DocletAbortException e) {
			final Throwable cause = Utils.getCause(e);
			if (cause.getLocalizedMessage() != null) {
				configuration().root.printError(cause.getLocalizedMessage());
			} else {
				configuration().root.printError(cause.toString());
			}
			return false;
		} catch (Throwable exc) {
			Utils.getCause(exc).printStackTrace();
			return false;
		}
		return true;
	}

	private AnnotationTypeDoc asAnnotationTypeDoc(ClassDoc doc) {
		final ProxyInstaller proxyInstaller = configuration().getProxyInstaller();
		final ClassDoc current = proxyInstaller.unwrap(doc);
		AnnotationTypeDoc annotationTypeDoc = null;
		if (current instanceof AnnotationTypeDoc) {
			annotationTypeDoc = (AnnotationTypeDoc) current;
		}
		if (annotationTypeDoc == null) {
			annotationTypeDoc = current.asAnnotationTypeDoc();
		}
		return proxyInstaller.wrap(annotationTypeDoc);
	}

	@Override
	protected void generateClassFiles(ClassDoc[] classes, ClassTree classtree) {
		Arrays.sort(classes);
		for (int i = 0; i < classes.length; i++) {
			final ClassDoc current = classes[i];
			if (!(configuration().isGeneratedDoc(current) && current.isIncluded())) {
				continue;
			}
			final ClassDoc previous = i == 0 ? null : classes[i - 1];
			final ClassDoc next = (i + 1) == classes.length ? null : classes[i + 1];
			try {
				final AnnotationTypeDoc annotationTypeDoc =
						current.isAnnotationType() ? asAnnotationTypeDoc(current) : null;
				if (annotationTypeDoc != null) {
					final AbstractBuilder annotationTypeBuilder = configuration().getBuilderFactory()
							.getAnnotationTypeBuilder(annotationTypeDoc, previous, next);
					annotationTypeBuilder.build();
				} else {
					final AbstractBuilder classBuilder = configuration().getBuilderFactory()
							.getClassBuilder(current, previous, next, classtree);
					classBuilder.build();
				}
			} catch (IOException e) {
				throw new DocletAbortException(e);
			} catch (DocletAbortException de) {
				throw de;
			} catch (Exception e) {
				throw new DocletAbortException(e);
			}
		}
	}

	@Override
	protected void generatePackageFiles(ClassTree classtree) throws Exception {
		final PackageDoc[] packages = configuration().packages;
		if (packages.length > 1) {
            PackageIndexFrameWriter.generate(configuration());
        }
		PackageDoc prev = null;
		PackageDoc next;
		for (int i = 0; i < packages.length; i++) {
			// if -nodeprecated option is set and the package is marked as
			// deprecated, do not generate the package-summary.html, package-frame.html
			// and package-tree.html pages for that package.
			if (!(configuration().nodeprecated && Util.isDeprecated(packages[i]))) {
				PackageFrameWriter.generate(configuration(), packages[i]);
				next = (i + 1 < packages.length) && (packages[i + 1].name().length() > 0) ? packages[i + 1] : null;
				//If the next package is unnamed package, skip 2 ahead if possible
				next = (i + 2) < packages.length && (next == null) ? packages[i + 2] : next;
				final AbstractBuilder packageSummaryBuilder = configuration()
						.getBuilderFactory().getPackageSummaryBuilder(packages[i], prev, next);
				packageSummaryBuilder.build();
				if (configuration().createtree) {
					PackageTreeWriter.generate(configuration(),
							packages[i], prev, next,
							configuration().nodeprecated);
				}
				prev = packages[i];
			}
		}
	}

	@Override
	protected void generateProfileFiles() throws Exception {
		if (configuration().showProfiles && configuration().profilePackages.size() > 0) {
			ProfileIndexFrameWriter.generate(configuration());
			Profile prevProfile = null;
			Profile nextProfile;
			String profileName;
			for (int i = 1; i < configuration().profiles.getProfileCount(); i++) {
				profileName = Profile.lookup(i).name;
				// Generate profile package pages only if there are any packages
				// in a profile to be documented. The profilePackages map will not
				// contain an entry for the profile if there are no packages to be documented.
				if (!configuration().shouldDocumentProfile(profileName)) {
					continue;
				}
				ProfilePackageIndexFrameWriter.generate(configuration(), profileName);
				final PackageDoc[] packages = configuration().profilePackages.get(profileName);
				PackageDoc prev = null;
				PackageDoc next;
				for (int j = 0; j < packages.length; j++) {
					// if -nodeprecated option is set and the package is marked as
					// deprecated, do not generate the profilename-package-summary.html
					// and profilename-package-frame.html pages for that package.
					if (!(configuration().nodeprecated && Util.isDeprecated(packages[j]))) {
						ProfilePackageFrameWriter.generate(configuration(), packages[j], i);
						if ((j + 1 < packages.length)
							&& (packages[j + 1].name().length() > 0)) {
							next = packages[j + 1];
						} else {
							next = null;
						}
						final AbstractBuilder profilePackageSummaryBuilder = configuration()
								.getBuilderFactory().getProfilePackageSummaryBuilder(
										packages[j], prev, next, Profile.lookup(i));
						profilePackageSummaryBuilder.build();
						prev = packages[j];
					}
				}
				nextProfile = (i + 1) < configuration().profiles.getProfileCount() ? Profile.lookup(i + 1) : null;
				final AbstractBuilder profileSummaryBuilder = configuration()
						.getBuilderFactory().getProfileSummaryBuilder(
								Profile.lookup(i), prevProfile, nextProfile);
				profileSummaryBuilder.build();
				prevProfile = Profile.lookup(i);
			}
		}
	}

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	protected void generateOtherFiles(RootDoc root, ClassTree classtree) throws Exception {
		super.generateOtherFiles(root, classtree);
		if (configuration().linksource) {
			SourceToHTMLConverter.convertRoot(configuration(),
					root, DocPaths.SOURCE_OUTPUT);
		}

		if (configuration().topFile.isEmpty()) {
			configuration().standardmessage.error("doclet.No_Non_Deprecated_Classes_To_Document"); //$NON-NLS-1$
			return;
		}
		final boolean nodeprecated = configuration().nodeprecated;
		Utils.performCopy(configuration().helpfile, configuration());
		Utils.performCopy(configuration().stylesheetfile, configuration());
		// do early to reduce memory footprint
		if (configuration().classuse) {
			ClassUseWriter.generate(configuration(), classtree);
		}
		final IndexBuilder indexbuilder = new IndexBuilder(configuration(), nodeprecated);

		if (configuration().createtree) {
			TreeWriter.generate(configuration(), classtree);
		}
		if (configuration().createindex) {
			if (configuration().splitindex) {
				SplitIndexWriter.generate(configuration(), indexbuilder);
			} else {
				SingleIndexWriter.generate(configuration(), indexbuilder);
			}
		}

		if (!(configuration().nodeprecatedlist || nodeprecated)) {
			configuration().getProxyInstaller().noProxy(configuration(), () -> {
				DeprecatedListWriter.generate(configuration());
			});
		}

		AllClassesFrameWriter.generate(configuration(),
				new IndexBuilder(configuration(), nodeprecated, true));

		FrameOutputWriter.generate(configuration());

		if (configuration().createoverview) {
			PackageIndexWriter.generate(configuration());
		}
		if (configuration().helpfile.length() == 0 && !configuration().nohelp) {
			HelpWriter.generate(configuration());
		}
		// If a stylesheet file is not specified, copy the default stylesheet
		// and replace newline with platform-specific newline.
		DocFile file;
		if (configuration().stylesheetfile.length() == 0) {
			file = DocFile.createFileForOutput(configuration(), DocPaths.STYLESHEET);
			file.copyResource(DocPaths.RESOURCES.resolve(DocPaths.STYLESHEET), false, true);
		}
		file = DocFile.createFileForOutput(configuration(), DocPaths.JAVASCRIPT);
		file.copyResource(DocPaths.RESOURCES.resolve(DocPaths.JAVASCRIPT), true, true);
	}

}
