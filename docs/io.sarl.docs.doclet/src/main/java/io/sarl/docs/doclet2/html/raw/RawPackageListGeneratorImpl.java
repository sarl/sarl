/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2022 the original authors or authors.
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

package io.sarl.docs.doclet2.html.raw;

import java.nio.file.Path;
import java.text.MessageFormat;

import javax.lang.model.element.PackageElement;
import javax.tools.Diagnostic.Kind;

import com.google.common.base.Strings;
import jdk.javadoc.doclet.Reporter;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.AbstractDocumentationGenerator;
import io.sarl.docs.doclet2.html.framework.DocletOptions;
import io.sarl.docs.doclet2.html.framework.Navigation;
import io.sarl.docs.doclet2.html.framework.Navigation.NavigationKind;

/** Generate the raw list of packages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class RawPackageListGeneratorImpl extends AbstractDocumentationGenerator implements RawPackageListGenerator {

	public RawPackageListGeneratorImpl() {
		//
	}

	@Override
	protected String getDocumentTitleFor(String elementName) {
		return "";
	}

	@Override
	protected void initNavigation(Navigation navigation) {
		navigation.setKind(NavigationKind.INDEX);
	}

	@Override
	public void generate(SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		initGenerator(null, null, reporter, environment, cliOptions);
		getReporter().print(Kind.NOTE, Messages.RawPackageListGeneratorImpl_0);
		computePaths(getPathBuilder().rawPackageList(), false);
		//
		final Path outputPath = getCliOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final StringBuilder content = new StringBuilder();
		for (final PackageElement packageElement : getTypeRepository().getPackages()) {
			final String qn = packageElement.getQualifiedName().toString();
			if (!Strings.isNullOrEmpty(qn)) {
				content.append(qn);
				content.append("\n"); //$NON-NLS-0$
			}
		}
		//
		if (!getCliOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.RawPackageListGeneratorImpl_1, outputPath.toString()));
			writeDocument(outputPath, content.toString());
		}
	}

}
