/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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

package io.sarl.docs.doclet2.html.summaries;

import java.nio.file.Path;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.SortedSet;

import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;

import jdk.javadoc.doclet.Reporter;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.DocletOptions;

/** Generate the type hierarchy for a package.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class PackageTreeSummaryGeneratorImpl extends AbstractTreeSummaryGenerator implements PackageTreeSummaryGenerator {

	private PackageElement packageElement;
	
	private SortedSet<TypeElement> packageTypes;

	/** Constructor.
	 */
	public PackageTreeSummaryGeneratorImpl() {
		super(Messages.PackageTreeSummaryGeneratorImpl_1);
	}

	@Override
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars(this.packageElement, this);
	}

	@Override
	public void generate(PackageElement packageElement, Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		this.packageElement = packageElement;
		this.packageTypes = getTypeRepository().getTypesInPackage(this.packageElement);
		setDefaultTitle(MessageFormat.format(Messages.PackageTreeSummaryGeneratorImpl_2, this.packageElement.getQualifiedName().toString()));
		generate(
				MessageFormat.format(Messages.PackageTreeSummaryGeneratorImpl_0, this.packageElement.getQualifiedName().toString()),
				getPathBuilder().packageTypeHierarchy(this.packageElement),
				cssStylesheets, jsScripts, environment, cliOptions, reporter);
	}

	@Override
	protected boolean isVisible(TypeElement type) {
		final TypeMirror tm = type.asType();
		for (final TypeElement type0 : this.packageTypes) {
			if (getEnvironment().getTypeUtils().isAssignable(type0.asType(), tm)) {
				return true;
			}
		}
		return false;
	}
	
}
