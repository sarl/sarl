/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
 *
 *------- FORKED SOURCE CODE:
 *
 * THIS CODE IS FORKED FROM JDK.JAVADOC INTERNAL PACKAGE AND ADAPTED TO THE SARL PURPOSE.
 * THE FORK WAS NECESSARY BECAUSE IT IS IMPOSSIBLE TO SUBCLASS THE TYPES FOR THE.
 * STANDARD HTML DOCLET THAT IS PROVIDED BY JDK.JAVADOC MODULE.
 *
 * Copyright (c) 2003, 2021, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package io.sarl.docs.doclet2.html.raw;

import java.text.MessageFormat;

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
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class RawPackageListGeneratorImpl extends AbstractDocumentationGenerator implements RawPackageListGenerator {

	/** Constructor.
	 */
	public RawPackageListGeneratorImpl() {
		//
	}

	@Override
	protected String getDocumentTitleFor(String elementName) {
		return ""; //$NON-NLS-1$
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
		final var outputPath = getDocletOptions().getOutputDirectory().resolve(getRelativePath());
		//
		final var content = new StringBuilder();
		for (final var packageElement : getTypeRepository().getPackages()) {
			final var qn = packageElement.getQualifiedName().toString();
			if (!Strings.isNullOrEmpty(qn)) {
				content.append(qn);
				content.append("\n"); //$NON-NLS-1$
			}
		}
		//
		if (!getDocletOptions().isFakeOutput()) {
			getReporter().print(Kind.NOTE, MessageFormat.format(Messages.RawPackageListGeneratorImpl_1, outputPath.toString()));
			writeDocument(outputPath, content.toString());
		}
	}

}
