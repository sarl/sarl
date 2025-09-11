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

package io.sarl.docs.doclet2.html.summaries;

import java.nio.file.Path;
import java.util.Collection;
import java.util.TreeSet;

import javax.lang.model.element.TypeElement;

import jdk.javadoc.doclet.Reporter;
import org.jsoup.nodes.Element;

import io.sarl.docs.doclet2.framework.SarlDocletEnvironment;
import io.sarl.docs.doclet2.html.framework.DocPaths;
import io.sarl.docs.doclet2.html.framework.DocletOptions;

/** Generate the all-type summary.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.1 20250911-224827
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class AllTypeSummaryGeneratorImpl extends AbstractSummaryGenerator implements AllTypeSummaryGenerator {

	/** Constructor.
	 */
	public AllTypeSummaryGeneratorImpl() {
		super(Messages.AllTypeSummaryGeneratorImpl_1);
	}

	@Override
	protected void generateNavigationBar() {
		getNavigation().generateNavigationBars((TypeElement) null, this);
	}

	@Override
	public void generate(Collection<Path> cssStylesheets, Collection<Path> jsScripts, SarlDocletEnvironment environment, DocletOptions cliOptions, Reporter reporter) throws Exception {
		generate(
				Messages.AllTypeSummaryGeneratorImpl_0, DocPaths.ALL_TYPE_HTML,
				cssStylesheets, jsScripts, environment, cliOptions, reporter);
	}

	@Override
	protected void generateBodyContent(Element parent) {
		final var types = getTypeRepository().getTypes();
		if (!types.isEmpty()) {
			final TreeSet<TypeElement> stypes = new TreeSet<>(getElementUtils().getTypeElementBasenameComparator());
			stypes.addAll(types);
			final var list = getHtmlFactory().createUlTag(parent, null);
			for (final var type : stypes) {
				final var entry = getHtmlFactory().createLiTag(list, null);
				entry.appendChildren(getHtmlFactory().createTypeLink(type, true, null, this));
			}
		}
	}

}
