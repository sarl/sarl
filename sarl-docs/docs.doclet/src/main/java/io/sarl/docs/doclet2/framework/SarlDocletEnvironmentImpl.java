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

package io.sarl.docs.doclet2.framework;

import java.util.Set;

import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject.Kind;

import com.google.common.collect.Sets;
import com.google.inject.Inject;
import com.sun.source.util.DocTrees;

import io.sarl.docs.doclet2.html.framework.DocletOptions;
import jdk.javadoc.doclet.DocletEnvironment;

/** Environment for the SARL doclet.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.doclet 0.15.1 20250911-224827
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.doclet
 * @since 0.13
 */
public class SarlDocletEnvironmentImpl implements SarlDocletEnvironment {

	private ApidocExcluder excluder;

	private DocletOptions docletOptions;

	private DocletEnvironment parent;

	@Override
	public void setParent(DocletEnvironment parent) {
		this.parent = parent;
	}

	@Override
	public DocletEnvironment getParent() {
		return this.parent;
	}

	@Override
	@Inject
	public void setApidocExcluder(ApidocExcluder excluder) {
		this.excluder = excluder;
	}

	@Override
	public ApidocExcluder getApidocExcluder() {
		return this.excluder;
	}

	@Override
	@Inject
	public void setDocletOptions(DocletOptions options) {
		this.docletOptions = options;
	}

	@Override
	public DocletOptions getDocletOptions() {
		return this.docletOptions;
	}

	@Override
	public Set<? extends Element> getSpecifiedElements() {
		return Sets.filter(getParent().getSpecifiedElements(),
				it -> !getApidocExcluder().isExcluded(it)
					&& (getDocletOptions().isDeprecatedFeaturesEnabled()
						|| !getElementUtils().isDeprecated(it)));
	}

	@Override
	public Set<? extends Element> getIncludedElements() {
		return Sets.filter(getParent().getIncludedElements(), it -> {
			return ! getApidocExcluder().isExcluded(it);
		});
	}

	@Override
	public DocTrees getDocTrees() {
		return getParent().getDocTrees();
	}

	@Override
	public Elements getElementUtils() {
		return getParent().getElementUtils();
	}

	@Override
	public Types getTypeUtils() {
		return getParent().getTypeUtils();
	}

	@Override
	public boolean isIncluded(Element e) {
		if (getApidocExcluder().isExcluded(e)) {
			return false;
		}
		if (!getDocletOptions().isDeprecatedFeaturesEnabled() && getElementUtils().isDeprecated(e)) {
			return false;
		}
		return getParent().isIncluded(e);
	}

	@Override
	public boolean isSelected(Element e) {
		if (getApidocExcluder().isExcluded(e)) {
			return false;
		}
		if (!getDocletOptions().isDeprecatedFeaturesEnabled() && getElementUtils().isDeprecated(e)) {
			return false;
		}
		return getParent().isSelected(e);
	}

	@SuppressWarnings("resource")
	@Override
	public JavaFileManager getJavaFileManager() {
		return getParent().getJavaFileManager();
	}

	@Override
	public SourceVersion getSourceVersion() {
		return getParent().getSourceVersion();
	}

	@Override
	public ModuleMode getModuleMode() {
		return getParent().getModuleMode();
	}

	@Override
	public Kind getFileKind(TypeElement type) {
		return getParent().getFileKind(type);
	}

}
