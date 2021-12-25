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

package io.sarl.docs.doclet2.framework;

import java.util.Set;

import javax.inject.Inject;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.Elements;
import javax.lang.model.util.Types;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject.Kind;

import com.google.common.collect.Sets;
import com.sun.source.util.DocTrees;
import jdk.javadoc.doclet.DocletEnvironment;

/** Environment for the SARL doclet.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class SarlDocletEnvironmentImpl implements SarlDocletEnvironment {

	private ApidocExcluder excluder;

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
	public Set<? extends Element> getSpecifiedElements() {
		return Sets.filter(getParent().getSpecifiedElements(), it -> {
			return ! getApidocExcluder().isExcluded(it);
		});
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
		return getParent().isIncluded(e);
	}

	@Override
	public boolean isSelected(Element e) {
		if (getApidocExcluder().isExcluded(e)) {
			return false;
		}
		return getParent().isSelected(e);
	}

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
