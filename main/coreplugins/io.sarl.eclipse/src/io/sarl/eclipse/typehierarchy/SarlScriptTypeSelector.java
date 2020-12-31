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

package io.sarl.eclipse.typehierarchy;

import javax.inject.Inject;

import com.google.common.base.Strings;
import org.eclipse.emf.common.util.URI;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.naming.IQualifiedNameProvider;

import io.sarl.eclipse.util.AbstractSarlScriptInteractiveSelector;

/** Abstract implementation of a shortcut for launching a SARL agent or SARL applications.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
class SarlScriptTypeSelector extends AbstractSarlScriptInteractiveSelector<XtendTypeDeclaration, Object> {

	@Inject
	private IQualifiedNameProvider nameProvider;

	@Override
	protected URI getResourceURIForValidEObject(Object object) {
		if (object instanceof XtendTypeDeclaration) {
			final XtendTypeDeclaration clazz = (XtendTypeDeclaration) object;
			return clazz.eResource().getURI();
		}
		return null;
	}

	@Override
	protected Class<XtendTypeDeclaration> getValidEObjectType() {
		return XtendTypeDeclaration.class;
	}

	@Override
	protected String getQualifiedNameFor(XtendTypeDeclaration element) {
		return this.nameProvider.getFullyQualifiedName(element).toString();
	}

	@Override
	protected boolean isSelectableElement(XtendTypeDeclaration element) {
		return element != null && !Strings.isNullOrEmpty(element.getName());
	}

	@Override
	protected String getElementLabel() {
		return Messages.SarlScriptTypeSelector_0;
	}

	@Override
	protected String getElementLongLabel() {
		return Messages.SarlScriptTypeSelector_2;
	}

	@Override
	protected String getElementsLabel() {
		return Messages.SarlScriptTypeSelector_1;
	}

	@Override
	protected Image getElementImage(Object element) {
		if (element != null) {
			final Image img = this.labelProvider.getImage(element);
			if (img != null) {
				return img;
			}
		}
		return JavaPluginImages.DESC_OBJS_UNKNOWN.createImage();
	}

}
