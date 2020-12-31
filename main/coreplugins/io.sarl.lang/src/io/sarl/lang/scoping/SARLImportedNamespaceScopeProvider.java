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

package io.sarl.lang.scoping;

import java.util.List;

import com.google.common.base.Strings;
import com.google.common.collect.Lists;
import com.google.inject.Singleton;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtend.core.scoping.XtendImportedNamespaceScopeProvider;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.impl.JvmDeclaredTypeImplCustom;
import org.eclipse.xtext.resource.EObjectDescription;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.resource.ISelectable;
import org.eclipse.xtext.scoping.impl.MultimapBasedSelectable;

/** This class is fixing the
 * <a href="https://github.com/sarl/sarl/issues/356">issue #356</a>.
 *
 * <p>This issue is due to a JvmDeclaredType with a null name.
 *
 * <p>This class provides a temporary workaround.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class SARLImportedNamespaceScopeProvider extends XtendImportedNamespaceScopeProvider {

	@Override
	protected ISelectable internalGetAllDescriptions(final Resource resource) {
		final List<IEObjectDescription> descriptions = Lists.newArrayList();
		for (final EObject content: resource.getContents()) {
			if (content instanceof JvmDeclaredType) {
				// Begin fixing of issue #356.
				final JvmDeclaredType type = (JvmDeclaredType) content;
				if (!Strings.isNullOrEmpty(type.getIdentifier())) {
					// End of fixing
					doGetAllDescriptions(type, descriptions);
				}
			}
		}
		return new MultimapBasedSelectable(descriptions);
	}

	private void doGetAllDescriptions(JvmDeclaredType type, List<IEObjectDescription> descriptions) {
		descriptions.add(EObjectDescription.create(getQualifiedNameConverter().toQualifiedName(type.getIdentifier()), type));
		final EList<JvmMember> members;
		if (type instanceof JvmDeclaredTypeImplCustom) {
			members = ((JvmDeclaredTypeImplCustom) type).basicGetMembers();
		} else {
			members = type.getMembers();
		}
		for (final JvmMember member: members) {
			if (member instanceof JvmDeclaredType) {
				// add nested types also with the dot delimiter
				descriptions.add(EObjectDescription.create(getQualifiedNameConverter().toQualifiedName(
						member.getQualifiedName('.')), member));
				doGetAllDescriptions((JvmDeclaredType) member, descriptions);
			}
		}
	}

}
