/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.sarl.lang.findreferences;

import java.util.Collections;
import java.util.Set;

import com.google.inject.Inject;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.findReferences.IReferenceFinder;
import org.eclipse.xtext.findReferences.TargetURIs;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;

/** Helper for finding declarator data.
 *
 * <p>This class is adapted from the Xtend private API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class Declarators {

	private static final TargetURIs.Key<DeclaratorsData> KEY = TargetURIs.Key.from("SARL", DeclaratorsData.class); //$NON-NLS-1$

	@Inject
	private IQualifiedNameConverter nameConverter;

	/** Replies the data for the declarator.
	 *
	 * @param targetURIs - the URIs of the targets.
	 * @param resourceAccess - the definition of the resource accesses.
	 * @return the data for the declarators.
	 */
	public DeclaratorsData getDeclaratorData(
			final TargetURIs targetURIs,
			IReferenceFinder.IResourceAccess resourceAccess) {
		DeclaratorsData result = targetURIs.getUserData(KEY);
		if (result != null) {
			return result;
		}
		final Set<QualifiedName> declaratorNames = CollectionLiterals.newHashSet();
		for (final URI uri : targetURIs.getTargetResourceURIs()) {
			resourceAccess.readOnly(uri, new IUnitOfWork<Object, ResourceSet>() {
				@SuppressWarnings("synthetic-access")
				@Override
				public Object exec(ResourceSet state) throws Exception {
					for (URI objectURI : targetURIs.getEObjectURIs(uri)) {
						EObject object = state.getEObject(objectURI, true);
						if (object != null) {
							JvmType type = EcoreUtil2.getContainerOfType(object, JvmType.class);
							if (type != null) {
								declaratorNames.add(Declarators.this.nameConverter.toQualifiedName(
										type.getIdentifier()).toLowerCase());
								declaratorNames.add(Declarators.this.nameConverter.toQualifiedName(
										type.getQualifiedName('.')).toLowerCase());
							}
						}
					}
					return null;
				}

			});
		}
		result = new DeclaratorsData(declaratorNames);
		targetURIs.putUserData(KEY, result);
		return result;
	}

	/*** The data related to the type reference declarator.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class DeclaratorsData {

		private final Set<QualifiedName> declaratorNames;

		/**
		 * @param declaratorNames the names declared in the declarator.
		 */
		protected DeclaratorsData(Set<QualifiedName> declaratorNames) {
			this.declaratorNames = declaratorNames;
		}

		/** Replies the declarator names.
		 *
		 * @return the declarator names.
		 */
		public Set<QualifiedName> getDeclaratorNames() {
			return Collections.unmodifiableSet(this.declaratorNames);
		}

	}

}
