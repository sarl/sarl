/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.findReferences.IReferenceFinder;
import org.eclipse.xtext.findReferences.TargetURIs;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

import io.sarl.lang.sarl.SarlPackage;

/** Helper for finding declarator data.
 *
 * <p>This class is adapted from the Xtend private API.
 * FIXME: Remove when https://bugs.eclipse.org/bugs/show_bug.cgi?id=484877 is fixed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Declarators {

	/**
	 * <p>This class is adapted from the Xtend private API.
	 * FIXME: Remove when https://bugs.eclipse.org/bugs/show_bug.cgi?id=484877 is fixed.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@Data
	public static class DeclaratorsData {

		private final Set<QualifiedName> declaratorNames;

		/**
		 * @param declaratorNames
		 */
		public DeclaratorsData(Set<QualifiedName> declaratorNames) {
			super();
			this.declaratorNames = declaratorNames;
		}

		@Override
		@Pure
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((this.declaratorNames== null) ? 0 : this.declaratorNames.hashCode());
			return result;
		}

		@Override
		@Pure
		public boolean equals(final Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Declarators.DeclaratorsData other = (Declarators.DeclaratorsData) obj;
			if (this.declaratorNames == null) {
				if (other.declaratorNames != null)
					return false;
			} else if (!this.declaratorNames.equals(other.declaratorNames))
				return false;
			return true;
		}

		@Override
		@Pure
		public String toString() {
			ToStringBuilder b = new ToStringBuilder(this);
			b.add("declaratorNames", this.declaratorNames);
			return b.toString();
		}

		@Pure
		public Set<QualifiedName> getDeclaratorNames() {
			return this.declaratorNames;
		}
	}

	public final static TargetURIs.Key<Declarators.DeclaratorsData> KEY = TargetURIs.Key.<Declarators.DeclaratorsData>from(
			SarlPackage.eNAME.toUpperCase(), Declarators.DeclaratorsData.class);

	@Inject
	private IQualifiedNameConverter nameConverter;

	public Declarators.DeclaratorsData getDeclaratorData(final TargetURIs targetURIs, final IReferenceFinder.IResourceAccess resourceAccess) {
		Declarators.DeclaratorsData result = targetURIs.<Declarators.DeclaratorsData>getUserData(Declarators.KEY);
		boolean _notEquals = (!Objects.equal(result, null));
		if (_notEquals) {
			return result;
		}
		final HashSet<QualifiedName> declaratorNames = CollectionLiterals.<QualifiedName>newHashSet();
		Collection<URI> _targetResourceURIs = targetURIs.getTargetResourceURIs();
		final Procedure1<URI> _function = new Procedure1<URI>() {
			@Override
			public void apply(final URI uri) {
				final IUnitOfWork<Object, ResourceSet> _function = new IUnitOfWork<Object, ResourceSet>() {
					@Override
					public Object exec(final ResourceSet it) throws Exception {
						Object _xblockexpression = null;
						{
							Collection<URI> _eObjectURIs = targetURIs.getEObjectURIs(uri);
							final Procedure1<URI> _function = new Procedure1<URI>() {
								@Override
								public void apply(final URI objectURI) {
									final EObject object = it.getEObject(objectURI, true);
									boolean _notEquals = (!Objects.equal(object, null));
									if (_notEquals) {
										final JvmType type = EcoreUtil2.<JvmType>getContainerOfType(object, JvmType.class);
										boolean _notEquals_1 = (!Objects.equal(type, null));
										if (_notEquals_1) {
											String _identifier = type.getIdentifier();
											QualifiedName _qualifiedName = Declarators.this.nameConverter.toQualifiedName(_identifier);
											QualifiedName _lowerCase = _qualifiedName.toLowerCase();
											declaratorNames.add(_lowerCase);
											String _qualifiedName_1 = type.getQualifiedName('.');
											QualifiedName _qualifiedName_2 = Declarators.this.nameConverter.toQualifiedName(_qualifiedName_1);
											QualifiedName _lowerCase_1 = _qualifiedName_2.toLowerCase();
											declaratorNames.add(_lowerCase_1);
										}
									}
								}
							};
							IterableExtensions.<URI>forEach(_eObjectURIs, _function);
							_xblockexpression = null;
						}
						return _xblockexpression;
					}
				};
				resourceAccess.<Object>readOnly(uri, _function);
			}
		};
		IterableExtensions.<URI>forEach(_targetResourceURIs, _function);
		Declarators.DeclaratorsData _declaratorsData = new Declarators.DeclaratorsData(declaratorNames);
		result = _declaratorsData;
		targetURIs.<Declarators.DeclaratorsData>putUserData(Declarators.KEY, result);
		return result;
	}

}
