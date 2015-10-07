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

import java.util.Set;

import com.google.inject.Inject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtend.core.jvmmodel.AnonymousClassUtil;
import org.eclipse.xtend.core.xtend.AnonymousClass;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.findReferences.ReferenceFinder;
import org.eclipse.xtext.findReferences.TargetURIs;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.imports.StaticallyImportedMemberProvider;
import org.eclipse.xtext.xbase.lib.Extension;
import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XtypePackage;


/** Finds cross-references to elements specified by their URIs.
 *
 * <p>This class is copied from the Xtend private API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLReferenceFinder extends ReferenceFinder {

	@Inject
	@Extension
	private StaticallyImportedMemberProvider memberProvider;

	@Inject
	@Extension
	private AnonymousClassUtil anonymousClassUtil;

	@Inject
	private Declarators declarators;

	@Override
	public void findReferences(
			final TargetURIs targetURIs,
			final IResourceDescription resourceDescription,
			IResourceAccess resourceAccess,
			final Acceptor acceptor,
			final IProgressMonitor monitor) {
		Set<QualifiedName> names = this.declarators.getDeclaratorData(targetURIs, resourceAccess).getDeclaratorNames();
		if (monitor.isCanceled()) {
			throw new OperationCanceledException();
		}
		final Set<QualifiedName> importedNames = IterableExtensions.toSet(resourceDescription.getImportedNames());
		if (IterableExtensions.exists(names, new Functions.Function1<QualifiedName, Boolean>() {
			@Override
			public Boolean apply(QualifiedName it) {
				return importedNames.contains(it);
			}
		})) {
			resourceAccess.readOnly(resourceDescription.getURI(), new IUnitOfWork<Object, ResourceSet>() {
				@Override
				public Object exec(ResourceSet state) throws Exception {
					findReferences(targetURIs, state.getResource(resourceDescription.getURI(), true), acceptor, monitor);
					return null;
				}

			});
		}
	}

	@Override
	protected void findLocalReferencesFromElement(TargetURIs targetURIs,
			EObject sourceCandidate, Resource localResource, Acceptor acceptor) {
		// ignore type references in package fragments
		if (sourceCandidate instanceof XAbstractFeatureCall
				&& ((XAbstractFeatureCall) sourceCandidate).isPackageFragment()) {
			return;
		}

		super.findLocalReferencesFromElement(targetURIs, sourceCandidate, localResource, acceptor);

		if (sourceCandidate instanceof XImportDeclaration) {
			XImportDeclaration importDeclaration = (XImportDeclaration) sourceCandidate;
			if (importDeclaration.isStatic() && !importDeclaration.isWildcard()) {
				addReferenceToFeatureFromStaticImport(importDeclaration, targetURIs, acceptor);
			}
		} else if (sourceCandidate instanceof XFeatureCall) {
			XFeatureCall featureCall = (XFeatureCall) sourceCandidate;
			if (featureCall.getActualReceiver() == null && featureCall.isStatic()) {
				addReferenceToTypeFromStaticImport(featureCall, targetURIs, acceptor);
			}
		} else if (sourceCandidate instanceof XMemberFeatureCall) {
			XMemberFeatureCall featureCall = (XMemberFeatureCall) sourceCandidate;
			if (featureCall.isStatic() && !featureCall.isStaticWithDeclaringType()) {
				addReferenceToTypeFromStaticImport(featureCall, targetURIs, acceptor);
			}
		} else if (sourceCandidate instanceof AnonymousClass) {
			addReferencesToSuper((AnonymousClass) sourceCandidate, targetURIs, acceptor);
		}
	}

	private static void addReferenceIfTarget(EObject candidate, TargetURIs targetURISet,
			EObject sourceElement, EReference reference, Acceptor acceptor) {
		URI candidateURI = EcoreUtil2.getPlatformResourceOrNormalizedURI(candidate);
		if (targetURISet.contains(candidateURI)) {
			URI sourceURI = EcoreUtil2.getPlatformResourceOrNormalizedURI(sourceElement);
			acceptor.accept(sourceElement, sourceURI, reference, -1, candidate, candidateURI);
		}
	}

	private void addReferenceToFeatureFromStaticImport(
			XImportDeclaration importDeclaration, TargetURIs targetURISet, Acceptor acceptor) {
		for (JvmFeature feature : this.memberProvider.getAllFeatures(importDeclaration)) {
			addReferenceIfTarget(feature, targetURISet, importDeclaration,
					XtypePackage.Literals.XIMPORT_DECLARATION__IMPORTED_TYPE, acceptor);
		}
	}

	private static void addReferenceToTypeFromStaticImport(XAbstractFeatureCall sourceCandidate,
			TargetURIs targetURISet, Acceptor acceptor) {
		JvmIdentifiableElement feature = sourceCandidate.getFeature();
		if (feature instanceof JvmMember) {
			JvmDeclaredType type = ((JvmMember) feature).getDeclaringType();
			addReferenceIfTarget(type, targetURISet, sourceCandidate,
					XbasePackage.Literals.XABSTRACT_FEATURE_CALL__FEATURE, acceptor);
		}
	}

	private void addReferencesToSuper(AnonymousClass anonymousClass, TargetURIs targetURISet, Acceptor acceptor) {
		XConstructorCall constructorCall = anonymousClass.getConstructorCall();
		JvmGenericType superType = this.anonymousClassUtil.getSuperType(anonymousClass);
		if (superType != null) {
			addReferenceIfTarget(superType, targetURISet, constructorCall,
					XbasePackage.Literals.XCONSTRUCTOR_CALL__CONSTRUCTOR, acceptor);
		}
		JvmConstructor superConstructor = this.anonymousClassUtil.getSuperTypeConstructor(anonymousClass);
		if (superConstructor != null) {
			addReferenceIfTarget(superConstructor, targetURISet, constructorCall,
					XbasePackage.Literals.XCONSTRUCTOR_CALL__CONSTRUCTOR, acceptor);
		}
	}

}
