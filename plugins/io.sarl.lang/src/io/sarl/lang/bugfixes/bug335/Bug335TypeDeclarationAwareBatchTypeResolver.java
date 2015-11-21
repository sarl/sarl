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

package io.sarl.lang.bugfixes.bug335;

import java.util.List;

import com.google.common.collect.Lists;
import org.eclipse.emf.common.util.TreeIterator;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendFile;
import org.eclipse.xtend.core.xtend.XtendMember;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.xtext.JvmMemberInitializableResource;
import org.eclipse.xtext.resource.DerivedStateAwareResource;
import org.eclipse.xtext.resource.persistence.StorageAwareResource;
import org.eclipse.xtext.xbase.typesystem.internal.LogicalContainerAwareBatchTypeResolver;

/**
 * Xtend batch type resolved.
 *
 * <p>FIXME: Fixed in Xtext 2.9.0.
 *
 * @deprecated Fixed in Xtext 2.9.0
 */
@Deprecated
@SuppressWarnings("all")
public class Bug335TypeDeclarationAwareBatchTypeResolver extends LogicalContainerAwareBatchTypeResolver {

	/**
	 * Checks the internal state of the resource and logs if type resolution was triggered unexpectedly.
	 * If such a condition is detected, an {@link IllegalStateException exception} is thrown.
	 * 
	 * @throws IllegalStateException if the resource is in an unexpected state.
	 */
	@Override
	protected void validateResourceState(Resource resource) {
		if (resource instanceof StorageAwareResource && ((StorageAwareResource) resource).isLoadedFromStorage()) {
			throw new IllegalStateException("Discouraged attempt to compute types for resource that was loaded from storage. Resource was : "+resource.getURI());
		}
		if (resource instanceof DerivedStateAwareResource && ((DerivedStateAwareResource) resource).isInitializing()) {
			throw new IllegalStateException("Discouraged attempt to compute types during model inference. Resource was : "+resource.getURI());
		}
		if (resource instanceof JvmMemberInitializableResource && ((JvmMemberInitializableResource) resource).isInitializingJvmMembers()) {
			throw new IllegalStateException("Discouraged attempt to compute types during JvmMember initialization. Resource was : "+resource.getURI());
		}
	}
	
	@Override
	protected List<EObject> getEntryPoints(EObject object) {
		 List<EObject> result = super.getEntryPoints(object);
		 EObject rootContainer = EcoreUtil.getRootContainer(object);
		 if (rootContainer instanceof XtendFile) {
			 result = Lists.newArrayList(result);
			 List<XtendTypeDeclaration> typeDeclarations = ((XtendFile) rootContainer).getXtendTypes();
			 for(XtendTypeDeclaration declaration: typeDeclarations) {
				 addXtendTypes(declaration, result);
			 }
			 return result;
		 }
		 return result;
	}

	/**
	 * Collects all Xtend type declarations and adds them to the list. The types are added
	 * from the innermost to the outermost type declaration. That is, nested classes are 
	 * added before their declarators are added. This greatly simplifies the implementation of
	 * {@code isHandled} in the concrete {@link org.eclipse.xtext.xbase.typesystem.internal.AbstractRootedReentrantTypeResolver}.
	 */
	private void addXtendTypes(XtendTypeDeclaration declaration, List<EObject> result) {
		for(XtendMember member: declaration.getMembers()) {
			TreeIterator<EObject> iterator = EcoreUtil2.getAllNonDerivedContents(member, true);
			while(iterator.hasNext()) {
				EObject next = iterator.next();
				if (next instanceof XtendTypeDeclaration) {
					addXtendTypes((XtendTypeDeclaration) next, result);
					iterator.prune();
				}
			}
		}
		result.add(declaration);
	}
	
}
