/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.bugfixes;

import static com.google.common.collect.Sets.newHashSet;

import java.util.Collection;
import java.util.Set;

import org.eclipse.emf.ecore.EReference;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.access.jdt.IJdtTypeProvider;
import org.eclipse.xtext.common.types.access.jdt.JdtTypeProviderFactory;
import org.eclipse.xtext.common.types.util.RawSuperTypes;
import org.eclipse.xtext.common.types.xtext.ui.ITypesProposalProvider;
import org.eclipse.xtext.common.types.xtext.ui.IntersectingJavaSearchScope;
import org.eclipse.xtext.common.types.xtext.ui.TypeMatchFilters;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalFactory;
import org.eclipse.xtext.xbase.ui.contentassist.ImportingTypesProposalProvider;

import com.google.common.collect.Sets;
import com.google.inject.Inject;

/**
 * Patches for the bugs 438233 and 438191 in Xtext.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://bugs.eclipse.org/bugs/show_bug.cgi?id=438233"
 * @see "https://bugs.eclipse.org/bugs/show_bug.cgi?id=438233"
 */
public class XtextBugFixes438191And438233 extends ImportingTypesProposalProvider {

	@Inject
	private RawSuperTypes superTypeCollector;
	
	@Inject
	private JdtTypeProviderFactory jdtTypeProviderFatory;
	
	/** {@inheritDoc}
	 */
	@Override
	@SuppressWarnings("all")
	public void createSubTypeProposals(final JvmType superType,
			ICompletionProposalFactory proposalFactory,
			ContentAssistContext context, EReference typeReference,
			final Filter filter, IValueConverter<String> valueConverter,
			ICompletionProposalAcceptor acceptor) {
		if (superType == null || superType.eIsProxy())
			return;
		if (superType.eResource() == null || superType.eResource().getResourceSet() == null)
			return;
		IJavaProject project = getProjectProvider().getJavaProject(superType.eResource().getResourceSet());
		if (project == null)
			return;

		String fqn = superType.getIdentifier();
		// java.lang.Object - no need to create hierarchy scope
		if (Object.class.getName().equals(fqn)) {
			createTypeProposals(project, proposalFactory, context, typeReference, filter, valueConverter, acceptor);
			return;
		} 

		final Collection<JvmType> superTypes = superTypeCollector.collect(superType);
		final Set<String> superTypeNames = Sets.newHashSet();
		for(JvmType collectedSuperType: superTypes) {
			superTypeNames.add(collectedSuperType.getIdentifier());
		}
		superTypeNames.remove(fqn);
		final IJdtTypeProvider provider = jdtTypeProviderFatory.createTypeProvider(superType.eResource().getResourceSet());
		try {
			IType type = project.findType(fqn);
			if (type != null) {
				IJavaSearchScope hierarchyScope = SearchEngine.createHierarchyScope(type);
				IJavaSearchScope projectScope = SearchEngine.createJavaSearchScope(new IJavaElement[] { project });
				IJavaSearchScope scope = new IntersectingJavaSearchScope(projectScope, hierarchyScope);
				final Set<String> alreadyAccepted = newHashSet(); 
				searchAndCreateProposals(scope, proposalFactory, context, typeReference, TypeMatchFilters.and(filter, new ITypesProposalProvider.Filter() {
					public boolean accept(int modifiers, char[] packageName, char[] simpleTypeName,
							char[][] enclosingTypeNames, String path) {
						StringBuilder fqName = new StringBuilder(
								packageName.length + simpleTypeName.length + 1);
						if (packageName.length != 0) {
							fqName.append(packageName);
							fqName.append('.');
						}
						for (char[] enclosingType: enclosingTypeNames) {
							fqName.append(enclosingType);
							fqName.append('.');
						}
						fqName.append(simpleTypeName);
						String fqNameAsString = fqName.toString();
						// the dirty state proposals
						if (!alreadyAccepted.contains(fqName)
								&& !(path.endsWith(".class")
									|| path.endsWith(".java"))) {
							JvmType type = provider.findTypeByName(fqNameAsString);
							return superTypeCollector.collect(type).contains(superType);
						} else {
							boolean b = !superTypeNames.contains(fqNameAsString);
							if (b) {
								alreadyAccepted.add(fqNameAsString);
							}
							return b;
						}
					}

					public int getSearchFor() {
						return filter.getSearchFor();
					}

				}), valueConverter, acceptor);
			}
		} catch (JavaModelException ex) {
			// ignore
		}
	}

}
