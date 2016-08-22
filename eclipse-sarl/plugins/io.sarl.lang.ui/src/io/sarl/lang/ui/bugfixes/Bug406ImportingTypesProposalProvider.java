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

package io.sarl.lang.ui.bugfixes;

import java.util.List;
import java.util.Set;

import com.google.common.base.CharMatcher;
import com.google.inject.Inject;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.compiler.IProblem;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.internal.compiler.env.AccessRestriction;
import org.eclipse.jdt.internal.core.search.BasicSearchEngine;
import org.eclipse.jdt.internal.core.search.IRestrictedAccessTypeRequestor;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.xtext.common.types.JvmAnnotationType;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmEnumerationType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.common.types.access.jdt.JdtTypeProviderFactory;
import org.eclipse.xtext.common.types.util.DeprecationUtil;
import org.eclipse.xtext.common.types.xtext.ui.ITypesProposalProvider;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.naming.IQualifiedNameConverter;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.IEObjectDescription;
import org.eclipse.xtext.scoping.IScope;
import org.eclipse.xtext.scoping.IScopeProvider;
import org.eclipse.xtext.ui.editor.IDirtyStateManager;
import org.eclipse.xtext.ui.editor.contentassist.ConfigurableCompletionProposal;
import org.eclipse.xtext.ui.editor.contentassist.ConfigurableCompletionProposal.IReplacementTextApplier;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext.Builder;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalFactory;
import org.eclipse.xtext.ui.editor.contentassist.PrefixMatcher;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.ui.contentassist.SARLImportingTypesProposalProvider;

/** Provider for Fixing issue <a href="https://github.com/eclipse/xtext-eclipse/issues/28">Xtext#</a>.
 *
 * <p>TODO: Remove when Xtext issue is fixed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class Bug406ImportingTypesProposalProvider extends SARLImportingTypesProposalProvider {

	@Inject
	private IScopeProvider scopeProvider;

	@Inject
	private IQualifiedNameConverter qualifiedNameConverter;

	@Inject
	private JdtTypeProviderFactory jdtTypeProviderFactory;

	@Inject
	private IDirtyStateManager dirtyStateManager;

	@Override
	protected void searchAndCreateProposals(IJavaSearchScope scope, final ICompletionProposalFactory proposalFactory,
			ContentAssistContext context, EReference typeReference, final Filter filter,
			final IValueConverter<String> valueConverter, final ICompletionProposalAcceptor acceptor) throws JavaModelException {
		String prefix = context.getPrefix();
		final List<String> split = Strings.split(prefix, '.');
		char[] typeName = null;
		char[] packageName = null;
		if (prefix.length() > 0 && !split.isEmpty()) {
			final CharMatcher dotMatcher = CharMatcher.is('.');
			if (Character.isUpperCase(split.get(split.size() - 1).charAt(0))) {
				typeName = split.get(split.size() - 1).toCharArray();
				if (split.size() > 1) {
					packageName = ("*" + dotMatcher.replaceFrom(prefix.substring(0, prefix.length() //$NON-NLS-1$
							- (typeName.length + 1)), "*.") + "*").toCharArray(); //$NON-NLS-1$ //$NON-NLS-2$
				}
			} else {
				if (prefix.endsWith(".")) { //$NON-NLS-1$
					prefix = prefix.substring(0, prefix.length() - 1);
				}
				packageName = ("*" + dotMatcher.replaceFrom(prefix, //$NON-NLS-1$
						"*.") + "*").toCharArray(); //$NON-NLS-1$ //$NON-NLS-2$
			}
		}
		IScope typeScope = null;
		if (context.getCurrentModel() != null) {
			typeScope = this.scopeProvider.getScope(context.getCurrentModel(), typeReference);
		}
		final IReplacementTextApplier textApplier = createTextApplier(context, typeScope,
				this.qualifiedNameConverter, valueConverter);
		final ICompletionProposalAcceptor scopeAware = textApplier != null ? new ICompletionProposalAcceptor.Delegate(acceptor) {
			@Override
			public void accept(ICompletionProposal proposal) {
				if (proposal instanceof ConfigurableCompletionProposal) {
					((ConfigurableCompletionProposal) proposal).setTextApplier(textApplier);
				}
				super.accept(proposal);
			}
		} : acceptor;
		final Builder contextBuilder = context.copy();
		final PrefixMatcher original = context.getMatcher();
		contextBuilder.setMatcher(new PrefixMatcher() {
			@Override
			public boolean isCandidateMatchingPrefix(String name, String prefix) {
				if (original.isCandidateMatchingPrefix(name, prefix)) {
					return true;
				}
				final String nameWithoutDollars = name.replace('$', '.');
				final String prefixWithoutDollars = prefix.replace('$', '.');
				final boolean nameOrPrefixHasDollars = (nameWithoutDollars != name) || (prefixWithoutDollars != prefix);
				if (nameOrPrefixHasDollars
						&& original.isCandidateMatchingPrefix(nameWithoutDollars, prefixWithoutDollars)) {
					return true;
				}
				String sub = nameWithoutDollars;
				int delimiter = sub.indexOf('.');
				while (delimiter != -1) {
					sub = sub.substring(delimiter + 1);
					delimiter = sub.indexOf('.');
					if ((delimiter == -1 || prefixWithoutDollars.length() > 0
							&& Character.isLowerCase(prefixWithoutDollars.charAt(0)))
						&& (original.isCandidateMatchingPrefix(sub, prefixWithoutDollars))) {
						return true;
					}
				}
				return false;
			}
		});
		final ContentAssistContext myContext = contextBuilder.toContext();
		final IJvmTypeProvider jvmTypeProvider = this.jdtTypeProviderFactory.findOrCreateTypeProvider(
				context.getResource().getResourceSet());
		final Set<String> filteredTypeNames = getDirtyTypeNames();
		final Filter dirtyTypenameFilter = new ITypesProposalProvider.Filter() {
			@SuppressWarnings("synthetic-access")
			@Override
			public boolean accept(int modifiers, char[] packageName, char[] simpleTypeName,
					char[][] enclosingTypeNames, String path) {
				if (path == null || path.endsWith(".class") || path.endsWith(".java")) { //$NON-NLS-1$ //$NON-NLS-2$
					final String identifier = getIdentifier(packageName, simpleTypeName, enclosingTypeNames);
					if (filteredTypeNames.contains(identifier)) {
						return false;
					}
				}
				return true;
			}

			@Override
			public int getSearchFor() {
				return filter.getSearchFor();
			}
		};

		final BasicSearchEngine searchEngine = new BasicSearchEngine();
		searchEngine.searchAllTypeNames(
				packageName, SearchPattern.R_PATTERN_MATCH,
				typeName, SearchPattern.R_PREFIX_MATCH | SearchPattern.R_CAMELCASE_MATCH,
				filter.getSearchFor(), scope,
				new IRestrictedAccessTypeRequestor() {
					@SuppressWarnings("synthetic-access")
					@Override
					public void acceptType(int modifiers, char[] packageName, char[] simpleTypeName,
							char[][] enclosingTypeNames, String path, AccessRestriction access) {
						if (dirtyTypenameFilter.accept(modifiers, packageName, simpleTypeName, enclosingTypeNames, path)
							&& filter.accept(modifiers, packageName, simpleTypeName, enclosingTypeNames, path)
							&& (!checkAccessRestriction() || (access == null
							|| access.getProblemId() != IProblem.ForbiddenReference
							&& !access.ignoreIfBetter()))) {
							final StringBuilder fqName = new StringBuilder(packageName.length + simpleTypeName.length + 1);
							if (packageName.length != 0) {
								fqName.append(packageName);
								fqName.append('.');
							}
							for (final char[] enclosingType: enclosingTypeNames) {
								/*
								 * the JDT index sometimes yields enclosingTypeNames in the form
								 * char[][] { { Outer$Middle } }
								 * rather than
								 * char[][] { { Outer }, { Middle } }
								 * thus we create the fqName as the binary name and post process the proposal later on
								 */
								fqName.append(enclosingType);
								fqName.append('$');
							}
							fqName.append(simpleTypeName);
							final String fqNameAsString = fqName.toString();
							createTypeProposal(fqNameAsString, modifiers, enclosingTypeNames.length > 0, proposalFactory,
									myContext, scopeAware, jvmTypeProvider, valueConverter);
						}
					}
				},
				IJavaSearchConstants.WAIT_UNTIL_READY_TO_SEARCH,
				new NullProgressMonitor() {
					@Override
					public boolean isCanceled() {
						return !acceptor.canAcceptMoreProposals();
					}
				});
		if (acceptor.canAcceptMoreProposals()) {
			final Iterable<IEObjectDescription> allDirtyTypes = this.dirtyStateManager.getExportedObjectsByType(
					TypesPackage.Literals.JVM_TYPE);
			for (final IEObjectDescription description: allDirtyTypes) {
				final QualifiedName qualifiedName = description.getQualifiedName();
				final int modifiers = getDirtyStateModifiers(context, description);
				if (filter.accept(modifiers, qualifiedName.skipLast(1).toString().toCharArray(),
						qualifiedName.getLastSegment().toCharArray(), new char[0][0],
						description.getEObjectURI().toPlatformString(true))) {
					final String fqName = description.getQualifiedName().toString();
					createTypeProposal(fqName, modifiers, fqName.indexOf('$') > 0, proposalFactory, myContext, scopeAware,
							jvmTypeProvider, valueConverter);
				}
			}
		}
	}

	/** Compute the JVM modifiers that corresponds to the given description.
	 *
	 * <p>This function fixes the issue related to the missed modifiers given to the content assist.
	 *
	 * @param context the current content assist context.
	 * @param description the description.
	 * @return the JVM modifiers.
	 */
	@SuppressWarnings("static-method")
	protected int getDirtyStateModifiers(ContentAssistContext context, IEObjectDescription description) {
		EObject eobject = description.getEObjectOrProxy();
		if (eobject.eIsProxy()) {
			eobject = EcoreUtil.resolve(eobject, context.getResource().getResourceSet());
		}
		int accessModifiers = Flags.AccPublic;
		int otherModifiers = 0;
		if (eobject instanceof JvmMember) {
			final JvmMember member = (JvmMember) eobject;
			accessModifiers = getDirtyStateAccessModifier(member);
			otherModifiers = getDirtyStateOtherModifiers(member);
		}
		return accessModifiers | otherModifiers;
	}

	private static int getDirtyStateAccessModifier(JvmMember member) {
		switch (member.getVisibility()) {
		case PUBLIC:
			return Flags.AccPublic;
		case PRIVATE:
			return Flags.AccPrivate;
		case PROTECTED:
			return Flags.AccProtected;
		case DEFAULT:
		default:
			break;
		}
		return Flags.AccDefault;
	}

	private static int getDirtyStateOtherModifiers(JvmMember member) {
		int modifiers = 0;
		if (DeprecationUtil.isDeprecated(member)) {
			modifiers |= Flags.AccDeprecated;
		}
		if (member instanceof JvmDeclaredType) {
			final JvmDeclaredType type = (JvmDeclaredType) member;
			if (type.isFinal()) {
				modifiers |= Flags.AccFinal;
			}
			if (type.isAbstract()) {
				modifiers |= Flags.AccAbstract;
			}
			if (type.isStatic()) {
				modifiers |= Flags.AccStatic;
			}
			if (type instanceof JvmEnumerationType) {
                modifiers |= Flags.AccEnum;
            } else  if (type instanceof JvmAnnotationType) {
                modifiers |= Flags.AccAnnotation;
            } else if (type instanceof JvmGenericType) {
                if (((JvmGenericType) type).isInterface()) {
                    modifiers |= Flags.AccInterface;
                }
            }
		}
		return modifiers;
	}

}
