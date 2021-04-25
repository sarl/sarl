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

package io.sarl.lang.ui.quickfix.acceptors;

import java.text.MessageFormat;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.core.search.SearchEngine;
import org.eclipse.jdt.core.search.SearchMatch;
import org.eclipse.jdt.core.search.SearchParticipant;
import org.eclipse.jdt.core.search.SearchPattern;
import org.eclipse.jdt.core.search.SearchRequestor;
import org.eclipse.jdt.internal.core.NamedMember;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.text.correction.IProposalRelevance;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;

import io.sarl.lang.core.Capacity;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.lang.util.OutParameter;
import io.sarl.lang.util.Utils;

/**
 * Add a capacity use.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
public final class CapacityUseAddModification extends SARLSemanticModification {

	private final JvmDeclaredType capacity;

	/** Constructor.
	 *
	 * @param capacity is the capacity.
	 */
	private CapacityUseAddModification(JvmDeclaredType capacity) {
		this.capacity = capacity;
	}

	private static boolean isEquivalentMethodName(String currentName, String declaredName) {
		return Strings.equal(currentName, declaredName);
	}

	private static void findCandidateTypesWithOperation(String methodSimpleName, JvmGenericType genericType, IAcceptor<JvmGenericType> acceptor) {
		for (final JvmMember jvmMember : genericType.getMembers()) {
			if (jvmMember instanceof JvmOperation) {
				final JvmOperation operation = (JvmOperation) jvmMember;
				if (isEquivalentMethodName(methodSimpleName, operation.getSimpleName())) {
					acceptor.accept(genericType);
					return;
				}
			}
		}
	}

	@SuppressWarnings("checkstyle:nestedifdepth")
	private static void findCandidateTypes(SARLQuickfixProvider provider, XtendTypeDeclaration sarlContainer,
			JvmIdentifiableElement jvmContainer, String methodSimpleName,
			IAcceptor<JvmGenericType> acceptor) throws JavaModelException, CoreException {
		final JvmType jvmCapacityType = provider.getTypeServices().getTypeReferences().findDeclaredType(Capacity.class, jvmContainer);
		if (jvmCapacityType != null) {
			final IJavaElement jdtCapacityType = provider.getJavaElementFinder().findElementFor(jvmCapacityType);
			if (jdtCapacityType instanceof IType) {
				final IType jdtCapacityIType = (IType) jdtCapacityType;
				final IJavaSearchScope hierarchyScope = SearchEngine.createStrictHierarchyScope(null, jdtCapacityIType, true, true, null);

				final SearchPattern pattern = SearchPattern.createPattern("*",
						IJavaSearchConstants.INTERFACE,
						IJavaSearchConstants.DECLARATIONS,
						SearchPattern.R_EXACT_MATCH | SearchPattern.R_CASE_SENSITIVE);

				final SearchParticipant[] participants = new SearchParticipant[] {
					SearchEngine.getDefaultSearchParticipant(),
				};
				final SearchEngine engine = new SearchEngine();

				final SearchRequestor requestor = new SearchRequestor() {
					@Override
					public void acceptSearchMatch(SearchMatch match) throws CoreException {
						if (match.getAccuracy() == SearchMatch.A_ACCURATE) {
							final Object element = match.getElement();
							if (element instanceof NamedMember) {
								final NamedMember member = (NamedMember) element;
								final String fqn = member.getFullyQualifiedName('.', false);
								final JvmType type = provider.getTypeServices().getTypeReferences().findDeclaredType(fqn, sarlContainer);
								if (type instanceof JvmGenericType) {
									final JvmGenericType genericType = (JvmGenericType) type;
									findCandidateTypesWithOperation(methodSimpleName, genericType, acceptor);
								}
							}
						}
					}
				};

				engine.search(pattern, participants, hierarchyScope, requestor, new NullProgressMonitor());
			}
		}
	}

	/** Create the quick fix if needed.
	 *
	 * <p>The user data contains the name of the container type, and the name of the new action.
	 *
	 * @param provider the quick fix provider.
	 * @param issue the issue to fix.
	 * @param owner the owner of the missed feature.
	 * @param acceptor the quick fix acceptor.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	public static void accept(SARLQuickfixProvider provider, Issue issue, EObject owner, IssueResolutionAcceptor acceptor) {
		if (owner instanceof XAbstractFeatureCall) {
			final XAbstractFeatureCall call = (XAbstractFeatureCall) owner;
			try {
				final String text = call.getConcreteSyntaxFeatureName();
				if (!Strings.isEmpty(text)) {
					final OutParameter<EObject> container = new OutParameter<>();
					if (Utils.getContainerOfType(call, container, null, XtendTypeDeclaration.class)) {
						final EObject containerObj = container.get();
						if (containerObj instanceof SarlAgent
								|| containerObj instanceof SarlBehavior
								|| containerObj instanceof SarlSkill) {
							final XtendTypeDeclaration typeDeclaration = (XtendTypeDeclaration) containerObj;
							final EObject jvmContainer = provider.getJvmAssociations().getPrimaryJvmElement(containerObj);
							if (jvmContainer instanceof JvmIdentifiableElement) {
								final IAcceptor<JvmGenericType> fqnAcceptor = fqn -> {
									final CapacityUseAddModification modification = new CapacityUseAddModification(fqn);
									modification.setIssue(issue);
									modification.setTools(provider);
									acceptor.accept(issue,
											MessageFormat.format(Messages.CapacityUseAddModification_0, fqn.getSimpleName()),
											MessageFormat.format(Messages.CapacityUseAddModification_1, fqn.getSimpleName()),
											JavaPluginImages.IMG_CORRECTION_ADD,
											modification,
											IProposalRelevance.IMPORT_EXPLICIT);
								};
								findCandidateTypes(
										provider,
										typeDeclaration,
										(JvmIdentifiableElement) jvmContainer,
										text,
										fqnAcceptor);
							}
						}
					}
				}
			} catch (Throwable exception) {
				//
			}
		}
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		final XtendTypeDeclaration container = EcoreUtil2.getContainerOfType(element, XtendTypeDeclaration.class);
		if (container != null) {
			final int insertOffset = getTools().getInsertOffset(container);
			final IXtextDocument document = context.getXtextDocument();
			final int length = getTools().getSpaceSize(document, insertOffset);
			final ReplacingAppendable appendable = getTools().getAppendableFactory().create(document,
					(XtextResource) element.eResource(), insertOffset, length);
			final boolean changeIndentation = container.getMembers().isEmpty();
			if (changeIndentation) {
				appendable.increaseIndentation();
			}
			appendable.newLine();
			appendable.append(
					getTools().getGrammarAccess().getUsesKeyword());
			appendable.append(" "); //$NON-NLS-1$
			appendable.append(this.capacity);
			if (changeIndentation) {
				appendable.decreaseIndentation();
			}
			appendable.newLine();
			appendable.commitChanges();
		}
	}

}
