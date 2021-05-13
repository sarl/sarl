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

package io.sarl.lang.ui.contentassist;

import java.util.Objects;
import javax.inject.Inject;

import com.google.common.base.Strings;
import org.apache.log4j.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IPath;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.search.IJavaSearchConstants;
import org.eclipse.jdt.internal.corext.util.JavaModelUtil;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.viewers.StyledString;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.Keyword;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.common.types.xtext.ui.ITypesProposalProvider;
import org.eclipse.xtext.common.types.xtext.ui.TypeMatchFilters;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.naming.IQualifiedNameProvider;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.ui.IImageHelper;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.conversion.XbaseValueConverterService;
import org.eclipse.xtext.xbase.typesystem.IExpressionScope;
import org.eclipse.xtext.xbase.ui.contentassist.XbaseReferenceProposalCreator;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.sarl.SarlAgent;
import io.sarl.lang.sarl.SarlBehavior;
import io.sarl.lang.sarl.SarlBehaviorUnit;
import io.sarl.lang.sarl.SarlCapacity;
import io.sarl.lang.sarl.SarlCapacityUses;
import io.sarl.lang.sarl.SarlClass;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.SarlInterface;
import io.sarl.lang.sarl.SarlRequiredCapacity;
import io.sarl.lang.sarl.SarlSkill;
import io.sarl.lang.services.SARLGrammarKeywordAccess;
import io.sarl.lang.ui.contentassist.general.SARLContentProposalPriorities;
import io.sarl.lang.ui.labeling.SARLImages;
import io.sarl.lang.util.SarlUtils;

/** Provides proposal for the content assist mechanism.
 *
 * <p>This provider:<ul>
 * <li>restricts the proposals according to the context when possible;</li>
 * <li>avoid to propose the hidden features (with "$" inside their names);</li>
 * <li>provides a valid name after the "package" keyword.</li>
 * </ul>
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("checkstyle:classfanoutcomplexity")
public class SARLProposalProvider extends AbstractSARLProposalProvider {

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private IQualifiedNameProvider qualifiedNameProvider;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Inject
	private XbaseValueConverterService valueConverter;

	@Inject
	private SARLImages images;

	@Inject
	private IImageHelper imageHelper;

	@Inject
	private SARLGrammarKeywordAccess keywords;

	//==============================================
	// Utilities functions
	//==============================================

	/** Complete for Java types.
	 *
	 * @param context the completion context.
	 * @param filter the filter for the types.
	 * @param acceptor the proposal acceptor.
	 */
	protected void completeJavaTypes(ContentAssistContext context,
			ITypesProposalProvider.Filter filter, ICompletionProposalAcceptor acceptor) {
		completeJavaTypes(context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				true,
				getQualifiedNameValueConverter(),
				filter,
				acceptor);
	}

	/** Complete for obtaining SARL events if the proposals are enabled.
	 *
	 * @param allowEventType is <code>true</code> for enabling the {@link Event} type to be in the proposals.
	 * @param isExtensionFilter indicates if the type filter is for "extends" or only based on visibility.
	 * @param context the completion context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeSarlEvents(boolean allowEventType, boolean isExtensionFilter, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			completeSubJavaTypes(Event.class, allowEventType, context,
					TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					getQualifiedNameValueConverter(),
					isExtensionFilter ? createExtensionFilter(context, IJavaSearchConstants.CLASS)
							: createVisibilityFilter(context, IJavaSearchConstants.CLASS), acceptor);
		}
	}

	/** Complete for obtaining SARL capacities if the proposals are enabled.
	 *
	 * @param allowCapacityType is <code>true</code> for enabling the {@link Capacity} type to be in the proposals.
	 * @param isExtensionFilter indicates if the type filter is for "extends" or only based on visibility.
	 * @param context the completion context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeSarlCapacities(boolean allowCapacityType, boolean isExtensionFilter, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			completeSubJavaTypes(Capacity.class, allowCapacityType, context,
					TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					getQualifiedNameValueConverter(),
					isExtensionFilter ? createExtensionFilter(context, IJavaSearchConstants.INSTANCEOF_TYPE_REFERENCE)
							: createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
		}
	}

	/** Complete for obtaining SARL agents if the proposals are enabled.
	 *
	 * @param allowAgentType is <code>true</code> for enabling the {@link Agent} type to be in the proposals.
	 * @param isExtensionFilter indicates if the type filter is for "extends" or only based on visibility.
	 * @param context the completion context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeSarlAgents(boolean allowAgentType, boolean isExtensionFilter, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			completeSubJavaTypes(Agent.class, allowAgentType, context,
					TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					getQualifiedNameValueConverter(),
					isExtensionFilter ? createExtensionFilter(context, IJavaSearchConstants.CLASS)
							: createVisibilityFilter(context, IJavaSearchConstants.CLASS), acceptor);
		}
	}

	/** Complete for obtaining SARL behaviors if the proposals are enabled.
	 *
	 * @param allowBehaviorType is <code>true</code> for enabling the {@link Behavior} type to be in the proposals.
	 * @param isExtensionFilter indicates if the type filter is for "extends" or only based on visibility.
	 * @param context the completion context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeSarlBehaviors(boolean allowBehaviorType, boolean isExtensionFilter, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			completeSubJavaTypes(Behavior.class, allowBehaviorType, context,
					TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					getQualifiedNameValueConverter(),
					isExtensionFilter ? createExtensionFilter(context, IJavaSearchConstants.CLASS)
							: createVisibilityFilter(context, IJavaSearchConstants.CLASS), acceptor);
		}
	}

	/** Complete for obtaining SARL skills if proposals are enabled.
	 *
	 * @param allowSkillType is <code>true</code> for enabling the {@link Skill} type to be in the proposals.
	 * @param isExtensionFilter indicates if the type filter is for "extends" or only based on visibility.
	 * @param context the completion context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeSarlSkills(boolean allowSkillType, boolean isExtensionFilter, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			completeSubJavaTypes(Skill.class, allowSkillType, context,
					TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					getQualifiedNameValueConverter(),
					isExtensionFilter ? createExtensionFilter(context, IJavaSearchConstants.CLASS)
							: createVisibilityFilter(context, IJavaSearchConstants.CLASS), acceptor);
		}
	}

	/** Complete for obtaining exception types if proposals are enabled.
	 *
	 * @param allowExceptionType is <code>true</code> for enabling the {@link Exception} type to be in the proposals.
	 * @param context the completion context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeExceptions(boolean allowExceptionType, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			completeSubJavaTypes(Exception.class, allowExceptionType, context,
					TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
					getQualifiedNameValueConverter(),
					createVisibilityFilter(context, IJavaSearchConstants.CLASS), acceptor);
		}
	}

	/** Complete for obtaining SARL types that are subtypes of the given type.
	 *
	 * @param superType the super-type.
	 * @param allowSuperTypeItself indicates if the super-type itself is allowed to be in the proposals.
	 * @param context the content assist context.
	 * @param reference the reference to the rule part to be complete.
	 * @param valueConverter the converter of the proposed values.
	 * @param filter the filter of the proposed values.
	 * @param acceptor the proposal acceptor.
	 */
	protected void completeSubJavaTypes(Class<?> superType, boolean allowSuperTypeItself, ContentAssistContext context,
			EReference reference, IValueConverter<String> valueConverter, final ITypesProposalProvider.Filter filter,
			ICompletionProposalAcceptor acceptor) {
		assert superType != null;
		final INode lastCompleteNode = context.getLastCompleteNode();
		if (lastCompleteNode instanceof ILeafNode && !((ILeafNode) lastCompleteNode).isHidden()) {
			if (lastCompleteNode.getLength() > 0 && lastCompleteNode.getTotalEndOffset() == context.getOffset()) {
				final String text = lastCompleteNode.getText();
				final char lastChar = text.charAt(text.length() - 1);
				if (Character.isJavaIdentifierPart(lastChar)) {
					return;
				}
			}
		}
		final ITypesProposalProvider.Filter subTypeFilter;
		if (allowSuperTypeItself) {
			subTypeFilter = filter;
		} else {
			final String superTypeQualifiedName = superType.getName();
			subTypeFilter = new ITypesProposalProvider.Filter() {

				@Override
				public boolean accept(int modifiers, char[] packageName, char[] simpleTypeName,
						char[][] enclosingTypeNames, String path) {
					final String fullName = JavaModelUtil.concatenateName(packageName, simpleTypeName);
					if (Objects.equals(superTypeQualifiedName, fullName)) {
						return false;
					}
					return filter.accept(modifiers, packageName, simpleTypeName, enclosingTypeNames, path);
				}

				@Override
				public int getSearchFor() {
					return filter.getSearchFor();
				}

			};
		}
		getTypesProposalProvider().createSubTypeProposals(
				this.typeReferences.findDeclaredType(superType, context.getCurrentModel()),
				this,
				context,
				reference,
				subTypeFilter,
				valueConverter,
				acceptor);
	}

	/** Create a filter for "extends".
	 *
	 * <p>An extension filter applies the following constraints: <ul>
	 * <li>the type is not an inner type;</li>
	 * <li>the type is not final;</li>
	 * <li>the type is visible.</li>
	 * </ul>
	 *
	 * @param context the proposal context.
	 * @param type the type of the expected proposals.
	 * @return the filter.
	 */
	protected ITypesProposalProvider.Filter createExtensionFilter(ContentAssistContext context, int type) {
		return new ExtensionFilter(context, type);
	}

	/** Replies the expected package for the given model.
	 *
	 * @param model the model.
	 * @return the expected package name.
	 */
	protected String getExpectedPackageName(EObject model) {
		final URI fileURI = model.eResource().getURI();
		for (final Pair<IStorage, IProject> storage: this.storage2UriMapper.getStorages(fileURI)) {
			if (storage.getFirst() instanceof IFile) {
				final IPath fileWorkspacePath = storage.getFirst().getFullPath();
				final IJavaProject javaProject = JavaCore.create(storage.getSecond());
				return extractProjectPath(fileWorkspacePath, javaProject);
			}
		}
		return null;
	}

	private static String extractProjectPath(IPath fileWorkspacePath, IJavaProject javaProject) {
		if (javaProject != null && javaProject.exists() && javaProject.isOpen()) {
			try {
				for (final IPackageFragmentRoot root: javaProject.getPackageFragmentRoots()) {
					if (!root.isArchive() && !root.isExternal()) {
						final IResource resource = root.getResource();
						if (resource != null) {
							final IPath sourceFolderPath = resource.getFullPath();
							if (sourceFolderPath.isPrefixOf(fileWorkspacePath)) {
								final IPath claspathRelativePath = fileWorkspacePath.makeRelativeTo(sourceFolderPath);
								return claspathRelativePath.removeLastSegments(1)
										.toString().replace("/", "."); //$NON-NLS-1$//$NON-NLS-2$
							}
						}
					}
				}
			} catch (JavaModelException e) {
				Logger.getLogger(SARLProposalProvider.class).error(e.getLocalizedMessage(), e);
			}
		}
		return null;
	}

	/** Complete the "extends" if the proposals are enabled.
	 *
	 * @param model the model.
	 * @param context the context.
	 * @param acceptor the proposal acceptor.
	 * @see #isSarlProposalEnabled()
	 */
	protected void completeExtends(EObject model, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			if (model instanceof SarlAgent) {
				completeSarlAgents(false, true, context, acceptor);
			} else if (model instanceof SarlBehavior) {
				completeSarlBehaviors(false, true, context, acceptor);
			} else if (model instanceof SarlCapacity) {
				completeSarlCapacities(false, true, context, acceptor);
			} else if (model instanceof SarlSkill) {
				completeSarlSkills(false, true, context, acceptor);
			} else if (model instanceof SarlEvent) {
				completeSarlEvents(false, true, context, acceptor);
			} else if (model instanceof SarlClass) {
				completeJavaTypes(
						context,
						createExtensionFilter(context, IJavaSearchConstants.CLASS),
						acceptor);
			} else if (model instanceof SarlInterface) {
				completeJavaTypes(
						context,
						createExtensionFilter(context, IJavaSearchConstants.INTERFACE),
						acceptor);
			}
		}
	}

	private void completeImplements(EObject model, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		if (isSarlProposalEnabled()) {
			if (model instanceof SarlSkill) {
				completeSarlCapacities(true, false, context, acceptor);
			} else if (model instanceof SarlClass) {
				completeJavaTypes(
						context,
						createVisibilityFilter(context, IJavaSearchConstants.INTERFACE),
						acceptor);
			}
		}
	}

	/** Replies if the SARL proposals are enabled.
	 * @return <code>true</code> if the proposals are proposed.
	 */
	protected boolean isSarlProposalEnabled() {
		final XbaseReferenceProposalCreator creator = getXbaseCrossReferenceProposalCreator();
		return creator.isShowTypeProposals() || creator.isShowSmartProposals();
	}

	//==============================================
	// Proposals Functions
	//==============================================

	@Override
	protected boolean isValidProposal(String proposal, String prefix, ContentAssistContext context) {
		// A valid proposal cannot be an hidden feature
		return !SarlUtils.isHiddenMember(proposal) && super.isValidProposal(proposal, prefix, context);
	}

	@Override
	public void completeSarlScript_Package(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		final String expectedPackage = getExpectedPackageName(model);
		if (!Strings.isNullOrEmpty(expectedPackage)) {
			final String codeProposal = this.valueConverter.getQualifiedNameValueConverter().toString(expectedPackage);
			final ICompletionProposal proposal = createCompletionProposal(codeProposal, expectedPackage,
					this.imageHelper.getImage(this.images.forPackage()), context);
			acceptor.accept(proposal);
			return;
		}
		super.completeSarlScript_Package(model, assignment, context, acceptor);
	}

	@Override
	public void completeJvmParameterizedTypeReference_Type(EObject model, Assignment assignment,
			ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		if (model instanceof SarlBehaviorUnit) {
			completeSarlEvents(true, false, context, acceptor);
		} else if (model instanceof SarlCapacityUses || model instanceof SarlRequiredCapacity) {
			completeSarlCapacities(false, false, context, acceptor);
		} else if (model instanceof XtendTypeDeclaration) {
			final EObject grammarElement = context.getLastCompleteNode().getGrammarElement();
			if (grammarElement instanceof Keyword) {
				final String keyword = ((Keyword) grammarElement).getValue();
				if (Objects.equals(keyword, this.keywords.getExtendsKeyword())) {
					completeExtends(model, context, acceptor);
				} else if (Objects.equals(keyword, this.keywords.getImplementsKeyword())) {
					completeImplements(model, context, acceptor);
				}
			}
		} else if (model instanceof SarlField) {
			completeJavaTypes(context, TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE, true,
					getQualifiedNameValueConverter(), createVisibilityFilter(context), acceptor);
		} else {
			super.completeJvmParameterizedTypeReference_Type(model, assignment, context, acceptor);
		}
	}

	@Override
	public void completeAOPMember_FiredEvents(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSarlEvents(true, false, context, acceptor);
	}

	@Override
	public void completeMember_FiredEvents(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSarlEvents(true, false, context, acceptor);
	}

	@Override
	public void completeCapacityMember_FiredEvents(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSarlEvents(true, false, context, acceptor);
	}

	@Override
	public void completeAOPMember_Exceptions(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeExceptions(true, context, acceptor);
	}

	@Override
	public void completeMember_Exceptions(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeExceptions(true, context, acceptor);
	}

	@Override
	public void completeCapacityMember_Exceptions(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeExceptions(true, context, acceptor);
	}

	@Override
	public final void completeType_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeAOPMember_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeMember_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeAnnotationField_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeType_Implements(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeAOPMember_Implements(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeMember_Implements(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public final void completeAnnotationField_Implements(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		// Do not propose because it is supported by the type reference completion.
	}

	@Override
	public void completeAOPMember_Guard(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		if (model instanceof SarlBehaviorUnit) {
			final SarlBehaviorUnit behaviorUnit = (SarlBehaviorUnit) model;
			final XExpression guardExpr = behaviorUnit.getGuard();
			if (guardExpr != null) {
				// Generate the proposals by considering the guard expression as an anchor.
				createLocalVariableAndImplicitProposals(guardExpr, IExpressionScope.Anchor.BEFORE, context, acceptor);
				return;
			}
			final XExpression body = behaviorUnit.getExpression();
			if (body != null) {
				// Generate the proposals by considering that all elements that accessible from the body are accessible from the guard to.
				// "it" is missed => it is manually added.
				final ICompletionProposal itProposal = createCompletionProposal(
						this.keywords.getItKeyword(),
						new StyledString(this.keywords.getItKeyword()),
						this.imageHelper.getImage(this.images.forLocalVariable(0)),
						SARLContentProposalPriorities.CONTEXTUAL_KEYWORD_PRIORITY,
						context.getPrefix(), context);
				acceptor.accept(itProposal);
				createLocalVariableAndImplicitProposals(body, context, acceptor);
			}
		}
	}

	/** Filter for "extends".
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class ExtensionFilter extends TypeMatchFilters.AbstractFilter {

		private final ITypesProposalProvider.Filter visibilityFilter;

		private final String modelFullName;

		/** Constructor.
		 *
		 * @param context the content assit context.
		 * @param searchFor the type of elements to search for.
		 */
		@SuppressWarnings("synthetic-access")
		ExtensionFilter(ContentAssistContext context, int searchFor) {
			super(searchFor);
			this.modelFullName = SARLProposalProvider.this.qualifiedNameProvider.getFullyQualifiedName(
					context.getCurrentModel()).toString();
			this.visibilityFilter = createVisibilityFilter(context, searchFor);
		}

		@Override
		public boolean accept(int modifiers, char[] packageName, char[] simpleTypeName,
				char[][] enclosingTypeNames, String path) {
			// Avoid auto reference of type.
			final String fullName = JavaModelUtil.concatenateName(packageName, simpleTypeName);
			if (Objects.equals(this.modelFullName, fullName)) {
				return false;
			}
			//The following tests are done by the visibility filter.
			//if (TypeMatchFilters.isInternalClass(simpleTypeName, enclosingTypeNames)) {
			//	return false;
			//}
			//if (!TypeMatchFilters.isAcceptableByPreference().accept(modifiers, packageName,
			//		simpleTypeName, enclosingTypeNames, path)) {
			//	return false;
			//}
			// Final modifier test
			if (Flags.isFinal(modifiers)) {
				return false;
			}
			return this.visibilityFilter.accept(modifiers, packageName, simpleTypeName, enclosingTypeNames, path);
		}

	}

}
