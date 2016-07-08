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
import org.eclipse.xtext.Assignment;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.common.types.xtext.ui.ITypesProposalProvider;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.nodemodel.ILeafNode;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.resource.IStorage2UriMapper;
import org.eclipse.xtext.util.Pair;
import org.eclipse.xtext.xbase.conversion.XbaseValueConverterService;

import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.core.Skill;
import io.sarl.lang.ui.images.SARLImages;

/** Provides proposal for the content assist mechanism.
 *
 * <p>This provider restrict the proposal according to the context.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProposalProvider extends AbstractSARLProposalProvider {

	@Inject
	private TypeReferences typeReferences;

	@Inject
	private IStorage2UriMapper storage2UriMapper;

	@Inject
	private XbaseValueConverterService valueConverter;

	@Inject
	private SARLImages images;

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
	 * @param context the proposal context.
	 * @param type the type of the expected proposals.
	 * @return the filter.
	 */
	protected ITypesProposalProvider.Filter createExtensionFilter(ContentAssistContext context, final int type) {
		final ITypesProposalProvider.Filter visibilityFilter = createVisibilityFilter(context, type);
		return new ITypesProposalProvider.Filter() {
			@Override
			public int getSearchFor() {
				return visibilityFilter.getSearchFor();
			}

			@Override
			public boolean accept(int modifiers, char[] packageName, char[] simpleTypeName,
					char[][] enclosingTypeNames, String path) {
				if (Flags.isFinal(modifiers)) {
					return false;
				}
				return visibilityFilter.accept(modifiers, packageName, simpleTypeName, enclosingTypeNames, path);
			}
		};
	}

	@Override
	public void completeInterface_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeJavaTypes(context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
	}

	@Override
	public void completeClass_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeJavaTypes(context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeClass_Implements(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeJavaTypes(context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
	}

	@Override
	public void completeAgent_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Agent.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeBehavior_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Behavior.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeCapacity_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Capacity.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
	}

	@Override
	public void completeSkill_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Skill.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeSkill_Implements(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Capacity.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
	}

	@Override
	public void completeEvent_Extends(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Event.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeBehaviorUnit_Name(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Event.class, true, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeCapacityUses_Capacities(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Capacity.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
	}

	@Override
	public void completeRequiredCapacity_Capacities(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Capacity.class, false, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.INTERFACE), acceptor);
	}

	@Override
	public void completeAction_FiredEvents(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Event.class, true, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createVisibilityFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeAction_Exceptions(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		completeSubJavaTypes(Exception.class, true, context,
				TypesPackage.Literals.JVM_PARAMETERIZED_TYPE_REFERENCE__TYPE,
				getQualifiedNameValueConverter(),
				createExtensionFilter(context, IJavaSearchConstants.CLASS), acceptor);
	}

	@Override
	public void completeSarlScript_Package(EObject model, Assignment assignment, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor) {
		final String expectedPackage = getExpectedPackageName(model);
		if (!Strings.isNullOrEmpty(expectedPackage)) {
			final String codeProposal = this.valueConverter.getQualifiedNameValueConverter().toString(expectedPackage);
			final ICompletionProposal proposal = createCompletionProposal(codeProposal, expectedPackage,
					this.images.forPackage().createImage(), context);
			acceptor.accept(proposal);
			return;
		}
		super.completeSarlScript_Package(model, assignment, context, acceptor);
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

}
