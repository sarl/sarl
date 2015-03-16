/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.sarl.lang.ui.quickfix.semantic;

import io.sarl.lang.sarl.FeatureContainer;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.lang.validation.IssueCodes;

import java.text.MessageFormat;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmParameterizedTypeReference;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolutionAcceptor;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.compiler.ImportManager;
import org.eclipse.xtext.xbase.ui.contentassist.ReplacingAppendable;
import org.eclipse.xtext.xtype.XImportDeclaration;
import org.eclipse.xtext.xtype.XImportSection;

/**
 * Quick fixes for {@link IssueCodes#MISSING_METHOD_IMPLEMENTATION}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class MissingMethodImplementationModification extends SARLSemanticModification {

	private final int startIndex;

	private MissingMethodImplementationModification(int startIndex) {
		this.startIndex = startIndex;
	}

	/** Create the quick fix if needed.
	 *
	 * @param provider - the quick fix provider.
	 * @param issue - the issue to fix.
	 * @param acceptor - the quick fix acceptor.
	 */
	public static void accept(SARLQuickfixProvider provider, Issue issue, IssueResolutionAcceptor acceptor) {
		String[] data = issue.getData();
		if (data != null && data.length > 0) {
			StringBuffer lines = new StringBuffer();
			int i;
			for (i = 0; i < data.length && !Strings.isEmpty(data[i]); ++i) {
				// Search for the end of the import declarations
			}
			++i;
			int startIndex = i;
			for (; i < data.length; i += 2) {
				lines.append(MessageFormat.format(Messages.SARLQuickfixProvider_14, data[i]));
			}
			if (lines.length() > 0) {
				String msg = MessageFormat.format(Messages.SARLQuickfixProvider_15, lines);
				MissingMethodImplementationModification modification = new MissingMethodImplementationModification(
						startIndex);
				modification.setIssue(issue);
				modification.setTools(provider);
				acceptor.accept(issue,
						Messages.SARLQuickfixProvider_16,
						msg,
						null,
						modification);
			}
		}
	}

	private void addMissedFunctions(FeatureContainer container, IXtextDocument document, EObject element, String[] data)
			throws Exception {
		SARLQuickfixProvider tools = getTools();
		int insertOffset = tools.getInsertOffset(container);
		int length = tools.getSpaceSize(document, insertOffset);
		ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) element.eResource(), insertOffset, length);
		boolean initialIndent = (container.getFeatures().isEmpty());
		appendable.newLine();
		if (initialIndent) {
			appendable.increaseIndentation();
		}
		for (int j = this.startIndex; j < data.length; j += 2) {
			appendable.newLine().append(data[j]).append(" "); //$NON-NLS-1$
			appendable.append(tools.getGrammarAccess().getXBlockExpressionAccess().getLeftCurlyBracketKeyword_1().getValue());
			appendable.increaseIndentation().newLine();
			appendable.append("// TODO ").append(//$NON-NLS-1$
					io.sarl.lang.genmodel.Messages.SARLCodeGenerator_0);
			if (!com.google.common.base.Strings.isNullOrEmpty(data[j + 1])) {
				appendable.newLine().append(data[j + 1]);
			}
			appendable.decreaseIndentation().newLine();
			appendable.append(tools.getGrammarAccess().getXBlockExpressionAccess().getRightCurlyBracketKeyword_3().getValue());
			appendable.newLine();
		}
		appendable.decreaseIndentation().newLine();
		appendable.commitChanges();
	}

	private void addMissedImports(SarlScript script, FeatureContainer container, IXtextDocument document, EObject element,
			String[] data) throws Exception {
		SARLQuickfixProvider tools = getTools();
		int insertOffset = tools.getImportInsertOffset(script);
		ReplacingAppendable appendable = tools.getAppendableFactory().create(document,
				(XtextResource) element.eResource(), insertOffset, 0);
		ImportManager importManager = new ImportManager();
		XImportSection importSection = script.getImportSection();
		if (importSection != null) {
			for (XImportDeclaration declaration : importSection.getImportDeclarations()) {
				JvmDeclaredType type = declaration.getImportedType();
				if (type != null) {
					importManager.addImportFor(type);
				}
			}
		}
		for (int j = 0; j < (this.startIndex - 1); ++j) {
			JvmParameterizedTypeReference typeRef = tools.newTypeRef(data[j], container);
			if (typeRef != null && importManager.addImportFor(typeRef.getType())) {
				appendable.newLine();
				appendable.append(
						tools.getGrammarAccess()
						.getXImportDeclarationAccess().getImportKeyword_0().getValue());
				appendable.append(" "); //$NON-NLS-1$
				appendable.append(typeRef.getQualifiedName());
			}
		}
		appendable.commitChanges();
	}

	@Override
	public void apply(EObject element, IModificationContext context) throws Exception {
		FeatureContainer container = EcoreUtil2.getContainerOfType(element, FeatureContainer.class);
		if (container != null) {
			Issue issue = getIssue();
			SarlScript script = EcoreUtil2.getContainerOfType(element, SarlScript.class);
			assert (script != null);
			String[] data = issue.getData();
			assert (data != null && data.length > 0);
			IXtextDocument document = context.getXtextDocument();

			addMissedFunctions(container, document, element, data);
			addMissedImports(script, container, document, element, data);
		}
	}

}
