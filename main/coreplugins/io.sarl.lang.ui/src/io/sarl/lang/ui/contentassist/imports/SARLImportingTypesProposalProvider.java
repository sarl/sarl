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

package io.sarl.lang.ui.contentassist.imports;

import javax.inject.Inject;

import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalFactory;
import org.eclipse.xtext.xbase.ui.contentassist.ImportingTypesProposalProvider;

import io.sarl.lang.typesystem.InheritanceHelper;
import io.sarl.lang.ui.labeling.IQualifiedNameImageProvider;

/** Provider of proposals for the types to be imported.
 * This provider is part of the content assist mechanism. Its goal is not to provide
 * features that are not types.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class SARLImportingTypesProposalProvider extends ImportingTypesProposalProvider {

	@Inject
	private IQualifiedNameImageProvider images;

	@Inject
	private InheritanceHelper inheritanceHelper;

	@Override
	protected void createTypeProposal(String typeName, int modifiers, boolean isInnerType,
			ICompletionProposalFactory proposalFactory, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor, IJvmTypeProvider jvmTypeProvider,
			IValueConverter<String> valueConverter) {
		super.createTypeProposal(typeName, modifiers, isInnerType, proposalFactory, context, acceptor, jvmTypeProvider,
				valueConverter);
	}

	@Override
	protected Image computeImage(String typeName, boolean isInnerType, int modifiers) {
		return super.computeImage(typeName, isInnerType, modifiers);
	}

}
