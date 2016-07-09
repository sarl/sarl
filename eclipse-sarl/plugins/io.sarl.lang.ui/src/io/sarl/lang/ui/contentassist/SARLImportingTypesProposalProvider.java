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

import java.util.Map;

import com.google.inject.Inject;
import org.arakhne.afc.references.SoftValueHashMap;
import org.eclipse.swt.graphics.Image;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.conversion.IValueConverter;
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor;
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalFactory;
import org.eclipse.xtext.xbase.ui.contentassist.ImportingTypesProposalProvider;

import io.sarl.lang.ui.images.IQualifiedNameImageProvider;

/** Provider of types to be imported.
 *
 * <p>This proposal does nothing special than the Xbase provider. But is is here allowing to filter types in SARL code.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLImportingTypesProposalProvider extends ImportingTypesProposalProvider {

	@Inject
	private IQualifiedNameImageProvider images;

	private final Map<String, Image> imageBuffer = new SoftValueHashMap<>();

	@Override
	protected void createTypeProposal(String typeName, int modifiers, boolean isInnerType,
			ICompletionProposalFactory proposalFactory, ContentAssistContext context,
			ICompletionProposalAcceptor acceptor, IJvmTypeProvider jvmTypeProvider,
			IValueConverter<String> valueConverter) {
		final Image image = this.images.getImageForQualifiedName(typeName, jvmTypeProvider);
		synchronized (this.imageBuffer) {
			this.imageBuffer.put(typeName, image);
		}
		super.createTypeProposal(typeName, modifiers, isInnerType, proposalFactory, context, acceptor, jvmTypeProvider,
				valueConverter);
	}

	@Override
	protected Image computeImage(String typeName, boolean isInnerType, int modifiers) {
		final Image image;
		synchronized (this.imageBuffer) {
			image = this.imageBuffer.get(typeName);
		}
		if (image != null) {
			return image;
		}
		return super.computeImage(typeName, isInnerType, modifiers);
	}

}
