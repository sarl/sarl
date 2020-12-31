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

import javax.inject.Inject;

import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.xtend.ide.autoedit.TokenTypeToPartitionMapper;
import org.eclipse.xtend.ide.contentassist.javadoc.XtendJavaDocContentAssistProcessor;
import org.eclipse.xtext.ui.editor.contentassist.DefaultContentAssistantFactory;
import org.eclipse.xtext.ui.editor.model.TerminalsTokenTypeToPartitionMapper;

/** Provider of a content assitant for the SARL language.
 *
 * <p>It is created for enabling content assist in Javadoc.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLContentAssistFactory extends DefaultContentAssistantFactory {

	@Inject
	private XtendJavaDocContentAssistProcessor javaDocContentAssistProcessor;

	@Override
	protected void setContentAssistProcessor(ContentAssistant assistant, SourceViewerConfiguration configuration,
			ISourceViewer sourceViewer) {
		super.setContentAssistProcessor(assistant, configuration, sourceViewer);
		assistant.setContentAssistProcessor(this.javaDocContentAssistProcessor, TokenTypeToPartitionMapper.JAVA_DOC_PARTITION);
		assistant.setContentAssistProcessor(null, TerminalsTokenTypeToPartitionMapper.SL_COMMENT_PARTITION);
		assistant.setContentAssistProcessor(null, TerminalsTokenTypeToPartitionMapper.COMMENT_PARTITION);
	}

}
