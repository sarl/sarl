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

package io.sarl.lang.ui.editor;

import java.lang.reflect.Field;
import javax.inject.Inject;

import com.google.inject.MembersInjector;
import com.google.inject.Provider;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRewriteTarget;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.source.IOverviewRuler;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.xtend.ide.editor.RichStringAwareSourceViewer;
import org.eclipse.xtend.ide.editor.TypedRegionMerger;
import org.eclipse.xtext.ui.editor.XtextSourceViewer;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.xbase.lib.Exceptions;

/** Viewer of SARL code.
 *
 * <p>Based on the Xtend implementation, extended with the auto-formating feature when pasting.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLSourceViewer extends RichStringAwareSourceViewer {

	@Inject
	private Provider<IDocumentAutoFormatter> autoFormatterProvider;

	@Inject
	private SARLSourceViewerPreferenceAccess preferences;

	/** Constructor.
	 *
	 * @param parent the container.
	 * @param ruler the vertical ruler.
	 * @param overviewRuler the overview ruler.
	 * @param showsAnnotationOverview the annotation shower.
	 * @param styles the styles.
	 */
	public SARLSourceViewer(Composite parent, IVerticalRuler ruler, IOverviewRuler overviewRuler,
			boolean showsAnnotationOverview, int styles) {
		super(parent, ruler, overviewRuler, showsAnnotationOverview, styles);
	}

	/** Replies if the auto-formatting feature is enable on paste actions.
	 *
	 * @return {@code true} if the auto-formatter is turn on.
	 * @since 0.7
	 */
	public boolean isAutoFormattingEnable() {
		return this.preferences.isAutoFormattingEnabled();
	}

	/** Replies the document auto-formatter.
	 *
	 * @return the service.
	 */
	public IDocumentAutoFormatter getDocumentAutoFormatter() {
		final IDocument document = getDocument();
		if (document instanceof IXtextDocument) {
			final IDocumentAutoFormatter formatter = this.autoFormatterProvider.get();
			formatter.bind((IXtextDocument) document, this.fContentFormatter);
			return formatter;
		}
		return new IDocumentAutoFormatter() {
			//
		};
	}

	@Override
	public void doOperation(int operation) {
		if (operation == ITextOperationTarget.PASTE && isAutoFormattingEnable()) {
			final IRewriteTarget target = getRewriteTarget();
			target.beginCompoundChange();
			final IDocumentAutoFormatter formatter = getDocumentAutoFormatter();
			formatter.beginAutoFormat();
			try {
				super.doOperation(operation);
			} finally {
				formatter.endAutoFormat();
				target.endCompoundChange();
			}
		} else {
			super.doOperation(operation);
		}
	}

	/** Factory of SARL code viewer.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class Factory implements XtextSourceViewer.Factory {

		@Inject
		private TypedRegionMerger merger;

		@Inject
		private MembersInjector<SARLSourceViewer> memberInjector;

		@Override
		public XtextSourceViewer createSourceViewer(Composite parent, IVerticalRuler ruler,
				IOverviewRuler overviewRuler, boolean showsAnnotationOverview, int styles) {
			final SARLSourceViewer result = new SARLSourceViewer(parent, ruler, overviewRuler, showsAnnotationOverview, styles);
			try {
				final Field field = RichStringAwareSourceViewer.class.getDeclaredField("merger"); //$NON-NLS-1$
				field.setAccessible(true);
				field.set(result, this.merger);
			} catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException exception) {
				Exceptions.sneakyThrow(exception);
			}
			this.memberInjector.injectMembers(result);
			return result;
		}

	}

}
