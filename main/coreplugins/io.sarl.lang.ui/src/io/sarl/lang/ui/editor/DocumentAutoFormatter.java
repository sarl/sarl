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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.formatter.IContentFormatter;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Exceptions;

/** A service that enables to do auto-formatting when a document changed.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class DocumentAutoFormatter implements IDocumentAutoFormatter {

	private Collection<RegionFormattingRequest> formattingRequests = Collections.synchronizedList(new ArrayList<>(1));

	private IDocumentListener autoFormatListener;

	private IXtextDocument document;

	private IContentFormatter contentFormatter;

	@Override
	public void bind(IXtextDocument document, IContentFormatter contentFormatter) {
		assert document != null;
		assert contentFormatter != null;
		this.document = document;
		this.contentFormatter = contentFormatter;
	}

	@Override
	public synchronized void beginAutoFormat() {
		if (this.document != null && this.autoFormatListener == null) {
			this.formattingRequests.clear();
			this.autoFormatListener = new IDocumentListener() {
				@Override
				public void documentAboutToBeChanged(DocumentEvent event) {
					//
				}

				@SuppressWarnings("synthetic-access")
				@Override
				public void documentChanged(DocumentEvent event) {
					if (!Strings.isEmpty(event.getText())
							&& event.getDocument() instanceof IXtextDocument) {
						DocumentAutoFormatter.this.formattingRequests.add(new RegionFormattingRequest(
								(IXtextDocument) event.getDocument(), event.getOffset(), event.getText().length()));
					}
				}
			};
			this.document.addDocumentListener(this.autoFormatListener);
		}
	}

	@Override
	public void endAutoFormat() {
		final Collection<RegionFormattingRequest> requests;
		synchronized (this) {
			requests = this.formattingRequests;
			this.formattingRequests = Collections.synchronizedList(new ArrayList<>(1));
			if (this.autoFormatListener != null) {
				final IDocumentListener listener = this.autoFormatListener;
				this.autoFormatListener = null;
				if (this.document != null) {
					this.document.removeDocumentListener(listener);
				}
			}
		}
		if (this.contentFormatter != null) {
			for (final RegionFormattingRequest request : requests) {
				formatRegion(request.document, request.offset, request.length);
			}
		}
	}

	/** Called for formatting a region.
	 *
	 * @param document the document to format.
	 * @param offset the offset of the text to format.
	 * @param length the length of the text.
	 */
	protected void formatRegion(IXtextDocument document, int offset, int length) {
		try {
			final int startLineIndex = document.getLineOfOffset(previousSiblingChar(document, offset));
			final int endLineIndex = document.getLineOfOffset(offset + length);
			int regionLength = 0;
			for (int i = startLineIndex; i <= endLineIndex; ++i) {
				regionLength += document.getLineLength(i);
			}
			if (regionLength > 0) {
				final int startOffset = document.getLineOffset(startLineIndex);
				for (final IRegion region : document.computePartitioning(startOffset, regionLength)) {
					this.contentFormatter.format(document, region);
				}
			}
		} catch (BadLocationException exception) {
			Exceptions.sneakyThrow(exception);
		}
	}

	private static int previousSiblingChar(IXtextDocument document, int offset) throws BadLocationException {
		int off = offset - 1;
		while (off >= 0 && Character.isWhitespace(document.getChar(off))) {
			--off;
		}
		return Math.max(0, off);
	}

	/** Request for formatting a region.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SuppressWarnings("checkstyle:visibilitymodifier")
	private static class RegionFormattingRequest {

		public final IXtextDocument document;

		public final int offset;

		public final int length;

		RegionFormattingRequest(IXtextDocument document, int offset, int length) {
			this.document = document;
			this.offset = offset;
			this.length = length;
		}

	}

}
