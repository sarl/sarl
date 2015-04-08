/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.ui.tests.quickfix;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

import java.io.ByteArrayOutputStream;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Matcher;

import org.eclipse.core.resources.IFile;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.ILineTracker;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextStore;
import org.eclipse.jface.text.Region;
import org.eclipse.xtext.resource.IFragmentProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.IXtextDocumentContentObserver;
import org.eclipse.xtext.ui.editor.model.IXtextModelListener;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.validation.Issue;

import com.google.inject.Inject;

/** Abstract implementation for the quick fix tests.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSARLQuickfixTest extends AbstractSarlUiTest {

	@Inject
	private SARLQuickfixProvider quickfixProvider;

	/** Create the filename for the given basename.
	 *
	 * @param issueCode - the code of the issue.
	 * @return the name of the file.
	 */
	private static String filename(String issueCode) {
		String fileName = issueCode.replaceAll("[^a-zA-Z0-9]+", "_"); //$NON-NLS-1$//$NON-NLS-2$
		String packageName = issueCode.replaceAll("[^a-zA-Z0-9_]+", ""); //$NON-NLS-1$//$NON-NLS-2$
		return pathStr(
				"io", "sarl", //$NON-NLS-1$//$NON-NLS-2$
				"lang", "ui", //$NON-NLS-1$//$NON-NLS-2$
				"tests", "quickfix", //$NON-NLS-1$//$NON-NLS-2$
				packageName, "fixing_" + fileName); //$NON-NLS-1$
	}

	/** Create an object that permits to test the details of a quick fix.
	 *
	 * @param issueCode - the code of the issue to test.
	 * @param invalidCode - the code that is generating the issue.
	 * @return the quick fixes for assertions.
	 */
	protected QuickFixAsserts getQuickFixAsserts(
			String issueCode,
			String invalidCode) {
		try {
			int filenameCounter = 0;
			String oFilename = filename(issueCode);
			String filename = oFilename;
			boolean foundFile = this.helper.isFileInSourceFolder(filename + ".sarl"); //$NON-NLS-1$
			while (foundFile) {
				++filenameCounter;
				filename = oFilename + Integer.toString(filenameCounter);
				foundFile = this.helper.isFileInSourceFolder(filename + ".sarl"); //$NON-NLS-1$
			}
			IFile file = this.helper.createFileInSourceFolder(filename, invalidCode);
			Resource scriptResource = this.helper.createSARLScriptResource(file, invalidCode);
			SarlScript script = (SarlScript) scriptResource.getContents().get(0);
			assertNotNull(script);

			List<Issue> issues = this.helper.getValidator().validate(script);
			StringBuilder issueLabels = new StringBuilder();
			Iterator<Issue> issueIterator = issues.iterator();
			List<IssueResolution> resolutions = null;
			Issue issue = null;
			while ((resolutions == null || resolutions.isEmpty()) && issueIterator.hasNext()) {
				Issue nextIssue = issueIterator.next();
				if (issueLabels.length() > 0) {
					issueLabels.append(", "); //$NON-NLS-1$
				}
				issueLabels.append(nextIssue.getCode());
				issueLabels.append(" - \""); //$NON-NLS-1$
				issueLabels.append(Strings.convertToJavaString(nextIssue.getMessage()));
				issueLabels.append("\""); //$NON-NLS-1$
				if (issueCode.equals(nextIssue.getCode())) {
					issue = nextIssue;
					resolutions = this.quickfixProvider.getResolutions(issue);
				}
			}
			if (issue == null) {
				fail("The issue '" + issueCode //$NON-NLS-1$
					+ "' was not found.\nAvailable issues are: " //$NON-NLS-1$
					+ issueLabels.toString());
			}
			if (resolutions == null || resolutions.isEmpty()) {
				fail("No resolution found for the issue '" + issueCode //$NON-NLS-1$
					+ "'."); //$NON-NLS-1$
			}

			QuickFixAsserts asserts = new QuickFixAsserts(
					issueCode,
					invalidCode,
					scriptResource,
					resolutions);

			return asserts;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/** Assert the quick fix is changing the document in the expected way.
	 *
	 * @param issueCode - the code of the issue to test.
	 * @param invalidCode - the code that is generating the issue.
	 * @param expectedLabel - the expected label for the quick fix.
	 * @param expectedDescription - the expected description for the quick fix.
	 * @param expectedResolution - the expected code after fixing.
	 */
	protected void assertQuickFix(
			String issueCode,
			String invalidCode,
			String expectedLabel,
			String expectedDescription,
			String expectedResolution) {
		QuickFixAsserts asserts = getQuickFixAsserts(issueCode, invalidCode);
		asserts.assertQuickFix(expectedLabel, expectedDescription, expectedResolution);
		asserts.assertNoQuickFix();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class QuickFixAsserts {

		private final String issueCode;
		private final String invalidCode;
		private final Resource scriptResource;
		private final Collection<IssueResolution> resolutions = new LinkedList<>();

		/**
		 * @param issueCode
		 * @param invalidCode
		 * @param scriptResource
		 * @param resolutions
		 */
		public QuickFixAsserts(
				String issueCode,
				String invalidCode,
				Resource scriptResource,
				Collection<IssueResolution> resolutions) {
			this.issueCode = issueCode;
			this.invalidCode = invalidCode;
			this.scriptResource = scriptResource;
			this.resolutions.addAll(resolutions);
		}

		private IssueResolution findResolution(String label) {
			Iterator<IssueResolution> iterator = this.resolutions.iterator();
			while (iterator.hasNext()) {
				IssueResolution resolution = iterator.next();
				String resolutionLabel = resolution.getLabel();
				if (resolutionLabel.equals(label)) {
					iterator.remove();
					return resolution;
				}
			}
			fail("Quick fix not found for the issue '" + this.issueCode //$NON-NLS-1$
					+ "'.\nAvailable quick fix resolutions: " + toString() ); //$NON-NLS-1$
			return null;
		}

		@Override
		public String toString() {
			StringBuilder buffer = new StringBuilder();
			for(IssueResolution resolution : this.resolutions) {
				if (buffer.length() > 0) {
					buffer.append(", "); //$NON-NLS-1$
				}
				buffer.append("\""); //$NON-NLS-1$
				buffer.append(Strings.convertToJavaString(resolution.getLabel()));
				buffer.append("\""); //$NON-NLS-1$
			}
			return "[ " + buffer.toString() + " ]"; //$NON-NLS-1$ //$NON-NLS-2$
		}

		/** Test the existence of a valid quick fix.
		 *
		 * @param expectedLabel - the expected label for the quick fix.
		 * @param expectedDescription - the expected description for the quick fix.
		 * @param expectedResolution - the expected code after fixing.
		 */
		public void assertQuickFix(
				String expectedLabel,
				String expectedDescription,
				String expectedResolution) {
			try {
				IssueResolution resolution = findResolution(expectedLabel);

				assertEquals(expectedLabel, resolution.getLabel());
				assertEquals(expectedDescription, resolution.getDescription());

				XtextResource xtextResource = new XtextResource(this.scriptResource.getURI());
				xtextResource.setFragmentProvider(new TestFragmentProvider(this.scriptResource));
				TestXtextDocument document = new TestXtextDocument(xtextResource, this.invalidCode);
				TestModificationContext modificationContext = new TestModificationContext(document);

				resolution.getModification().apply(modificationContext);

				String content;
				try (ByteArrayOutputStream os = new ByteArrayOutputStream()) {
					this.scriptResource.save(os, null);
					content = os.toString();
				}
				if (!expectedResolution.equals(content)) {
					content = document.toString();
				}
				assertEquals("Invalid quick fix.", expectedResolution, content); //$NON-NLS-1$
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		/** Test if there is no more quick fix.
		 */
		public void assertNoQuickFix() {
			assertEquals("Expecting no quick fix, but got: " //$NON-NLS-1$
					+ toString(),
					0, this.resolutions.size());

		}

	} // class QuickFixAsserts

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TestModificationContext implements IModificationContext {

		private final IXtextDocument document;

		/**
		 * @param document
		 */
		public TestModificationContext(IXtextDocument document) {
			this.document = document;
		}

		@Override
		public IXtextDocument getXtextDocument() {
			return this.document;
		}

		@Override
		public IXtextDocument getXtextDocument(URI uri) {
			return this.document;
		}

	} // class TestModificationContext

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TestFragmentProvider implements IFragmentProvider {

		private final Resource scriptResource;

		/**
		 * @param scriptResource
		 */
		public TestFragmentProvider(Resource scriptResource) {
			this.scriptResource = scriptResource;
		}

		@Override
		public String getFragment(EObject obj, Fallback fallback) {
			throw new UnsupportedOperationException();
		}
		@Override
		public EObject getEObject(Resource resource, String fragment, Fallback fallback) {
			return this.scriptResource.getEObject(fragment);
		}

	} // class TestFragmentProvider

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TestTextStore implements ITextStore {

		private final StringBuilder content = new StringBuilder();

		/**
		 * @param initialContent - initialContent
		 */
		public TestTextStore(String initialContent) {
			if (initialContent != null) {
				this.content.append(initialContent);
			}
		}

		@Override
		public char get(int offset) {
			return this.content.charAt(offset);
		}

		@Override
		public String get(int offset, int length) {
			return this.content.substring(offset, offset + length);
		}

		@Override
		public int getLength() {
			return this.content.length();
		}

		@Override
		public void replace(int offset, int length, String text) {
			this.content.replace(offset, offset + length, text);
		}

		@Override
		public void set(String text) {
			this.content.setLength(0);
			this.content.append(text);
		}

		@Override
		public String toString() {
			return this.content.toString();
		}

	} // class TestTextStore

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TestLineTracker implements ILineTracker {

		/** The predefined delimiters of this tracker */
		private final static String[] STR_DELIMITERS = { "\r", "\n", "\r\n" }; //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

		private final StringBuilder content = new StringBuilder();
		private String[] lines = null;

		private static String[] splitInLines(String text) {
			StringBuilder delims = new StringBuilder();
			for(String delim : STR_DELIMITERS) {
				if (delims.length() > 0) {
					delims.append("|"); //$NON-NLS-1$
				}
				delims.append("(" + Matcher.quoteReplacement(delim) + ")"); //$NON-NLS-1$//$NON-NLS-2$
			}
			return text.split(delims.toString());
		}

		private void ensureLines() {
			if (this.lines == null) {
				this.lines = splitInLines(this.content.toString());
			}
		}

		/**
		 * @param initialContent
		 */
		public TestLineTracker(String initialContent) {
			this.content.append(initialContent);
		}

		@Override
		public String[] getLegalLineDelimiters() {
			return STR_DELIMITERS;
		}

		@Override
		public String getLineDelimiter(int line) throws BadLocationException {
			ensureLines();
			if (line < 0 || line >= this.lines.length) {
				throw new BadLocationException();
			}
			String text = this.lines[line];
			for(String delim : STR_DELIMITERS) {
				if (text.endsWith(delim)) {
					return delim;
				}
			}
			return null;
		}

		@Override
		public int computeNumberOfLines(String text) {
			String[] lines = splitInLines(text);
			return lines.length;
		}

		@Override
		public int getNumberOfLines() {
			ensureLines();
			return this.lines.length;
		}

		@Override
		public int getNumberOfLines(int offset, int length) throws BadLocationException {
			String subText = this.content.substring(offset, offset + length);
			return computeNumberOfLines(subText);
		}

		@Override
		public int getLineOffset(int line) throws BadLocationException {
			ensureLines();
			if (line < 0 || line >= this.lines.length) {
				throw new BadLocationException();
			}
			int offset = 0;
			for(int i=0; i < this.lines.length && i < line; ++i) {
				offset += this.lines[i].length();
			}
			return offset;
		}

		@Override
		public int getLineLength(int line) throws BadLocationException {
			ensureLines();
			if (line < 0 || line >= this.lines.length) {
				throw new BadLocationException();
			}
			return this.lines[line].length();
		}

		@Override
		public int getLineNumberOfOffset(int offset) throws BadLocationException {
			if (offset < 0) {
				throw new BadLocationException();
			}
			ensureLines();
			int tmpOffset = 0;
			for(int i=0; i < this.lines.length; ++i) {
				tmpOffset += this.lines[i].length();
				if (offset < tmpOffset) {
					return i;
				}
			}
			throw new BadLocationException();
		}

		@Override
		public IRegion getLineInformationOfOffset(int offset) throws BadLocationException {
			if (offset < 0) {
				throw new BadLocationException();
			}
			ensureLines();
			int tmpOffset = 0;
			int previousOffset;
			for(int i = 0; i < this.lines.length; ++i) {
				previousOffset = tmpOffset;
				tmpOffset += this.lines[i].length();
				if (offset < tmpOffset) {
					return new Region(previousOffset, this.lines[i].length());
				}
			}
			throw new BadLocationException();
		}

		@Override
		public IRegion getLineInformation(int line) throws BadLocationException {
			if (line < 0 || line >= this.lines.length) {
				throw new BadLocationException();
			}
			ensureLines();
			int offset = 0;
			for(int i = 0; i < (line - 1); ++i) {
				offset += this.lines[i].length();
			}
			return new Region(offset, this.lines[line].length());
		}

		@Override
		public void replace(int offset, int length, String text) throws BadLocationException {
			this.lines = null;
			this.content.replace(offset, offset + length, text);
		}

		@Override
		public void set(String text) {
			this.lines = null;
			this.content.setLength(0);
			this.content.append(text);
		}

		@Override
		public String toString() {
			return this.content.toString();
		}

	} // class TestLineTracker

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class TestXtextDocument extends Document implements IXtextDocument {

		private final XtextResource resource;
		private final TestTextStore textStore;

		/**
		 * @param resource
		 * @param initialText
		 */
		public TestXtextDocument(XtextResource resource, String initialText) {
			this.resource = resource;
			this.textStore = new TestTextStore(initialText);
			setTextStore(this.textStore);
			setLineTracker(new TestLineTracker(initialText));
		}

		@Override
		public String toString() {
			return this.textStore.toString();
		}

		@Override
		public <T> T readOnly(IUnitOfWork<T, XtextResource> work) {
			throw new UnsupportedOperationException();
		}

		@Override
		public <T> T priorityReadOnly(IUnitOfWork<T, XtextResource> work) {
			throw new UnsupportedOperationException();
		}

		@Override
		public <T> T modify(IUnitOfWork<T, XtextResource> work) {
			try {
				return work.exec(this.resource);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		/** {@inheritDoc}
		 */
		@Override
		public <T> T getAdapter(Class<T> adapterType) {
			throw new UnsupportedOperationException();
		}

		/** {@inheritDoc}
		 */
		@Override
		public void addModelListener(IXtextModelListener listener) {
			throw new UnsupportedOperationException();
		}

		/** {@inheritDoc}
		 */
		@Override
		public void removeModelListener(IXtextModelListener listener) {
			throw new UnsupportedOperationException();
		}

		/** {@inheritDoc}
		 */
		@Override
		public void addXtextDocumentContentObserver(IXtextDocumentContentObserver listener) {
			throw new UnsupportedOperationException();
		}

		/** {@inheritDoc}
		 */
		@Override
		public void removeXtextDocumentContentObserver(IXtextDocumentContentObserver listener) {
			throw new UnsupportedOperationException();
		}

	} // class TestXtextDocument

}
