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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.ui.tests.quickfix;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import javax.inject.Inject;

import org.arakhne.afc.text.TextUtil;
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
import org.eclipse.xtext.resource.SaveOptions;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.ui.editor.model.IXtextDocument;
import org.eclipse.xtext.ui.editor.model.IXtextDocumentContentObserver;
import org.eclipse.xtext.ui.editor.model.IXtextModelListener;
import org.eclipse.xtext.ui.editor.model.edit.IModification;
import org.eclipse.xtext.ui.editor.model.edit.IModificationContext;
import org.eclipse.xtext.ui.editor.quickfix.IssueResolution;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.util.concurrent.IUnitOfWork;
import org.eclipse.xtext.validation.Issue;
import org.junit.ComparisonFailure;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.ui.quickfix.SARLQuickfixProvider;
import io.sarl.tests.api.AbstractSarlUiTest;

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

	/** Create an object that permits to test the details of a quick fix.
	 *
	 * @param issueCode the code of the issue to test.
	 * @param invalidCode the code that is generating the issue.
	 * @return the quick fixes for assertions.
	 */
	protected QuickFixAsserts getQuickFixAsserts(
			String issueCode,
			String invalidCode) {
		return getQuickFixAsserts(issueCode, invalidCode, true);
	}

	/** Create an object that permits to test the details of a quick fix.
	 *
	 * @param issueCode the code of the issue to test.
	 * @param invalidCode the code that is generating the issue.
	 * @param failOnErrorInCode indicates if this function fails on error in the code.
	 * @return the quick fixes for assertions.
	 */
	protected QuickFixAsserts getQuickFixAsserts(
			String issueCode,
			String invalidCode,
			boolean failOnErrorInCode) {
		try {
			String baseName = issueCode.replaceAll("[^a-zA-Z0-9]+", "_"); //$NON-NLS-1$//$NON-NLS-2$
			String packageName = baseName.toLowerCase(); 
			baseName = baseName.substring(0, 1).toUpperCase() + baseName.substring(1);
			String filename = helper().generateFilename(
					"io", "sarl", "tests", "quickfix", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$//$NON-NLS-4$
					packageName, baseName);
			SarlScript script = helper().sarlScript(filename, invalidCode, failOnErrorInCode);
			Thread.yield();
			assertNotNull(script);
			Resource scriptResource = script.eResource();
			Thread.yield();

			List<Issue> issues = issues(script);
			StringBuilder issueLabels = new StringBuilder();
			Iterator<Issue> issueIterator = issues.iterator();
			List<IssueResolution> resolutions = new ArrayList<>();
			Issue issue = null;
			while (issueIterator.hasNext()) {
				Issue nextIssue = issueIterator.next();
				if (issueLabels.length() > 0) {
					issueLabels.append(","); //$NON-NLS-1$
					issueLabels.append(getLineSeparator());
				}
				issueLabels.append(nextIssue.getCode());
				issueLabels.append(" - \""); //$NON-NLS-1$
				issueLabels.append(Strings.convertToJavaString(nextIssue.getMessage()));
				issueLabels.append("\""); //$NON-NLS-1$
				if (issueCode.equals(nextIssue.getCode())) {
					issue = nextIssue;
					resolutions.addAll(this.quickfixProvider.getResolutions(issue));
				}
			}
			if (issueLabels.length() > 0) {
				issueLabels.append("."); //$NON-NLS-1$
				issueLabels.append(getLineSeparator());
			}
			if (issue == null) {
				fail("The issue '" + issueCode //$NON-NLS-1$
						+ "' was not found.\nAvailable issues are: " //$NON-NLS-1$
						+ issueLabels.toString());
				return null;
			}
			if (resolutions.isEmpty()) {
				fail("No resolution found for the issue '" + issueCode //$NON-NLS-1$
					+ "'."); //$NON-NLS-1$
				return null;
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
	 * @param issueCode the code of the issue to test.
	 * @param invalidCode the code that is generating the issue.
	 * @param expectedLabel the expected label for the quick fix.
	 * @param expectedResolution the expected code after fixing.
	 * @param expectedResolution2 the expected code after fixing.
	 * @return the matching resolved text.
	 */
	protected String assertQuickFix(
			String issueCode,
			String invalidCode,
			String expectedLabel,
			String expectedResolution,
			String... expectedResolution2) {
		return assertQuickFix(false, issueCode, invalidCode, expectedLabel, expectedResolution,
				expectedResolution2);
	}

	/** Assert the quick fix is changing the document in the expected way.
	 *
	 * @param issueCode the code of the issue to test.
	 * @param invalidCode the code that is generating the issue.
	 * @param expectedLabel the expected label for the quick fix.
	 * @param expectedResolution the expected code after fixing.
	 * @param expectedResolution2 the expected code after fixing.
	 * @return the matching resolved text.
	 */
	protected String assertQuickFixWithErrors(
			String issueCode,
			String invalidCode,
			String expectedLabel,
			String expectedResolution,
			String... expectedResolution2) {
		return assertQuickFixWithErrors(false, issueCode, invalidCode, expectedLabel, expectedResolution,
				expectedResolution2);
	}
	
	/** Assert the quick fix is changing the document in the expected way.
	 *
	 * @param changeOriginalDocument change the original document.
	 * @param issueCode the code of the issue to test.
	 * @param invalidCode the code that is generating the issue.
	 * @param expectedLabel the expected label for the quick fix.
	 * @param expectedResolution the expected code after fixing.
	 * @param expectedResolution2 the expected code after fixing.
	 * @return the matching resolved text.
	 */
	protected String assertQuickFix(
			boolean changeOriginalDocument,
			String issueCode,
			String invalidCode,
			String expectedLabel,
			String expectedResolution,
			String... expectedResolution2) {
		QuickFixAsserts asserts = getQuickFixAsserts(issueCode, invalidCode);
		String[] expected = new String[expectedResolution2.length + 1];
		expected[0] = expectedResolution;
		System.arraycopy(expectedResolution2, 0, expected, 1, expectedResolution2.length);
		return asserts.assertQuickFix(changeOriginalDocument, expectedLabel, expected);
	}

	/** Assert the quick fix is changing the document in the expected way.
	 *
	 * @param changeOriginalDocument change the original document.
	 * @param issueCode the code of the issue to test.
	 * @param invalidCode the code that is generating the issue.
	 * @param expectedLabel the expected label for the quick fix.
	 * @param expectedResolution the expected code after fixing.
	 * @param expectedResolution2 the expected code after fixing.
	 * @return the matching resolved text.
	 */
	protected String assertQuickFixWithErrors(
			boolean changeOriginalDocument,
			String issueCode,
			String invalidCode,
			String expectedLabel,
			String expectedResolution,
			String... expectedResolution2) {
		QuickFixAsserts asserts = getQuickFixAsserts(issueCode, invalidCode, false);
		String[] expected = new String[expectedResolution2.length + 1];
		expected[0] = expectedResolution;
		System.arraycopy(expectedResolution2, 0, expected, 1, expectedResolution2.length);
		return asserts.assertQuickFix(changeOriginalDocument, expectedLabel, expected);
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

		/** Replies the resolution that corresponds to the given label.
		 *
		 * @param label the label of the resolution to search for.
		 * @param failIfNotFound fails if no resolution found.
		 * @param removeWhenFound indicates if the resolution must be removed for the list of the resolutions
		 * when it was found.
		 * @return the resolution or {@code null}.
		 */
		public IssueResolution findResolution(String label, boolean failIfNotFound, boolean removeWhenFound) {
			Iterator<IssueResolution> iterator = this.resolutions.iterator();
			String close = null;
			int distance = Integer.MAX_VALUE;
			while (iterator.hasNext()) {
				IssueResolution resolution = iterator.next();
				String resolutionLabel = resolution.getLabel();
				int d = TextUtil.getLevenshteinDistance(resolutionLabel, label);
				if (d == 0) {
					if (removeWhenFound) {
						iterator.remove();
					}
					return resolution;
				} else if (close == null || d < distance) {
					distance = d;
					close = resolutionLabel;
				}
			}
			if (failIfNotFound) {
				throw new ComparisonFailure(
						"Quick fix not found for the issue '" + this.issueCode + "'.", //$NON-NLS-1$//$NON-NLS-2$
						label, close);
			}
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
		
		/** Replies the number of resolutions.
		 *
		 * @return the number of resolutions.
		 */
		public int getResolutionCount() {
			return this.resolutions.size();
		}

		private static String unifiesNewLineCharacters(String content) {
			String result = Strings.emptyIfNull(content);
			result = result.replaceAll("\r\n", "\n"); //$NON-NLS-1$ //$NON-NLS-2$
			result = result.replaceAll("\r", "\n"); //$NON-NLS-1$ //$NON-NLS-2$
			return result.trim();
		}

		/** Test the existence of a valid quick fix.
		 *
		 * @param expectedLabel the expected label for the quick fix.
		 * @param expectedResolutions the expected codes after fixing.
		 * @return the matching resolved text.
		 */
		public String assertQuickFix(
				String expectedLabel,
				String... expectedResolutions) {
			return assertQuickFix(false, expectedLabel, expectedResolutions);
		}

		/** Test the existence of a valid quick fix.
		 *
		 * @param changeOriginalDocument change the original document
		 * @param expectedLabel the expected label for the quick fix.
		 * @param expectedResolutions the expected codes after fixing.
		 * @return the matching resolved text.
		 */
		public String assertQuickFix(
				boolean changeOriginalDocument,
				String expectedLabel,
				String... expectedResolutions) {
			try {
				assert (expectedResolutions.length > 0);

				IssueResolution resolution = findResolution(expectedLabel, true, true);

				assertEquals(expectedLabel, resolution.getLabel());

				String newContent;

				if (changeOriginalDocument) {
					resolution.apply();
					IXtextDocument document = resolution.getModificationContext().getXtextDocument();
					newContent = document.get();
				} else {
					XtextResource xtextResource = new XtextResource(this.scriptResource.getURI());
					xtextResource.setFragmentProvider(new TestFragmentProvider(this.scriptResource));
					TestXtextDocument document = new TestXtextDocument(xtextResource, this.invalidCode);
					TestModificationContext modificationContext = new TestModificationContext(document);

					String oldContent = Objects.toString(document);

					final IModification modification = resolution.getModification();

					modification.apply(modificationContext);

					newContent = document.toString();
					newContent = Objects.toString(newContent).trim();
					if (Objects.equals(newContent, oldContent)) {
						// Save the resource for ensuring EMF changes are comitted.
						try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
							SaveOptions options = new SaveOptions(false, false) {
								//
							};
							this.scriptResource.save(baos, options.toOptionsMap());
							baos.flush();
							newContent = baos.toString();
						}
						newContent = Objects.toString(newContent);
					}
				}

				newContent = unifiesNewLineCharacters(newContent);

				final Set<String> expected = new TreeSet<>();
				for (final String expectedResolution : expectedResolutions) {
					final String ex = unifiesNewLineCharacters(expectedResolution);
					expected.add(ex);
				}

				String closeResolution = null;
				int distance = Integer.MAX_VALUE;
				for (String expectedResolution : expected) {
					int d = TextUtil.getLevenshteinDistance(expectedResolution, newContent);
					if (d == 0) {
						return newContent;
					}
					if (closeResolution == null || d < distance) {
						distance = d;
						closeResolution = expectedResolution;
					}
				}
				throw new ComparisonFailure(
						"Invalid quick fix", //$NON-NLS-1$
						closeResolution,
						newContent);
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
		 * @param initialContent initialContent
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
		private final static String[] STR_DELIMITERS = { System.getProperty("line.separator"), "\r", "\n" }; //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

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
