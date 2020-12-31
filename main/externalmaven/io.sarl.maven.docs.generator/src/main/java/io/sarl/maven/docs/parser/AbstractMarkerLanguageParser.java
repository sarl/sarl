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

package io.sarl.maven.docs.parser;

import java.io.File;
import java.io.Reader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.inject.Inject;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.maven.docs.Messages;
import io.sarl.maven.docs.parser.SarlDocumentationParser.Tag;

/** Abstract parser for all the marker languages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractMarkerLanguageParser {

	/** List of the filename extensions that corresponds to HTML files.
	 */
	public static final String[] HTML_FILE_EXTENSIONS = new String[] {
		".html", ".htm",  //$NON-NLS-1$//$NON-NLS-2$
	};

	private SarlDocumentationParser parser;

	private boolean githubExtension;

	private static String select(String str1, String str2) {
		if (Strings.isEmpty(str1)) {
			return str2;
		}
		return str1;
	}

	/** Parse a range of integers.
	 *
	 * <p>The supported formats are:<ul>
	 * <li>{@code n} for exactly {@code n},</li>
	 * <li>{@code n-m} for {@code n} to {@code m},</li>
	 * <li>{@code n-} for {@code n} to infinity,</li>
	 * <li>{@code -m} for 1 to {@code n}.</li>
	 * </ul>
	 *
	 * @param stringRange the string representation to parse.
	 * @param minValue the minimal value in the possible values for the range.
	 * @return the integer range.
	 */
	public static IntegerRange parseRange(String stringRange, int minValue) {
		final String sepPattern = "[,;\\-:]"; //$NON-NLS-1$
		try {
			final Matcher matcher = Pattern.compile("^\\s*" //$NON-NLS-1$
					+ "(?:(?<left>[0-9]+)\\s*(?:(?<sep1>" + sepPattern + ")\\s*(?<right1>[0-9]+)?)?)" //$NON-NLS-1$ //$NON-NLS-2$
					+ "|(?:(?<sep2>" + sepPattern + ")\\s*(?<right2>[0-9]+))" //$NON-NLS-1$ //$NON-NLS-2$
					+ "\\s*$") //$NON-NLS-1$
					.matcher(stringRange);
			if (matcher.matches()) {
				final String left = matcher.group("left"); //$NON-NLS-1$
				final String sep = select(matcher.group("sep1"), matcher.group("sep2")); //$NON-NLS-1$ //$NON-NLS-2$
				final String right = select(matcher.group("right1"), matcher.group("right2")); //$NON-NLS-1$ //$NON-NLS-2$
				if (Strings.isEmpty(left)) {
					if (!Strings.isEmpty(sep) && !Strings.isEmpty(right)) {
						return new IntegerRange(minValue, Math.max(minValue, Integer.valueOf(right)));
					}
				} else {
					final int leftValue = Math.max(minValue, Integer.valueOf(left));
					if (Strings.isEmpty(sep)) {
						return new IntegerRange(leftValue, leftValue);
					}
					if (Strings.isEmpty(right)) {
						return new IntegerRange(leftValue, Integer.MAX_VALUE);
					}
					final int rightValue = Math.max(minValue, Integer.valueOf(right));
					if (rightValue < leftValue) {
						return new IntegerRange(rightValue, leftValue);
					}
					return new IntegerRange(leftValue, rightValue);
				}
			}
		} catch (Throwable exception) {
			throw new IllegalArgumentException(MessageFormat.format(Messages.GenerateMojo_4, stringRange), exception);
		}
		throw new IllegalArgumentException(MessageFormat.format(Messages.GenerateMojo_4, stringRange));
	}

	/** Replies if the Github extension should be applied.
	 *
	 * @return {@code true} if the Github extension is supported.
	 */
	public boolean isGithubExtensionEnable() {
		return this.githubExtension;
	}

	/** Set the flag that indicates if the Github extension should be applied.
	 *
	 * @param enable {@code true} if the Github extension is supported.
	 */
	public void setGithubExtensionEnable(boolean enable) {
		this.githubExtension = enable;
	}

	/** Replies if the given extension is for HTML file.
	 *
	 * @param extension the extension to test.
	 * @return {@code true} if the extension is for a HTML file.
	 */
	public static boolean isHtmlFileExtension(String extension) {
		for (final String ext : HTML_FILE_EXTENSIONS) {
			if (Strings.equal(ext, extension)) {
				return true;
			}
		}
		return false;
	}

	/** Change the document parser.
	 *
	 * @param parser the documentation parser.
	 */
	@Inject
	public void setDocumentParser(SarlDocumentationParser parser) {
		assert parser != null;
		this.parser = parser;
		this.parser.reset();
	}

	/** Replies the document parser.
	 *
	 * @return the documentation parser.
	 */
	public SarlDocumentationParser getDocumentParser() {
		return this.parser;
	}

	/** Extract the page title from the given content.
	 *
	 * @param content the content of the page.
	 * @return the page title
	 */
	public abstract String extractPageTitle(String content);

	/** Read the given file and transform its content in order to have a raw text.
	 *
	 * @param inputFile the input file.
	 * @return the raw file content.
	 */
	public final String transform(File inputFile) {
		return transform(inputFile, true);
	}

	/** Read the given input stream and transform its content in order to have a raw text.
	 *
	 * @param reader the input stream.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @return the raw file content.
	 */
	public final String transform(Reader reader, File inputFile) {
		return transform(reader, inputFile, true);
	}

	/** Read the given input content and transform it in order to have a raw text.
	 *
	 * @param content the content to parse.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @return the raw file content.
	 */
	public final String transform(CharSequence content, File inputFile) {
		return transform(content, inputFile, true);
	}

	/** Read the given file and transform its content in order to have a raw text.
	 *
	 * @param inputFile the input file.
	 * @param validationOfInternalLinks indicates if the internal links should be validated.
	 * @return the raw file content.
	 */
	public String transform(File inputFile, boolean validationOfInternalLinks) {
		preProcessingTransformation(null, inputFile, validationOfInternalLinks);
		final String rawContent = getDocumentParser().transform(inputFile);
		return postProcessingTransformation(rawContent, validationOfInternalLinks);
	}

	/** Read the given input stream and transform its content in order to have a raw text.
	 *
	 * @param reader the input stream.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @param validationOfInternalLinks indicates if the internal links should be validated.
	 * @return the raw file content.
	 */
	public String transform(Reader reader, File inputFile, boolean validationOfInternalLinks) {
		preProcessingTransformation(null, inputFile, validationOfInternalLinks);
		final String rawContent = getDocumentParser().transform(reader, inputFile);
		return postProcessingTransformation(rawContent, validationOfInternalLinks);
	}

	/** Read the given input content and transform it in order to have a raw text.
	 *
	 * @param content the content to parse.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @param validationOfInternalLinks indicates if the internal links should be validated.
	 * @return the file content.
	 */
	public String transform(CharSequence content, File inputFile, boolean validationOfInternalLinks) {
		preProcessingTransformation(content, inputFile, validationOfInternalLinks);
		final String rawContent = getDocumentParser().transform(content, inputFile);
		return postProcessingTransformation(rawContent, validationOfInternalLinks);
	}

	/** Pre processing of a transformation.
	 *
	 * @param content the preloaded content.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @param validationOfInternalLinks indicates if the internal links should be validated.
	 */
	protected void preProcessingTransformation(CharSequence content, File inputFile, boolean validationOfInternalLinks) {
		//
	}

	/** Post processing of the content for a transformation.
	 *
	 * @param content the extracted content.
	 * @param validationOfInternalLinks indicates if the internal links should be validated.
	 * @return the post processing result.
	 */
	protected abstract String postProcessingTransformation(String content, boolean validationOfInternalLinks);

	/** Extract the validation components from the given file.
	 *
	 * @param inputFile the input file.
	 * @return the validation components.
	 */
	public Iterable<ValidationComponent> getStandardValidationComponents(File inputFile) {
		final ValidationHandler handler = new ValidationHandler();
		final SarlDocumentationParser parser = getDocumentParser();
		parser.extractValidationComponents(inputFile, handler);
		return handler.getComponents();
	}

	/** Extract the validation components from the given file.
	 *
	 * @param content the content to parse.
	 * @param inputFile the input file.
	 * @return the validation components.
	 */
	public Iterable<ValidationComponent> getStandardValidationComponents(CharSequence content, File inputFile) {
		final ValidationHandler handler = new ValidationHandler();
		getDocumentParser().extractValidationComponents(content, inputFile, handler);
		return handler.getComponents();
	}

	/** Extract the validation components that are specific to the marker language.
	 *
	 * @param inputFile the input file.
	 * @param rootFolder the root folder in which the input file is located.
	 * @param context the generation context.
	 * @return the validation components.
	 */
	public final List<DynamicValidationComponent> getMarkerSpecificValidationComponents(File inputFile,
			File rootFolder,
			DynamicValidationContext context) {
		return getSpecificValidationComponents(
				transform(inputFile, false),
				inputFile,
				rootFolder,
				context);
	}

	/** Extract the validation components that are specific to the marker language.
	 *
	 * @param text the text to validate.
	 * @param inputFile the input file.
	 * @param rootFolder the root folder in which the input file is located.
	 * @param context the generation context.
	 * @return the validation components.
	 */
	public final List<DynamicValidationComponent> getMarkerSpecificValidationComponents(String text, File inputFile,
			File rootFolder,
			DynamicValidationContext context) {
		return getSpecificValidationComponents(
				transform(inputFile, false),
				inputFile,
				rootFolder,
				context);
	}

	/** Extract the validation components that are specific to the marker language.
	 *
	 * @param text the text to validate.
	 * @param inputFile the input file.
	 * @param rootFolder the root folder in which the file is located.
	 * @param context the generation context.
	 * @return the validation components.
	 */
	protected abstract List<DynamicValidationComponent> getSpecificValidationComponents(String text, File inputFile,
			File rootFolder, DynamicValidationContext context);

	/** Validation handler.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private static class ValidationHandler implements Procedure1<Map<Tag, List<ValidationComponentData>>> {

		private final List<ValidationComponent> components = new ArrayList<>();

		ValidationHandler() {
			//
		}

		/** Replies the components.
		 *
		 * @return the components.
		 */
		public List<ValidationComponent> getComponents() {
			return this.components;
		}

		@Override
		public void apply(Map<Tag, List<ValidationComponentData>> it) {
			for (final Entry<Tag, List<ValidationComponentData>> entry : it.entrySet()) {
				final boolean isCompilable;
				final boolean isExecutable;
				switch (entry.getKey()) {
				case SUCCESS:
					isCompilable = true;
					isExecutable = false;
					break;
				case FAILURE:
					isCompilable = false;
					isExecutable = false;
					break;
				case FACT:
					isCompilable = true;
					isExecutable = true;
					break;
					//$CASES-OMITTED$
				default:
					continue;
				}
				for (final ValidationComponentData code : entry.getValue()) {
					final ValidationComponent component = new ValidationComponent();
					component.setCompilable(isCompilable);
					component.setExecutable(isExecutable);
					component.setSourceFile(code.file);
					component.setOffsetInSourceFile(code.offset);
					component.setLengthInSourceFile(code.length);
					component.setLinenoInSourceFile(code.lineno);
					component.setEndLinenoInSourceFile(code.endLineno);
					component.setCode(code.code);
					this.components.add(component);
				}
			}
		}

	}

	/** Context that describes the references.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.7
	 */
	public static class ReferenceContext {

		private static final String MANY = new String();

		private Map<String, String> anchorToTitle = new TreeMap<>();

		private Map<String, String> simpleAnchorToAnchor = new TreeMap<>();

		private Map<String, String> titleToAnchor = new TreeMap<>();

		/** Register a section.
		 *
		 * @param keyWithSectionNumber the key of the section with the section number.
		 * @param keyWithoutSectionNumber the key of the section without the section number.
		 * @param title the title of the section.
		 */
		public void registerSection(String keyWithSectionNumber, String keyWithoutSectionNumber, String title) {
			this.anchorToTitle.put(keyWithSectionNumber, title);
			if (this.simpleAnchorToAnchor.containsKey(keyWithoutSectionNumber)) {
				this.simpleAnchorToAnchor.put(keyWithoutSectionNumber, MANY);
			} else {
				this.simpleAnchorToAnchor.put(keyWithoutSectionNumber, keyWithSectionNumber);
			}
			if (this.titleToAnchor.containsKey(title)) {
				this.titleToAnchor.put(title, MANY);
			} else {
				this.titleToAnchor.put(title, keyWithSectionNumber);
			}
		}

		/** Validate the given anchor and reply the one that is validated.
		 *
		 * @param anchor the anchor to validate.
		 * @param line the number of the line at which the anchor is located into the file.  If negative value, the line number is unknown.
		 * @return the validated anchor text.
		 * @throws InvalidAnchorLabelException a runtime exception
		 * @since 0.12
		 */
		public String validateAnchor(String anchor, int line) {
			if (!this.anchorToTitle.containsKey(anchor)) {
				String anc = this.simpleAnchorToAnchor.get(anchor);
				if (!Strings.isEmpty(anc) && anc != MANY) {
					return anc;
				}
				anc = this.titleToAnchor.get(anchor);
				if (!Strings.isEmpty(anc) && anc != MANY) {
					return anc;
				}
				final String[] anchors = new String[this.anchorToTitle.size()];
				int i = 0;
				for (final String eanchor : this.anchorToTitle.keySet()) {
					anchors[i] = eanchor;
					++i;
				}
				Arrays.sort(anchors);
				throw new InvalidAnchorLabelException(anchor, line, anchors);
			}
			return anchor;
		}

		@Override
		public String toString() {
			final StringBuilder b = new StringBuilder();
			for (final String key : this.anchorToTitle.keySet()) {
				b.append(key);
				b.append("\n"); //$NON-NLS-1$
			}
			return b.toString();
		}

	}

}
