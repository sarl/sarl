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

package io.sarl.maven.docs.markdown;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.inject.Inject;

import com.google.common.collect.Iterables;
import com.vladsch.flexmark.ast.Heading;
import com.vladsch.flexmark.ast.Image;
import com.vladsch.flexmark.ast.Link;
import com.vladsch.flexmark.ast.Paragraph;
import com.vladsch.flexmark.parser.Parser;
import com.vladsch.flexmark.util.ast.Node;
import com.vladsch.flexmark.util.ast.NodeVisitor;
import com.vladsch.flexmark.util.ast.VisitHandler;
import com.vladsch.flexmark.util.data.MutableDataSet;
import com.vladsch.flexmark.util.sequence.BasedSequence;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.URISchemeType;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.IntegerRange;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import io.sarl.maven.docs.bugfixes.FileSystemAddons;
import io.sarl.maven.docs.parser.AbstractMarkerLanguageParser;
import io.sarl.maven.docs.parser.DynamicValidationComponent;
import io.sarl.maven.docs.parser.DynamicValidationContext;
import io.sarl.maven.docs.parser.SarlDocumentationParser;
import io.sarl.maven.docs.parser.SectionNumber;
import io.sarl.maven.docs.testing.ReflectExtensions;

/** Markdown parser.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class MarkdownParser extends AbstractMarkerLanguageParser {

	/** List of the filename extensions that corresponds to Markdown files.
	 */
	public static final String[] MARKDOWN_FILE_EXTENSIONS = new String[] {
		".md", ".markdown", ".mdown", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		".mkdn", ".mkd", ".mdwn", //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		".mdtxt", ".mdtext", //$NON-NLS-1$//$NON-NLS-2$
	};

	/** Default level at which the titles may appear in the outline.
	 */
	public static final int DEFAULT_OUTLINE_TOP_LEVEL = 2;

	/** Indicates if the sections should be numbered by default.
	 */
	public static final boolean DEFAULT_SECTION_NUMBERING = true;

	/** Indicates if a hyperlinks to the operation should be created for each operation name,
	 * that is generated.
	 */
	public static final boolean DEFAULT_ADD_LINK_TO_OPERATION_NAME = true;

	/** Indicates the default name of the style for the outline.
	 */
	public static final String DEFAULT_OUTLINE_STYLE_ID = "page_outline"; //$NON-NLS-1$

	/** The default format, compatible with {@link MessageFormat} for the section titles.
	 */
	public static final String DEFAULT_SECTION_TITLE_FORMAT = "{0} {1}. {2}"; //$NON-NLS-1$

	/** The default format, compatible with {@link MessageFormat} for the outline entry without auto-numbering.
	 */
	public static final String DEFAULT_OUTLINE_ENTRY_WO_AUTONUMBERING = "{0} [{1}](#{2})"; //$NON-NLS-1$

	/** The default format, compatible with {@link MessageFormat} for the outline entry with auto-numbering.
	 */
	public static final String DEFAULT_OUTLINE_ENTRY_W_AUTONUMBERING = "{0} [{1}. {2}](#{3})"; //$NON-NLS-1$

	/** Name of the property that contains the pattern for information notes.
	 * @since 0.12
	 */
	public static final String INFO_NOTE_PATTERN_PROPERTY = "io.sarl.maven.docs.generator.infonote";

	/** Name of the property that contains the pattern for warning notes.
	 * @since 0.12
	 */
	public static final String WARNING_NOTE_PATTERN_PROPERTY = "io.sarl.maven.docs.generator.warningnote";

	/** Name of the property that contains the pattern for danger notes.
	 * @since 0.12
	 */
	public static final String DANGER_NOTE_PATTERN_PROPERTY = "io.sarl.maven.docs.generator.dangernote";

	private static final String MARKDOWN_INFORMATION_NOTE_PATTERN1 = "^\\>[ \t\n\r]*\\*\\*\\_(.*?):?\\_\\*\\*(.*)$";

	private static final String MARKDOWN_INFORMATION_NOTE_PATTERN2 = "^\\>(.*)$";

	private static final String SECTION_PATTERN_AUTONUMBERING =
			"^([#]+)\\s*([0-9]+(?:\\.[0-9]+)*\\.?)?\\s*(.*?)\\s*(?:\\{\\s*([a-z\\-]+)\\s*\\})?\\s*$"; //$NON-NLS-1$

	private static final String SECTION_PATTERN_NO_AUTONUMBERING =
			"^([#]+)\\s*(.*?)\\s*(?:\\{\\s*([a-z\\-]+)\\s*\\})?\\s*$"; //$NON-NLS-1$

	private static final String SECTION_PATTERN_TITLE_EXTRACTOR =
			"^(?:[#]+)\\s*((?:[0-9]+(?:\\.[0-9]+)*\\.?)?\\s*.*?\\s*(?:\\{\\s*([a-z\\-]+)\\s*\\})?)\\s*$"; //$NON-NLS-1$

	private static final String SECTION_PATTERN_TITLE_EXTRACTOR_WITHOUT_MD_PREFIX =
			"^\\s*([0-9]+(?:\\.[0-9]+)*\\.?)\\s*(.*?)$"; //$NON-NLS-1$

	private static final Set<String> WARNING_LABELS = new TreeSet<>();

	private static final Set<String> DANGER_LABELS = new TreeSet<>();

	private IntegerRange outlineDepthRange = new IntegerRange(DEFAULT_OUTLINE_TOP_LEVEL, DEFAULT_OUTLINE_TOP_LEVEL);

	private boolean addLinkToOperationName = DEFAULT_ADD_LINK_TO_OPERATION_NAME;

	private boolean sectionNumbering = DEFAULT_SECTION_NUMBERING;

	private String sectionTitleFormat = DEFAULT_SECTION_TITLE_FORMAT;

	private String sectionNumberFormat = SectionNumber.DEFAULT_SECTION_NUMBER_FORMAT;

	private String outlineEntryWithNumberFormat = DEFAULT_OUTLINE_ENTRY_W_AUTONUMBERING;

	private String outlineEntryWithoutNumberFormat = DEFAULT_OUTLINE_ENTRY_WO_AUTONUMBERING;

	private String outlineStyleId = DEFAULT_OUTLINE_STYLE_ID;

	private boolean localFileReferenceValidation = true;

	private boolean remoteReferenceValidation = true;

	private boolean localImageReferenceValidation = true;

	private boolean transformMdToHtmlReferences = true;

	private boolean transformPureHtmlReferences = true;

	private boolean generateOutline = true;

	private String externalOutlineMarker;

	private boolean kramdown;

	static {
		for (final String label : Messages.WARNING.split(",")) {
			WARNING_LABELS.add(label.trim().toLowerCase());
		}
		for (final String label : Messages.DANGER.split(",")) {
			DANGER_LABELS.add(label.trim().toLowerCase());
		}
	}

	@Override
	@Inject
	public void setDocumentParser(SarlDocumentationParser parser) {
		super.setDocumentParser(parser);
		updateBlockFormatter();
	}

	@Override
	public void setGithubExtensionEnable(boolean enable) {
		super.setGithubExtensionEnable(enable);
		updateBlockFormatter();
	}

	private void updateBlockFormatter() {
		final Function2<String, String, String> formatter;
		if (isGithubExtensionEnable()) {
			formatter = SarlDocumentationParser.getFencedCodeBlockFormatter();
		} else {
			formatter = SarlDocumentationParser.getBasicCodeBlockFormatter();
		}
		getDocumentParser().setBlockCodeTemplate(formatter);
	}

	@Override
	public String extractPageTitle(String content) {
		final Pattern sectionPattern = Pattern.compile(
				isAutoSectionNumbering() ? SECTION_PATTERN_AUTONUMBERING : SECTION_PATTERN_NO_AUTONUMBERING,
						Pattern.MULTILINE);
		final Matcher matcher = sectionPattern.matcher(content);
		final IntegerRange depthRange = getOutlineDepthRange();
		final int titleGroupId;
		if (isAutoSectionNumbering()) {
			titleGroupId = 3;
		} else {
			titleGroupId = 2;
		}
		while (matcher.find()) {
			final String prefix = matcher.group(1);
			final int clevel = prefix.length();
			if (clevel < depthRange.getStart()) {
				final String title = matcher.group(titleGroupId);
				if (!Strings.isEmpty(title)) {
					return title;
				}
			}
		}
		return null;
	}

	/** Replies the style identifier that should be used for rendering the outline.
	 *
	 * <p>If an identifier exists, the outline will be enclosing by an HTML div tag with
	 * the class and id attributes set to the value.
	 *
	 * @return the outline style identifier.
	 */
	public String getOutlineStyleId() {
		return this.outlineStyleId;
	}

	/** Change the style identifier that should be used for rendering the outline.
	 *
	 * <p>If an identifier exists, the outline will be enclosing by an HTML div tag with
	 * the class and id attributes set to the value.
	 *
	 * @param id the outline style identifier.
	 */
	public void setOutlineStyleId(String id) {
		this.outlineStyleId = id;
	}

	/** Replies if the references to the Markdown files should be transform to references to HTML pages.
	 *
	 * @return {@code true} if the references should be validated.
	 * @see #isPureHtmlReferenceTransformation()
	 */
	public boolean isMarkdownToHtmlReferenceTransformation() {
		return this.transformMdToHtmlReferences;
	}

	/** Change the flag that indicates if the references the Markdown files should be transform to references to HTML pages.
	 *
	 * @param transform {@code true} if the references should be validated.
	 * @see #setPureHtmlReferenceTransformation(boolean)
	 */
	public void setMarkdownToHtmlReferenceTransformation(boolean transform) {
		this.transformMdToHtmlReferences = transform;
	}

	/** Replies if the pure HTML references (in "a" tags) should be transform to references to HTML pages.
	 *
	 * @return {@code true} if the references should be validated.
	 * @see #isMarkdownToHtmlReferenceTransformation()
	 */
	public boolean isPureHtmlReferenceTransformation() {
		return this.transformPureHtmlReferences;
	}

	/** Change the flag that indicates if the pure html references should be transform to references to HTML pages.
	 *
	 * @param transform {@code true} if the references should be validated.
	 * @see #setMarkdownToHtmlReferenceTransformation(boolean)
	 */
	public void setPureHtmlReferenceTransformation(boolean transform) {
		this.transformPureHtmlReferences = transform;
	}

	/** Replies if the references to the local files should be validated.
	 *
	 * @return {@code true} if the references to the local files should be validated.
	 */
	public boolean isLocalFileReferenceValidation() {
		return this.localFileReferenceValidation;
	}

	/** Change the flag that indicates if the references to the local files should be validated.
	 *
	 * @param validate {@code true} if the references to the local files should be validated.
	 */
	public void setLocalFileReferenceValidation(boolean validate) {
		this.localFileReferenceValidation = validate;
	}

	/** Replies if the references to the remote Internet pages should be validated.
	 *
	 * @return {@code true} if the references to the local files should be validated.
	 */
	public boolean isRemoteReferenceValidation() {
		return this.remoteReferenceValidation;
	}

	/** Change the flag that indicates if the references to the remote Internet pages should be validated.
	 *
	 * @param validate {@code true} if the references to the remote Internet pages should be validated.
	 */
	public void setRemoteReferenceValidation(boolean validate) {
		this.remoteReferenceValidation = validate;
	}

	/** Replies if the references to the local images should be validated.
	 *
	 * @return {@code true} if the references to the local images should be validated.
	 */
	public boolean isLocalImageReferenceValidation() {
		return this.localImageReferenceValidation;
	}

	/** Change the flag that indicates if the references to the local images should be validated.
	 *
	 * @param validate {@code true} if the references to the local images should be validated.
	 */
	public void setLocalImageReferenceValidation(boolean validate) {
		this.localImageReferenceValidation = validate;
	}

	/** Change the formats to be applied to the outline entries.
	 *
	 * <p>The format must be compatible with {@link MessageFormat}.
	 *
	 * <p>If section auto-numbering is on,
	 * the first parameter <code>{0}</code> equals to the prefix,
	 * the second parameter <code>{1}</code> equals to the string representation of the section number,
	 * the third parameter <code>{2}</code> equals to the title text, and the fourth parameter
	 * <code>{3}</code> is the reference id of the section.
	 *
	 * <p>If section auto-numbering is off,
	 * the first parameter <code>{0}</code> equals to the prefix,
	 * the second parameter <code>{1}</code> equals to the title text, and the third parameter
	 * <code>{2}</code> is the reference id of the section.
	 *
	 * @param formatWithoutNumbers the format for the outline entries without section numbers.
	 * @param formatWithNumbers the format for the outline entries with section numbers.
	 */
	public void setOutlineEntryFormat(String formatWithoutNumbers, String formatWithNumbers) {
		if (!Strings.isEmpty(formatWithoutNumbers)) {
			this.outlineEntryWithoutNumberFormat = formatWithoutNumbers;
		}
		if (!Strings.isEmpty(formatWithNumbers)) {
			this.outlineEntryWithNumberFormat = formatWithNumbers;
		}
	}

	/** Replies the format to be applied to the outline entries.
	 *
	 * <p>The format must be compatible with {@link MessageFormat}.
	 *
	 * <p>If section auto-numbering is on,
	 * the first parameter <code>{0}</code> equals to the prefix,
	 * the second parameter <code>{1}</code> equals to the string representation of the section number,
	 * the third parameter <code>{2}</code> equals to the title text, and the fourth parameter
	 * <code>{3}</code> is the reference id of the section.
	 *
	 * <p>If section auto-numbering is off,
	 * the first parameter <code>{0}</code> equals to the prefix,
	 * the second parameter <code>{1}</code> equals to the title text, and the third parameter
	 * <code>{2}</code> is the reference id of the section.
	 *
	 * @return the format.
	 */
	public String getOutlineEntryFormat() {
		return isAutoSectionNumbering() ? this.outlineEntryWithNumberFormat : this.outlineEntryWithoutNumberFormat;
	}

	/** Change the format to be applied to the section titles.
	 *
	 * <p>The format must be compatible with {@link MessageFormat}, with
	 * the first parameter <code>{0}</code> equals to the Markdown prefix,
	 * the second parameter <code>{1}</code> equals to the string representation of the section number,
	 * the third parameter <code>{2}</code> equals to the title text, and the fourth parameter
	 * <code>{3}</code> is the identifier of the section.
	 *
	 * @param format the format.
	 */
	public void setSectionTitleFormat(String format) {
		if (!Strings.isEmpty(format)) {
			this.sectionTitleFormat = format;
		}
	}

	/** Replies the format to be applied to the section titles.
	 *
	 * <p>The format must be compatible with {@link MessageFormat}, with
	 * the first parameter <code>{0}</code> equals to the string representation of the section number,
	 * the second parameter <code>{1}</code> equals to the string representation of the section number,
	 * the third parameter <code>{2}</code> equals to the title text, and the fourth parameter
	 * <code>{3}</code> is the identifier of the section.
	 *
	 * @return the format.
	 */
	public String getSectionTitleFormat() {
		return this.sectionTitleFormat;
	}

	/** Change the format to be applied to the section numbers.
	 *
	 * <p>The format must be compatible with {@link MessageFormat}, with
	 * the first parameter <code>{0}</code> equals to the first part of the full section number, and
	 * the second parameter <code>{1}</code> equals to a single section number.
	 *
	 * @param format the format.
	 */
	public void setSectionNumberFormat(String format) {
		if (!Strings.isEmpty(format)) {
			this.sectionNumberFormat = format;
		}
	}

	/** Replies the format to be applied to the section numbers.
	 *
	 * <p>The format must be compatible with {@link MessageFormat}, with
	 * the first parameter <code>{0}</code> equals to the first part of the full section number, and
	 * the second parameter <code>{1}</code> equals to a single section number.
	 *
	 * @return the format.
	 */
	public String getSectionNumberFormat() {
		return this.sectionNumberFormat;
	}

	/** Set if the outline must be generated or not by this parser.
	 *
	 * @param enable {@code true} if the outline is automatically generated.
	 * @since 0.12
	 */
	public void setOutlineGeneration(boolean enable) {
		this.generateOutline = enable;
	}

	/** Replies if the outline must be generated or not by this parser.
	 *
	 * @return {@code true} if the outline is automatically generated.
	 * @since 0.12
	 */
	public boolean isOutlineGeneration() {
		return this.generateOutline;
	}

	/** Set the external marker that is used by a Markdown generator in order to
	 * automatically generate the outline.
	 *
	 * @param marker the external marker; or {@code null} if none.
	 * @since 0.12
	 */
	public void setOutlineExternalMarker(String marker) {
		this.externalOutlineMarker = marker;
	}

	/** Replies the external marker that is used by a Markdown generator in order to
	 * automatically generate the outline.
	 *
	 * @return the external marker; or {@code null} if none.
	 * @since 0.12
	 */
	public String getOutlineExternalMarker() {
		return this.externalOutlineMarker;
	}

	/** Change the flag that indicates if the bug fixes related to Kramdown are enabled.
	 * For example, the automatic section numbering is not working as expected into
	 * Kramdown; so that is should be fixed into our code.
	 *
	 * @param enable is {@code true} if the bug fixes are enabled.
	 * @since 0.12
	 */
	public void setKramdownFix(boolean enable) {
		this.kramdown = enable;
	}

	/** Replies if the bug fixes related to Kramdown are enabled.
	 *
	 * @return {@code true} if the bug fixes are enabled.
	 * @since 0.12
	 */
	public boolean isKramdownFix() {
		return this.kramdown;
	}

	/** Change the level at which the titles may appear in the outline.
	 *
	 * @param level the level, at least 1.
	 */
	public void setOutlineDepthRange(IntegerRange level) {
		if (level == null) {
			this.outlineDepthRange = new IntegerRange(DEFAULT_OUTLINE_TOP_LEVEL, DEFAULT_OUTLINE_TOP_LEVEL);
		} else {
			this.outlineDepthRange = level;
		}
	}

	/** Replies the level at which the titles may appear in the outline.
	 *
	 * @return the level, at least 1.
	 */
	public IntegerRange getOutlineDepthRange() {
		return this.outlineDepthRange;
	}

	/** Set if the sections are automatically numbered.
	 *
	 * @param enable {@code true} if the section are automatically numbered.
	 */
	public void setAutoSectionNumbering(boolean enable) {
		this.sectionNumbering = enable;
	}

	/** Replies if the sections are automatically numbered.
	 *
	 * @return {@code true} if the section are automatically numbered.
	 */
	public boolean isAutoSectionNumbering() {
		return this.sectionNumbering;
	}

	/** Chagne the flag for the creation of a hyperlink to the operation documentation
	 * to each generated operation name.
	 *
	 * @param enable {@code true} if the hyperlink is created.
	 */
	public void setAddLinkToOperationName(boolean enable) {
		this.addLinkToOperationName = enable;
	}

	/** Replies if a hyperlink to the operation documentation should be added to each generated operation name.
	 *
	 * @return {@code true} if the hyperlink is created.
	 */
	public boolean isAddLinkToOperationName() {
		return this.addLinkToOperationName;
	}

	/** Replies the hyperlink to the given operation.
	 *
	 * @param method the method.
	 * @return the link, or {@code null} if not found.
	 */
	@SuppressWarnings("static-method")
	protected URL findOperationLink(Method method) {
		return null;
	}

	@Override
	protected void preProcessingTransformation(CharSequence content, File inputFile, boolean validationOfInternalLinks) {
		Function<Method, String> formatter = null;
		if (isAddLinkToOperationName()) {
			formatter = it -> {
				final URL url = findOperationLink(it);
				if (url != null) {
					final StringBuilder name = new StringBuilder();
					name.append("[").append(it.getName()).append("]("); //$NON-NLS-1$//$NON-NLS-2$
					name.append(url.toExternalForm()).append(")"); //$NON-NLS-1$
					return name.toString();
				}
				return it.getName();
			};
		}
		ReflectExtensions.setDefaultNameFormatter(formatter);
	}

	@Override
	protected String postProcessingTransformation(String content, boolean validationOfInternalLinks) {
		String result = updateOutline(content);
		final ReferenceContext references = validationOfInternalLinks ? extractReferencableElements(result) : null;
		result = transformMardownLinks(result, references);
		result = transformHtmlLinks(result, references);
		result = transformInformationNotes(result);
		return result;
	}

	/** Transform the information notes into the given Markdown text.
	 *
	 * @param text the content to parse.
	 * @return the updated text.
	 * @since 0.12
	 */
	@SuppressWarnings("static-method")
	protected String transformInformationNotes(String text) {
		final String info = Strings.emptyIfNull(System.getProperty(INFO_NOTE_PATTERN_PROPERTY));
		final String warning = Strings.emptyIfNull(System.getProperty(WARNING_NOTE_PATTERN_PROPERTY));
		final String danger = Strings.emptyIfNull(System.getProperty(DANGER_NOTE_PATTERN_PROPERTY));
		if (Strings.isEmpty(info) && Strings.isEmpty(warning) && Strings.isEmpty(danger)) {
			return text;
		}

		final Pattern startPattern = Pattern.compile(MARKDOWN_INFORMATION_NOTE_PATTERN1);
		final Pattern continuePattern = Pattern.compile(MARKDOWN_INFORMATION_NOTE_PATTERN2);
		final StringBuilder result = new StringBuilder();
		String currentName = null;
		StringBuilder currentNote = null;
		for (final String line : text.split("\r*\n\r*")) {
			String newLine = null;
			if (currentName == null) {
				final Matcher matcher = startPattern.matcher(line);
				if (matcher.matches()) {
					currentName = matcher.group(1).trim();
					currentNote = new StringBuilder(matcher.group(2).trim());
				} else {
					currentName = null;
					currentNote = null;
					newLine = line;
				}
			} else {
				final Matcher matcher = continuePattern.matcher(line);
				if (matcher.matches()) {
					assert currentNote != null;
					currentNote.append(" ").append(matcher.group(1).trim());
				} else {
					updateBuffer(result, currentName, currentNote, info, warning, danger);
					currentName = null;
					currentNote = null;
					newLine = line;
				}
			}
			updateBuffer(result, newLine);
		}
		updateBuffer(result, currentName, currentNote, info, warning, danger);
		return result.toString();
	}

	private static void updateBuffer(StringBuilder result, String name, StringBuilder note, String info, String warning, String danger) {
		if (!Strings.isEmpty(name) && note != null) {
			final int type = parseType(name);
			switch (type) {
			case 0:
				if (!Strings.isEmpty(info)) {
					String res = info.replaceAll(Pattern.quote("$1"), Matcher.quoteReplacement(name));
					res = res.replaceAll(Pattern.quote("$2"), Matcher.quoteReplacement(note.toString()));
					updateBuffer(result, res);
				} else {
					updateBuffer(result, "> **_" + name + ":_** " + note);
				}
				break;
			case 1:
				if (!Strings.isEmpty(warning)) {
					String res = warning.replaceAll(Pattern.quote("$1"), Matcher.quoteReplacement(name));
					res = res.replaceAll(Pattern.quote("$2"), Matcher.quoteReplacement(note.toString()));
					updateBuffer(result, res);
				} else {
					updateBuffer(result, "> **_" + name + ":_** " + note);
				}
				break;
			case 2:
				if (!Strings.isEmpty(danger)) {
					String res = danger.replaceAll(Pattern.quote("$1"), Matcher.quoteReplacement(name));
					res = res.replaceAll(Pattern.quote("$2"), Matcher.quoteReplacement(note.toString()));
					updateBuffer(result, res);
				} else {
					updateBuffer(result, "> **_" + name + ":_** " + note);
				}
				break;
			default:
				break;
			}
		}
	}

	private static void updateBuffer(StringBuilder result, String line) {
		if (line != null) {
			result.append(line).append("\n");
		}
	}

	private static int parseType(String name) {
		final String label = name.toLowerCase();
		if (WARNING_LABELS.contains(label)) {
			return 1;
		}
		if (DANGER_LABELS.contains(label)) {
			return 2;
		}
		return 0;
	}

	/** Extract all the referencable objects from the given content.
	 *
	 * @param text the content to parse.
	 * @return the referencables objects
	 */
	@SuppressWarnings("static-method")
	protected ReferenceContext extractReferencableElements(String text) {
		final ReferenceContext context = new ReferenceContext();

		// Visit the links and record the transformations
		final MutableDataSet options = new MutableDataSet();
		final Parser parser = Parser.builder(options).build();
		final Node document = parser.parse(text);
		final Pattern pattern = Pattern.compile(SECTION_PATTERN_AUTONUMBERING);
		NodeVisitor visitor = new NodeVisitor(
				new VisitHandler<>(Paragraph.class, it -> {
					final CharSequence paragraphText = it.getContentChars();
					final Matcher matcher = pattern.matcher(paragraphText);
					if (matcher.find()) {
						final String number = matcher.group(2);
						final String title = matcher.group(3);
						final String key1 = computeHeaderId(number, title);
						final String key2 = computeHeaderId(null, title);
						context.registerSection(key1, key2, title);
					}
				}));
		visitor.visitChildren(document);

		final Pattern pattern1 = Pattern.compile(SECTION_PATTERN_TITLE_EXTRACTOR_WITHOUT_MD_PREFIX);
		visitor = new NodeVisitor(
				new VisitHandler<>(Heading.class, it -> {
					String key = it.getAnchorRefId();
					String title = it.getText().toString();
					// Sometimes, the title already contains the section number.
					// It should be removed.
					final Matcher matcher = pattern1.matcher(title);
					if (matcher.find()) {
						final String number = matcher.group(1);
						title = matcher.group(2);
						if (Strings.isEmpty(key)) {
							key = computeHeaderId(number, title);
						}
					}
					final String key2 = computeHeaderId(null, title);
					if (Strings.isEmpty(key)) {
						key = key2;
					}
					context.registerSection(key, key2, title);
				}));
		visitor.visitChildren(document);

		return context;
	}

	/** Apply link transformation on the HTML links.
	 *
	 * @param content the original content.
	 * @param references the references into the document.
	 * @return the result of the transformation.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	protected String transformHtmlLinks(String content, ReferenceContext references) {
		if (!isPureHtmlReferenceTransformation()) {
			return content;
		}

		// Prepare replacement data structures
		final Map<String, String> replacements = new TreeMap<>();

		// Visit the links and record the transformations
		final org.jsoup.select.NodeVisitor visitor = new org.jsoup.select.NodeVisitor() {
			@Override
			public void tail(org.jsoup.nodes.Node node, int depth) {
				//
			}

			@Override
			public void head(org.jsoup.nodes.Node node, int depth) {
				if (node instanceof Element) {
					final Element tag = (Element) node;
					if ("a".equals(tag.nodeName()) && tag.hasAttr("href")) { //$NON-NLS-1$ //$NON-NLS-2$
						final String href = tag.attr("href"); //$NON-NLS-1$
						if (!Strings.isEmpty(href)) {
							URL url = FileSystem.convertStringToURL(href, true);
							url = transformURL(url, -1, references);
							if (url != null) {
								final String newUrl = convertURLToString(url);
								if (!Strings.isEmpty(newUrl) && !Objects.equals(href, newUrl)) {
									replacements.put(href, newUrl);
								}
							}
						}
					}
				}
			}
		};
		final Document htmlDocument = Jsoup.parse(content);
		htmlDocument.traverse(visitor);

		// Apply the replacements
		if (!replacements.isEmpty()) {
			String buffer = content;
			for (final Entry<String, String> entry : replacements.entrySet()) {
				final String source = entry.getKey();
				final String dest = entry.getValue();
				buffer = buffer.replaceAll(Pattern.quote(source), Matcher.quoteReplacement(dest));
			}
			return buffer;
		}
		return content;
	}

	/** Apply link transformation on the Markdown links.
	 *
	 * @param content the original content.
	 * @param references the references into the document.
	 * @return the result of the transformation.
	 */
	protected String transformMardownLinks(String content, ReferenceContext references) {
		if (!isMarkdownToHtmlReferenceTransformation()) {
			return content;
		}

		// Prepare replacement data structures
		final Map<BasedSequence, String> replacements = new TreeMap<>((cmp1, cmp2) -> {
			final int cmp = Integer.compare(cmp2.getStartOffset(), cmp1.getStartOffset());
			if (cmp != 0) {
				return cmp;
			}
			return Integer.compare(cmp2.getEndOffset(), cmp1.getEndOffset());
		});

		// Visit the links and record the transformations
		final MutableDataSet options = new MutableDataSet();
		final Parser parser = Parser.builder(options).build();
		final Node document = parser.parse(content);
		final NodeVisitor visitor = new NodeVisitor(
				new VisitHandler<>(Link.class, it -> {
					URL url = FileSystem.convertStringToURL(it.getUrl().toString(), true);
					url = transformURL(url, it.getLineNumber(), references);
					if (url != null) {
						replacements.put(it.getUrl(), convertURLToString(url));
					}
				}));
		visitor.visitChildren(document);

		// Apply the replacements
		if (!replacements.isEmpty()) {
			final StringBuilder buffer = new StringBuilder(content);
			for (final Entry<BasedSequence, String> entry : replacements.entrySet()) {
				final BasedSequence seq = entry.getKey();
				buffer.replace(seq.getStartOffset(), seq.getEndOffset(), entry.getValue());
			}
			return buffer.toString();
		}
		return content;
	}

	/** Convert the given URL to its string representation that is compatible with Markdown document linking mechanism.
	 *
	 * @param url the URL to transform.
	 * @return the sting representation of the URL.
	 */
	static String convertURLToString(URL url) {
		if (URISchemeType.FILE.isURL(url)) {
			final StringBuilder externalForm = new StringBuilder();
			externalForm.append(url.getPath());
			final String ref = url.getRef();
			if (!Strings.isEmpty(ref)) {
				externalForm.append("#").append(ref); //$NON-NLS-1$
			}
			return externalForm.toString();
		}
		return url.toExternalForm();
	}

	/** Transform an URL from Markdown format to HTML format.
	 *
	 * <p>Usually, the file extension ".md" is replaced by ".html".
	 *
	 * <p>This function replaces the anchor to the local reference with the correct one (modified by outline feature).
	 *
	 * @param link the link to transform.
	 * @param line the number of the line at which the link is located. If negative value, the line number is unknown.
	 * @param references the set of references from the local document.
	 * @return the result of the transformation. {@code null} if the link should not changed.
	 * @since 0.12
	 */
	protected URL transformURL(URL link, int line, ReferenceContext references) {
		if (URISchemeType.FILE.isURL(link)) {
			File filename = FileSystem.convertURLToFile(link);
			if (Strings.isEmpty(filename.getName())) {
				// This is a link to the local document.
				final String anchor = transformURLAnchor(filename, line, link.getRef(), references);
				final URL url = FileSystemAddons.convertFileToURL(filename, true);
				if (!Strings.isEmpty(anchor)) {
					try {
						return new URL(url.toExternalForm() + "#" + anchor); //$NON-NLS-1$
					} catch (MalformedURLException e) {
						//
					}
				}
				return url;
			}
			// This is a link to another document.
			final String extension = FileSystem.extension(filename);
			if (isMarkdownFileExtension(extension)) {
				filename = FileSystem.replaceExtension(filename, ".html"); //$NON-NLS-1$
				final String anchor = transformURLAnchor(filename, line, link.getRef(), null);
				final URL url = FileSystemAddons.convertFileToURL(filename, true);
				if (!Strings.isEmpty(anchor)) {
					try {
						return new URL(url.toExternalForm() + "#" + anchor); //$NON-NLS-1$
					} catch (MalformedURLException e) {
						//
					}
				}
				return url;
			}
		}
		return null;
	}

	/** Transform the anchor of an URL from Markdown format to HTML format.
	 *
	 * @param file the linked file.
	 * @param line the number of the line at which the anchor is located. If negative value, the line number is unknown.
	 * @param anchor the anchor to transform.
	 * @param references the set of references from the local document, or {@code null}.
	 * @return the result of the transformation.
	 * @since 0.12
	 */
	@SuppressWarnings("static-method")
	protected String transformURLAnchor(File file, int line, String anchor, ReferenceContext references) {
		String anc = anchor;
		if (references != null) {
			anc = references.validateAnchor(anc, line);
		}
		return anc;
	}

	/** Replies if the given extension is for Markdown file.
	 *
	 * @param extension the extension to test.
	 * @return {@code true} if the extension is for a Markdown file.
	 */
	public static boolean isMarkdownFileExtension(String extension) {
		for (final String ext : MARKDOWN_FILE_EXTENSIONS) {
			if (Strings.equal(ext, extension)) {
				return true;
			}
		}
		return false;
	}

	/** Update the outline tags: numbering of the sections, generation of the outline.
	 *
	 * @param content the content with outline tag.
	 * @return the content with expended outline.
	 */
	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	protected String updateOutline(String content) {
		final Pattern sectionPattern = Pattern.compile(
				isAutoSectionNumbering() ? SECTION_PATTERN_AUTONUMBERING : SECTION_PATTERN_NO_AUTONUMBERING,
						Pattern.MULTILINE);
		final Matcher matcher = sectionPattern.matcher(content);

		final Set<String> identifiers = new TreeSet<>();
		final StringBuilder outline = new StringBuilder();
		outline.append("\n"); //$NON-NLS-1$
		final String outlineStyleId = getOutlineStyleId();
		final boolean styledOutline = !Strings.isEmpty(outlineStyleId);
		if (styledOutline) {
			outline.append("<ul class=\"").append(Strings.convertToJavaString(outlineStyleId)); //$NON-NLS-1$
			outline.append("\" id=\"").append(Strings.convertToJavaString(outlineStyleId)); //$NON-NLS-1$
			outline.append("\">\n\n"); //$NON-NLS-1$
		}
		final IntegerRange outlineDepthRange = getOutlineDepthRange();

		final StringBuffer output;
		final SectionNumber sections;
		final int titleGroupId;
		if (isAutoSectionNumbering()) {
			output = new StringBuffer();
			sections = new SectionNumber();
			titleGroupId = 3;
		} else {
			output = null;
			sections = null;
			titleGroupId = 2;
		}

		int prevLevel = 0;
		int nbOpened = 0;

		while (matcher.find()) {
			final String prefix = matcher.group(1);
			final int clevel = prefix.length();
			if (outlineDepthRange.contains(clevel)) {
				final int relLevel = clevel - outlineDepthRange.getStart();
				final String title = matcher.group(titleGroupId);
				String sectionId = matcher.group(titleGroupId + 1);

				if (output != null) {
					assert sections != null;

					String sectionNumber = matcher.group(2);
					if (!Strings.isEmpty(sectionNumber)) {
						sections.setFromString(sectionNumber, relLevel + 1);
					} else {
						sections.increment(relLevel + 1);
					}
					sectionNumber = formatSectionNumber(sections);

					if (Strings.isEmpty(sectionId)) {
						sectionId = computeHeaderId(sectionNumber, title);
						if (!identifiers.add(sectionId)) {
							int idNum = 1;
							String nbId = sectionId + "-" + idNum; //$NON-NLS-1$
							while (!identifiers.add(nbId)) {
								++idNum;
								nbId = sectionId + "-" + idNum; //$NON-NLS-1$
							}
							sectionId = nbId;
						}
					}

					matcher.appendReplacement(output, formatSectionTitle(prefix, sectionNumber, title, sectionId));

					if (styledOutline && (relLevel > 0 || prevLevel > 0) && relLevel != prevLevel) {
						if (relLevel > prevLevel) {
							for (int i = prevLevel; i < relLevel; ++i) {
								outline.append("<ul>\n"); //$NON-NLS-1$
								++nbOpened;
							}
						} else {
							for (int i = relLevel; i < prevLevel; ++i) {
								outline.append("</ul>\n"); //$NON-NLS-1$
								--nbOpened;
							}
						}
					}

					addOutlineEntry(outline, relLevel + 1, sectionNumber, title, sectionId, styledOutline);
				} else {
					if (Strings.isEmpty(sectionId)) {
						sectionId = computeHeaderId(null, title);
						if (!identifiers.add(sectionId)) {
							int idNum = 1;
							String nbId = sectionId + "-" + idNum; //$NON-NLS-1$
							while (!identifiers.add(nbId)) {
								++idNum;
								nbId = sectionId + "-" + idNum; //$NON-NLS-1$
							}
							sectionId = nbId;
						}
					}

					addOutlineEntry(outline, relLevel + 1, null, title, sectionId, styledOutline);
				}
				prevLevel = relLevel;
			}
		}

		final String newContent;
		if (output != null) {
			matcher.appendTail(output);
			newContent = output.toString();
		} else {
			newContent = content;
		}

		outline.append("\n"); //$NON-NLS-1$
		if (styledOutline) {
			for (int i = 0; i <= nbOpened; ++i) {
				outline.append("</ul>\n"); //$NON-NLS-1$
			}
		}

		final String outlineTag = getDocumentParser().getOutlineOutputTag();
		if (isOutlineGeneration()) {
			final String externalMarker = getOutlineExternalMarker();
			if (!Strings.isEmpty(externalMarker)) {
				return newContent.replaceAll(Pattern.quote(outlineTag), Matcher.quoteReplacement(externalMarker));
			}
			return newContent.replaceAll(Pattern.quote(outlineTag), outline.toString());
		}
		return newContent.replaceAll(Pattern.quote(outlineTag), ""); //$NON-NLS-1$
	}

	/** Create the id of a section header.
	 *
	 * <p>The ID format follows the ReadCarpet standards.
	 *
	 * @param headerNumber the number of the header, or {@code null}.
	 * @param headerText the section header text.
	 * @return the identifier.
	 */
	public String computeHeaderId(String headerNumber, String headerText) {
		if (Strings.isEmpty(headerNumber)) {
			return computeHeaderIdForText(Strings.emptyIfNull(headerText));
		}
		final String nb = computeHeaderIdForNumber(headerNumber);
		return nb + "-" + computeHeaderIdForText(Strings.emptyIfNull(headerText)); //$NON-NLS-1$
	}

	/** Create the id of a section header.
	 *
	 * <p>The ID format follows the ReadCarpet standards.
	 *
	 * @param header the section header text.
	 * @return the identifier.
	 * @since 0.12
	 */
	private String computeHeaderIdForNumber(String header) {
		String id = header;
		if (isKramdownFix()) {
			id = id .replaceAll("\\.", ""); //$NON-NLS-1$ //$NON-NLS-2$
		}
		id = id.replaceAll("[^a-zA-Z0-9]+", "-"); //$NON-NLS-1$ //$NON-NLS-2$
		id = id.toLowerCase();
		id = id.replaceFirst("^[^a-zA-Z0-9]+", ""); //$NON-NLS-1$ //$NON-NLS-2$
		id = id.replaceFirst("[^a-zA-Z0-9]+$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		if (Strings.isEmpty(id)) {
			return "section"; //$NON-NLS-1$
		}
		return id;
	}

	/** Create the id of a section header.
	 *
	 * <p>The ID format follows the ReadCarpet standards.
	 *
	 * @param header the section header text.
	 * @return the identifier.
	 * @since 0.12
	 */
	private String computeHeaderIdForText(String header) {
		String id = header.replaceAll("[^a-zA-Z0-9]+", "-"); //$NON-NLS-1$ //$NON-NLS-2$
		id = id.toLowerCase();
		id = id.replaceFirst("^[^a-zA-Z0-9]+", ""); //$NON-NLS-1$ //$NON-NLS-2$
		id = id.replaceFirst("[^a-zA-Z0-9]+$", ""); //$NON-NLS-1$ //$NON-NLS-2$
		if (Strings.isEmpty(id)) {
			return "section"; //$NON-NLS-1$
		}
		return id;
	}

	/** Update the outline entry.
	 *
	 * @param outline the outline.
	 * @param level the depth level in the outline.
	 * @param sectionNumber the auto-computed section number, or {@code null} if no auto-computed number.
	 * @param title the title of the section.
	 * @param sectionId the identifier of the section.
	 * @param htmlOutput indicates if the output should be HTML or not.
	 */
	protected void addOutlineEntry(StringBuilder outline, int level, String sectionNumber, String title,
			String sectionId, boolean htmlOutput) {
		if (htmlOutput) {
			indent(outline, level - 1, "  "); //$NON-NLS-1$
			outline.append("<li><a href=\"#"); //$NON-NLS-1$
			outline.append(sectionId);
			outline.append("\">"); //$NON-NLS-1$
			if (isAutoSectionNumbering() && !Strings.isEmpty(sectionNumber)) {
				outline.append(sectionNumber).append(". "); //$NON-NLS-1$
			}
			outline.append(title);
			outline.append("</a></li>"); //$NON-NLS-1$
		} else {
			final String prefix = "*"; //$NON-NLS-1$
			final String entry;
			outline.append("> "); //$NON-NLS-1$
			indent(outline, level - 1, "\t"); //$NON-NLS-1$
			if (isAutoSectionNumbering()) {
				entry = MessageFormat.format(getOutlineEntryFormat(), prefix,
						Strings.emptyIfNull(sectionNumber), title, sectionId);
			} else {
				entry = MessageFormat.format(getOutlineEntryFormat(), prefix, title, sectionId);
			}
			outline.append(entry);
		}
		outline.append("\n"); //$NON-NLS-1$
	}

	/** Format the section numbers.
	 *
	 * @param numbers the section numbers, level per level.
	 * @return the formatted section number.
	 */
	protected String formatSectionNumber(SectionNumber numbers) {
		return numbers.toString(getSectionNumberFormat());
	}

	/** Format the section title.
	 *
	 * @param prefix the Markdown prefix.
	 * @param sectionNumber the section number.
	 * @param title the section title.
	 * @param sectionId the identifier of the section.
	 * @return the formatted section title.
	 */
	protected String formatSectionTitle(String prefix, String sectionNumber, String title, String sectionId) {
		return MessageFormat.format(getSectionTitleFormat(), prefix, sectionNumber, title, sectionId) + "\n"; //$NON-NLS-1$
	}

	/** Create indentation in the given buffer.
	 *
	 * @param buffer the buffer.
	 * @param number the number of identations.
	 * @param character the string for a single indentation.
	 */
	protected static void indent(StringBuilder buffer, int number, String character) {
		for (int i = 0; i < number; ++i) {
			buffer.append(character);
		}
	}

	@Override
	protected List<DynamicValidationComponent> getSpecificValidationComponents(String text, File inputFile,
			File rootFolder,
			DynamicValidationContext context) {
		final MutableDataSet options = new MutableDataSet();
		final Parser parser = Parser.builder(options).build();
		final Node document = parser.parse(text);
		final List<DynamicValidationComponent> validators = new ArrayList<>();
		File cfile;
		try {
			cfile = FileSystem.makeRelative(inputFile, rootFolder);
		} catch (IOException exception) {
			cfile = inputFile.getParentFile();
		}
		final File currentFile = cfile;
		final NodeVisitor visitor = new NodeVisitor(
				new VisitHandler<>(Link.class, it -> {
					final Iterable<DynamicValidationComponent> components = createValidatorComponents(it,
							currentFile, context);
					for (final DynamicValidationComponent component : components) {
						validators.add(component);
					}
				}),
				new VisitHandler<>(Image.class, it -> {
					final Iterable<DynamicValidationComponent> components = createValidatorComponents(it,
							currentFile, context);
					for (final DynamicValidationComponent component : components) {
						validators.add(component);
					}
				}));
		visitor.visitChildren(document);
		return validators;
	}

	/** Compute the number of lines for reaching the given node.
	 *
	 * @param node the node.
	 * @return the line number for the node.
	 */
	protected static int computeLineNo(Node node) {
		final int offset = node.getStartOffset();
		final BasedSequence seq = node.getDocument().getChars();
		int tmpOffset = seq.endOfLine(0);
		int lineno = 1;
		while (tmpOffset < offset) {
			++lineno;
			tmpOffset = seq.endOfLineAnyEOL(tmpOffset + seq.eolStartLength(tmpOffset));
		}
		return lineno;
	}

	/** Create a validation component for an image reference.
	 *
	 * @param it the image reference.
	 * @param currentFile the current file.
	 * @param context the validation context.
	 * @return the validation components.
	 */
	protected Iterable<DynamicValidationComponent> createValidatorComponents(Image it, File currentFile,
			DynamicValidationContext context) {
		final Collection<DynamicValidationComponent> components = new ArrayList<>();
		if (isLocalImageReferenceValidation()) {
			final int lineno = computeLineNo(it);
			final URL url = FileSystem.convertStringToURL(it.getUrl().toString(), true);
			if (URISchemeType.FILE.isURL(url)) {
				final DynamicValidationComponent component = createLocalImageValidatorComponent(
						it, url, lineno, currentFile, context);
				if (component != null) {
					components.add(component);
				}
			}
		}
		return components;
	}

	/** Create a validation component for an hyper reference.
	 *
	 * @param it the hyper reference.
	 * @param currentFile the current file.
	 * @param context the validation context.
	 * @return the validation components.
	 */
	protected Iterable<DynamicValidationComponent> createValidatorComponents(Link it, File currentFile,
			DynamicValidationContext context) {
		final Collection<DynamicValidationComponent> components = new ArrayList<>();
		if (Strings.equal(":", it.getUrl().toStringOrNull())) { //$NON-NLS-1$
			// Special case: the hyperlink is not referencing something
			return components;
		}
		if (isLocalFileReferenceValidation() || isRemoteReferenceValidation()) {
			final int lineno = computeLineNo(it);
			final URL url = FileSystem.convertStringToURL(it.getUrl().toString(), true);
			if (URISchemeType.HTTP.isURL(url) || URISchemeType.HTTPS.isURL(url) || URISchemeType.FTP.isURL(url)) {
				if (isRemoteReferenceValidation()) {
					final Collection<DynamicValidationComponent> newComponents = createRemoteReferenceValidatorComponents(
							it, url, lineno, currentFile, context);
					if (newComponents != null && !newComponents.isEmpty()) {
						components.addAll(newComponents);
					}
				}
			} else if (URISchemeType.FILE.isURL(url)) {
				if (isLocalFileReferenceValidation()) {
					final Collection<DynamicValidationComponent> newComponents = createLocalFileValidatorComponents(
							it, url, lineno, currentFile, context);
					if (newComponents != null && !newComponents.isEmpty()) {
						components.addAll(newComponents);
					}
				}
			}
		}
		return components;
	}

	/** Create a validation component for a reference to a local image.
	 *
	 * @param it the reference.
	 * @param url the parsed URL of the link.
	 * @param lineno the position of the link into the Markdown file.
	 * @param currentFile the current file.
	 * @param context the validation context.
	 * @return the validation component.
	 */
	@SuppressWarnings("static-method")
	protected DynamicValidationComponent createLocalImageValidatorComponent(Image it, URL url, int lineno,
			File currentFile, DynamicValidationContext context) {
		File fn = FileSystem.convertURLToFile(url);
		if (!fn.isAbsolute()) {
			fn = FileSystem.join(currentFile.getParentFile(), fn);
		}
		final File filename = fn;
		return new DynamicValidationComponent() {
			@Override
			public String functionName() {
				return "Image_reference_test_" + lineno + "_"; //$NON-NLS-1$ //$NON-NLS-2$
			}

			@Override
			public void generateValidationCode(ITreeAppendable it) {
				context.appendFileExistenceTest(it, filename, Messages.MarkdownParser_0);
			}
		};
	}

	/** Create a validation component for an hyper reference to a local file.
	 *
	 * @param it the hyper reference.
	 * @param url the parsed URL of the link.
	 * @param lineno the position of the link into the Markdown file.
	 * @param currentFile the current File.
	 * @param context the validation context.
	 * @return the validation component.
	 */
	@SuppressWarnings("static-method")
	protected Collection<DynamicValidationComponent> createLocalFileValidatorComponents(Link it, URL url, int lineno,
			File currentFile, DynamicValidationContext context) {
		File fn = FileSystem.convertURLToFile(url);
		if (Strings.isEmpty(fn.getName())) {
			// Special case: the URL should point to a anchor in the current document.
			final String linkRef = url.getRef();
			if (!Strings.isEmpty(linkRef)) {
				return Arrays.asList(new DynamicValidationComponent() {
					@Override
					public String functionName() {
						return "Documentation_reference_anchor_test_" + lineno + "_"; //$NON-NLS-1$ //$NON-NLS-2$
					}

					@Override
					public void generateValidationCode(ITreeAppendable it) {
						context.setTempResourceRoots(null);
						context.appendTitleAnchorExistenceTest(it, currentFile,
								url.getRef(),
								SECTION_PATTERN_TITLE_EXTRACTOR,
								Iterables.concat(
										Arrays.asList(MARKDOWN_FILE_EXTENSIONS),
										Arrays.asList(HTML_FILE_EXTENSIONS)));
					}
				});
			}
			// No need to validate the current file's existence and anchor.
			return null;
		}
		if (!fn.isAbsolute()) {
			fn = FileSystem.join(currentFile.getParentFile(), fn);
		}
		final File filename = fn;
		final String extension = FileSystem.extension(filename);
		if (isMarkdownFileExtension(extension) || isHtmlFileExtension(extension)) {
			// Special case: the file may be a HTML or a Markdown file.
			final DynamicValidationComponent existence = new DynamicValidationComponent() {
				@Override
				public String functionName() {
					return "Documentation_reference_test_" + lineno + "_"; //$NON-NLS-1$ //$NON-NLS-2$
				}

				@Override
				public void generateValidationCode(ITreeAppendable it) {
					context.setTempResourceRoots(context.getSourceRoots());
					context.appendFileExistenceTest(it, filename, Messages.MarkdownParser_1,
							Iterables.concat(
									Arrays.asList(MARKDOWN_FILE_EXTENSIONS),
									Arrays.asList(HTML_FILE_EXTENSIONS)));
				}
			};
			if (!Strings.isEmpty(url.getRef())) {
				final DynamicValidationComponent refValidity = new DynamicValidationComponent() {
					@Override
					public String functionName() {
						return "Documentation_reference_anchor_test_" + lineno + "_"; //$NON-NLS-1$ //$NON-NLS-2$
					}

					@Override
					public void generateValidationCode(ITreeAppendable it) {
						context.setTempResourceRoots(null);
						context.appendTitleAnchorExistenceTest(it, filename,
								url.getRef(),
								SECTION_PATTERN_TITLE_EXTRACTOR,
								Iterables.concat(
										Arrays.asList(MARKDOWN_FILE_EXTENSIONS),
										Arrays.asList(HTML_FILE_EXTENSIONS)));
					}
				};
				return Arrays.asList(existence, refValidity);
			}
			return Collections.singleton(existence);
		}
		return Arrays.asList(new DynamicValidationComponent() {
			@Override
			public String functionName() {
				return "File_reference_test_" + lineno + "_"; //$NON-NLS-1$ //$NON-NLS-2$
			}

			@Override
			public void generateValidationCode(ITreeAppendable it) {
				context.appendFileExistenceTest(it, filename, Messages.MarkdownParser_1);
			}
		});
	}

	/** Create a validation component for an hyper reference to a remote Internet page.
	 *
	 * @param it the hyper reference.
	 * @param url the parsed URL of the link.
	 * @param lineno the position of the link into the Markdown file.
	 * @param currentFile the current file.
	 * @param context the validation context.
	 * @return the validation component.
	 */
	@SuppressWarnings("static-method")
	protected Collection<DynamicValidationComponent> createRemoteReferenceValidatorComponents(Link it, URL url, int lineno,
			File currentFile, DynamicValidationContext context) {
		return Collections.singleton(new DynamicValidationComponent() {
			@Override
			public String functionName() {
				return "Web_reference_test_" + lineno + "_"; //$NON-NLS-1$ //$NON-NLS-2$
			}

			@Override
			public void generateValidationCode(ITreeAppendable it) {
				it.append("assertURLAccessibility(").append(Integer.toString(lineno)); //$NON-NLS-1$
				it.append(", new "); //$NON-NLS-1$
				it.append(URL.class).append("(\""); //$NON-NLS-1$
				it.append(Strings.convertToJavaString(url.toExternalForm()));
				it.append("\"));"); //$NON-NLS-1$
			}
		});
	}

}
