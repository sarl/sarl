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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.lang.ref.WeakReference;
import java.lang.reflect.TypeVariable;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.inject.Named;

import com.google.common.base.Strings;
import com.google.common.base.Throwables;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.ReflectionUtil;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.xbase.lib.Functions.Function2;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.lang.core.Capacity;
import io.sarl.lang.core.Event;
import io.sarl.lang.sarl.actionprototype.IActionPrototypeProvider;
import io.sarl.lang.util.OutParameter;
import io.sarl.maven.docs.testing.NoXtextResourceException;
import io.sarl.maven.docs.testing.ReflectExtensions;
import io.sarl.maven.docs.testing.ScriptExecutor;

/** Generator of the marker language files for the modified marker language for SARL.
 *
 * @author $Author: sgalland$
 * @author $Author: alombard$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlDocumentationParser {

	/** Default pattern for formatting inline code.
	 */
	public static final String DEFAULT_INLINE_FORMAT = "`{0}`"; //$NON-NLS-1$

	/** Default string to put for the outline location.
	 */
	public static final String DEFAULT_OUTLINE_OUTPUT_TAG = "[::Outline::]"; //$NON-NLS-1$

	/** Default text for line continuation.
	 */
	public static final String DEFAULT_LINE_CONTINUATION = " "; //$NON-NLS-1$

	private static final String DEFAULT_TAG_NAME_PATTERN = "\\[:(.*?)[:!]?\\]"; //$NON-NLS-1$

	private static final int PATTERN_COMPILE_OPTIONS = Pattern.DOTALL | Pattern.MULTILINE;

	private Injector injector;

	private IActionPrototypeProvider actionPrototypeProvider;

	private Map<Tag, String> rawPatterns = new HashMap<>();

	private Map<Tag, Pattern> compiledPatterns = new HashMap<>();

	private String inlineFormat;

	private Function2<String, String, String> blockFormat;

	private String outlineOutputTag;

	private String dynamicNameExtractionPattern;

	private Deque<Properties> additionalPropertyProvidersByPriority = new LinkedList<>();

	private String lineSeparator;

	private String languageName;

	private ScriptExecutor scriptExecutor;

	private String lineContinuation;

	/** Constructor.
	 */
	public SarlDocumentationParser() {
		reset();
	}

	/** Change the injector.
	 *
	 * @param injector the injector.
	 */
	@Inject
	public void setInjector(Injector injector) {
		assert injector != null;
		this.injector =  injector;
	}

	/** Change the name of the language.
	 *
	 * @param outputLanguage the language name, or {@code null} for ignoring the language name.
	 */
	@Inject
	public void setOutputLanguage(@Named(Constants.LANGUAGE_NAME) String outputLanguage) {
		if (!Strings.isNullOrEmpty(outputLanguage)) {
			final String[] parts = outputLanguage.split("\\.+"); //$NON-NLS-1$
			if (parts.length > 0) {
				final String simpleName = parts[parts.length - 1];
				if (!Strings.isNullOrEmpty(simpleName)) {
					this.languageName = simpleName;
					return;
				}
			}
		}
		this.languageName = null;
	}

	/** Change the script executor.
	 *
	 * @param executor the script executor.
	 */
	@Inject
	public void setScriptExecutor(ScriptExecutor executor) {
		this.scriptExecutor = executor;
	}

	/** Replies the script executor.
	 *
	 * @return the script executor.
	 */
	public ScriptExecutor getScriptExecutor() {
		return this.scriptExecutor;
	}

	/** Replies the string of character to put in the text when line continuation  is detected.
	 *
	 * @return the line continuation string of characters, or {@code null} to ignore line continuations.
	 */
	public String getLineContinuation() {
		return this.lineContinuation;
	}

	/** Change the string of character to put in the text when line continuation  is detected.
	 *
	 * @param lineContinuationText the line continuation string of characters, or {@code null} to ignore line continuations.
	 */
	public void setLineContinuation(String lineContinuationText) {
		this.lineContinuation = lineContinuationText;
	}

	/** Replies the fenced code block formatter.
	 *
	 * <p>This code block formatter is usually used by Github.
	 *
	 * @return the formatter.
	 */
	public static Function2<String, String, String> getFencedCodeBlockFormatter() {
		return (languageName, content) -> {
			/*final StringBuilder result = new StringBuilder();
			result.append("<div class=\\\"highlight"); //$NON-NLS-1$
			if (!Strings.isNullOrEmpty(languageName)) {
				result.append(" highlight-").append(languageName); //$NON-NLS-1$
			}
			result.append("\"><pre>\n").append(content).append("</pre></div>\n"); //$NON-NLS-1$ //$NON-NLS-2$
			return result.toString();*/
			return "```" + Strings.nullToEmpty(languageName).toLowerCase() + "\n" //$NON-NLS-1$ //$NON-NLS-2$
					+ content
					+ "```\n"; //$NON-NLS-1$
		};
	}

	/** Replies the basic code block formatter.
	 *
	 * @return the formatter.
	 */
	public static Function2<String, String, String> getBasicCodeBlockFormatter() {
		return (languageName, content) -> {
			return Pattern.compile("^", Pattern.MULTILINE).matcher(content).replaceAll("\t"); //$NON-NLS-1$ //$NON-NLS-2$
		};
	}

	/** Replies the name of the language.
	 *
	 * @return the language name, or {@code null} for ignoring the language name.
	 */
	public String getOutputLanguage() {
		return this.languageName;
	}

	/** Change the provider of action prototypes.
	 *
	 * @param provider the provider.
	 */
	@Inject
	public void setActionPrototypeProvider(IActionPrototypeProvider provider) {
		assert provider != null;
		this.actionPrototypeProvider =  provider;
	}

	/** Replies the provider of action prototypes.
	 *
	 * @return the provider.
	 */
	@Inject
	public IActionPrototypeProvider getActionPrototypeProvider() {
		return this.actionPrototypeProvider;
	}

	/** Reset to the default settings.
	 */
	public void reset() {
		this.rawPatterns.clear();
		this.compiledPatterns.clear();
		this.inlineFormat = DEFAULT_INLINE_FORMAT;
		this.blockFormat = null;
		this.outlineOutputTag = DEFAULT_OUTLINE_OUTPUT_TAG;
		this.dynamicNameExtractionPattern = DEFAULT_TAG_NAME_PATTERN;
		this.lineContinuation = DEFAULT_LINE_CONTINUATION;
	}

	/** Add a provider of properties that could be used for finding replacement values.
	 *
	 * <p>The given provider will be consider prior to the already declared providers.
	 *
	 * @param properties the property provider.
	 */
	public void addHighPropertyProvider(Properties properties) {
		this.additionalPropertyProvidersByPriority.addFirst(properties);
	}

	/** Add a provider of properties that could be used for finding replacement values.
	 *
	 * <p>The given provider will be consider after the already declared providers.
	 *
	 * @param properties the property provider.
	 */
	public void addLowPropertyProvider(Properties properties) {
		this.additionalPropertyProvidersByPriority.addLast(properties);
	}

	/** Replies additional providers of properties that could be used for finding replacement values.
	 *
	 * <p>The providers are provided from the higher priority to the lower priority.
	 *
	 * @return the property providers.
	 */
	public Iterable<Properties> getPropertyProvidersByPriority() {
		return Collections.unmodifiableCollection(this.additionalPropertyProvidersByPriority);
	}

	/** Change the pattern of the tag.
	 *
	 * @param tag the tag.
	 * @param regex the regular expression.
	 */
	public void setPattern(Tag tag, String regex) {
		if (Strings.isNullOrEmpty(regex)) {
			this.rawPatterns.remove(tag);
			this.compiledPatterns.remove(tag);
		} else {
			this.rawPatterns.put(tag, regex);
			this.compiledPatterns.put(tag, Pattern.compile("^\\s*" + regex, PATTERN_COMPILE_OPTIONS)); //$NON-NLS-1$
		}
	}

	/** Replies the pattern of the tag.
	 *
	 * @param tag the tag.
	 * @return the regular expression pattern.
	 */
	public String getPattern(Tag tag) {
		final String pattern = this.rawPatterns.get(tag);
		if (pattern == null) {
			return tag.getDefaultPattern();
		}
		return pattern;
	}

	/** Replies the tag that is matching the given text.
	 *
	 * @param text the text to match.
	 * @return the tag or {@code null}.
	 */
	public Tag getTagForPattern(CharSequence text) {
		for (final Tag tag : Tag.values()) {
			Pattern pattern = this.compiledPatterns.get(tag);
			if (pattern == null) {
				pattern = Pattern.compile("^\\s*" + getPattern(tag), Pattern.DOTALL); //$NON-NLS-1$
				this.compiledPatterns.put(tag, pattern);
			}
			final Matcher matcher = pattern.matcher(text);
			if (matcher.find()) {
				return tag;
			}
		}
		return null;
	}

	/** Change the pattern for the failure tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setFailurePattern(String regex) {
		setPattern(Tag.FAILURE, regex);
	}

	/** Replies the pattern for the failure tag.
	 *
	 * @return the regular expression.
	 */
	public final String getFailurePattern() {
		return getPattern(Tag.FAILURE);
	}

	/** Change the pattern for the success tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setSuccessPattern(String regex) {
		setPattern(Tag.SUCCESS, regex);
	}

	/** Replies the pattern for the success tag.
	 *
	 * @return the regular expression.
	 */
	public final String getSuccessPattern() {
		return getPattern(Tag.SUCCESS);
	}

	/** Change the pattern for the definition tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setDefinitionPattern(String regex) {
		setPattern(Tag.DEFINITION, regex);
	}

	/** Replies the pattern for the definition tag.
	 *
	 * @return the regular expression.
	 */
	public final String getDefinitionPattern() {
		return getPattern(Tag.DEFINITION);
	}

	/** Change the pattern for the reference tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setReferencePattern(String regex) {
		setPattern(Tag.REFERENCE, regex);
	}

	/** Replies the pattern for the reference tag.
	 *
	 * @return the regular expression.
	 */
	public final String getReferencePattern() {
		return getPattern(Tag.REFERENCE);
	}

	/** Change the pattern for the "off" tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setOffPattern(String regex) {
		setPattern(Tag.OFF, regex);
	}

	/** Replies the pattern for the "off" tag.
	 *
	 * @return the regular expression.
	 */
	public final String getOffPattern() {
		return getPattern(Tag.OFF);
	}

	/** Change the pattern for the "on" tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setOnPattern(String regex) {
		setPattern(Tag.ON, regex);
	}

	/** Replies the pattern for the "on" tag.
	 *
	 * @return the regular expression.
	 */
	public final String getOnPattern() {
		return getPattern(Tag.ON);
	}

	/** Change the pattern for the fact tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setFactPattern(String regex) {
		setPattern(Tag.FACT, regex);
	}

	/** Replies the pattern for the fact tag.
	 *
	 * @return the regular expression.
	 */
	public final String getFactPattern() {
		return getPattern(Tag.FACT);
	}

	/** Change the pattern for the include tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setIncludePattern(String regex) {
		setPattern(Tag.INCLUDE, regex);
	}

	/** Replies the pattern for the include tag.
	 *
	 * @return the regular expression.
	 */
	public final String getIncludePattern() {
		return getPattern(Tag.INCLUDE);
	}

	/** Change the pattern for the outline tag.
	 *
	 * @param regex the regular expression.
	 */
	public final void setOutlinePattern(String regex) {
		setPattern(Tag.OUTLINE, regex);
	}

	/** Replies the pattern for the outline tag.
	 *
	 * @return the regular expression.
	 */
	public final String getOutlinePattern() {
		return getPattern(Tag.OUTLINE);
	}

	/** Set the template for inline codes.
	 *
	 * <p>The template should follow the {@link MessageFormat} specifications.
	 *
	 * @param template the template.
	 */
	public void setInlineCodeTemplate(String template) {
		if (!Strings.isNullOrEmpty(template)) {
			this.inlineFormat = template;
		}
	}

	/** Replies the template for inline codes.
	 *
	 * <p>The template should follow the {@link MessageFormat} specifications.
	 *
	 * @return the template.
	 */
	public String getInlineCodeTemplate() {
		return this.inlineFormat;
	}

	/** Set the template for block codes.
	 *
	 * <p>The first parameter of the function is the language name. The second parameter is
	 * the code to format.
	 *
	 * @param template the template.
	 */
	public void setBlockCodeTemplate(Function2<String, String, String> template) {
		this.blockFormat = template;
	}

	/** Replies the template for block codes.
	 *
	 * <p>The first parameter of the function is the language name. The second parameter is
	 * the code to format.
	 *
	 * @return the template.
	 */
	public Function2<String, String, String> getBlockCodeTemplate() {
		return this.blockFormat;
	}

	/** Change the outline output tag that will be output when the outline
	 * tag is found in the input content.
	 *
	 * @param tag the tag for outline.
	 */
	protected void setOutlineOutputTag(String tag) {
		this.outlineOutputTag = tag;
	}

	/** Replies the outline output tag that will be output when the outline
	 * tag is found in the input content.
	 *
	 * @return the tag.
	 */
	public String getOutlineOutputTag() {
		return this.outlineOutputTag;
	}

	/** Set the regular expression for extracting the dynamic name of a tag string.
	 *
	 * @param pattern the regex.
	 */
	public void setDynamicNameExtractionPattern(String pattern) {
		if (!Strings.isNullOrEmpty(pattern)) {
			this.dynamicNameExtractionPattern = pattern;
		}
	}

	/** Replies the regular expression for extracting the dynamic name of a tag string.
	 *
	 * @return the regex.
	 */
	public String getDynamicNameExtractionPattern() {
		return this.dynamicNameExtractionPattern;
	}

	/** Replies the OS-dependent line separator.
	 *
	 * @return the line separator from the {@code "line.separator"} property, or {@code "\n"}.
	 */
	public String getLineSeparator() {
		if (Strings.isNullOrEmpty(this.lineSeparator)) {
			final String nl = System.getProperty("line.separator"); //$NON-NLS-1$
			if (Strings.isNullOrEmpty(nl)) {
				return "\n"; //$NON-NLS-1$
			}
			return nl;
		}
		return this.lineSeparator;
	}

	/** Change the OS-dependent line separator.
	 *
	 * @param lineSeparator the line separator or {@code null} to use the system configuration.
	 */
	public void setLineSeparator(String lineSeparator) {
		this.lineSeparator = lineSeparator;
	}

	private String buildGeneralTagPattern() {
		final StringBuilder pattern = new StringBuilder();
		int nbTags = 0;
		for (final Tag tag : Tag.values()) {
			if (nbTags >= 1) {
				pattern.append("|"); //$NON-NLS-1$
			}
			pattern.append("(?:"); //$NON-NLS-1$
			if (tag.isEnclosingSpaceCouldRemovable()) {
				pattern.append("[ \\t]*"); //$NON-NLS-1$
			}
			pattern.append("("); //$NON-NLS-1$
			pattern.append(getPattern(tag));
			pattern.append(")"); //$NON-NLS-1$
			if (tag.hasParameter()) {
				pattern.append("(?:"); //$NON-NLS-1$
				pattern.append("(?:\\(\\s*([^\\)]*?)\\s*\\))|"); //$NON-NLS-1$
				pattern.append("(?:\\{\\s*([^\\}]*?)\\s*\\})|"); //$NON-NLS-1$
				pattern.append("(?:\\|\\s*([^\\|]*?)\\s*\\|)|"); //$NON-NLS-1$
				pattern.append("(?:\\$\\s*([^\\$]*?)\\s*\\$)"); //$NON-NLS-1$
				pattern.append(")"); //$NON-NLS-1$
			}
			if (tag.isEnclosingSpaceCouldRemovable()) {
				pattern.append("[ \\t]*"); //$NON-NLS-1$
			}
			pattern.append(")"); //$NON-NLS-1$
			++nbTags;
		}
		if (nbTags > 0) {
			pattern.insert(0, "(" + org.eclipse.xtext.util.Strings.convertToJavaString(getLineSeparator()) //$NON-NLS-1$
				+ ")|"); //$NON-NLS-1$
			return pattern.toString();
		}
		return null;
	}

	/** Extract the dynamic name of that from the raw text.
	 *
	 * @param tag the tag to extract for.
	 * @param name the raw text.
	 * @param dynamicName the dynamic name.
	 */
	protected void extractDynamicName(Tag tag, CharSequence name, OutParameter<String> dynamicName) {
		if (tag.hasDynamicName()) {
			final Pattern pattern = Pattern.compile(getDynamicNameExtractionPattern());
			final Matcher matcher = pattern.matcher(name);
			if (matcher.matches()) {
				dynamicName.set(Strings.nullToEmpty(matcher.group(1)));
				return;
			}
		}
		dynamicName.set(name.toString());
	}

	private static int findFirstGroup(Matcher matcher, int startIdx) {
		final int len = matcher.groupCount();
		for (int i = startIdx + 1; i <= len; ++i) {
			final String value = matcher.group(i);
			if (value != null) {
				return i;
			}
		}
		return 1;
	}

	/** Read the given file and transform its content in order to have a raw text.
	 *
	 * @param inputFile the input file.
	 * @return the raw file context.
	 */
	public String transform(File inputFile) {
		final String content;
		try (FileReader reader = new FileReader(inputFile)) {
			content = read(reader, null);
		} catch (IOException exception) {
			reportError(Messages.SarlDocumentationParser_0, exception);
			return null;
		}
		return transform(content, inputFile);
	}

	/** Read the given input stream and transform its content in order to have a raw text.
	 *
	 * @param reader the input stream.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @return the raw file context.
	 */
	public String transform(Reader reader, File inputFile) {
		final String content;
		try {
			content = read(reader, null);
		} catch (IOException exception) {
			reportError(Messages.SarlDocumentationParser_0, exception);
			return null;
		}
		return transform(content, inputFile);
	}

	/** Read the given input content and transform it in order to have a raw text.
	 *
	 * @param content the content to parse.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @return the raw file context.
	 */
	public String transform(CharSequence content, File inputFile) {
		final ParsingContext rootContextForReplacements = new ParsingContext();
		initializeContext(rootContextForReplacements);
		CharSequence rawContent = preProcessing(content);
		Stage stage = Stage.first();
		do {
			final ContentParserInterceptor interceptor = new ContentParserInterceptor();
			// Reset the lineno and offset because they are not reset between the different stages.
			rootContextForReplacements.setLineNo(1);
			rootContextForReplacements.setOffset(0);
			final boolean hasChanged = parse(rawContent, inputFile, 0, stage, rootContextForReplacements, interceptor);
			if (hasChanged) {
				rawContent = interceptor.getResult();
			}
			stage = stage.next();
		} while (stage != null);
		return postProcessing(rawContent);
	}

	/** Do a pre processing of the text.
	 *
	 * @param text the text to pre process.
	 * @return the pre-processed text.
	 */
	@SuppressWarnings("static-method")
	protected CharSequence preProcessing(CharSequence text) {
		return text;
	}

	/** Do a post processing of the text.
	 *
	 * @param text the text to post process.
	 * @return the post-processed text.
	 */
	protected String postProcessing(CharSequence text) {
		final String lineContinuation = getLineContinuation();
		if (lineContinuation != null) {
			final Pattern pattern = Pattern.compile(
					"\\s*\\\\[\\n\\r]+\\s*", //$NON-NLS-1$
					Pattern.DOTALL);
			final Matcher matcher = pattern.matcher(text.toString().trim());
			return matcher.replaceAll(lineContinuation);
		}
		return text.toString().trim();
	}

	/** Read the given input content and extract validation components.
	 *
	 * @param inputFile the input file.
	 * @param observer the oberserver to be called with extracted information. The parameter of the lambda maps
	 *     the tags to the associated list of the extraction information.
	 */
	public void extractValidationComponents(File inputFile,
			Procedure1<Map<Tag, List<ValidationComponentData>>> observer) {
		final String content;
		final AtomicInteger nblines = new AtomicInteger();
		try (FileReader reader = new FileReader(inputFile)) {
			content = read(reader, nblines);
		} catch (IOException exception) {
			reportError(Messages.SarlDocumentationParser_0, exception);
			return;
		}
		extractValidationComponents(content, inputFile, observer);
	}

	/** Read the given input content and extract validation components.
	 *
	 * @param reader the input stream.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @param observer the oberserver to be called with extracted information. The parameter of the lambda maps
	 *     the tags to the associated list of the extraction information.
	 */
	public void extractValidationComponents(Reader reader, File inputFile,
			Procedure1<Map<Tag, List<ValidationComponentData>>> observer) {
		final String content;
		final AtomicInteger nblines = new AtomicInteger();
		try {
			content = read(reader, nblines);
		} catch (IOException exception) {
			reportError(Messages.SarlDocumentationParser_0, exception);
			return;
		}
		extractValidationComponents(content, inputFile, observer);
	}

	/** Read the given input content and extract validation components.
	 *
	 * @param content the content to parse.
	 * @param inputFile the name of the input file for locating included features and formatting error messages.
	 * @param observer the oberserver to be called with extracted information. The parameter of the lambda maps
	 *     the tags to the associated list of the extraction information.
	 */
	public void extractValidationComponents(CharSequence content, File inputFile,
			Procedure1<Map<Tag, List<ValidationComponentData>>> observer) {
		//
		// STEP 1: Extract the raw text
		//
		final Map<Tag, List<ValidationComponentData>> components = new TreeMap<>();
		final ContentParserInterceptor interceptor = new ContentParserInterceptor(new ParserInterceptor() {
			@Override
			public void tag(ParsingContext context, Tag tag, String dynamicName, String parameter,
					String blockValue) {
				if (tag.isOpeningTag() || tag.hasParameter()) {
					List<ValidationComponentData> values = components.get(tag);
					if (values == null) {
						values = new ArrayList<>();
						components.put(tag, values);
					}
					final ValidationComponentData data = new ValidationComponentData();
					data.file = context.getCurrentFile();
					data.lineno = context.getLineNo();
					data.endLineno = context.getEndLineNo();
					data.offset = context.getOffset();
					data.length = context.getLength();
					if (tag.isOpeningTag()) {
						data.code = Strings.nullToEmpty(blockValue).trim();
					} else {
						data.code = Strings.nullToEmpty(parameter).trim();
					}
					values.add(data);
				}
			}
		});
		final ParsingContext rootContextForReplacements = new ParsingContext(true, true);
		initializeContext(rootContextForReplacements);
		parse(content, inputFile, 0, Stage.FIRST, rootContextForReplacements, interceptor);
		//
		// STEP 2: Do macro replacement in the captured elements.
		//
		final Collection<List<ValidationComponentData>> allTexts = new ArrayList<>(components.values());
		for (final List<ValidationComponentData> values : allTexts) {
			for (final ValidationComponentData data : values) {
				final ContentParserInterceptor localInterceptor = new ContentParserInterceptor(interceptor);
				parse(data.code, inputFile, 0, Stage.SECOND, rootContextForReplacements, localInterceptor);
				final String newCapturedText = localInterceptor.getResult();
				data.code = newCapturedText;
			}
		}

		observer.apply(components);
	}

	/** Initialize the given context.
	 *
	 * @param context the context.
	 */
	protected void initializeContext(ParsingContext context) {
		context.setLineNo(1);
		context.setOffset(0);
		context.setScriptExecutor(getScriptExecutor());
	}

	/** Parse the given source text.
	 *
	 * @param source the source text.
	 * @param file the file to be read.
	 * @param startIndex the start index in the file.
	 * @param stage the number of the stage to run.
	 * @param parentContext the parent context, or {@code null} if none.
	 * @param interceptor interceptor of the parsed elements.
	 * @return {@code true} if a special tag was found.
	 */
	protected boolean parse(CharSequence source, File file, int startIndex, Stage stage,
			ParsingContext parentContext, ParserInterceptor interceptor) {
		final ParsingContext context = this.injector.getInstance(ParsingContext.class);
		context.setParser(this);
		context.setText(source);
		context.setCurrentFile(file);
		context.setStartIndex(startIndex);
		context.setParserInterceptor(interceptor);
		context.setInlineCodeFormat(getInlineCodeTemplate());
		context.setBlockCodeFormat(getBlockCodeTemplate());
		context.setOutlineOutputTag(getOutlineOutputTag());
		context.setLineSeparator(getLineSeparator());
		context.setOutputLanguage(getOutputLanguage());
		context.setStage(stage);
		final String regex = buildGeneralTagPattern();
		if (!Strings.isNullOrEmpty(regex)) {
			final Pattern patterns = Pattern.compile(regex, PATTERN_COMPILE_OPTIONS);
			final Matcher matcher = patterns.matcher(source);
			context.setMatcher(matcher);
			if (parentContext != null) {
				context.linkTo(parentContext);
			}
			return parse(context);
		}
		return false;
	}

	/** Parse the given source text.
	 *
	 * @param context the parsing context.
	 * @return {@code true} if a special tag was found.
	 * @throws ParsingException a runtime exception
	 */
	protected boolean parse(ParsingContext context) {
		try {
			context.getParserInterceptor().openContext(context);
			boolean specialTagFound = false;
			final String lineSeparator = getLineSeparator();
			while (context.getMatcher().find()) {
				int groupIndex = findFirstGroup(context.getMatcher(), 0);
				final String tagName = context.getMatcher().group(groupIndex);
				if (lineSeparator.equals(tagName)) {
					context.incrementLineNo();
					context.incrementOffset(lineSeparator.length());
					continue;
				}

				final int regionOffset = context.getMatcher().start();
				final int regionLength = context.getMatcher().end() - regionOffset;
				context.setOffset(regionOffset);
				context.setLength(regionLength);
				final int startLine = context.getLineNo();
				final int nbLines = countLines(context.getMatcher().group());
				final int endLine = startLine + nbLines - 1;
				context.setEndLineNo(endLine);

				final Tag tag = getTagForPattern(tagName);
				if (tag != null) {
					if (tag.isActive(context)) {
						final String tagDynamicName;
						if (tag.hasDynamicName()) {
							final OutParameter<String> dname = new OutParameter<>();
							extractDynamicName(tag, tagName, dname);
							tagDynamicName = dname.get();
						} else {
							tagDynamicName = null;
						}
						final String parameterValue;
						if (tag.hasParameter()) {
							groupIndex = findFirstGroup(context.getMatcher(), groupIndex);
							final String parameter = Strings.nullToEmpty(context.getMatcher().group(groupIndex));
							final ContentParserInterceptor subInterceptor = new ContentParserInterceptor(context.getParserInterceptor());
							final boolean inBlock = context.setInBlock(false);
							final boolean inParam = context.setInParameter(true);
							parse(parameter, context.getCurrentFile(),
									context.getMatcher().start(groupIndex) + context.getStartIndex(),
									context.getStage(),
									context,
									subInterceptor);
							context.setInParameter(inParam);
							context.setInBlock(inBlock);
							parameterValue = Strings.emptyToNull(subInterceptor.getResult());
						} else {
							parameterValue = null;
						}
						final String blockContent;
						if (tag.isOpeningTag()) {
							groupIndex = findFirstGroup(context.getMatcher(), groupIndex);
							final String tagContent = context.getMatcher().group(groupIndex);
							final ContentParserInterceptor subInterceptor = new ContentParserInterceptor(context.getParserInterceptor());
							final boolean inBlock = context.setInBlock(true);
							final boolean inParam = context.setInParameter(false);
							context.setVisibleInBlock(false);
							parse(Strings.nullToEmpty(tagContent), context.getCurrentFile(),
									context.getMatcher().start(groupIndex) + context.getStartIndex(),
									context.getStage(),
									context,
									subInterceptor);
							context.setInParameter(inParam);
							context.setInBlock(inBlock);
							blockContent = Strings.nullToEmpty(subInterceptor.getResult());
						} else {
							blockContent = null;
						}
						specialTagFound = true;
						try {
							context.getParserInterceptor().tag(context, tag, tagDynamicName, parameterValue, blockContent);
						} catch (Throwable exception) {
							reportError(context, Messages.SarlDocumentationParser_6, tagName, exception);
							return false;
						}
					} else {
						// Ignore the tag for this stage.
					}
				} else {
					reportError(context, Messages.SarlDocumentationParser_1, tagName);
					return false;
				}

				context.setLineNo(endLine);
				context.setEndLineNo(endLine);
			}
			context.getParserInterceptor().closeContext(context);
			return specialTagFound;
		} catch (ParsingException exception) {
			throw exception;
		} catch (Throwable exception) {
			final Throwable rootException = Throwables.getRootCause(exception);
			throw new ParsingException(
					rootException.getClass().getName() + " - " + rootException.getLocalizedMessage(), //$NON-NLS-1$
					context.getCurrentFile(),
					context.getLineNo(),
					rootException);
		}
	}

	private int countLines(String text) {
		final String[] lines = text.split(Pattern.quote(getLineSeparator()));
		return lines.length;
	}

	/** Report an error.
	 *
	 * @param context the context.
	 * @param message the message in a format compatible with {@link MessageFormat}. The first argument is
	 *     the filename. The second argument is the line number. The third argument is the position in the file.
	 * @param parameter additional parameter, starting at {4}.
	 */
	protected static void reportError(ParsingContext context, String message, Object... parameter) {
		final File file = context.getCurrentFile();
		final int offset = context.getMatcher().start() + context.getStartIndex();
		final int lineno = context.getLineNo();
		Throwable cause = null;
		for (final Object param : parameter) {
			if (param instanceof Throwable) {
				cause = (Throwable) param;
				break;
			}
		}
		final Object[] args = new Object[parameter.length + 3];
		args[0] = file;
		args[1] = lineno;
		args[2] = offset;
		System.arraycopy(parameter, 0, args, 3, parameter.length);
		final String msg = MessageFormat.format(message, args);
		if (cause != null) {
			throw new ParsingException(msg, file, lineno, Throwables.getRootCause(cause));
		}
		throw new ParsingException(msg, file, lineno);
	}

	/** Report an error.
	 *
	 * @param message the message in a format compatible with {@link MessageFormat}.
	 * @param parameters the parameters, starting at {1}.
	 */
	protected static void reportError(String message, Object... parameters) {
		Throwable cause = null;
		for (int i = 0; cause == null && i < parameters.length; ++i) {
			if (parameters[i] instanceof Throwable) {
				cause = (Throwable) parameters[i];
			}
		}
		final String msg = MessageFormat.format(message, parameters);
		if (cause != null) {
			throw new ParsingException(msg, null, 1, Throwables.getRootCause(cause));
		}
		throw new ParsingException(msg, null, 1);
	}

	/** Read the content of a file.
	 *
	 * @param context the current parsing context.
	 * @param file the file to read.
	 * @return the content.
	 * @throws IOException if the content cannot be read.
	 */
	protected static String read(ParsingContext context, File file) throws IOException {
		File filename = file;
		if (context != null && !filename.isAbsolute()) {
			filename = FileSystem.makeAbsolute(filename, context.getCurrentDirectory());
		}
		try (FileReader reader = new FileReader(filename)) {
			return read(reader, null);
		}
	}

	/** Read the content of a file.
	 *
	 * @param file the file to read.
	 * @param nblines the number of lines that is read.
	 * @return the content.
	 * @throws IOException if the content cannot be read.
	 */
	protected static String read(Reader file, AtomicInteger nblines) throws IOException {
		if (nblines != null) {
			nblines.set(0);
		}
		final StringBuilder content = new StringBuilder();
		try (BufferedReader reader = new BufferedReader(file)) {
			String line = reader.readLine();
			boolean first = true;
			while (line != null) {
				if (nblines != null) {
					nblines.incrementAndGet();
				}
				if (first) {
					first = false;
				} else {
					content.append("\n"); //$NON-NLS-1$
				}
				content.append(line);
				line = reader.readLine();
			}
		}
		return content.toString();
	}

	/** Format the given text in order to be suitable for being output as a block.
	 *
	 * @param content the text.
	 * @param languageName the name of the output language.
	 * @param blockFormat the formatting template.
	 * @return the formatted text.
	 */
	protected static String formatBlockText(String content, String languageName, Function2<String, String, String> blockFormat) {
		String replacement = Strings.nullToEmpty(content);
		final String[] lines = replacement.trim().split("[\n\r]+"); //$NON-NLS-1$
		int minIndent = Integer.MAX_VALUE;
		final Pattern wpPattern = Pattern.compile("^(\\s*)[^\\s]"); //$NON-NLS-1$
		for (int i = lines.length > 1 ? 1 : 0; i < lines.length; ++i) {
			final String line = lines[i];
			final Matcher matcher = wpPattern.matcher(line);
			if (matcher.find()) {
				final int n = matcher.group(1).length();
				if (n < minIndent) {
					minIndent = n;
					if (minIndent <= 0) {
						break;
					}
				}
			}
		}
		final StringBuilder buffer = new StringBuilder();
		buffer.append(lines[0]);
		buffer.append("\n"); //$NON-NLS-1$
		for (int i = 1; i < lines.length; ++i) {
			final String line = lines[i];
			buffer.append(line.replaceFirst("^\\s{0," + minIndent + "}", "")); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
			buffer.append("\n"); //$NON-NLS-1$
		}
		replacement = buffer.toString().replaceFirst("[\n\r]+$", "\n"); //$NON-NLS-1$ //$NON-NLS-2$
		if (!Strings.isNullOrEmpty(replacement) && !"\n".equals(replacement)) { //$NON-NLS-1$
			if (blockFormat != null) {
				return blockFormat.apply(languageName, replacement);
			}
			return replacement;
		}
		return ""; //$NON-NLS-1$
	}

	/** Exception in parser.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class ParsingException extends RuntimeException {

		private static final long serialVersionUID = 6654436628872799744L;

		private final File file;

		private final int lineno;

		/** Constructor.
		 *
		 * @param message the message.
		 * @param file the file with error.
		 * @param lineno the line number of the error.
		 */
		public ParsingException(String message, File file, int lineno) {
			super(message);
			this.file = file;
			this.lineno = lineno;
		}

		/** Constructor.
		 *
		 * @param message the message.
		 * @param file the file with error.
		 * @param lineno the line number of the error.
		 * @param cause the cause of the error.
		 */
		public ParsingException(String message, File file, int lineno, Throwable cause) {
			super(message, cause);
			this.file = file;
			this.lineno = lineno;
		}

		/** Replies the file with error.
		 *
		 * @return the file.
		 */
		public File getFile() {
			return this.file;
		}

		/** Replies the line number of the error.
		 *
		 * @return the line number.
		 */
		public int getLineno() {
			return this.lineno;
		}

	}

	/** Interceptor of the parsed elements.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class ParsingContext {

		private Map<String, String> replacements = new TreeMap<>();

		private CharSequence text;

		private Matcher matcher;

		private File currentFile;

		private ParserInterceptor interceptor;

		private String inlineCodeFormat;

		private Function2<String, String, String> blockCodeFormat;

		private String outlineOutputTag;

		private int startIndex;

		private Stage stage = Stage.FIRST;

		private boolean inParameter;

		private boolean inBlock;

		private boolean isVisibleInblock;

		private boolean forceVisibility;

		private boolean[] isParsing = new boolean[] {true};

		private WeakReference<SarlDocumentationParser> parser;

		private int[] lineno = new int[] {1};

		private int endLineno = 1;

		private int[] offset = new int[] {0};

		private int length;

		private String lineSeparator;

		private String outputLanguage;

		private ScriptExecutor scriptExecutor;

		private boolean isTestingPhase;

		/** Constructor with standard visibility configuration.
		 */
		public ParsingContext() {
			//
		}

		/** Constructor with specific visibility configuration.
		 *
		 * @param forceVisibility forces all the elements to be visible. The visiblilty tags such as
		 *     {@link Tag#ON} and {@link Tag#OFF} will have no effect if the value of this argument is
		 *     {@code true}.
		 * @param isTestingPhase indicates if the context is created for testing phase.
		 */
		public ParsingContext(boolean forceVisibility, boolean isTestingPhase) {
			this.forceVisibility = forceVisibility;
			this.isTestingPhase = isTestingPhase;
		}

		/** Change the script executor.
		 *
		 * @param executor the script executor.
		 */
		public void setScriptExecutor(ScriptExecutor executor) {
			this.scriptExecutor = executor;
		}

		/** Replies the script executor.
		 *
		 * @return the script executor.
		 */
		public ScriptExecutor getScriptExecutor() {
			return this.scriptExecutor;
		}

		/** Change the name of the language.
		 *
		 * @param outputLanguage the language name, or {@code null} for ignoring the language name.
		 */
		public void setOutputLanguage(String outputLanguage) {
			this.outputLanguage = outputLanguage;
		}

		/** Replies the name of the language.
		 *
		 * @return the language name, or {@code null} for ignoring the language name.
		 */
		public String getOutputLanguage() {
			return this.outputLanguage;
		}

		/** Replies the line separator.
		 *
		 * @return the line separator.
		 */
		public String getLineSeparator() {
			return this.lineSeparator;
		}

		/** Change the line separator in the context.
		 *
		 * @param lineSeparator the line separator.
		 */
		public void setLineSeparator(String lineSeparator) {
			this.lineSeparator = lineSeparator;
		}

		/** Replies the line number.
		 *
		 * @return the line number.
		 */
		public int getLineNo() {
			return this.lineno[0];
		}

		/** Increment the line number.
		 */
		public void incrementLineNo() {
			++(this.lineno[0]);
			if (this.lineno[0] > this.endLineno) {
				this.endLineno = this.lineno[0];
			}
		}

		/** Increment the line number.
		 *
		 * @param amount the amount.
		 */
		public void incrementLineNo(int amount) {
			if (amount > 0) {
				(this.lineno[0]) += amount;
				if (this.lineno[0] > this.endLineno) {
					this.endLineno = this.lineno[0];
				}
			}
		}

		/** Change the line number.
		 *
		 * @param lineno the line number.
		 */
		public void setLineNo(int lineno) {
			this.lineno[0] = lineno;
			if (this.lineno[0] > this.endLineno) {
				this.endLineno = this.lineno[0];
			}
		}

		/** Change the end line number.
		 *
		 * @param lineno the end line number.
		 * @since 0.11
		 */
		public void setEndLineNo(int lineno) {
			this.endLineno = lineno;
		}

		/** Replies the end line number.
		 *
		 * @return the end line number.
		 * @since 0.11
		 */
		public int getEndLineNo() {
			return this.endLineno;
		}

		/** Replies the offset.
		 *
		 * @return the offset.
		 * @since 0.11
		 */
		public int getOffset() {
			return this.offset[0];
		}

		/** Increment the offset.
		 *
		 * @param amount the amount for increasing the offset.
		 * @since 0.11
		 */
		public void incrementOffset(int amount) {
			if (amount > 0) {
				(this.offset[0]) += amount;
			}
		}

		/** Change the offset.
		 *
		 * @param offset the offset.
		 * @since 0.11
		 */
		public void setOffset(int offset) {
			this.offset[0] = offset;
		}

		/** Replies the length.
		 *
		 * @return the length.
		 * @since 0.11
		 */
		public int getLength() {
			return this.length;
		}

		/** Change the length.
		 *
		 * @param length the length.
		 * @since 0.11
		 */
		public void setLength(int length) {
			this.length = length;
		}

		@Override
		public String toString() {
			return this.interceptor.toString();
		}

		/** Set the flag that indicates if the text is visible when inside a block.
		 *
		 * @param visible {@code true} if the content should be visible.
		 */
		public void setVisibleInBlock(boolean visible) {
			this.isVisibleInblock = visible;
		}

		/** Replies if the text is visible.
		 *
		 * @return {@code true} if the content is outside a block or the visibility flag is on.
		 */
		public boolean isVisible() {
			return !isInBlock() || this.isVisibleInblock || this.forceVisibility;
		}

		/** Replies if the context is created within a testing phase.
		 *
		 * <p>Usually, no code for generated the marker language is run during a testing phase.
		 *
		 * @return {@code true} if the content is outside a block or the visibility flag is on.
		 */
		public boolean isTestingPhase() {
			return this.isTestingPhase;
		}

		/** Set the flag that indicates if the parser is on.
		 *
		 * @param enable {@code true} if parser is on.
		 */
		public void setParsing(boolean enable) {
			this.isParsing[0] = enable;
		}

		/** Replies the flag that indicates if the parser is on.
		 *
		 * @return {@code true} if parser is on.
		 */
		public boolean isParsing() {
			return this.isParsing[0];
		}

		/** Set if the parsing is for the content of a block tag.
		 *
		 * @param inblock {@code true} if the content is the content of a block.
		 * @return the old value of the flag.
		 */
		public boolean setInBlock(boolean inblock) {
			final boolean old = this.inBlock;
			this.inBlock = inblock;
			return old;
		}

		/** Replies if the parsing is for the content of a block tag.
		 *
		 * @return {@code true} if the content is the content of a block.
		 */
		public boolean isInBlock() {
			return this.inBlock;
		}

		/** Set if the parsing is for the content of a tag parameter.
		 *
		 * @param inparam {@code true} if the content is the content of a parameter.
		 * @return the old value of the flag.
		 */
		public boolean setInParameter(boolean inparam) {
			final boolean old = this.inParameter;
			this.inParameter = inparam;
			return old;
		}

		/** Replies if the parsing is for the content of a tag parameter.
		 *
		 * @return {@code true} if the content is the content of a parameter.
		 */
		public boolean isInParameter() {
			return this.inParameter;
		}

		/** Link this context to the given parent context.
		 *
		 * @param parentContext the parent context.
		 */
		public void linkTo(ParsingContext parentContext) {
			this.replacements = parentContext.replacements;
			this.inBlock = parentContext.inBlock;
			this.isParsing = parentContext.isParsing;
			this.isVisibleInblock = parentContext.isVisibleInblock;
			this.forceVisibility = parentContext.forceVisibility;
			this.isTestingPhase = parentContext.isTestingPhase;
			this.lineno = parentContext.lineno;
			this.offset = parentContext.offset;
			this.scriptExecutor = parentContext.scriptExecutor;
		}

		/** Declare a replacement.
		 *
		 * @param id the identifier of the replacement.
		 * @param to the replacement value.
		 */
		public void declareReplacement(String id, String to) {
			this.replacements.put(id, to);
		}

		/** Get the replacement for the given id.
		 *
		 * @param id the identifier of the replacement.
		 * @return the replacement value, or {@code null} if none.
		 */
		public String getReplacement(String id) {
			return this.replacements.get(id);
		}

		/** Change the stage.
		 *
		 * @param stage the stage level.
		 */
		protected void setStage(Stage stage) {
			assert stage != null;
			this.stage = stage;
		}

		/** Replies the stage.
		 *
		 * @return the stage level.
		 */
		public Stage getStage() {
			return this.stage;
		}

		/** Change the text.
		 *
		 * @param text the text.
		 */
		protected void setText(CharSequence text) {
			this.text = text;
		}

		/** Replies the text.
		 *
		 * @return the text.
		 */
		public CharSequence getText() {
			return this.text;
		}

		/** Change the parser reference.
		 *
		 * @param parser the parser.
		 */
		protected void setParser(SarlDocumentationParser parser) {
			this.parser = new WeakReference<>(parser);
		}

		/** Replies the parser reference.
		 *
		 * @return the start index.
		 */
		public SarlDocumentationParser getParser() {
			return this.parser.get();
		}

		/** Change the start index of the context.
		 *
		 * @param startIndex the start index.
		 */
		protected void setStartIndex(int startIndex) {
			this.startIndex = startIndex;
		}

		/** Replies the start index of the context.
		 *
		 * @return the start index.
		 */
		public int getStartIndex() {
			return this.startIndex;
		}

		/** Change the inline code format.
		 *
		 * @param format the inline format.
		 */
		protected void setInlineCodeFormat(String format) {
			this.inlineCodeFormat = format;
		}

		/** Replies the inline code format that is compatible with {@link MessageFormat}.
		 *
		 * <p>The first argument is the code to inline.
		 *
		 * @return the format.
		 */
		public String getInlineCodeFormat() {
			return this.inlineCodeFormat;
		}

		/** Change the block code format.
		 *
		 * @param format the block code format.
		 */
		protected void setBlockCodeFormat(Function2<String, String, String> format) {
			this.blockCodeFormat = format;
		}

		/** Replies the inline format that is compatible with {@link MessageFormat}.
		 *
		 * <p>The first argument is the code to put in a block.
		 *
		 * @return the format.
		 */
		public Function2<String, String, String> getBlockCodeFormat() {
			return this.blockCodeFormat;
		}

		/** Change the outline output tag that will be output when the outline
		 * tag is found in the input content.
		 *
		 * @param tag the tag for outline.
		 */
		protected void setOutlineOutputTag(String tag) {
			this.outlineOutputTag = tag;
		}

		/** Replies the outline output tag that will be output when the outline
		 * tag is found in the input content.
		 *
		 * @return the tag.
		 */
		public String getOutlineOutputTag() {
			return this.outlineOutputTag;
		}

		/** Change the matcher.
		 *
		 * @param matcher the matcher.
		 */
		protected void setMatcher(Matcher matcher) {
			this.matcher = matcher;
		}

		/** Replies the matcher.
		 *
		 * @return the matcher.
		 */
		public Matcher getMatcher() {
			return this.matcher;
		}

		/** Change the current file.
		 *
		 * @param file the current file.
		 */
		protected void setCurrentFile(File file) {
			this.currentFile = file;
		}

		/** Replies the current directory.
		 *
		 * @return the current directory.
		 */
		public File getCurrentDirectory() {
			return this.currentFile.getParentFile();
		}

		/** Replies the current file.
		 *
		 * @return the current file.
		 */
		public File getCurrentFile() {
			return this.currentFile;
		}

		/** Change the parser interceptor.
		 *
		 * @param interceptor the interceptor.
		 */
		protected void setParserInterceptor(ParserInterceptor interceptor) {
			this.interceptor = interceptor;
		}

		/** Replies the parser interceptor.
		 *
		 * @return the parser interceptor.
		 */
		public ParserInterceptor getParserInterceptor() {
			return this.interceptor;
		}

	}

	/** Interceptor of the parsed elements.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public interface ParserInterceptor {

		/** A context block has started.
		 *
		 * @param context the parsing context.
		 */
		default void openContext(ParsingContext context) {
			//
		}

		/** A context block has finished.
		 *
		 * @param context the parsing context.
		 */
		default void closeContext(ParsingContext context) {
			//
		}

		/** A simple tag was found.
		 *
		 * @param context the parsing context.
		 * @param tag the found tag.
		 * @param dynamicName the name of the tag if it could be dynamically defined, or {@code null} if not.
		 * @param parameter the parameter value or {@code null} if no parameter was given
		 *     or the parameter value is empty.
		 * @param blockValue the value inside the block. It is {@code null} if the tag is not a block tag.
		 */
		default void tag(ParsingContext context, Tag tag, String dynamicName, String parameter, String blockValue) {
			//
		}

		/** A outline tag is detected.
		 *
		 * @param context the parsing context.
		 */
		default void outline(ParsingContext context) {
			//
		}

	}

	/** Interceptor of the parsed elements for the parameters.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class DelegateParserInterceptor implements ParserInterceptor {

		private ParserInterceptor delegate;

		/** Constructor with no delegate.
		 */
		public DelegateParserInterceptor() {
			//
		}

		/** Constructor delegate.
		 *
		 * @param delegate the delegate.
		 */
		public DelegateParserInterceptor(ParserInterceptor delegate) {
			this.delegate = delegate;
		}

		/** Change the delegate.
		 *
		 * @param delegate the delegate.
		 */
		public void setDelegate(ParserInterceptor delegate) {
			this.delegate = delegate;
		}

		/** Replies the delegate.
		 *
		 * @return the delegate.
		 */
		public ParserInterceptor getDelegate() {
			return this.delegate;
		}

		@Override
		public void openContext(ParsingContext context) {
			final ParserInterceptor delegate = getDelegate();
			if (delegate != null) {
				delegate.openContext(context);
			}
		}

		@Override
		public void closeContext(ParsingContext context) {
			final ParserInterceptor delegate = getDelegate();
			if (delegate != null) {
				delegate.closeContext(context);
			}
		}

		@Override
		public void tag(ParsingContext context, Tag tag, String dynamicName, String parameter, String blockValue) {
			final ParserInterceptor delegate = getDelegate();
			if (delegate != null) {
				delegate.tag(context, tag, dynamicName, parameter, blockValue);
			}
		}

		@Override
		public void outline(ParsingContext context) {
			final ParserInterceptor delegate = getDelegate();
			if (delegate != null) {
				delegate.outline(context);
			}
		}

	}

	/** Interceptor of the parsed elements for the parameters.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	private static class ContentParserInterceptor extends DelegateParserInterceptor {

		final StringBuffer buffer = new StringBuffer();

		/** Constructor.
		 *
		 * @param delegate the delegate.
		 */
		ContentParserInterceptor(ParserInterceptor delegate) {
			ParserInterceptor del = delegate;
			while (del instanceof ContentParserInterceptor) {
				del = ((ContentParserInterceptor) del).getDelegate();
			}
			setDelegate(del);
		}

		/** Constructor.
		 */
		ContentParserInterceptor() {
			super();
		}

		@Override
		public String toString() {
			return getResult();
		}

		/** Replies the value of the buffer related to the transformation process.
		 *
		 * <p>The result should be the one that is expected as the generated file by this parser.
		 *
		 * @return the value.
		 */
		public String getResult() {
			return this.buffer.toString();
		}

		private StringBuffer getVisibleBuffer(boolean isVisible) {
			return isVisible ? this.buffer : new StringBuffer();
		}

		@Override
		public void closeContext(ParsingContext context) {
			context.getMatcher().appendTail(getVisibleBuffer(context.isVisible()));
		}

		@Override
		public void tag(ParsingContext context, Tag tag, String dynamicName, String parameter, String blockValue) {
			final boolean isVisible = context.isVisible();
			final String replacement = tag.passThrough(context, dynamicName, parameter, blockValue);
			context.getMatcher().appendReplacement(getVisibleBuffer(isVisible), replacement);
			super.tag(context, tag, dynamicName, parameter, blockValue);
		}

	}

	/** Definition of the special tags.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public enum Stage {
		/** Stage 1: general parsing.
		 */
		FIRST {
			@Override
			public Stage next() {
				return SECOND;
			}
		},

		/** Stage 2: Replacements with captured texts.
		 */
		SECOND {
			@Override
			public Stage next() {
				return null;
			}
		};

		/** Replies the first stage.
		 *
		 * @return the first stage.
		 */
		public static Stage first() {
			return FIRST;
		}

		/** Replies the next stage.
		 *
		 * @return the next stage.
		 */
		public abstract Stage next();

	}

	/** Definition of the special tags.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public enum Tag {

		/** {@code [:ParserOn]} switches on the parser.
		 */
		PARSER_ON {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_PARSERON_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				context.setParsing(true);
				return context.getStage() == Stage.SECOND ? "" : "[:ParserOn]"; //$NON-NLS-1$ //$NON-NLS-2$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return true;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:ParserOff]} switches off the parser.
		 */
		PARSER_OFF {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_PARSEROFF_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				context.setParsing(false);
				return context.getStage() == Stage.SECOND ? "" : "[:ParserOff]"; //$NON-NLS-1$ //$NON-NLS-2$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return true;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Outline:]} is replaced by the page's outline.
		 */
		OUTLINE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_OUTLINE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				context.getParserInterceptor().outline(context);
				if (!context.isTestingPhase()) {
					final String tag = context.getOutlineOutputTag();
					if (!Strings.isNullOrEmpty(tag)) {
						return Strings.nullToEmpty(context.getOutlineOutputTag());
					}
				}
				return ""; //$NON-NLS-1$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return true;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Include:](path)} enables to include of files.
		 */
		INCLUDE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_INCLUDE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return true;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (parameter == null) {
					reportError(context, Messages.SarlDocumentationParser_2, name());
					return null;
				}
				final File filename = FileSystem.convertStringToFile(parameter);
				try {
					final int oldLine = context.getLineNo();
					final int oldEndLine = context.getEndLineNo();
					final int oldOffset = context.getOffset();
					final int oldLength = context.getLength();
					final File oldFile = context.getCurrentFile();
					final String fileContent = read(context, filename);
					context.setLineNo(1);
					context.setEndLineNo(1);
					context.setOffset(0);
					context.setLength(0);
					context.setCurrentFile(filename);
					final ContentParserInterceptor subInterceptor = new ContentParserInterceptor(context.getParserInterceptor());
					context.getParser().parse(
							fileContent,
							filename,
							0,
							context.getStage(),
							context,
							subInterceptor);
					context.setLineNo(oldLine);
					context.setEndLineNo(oldEndLine);
					context.setOffset(oldOffset);
					context.setLength(oldLength);
					context.setCurrentFile(oldFile);
					return subInterceptor.getResult();
				} catch (IOException exception) {
					reportError(context, Messages.SarlDocumentationParser_3, exception);
					return null;
				}
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Fact:](expression)} tests the given expression to be true or not {@code null}.
		 */
		FACT {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_FACT_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return true;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (context.isInBlock()) {
					reportError(context, Messages.SarlDocumentationParser_5, name());
					return null;
				}
				return ""; //$NON-NLS-1$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return true;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Dynamic:](code)} returns the text to put in the documentation text.
		 */
		DYNAMIC {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_DYNAMIC_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return true;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (context.isInBlock()) {
					reportError(context, Messages.SarlDocumentationParser_5, name());
					return null;
				}
				if (!context.isTestingPhase()) {
					String code = parameter;
					if (Strings.isNullOrEmpty(code)) {
						code = blockValue;
					}
					if (!Strings.isNullOrEmpty(code)) {
						final ScriptExecutor executor = context.getScriptExecutor();
						if (executor != null) {
							System.setProperty(ScriptExecutor.PROP_CURRENT_FILE, context.getCurrentFile().getAbsolutePath());
							System.setProperty(ScriptExecutor.PROP_CURRENT_FOLDER, context.getCurrentFile().getParentFile().getAbsolutePath());
							try {
								final Object result = executor.execute(context.getLineNo(), code);
								if (result != null) {
									final String stringResult = Strings.nullToEmpty(Objects.toString(result));
									return stringResult;
								}
							} catch (NoXtextResourceException exception) {
								// Ignore this exception because it is already logged by the SARL compiler
							} catch (Throwable exception) {
								final Throwable root = Throwables.getRootCause(exception);
								Throwables.throwIfUnchecked(root);
							    throw new RuntimeException(root);
							}
						}
					}
				}
				return ""; //$NON-NLS-1$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:DynamicCode:](code)} returns the text to put in the documentation text within a code block.
		 *
		 * @since 0.7
		 */
		DYNAMIC_CODE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_DYNAMIC_CODE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return true;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (context.isInBlock()) {
					reportError(context, Messages.SarlDocumentationParser_5, name());
					return null;
				}
				if (!context.isTestingPhase()) {
					String code = blockValue;
					if (Strings.isNullOrEmpty(code)) {
						code = parameter;
					}
					if (!Strings.isNullOrEmpty(code)) {
						final ScriptExecutor executor = context.getScriptExecutor();
						if (executor != null) {
							System.setProperty(ScriptExecutor.PROP_CURRENT_FILE, context.getCurrentFile().getAbsolutePath());
							System.setProperty(ScriptExecutor.PROP_CURRENT_FOLDER, context.getCurrentFile().getParentFile().getAbsolutePath());
							try {
								final Object result = executor.execute(context.getLineNo(), code);
								if (result != null) {
									final String stringResult = Strings.nullToEmpty(Objects.toString(result));
									return formatBlockText(stringResult, context.getOutputLanguage(), context.getBlockCodeFormat());
								}
							} catch (NoXtextResourceException exception) {
								// Ignore this exception because it is already logged by the SARL compiler
							} catch (Throwable exception) {
								final Throwable root = Throwables.getRootCause(exception);
								Throwables.throwIfUnchecked(root);
							    throw new RuntimeException(root);
							}
						}
					}
				}
				return ""; //$NON-NLS-1$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return true;
			}
		},

		/** {@code [:On]} switches on the output of the code.
		 */
		ON {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_ON_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				context.setVisibleInBlock(true);
				return ""; //$NON-NLS-1$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Off]} switches off the output of the code.
		 */
		OFF {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_OFF_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				context.setVisibleInBlock(false);
				return ""; //$NON-NLS-1$
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Success:]} starts a block of code should be successfull when compiled.
		 */
		SUCCESS {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_SUCCESS_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return true;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (context.isInBlock()) {
					reportError(context, Messages.SarlDocumentationParser_5, name());
					return null;
				}
				return formatBlockText(blockValue, context.getOutputLanguage(), context.getBlockCodeFormat());
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return true;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:Failure:]} starts a block of code should not be successfull when compiled.
		 */
		FAILURE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_FAILURE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return true;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (context.isInBlock()) {
					reportError(context, Messages.SarlDocumentationParser_5, name());
					return null;
				}
				return formatBlockText(blockValue, context.getOutputLanguage(), context.getBlockCodeFormat());
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return true;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code &lt;--- comment --&gt;}.
		 */
		COMMENT {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_COMMENT_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				/*final String text = Strings.nullToEmpty(blockValue);
				final String[] lines = text.split(Pattern.quote(context.getLineSeparator()));
				context.incrementLineNo_(lines.length);*/
				return new String();
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return true;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return true;
			}
		},

		/** {@code [:ShowType:]} outputs the Java type with a SARL syntax.
		 */
		SHOW_TYPE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_SHOWTYPE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return false;
			}

			@Override
			public boolean hasParameter() {
				return true;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				if (context.isInBlock() || context.isInParameter()) {
					reportError(context, Messages.SarlDocumentationParser_5, name());
					return null;
				}
				if (!context.isTestingPhase()) {
					final Class<?> javaType;
					try {
						javaType = ReflectionUtil.forName(parameter);
					} catch (ClassNotFoundException exception) {
						reportError(context, Messages.SarlDocumentationParser_0, exception);
						return null;
					}
					final String block;
					if (javaType.isInterface()) {
						if (Capacity.class.isAssignableFrom(javaType)) {
							block = extractCapacity(javaType.asSubclass(Capacity.class));
						} else {
							block = extractInterface(javaType);
						}
					} else if (javaType.isEnum()) {
						block = extractEnumeration(javaType);
					} else if (javaType.isAnnotation()) {
						block = extractAnnotation(javaType);
					} else {
						if (Event.class.isAssignableFrom(javaType)) {
							block = extractEvent(javaType.asSubclass(Event.class));
						} else {
							block = extractClass(javaType);
						}
					}
					return formatBlockText(block, context.getOutputLanguage(), context.getBlockCodeFormat());
				}
				return ""; //$NON-NLS-1$
			}

			private void appendGenericTypes(StringBuilder it, Class<?> type) {
				if (type.getTypeParameters() != null && type.getTypeParameters().length > 0) {
					it.append("<"); //$NON-NLS-1$
					boolean first = true;
					for (final TypeVariable<?> genType : type.getTypeParameters()) {
						if (first) {
							first = false;
						} else {
							it.append(", ");
						}
						it.append(genType.getName());
					}
					it.append(">"); //$NON-NLS-1$
				}
			}

			private String extractCapacity(Class<? extends Capacity> type) {
				final StringBuilder it = new StringBuilder();
				it.append("capacity ").append(type.getSimpleName()); //$NON-NLS-1$
				if (type.getSuperclass() != null && !Capacity.class.equals(type.getSuperclass())) {
					it.append(" extends ").append(type.getSuperclass().getSimpleName()); //$NON-NLS-1$
				}
				it.append(" {\n"); //$NON-NLS-1$
				ReflectExtensions.appendPublicMethods(it, true, type);
				it.append("}"); //$NON-NLS-1$
				return it.toString();
			}

			private String extractEvent(Class<? extends Event> type) {
				final StringBuilder it = new StringBuilder();
				it.append("event ").append(type.getSimpleName()); //$NON-NLS-1$
				if (type.getSuperclass() != null && !Event.class.equals(type.getSuperclass())) {
					it.append(" extends ").append(type.getSuperclass().getSimpleName()); //$NON-NLS-1$
				}
				it.append(" {\n"); //$NON-NLS-1$
				ReflectExtensions.appendPublicFields(it, true, type);
				it.append("}"); //$NON-NLS-1$
				return it.toString();
			}

			private String extractInterface(Class<?> type) {
				final StringBuilder it = new StringBuilder();
				it.append("interface ").append(type.getSimpleName()); //$NON-NLS-1$
				appendGenericTypes(it, type);
				if (type.getSuperclass() != null && !Object.class.equals(type.getSuperclass())) {
					it.append(" extends ").append(type.getSuperclass().getSimpleName()); //$NON-NLS-1$
				}
				it.append(" {\n"); //$NON-NLS-1$
				ReflectExtensions.appendPublicMethods(it, true, type);
				it.append("}"); //$NON-NLS-1$
				return it.toString();
			}

			private String extractEnumeration(Class<?> type) {
				final StringBuilder it = new StringBuilder();
				it.append("enum ").append(type.getSimpleName()); //$NON-NLS-1$
				it.append(" {\n"); //$NON-NLS-1$
				for (final Object cst : type.getEnumConstants()) {
					it.append("\t").append(((Enum<?>) cst).name()).append(",\n"); //$NON-NLS-1$ //$NON-NLS-2$
				}
				it.append("}"); //$NON-NLS-1$
				return it.toString();
			}

			private String extractAnnotation(Class<?> type) {
				final StringBuilder it = new StringBuilder();
				it.append("annotation ").append(type.getSimpleName()); //$NON-NLS-1$
				it.append(" {\n"); //$NON-NLS-1$
				ReflectExtensions.appendPublicMethods(it, true, type);
				it.append("}"); //$NON-NLS-1$
				return it.toString();
			}

			private String extractClass(Class<?> type) {
				final StringBuilder it = new StringBuilder();
				it.append("class ").append(type.getSimpleName()); //$NON-NLS-1$
				appendGenericTypes(it, type);
				if (type.getSuperclass() != null && !Object.class.equals(type.getSuperclass())) {
					it.append(" extends ").append(type.getSuperclass().getSimpleName()); //$NON-NLS-1$
				}
				if (type.getInterfaces().length > 0) {
					if (type.getSuperclass() != null && !Object.class.equals(type.getSuperclass())) {
						it.append("\n\t\t"); //$NON-NLS-1$
					} else {
						it.append(" "); //$NON-NLS-1$
					}
					it.append("implements "); //$NON-NLS-1$
					boolean first = true;
					for (final Class<?> interfaceType : type.getInterfaces()) {
						if (first) {
							first = false;
						} else {
							it.append(", "); //$NON-NLS-1$
						}
						it.append(interfaceType.getSimpleName());
					}
				}
				it.append(" {\n"); //$NON-NLS-1$
				ReflectExtensions.appendPublicMethods(it, true, type);
				it.append("}"); //$NON-NLS-1$
				return it.toString();
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return true;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:id:]} is replaced by the saved text with the given id with code block.
		 */
		REFERENCE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_REFERENCE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return true;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				final String replacement = getCapturedValue(context, dynamicTag);
				if (!context.isInBlock() && !context.isInParameter()) {
					final String format = context.getInlineCodeFormat();
					if (!Strings.isNullOrEmpty(format)) {
						return MessageFormat.format(format, replacement);
					}
				}
				return replacement;
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.SECOND;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:id!]} is replaced by the saved text with the given id without code block.
		 */
		RAW_REFERENCE {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_RAW_REFERENCE_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return true;
			}

			@Override
			public boolean hasParameter() {
				return false;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				return getCapturedValue(context, dynamicTag);
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.SECOND;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		},

		/** {@code [:id](value)} saves the given text value with the given id.
		 */
		DEFINITION {
			@Override
			public String getDefaultPattern() {
				return DEFAULT_DEFINITION_PATTERN;
			}

			@Override
			public boolean hasDynamicName() {
				return true;
			}

			@Override
			public boolean hasParameter() {
				return true;
			}

			@Override
			public boolean isOpeningTag() {
				return false;
			}

			@Override
			public String passThrough(ParsingContext context, String dynamicTag, String parameter, String blockValue) {
				context.declareReplacement(dynamicTag, parameter);
				return parameter;
			}

			@Override
			public boolean isActive(ParsingContext context) {
				return context.isParsing() && context.getStage() == Stage.FIRST;
			}

			@Override
			public boolean isEnclosingSpaceCouldRemovable() {
				return false;
			}

			@Override
			public boolean isInternalTextAsBlockContent() {
				return false;
			}
		};

		/** Default pattern.
		 */
		static final String DEFAULT_OUTLINE_PATTERN = "\\[:Outline:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_INCLUDE_PATTERN = "\\[:Include:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_FACT_PATTERN = "\\[:Fact:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_DYNAMIC_PATTERN = "\\[:Dynamic:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 *
		 * @since 0.7
		 */
		static final String DEFAULT_DYNAMIC_CODE_PATTERN = "\\[:DynamicCode:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_ON_PATTERN = "\\[:On\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_OFF_PATTERN = "\\[:Off\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_REFERENCE_PATTERN = "\\[:[a-zA-Z0-9\\._\\-@~]+:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_RAW_REFERENCE_PATTERN = "\\[:[a-zA-Z0-9\\._\\-@~]+\\!\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_DEFINITION_PATTERN = "\\[:[a-zA-Z0-9\\._\\-@~]+\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_SUCCESS_PATTERN = "\\[:Success:\\](.*?)\\[:End:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_FAILURE_PATTERN = "\\[:Failure:\\](.*?)\\[:End:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_SHOWTYPE_PATTERN = "\\[:ShowType:\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_COMMENT_PATTERN = "\\<\\!\\-{3}(.*?)\\-{2}\\>"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_PARSERON_PATTERN = "\\[:ParserOn\\]"; //$NON-NLS-1$

		/** Default pattern.
		 */
		static final String DEFAULT_PARSEROFF_PATTERN = "\\[:ParserOff\\]"; //$NON-NLS-1$

		/** Replies the default pattern.
		 *
		 * @return the pattern.
		 */
		public abstract String getDefaultPattern();

		/** Replies if the tag has a parameter.
		 *
		 * @return {@code true} if a parameter is needed for the tag.
		 */
		public abstract boolean hasParameter();

		/** Replies if the tag has a dynamic name.
		 *
		 * @return {@code true} if the tag may have a dynamic name.
		 */
		public abstract boolean hasDynamicName();

		/** Replies if the tag is an opening tag.
		 *
		 * @return {@code true} if the tag is opening a block.
		 */
		public abstract boolean isOpeningTag();

		/** Replies if the tag has an internal group that should be given to the
		 * {@link #passThrough(ParsingContext, String, String, String)}
		 * as the block content.
		 *
		 * @return {@code true} if the tag contains a block of text.
		 */
		public abstract boolean isInternalTextAsBlockContent();

		/** Replies the string representation when this tag is interpreted.
		 *
		 * <p>Usually, this function is called for obtaining the raw text,
		 * without the tags.
		 *
		 * @param context the parsing context.
		 * @param dynamicName the name of the tag if it could be dynamically defined, or {@code null}.
		 * @param parameter the value of tag parameter, or {@code null} if no parameter value.
		 * @param blockValue the value inside the block, or {@code null} if not a block tag.
		 * @return the raw text for the tag.
		 */
		public abstract String passThrough(ParsingContext context, String dynamicName, String parameter, String blockValue);

		/** Replies if the space around the tag could be removed or not.
		 *
		 * @return {@code true} of enclosing spaces could be removed.
		 */
		public abstract boolean isEnclosingSpaceCouldRemovable();

		/** Replies if the tag is active regarding the current context.
		 *
		 * @param context the context.
		 * @return {@code true} if the tag is active.
		 */
		public abstract boolean isActive(ParsingContext context);

		/** Replies the captured value.
		 *
		 * @param context the parsing context.
		 * @param tagName the name of the tag.
		 * @return the captured value.
		 */
		protected static String getCapturedValue(ParsingContext context, String tagName) {
			String replacement = null;
			if (!Strings.isNullOrEmpty(tagName)) {
				replacement = context.getReplacement(tagName);
				if (replacement == null) {
					for (final Properties provider : context.getParser().getPropertyProvidersByPriority()) {
						if (provider != null) {
							final Object obj = provider.getOrDefault(tagName, null);
							if (obj != null) {
								replacement = obj.toString();
								break;
							}
						}
					}
				}
				if (replacement == null) {
					replacement = System.getProperty(tagName);
				}
				if (replacement == null) {
					replacement = System.getenv(tagName);
				}
			}
			if (replacement == null) {
				reportError(context, Messages.SarlDocumentationParser_4, tagName);
				return null;
			}
			return replacement;
		}

	}

}
