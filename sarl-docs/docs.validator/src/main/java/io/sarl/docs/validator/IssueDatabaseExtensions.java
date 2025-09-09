/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.docs.validator;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Modifier;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.google.gson.Gson;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.json.JsonBuffer;
import org.arakhne.afc.vmutil.json.JsonableObject;
import org.eclipse.xtext.diagnostics.Diagnostic;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.validation.IssueCodes;
import io.sarl.lang.validation.SyntaxIssueCodes;

/** Extended Functions for obtaining information on SARL issues.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version docs.validator 0.15.0 20250909-115750
 * @mavengroupid io.sarl.docs
 * @mavenartifactid docs.validator
 * @since 0.12
 */
public final class IssueDatabaseExtensions {

	private static String DEFAULT_ISSUE_DESCRIPTION_FILENAME = "issue_descriptions.json"; //$NON-NLS-1$

	private static String DEFAULT_SARL_ISSUE_DESCRIPTION_FILENAME = "issue_descriptions_sarl.json"; //$NON-NLS-1$

	private static String DEFAULT_XTEND_ISSUE_DESCRIPTION_FILENAME = "issue_descriptions_xtend.json"; //$NON-NLS-1$

	private static String DEFAULT_XBASE_ISSUE_DESCRIPTION_FILENAME = "issue_descriptions_xbase.json"; //$NON-NLS-1$

	private static String DEFAULT_XTEXT_ISSUE_DESCRIPTION_FILENAME = "issue_descriptions_xtext.json"; //$NON-NLS-1$

	private static final String[] MESSAGES_TO_IGNORE = {
			"AbstractSARLSubValidator_3", //$NON-NLS-1$
			"AbstractSARLSubValidator_4", //$NON-NLS-1$
			"AbstractSARLSubValidator_5", //$NON-NLS-1$
			"AbstractSARLSubValidator_6", //$NON-NLS-1$
			"AbstractSARLSubValidator_7", //$NON-NLS-1$
			"AbstractSARLSubValidator_8", //$NON-NLS-1$
			"SARLBehaviorUnitValidator_10", //$NON-NLS-1$
			"SARLFeatureModifierValidator_1", //$NON-NLS-1$
			"SARLFeatureModifierValidator_3", //$NON-NLS-1$
			"SARLFeatureModifierValidator_11", //$NON-NLS-1$
			"SARLInheritanceValidator_4", //$NON-NLS-1$
			"SARLInheritanceValidator_5", //$NON-NLS-1$
			"SARLMemberValidator_4", //$NON-NLS-1$
			"SARLMemberValidator_5", //$NON-NLS-1$
			"SARLMemberValidator_31",  //$NON-NLS-1$
			"SARLMemberValidator_32", //$NON-NLS-1$
			"SARLModifierValidator_4", //$NON-NLS-1$
			"SARLModifierValidator_5", //$NON-NLS-1$
			"SARLNamingValidator_3", //$NON-NLS-1$
			"SARLNamingValidator_7", //$NON-NLS-1$
			"SARLNamingValidator_8", //$NON-NLS-1$
	};

	private static final String HIDDEN_STRING = "****"; //$NON-NLS-1$
	
	/** Read the default property files for SARL that describes the issues of the SARL compiler.
	 * The files are {@code issue_descriptions_sarl.json}, {@code issue_descriptions_xtend.json},
	 * {@code issue_descriptions_xbase.json} and {@code issue_descriptions_xtext.json}.
	 *
	 * @return the description of the issues.
	 * @since 0.14
	 */
	@Pure
	public static List<IssueDescription> readIssueDescriptionsFromSarlJsonFiles() {
		final var props = new ArrayList<IssueDescription>();
		readIssueDescriptions(DEFAULT_SARL_ISSUE_DESCRIPTION_FILENAME, props);
		readIssueDescriptions(DEFAULT_XTEND_ISSUE_DESCRIPTION_FILENAME, props);
		readIssueDescriptions(DEFAULT_XBASE_ISSUE_DESCRIPTION_FILENAME, props);
		readIssueDescriptions(DEFAULT_XTEXT_ISSUE_DESCRIPTION_FILENAME, props);
		return props;
	}

	/** Read the default property file {@code issue_descriptions.json} that describes
	 * the issues of the SARL compiler.
	 *
	 * @return the description of the issues.
	 * @since 0.14
	 */
	@Pure
	public static List<IssueDescription> readIssueDescriptionsFromDefaultJsonFile() {
		final var props = new ArrayList<IssueDescription>();
		readIssueDescriptions(DEFAULT_ISSUE_DESCRIPTION_FILENAME, props);
		return props;
	}

	/** Read a property file that describes the issues of the SARL compiler.
	 *
	 * @param filename the file to read.
	 * @return the description of the issues.
	 */
	@Pure
	public static List<IssueDescription> readIssueDescriptions(String filename) {
		final var props = new ArrayList<IssueDescription>();
		readIssueDescriptions(filename, props);
		DocumentationLogger.getLogger().info(MessageFormat.format(Messages.IssueDatabaseExtensions_7,
				Integer.valueOf(props.size()), filename));
		return props;
	}

	/** Read a property file that describes the issues of the SARL compiler.
	 *
	 * @param filename the file to read.
	 * @param descriptions the description of the issues.
	 * @since 0.14
	 */
	public static void readIssueDescriptions(String filename, List<IssueDescription> descriptions) {
		var file = new File(filename);
		if (!file.isAbsolute()) {
			final var folder = FileSystem.convertStringToFile(System.getProperty(ScriptExecutor.PROP_CURRENT_FOLDER));
			assert folder != null : "Folder not found in system properties: " + ScriptExecutor.PROP_CURRENT_FOLDER; //$NON-NLS-1$
			file  = FileSystem.makeAbsolute(file, folder);
			readIssueDescriptions(file, descriptions);
		}
	}

	/** Read a property file that describes the issues of the SARL compiler.
	 *
	 * @param filename the file to read.
	 * @return the description of the issues.
	 */
	@Pure
	public static List<IssueDescription> readIssueDescriptions(File filename) {
		final var props = new ArrayList<IssueDescription>();
		readIssueDescriptions(filename, props);
		DocumentationLogger.getLogger().info(MessageFormat.format(Messages.IssueDatabaseExtensions_7,
				Integer.valueOf(props.size()), filename.getName()));
		return props;
	}

	/** Read a property file that describes the issues of the SARL compiler.
	 *
	 * @param filename the file to read.
	 * @param descriptions the description of the issues.
	 * @since 0.14
	 */
	@SuppressWarnings("resource")
	public static void readIssueDescriptions(File filename, List<IssueDescription> descriptions) {
		final var gson = new Gson();
		try (final var stream = new FileReader(filename)) {
			final var reader = gson.newJsonReader(stream);
			reader.beginArray();
			while (reader.hasNext()) {
				reader.beginObject();
				String code = null;
				String message = null;
				String cause = null;
				String solution = null;
				String levelStr = null;
				boolean checkJavaDef = true;
				while (reader.hasNext()) {
					final var name = reader.nextName();
					switch (Strings.emptyIfNull(name)) {
					case "code": //$NON-NLS-1$
						code = reader.nextString();
						break;
					case "message": //$NON-NLS-1$
						message = reader.nextString();
						break;
					case "cause": //$NON-NLS-1$
						cause = reader.nextString();
						break;
					case "solution": //$NON-NLS-1$
						solution = reader.nextString();
						break;
					case "level": //$NON-NLS-1$
						levelStr = reader.nextString();
						break;
					case "ignoreMessageCheck": //$NON-NLS-1$
						checkJavaDef = !reader.nextBoolean();
						break;
					default:
						reader.skipValue();
					}
				}
				reader.endObject();
				if (!Strings.isEmpty(code)) {
					IssueLevel level = null;
					String delegate = null;
					IssueLevel defaultLevel = null;
					level = IssueLevel.fromJson(levelStr);
					if (level == null) {
						throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_0, code, levelStr));
					}
					if (level == IssueLevel.DELEGATE) {
						delegate = level.getDelegate(levelStr);
						if (Strings.isEmpty(delegate)) {
							throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_1, code, levelStr));
						}
					}
					if (level == IssueLevel.CONFIGURABLE || level == IssueLevel.DELEGATE) {
						defaultLevel = level.getDefaultLevel(levelStr);
						if (defaultLevel == null) {
							throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_2, code, levelStr));
						}
					}
					if (level != IssueLevel.IGNORE) {
						if (Strings.isEmpty(message)) {
							throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_3, code));
						}
						if (Strings.isEmpty(cause)) {
							throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_4, code));
						}
						if (Strings.isEmpty(levelStr)) {
							throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_5, code));
						}
					}
					final var description = new IssueDescription(code);
					description.message = message;
					description.cause = cause;
					description.solution = solution;
					description.level = level;
					description.delegate = Strings.isEmpty(delegate) ? null : delegate;
					description.defaultLevel = defaultLevel;
					description.javaMessageDefinitionCheck = checkJavaDef;
					descriptions.add(description);
				}
			}
			reader.endArray();
		} catch (IOException e) {
			throw new RuntimeException(MessageFormat.format(Messages.IssueDatabaseExtensions_6, e.getLocalizedMessage()), e);
		}
	}

	/** Replies a table description for the list of issues.
	 *
	 * <p>The result is a table with 3 columns:<ul>
	 * <li>Column 1: index of the issue</li>
	 * <li>Column 2: the code of the issue</li>
	 * <li>Column 3: the message and description of the issue</li>
	 * <li>Column 4: the importance level of the issue</li>
	 * </ul>
	 *
	 * @param descriptions the list of issue descriptions to render.
	 * @return the table content.
	 */
	@Pure
	public static List<List<String>> asTable(List<IssueDescription> descriptions) {
		final var content = new ArrayList<List<String>>();
		var major = 0;
		var minor = 1;
		IssueDescription prevIssue = null;
		for (var i = 0; i < descriptions.size(); ++i) {
			final var description = descriptions.get(i);
			if (description.level != IssueLevel.IGNORE) {
				final var columns = new ArrayList<String>();

				// Column "N."
				if (Strings.equal(prevIssue == null ? null : prevIssue.getCode(), description.getCode())) {
					++minor;
				} else {
					++major;
					minor = 1;
				}
	
				final IssueDescription nextIssue;
				if (i < descriptions.size() - 1) {
					nextIssue = descriptions.get(i + 1);
				} else {
					nextIssue = null;
				}

				columns.add(formatIndex(major, minor, prevIssue, description, nextIssue));

				// Column "Message and Description"
				final var msg = new StringBuilder();
				msg.append(head(Messages.IssueDatabaseExtensions_8)).append(description.message).append(nl());
				msg.append(head(Messages.IssueDatabaseExtensions_9)).append(description.cause);
				if (!Strings.isEmpty(description.solution)) {
					msg.append(nl()).append(head(Messages.IssueDatabaseExtensions_10)).append(description.solution);
				}
				if (!Strings.isEmpty(description.delegate)) {
					msg.append(nl()).append(head(Messages.IssueDatabaseExtensions_11)).append(description.delegate);
				}
				columns.add(msg.toString());

				// Column "Level"
				columns.add(description.level.getLabel(description.delegate, description.defaultLevel));
	
				// Column "Code"
				final var code = new StringBuilder();
				code.append("["); //$NON-NLS-1$
				code.append(description.getShortDisplayCode());
				code.append("](: \""); //$NON-NLS-1$
				code.append(description.getLongDisplayCode());
				code.append("\")"); //$NON-NLS-1$
				columns.add(code.toString());

				
				content.add(columns);

				prevIssue = description;
			}
		}
		DocumentationLogger.getLogger().info(MessageFormat.format(Messages.IssueDatabaseExtensions_12,
				Integer.valueOf(descriptions.size()), Integer.valueOf(major)));
		return content;
	}

	private static String formatIndex(int major, int minor, IssueDescription prevIssue, IssueDescription currentIssue,
			IssueDescription nextIssue) {
		final var pcode = prevIssue == null ? null : prevIssue.getCode();
		final var ccode = currentIssue == null ? null : currentIssue.getCode();
		final var ncode = nextIssue == null ? null : nextIssue.getCode();
		if (Strings.equal(pcode, ccode) || Strings.equal(ccode, ncode)) {
			// Long numbering
			final char c = (char) ('a' + (minor - 1));
			return major + Character.toString(c);
		}
		// Short numbering
		return Integer.toString(major);
	}

	/** Sorts the given list and replies it.
	 *
	 * @param descriptions the list to sort.
	 * @return {@code descriptions}.
	 */
	@Pure
	public static List<IssueDescription> sort(List<IssueDescription> descriptions) {
		descriptions.sort((a, b) -> {
			var cmp = a.getShortDisplayCode().compareToIgnoreCase(b.getShortDisplayCode());
			if (cmp != 0) {
				return cmp;
			}
			cmp = a.getCodePrefix().compareToIgnoreCase(b.getCodePrefix());
			if (cmp != 0) {
				return cmp;
			}
			cmp = a.getSortLevel().compareTo(b.getSortLevel());
			if (cmp != 0) {
				return cmp;
			}
			return a.message.compareToIgnoreCase(b.message);
		});
		return descriptions;
	}

	private static String head(String value) {
		return "**" + value + "** "; //$NON-NLS-1$ //$NON-NLS-2$
	}

	private static String nl() {
		return "\n"; //$NON-NLS-1$
	}

	/** Validate the fact that all the issue codes within the given argument are found into
	 * the SARL, Xtend, Xbase and Xtext issue codes.
	 *
	 * @param descriptions the list of issue descriptions to validate.
	 * @return {@code descriptions}
	 */
	@Pure
	public static List<IssueDescription> validateSarlIssueCodes(List<IssueDescription> descriptions) {
		final var definedCodes = buildSarlIssueCodeList();
		DocumentationLogger.getLogger().info(MessageFormat.format(Messages.IssueDatabaseExtensions_13,
				Integer.valueOf(definedCodes.size())));
		if (!validateIssueCodes(descriptions, definedCodes, "SARL")) { //$NON-NLS-1$
			throw new IllegalStateException(Messages.IssueDatabaseExtensions_14);
		}
		return descriptions;
	}

	private static boolean validateIssueCodes(List<IssueDescription> descriptions, Set<String> definedCodes, String label) {
		var success = true;
		final var notDocumentedCodes = new TreeSet<String>();
		notDocumentedCodes.addAll(definedCodes);
		for (final var description : descriptions) {
			notDocumentedCodes.remove(description.getCode());
			if (!definedCodes.contains(description.getCode())) {
				success = false;
				DocumentationLogger.getLogger().severe(MessageFormat.format(Messages.IssueDatabaseExtensions_15, description.getCode(), label));
			}
		}
		if (!success) {
			return false;
		}
		for (final var notDocumentedCode : notDocumentedCodes) {
			success = false;
			DocumentationLogger.getLogger().severe(MessageFormat.format(Messages.IssueDatabaseExtensions_16, notDocumentedCode, label));
		}
		return success;
	}

	/** Validate the fact that all the issue codes within the given argument are found into
	 * the Janus run-time environment.
	 *
	 * @param descriptions the list of issue descriptions to validate.
	 * @param issueListList the classes that contains the issue codes.
	 * @param sourceName the name of the source of the issue codes stored in {@code issueListList}. 
	 * @return {@code descriptions}
	 * @since 0.14
	 */
	@Pure
	public static List<IssueDescription> validate(List<IssueDescription> descriptions, Collection<Class<?>> issueListList, String sourceName) {
		final var definedCodes = buildIssueCodeList(issueListList);
		DocumentationLogger.getLogger().info(MessageFormat.format(Messages.IssueDatabaseExtensions_17,
				Integer.valueOf(definedCodes.size())));
		if (!validateIssueCodes(descriptions, definedCodes, sourceName)) {
			throw new IllegalStateException(MessageFormat.format(Messages.IssueDatabaseExtensions_18, sourceName));
		}
		return descriptions;
	}

	private static Set<String> buildIssueCodeList(Collection<Class<?>> types) {
		final var codes = new TreeSet<String>();
		for (final var type : types) {
			extractIssueCodes(type, codes);
		}
		return codes;
	}

	private static void extractIssueCodes(Class<?> definitions, Set<String> codes) {
		for (final var field : definitions.getDeclaredFields()) {
			if (String.class.equals(field.getType()) && Modifier.isStatic(field.getModifiers())
					&& Modifier.isPublic(field.getModifiers())) {
				try {
					final var value = (String) field.get(null);
					if (!Strings.isEmpty(value) && !value.endsWith(".")) { //$NON-NLS-1$
						codes.add(value);
					}
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
	}

	private static Set<String> buildSarlIssueCodeList() {
		final var codes = new TreeSet<String>();
		// Validation issues
		extractIssueCodes(IssueCodes.class, codes);
		extractIssueCodes(org.eclipse.xtend.core.validation.IssueCodes.class, codes);
		extractIssueCodes(org.eclipse.xtext.xbase.validation.IssueCodes.class, codes);
		// Generation issues
		extractIssueCodes(org.eclipse.xtext.validation.IssueCodes.class, codes);
		// Syntax issues
		extractIssueCodes(SyntaxIssueCodes.class, codes);
		codes.add(Diagnostic.SYNTAX_DIAGNOSTIC);
		codes.add(Diagnostic.SYNTAX_DIAGNOSTIC_WITH_RANGE);
		// Linking issues
		codes.add(Diagnostic.LINKING_DIAGNOSTIC);
		return codes;
	}


	private static Set<String> buildSarlIssueMessageSourceList() {
		final var codes = new TreeSet<String>();
		extractIssueMessages(io.sarl.lang.validation.Messages.class, codes);
		extractIssueMessages(io.sarl.lang.validation.subvalidators.Messages.class, codes, MESSAGES_TO_IGNORE);
		return codes;
	}

	private static void extractIssueMessages(Class<?> definitions, Set<String> messages, String... ignorableLabels) {
		final var ignore = new TreeSet<>(Arrays.asList(ignorableLabels));
		final var regex = Pattern.compile("^[a-zA-Z0-9_]+_[0-9]+$"); //$NON-NLS-1$
		for (final var field : definitions.getDeclaredFields()) {
			if (String.class.equals(field.getType()) && Modifier.isStatic(field.getModifiers())
					&& Modifier.isPublic(field.getModifiers())
					&& (ignore.isEmpty() || !ignore.contains(field.getName()))) {
				final var matcher = regex.matcher(field.getName());
				if (matcher.find()) {
					try {
						final var value = (String) field.get(null);
						if (!Strings.isEmpty(value)) {
							messages.add(value);
						}
					} catch (Exception e) {
						throw new RuntimeException(e);
					}
				}
			}
		}
	}
			
	/** Validate all the issue messages to be defined in the SARL compiler code.
	 *
	 * @param descriptions the list of issue descriptions to validate.
	 * @return {@code true} if all the messages are defined.
	 * @since 0.14
	 */
	@Pure
	public static boolean validateSarlIssueMessages(List<IssueDescription> descriptions) {
		final var definedMessages = buildSarlIssueMessageSourceList();
		final var logger = DocumentationLogger.getLogger();
		logger.info(MessageFormat.format(Messages.IssueDatabaseExtensions_30,
				Integer.valueOf(definedMessages.size())));

		final var patternSelection0 = Pattern.compile("\\*+[^*]+\\*+"); //$NON-NLS-1$
		final var documentedMessages = descriptions.stream()
				.filter(it -> it.javaMessageDefinitionCheck)
				.map(it -> canonicalMessage(it.message, patternSelection0))
				.collect(Collectors.toSet());

		final var count = Integer.valueOf(definedMessages.size());
		var i = 1;
		for (final var dm : definedMessages) {
			final var definedMessage = canonicalMessage(dm);
			if (!documentedMessages.remove(definedMessage)) {
				throw new RuntimeException(MessageFormat.format(Messages.IssueDatabaseExtensions_31, dm, definedMessage, Integer.valueOf(i), count));
			}
			++i;
		}

		if (!documentedMessages.isEmpty()) {
			if (documentedMessages.size() > 1) {
				final var buffer = new StringBuilder();
				for (final var message : documentedMessages) {
					if (!Strings.isEmpty(message)) {
						buffer.append(message).append("\n"); //$NON-NLS-1$
					}
				}
				DocumentationLogger.getLogger().warning(MessageFormat.format(Messages.IssueDatabaseExtensions_33, buffer.toString()));
			} else {
				DocumentationLogger.getLogger().warning(MessageFormat.format(Messages.IssueDatabaseExtensions_32, documentedMessages.iterator().next()));
			}
		}
	
		return true;
	}

	private static String canonicalMessage(String message, Pattern pattern) {
		if (!Strings.isEmpty(message)) {
			final var matcher = pattern.matcher(message);
			return matcher.replaceAll(HIDDEN_STRING);
		}
		return message;
	}

	private static String canonicalMessage(String message) {
		if (!Strings.isEmpty(message) && message.contains("{")) { //$NON-NLS-1$
			return MessageFormat.format(message, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING,
					HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING,
					HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING,
					HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING, HIDDEN_STRING,
					HIDDEN_STRING, HIDDEN_STRING).replaceAll(Pattern.quote(HIDDEN_STRING)
							+ "(" + Pattern.quote(HIDDEN_STRING) + ")+", HIDDEN_STRING); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return message;
	}

	/** Description of an issue.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version docs.validator 0.15.0 20250909-115750
	 * @mavengroupid io.sarl.docs
	 * @mavenartifactid docs.validator
	 * @since 0.12
	 */
	public static final class IssueDescription implements JsonableObject {

		/** Internal code of the issue.
		 */
		private final String rawCode;
	
		/** Internal display code of the issue.
		 */
		private final String shortDisplayCode;

		/** Error message associated to the issue.
		 */
		public String message;

		/** Explanation of the issue's cause.
		 */
		public String cause;

		/** Guideline for solving the issue.
		 */
		public String solution;

		/** Level of importance for the issue.
		 */
		public IssueLevel level;

		/** Code of the issue to which the importance level determination is delegated to.
		 */
		public String delegate;

		/** Default level of importance for the issue when the {@link #level} is {@link IssueLevel#CONFIGURABLE}.
		 */
		public IssueLevel defaultLevel;

		/** Indicates if the definition of he message in the Java code should be checked.
		 * Usually, only the SARL's messages are checking. Those from Xtext, Xbase and Xtend are ignored
		 * because they cannot be retrieved programmatically.
		 * By default, the check is enabled.
		 *
		 * @since 0.14
		 */
		public boolean javaMessageDefinitionCheck = true; 

		/** Constructor.
		 *
		 * @param code the code; with possible {@code &amp;dot;} inside.
		 */
		IssueDescription(final String code) {
			final var lastIndex = code.lastIndexOf('.');
			this.rawCode = code.replaceAll(Pattern.quote("&dot;"), "."); //$NON-NLS-1$ //$NON-NLS-2$
			if (lastIndex >= 0 && lastIndex < code.length() - 1) {
				this.shortDisplayCode = code.substring(lastIndex + 1).replaceAll(Pattern.quote("&dot;"), "."); //$NON-NLS-1$ //$NON-NLS-2$
			} else {
				this.shortDisplayCode = code;
			}
		}
		
		
		@Override
		public String toString() {
			final var buf = new JsonBuffer();
			toJson(buf);
			return buf.toString();
		}

		@Override
		public void toJson(JsonBuffer buf) {
			buf.add("rawCode", this.rawCode); //$NON-NLS-1$
			buf.add("displayCode", this.shortDisplayCode); //$NON-NLS-1$
			buf.add("message", this.message); //$NON-NLS-1$
			buf.add("cause", this.cause); //$NON-NLS-1$
			buf.add("solving", this.solution); //$NON-NLS-1$
			final var substring = this.level.toJson(this.delegate,
					this.defaultLevel == null ? null : this.defaultLevel.toJson(null, null));
			buf.add("level", substring); //$NON-NLS-1$
		}

		/** Replies the raw code.
		 *
		 * @return the raw code.
		 */
		public String getCode() {
			return this.rawCode;
		}
		
		/** Replies the prefix of the code.
		 *
		 * @return the prefix.
		 */
		public String getCodePrefix() {
			final var lastIndex = this.rawCode.lastIndexOf('.');
			if (lastIndex > 0) {
				return this.rawCode.substring(0, lastIndex);
			}
			return ""; //$NON-NLS-1$
		}

		/** Replies the base of the code to be displayed.
		 *
		 * @return the display code.
		 */
		public String getShortDisplayCode() {
			return this.shortDisplayCode;
		}

		/** Replies the full code to be displayed.
		 *
		 * @return the display code.
		 */
		public String getLongDisplayCode() {
			return this.rawCode;
		}

		private IssueLevel getSortLevel() {
			if (this.level == IssueLevel.CONFIGURABLE) {
				return this.defaultLevel;
			}
			return this.level;
		}

	}

	/** Level of an issue.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version docs.validator 0.15.0 20250909-115750
	 * @mavengroupid io.sarl.docs
	 * @mavenartifactid docs.validator
	 * @since 0.12
	 */
	public enum IssueLevel {
		/** It is an error.
		 */
		ERROR {
			@Override
			public String toJson(String delegate, String defaultLevel) {
				return "error"; //$NON-NLS-1$
			}
			@Override
			public String getLabel(String delegate, IssueLevel defaultLevel) {
				return Messages.IssueDatabaseExtensions_19;
			}
			@Override
			public String getDelegate(String json) {
				return null;
			}
			@Override
			public IssueLevel getDefaultLevel(String json) {
				return null;
			}
		},
		/** It is a warning.
		 */
		WARNING {
			@Override
			public String toJson(String delegate, String defaultLevel) {
				return "warning"; //$NON-NLS-1$
			}
			@Override
			public String getLabel(String delegate, IssueLevel defaultLevel) {
				return Messages.IssueDatabaseExtensions_20;
			}
			@Override
			public String getDelegate(String json) {
				return null;
			}
			@Override
			public IssueLevel getDefaultLevel(String json) {
				return null;
			}
		},
		/** It is an information message.
		 */
		INFO {
			@Override
			public String toJson(String delegate, String defaultLevel) {
				return "info"; //$NON-NLS-1$
			}
			@Override
			public String getLabel(String delegate, IssueLevel defaultLevel) {
				return Messages.IssueDatabaseExtensions_21;
			}
			@Override
			public String getDelegate(String json) {
				return null;
			}
			@Override
			public IssueLevel getDefaultLevel(String json) {
				return null;
			}
		},
		/** Issue is ignored.
		 */
		IGNORE {
			@Override
			public String toJson(String delegate, String defaultLevel) {
				return "ignore"; //$NON-NLS-1$
			}
			@Override
			public String getLabel(String delegate, IssueLevel defaultLevel) {
				return Messages.IssueDatabaseExtensions_22;
			}
			@Override
			public String getDelegate(String json) {
				return null;
			}
			@Override
			public IssueLevel getDefaultLevel(String json) {
				return null;
			}
		},
		/** Level is configurable by the SARL user into the compiler configuration.
		 */
		CONFIGURABLE {
			@Override
			public String toJson(String delegate, String defaultLevel) {
				return "c/" + defaultLevel.toLowerCase(); //$NON-NLS-1$
			}
			@Override
			public String getLabel(String delegate, IssueLevel defaultLevel) {
				if (defaultLevel != null) {
					return MessageFormat.format(Messages.IssueDatabaseExtensions_23, defaultLevel.getLabel(null, null));
				}
				return Messages.IssueDatabaseExtensions_24;
			}
			@Override
			public String getDelegate(String json) {
				return null;
			}
			@Override
			public IssueLevel getDefaultLevel(String json) {
				try {
					if (json.startsWith("c/")) { //$NON-NLS-1$
						return fromJson(json.substring(2));
					}
				} catch (Throwable e) {
					//
				}
				return null;
			}
		},
		/** Level is delegated to another issue (usually inside the Eclipse Java Compiler) in order
		 * to determine the issue level. A default level may be specified in the delegate cannot provide
		 * one.
		 */
		DELEGATE {
			@Override
			public String toJson(String delegate, String defaultLevel) {
				assert delegate != null;
				return "d/" + delegate + "/" + defaultLevel.toLowerCase(); //$NON-NLS-1$ //$NON-NLS-2$
			}
			@Override
			public String getLabel(String delegate, IssueLevel defaultLevel) {
				if (defaultLevel != null) {
					return MessageFormat.format(Messages.IssueDatabaseExtensions_25, defaultLevel.getLabel(null, null));
				}
				return Messages.IssueDatabaseExtensions_26;
			}
			@Override
			public String getDelegate(String json) {
				try {
					if (json.startsWith("d/")) { //$NON-NLS-1$
						final var idx = json.indexOf('/', 2);
						if (idx > 2) {
							return json.substring(2, idx);
						}
					}
				} catch (Throwable e) {
					//
				}
				return null;
			}
			@Override
			public IssueLevel getDefaultLevel(String json) {
				try {
					if (json.startsWith("d/")) { //$NON-NLS-1$
						final var idx = json.indexOf('/', 2);
						if (idx >= 2 && idx < json.length() - 1) {
							return fromJson(json.substring(idx + 1));
						}
					}
				} catch (Throwable e) {
					//
				}
				return null;
			}
		};

		/** Replies the id that is usefull for Json storage.
		 *
		 * @param delegate the issue code to which the level is delegate to. It may be {@code null}.
		 * @param defaultLevel the default level in case of configurable level. It may be {@code null}.
		 * @return the Json id.
		 */
		public abstract String toJson(String delegate, String defaultLevel);

		/** Replies enumeration from a Json string.
		 *
		 * @param label the Json label.
		 * @return the enumeration.
		 */
		public static IssueLevel fromJson(String label) {
			try {
				if (label.startsWith("c/")) { //$NON-NLS-1$
					return CONFIGURABLE;
				}
				if (label.startsWith("d/")) { //$NON-NLS-1$
					return DELEGATE;
				}
				return valueOf(label.toUpperCase());
			} catch (Throwable e) {
				//
			}
			return null;
		}

		/** Parse from a Json string the delegate issue code for a configurable level.
		 *
		 * @param label the Json label.
		 * @return the delegat eissue coe.
		 */
		public abstract String getDelegate(String label);

		/** Parse from a Json string the default level for a configurable level.
		 *
		 * @param label the Json label.
		 * @return the default level.
		 */
		public abstract IssueLevel getDefaultLevel(String label);

		/** Replies the human-readable label.
		 *
		 * @param delegate the issue code to which the level is delegate to. It may be {@code null}.
		 * @param defaultLevel the default level in case of configurable level.
		 * @return the label.
		 */
		public abstract String getLabel(String delegate, IssueLevel defaultLevel);

	}

}