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

package io.sarl.lang.extralanguage.compiler;

import java.util.ArrayList;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.inject.Inject;

import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Converter from Jvm feature name to the extra language feature name.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ExtraLanguageFeatureNameConverter {

	private final IExtraLanguageConversionInitializer initializer;

	private final IExtraLanguageGeneratorContext context;

	@Inject
	private FeatureNameConverterRuleReader ruleReader;

	@Inject
	private SarlJvmModelAssociations associations;

	@Inject
	private IdentifiableSimpleNameProvider simpleNameProvider;

	@Inject
	private ILogicalContainerProvider logicalContainerProvider;

	@Inject
	private SARLGrammarKeywordAccess sarlKeywords;

	private final IExtraLanguageKeywordProvider keywords;

	private Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> conversions;

	private final Function1<? super XExpression, ? extends String> referenceNameLambda;

	private final Function1<? super JvmIdentifiableElement, ? extends String> referenceNameLambda2;

	/** Constructor.
	 *
	 * @param initializer the initializer.
	 * @param context the generation context.
	 * @param keywords the provider of extra-language keywords.
	 */
	public ExtraLanguageFeatureNameConverter(IExtraLanguageConversionInitializer initializer,
			IExtraLanguageGeneratorContext context, IExtraLanguageKeywordProvider keywords) {
		this.initializer = initializer;
		this.context = context;
		this.referenceNameLambda = expr -> null;
		this.referenceNameLambda2 = expr -> null;
		this.keywords = keywords;
	}

	/** Build the mapping table.
	 *
	 * @return the mapping table.
	 */
	protected Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> initMapping() {
		final Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> map = new TreeMap<>();
		if (!this.ruleReader.initializeConversions(map, this.context) && this.initializer != null) {
			this.initializer.initializeConversions((simpleName, source, target) -> {
				final char c = getKey(simpleName);
				if (c == FeaturePattern.ALL_PATTERN_CHAR) {
					for (int i = 'a'; i <= 'z'; ++i) {
						createMappingEntry(map, (char) i, source, target);
					}
				} else {
					createMappingEntry(map, c, source, target);
				}
			});
		}
		return map;
	}

	private static void createMappingEntry(Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> map, char character,
			String source, String target) {
		List<Pair<FeaturePattern, FeatureReplacement>> internalStruct = map.get(character);
		if (internalStruct == null) {
			internalStruct = new ArrayList<>();
			map.put(character, internalStruct);
		}
		internalStruct.add(new Pair<>(new FeaturePattern(source), new FeatureReplacement(target)));
	}

	/** Compute the mapping key for the given simple name.
	 *
	 * @param name the simple name.
	 * @return the mapping ket for the simple name.
	 */
	public static char getKey(String name) {
		return Character.toLowerCase(name.charAt(0));
	}

	/** Replies the type of conversion for the given feature call.
	 *
	 * @param featureCall the feature call.
	 * @return the type of conversion.
	 */
	public ConversionType getConversionTypeFor(XAbstractFeatureCall featureCall) {
		if (this.conversions == null) {
			this.conversions = initMapping();
		}
		final List<Object> receiver = new ArrayList<>();
		AbstractExpressionGenerator.buildCallReceiver(
				featureCall,
				this.keywords.getThisKeywordLambda(),
				this.referenceNameLambda,
				receiver);
		final String simpleName = AbstractExpressionGenerator.getCallSimpleName(
				featureCall,
				this.logicalContainerProvider,
				this.simpleNameProvider,
				this.keywords.getNullKeywordLambda(),
				this.keywords.getThisKeywordLambda(),
				this.keywords.getSuperKeywordLambda(),
				this.referenceNameLambda2);
		final List<Pair<FeaturePattern, FeatureReplacement>> struct = this.conversions.get(getKey(simpleName));
		if (struct != null) {
			final String replacementId = featureCall.getFeature().getIdentifier();
			final FeatureReplacement replacement = matchFirstPattern(struct, replacementId, simpleName, receiver);
			if (replacement != null) {
				if (replacement.hasReplacement()) {
					return ConversionType.EXPLICIT;
				}
				return ConversionType.FORBIDDEN_CONVERSION;
			}
		}
		return ConversionType.IMPLICIT;
	}

	/** Convert a full call to a feature.
	 *
	 * <p>This function is supposed to change the two list parameters for reflecting the conversion.
	 *
	 * @param simpleName the simple name of the feature to be called.
	 * @param calledFeature the called feature.
	 * @param leftOperand the description of the elements into the left operand (usually, before assignment sign).
	 * @param receiver the description of the receiver, i.e. the object on which the feature is called.
	 * @param arguments the list of the arguments.
	 * @return a description of the conversion; or {@code null} for ignoring the call.
	 */
	public ConversionResult convertFeatureCall(String simpleName, JvmIdentifiableElement calledFeature, List<Object> leftOperand,
			List<Object> receiver, List<XExpression> arguments) {
		if (this.conversions == null) {
			this.conversions = initMapping();
		}
		final List<Pair<FeaturePattern, FeatureReplacement>> struct = this.conversions.get(getKey(simpleName));
		if (struct != null) {
			final String replacementId = calledFeature.getIdentifier();
			final FeatureReplacement replacement = matchFirstPattern(struct, replacementId, simpleName, receiver);
			if (replacement != null) {
				if (replacement.hasReplacement()) {
					return replacement.replace(calledFeature, leftOperand, receiver, arguments);
				}
				return null;
			}
		}
		return new ConversionResult(simpleName);
	}

	private static void loopReceiver(LinkedList<String> sourceFeature, Object obj) {
		if (obj instanceof XMemberFeatureCall) {
			final XMemberFeatureCall memberFeatureCall = (XMemberFeatureCall) obj;
			sourceFeature.addFirst(memberFeatureCall.getFeature().getSimpleName());
			loopReceiver(sourceFeature, memberFeatureCall.getMemberCallTarget());
		} else if (obj instanceof XFeatureCall) {
			final XFeatureCall featureCall = (XFeatureCall) obj;
			sourceFeature.addFirst(featureCall.getFeature().getIdentifier());
		}
	}

	private FeatureReplacement matchFirstPattern(List<Pair<FeaturePattern, FeatureReplacement>> patterns,
			String source, String simpleName, List<Object> receiver) {
		final LinkedList<String> sourceFeature = new LinkedList<>();
		for (final Object obj : receiver) {
			loopReceiver(sourceFeature, obj);
		}
		sourceFeature.add(source);
		final boolean isSarlKeyword = this.sarlKeywords.getThisKeyword().equals(simpleName) || this.sarlKeywords.getSuperKeyword().equals(simpleName);
		final Deque<String> sarlKeyword;
		if (isSarlKeyword) {
			sarlKeyword = new LinkedList<>();
			sarlKeyword.add(simpleName);
		} else {
			sarlKeyword = null;
		}
		for (final Pair<FeaturePattern, FeatureReplacement> patternMatching : patterns) {
			final FeaturePattern pattern = patternMatching.getKey();
			if (pattern.isNameReplacement()) {
				if (isSarlKeyword && pattern.matches(sarlKeyword)) {
					return patternMatching.getValue();
				}
			} else if (pattern.matches(sourceFeature)) {
				return patternMatching.getValue();
			}
		}
		return null;
	}

	private static FeatureReplacement matchFirstPattern(List<Pair<FeaturePattern, FeatureReplacement>> patterns, String source) {
		final LinkedList<String> sourceFeature = new LinkedList<>();
		sourceFeature.add(source);
		for (final Pair<FeaturePattern, FeatureReplacement> patternMatching : patterns) {
			final FeaturePattern pattern = patternMatching.getKey();
			if (pattern.isNameReplacement() && pattern.matches(sourceFeature)) {
				return patternMatching.getValue();
			}
		}
		return null;
	}

	/** Convert the given name for the given feature to its equivalent in the extra language.
	 *
	 * <p>Usually, this function is called to convert the function's name when it is declared.
	 *
	 * @param simpleName the feature's simple name to convert.
	 * @param feature the JVM feature that corresponds to the name.
	 * @return the conversion result, or {@code null} if the equivalent function not exists,
	 *     or {@code simpleName} if the function name should stay unchanged.
	 */
	public String convertDeclarationName(String simpleName, SarlAction feature) {
		assert simpleName != null;
		assert feature != null;
		final JvmOperation operation = this.associations.getDirectlyInferredOperation(feature);
		if (operation != null) {
			if (this.conversions == null) {
				this.conversions = initMapping();
			}
			final List<Pair<FeaturePattern, FeatureReplacement>> struct = this.conversions.get(getKey(simpleName));
			if (struct == null) {
				return simpleName;
			}
			final FeatureReplacement replacement = matchFirstPattern(struct, operation.getIdentifier());
			if (replacement != null) {
				if (replacement.hasReplacement()) {
					return replacement.getText();
				}
				return null;
			}
			return simpleName;
		}
		return simpleName;
	}

	/** Describes the result of a replacement.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class ConversionResult {

		private final String text;

		private final Object[] conversion;

		/** Constructor for feature renaming.
		 *
		 * @param text the text.
		 */
		protected ConversionResult(String text) {
			this.text = text;
			this.conversion = null;
		}

		/** Constructor for complex conversion.
		 *
		 * @param conversion is the description of the conversion.
		 */
		protected ConversionResult(Object[] conversion) {
			this.text = null;
			this.conversion = conversion;
		}

		/** Replies if the replacement result is a simple feature renaming.
		 *
		 * @return {@code true} if the feature should be renamed, {@code false} for a complex replacement.
		 */
		public boolean isFeatureRenaming() {
			return !Strings.isEmpty(this.text);
		}

		/** Replies the complex conversion.
		 *
		 * <p>The replied value is an array of {@link CharSequence}, {@link JvmType}, {@link LightweightTypeReference},
		 * or {@link XExpression}.
		 *
		 * @return the complex conversion.
		 */
		public Object[] toComplexConversion() {
			return this.conversion;
		}

		@Override
		public String toString() {
			return this.text;
		}

	}

	/** Reader of the conversion rules.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class FeatureNameConverterRuleReader {

		/** initialize the conversions mapping.
		 *
		 * @param result the result.
		 * @param context the generation context.
		 * @return {@code true} if rules are read.
		 */
		@SuppressWarnings("static-method")
		public boolean initializeConversions(Map<Character, List<Pair<FeaturePattern, FeatureReplacement>>> result,
				IExtraLanguageGeneratorContext context) {
			return false;
		}

	}

	/** Feature pattern.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class FeaturePattern {

		private static final String SEPARATOR_PATTERN = "\\/+"; //$NON-NLS-1$

		private static final char ALL_PATTERN_CHAR = '*';

		private static final String ALL_PATTERN = Character.toString(ALL_PATTERN_CHAR);

		private static final String ANY_PATTERN = ".*"; //$NON-NLS-1$

		private static final char OPARENTHESIS = '(';

		private static final char CPARENTHESIS = ')';

		private static final char SEPARATOR = '/';

		private static final char DOT = '.';

		private final boolean isNameReplacement;

		private final String rawFeature;

		private final Pattern featurePattern;

		private final Pattern[] pathPatterns;

		/** Constructor.
		 *
		 * <p>The general format of the textual representation is:
		 * <pre><code>
		 * [path/]featureIdentifier
		 * </code></pre>
		 * The <code>featureIdentifier</code> is the identifier of the feature.
		 * The <code>path</code> is a sequence of type identifiers or field names.
		 *
		 * <p>The special character <code>*</code> may be used for specifying "anything".
		 *
		 * @param specification the textual representation of the pattern.
		 */
		public FeaturePattern(String specification) {
			final String[] elements = specification.split(SEPARATOR_PATTERN);
			final String last = elements[elements.length - 1];
			if (last.contains(ALL_PATTERN)) {
				this.rawFeature = null;
				this.featurePattern = Pattern.compile(protect(last));
			} else {
				this.rawFeature = last;
				this.featurePattern = null;
			}
			this.pathPatterns = new Pattern[elements.length - 1];
			if (this.pathPatterns.length > 0) {
				for (int i = 0; i < this.pathPatterns.length; ++i) {
					this.pathPatterns[i] = Pattern.compile(protect(elements[i]));
				}
			}
			this.isNameReplacement = specification.charAt(specification.length() - 1) != CPARENTHESIS;
		}

		@Override
		public String toString() {
			if (this.rawFeature != null) {
				return this.rawFeature;
			}
			return Objects.toString(this.featurePattern);
		}

		private static String protect(String source) {
			if (Strings.equal(source, ALL_PATTERN)) {
				return ANY_PATTERN;
			}
			final StringBuilder builder = new StringBuilder();
			boolean first = true;
			for (final String element : source.split(Pattern.quote(ALL_PATTERN))) {
				if (first) {
					first = false;
				} else {
					builder.append(ANY_PATTERN);
				}
				builder.append(Pattern.quote(element));
			}
			return builder.toString();
		}

		/** Replies if this pattern is only for a simple name replacement.
		 *
		 * @return {@code true} if the pattern corresponds to a simple name only.
		 */
		public boolean isNameReplacement() {
			return this.isNameReplacement;
		}

		/** Replies if the pattern matches the given identifier.
		 *
		 * @param feature the feature to test.
		 * @return {@code true} if the pattern matches.
		 */
		public boolean matches(Deque<String> feature) {
			boolean match;
			if (this.rawFeature != null) {
				match = this.rawFeature.equals(feature.getLast());
			} else {
				final Matcher featureMatcher = this.featurePattern.matcher(feature.getLast());
				match = featureMatcher.matches();
			}
			if (match && this.pathPatterns.length > 0) {
				final Iterator<String> iterator = feature.descendingIterator();
				// Skip last
				iterator.next();
				for (int j = this.pathPatterns.length - 1;
						match && iterator.hasNext() && j >= 0;
						--j) {
					final String component = iterator.next();
					final Pattern pathPattern = this.pathPatterns[j];
					final Matcher pathMatcher = pathPattern.matcher(component);
					match = pathMatcher.matches();
				}
			}
			return match;
		}

		/** Replies the simple name of the feature from the given textual representation.
		 *
		 * @param textualRepresentation the textual representation.
		 * @return the simple name.
		 */
		public static String simpleName(String textualRepresentation) {
			int start = textualRepresentation.lastIndexOf(SEPARATOR);
			int end = textualRepresentation.length();
			if (start < 0) {
				start = 0;
			} else {
				++start;
			}
			if (textualRepresentation.charAt(textualRepresentation.length() - 1) == CPARENTHESIS) {
				end = textualRepresentation.lastIndexOf(OPARENTHESIS);
				assert end > 0;
			}
			final int dot = textualRepresentation.lastIndexOf(DOT, end - 1);
			if (dot + 1 >= start) {
				start = dot + 1;
			}
			return textualRepresentation.substring(start, end);
		}

	}

	/** Feature replacement.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class FeatureReplacement {

		private static final char PROTECT_CHAR = '\\';

		private static final char VARIABLE_CHAR = '$';

		private static final char ALL_CHAR = '*';

		private static final String ARGUMENT_SEPARATOR = ","; //$NON-NLS-1$

		private final boolean hasReplacement;

		private List<String> staticParts = new ArrayList<>();

		private List<Integer> dynamicParts = new ArrayList<>();

		private String raw;

		/** Constructor.
		 *
		 * <p>The general format of the textual representation may contains:<ul>
		 * <li><code>$0</code> for the receiver.</li>
		 * <li><code>$n</code> for the n-th argument.</li>
		 * <li><code>$*</code> all the arguments.</li>
		 * </ul>
		 *
		 * @param specification the textual representation of the pattern.
		 */
		public FeatureReplacement(String specification) {
			StringBuilder builder = new StringBuilder();
			boolean isProtected = false;
			StringBuilder isVariable = null;
			int i = 0;
			while (i < specification.length()) {
				final char character = specification.charAt(i);
				if (isProtected) {
					isProtected = false;
					builder.append(character);
					++i;
				} else if (isVariable != null) {
					if (character == ALL_CHAR && isVariable.length() == 0) {
						this.dynamicParts.add(-1);
						isVariable = null;
						++i;
					} else if (character >= '0' && character <= '9') {
						isVariable.append(character);
						++i;
					} else {
						final int varNumber = Integer.parseInt(isVariable.toString());
						this.dynamicParts.add(varNumber);
						isVariable = null;
					}
				} else {
					switch (character) {
					case PROTECT_CHAR:
						isProtected = true;
						break;
					case VARIABLE_CHAR:
						this.staticParts.add(builder.toString());
						builder = new StringBuilder();
						isVariable = new StringBuilder();
						break;
					default:
						builder.append(character);
					}
					++i;
				}
			}
			if (isVariable != null) {
				final int varNumber = Integer.parseInt(isVariable.toString());
				this.dynamicParts.add(varNumber);
			} else if (builder.length() > 0) {
				this.staticParts.add(builder.toString());
			}
			this.hasReplacement = !specification.isEmpty();
			this.raw = specification;
		}

		@Override
		public String toString() {
			return this.raw;
		}

		/** Replies if a replacement is defined.
		 *
		 * @return {@code true} if a replacement is defined.
		 */
		public boolean hasReplacement() {
			return this.hasReplacement;
		}

		/** Do the replacement.
		 *
		 * @param calledFeature the called feature.
		 * @param leftOperand the description of the elements into the left operand (usually, before assignment sign).
		 * @param receiver the description of the receiver, i.e. the object on which the feature is called.
		 * @param arguments the list of the arguments.
		 * @return the new simple name, or {@code null} if the equivalent function not exists,
		 *     or {@code simpleName} if the function name should stay unchanged.
		 */
		public ConversionResult replace(JvmIdentifiableElement calledFeature, List<Object> leftOperand,
				List<Object> receiver, List<XExpression> arguments) {
			assert this.hasReplacement;
			if (!this.dynamicParts.isEmpty()) {
				final List<Object> content = new ArrayList<>(this.staticParts.size() + this.dynamicParts.size());
				final Iterator<String> staticIterator = this.staticParts.iterator();
				final Iterator<Integer> dynamicIterator = this.dynamicParts.iterator();
				while (staticIterator.hasNext()) {
					assert staticIterator.hasNext();
					content.add(staticIterator.next());
					if (dynamicIterator.hasNext()) {
						final int varNumber = dynamicIterator.next().intValue();
						if (varNumber == -1) {
							boolean first = true;
							for (final XExpression arg : arguments) {
								if (first) {
									first = false;
								} else {
									content.add(ARGUMENT_SEPARATOR);
								}
								content.add(arg);
							}
						} else if (varNumber == 0) {
							content.add(receiver.get(receiver.size() - 1));
						} else if (varNumber > 0 && varNumber <= arguments.size()) {
							content.add(arguments.get(varNumber - 1));
						}
					}
				}
				return new ConversionResult(content.toArray());
			}
			return new ConversionResult(this.staticParts.get(this.staticParts.size() - 1));
		}

		/** Replies the raw text for this replacement.
		 *
		 * @return the replacement text.
		 */
		public String getText() {
			return this.raw;
		}

	}

	/** Type of feature conversion.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public enum ConversionType {

		/** No conversion is allowed to the target language.
		 */
		FORBIDDEN_CONVERSION,

		/** Conversion is explicitly set.
		 */
		EXPLICIT,

		/** Implicit conversion. Usually, the feature remains the same.
		 */
		IMPLICIT,

	}

}
