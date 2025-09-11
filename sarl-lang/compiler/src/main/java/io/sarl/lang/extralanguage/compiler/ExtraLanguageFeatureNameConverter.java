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

package io.sarl.lang.extralanguage.compiler;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.google.inject.Inject;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Pair;

import io.sarl.lang.jvmmodel.SarlJvmModelAssociations;
import io.sarl.lang.sarl.SarlAction;
import io.sarl.lang.services.SARLGrammarKeywordAccess;

/** Converter from Jvm feature name to the extra language feature name.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
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
		final var map = new TreeMap<Character, List<Pair<FeaturePattern, FeatureReplacement>>>();
		if (!this.ruleReader.initializeConversions(map, this.context) && this.initializer != null) {
			this.initializer.initializeConversions((simpleName, source, target) -> {
				final var c = getKey(simpleName);
				if (c == FeaturePattern.ALL_PATTERN_CHAR) {
					for (var i = 'a'; i <= 'z'; ++i) {
						createMappingEntry(map, i, source, target);
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
		var internalStruct = map.get(Character.valueOf(character));
		if (internalStruct == null) {
			internalStruct = new ArrayList<>();
			map.put(Character.valueOf(character), internalStruct);
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
		final var receiver = new ArrayList<>();
		AbstractExpressionGenerator.buildCallReceiver(
				featureCall,
				this.keywords.getThisKeywordLambda(),
				this.referenceNameLambda,
				receiver);
		final var simpleName = AbstractExpressionGenerator.getCallSimpleName(
				featureCall,
				this.logicalContainerProvider,
				this.simpleNameProvider,
				this.keywords.getNullKeywordLambda(),
				this.keywords.getThisKeywordLambda(),
				this.keywords.getSuperKeywordLambda(),
				this.referenceNameLambda2);
		final var struct = this.conversions.get(Character.valueOf(getKey(simpleName)));
		if (struct != null) {
			final var replacementId = featureCall.getFeature().getIdentifier();
			final var replacement = matchFirstPattern(struct, replacementId, simpleName, receiver);
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
		final var struct = this.conversions.get(Character.valueOf(getKey(simpleName)));
		if (struct != null) {
			final var replacementId = calledFeature.getIdentifier();
			final var replacement = matchFirstPattern(struct, replacementId, simpleName, receiver);
			if (replacement != null) {
				if (replacement.hasReplacement()) {
					return replacement.replace(calledFeature, leftOperand, receiver, arguments);
				}
				return null;
			}
		}
		return new ConversionResult(simpleName, null);
	}

	private static void loopReceiver(LinkedList<String> sourceFeature, Object obj) {
		if (obj instanceof XMemberFeatureCall memberFeatureCall) {
			sourceFeature.addFirst(memberFeatureCall.getFeature().getSimpleName());
			loopReceiver(sourceFeature, memberFeatureCall.getMemberCallTarget());
		} else if (obj instanceof XFeatureCall featureCall) {
			sourceFeature.addFirst(featureCall.getFeature().getIdentifier());
		}
	}

	private FeatureReplacement matchFirstPattern(List<Pair<FeaturePattern, FeatureReplacement>> patterns,
			String source, String simpleName, List<Object> receiver) {
		final var sourceFeature = new LinkedList<String>();
		for (final var obj : receiver) {
			loopReceiver(sourceFeature, obj);
		}
		sourceFeature.add(source);
		final var isSarlKeyword = this.sarlKeywords.getThisKeyword().equals(simpleName) || this.sarlKeywords.getSuperKeyword().equals(simpleName);
		final Deque<String> sarlKeyword;
		if (isSarlKeyword) {
			sarlKeyword = new LinkedList<>();
			sarlKeyword.add(simpleName);
		} else {
			sarlKeyword = null;
		}
		for (final var patternMatching : patterns) {
			final var pattern = patternMatching.getKey();
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
		final var sourceFeature = new LinkedList<String>();
		sourceFeature.add(source);
		for (final var patternMatching : patterns) {
			final var pattern = patternMatching.getKey();
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
		final var operation = this.associations.getDirectlyInferredOperation(feature);
		if (operation != null) {
			if (this.conversions == null) {
				this.conversions = initMapping();
			}
			final var struct = this.conversions.get(Character.valueOf(getKey(simpleName)));
			if (struct == null) {
				return simpleName;
			}
			final var replacement = matchFirstPattern(struct, operation.getIdentifier());
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

}
