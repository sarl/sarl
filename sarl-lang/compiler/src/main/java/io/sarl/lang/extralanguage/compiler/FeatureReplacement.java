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
import java.util.List;

import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.xbase.XExpression;

/** Feature replacement.
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.6
 */
public class FeatureReplacement {

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
	 * <li>{@code $0} for the receiver.</li>
	 * <li>{@code $n} for the n-th argument.</li>
	 * <li>{@code $*} all the arguments.</li>
	 * </ul>
	 *
	 * @param specification the textual representation of the pattern.
	 */
	public FeatureReplacement(String specification) {
		var builder = new StringBuilder();
		var isProtected = false;
		StringBuilder isVariable = null;
		var i = 0;
		while (i < specification.length()) {
			final var character = specification.charAt(i);
			if (isProtected) {
				isProtected = false;
				builder.append(character);
				++i;
			} else if (isVariable != null) {
				if (character == ALL_CHAR && isVariable.length() == 0) {
					this.dynamicParts.add(Integer.valueOf(-1));
					isVariable = null;
					++i;
				} else if (character >= '0' && character <= '9') {
					isVariable.append(character);
					++i;
				} else {
					final int varNumber = Integer.parseInt(isVariable.toString());
					this.dynamicParts.add(Integer.valueOf(varNumber));
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
			final var varNumber = Integer.valueOf(isVariable.toString());
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
			final var content = new ArrayList<>(this.staticParts.size() + this.dynamicParts.size());
			final var staticIterator = this.staticParts.iterator();
			final var dynamicIterator = this.dynamicParts.iterator();
			while (staticIterator.hasNext()) {
				assert staticIterator.hasNext();
				content.add(staticIterator.next());
				if (dynamicIterator.hasNext()) {
					final var varNumber = dynamicIterator.next().intValue();
					if (varNumber == -1) {
						var first = true;
						for (final var arg : arguments) {
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
			return new ConversionResult(null, content.toArray());
		}
		return new ConversionResult(this.staticParts.get(this.staticParts.size() - 1), null);
	}

	/** Replies the raw text for this replacement.
	 *
	 * @return the replacement text.
	 */
	public String getText() {
		return this.raw;
	}

}
