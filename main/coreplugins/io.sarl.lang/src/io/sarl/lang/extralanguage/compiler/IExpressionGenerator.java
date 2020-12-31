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

import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

/** Generator of XExpression.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public interface IExpressionGenerator {

	/** Generate the code for the given XExpression.
	 *
	 * <p>The given expression is not expecting to be returned by a function.
	 *
	 * @param expression the expression to be generated.
	 * @param output the output.
	 * @param context the generator context.
	 * @return the lastly encountered expression.
	 */
	default XExpression generate(XExpression expression, IAppendable output, IExtraLanguageGeneratorContext context) {
		return generate(expression, null, output, context);
	}

	/** Generate the code for the given XExpression.
	 *
	 * @param expression the expression to be generated.
	 * @param expectedType the type that is expected for the expression in the context of a function return.
	 * @param output the output.
	 * @param context the generator context.
	 * @return the lastly encountered expression.
	 */
	XExpression generate(XExpression expression, LightweightTypeReference expectedType, IAppendable output, IExtraLanguageGeneratorContext context);

	/** Replies the type converter.
	 *
	 * @param context the context of the generation.
	 * @return the converter.
	 */
	ExtraLanguageTypeConverter getTypeConverter(IExtraLanguageGeneratorContext context);

	/** Replies the feature name converter.
	 *
	 * @param context the context of the generation.
	 * @return the converter.
	 */
	ExtraLanguageFeatureNameConverter getFeatureNameConverter(IExtraLanguageGeneratorContext context);

}
