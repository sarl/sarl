/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.bugfixes.refused.bug623;

import com.google.inject.Inject;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.RichString;
import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtend2.lib.StringConcatenationClient;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XClosure;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XVariableDeclaration;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import io.sarl.lang.compilation.typesystem.SARLReentrantTypeResolver;

/**
 * Fixing the SARL issue 623: Generate real lambda.
 *
 * <p>This bug fix could be remove when Xtend PR 193 is merge.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-xtend/pull/193"
 */
public class Bug623SARLReentrantTypeResolver extends SARLReentrantTypeResolver {

	/** The provider of generation configuration.
	 */
	@Inject
	private IGeneratorConfigProvider generatorConfigProvider;

	@Override
	protected String getInvalidWritableVariableAccessMessage(XVariableDeclaration variable,
			XAbstractFeatureCall featureCall, IResolvedTypes resolvedTypes) {
		final EObject containingStructure = getNearestClosureOrTypeDeclaration(featureCall, resolvedTypes);
		if (containingStructure instanceof XClosure && !EcoreUtil.isAncestor(containingStructure, variable)) {
			final GeneratorConfig generatorConfig = this.generatorConfigProvider.get(
					EcoreUtil.getRootContainer(containingStructure));
			if (generatorConfig != null && generatorConfig.getJavaSourceVersion().isAtLeast(JavaVersion.JAVA8)) {
				return null;
			}
		}
		return super.getInvalidWritableVariableAccessMessage(variable, featureCall, resolvedTypes);
	}

	@SuppressWarnings("all")
	private EObject getNearestClosureOrTypeDeclaration(EObject object, IResolvedTypes resolvedTypes) {
		EObject candidate = object;
		while(candidate != null) {
			if (candidate instanceof XClosure) {
				return candidate;
			}
			if (candidate instanceof XConstructorCall) {
				// skip anonymous class constructors themselves
				if (candidate.eContainingFeature() == XtendPackage.Literals.ANONYMOUS_CLASS__CONSTRUCTOR_CALL) {
					candidate = candidate.eContainer();
				}
			} else if (candidate instanceof XtendTypeDeclaration) {
				return candidate;
			}
			if (candidate instanceof RichString) {
				LightweightTypeReference type = resolvedTypes.getActualType((RichString)candidate);
				if (type != null && type.isType(StringConcatenationClient.class)) {
					return candidate;
				}
			}
			candidate = candidate.eContainer();
		}
		return null;
	}

}
