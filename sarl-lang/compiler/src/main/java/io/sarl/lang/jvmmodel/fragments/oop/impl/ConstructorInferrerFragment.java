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

package io.sarl.lang.jvmmodel.fragments.oop.impl;

import org.eclipse.xtend.core.xtend.XtendConstructor;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.lib.IterableExtensions;

import io.sarl.lang.core.annotation.DefaultValueUse;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerExecutableFragment;
import io.sarl.lang.jvmmodel.fragments.oop.IConstructorInferrerFragment;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the constructors to the JVM model.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version compiler 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
 * @since 0.15
 */
public class ConstructorInferrerFragment extends AbstractJvmModelInferrerExecutableFragment implements IConstructorInferrerFragment {

	@Override
	public void transformStatic(XtendConstructor source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer) {
		final var staticConstructor = this.jvmTypesFactory.createJvmOperation();
		container.getMembers().add(staticConstructor);
		this.associator.associatePrimary(source, staticConstructor);
		staticConstructor.setSimpleName(Utils.getStaticConstructorName());
		staticConstructor.setVisibility(JvmVisibility.PRIVATE);
		staticConstructor.setStatic(true);
		staticConstructor.setReturnType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE));
		setBody(baseInferrer, staticConstructor, source.getExpression());
		copyAndCleanDocumentationTo(source, staticConstructor);
	}

	@Override
	public void transform(XtendConstructor source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer) {
		final var constructorName = container.getSimpleName();

		final var context = baseInferrer.getContext(container);

		assert context != null;

		final var isVarArgs = Utils.isVarArg(source.getParameters());

		// Generate the unique identifier of the constructor.
		final var actionKey = this.sarlSignatureProvider.createConstructorQualifiedName(container);

		// Generate all the constructor signatures related to the constructor to create.
		final var constructorSignatures = this.sarlSignatureProvider.createPrototypeFromSarlModel(
				context.getActionPrototypeContext(this.sarlSignatureProvider),
				actionKey,
				Utils.isVarArg(source.getParameters()), source.getParameters());

		// Generate the main Java constructor.
		final var constructor = this.jvmTypesFactory.createJvmConstructor();
		container.getMembers().add(constructor);
		this.associator.associatePrimary(source, constructor);
		constructor.setSimpleName(constructorName);
		setVisibility(constructor, source);
		constructor.setVarArgs(isVarArgs);

		// Generate the parameters
		final var paramList = constructorSignatures.getOriginalParameterTypes();
		translateSarlFormalParameters(
				context,
				constructor, container, isVarArgs,
				source.getParameters(), false, paramList, false,
				baseInferrer);

		// Generate additional information (type parameters, exceptions...)
		baseInferrer.copyAndFixTypeParameters(source.getTypeParameters(), constructor);
		for (final var exception : source.getExceptions()) {
			constructor.getExceptions().add(this.jvmTypeBuilder.cloneWithProxies(exception));
		}
		baseInferrer.translateAnnotationsTo(source.getAnnotations(), constructor);

		// Set the body.
		setBody(baseInferrer, constructor, source.getExpression());

		// The signature definition of the constructor.
		final var sigKey = this.sarlSignatureProvider.createParameterTypesFromJvmModel(
				isVarArgs, constructor.getParameters());

		// Update the list of generated constructors
		context.getGeneratedConstructors().put(sigKey, constructor);

		copyAndCleanDocumentationTo(source, constructor);

		final Runnable differedGeneration  = () -> {
			// Generate the Java functions that correspond to the action with the parameter default values applied.
			for (final var entry : constructorSignatures.getInferredParameterTypes().entrySet()) {
				if (!context.getGeneratedConstructors().containsKey(entry.getKey())) {
					final var otherSignature = entry.getValue();
					// Generate the additional constructor that is invoke the main constructor previously generated.
					translateSarlFormalParametersForLocalHiddenDefaultValues(context, null);
					final var constructor2 = this.jvmTypesFactory.createJvmConstructor();
					container.getMembers().add(constructor2);
					copyAndCleanDocumentationTo(source, constructor2);
					constructor2.setSimpleName(container.getSimpleName());
					constructor2.setVisibility(constructor.getVisibility());
					constructor2.setVarArgs(isVarArgs);

					final var args = translateSarlFormalParametersForSyntheticOperation(
							constructor2, container, isVarArgs, otherSignature, baseInferrer);

					addAnnotationSafe(baseInferrer,
							constructor2, DefaultValueUse.class,
							constructorSignatures.getFormalParameterTypes().toString());
					appendGeneratedAnnotation(baseInferrer, constructor2, context);

					setBody(constructor2, toStringConcatenation(
							"this(" //$NON-NLS-1$
							+ IterableExtensions.join(args, ", ") //$NON-NLS-1$
							+ ");")); //$NON-NLS-1$

					// Update the list of the generated constructors.
					context.getGeneratedConstructors().put(entry.getKey(), constructor2);
				}
			}
		};

		context.getPreFinalizationElements().add(differedGeneration);
		context.setActionIndex(context.getActionIndex() + 1);
		context.incrementSerial(sigKey.hashCode());
		context.setInjectable(constructor);
	}

}
