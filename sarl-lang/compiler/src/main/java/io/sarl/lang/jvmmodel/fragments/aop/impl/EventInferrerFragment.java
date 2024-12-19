/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.lang.jvmmodel.fragments.aop.impl;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

import com.google.common.base.Strings;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.GenerationContext;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerTypeFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IEventInferrerFragment;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the events to the JVM model.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
public class EventInferrerFragment extends AbstractJvmModelInferrerTypeFragment implements IEventInferrerFragment {

	@Override
	public void transform(SarlEvent source, JvmGenericType inferredJvmType, IBaseJvmModelInferrer baseInferrer) {
		// Issue #356: do not generate if the event has no name.
		assert source != null;
		assert inferredJvmType != null;
		if (Strings.isNullOrEmpty(source.getName())) {
			return;
		}
		// Create the generation context that is used by the other transformation functions.
		final var context = baseInferrer.openContext(source, inferredJvmType, Arrays.asList(
				SarlField.class, SarlConstructor.class));
		try {
			// Initialize the context with inheriting features
			Utils.populateInheritanceContext(
					inferredJvmType,
					source.getExtends(),
					null,
					context.getInheritedFinalOperations(),
					context.getInheritedOverridableOperations(),
					null,
					context.getInheritedOperationsToImplement(),
					null,
					this.sarlSignatureProvider);

			// Copy the documentation
			this.jvmTypeBuilder.copyDocumentationTo(source, inferredJvmType);

			// Change the modifiers on the generated type.
			setVisibility(inferredJvmType, source);
			inferredJvmType.setStatic(false);
			final var isAbstract = source.isAbstract() || Utils.hasAbstractMember(source);
			inferredJvmType.setAbstract(isAbstract);
			inferredJvmType.setStrictFloatingPoint(false);
			inferredJvmType.setFinal(!isAbstract && source.isFinal());

			// Generate the annotations.
			baseInferrer.translateAnnotationsTo(source.getAnnotations(), inferredJvmType);

			// Generate the extended types.
			appendConstrainedExtends(baseInferrer, context, inferredJvmType, Event.class, SarlEvent.class, source.getExtends());

			// Fixing the bounds of the type parameters
			baseInferrer.fixTypeParameters(inferredJvmType);
			
			// Issue #363: do not generate the event if the SARL library is incompatible.
			if (Utils.isCompatibleSARLLibraryOnClasspath(this.jvmTypeReferences, source)) {
				// Generate the members of the generated type.
				appendAOPMembers(baseInferrer, inferredJvmType, source, context);
			}
		
			// Issue #902: Hidden function "matchesTypeBounds"
			if (!source.getTypeParameters().isEmpty()) {
				final var typeBoundMatchOperation = this.jvmTypesFactory.createJvmOperation();
				typeBoundMatchOperation.setAbstract(false);
				typeBoundMatchOperation.setNative(false);
				typeBoundMatchOperation.setSynchronized(false);
				typeBoundMatchOperation.setStrictFloatingPoint(false);
				typeBoundMatchOperation.setFinal(false);
				typeBoundMatchOperation.setVisibility(JvmVisibility.PUBLIC);
				typeBoundMatchOperation.setStatic(true);
				typeBoundMatchOperation.setSimpleName(SarlUtils.HIDDEN_MEMBER_CHARACTER + "matchesTypeBounds"); //$NON-NLS-1$
				typeBoundMatchOperation.setReturnType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(Boolean.TYPE));
				// Add to container
				inferredJvmType.getMembers().add(typeBoundMatchOperation);
				this.associator.associate(source, typeBoundMatchOperation);
				// First parameter: it
				var jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
				jvmParam.setName(this.grammarKeywordAccess.getItKeyword());
				jvmParam.setParameterType(this.jvmTypeReferences.createTypeRef(inferredJvmType));
				this.associator.associate(source, jvmParam);
				typeBoundMatchOperation.getParameters().add(jvmParam);
				// Rest of parameters: bounds
				jvmParam = this.jvmTypesFactory.createJvmFormalParameter();
				jvmParam.setName("bounds"); //$NON-NLS-1$
				jvmParam.setParameterType(this.jvmTypeReferences.createArrayType(
						this.jvmTypeReferences.getTypeForName(Class.class, source)));
				this.associator.associate(source, jvmParam);
				typeBoundMatchOperation.getParameters().add(jvmParam);
				typeBoundMatchOperation.setVarArgs(true);
				// Body
				final var declaredFields = source.getMembers().stream().filter(it0 -> it0 instanceof XtendField).map(it0 -> (XtendField) it0).toList();
				setBody(typeBoundMatchOperation, it -> {
					it.append("if (bounds != null && bounds.length == "); //$NON-NLS-1$
					it.append(Integer.toString(source.getTypeParameters().size()));
					it.append(") {").increaseIndentation(); //$NON-NLS-1$
					var i = 0;
					for (final var parameter : source.getTypeParameters()) {
						final var matchableFields = declaredFields.stream().filter(it0 -> {
							return it0.getType().getIdentifier().equals(parameter.getIdentifier());
						}).toList();
						if (!matchableFields.isEmpty()) {
							it.newLine().append("if ("); //$NON-NLS-1$
							var first = true;
							for (final var matchField : matchableFields) {
								if (first) {
									first = false;
								} else {
									it.append(" || "); //$NON-NLS-1$
								}
								it.append("(it.").append(matchField.getName()).append(" != null && !bounds["); //$NON-NLS-1$ //$NON-NLS-2$
								it.append(Integer.toString(i)).append("].isInstance(it."); //$NON-NLS-1$
								it.append(matchField.getName()).append("))"); //$NON-NLS-1$
							}
							it.append(") {").increaseIndentation().newLine(); //$NON-NLS-1$
							it.append("return false;").decreaseIndentation().newLine(); //$NON-NLS-1$
							it.append("}"); //$NON-NLS-1$
						}
						++i;
					}
					it.newLine().append("return true;"); //$NON-NLS-1$
					it.decreaseIndentation().newLine().append("}").newLine(); //$NON-NLS-1$
					it.append("return false;"); //$NON-NLS-1$
				});
				// Annotations
				appendGeneratedAnnotation(baseInferrer, typeBoundMatchOperation, context);
				addAnnotationSafe(baseInferrer, typeBoundMatchOperation, Pure.class);
			}

			// Change the injectable flag
			context.setInjectable(inferredJvmType.getExtendedClass());

			// Add the default constructors for the behavior, if not already added
			appendDefaultConstructors(baseInferrer, source, inferredJvmType);

			// Add functions dedicated to comparisons (equals, hashCode, etc.)
			appendComparisonFunctions(baseInferrer, context, source, inferredJvmType);

			// Add functions dedicated to String representation(toString, etc.)
			appendToStringFunctions(baseInferrer, context, source, inferredJvmType);

			// Add clone functions if the generated type is cloneable
			appendCloneFunctionIfCloneable(baseInferrer, context, source, inferredJvmType);

			// Add serialVersionUID field if the generated type is serializable
			appendSerialNumberIfSerializable(baseInferrer, context, source, inferredJvmType);

			// Add the specification version of SARL
			appendSARLSpecificationVersion(baseInferrer, context, source, inferredJvmType);

			// Add the type of SARL Element
			appendSARLElementType(baseInferrer, source, inferredJvmType);

			// Add the type of SARL Element
			appendInjectableAnnotationIfInjectable(baseInferrer, inferredJvmType, context);

			// Add the Xbase annotation that avoid JaCoCo to analyze the generate code
			appendXbaseGeneratedAnnotation(baseInferrer, inferredJvmType);

			// Resolving any name conflict with the generated JVM type
			this.nameClashResolver.resolveNameClashes(inferredJvmType);
		} finally {
			baseInferrer.closeContext(context);
		}
	}

	private static boolean isAppendToStringFunctionsEnable(GenerationContext context) {
		return context.getGeneratorConfig2().isGenerateToStringFunctions();
	}

	/** Create the functions that are related to the {@code toString} function.
	 *
	 * @param baseInferrer the inferrer that is the considered as the base (starting point) of inferring process.
	 * @param context the current generation context.
	 * @param source the source object.
	 * @param target the inferred JVM object.
	 */
	private void appendToStringFunctions(IBaseJvmModelInferrer baseInferrer, GenerationContext context,
			XtendTypeDeclaration source, final JvmGenericType target) {
		if (!isAppendToStringFunctionsEnable(context)) {
			return;
		}
		// Create a list of the declared non-static fields.
		final var declaredInstanceFields = new ArrayList<JvmField>();
		for (final var field : target.getDeclaredFields()) {
			if (!field.isStatic()) {
				declaredInstanceFields.add(field);
			}
		}

		if (!declaredInstanceFields.isEmpty()) {
			final var voidType = baseInferrer.getJvmTypeReferenceBuilder().typeRef(Void.TYPE);
			final var op = this.jvmTypeBuilder.toMethod(
					source,
					"toString", //$NON-NLS-1$
					voidType, it2 -> {
						it2.setVisibility(JvmVisibility.PROTECTED);
						this.jvmTypeBuilder.setDocumentation(it2,
								MessageFormat.format(Messages.SARLJvmModelInferrer_2,
										target.getSimpleName()));
						final var param = this.jvmTypesFactory.createJvmFormalParameter();
						param.setName("builder"); //$NON-NLS-1$
						param.setParameterType(baseInferrer.getJvmTypeReferenceBuilder().typeRef(ToStringBuilder.class));
						it2.getParameters().add(param);
						setBody(it2, it3 -> {
							it3.append("super.toString(builder);"); //$NON-NLS-1$
							for (final var attr : declaredInstanceFields) {
								it3.newLine();
								it3.append("builder.add(\"" + attr.getSimpleName() //$NON-NLS-1$
								+ "\", this." //$NON-NLS-1$
								+ attr.getSimpleName() + ");"); //$NON-NLS-1$
							}
						});
					});
			if (op != null) {
				appendGeneratedAnnotation(baseInferrer, op, context);
				if (context.getGeneratorConfig2().isGeneratePureAnnotation()) {
					addAnnotationSafe(baseInferrer, op, Pure.class);
				}
				target.getMembers().add(op);
			}
		}
	}

}
