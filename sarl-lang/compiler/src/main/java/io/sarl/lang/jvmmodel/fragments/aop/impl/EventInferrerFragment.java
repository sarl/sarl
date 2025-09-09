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

package io.sarl.lang.jvmmodel.fragments.aop.impl;

import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;
import java.util.stream.Stream;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtend.core.xtend.XtendField;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmField;
import org.eclipse.xtext.common.types.JvmFormalParameter;
import org.eclipse.xtext.common.types.JvmGenericType;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.JvmTypeReference;
import org.eclipse.xtext.common.types.JvmVisibility;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.compiler.output.ITreeAppendable;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;

import com.google.common.base.Strings;

import io.sarl.lang.core.Event;
import io.sarl.lang.core.annotation.SyntheticEventConstructor;
import io.sarl.lang.core.annotation.SyntheticEventConstructorMandatoryParameter;
import io.sarl.lang.core.util.SarlUtils;
import io.sarl.lang.jvmmodel.GenerationContext;
import io.sarl.lang.jvmmodel.IBaseJvmModelInferrer;
import io.sarl.lang.jvmmodel.Messages;
import io.sarl.lang.jvmmodel.fragments.AbstractJvmModelInferrerTypeFragment;
import io.sarl.lang.jvmmodel.fragments.aop.IEventInferrerFragment;
import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlEvent;
import io.sarl.lang.sarl.SarlField;
import io.sarl.lang.sarl.actionprototype.ActionParameterTypes;
import io.sarl.lang.sarl.actionprototype.FormalParameterProvider;
import io.sarl.lang.sarl.actionprototype.InferredValuedParameter;
import io.sarl.lang.util.Utils;

/** Fragment for inferred the events to the JVM model.
 *
 * @author $Author: sgalland$
 * @version compiler 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler
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

			// Generate the constructors based on the defined fields.
			// Synthetic constructors are generated only if no constructor is provided in the original
			// code of the event, and all the inherited constructors are synthetic also.
			// If these two conditions are not meet, no synthetic constructor is generated
			if (!context.hasConstructor()) {
				generateSyntheticConstructorForFields(context, source, inferredJvmType, baseInferrer);
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

	private void generateSyntheticConstructorForFields(GenerationContext context, SarlEvent source, JvmGenericType container,
			IBaseJvmModelInferrer baseInferrer) {
		final var extendedClass = container.getExtendedClass();
		if (extendedClass != null) {
			final JvmType superType = extendedClass.getType();
			if (superType instanceof JvmGenericType superGenericType) {
				SyntheticConstructorGenerator generator = null;
				if (superGenericType.getQualifiedName().equals(Event.class.getName())) {
					final var declaredFields = extractDeclaredFields(source);
					if (!declaredFields.isEmpty()) {
						generator = new FieldBasedGenerator(declaredFields);
					}
				} else {
					final var inheritedConstructors = extractInheritedConstructors(superGenericType, container);
					if (inheritedConstructors != null) {
						final var declaredFields = extractDeclaredFields(source);
						final var hasLocalField = !declaredFields.isEmpty();
						final var hasInheritedConstructor = inheritedConstructors.iterator().hasNext();
						if (hasLocalField && hasInheritedConstructor) {
							generator = new InheritedConstructorFieldBasedGenerator(inheritedConstructors, declaredFields);
						} else if (hasLocalField) {
							generator = new FieldBasedGenerator(declaredFields);
						}
					}
					// The other cases are supported by appendDefaultConstructors()
				}
				if (generator != null) {
					generator.generate(context, source, container, baseInferrer);
				}
			}
		}
	}

	/** Replies the fields declared in the given event.
	 *
	 * @param event the event to analyze.
	 * @return the list of fields in the {@code event}.
	 */
	protected static List<XtendField> extractDeclaredFields(SarlEvent event) {
		return event.getMembers().stream()
				.filter(it0 -> it0 instanceof XtendField)
				.map(it0 -> (XtendField) it0)
				.filter(it0 -> !it0.isStatic() && (!it0.isFinal() || it0.getInitialValue() == null))
				.toList();
	}

	/** Replies the inherited constructors in a given type.
	 *
	 * @param superGenericType the type that is the super type from which
	 *     inherited functions are defined.
	 * @param container the type from which the inherited constructors must be
	 *     determined.
	 * @return the list of inherited synthetic constructors in the super type; or {@code null} if a not-synthetic
	 *     constructor was found and no default constructor defined in the super type.
	 */
	protected Iterable<JvmConstructor> extractInheritedConstructors(JvmGenericType superGenericType, JvmGenericType container) {
		final var constructors = new ArrayList<JvmConstructor>();
		var foundNotSyntheticConstructor = false;
		var foundDefaultConstructor = false;
		for (final var constructor : getVisibleInheritedJvmConstructors(superGenericType, container)) {
			if (constructor.getParameters().isEmpty()) {
				foundDefaultConstructor = true;
			}
			if (this.annotationUtils.findAnnotation(constructor, SyntheticEventConstructor.class.getName()) == null) {
				foundNotSyntheticConstructor = true;
			} else {
				constructors.add(constructor);
			}
		}
		if (foundNotSyntheticConstructor && !foundDefaultConstructor) {
			return null;
		}
		return constructors;
	}

	/** Generator of synthetic constructors.
	 *
	 * @author $Author: sgalland$
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.15
	 */
	@SuppressWarnings("synthetic-access")
	private abstract class SyntheticConstructorGenerator implements FormalParameterProvider {

		@Override
		public final XExpression getFormalParameterDefaultValue(int position) {
			return null;
		}

		@Override
		public final String getFormalParameterDefaultValueString(int position) {
			return ""; //$NON-NLS-1$
		}

		/** Replies the list of constructors to be generated.
		 *
		 * @param container the receiver of the generated elements.
		 * @return the list of constructors to be generated.
		 */
		protected abstract Stream<GeneratedSyntheticConstructor> getConstructors(JvmGenericType container);

		/** Generate the synthetic constructors.
		 *
		 * @param context the current generation context.
		 * @param source the SARL source object.
		 * @param container the receiver of the generated elements.
		 * @param baseInferrer the provider of base functions for inferring.
		 */
		public void generate(GenerationContext context, SarlEvent source, JvmGenericType container,
				IBaseJvmModelInferrer baseInferrer) {
			getConstructors(container).forEach(constructor -> {

				final var jvmConstructor = EventInferrerFragment.this.jvmTypesFactory.createJvmConstructor();
				container.getMembers().add(jvmConstructor);
				final var constructorName = container.getSimpleName();
				jvmConstructor.setSimpleName(constructorName);
				jvmConstructor.setVisibility(JvmVisibility.PUBLIC);
				jvmConstructor.setVarArgs(false);

				final var names = new ArrayList<String>();

				for (final var parameter : constructor.parameters) {
					final var formalParameter = EventInferrerFragment.this.jvmTypesFactory.createJvmFormalParameter();
					jvmConstructor.getParameters().add(formalParameter);
					formalParameter.setName(parameter.name());
					final var originalType = parameter.type();
					final var jType = originalType.toJavaCompliantTypeReference();
					final var paramType = cloneWithProxiesFromOtherResource(
							jType,
							jvmConstructor,
							baseInferrer);
					formalParameter.setParameterType(paramType);
					names.add(formalParameter.getName());

					if (parameter.isFinal()) {
						addAnnotationSafe(baseInferrer, formalParameter, SyntheticEventConstructorMandatoryParameter.class);
					}
					markInitialized(parameter, jvmConstructor);
				}

				setBody(jvmConstructor, constructor.body);

				appendGeneratedAnnotation(baseInferrer, jvmConstructor, context);
				addAnnotationSafe(baseInferrer, jvmConstructor, SyntheticEventConstructor.class);

				context.addGeneratedConstructor(constructor.signature(), jvmConstructor);
			});
		}
		
		private void markInitialized(GeneratedSyntheticFormalParameter parameter, JvmConstructor constructor) {
			final var source = parameter.source();
			if (source instanceof XtendField sfield) {
				final var inferredField = EventInferrerFragment.this.sarlAssociations.getJvmField(sfield);
				if (inferredField != null) {
					EventInferrerFragment.this.readAndWriteTracking.markInitialized(inferredField, constructor);
				}
			}
			EventInferrerFragment.this.readAndWriteTracking.markInitialized(source, constructor);
		}

		/** Replies if the given object is the formal parameter associated to a final field.
		 *
		 * @param obj the JVM object of the formal parameter.
		 * @return {@code true} if the given object is for a final field.
		 */
		protected boolean isForFinalField(EObject obj) {
			if (obj instanceof XtendField target) {
				return target.isFinal();
			}
			if (obj instanceof JvmField target) {
				return target.isFinal();
			}
			if (obj instanceof JvmFormalParameter target) {
				return EventInferrerFragment.this.annotationUtils.findAnnotation(target, SyntheticEventConstructorMandatoryParameter.class.getName()) != null;
			}
			return false;
		}

		/** Description of a generated synthetic constructor.
		 *
		 * @param signature the formal definition of the constructor signature.
		 * @param parameters list of details for the formal parameters.
		 * @param body the generator for the constructor body.
		 * @author $Author: sgalland$
		 * @version compiler 0.15.0 20250909-115746
		 * @mavengroupid io.sarl.lang
		 * @mavenartifactid compiler
		 * @since 0.15
		 */
		protected record GeneratedSyntheticConstructor(ActionParameterTypes signature, List<GeneratedSyntheticFormalParameter> parameters, Procedure1<ITreeAppendable> body) {
			//
		}

		/** Description of a generated synthetic formal parameter.
		 *
		 * @param name the name of the parameter.
		 * @param type the type of the parameter.
		 * @param source the Ecore object that is the source of the parameter.
		 * @param isFinal indicates if the parameter is associated to a final field.
		 * @author $Author: sgalland$
		 * @version compiler 0.15.0 20250909-115746
		 * @mavengroupid io.sarl.lang
		 * @mavenartifactid compiler
		 * @since 0.15
		 */
		protected record GeneratedSyntheticFormalParameter(String name, LightweightTypeReference type, EObject source, boolean isFinal) {
			//
		}

	}

	/** Provider of formal parameters based on declared fields.
	 *
	 * @author $Author: sgalland$
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.15
	 */
	@SuppressWarnings("synthetic-access")
	private class FieldBasedGenerator extends SyntheticConstructorGenerator {

		private final List<XtendField> fields;

		/** Constructor.
		 *
		 * @param fields declared fields.
		 */
		FieldBasedGenerator(List<XtendField> fields) {
			this.fields = fields;
		}

		@Override
		public int getFormalParameterCount() {
			return this.fields.size();
		}

		@Override
		public String getFormalParameterName(int position) {
			return this.fields.get(position).getName();
		}

		@Override
		public String getFormalParameterType(int position, boolean isVarargs) {
			return this.fields.get(position).getType().getQualifiedName();
		}

		@Override
		public JvmTypeReference getFormalParameterTypeReference(int position, boolean isVarargs) {
			return this.fields.get(position).getType();
		}

		@Override
		public boolean hasFormalParameterDefaultValue(int position) {
			return !this.fields.get(position).isFinal();
		}

		@Override
		public EObject getFormalParameter(int position) {
			return this.fields.get(position);
		}

		@Override
		protected Stream<GeneratedSyntheticConstructor> getConstructors(JvmGenericType container) {
			final var signatures = EventInferrerFragment.this.sarlSignatureProvider.buildSignatures(container, this);
			return signatures.entrySet().stream().map(signature -> {
				final List<GeneratedSyntheticFormalParameter> parameters = new ArrayList<>(signature.getValue().size());
				final List<String> names = new ArrayList<>();
				for (final var parameter : signature.getValue()) {
					if (!(parameter instanceof InferredValuedParameter)) {
						final var eobj = parameter.getParameter();
						final var syntheticParameter = new GeneratedSyntheticFormalParameter(
								parameter.getName(), parameter.getType(), eobj, isForFinalField(eobj));
						names.add(syntheticParameter.name());
						parameters.add(syntheticParameter);
					}
				}
				return new GeneratedSyntheticConstructor(signature.getKey(), parameters, receiver -> {
					receiver.append("super();"); //$NON-NLS-1$
					for (final var name : names) {
						receiver.newLine().append("this.").append(name).append(" = ").append(name).append(";");  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
					}
				});
			});
		}

	}

	/** Provider of formal parameters based on inherited constructors and declared fields.
	 *
	 * @author $Author: sgalland$
	 * @version compiler 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid compiler
	 * @since 0.15
	 */
	@SuppressWarnings("synthetic-access")
	private class InheritedConstructorFieldBasedGenerator extends SyntheticConstructorGenerator {

		private final List<GeneratedSyntheticFormalParameter> parameters = new ArrayList<>();

		/** Constructor.
		 *
		 * @param inheritedConstructors the constructors that are inherited.
		 * @param fields declared fields.
		 */
		InheritedConstructorFieldBasedGenerator(Iterable<JvmConstructor> inheritedConstructors, List<XtendField> fields) {
			final var defined = new TreeSet<String>();
			for (final var field : fields) {
				if (defined.add(field.getName())) {
					final var type = Utils.toLightweightTypeReference(field.getType(), EventInferrerFragment.this.services, field.eResource().getResourceSet());
					final var syntheticParam = new GeneratedSyntheticFormalParameter(field.getName(), type, field, field.isFinal());
					this.parameters.addLast(syntheticParam);
				}
			}
			final var params = new ArrayList<JvmFormalParameter>();
			for (final var inheritedConstructor : inheritedConstructors) {
				for (final var parameter : inheritedConstructor.getParameters()) {
					if (defined.add(parameter.getName())) {
						params.add(parameter);
					}
				}
			}
			for (final var param : params) {
				final var type = Utils.toLightweightTypeReference(param.getParameterType(), EventInferrerFragment.this.services, param.eResource().getResourceSet());
				final var syntheticParam = new GeneratedSyntheticFormalParameter(param.getName(), type, param, isForFinalField(param));
				this.parameters.addFirst(syntheticParam);
			}
		}

		@Override
		public int getFormalParameterCount() {
			return this.parameters.size(); 
		}

		@Override
		public String getFormalParameterName(int position) {
			return this.parameters.get(position).name; 
		}

		@Override
		public String getFormalParameterType(int position, boolean isVarargs) {
			return this.parameters.get(position).type.getIdentifier(); 
		}

		@Override
		public JvmTypeReference getFormalParameterTypeReference(int position, boolean isVarargs) {
			return this.parameters.get(position).type.toJavaCompliantTypeReference(); 
		}

		@Override
		public boolean hasFormalParameterDefaultValue(int position) {
			return !this.parameters.get(position).isFinal; 
		}

		@Override
		public EObject getFormalParameter(int position) {
			return this.parameters.get(position).source; 
		}

		@Override
		protected Stream<GeneratedSyntheticConstructor> getConstructors(JvmGenericType container) {
			final var signatures = EventInferrerFragment.this.sarlSignatureProvider.buildSignatures(container, this);
			return signatures.entrySet().stream().map(signature -> {
				final List<GeneratedSyntheticFormalParameter> parameters = new ArrayList<>(signature.getValue().size());
				final List<String> inheritedNames = new ArrayList<>();
				final List<String> localNames = new ArrayList<>();
				for (final var parameter : signature.getValue()) {
					if (!(parameter instanceof InferredValuedParameter)) {
						final var syntheticParameter = new GeneratedSyntheticFormalParameter(
								parameter.getName(), parameter.getType(), parameter.getParameter(), isForFinalField(parameter.getParameter()));
						if (parameter.getParameter() instanceof XtendField) {
							localNames.add(syntheticParameter.name());
						} else {
							inheritedNames.add(syntheticParameter.name());
						}
						parameters.add(syntheticParameter);
					}
				}
				return new GeneratedSyntheticConstructor(signature.getKey(), parameters, receiver -> {
					receiver.append("super("); //$NON-NLS-1$
					var first = true;
					for (final var name : inheritedNames) {
						if (first) {
							first = false;
						} else {
							receiver.append(", "); //$NON-NLS-1$
						}
						receiver.append(name);
					}
					receiver.append(");"); //$NON-NLS-1$
					for (final var name : localNames) {
						receiver.newLine().append("this.").append(name).append(" = ").append(name).append(";");  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
					}
				});
			});
		}

	}

}
