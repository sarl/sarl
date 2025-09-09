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

package io.sarl.lang.core.annotations;

import java.util.Objects;

import org.eclipse.xtend.lib.annotations.AccessorsProcessor;
import org.eclipse.xtend.lib.macro.TransformationContext;
import org.eclipse.xtend.lib.macro.declaration.MutableFieldDeclaration;
import org.eclipse.xtend.lib.macro.declaration.MutableTypeDeclaration;
import org.eclipse.xtend.lib.macro.declaration.ResolvedMethod;
import org.eclipse.xtend.lib.macro.declaration.TypeReference;
import org.eclipse.xtend.lib.macro.declaration.Visibility;
import org.eclipse.xtend2.lib.StringConcatenationClient;

import io.sarl.lang.core.Agent;
import jakarta.inject.Singleton;

/** Processor for the {@code @Accessors} active annotations.
 *
 * <p>This processor that is defined in SARL has the following properties compared to
 * the super processor:<ul>
 * <li>Ensure that the visibility of the generated functions is not higher
 * than the visibility allowed into the containing type.</li>
 * </ul>
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version core 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid core
 * @since 0.9
 */
@Singleton
public class SarlAccessorsProcessor extends AccessorsProcessor {

	/** Constructor.
	 */
	public SarlAccessorsProcessor() {
		//
	}

	@Override
	protected void _transform(MutableFieldDeclaration it, TransformationContext context) {
		final var util = new Util(context);
		if (util.shouldAddGetter(it)) {
			var visibility = util.toVisibility(util.getGetterType(it));
			visibility = applyMinMaxVisibility(visibility, it, context);
			util.addGetter(it, visibility);
		}
		if (util.shouldAddSetter(it)) {
			var visibility = util.toVisibility(util.getSetterType(it));
			visibility = applyMinMaxVisibility(visibility, it, context);
			util.addSetter(it, visibility);
		}
	}

	/** Apply the minimum and maximum visibilities to the given one.
	 *
	 * @param visibility the visibility.
	 * @param it the field associated to the accessors to generate.
	 * @param context the transformation context.
	 * @return the given {@code visibility}, or the min/max visibility if the given one is too high.
	 */
	@SuppressWarnings("static-method")
	protected Visibility applyMinMaxVisibility(Visibility visibility, MutableFieldDeclaration it, TransformationContext context) {
		if (context.findTypeGlobally(Agent.class).isAssignableFrom(it.getDeclaringType())) {
			if (visibility.compareTo(Visibility.PROTECTED) > 0) {
				return Visibility.PROTECTED;
			}
		}
		return visibility;
	}

	/** Utilities for the accessor processor. This class is overridden for fixing Issue #1073.
	 *
	 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
	 * @version core 0.15.0 20250909-115746
	 * @mavengroupid io.sarl.lang
	 * @mavenartifactid core
	 * @since 0.12
	 */
	public class Util extends AccessorsProcessor.Util {

		private final TransformationContext context;

		/** Constructor.
		 *
		 * @param context the accessor context.
		 */
		public Util(TransformationContext context) {
			super(context);
			this.context = context;
		}

		@Override
		public void addSetter(MutableFieldDeclaration field, Visibility visibility) {
			validateSetter(field);
			final var name = getSetterName(field);
			final var type = field.getDeclaringType();
			final var isVarArgs = isInheritedVarargMethod(type, name, orObject(field.getType()));
			type.addMethod(name, it -> {
				this.context.setPrimarySourceElement(it, this.context.getPrimarySourceElement(field));
				it.setReturnType(this.context.getPrimitiveVoid());
				final var param = it.addParameter(field.getSimpleName(),
						orObject(field.getType()));
				it.setBody(new StringConcatenationClient() {
					@Override
					protected void appendTo(StringConcatenationClient.TargetStringConcatenation builder) {
						builder.append(Util.this.fieldOwner(field));
						builder.append("."); //$NON-NLS-1$
						builder.append(field.getSimpleName());
						builder.append(" = "); //$NON-NLS-1$
						builder.append(param.getSimpleName());
						builder.append(";"); //$NON-NLS-1$
					}
				});
				it.setStatic(field.isStatic());
				it.setVisibility(visibility);
				it.setVarArgs(isVarArgs);
			});
		}

		/** Replies if the given method is inherited from a supertype and considered the varargs arguments.
		 *
		 * @param type the type to check and from which the hierarchy is explored.
		 * @param name the name of the method to search for.
		 * @param paramType the variadic argument type.
		 * @return {@code true} if the method is inherited.
		 */
		protected boolean isInheritedVarargMethod(MutableTypeDeclaration type, String name, TypeReference paramType) {
			if (paramType.isArray()) {
				final var ref = this.context.newTypeReference(type);
				for (final var superType : ref.getDeclaredSuperTypes()) {
					for (final var method : superType.getAllResolvedMethods()) {
						if (Objects.equals(name, method.getDeclaration().getSimpleName())
							&& isSingleVarargParameter(method, paramType)) {
							return true;
						}
					}
				}
			}
			return false;
		}

		private boolean isSingleVarargParameter(ResolvedMethod method, TypeReference paramType) {
			var first = true;
			for (final var parameter : method.getResolvedParameters()) {
				if (paramType.equals(parameter.getResolvedType())) {
					return first && method.getDeclaration().isVarArgs();
				}
				first = false;
			}
			return false;
		}

		/** Replies the owner of the given field. It may be {@code this} or the class (if the field is static).
		 *
		 * @param it the field declaration.
		 * @return the enclosing type if the field is static, otherwise {@code this}.
		 */
		protected Object fieldOwner(final MutableFieldDeclaration it) {
			if (it.isStatic()) {
				return this.context.newTypeReference(it.getDeclaringType());
			}
			return "this"; //$NON-NLS-1$
		}

		/** Replies the given reference if it not {@code null}, or the {@link Object} type if it is {@code null}.
		 *
		 * @param ref the reference.
		 * @return the reference or the reference to {@link Object}.
		 */
		protected TypeReference orObject(final TypeReference ref) {
			if (ref == null) {
				return this.context.getObject();
			}
			return ref;
		}

	}

}
