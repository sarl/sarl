/*
 * $Id$
 *
 * File is automatically generated by the Xtext language generator.
 * Do not change it.
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.lang.codebuilder.builders;

import io.sarl.lang.sarl.SarlConstructor;
import io.sarl.lang.sarl.SarlFactory;
import java.util.function.Predicate;
import javax.inject.Inject;
import javax.inject.Provider;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.xtend.XtendFactory;
import org.eclipse.xtend.core.xtend.XtendTypeDeclaration;
import org.eclipse.xtext.common.types.access.IJvmTypeProvider;
import org.eclipse.xtext.util.EmfFormatter;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.compiler.DocumentationAdapter;
import org.eclipse.xtext.xbase.lib.Pure;

/** Builder of a Sarl SarlConstructor.
 */
@SuppressWarnings("all")
public class SarlConstructorBuilderImpl extends AbstractBuilder implements ISarlConstructorBuilder {

	@Inject
	private Provider<IFormalParameterBuilder> parameterProvider;
	@Inject
	private Provider<IBlockExpressionBuilder> blockExpressionProvider;
	@Inject
	private Provider<IExpressionBuilder> expressionProvider;
	private EObject container;

	private SarlConstructor sarlConstructor;

	/** Initialize the Ecore element.
	 * @param container the container of the SarlConstructor.
	 */
	public void eInit(XtendTypeDeclaration container, IJvmTypeProvider context) {
		setTypeResolutionContext(context);
		if (this.sarlConstructor == null) {
			this.container = container;
			this.sarlConstructor = SarlFactory.eINSTANCE.createSarlConstructor();
			this.sarlConstructor.setAnnotationInfo(XtendFactory.eINSTANCE.createXtendMember());
			container.getMembers().add(this.sarlConstructor);
		}
	}

	/** Replies the generated element.
	 */
	@Pure
	public SarlConstructor getSarlConstructor() {
		return this.sarlConstructor;
	}

	/** Replies the resource.
	 */
	@Pure
	public Resource eResource() {
		return getSarlConstructor().eResource();
	}

	/** Change the documentation of the element.
	 *
	 * <p>The documentation will be displayed just before the element.
	 *
	 * @param doc the documentation.
	 */
	public void setDocumentation(String doc) {
		if (Strings.isEmpty(doc)) {
			getSarlConstructor().eAdapters().removeIf(new Predicate<Adapter>() {
				public boolean test(Adapter adapter) {
					return adapter.isAdapterForType(DocumentationAdapter.class);
				}
			});
		} else {
			DocumentationAdapter adapter = (DocumentationAdapter) EcoreUtil.getExistingAdapter(
					getSarlConstructor(), DocumentationAdapter.class);
			if (adapter == null) {
				adapter = new DocumentationAdapter();
				getSarlConstructor().eAdapters().add(adapter);
			}
			adapter.setDocumentation(doc);
		}
	}

	/** Add a formal parameter.
	 * @param name the name of the formal parameter.
	 */
	public IFormalParameterBuilder addParameter(String name) {
		IFormalParameterBuilder builder = this.parameterProvider.get();
		builder.eInit(this.sarlConstructor, name, getTypeResolutionContext());
		return builder;
	}

	/** Add a throwable exception.
	 * @param type the fully qualified name of the exception.
	 */
	public void addException(String type) {
		this.sarlConstructor.getExceptions().add(newTypeRef(this.container, type));
	}

	/** Create the block of code.
	 * @return the block builder.
	 */
	public IBlockExpressionBuilder getExpression() {
		IBlockExpressionBuilder block = this.blockExpressionProvider.get();
		block.eInit(getTypeResolutionContext());
		XBlockExpression expr = block.getXBlockExpression();
		this.sarlConstructor.setExpression(expr);
		return block;
	}

	/** Add a modifier.
	 * @param modifier the modifier to add.
	 */
	public void addModifier(String modifier) {
		if (!Strings.isEmpty(modifier)) {
			getSarlConstructor().getModifiers().add(modifier);
		}
	}

	@Override
	@Pure
	public String toString() {
		return EmfFormatter.objToStr(getSarlConstructor());
	}

	@Inject
	private Provider<ITypeParameterBuilder> iTypeParameterBuilderProvider;

	/** Add a type parameter.
	 * @param name the simple name of the type parameter.
	 * @return the builder of type parameter.
	 */
	public ITypeParameterBuilder addTypeParameter(String name) {
		ITypeParameterBuilder builder = this.iTypeParameterBuilderProvider.get();
		final SarlConstructor object = getSarlConstructor();
		builder.eInit(object, name, getTypeResolutionContext());
		object.getTypeParameters().add(builder.getJvmTypeParameter());
		return builder;
	}

}
