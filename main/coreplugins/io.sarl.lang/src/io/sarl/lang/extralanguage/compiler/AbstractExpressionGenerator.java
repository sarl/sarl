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
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.google.common.collect.Lists;
import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.common.types.JvmConstructor;
import org.eclipse.xtext.common.types.JvmDeclaredType;
import org.eclipse.xtext.common.types.JvmExecutable;
import org.eclipse.xtext.common.types.JvmFeature;
import org.eclipse.xtext.common.types.JvmIdentifiableElement;
import org.eclipse.xtext.common.types.JvmMember;
import org.eclipse.xtext.common.types.JvmOperation;
import org.eclipse.xtext.common.types.JvmType;
import org.eclipse.xtext.common.types.util.TypeReferences;
import org.eclipse.xtext.naming.QualifiedName;
import org.eclipse.xtext.resource.persistence.StorageAwareResource;
import org.eclipse.xtext.util.PolymorphicDispatcher;
import org.eclipse.xtext.xbase.XAbstractFeatureCall;
import org.eclipse.xtext.xbase.XBinaryOperation;
import org.eclipse.xtext.xbase.XBlockExpression;
import org.eclipse.xtext.xbase.XConstructorCall;
import org.eclipse.xtext.xbase.XExpression;
import org.eclipse.xtext.xbase.XFeatureCall;
import org.eclipse.xtext.xbase.XMemberFeatureCall;
import org.eclipse.xtext.xbase.compiler.IAppendable;
import org.eclipse.xtext.xbase.featurecalls.IdentifiableSimpleNameProvider;
import org.eclipse.xtext.xbase.jvmmodel.ILogicalContainerProvider;
import org.eclipse.xtext.xbase.jvmmodel.JvmTypesBuilder;
import org.eclipse.xtext.xbase.lib.Functions.Function0;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.scoping.featurecalls.OperatorMapping;
import org.eclipse.xtext.xbase.typesystem.IBatchTypeResolver;
import org.eclipse.xtext.xbase.typesystem.IResolvedTypes;
import org.eclipse.xtext.xbase.typesystem.references.LightweightTypeReference;
import org.eclipse.xtext.xbase.util.XExpressionHelper;

import io.sarl.lang.extralanguage.compiler.ExtraLanguageFeatureNameConverter.ConversionResult;

/** Abstract Generator of XExpression.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractExpressionGenerator implements IExpressionGenerator {

	private static final String TYPE_CONVERTER_INSTANCE = "typeConverterInstance"; //$NON-NLS-1$

	private static final String FEATURE_NAME_CONVERTER_INSTANCE = "featureNameConverterInstance"; //$NON-NLS-1$

	private final PolymorphicDispatcher<XExpression> generateDispatcher;

	private OperatorMapping operatorMapping;

	private TypeReferences typeReferences;

	private XExpressionHelper expressionHelper;

	private IdentifiableSimpleNameProvider featureNameProvider;

	private ILogicalContainerProvider contextProvider;

	private IBatchTypeResolver typeResolver;

	private JvmTypesBuilder jvmTypesBuilder;

	private Injector injector;

	private final IExtraLanguageKeywordProvider keywords;

	/** Constructor.
	 *
	 * @param keywordProvider the provider of extra-language keywords.
	 */
	public AbstractExpressionGenerator(IExtraLanguageKeywordProvider keywordProvider) {
		assert keywordProvider != null;
		this.generateDispatcher = new PolymorphicDispatcher<>(
				"_generate", 3, 3, //$NON-NLS-1$
				Collections.singletonList(this));
		this.keywords = keywordProvider;
	}

	/** Change the type builder.
	 *
	 * @param builder the builder.
	 */
	@Inject
	public void setTypeBuilder(JvmTypesBuilder builder) {
		this.jvmTypesBuilder = builder;
	}

	/** Replies the type builder.
	 *
	 * @return the builder.
	 */
	public JvmTypesBuilder getTypeBuilder() {
		return this.jvmTypesBuilder;
	}

	/** Change the injector.
	 *
	 * @param injector the injector.
	 */
	@Inject
	public void setInjector(Injector injector) {
		this.injector = injector;
	}

	/** Replies the provider of the extra-language keywords.
	 *
	 * @return the provider.
	 */
	public IExtraLanguageKeywordProvider getExtraLanguageKeywordProvider() {
		return this.keywords;
	}

	/** Change the type resolver for expressions.
	 *
	 * @param resolver the batch type resolver.
	 */
	@Inject
	public void setTypeResolver(IBatchTypeResolver resolver) {
		this.typeResolver = resolver;
	}

	/** Replies the type resolver for expressions.
	 *
	 * @return the batch type resolver.
	 */
	public IBatchTypeResolver getTypeResolver() {
		return this.typeResolver;
	}

	/** Change the provider of the logical container.
	 *
	 * @param provider the logical container provider.
	 */
	@Inject
	public void setLogicalContainerProvider(ILogicalContainerProvider provider) {
		this.contextProvider = provider;
	}

	/** Replies the provider of the logical container.
	 *
	 * @return the logical container provider.
	 */
	public ILogicalContainerProvider getLogicalContainerProvider() {
		return this.contextProvider;
	}

	/** Change the basic feature name provider.
	 *
	 * @param provider the feature name provider.
	 */
	@Inject
	public void setFeatureNameProvider(IdentifiableSimpleNameProvider provider) {
		this.featureNameProvider = provider;
	}

	/** Replies the basic feature name provider.
	 *
	 * @return the feature name provider.
	 */
	public IdentifiableSimpleNameProvider getFeatureNameProvider() {
		return this.featureNameProvider;
	}

	/** Change the expression helper.
	 *
	 * @param helper the expression helper.
	 */
	@Inject
	public void setExpressionHelper(XExpressionHelper helper) {
		this.expressionHelper = helper;
	}

	/** Replies the expression helper.
	 *
	 * @return the expression helper.
	 */
	public XExpressionHelper getExpressionHelper() {
		return this.expressionHelper;
	}

	/** Change the type reference finder.
	 *
	 * @param finder the type reference finder.
	 */
	@Inject
	public void setTypeReferences(TypeReferences finder) {
		this.typeReferences = finder;
	}

	/** Replies the type reference finder.
	 *
	 * @return the type reference finder.
	 */
	public TypeReferences getTypeReferences() {
		return this.typeReferences;
	}

	/** Change the mapping for the operators.
	 *
	 * @param mapping the mapping.
	 */
	@Inject
	public void setOperatorMapping(OperatorMapping mapping) {
		this.operatorMapping = mapping;
	}

	/** Replies the mapping for the operators.
	 *
	 * @return the mapping.
	 */
	public OperatorMapping getOperatorMapping() {
		return this.operatorMapping;
	}

	/** Compute the expected type of the given expression.
	 *
	 * @param expr the expression.
	 * @return the expected type of the argument.
	 */
	protected LightweightTypeReference getExpectedType(XExpression expr) {
		final IResolvedTypes resolvedTypes = getTypeResolver().resolveTypes(expr);
		final LightweightTypeReference actualType = resolvedTypes.getActualType(expr);
		return actualType;
	}

	/** Replies the initializer for the type converter.
	 *
	 * @return the initializer
	 */
	@SuppressWarnings("static-method")
	protected IExtraLanguageConversionInitializer getTypeConverterInitializer() {
		return null;
	}

	@Override
	public ExtraLanguageTypeConverter getTypeConverter(IExtraLanguageGeneratorContext context) {
		ExtraLanguageTypeConverter converter = context.getData(TYPE_CONVERTER_INSTANCE, ExtraLanguageTypeConverter.class);
		if (converter == null) {
			converter = createTypeConverterInstance(getTypeConverterInitializer(), context);
			this.injector.injectMembers(converter);
			context.setData(TYPE_CONVERTER_INSTANCE, converter);
		}
		return converter;
	}

	/** Create the instance of the type converter.
	 *
	 * @param initializer the converter initializer.
	 * @param context the genetation context.
	 * @return the type converter.
	 */
	@SuppressWarnings("static-method")
	protected ExtraLanguageTypeConverter createTypeConverterInstance(
			IExtraLanguageConversionInitializer initializer,
			IExtraLanguageGeneratorContext context) {
		return new ExtraLanguageTypeConverter(initializer, context);
	}

	/** Replies the initializer for the feature name converter.
	 *
	 * @return the initializer
	 */
	@SuppressWarnings("static-method")
	protected IExtraLanguageConversionInitializer getFeatureNameConverterInitializer() {
		return null;
	}

	@Override
	public ExtraLanguageFeatureNameConverter getFeatureNameConverter(IExtraLanguageGeneratorContext context) {
		ExtraLanguageFeatureNameConverter converter = context.getData(FEATURE_NAME_CONVERTER_INSTANCE,
				ExtraLanguageFeatureNameConverter.class);
		if (converter == null) {
			converter = createFeatureNameConverterInstance(getFeatureNameConverterInitializer(), context);
			this.injector.injectMembers(converter);
			context.setData(TYPE_CONVERTER_INSTANCE, converter);
		}
		return converter;
	}

	/** Create the instance of the feature name converter.
	 *
	 * @param initializer the converter initializer.
	 * @param context the generation context.
	 * @return the feature name converter.
	 */
	protected ExtraLanguageFeatureNameConverter createFeatureNameConverterInstance(
			IExtraLanguageConversionInitializer initializer,
			IExtraLanguageGeneratorContext context) {
		return new ExtraLanguageFeatureNameConverter(initializer, context, getExtraLanguageKeywordProvider());
	}

	@Override
	public XExpression generate(XExpression expression, LightweightTypeReference expectedType, IAppendable output,
			IExtraLanguageGeneratorContext context) {
		final LightweightTypeReference old = context.setExpectedExpressionType(expectedType);
		try {
			before(expression, output, context);
			return this.generateDispatcher.invoke(expression, output, context);
		} finally {
			after(expression, output, context);
			context.setExpectedExpressionType(old);
		}
	}

	/** Invoked before an expression is processed.
	 *
	 * @param expression the expression.
	 * @param output the output.
	 * @param context the context.
	 */
	protected void before(XExpression expression, IAppendable output, IExtraLanguageGeneratorContext context) {
		//
	}

	/** Invoked after an expression is processed.
	 *
	 * @param expression the expression.
	 * @param output the output.
	 * @param context the context.
	 */
	protected void after(XExpression expression, IAppendable output, IExtraLanguageGeneratorContext context) {
		//
	}

	/** Get the string representation of an operator.
	 *
	 * @param call the call to the operator feature.
	 * @return the string representation of the operator or {@code null} if not a valid operator.
	 */
	protected String getOperatorSymbol(XAbstractFeatureCall call) {
		if (call != null) {
			final Resource res = call.eResource();
			if (res instanceof StorageAwareResource) {
				final boolean isLoadedFromStorage = ((StorageAwareResource) res).isLoadedFromStorage();
				if (isLoadedFromStorage) {
					final QualifiedName operator = getOperatorMapping().getOperator(
							QualifiedName.create(call.getFeature().getSimpleName()));
					return Objects.toString(operator);
				}
			}
			return call.getConcreteSyntaxFeatureName();
		}
		return null;
	}

	/** Compute the simple name for the called feature.
	 *
	 * @param featureCall the feature call.
	 * @param logicalContainerProvider the provider of logicial container.
	 * @param featureNameProvider the provider of feature name.
	 * @param nullKeyword the null-equivalent keyword.
	 * @param thisKeyword the this-equivalent keyword.
	 * @param superKeyword the super-equivalent keyword.
	 * @param referenceNameLambda replies the reference name or {@code null} if none.
	 * @return the simple name.
	 */
	public static String getCallSimpleName(XAbstractFeatureCall featureCall,
			ILogicalContainerProvider logicalContainerProvider,
			IdentifiableSimpleNameProvider featureNameProvider,
			Function0<? extends String> nullKeyword,
			Function0<? extends String> thisKeyword,
			Function0<? extends String> superKeyword,
			Function1<? super JvmIdentifiableElement, ? extends String> referenceNameLambda) {
		String name = null;
		final JvmIdentifiableElement calledFeature = featureCall.getFeature();
		if (calledFeature instanceof JvmConstructor) {
			final JvmDeclaredType constructorContainer = ((JvmConstructor) calledFeature).getDeclaringType();
			final JvmIdentifiableElement logicalContainer = logicalContainerProvider.getNearestLogicalContainer(featureCall);
			final JvmDeclaredType contextType = ((JvmMember) logicalContainer).getDeclaringType();
			if (contextType == constructorContainer) {
				name = thisKeyword.apply();
			} else {
				name = superKeyword.apply();
			}
		} else if (calledFeature != null) {
			final String referenceName = referenceNameLambda.apply(calledFeature);
			if (referenceName != null) {
				name = referenceName;
			} else if (calledFeature instanceof JvmOperation) {
				name = featureNameProvider.getSimpleName(calledFeature);
			} else {
				name = featureCall.getConcreteSyntaxFeatureName();
			}
		}
		if (name == null) {
			return nullKeyword.apply();
		}
		return name;
	}

	/** Compute the list of object that serve as the receiver for the given call.
	 *
	 * @param call the feature call to analyze.
	 * @param output the objects that constitute the call's receiver.
	 * @param thisKeyword the "this" keyword.
	 * @param referenceNameDefinition replies the name of the expression, if defined.
	 * @return {@code true} if a receiver was found; otherwise {@code false}.
	 */
	public static boolean buildCallReceiver(XAbstractFeatureCall call, Function0<? extends String> thisKeyword,
			Function1<? super XExpression, ? extends String> referenceNameDefinition, List<Object> output) {
		if (call.isStatic()) {
			if (call instanceof XMemberFeatureCall) {
				final XMemberFeatureCall memberFeatureCall = (XMemberFeatureCall) call;
				if (memberFeatureCall.isStaticWithDeclaringType()) {
					final XAbstractFeatureCall target = (XAbstractFeatureCall) memberFeatureCall.getMemberCallTarget();
					final JvmType declaringType = (JvmType) target.getFeature();
					output.add(declaringType);
					return true;
				}
			}
			output.add(((JvmFeature) call.getFeature()).getDeclaringType());
			return true;
		}
		final XExpression receiver = call.getActualReceiver();
		if (receiver == null) {
			return false;
		}
		final XExpression implicit = call.getImplicitReceiver();
		if (receiver == implicit) {
			output.add(thisKeyword.apply());
		} else {
			output.add(receiver);
			if (receiver instanceof XAbstractFeatureCall) {
				// some local types have a reference name bound to the empty string
				// which is the reason why we have to check for an empty string as a valid
				// reference name here
				// see AnonymousClassCompilerTest.testCapturedLocalVar_04
				// if it turns out that we have to deal with generics there too, we may
				// have to create a field in the synthesized local class with a unique
				// name that points to 'this'
				if (((XAbstractFeatureCall) receiver).getFeature() instanceof JvmType) {
					final String referenceName = referenceNameDefinition.apply(receiver);
					if (referenceName != null && referenceName.length() == 0) {
						return false;
					}
				}
			}
		}
		return true;
	}

	/** A specific feature call generator.
	 *
	 * <p>Thus generator should be overriden in order to provide specific language implementation.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected abstract class FeatureCallGenerator {

		/** Generation context.
		 */
		protected final IExtraLanguageGeneratorContext context;

		/** Receiver of code.
		 */
		protected final ExtraLanguageAppendable codeReceiver;

		private final Function1<? super XExpression, ? extends String> referenceNameLambda;

		private final Function1<? super JvmIdentifiableElement, ? extends String> referenceNameLambda2;

		/** Constructor.
		 *
		 * @param context the generation context.
		 * @param codeReceiver the receiver for the target language code.
		 */
		protected FeatureCallGenerator(IExtraLanguageGeneratorContext context, ExtraLanguageAppendable codeReceiver) {
			this.context = context;
			this.codeReceiver = codeReceiver;
			this.referenceNameLambda = expr -> getReferenceName(expr);
			this.referenceNameLambda2 = expr -> {
				if (this.codeReceiver.hasName(expr)) {
					return this.codeReceiver.getName(expr);
				}
				return null;
			};
		}

		private XExpression normalizeBlockExpression(XExpression expr) {
			if (expr instanceof XBlockExpression) {
				final XBlockExpression block = (XBlockExpression) expr;
				if (block.getExpressions().size() == 1) {
					return normalizeBlockExpression(block.getExpressions().get(0));
				}
			}
			return expr;
		}

		private List<XExpression> getActualArguments(XAbstractFeatureCall expr) {
			final List<XExpression> actualArguments = expr.getActualArguments();
			return Lists.transform(actualArguments, it  -> normalizeBlockExpression(it));
		}

		private List<XExpression> getActualArguments(XConstructorCall expr) {
			final List<XExpression> actualArguments = expr.getArguments();
			return Lists.transform(actualArguments, it  -> normalizeBlockExpression(it));
		}

		/**  Generate a feature call.
		 *
		 * @param expr the call expression.
		 */
		public void generate(XAbstractFeatureCall expr) {
			if (expr.isTypeLiteral()) {
				final JvmType type = (JvmType) expr.getFeature();
				this.codeReceiver.append(type);
			//} else if (getExpressionHelper().isShortCircuitOperation(expr)) {
			//	generateShortCircuitInvocation(expr);
			} else {
				if (expr instanceof XMemberFeatureCall && ((XMemberFeatureCall) expr).isNullSafe()) {
					featureCalltoJavaExpression(expr, () -> expr);
				} else {
					featureCalltoJavaExpression(expr, null);
				}
			}
		}

		/**  Generate a constructor call.
		 *
		 * @param expr the call expression.
		 */
		public void generate(XConstructorCall expr) {
			final List<Object> leftOperand = new ArrayList<>();
			final List<Object> receiver = new ArrayList<>();
			final JvmConstructor feature = expr.getConstructor();
			final List<XExpression> args = getActualArguments(expr);
			final JvmType type = expr.getConstructor().getDeclaringType();
			this.codeReceiver.getImportManager().addImportFor(type);
			internalAppendCall(feature, leftOperand, receiver, type.getSimpleName(), args, null);
		}

		private boolean needMultiAssignment(XAbstractFeatureCall expr) {
			if (expr instanceof XBinaryOperation) {
				final XBinaryOperation binaryOperation = (XBinaryOperation) expr;
				return binaryOperation.isReassignFirstArgument();
			}
			return false;
		}

		private String getReferenceName(XExpression expr) {
			if (this.codeReceiver.hasName(expr)) {
				return this.codeReceiver.getName(expr);
			}
			if (expr instanceof XFeatureCall) {
				final XFeatureCall featureCall = (XFeatureCall) expr;
				if (this.codeReceiver.hasName(featureCall.getFeature())) {
					return this.codeReceiver.getName(featureCall.getFeature());
				}
			}
			return null;
		}

		private void buildLeftOperand(XAbstractFeatureCall expr, List<Object> output) {
			final XBinaryOperation binaryOperation = (XBinaryOperation) expr;
			final XAbstractFeatureCall leftOperand = (XAbstractFeatureCall) binaryOperation.getLeftOperand();
			final JvmIdentifiableElement feature = leftOperand.getFeature();
			if (this.codeReceiver.hasName(feature)) {
				output.add(this.codeReceiver.getName(feature));
				return;
			}
			buildCallReceiver(leftOperand, getExtraLanguageKeywordProvider().getThisKeywordLambda(),
					this.referenceNameLambda, output);
			output.add(feature.getSimpleName());
		}

		@SuppressWarnings("checkstyle:npathcomplexity")
		private void featureCalltoJavaExpression(XAbstractFeatureCall call, Function0<? extends XExpression> beginOfBlock) {
			final List<Object> leftOperand = new ArrayList<>();
			if (needMultiAssignment(call)) {
				buildLeftOperand(call, leftOperand);
			}
			final List<Object> receiver = new ArrayList<>();
			buildCallReceiver(call, getExtraLanguageKeywordProvider().getThisKeywordLambda(),
					this.referenceNameLambda, receiver);
			final JvmIdentifiableElement feature = call.getFeature();
			List<XExpression> args = null;
			if (feature instanceof JvmExecutable) {
				args = getActualArguments(call);
			}
			final String name = getCallSimpleName(
					call,
					getLogicalContainerProvider(),
					getFeatureNameProvider(),
					getExtraLanguageKeywordProvider().getNullKeywordLambda(),
					getExtraLanguageKeywordProvider().getThisKeywordLambda(),
					getExtraLanguageKeywordProvider().getSuperKeywordLambda(),
					this.referenceNameLambda2);
			internalAppendCall(feature, leftOperand, receiver, name, args, beginOfBlock);
		}

		private void internalAppendCall(JvmIdentifiableElement calledFeature, List<Object> leftOperand,
				List<Object> receiver, String name, List<XExpression> args,
				Function0<? extends XExpression> beginOfBlock) {
			final ExtraLanguageFeatureNameConverter converter = getFeatureNameConverter(this.context);
			final ConversionResult result = converter.convertFeatureCall(name, calledFeature, leftOperand, receiver, args);
			if (result != null) {
				if (result.isFeatureRenaming()) {
					appendCall(calledFeature, leftOperand, receiver, result.toString(), args, beginOfBlock);
				} else {
					for (final Object obj : result.toComplexConversion()) {
						if (obj instanceof CharSequence) {
							this.codeReceiver.append((CharSequence) obj);
						} else if (obj instanceof JvmType) {
							this.codeReceiver.append((JvmType) obj);
						} else if (obj instanceof LightweightTypeReference) {
							this.codeReceiver.append((LightweightTypeReference) obj);
						} else if (obj instanceof XExpression) {
							AbstractExpressionGenerator.this.generate(
									(XExpression) obj, this.codeReceiver, this.context);
						}
					}
				}
			}
		}

		/** Invoked to generate a feature call.
		 *
		 * <p>The given values to the arguments are already converter by the {@link ExtraLanguageFeatureNameConverter}.
		 *
		 * @param calledFeature the called feature.
		 * @param leftOperand the elements to put consider as left operand of an assignment.
		 * @param receiver the receiver of the call.
		 * @param name the name of the called feature.
		 * @param args the arguments.
		 * @param beginOfBlock the expression to be tested at beginning of the block. It should be a "if EXPR not not".
		 */
		protected abstract void appendCall(JvmIdentifiableElement calledFeature, List<Object> leftOperand,
				List<Object> receiver, String name, List<XExpression> args,
				Function0<? extends XExpression> beginOfBlock);

	}

}
